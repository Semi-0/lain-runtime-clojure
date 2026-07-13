(ns propagators.compiler-2.runtime.application
  "Application propagator for compiler-2 network closures.

  Closure cells are data. This namespace owns runtime application: bind
  arguments into an activation-local environment, compile the
  body into a transient activation network, run it, and emit only the declared
  output diff back to the outer network.
  "
  (:require [clojure.set :as set]
            [propagators.boundary :as boundary]
            [propagators.cells.merge :as cell-merge]
            [propagators.cells.snapshot :refer [pop-inputs]]
            [propagators.cells.value :as value]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.model.env :as env]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.helpers.task-queue :as tq]
            [propagators.ids :as ids]
            [propagators.layered :as layered]
            [propagators.message :as msg :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]
            [propagators.stdlib.prop :as stdlib-prop]))

(def apply-closure-props-key :compiler-2/apply-closure-props)
(def apply-application-props-key :compiler-2/apply-application-props)
(def execute-sub-env-props-key :compiler-2/execute-sub-env-props)
(def application-extra-output-ids-key :compiler-2/application-extra-output-ids)

(defn- inner->outer-boundary-map
  [network]
  (into {}
        (map (fn [[outer inner]] [inner outer]))
        (merge (get (net/net-dict-or-empty network) :avatars-in {})
               (get (net/net-dict-or-empty network) :avatars-out {}))))

(defn- externalize-closure-value
  [v network]
  (let [closure-env (closure-value/closure-env v)]
    (if (value/unusable? closure-env)
      v
      (obj/compound-object
       (assoc (closure-value/slot-map v)
              closure-value/closure-env-slot
              (env/externalize-env closure-env
                                   (inner->outer-boundary-map network)))))))

(declare cell-content-or-nothing)

(defn- activation-cell-value
  [network id]
  (let [content (cell-content-or-nothing network id)]
    (if (value/unusable? content)
      (h/strongest-or-nothing network id)
      content)))

(defn- single-slot-value
  [values]
  (reduce (fn [acc v]
            (cond
              (value/unusable? acc) v
              (= acc v) acc
              :else (reduced value/contradiction)))
          value/nothing
          values))

(defn- accessor-slot-value
  [v network slot-key]
  (let [parent-values
        (->> (obj/accessor-parent-ids v slot-key)
             (filter #(contains? (net/net-env network) %))
             (map #(activation-cell-value network %))
             (remove value/unusable?))]
    (cond
      (seq parent-values)
      (single-slot-value parent-values)

      (obj/accessor-source-slot-present? v slot-key)
      (obj/accessor-source-slot-value v slot-key)

      :else
      value/nothing)))

(defn- externalize-accessor-value
  ([v network]
   (externalize-accessor-value v network #{}))
  ([v network seen]
   (cond
     (not (obj/accessor-network? v))
     v

     (contains? seen (System/identityHashCode v))
     value/nothing

     :else
     (let [seen' (conj seen (System/identityHashCode v))
           source-slots
           (into {}
                 (keep (fn [slot-key]
                         (let [slot-value
                               (externalize-accessor-value
                                (accessor-slot-value v network slot-key)
                                network
                                seen')]
                           (when-not (value/unusable? slot-value)
                             [slot-key slot-value]))))
                 (obj/accessor-slot-keys v))]
       (if (seq source-slots)
         (obj/as-accessor-network source-slots)
         value/nothing)))))

(defn- externalize-output-value
  [v network]
  (cond
    (scope-source/scope-value? v)
    (scope-source/map-base v #(externalize-output-value % network))

    (closure-value/closure-info? v)
    (externalize-closure-value v network)

    (obj/accessor-network? v)
    (externalize-accessor-value v network)

    :else
    v))

(defn- externalized-cell-value
  [network id]
  (let [content (cell-content-or-nothing network id)
        strongest (h/strongest-or-nothing network id)
        content-value (when-not (value/unusable? content)
                        (externalize-output-value content network))]
    (if (and content-value
             (not (value/unusable? content-value)))
      content-value
      (externalize-output-value strongest network))))

(defn- output-symbols
  [output]
  (cond
    (nil? output) []
    (symbol? output) [output]
    (vector? output) output
    :else []))

(defn declare-closure-environment
  "Declare one live closure frame and all of its addressed locals."
  [network lexical-env-id frame-id inputs output-targets input-ids]
  (let [declarations (concat (keep (fn [[sym id]] (when sym [sym id]))
                                   output-targets)
                             (map vector inputs input-ids))
        [sub-props network']
        ((env/p:scope-frame lexical-env-id frame-id (map first declarations))
         (h/ensure-cell network frame-id))]
    (reduce
     (fn [[props n] [sym id]]
       (let [[ids n'] ((env/p:declare-fixed-local sym frame-id id) n)]
         [(into props ids) n']))
     [(vec sub-props) network']
     declarations)))

(defn ^:deprecated closure-body-env
  [lexical-env inputs output-targets input-ids]
  (let [base-env (reduce (fn [scoped-env [sym id]]
                           (if sym
                             (env/bind-local scoped-env sym (env/cell-binding id))
                             scoped-env))
                         (env/sub-env lexical-env)
                         output-targets)]
    (reduce (fn [scoped-env [sym id]]
              (env/bind-local scoped-env sym (env/cell-binding id)))
            base-env
            (map vector inputs input-ids))))

(defn- compile-body
  [compile* body env-id state]
  (compile* (assoc state :env env-id :compiler compile*) body))

(defn prepare-closure-frame
  "Compile a closure body against an already-bound frame environment.

  Returns declaration data only; callers choose transient execution or an
  outer-network topology diff."
  ([network closure-info frame-env-id compile-state]
   (prepare-closure-frame dispatch/default-compiler
                          network closure-info frame-env-id compile-state))
  ([compile* network closure-info frame-env-id compile-state]
   (let [[state result]
         (compile-body compile*
                       (closure-value/closure-body closure-info)
                       frame-env-id
                       (merge {:net network
                               :env frame-env-id
                               :seed [:compiler-2/apply-closure
                                      (closure-value/closure-scope closure-info)]
                               :path []
                               :props []
                               :applications []
                               :compiler compile*}
                              compile-state))]
     {:state state
      :net (:net state)
      :props (:props state)
      :result result
      :result-id (env/binding-id result)})))

(defn- install-output-adapter
  [n result-id out-inner]
  (if (and result-id (not= result-id out-inner))
    (let [[prop-id n'] ((stdlib-prop/id result-id out-inner) n)]
      [n' [prop-id]])
    [n []]))

(defn- install-output-adapters
  [n result-id output-inners]
  (if (= 1 (count output-inners))
    (install-output-adapter n result-id (first output-inners))
    [n []]))

(defn- application-extra-output-ids
  [network]
  (vec (get (net/net-dict-or-empty network)
            application-extra-output-ids-key
            #{})))

(defn- application-external-output-ids
  [network output-ids]
  (vec (distinct (concat output-ids
                         (application-extra-output-ids network)))))

(defn- output-inners-ready?
  [network output-inners]
  (and (seq output-inners)
       (every? (fn [out-inner]
                 (not (value/unusable?
                       (externalized-cell-value network out-inner))))
               output-inners)))

(defn- cell-content-or-nothing
  [network id]
  (if (contains? (net/net-env network) id)
    (net/network-cell-content network id)
    value/nothing))

(defn- external-output-candidate-ids
  [network ext]
  (vec (distinct
        (keep identity
              [(net/lookup-inner-out network ext)
               (when (contains? (net/net-env network) ext)
                 ext)]))))

(defn- externalized-output-message
  [network-from network-to ext]
  (let [outer-content (cell-content-or-nothing network-to ext)
        outer-value (if (value/unusable? outer-content)
                      (h/strongest-or-nothing network-to ext)
                      outer-content)]
    (some (fn [candidate-id]
            (let [output-value (externalized-cell-value network-from candidate-id)]
              (when (and (not (value/unusable? output-value))
                         (cell-merge/cell-updated? output-value
                                                   outer-value
                                                   network-to))
                (message ext output-value))))
          (external-output-candidate-ids network-from ext))))

(defn- externalized-output-messages
  [network-from network-to external-outputs]
  (keep identity
        (map #(externalized-output-message network-from network-to %)
             (vec external-outputs))))

(defn- run-activation-network
  [n inner-inputs prop-ids]
  (let [input-prop-ids (pop-inputs inner-inputs (net/net-graph n))
        tasks (tq/enqueue-all tq/empty-queue
                              (concat prop-ids input-prop-ids))]
    (core/run-tasks tasks n)))

(defn- run-closure-body
  [compile* network closure-info arg-ids output-targets]
  (let [inputs (closure-value/closure-inputs closure-info)
        output (closure-value/closure-output closure-info)
        lexical-env (closure-value/closure-env closure-info)
        body (closure-value/closure-body closure-info)
        output-ids (mapv second output-targets)
        external-output-ids (application-external-output-ids network output-ids)]
    (if (or (value/unusable? lexical-env)
            (value/unusable? body)
            (not= (count inputs) (count arg-ids)))
      network
      (-> (reduce h/ensure-cell network external-output-ids)
          (boundary/create-boundary-outputs external-output-ids)
          (boundary/create-boundary-inputs arg-ids)
          (#(let [inner-inputs (mapv (partial net/lookup-inner-in %) arg-ids)
                  output-inners (mapv (partial net/lookup-inner-out %) output-ids)
                  frame-id (h/stable-node-id :compiler-2/transient-frame
                                             lexical-env arg-ids output-ids)
                  lexical-env-value (h/strongest-or-nothing network lexical-env)
                  with-lexical-env (h/seed-cell % lexical-env lexical-env-value)
                  [env-props activation-net]
                  (declare-closure-environment
                   with-lexical-env
                   lexical-env
                   frame-id
                   inputs
                   (mapv (fn [[sym _id] inner-id] [sym inner-id])
                         output-targets
                         output-inners)
                   inner-inputs)
                  prepared (prepare-closure-frame
                            compile*
                            activation-net
                            closure-info
                            frame-id
                            {:seed [:compiler-2/apply-closure
                                    (closure-value/closure-scope closure-info)
                                    arg-ids
                                    output-ids]})
                  result-id (:result-id prepared)
                  body-net (run-activation-network (:net prepared)
                                                   inner-inputs
                                                   (into (vec env-props)
                                                         (:props prepared)))]
              (if (output-inners-ready? body-net output-inners)
                body-net
                (let [[activation-net adapter-props]
                      (install-output-adapters body-net
                                               result-id
                                               output-inners)]
                  (run-activation-network activation-net
                                          inner-inputs
                                          adapter-props)))))))))

(defn closure-call-plan
  [closure-info arg-ids out-id]
  (let [input-count (count (closure-value/closure-inputs closure-info))
        output-syms (output-symbols (closure-value/closure-output closure-info))
        output-count (count output-syms)
        arg-ids (vec arg-ids)
        implicit-return? (closure-value/implicit-return-output?
                          (closure-value/closure-output closure-info))]
    (cond
      (and implicit-return? (= (count arg-ids) input-count))
      {:input-ids arg-ids
       :targets [[(first output-syms) out-id]]}

      (pos? output-count)
      (when (= (count arg-ids) (+ input-count output-count))
        {:input-ids (subvec arg-ids 0 input-count)
         :targets (mapv vector
                        output-syms
                        (subvec arg-ids input-count))})

      (= (count arg-ids) input-count)
      {:input-ids arg-ids
       :targets [[nil out-id]]})))

(defn closure-application-messages-with
  [compile* closure-id _args-id scheduled-arg-ids out-id network]
  (let [closure-cv (h/strongest-or-nothing network closure-id)
        closure-info (scope-source/unwrap closure-cv)
        arg-ids (vec scheduled-arg-ids)]
    (if (or (value/unusable? closure-cv)
            (not (closure-value/closure-info? closure-info)))
      []
      (let [{:keys [input-ids targets]} (closure-call-plan closure-info
                                                           arg-ids
                                                           out-id)
            input-values (mapv #(h/strongest-or-nothing network %) input-ids)]
        (if (or (nil? targets)
                (value/any-unusable-values? input-values))
          []
          (let [output-ids (mapv second targets)
                network* (reduce h/ensure-cell network output-ids)
                after-body (run-closure-body compile*
                                             network*
                                             closure-info
                                             (vec input-ids)
                                             targets)]
            {:messages (externalized-output-messages after-body
                                                     network*
                                                     (application-external-output-ids
                                                      network*
                                                      output-ids))}))))))

(defn closure-application-messages
  [closure-id args-id scheduled-arg-ids out-id network]
  (closure-application-messages-with dispatch/default-compiler
                                     closure-id args-id scheduled-arg-ids
                                     out-id network))

(defn p:apply-closure-with
  "Apply a compiler-2 closure-info cell to argument cells and one output cell."
  [compile* closure-id args-id arg-ids out-id]
  (let [arg-ids (vec arg-ids)
        activate (fn [_inputs _outputs network]
                   (closure-application-messages-with compile*
                                                      closure-id
                                                      args-id
                                                      arg-ids
                                                      out-id
                                                      network))
        inputs (into [closure-id args-id] arg-ids)]
    (fn [network]
      (let [network* (reduce h/ensure-cell network (conj inputs out-id))
            [prop-id n] ((prop/construct-propagator :compiler-2/apply-closure
                                                    activate inputs [out-id])
                         network*)]
        [prop-id
         (net/update-net-dict-entry n
                                    apply-closure-props-key
                                    (fnil conj #{})
                                    prop-id)]))))

(defn p:apply-closure
  [closure-id args-id arg-ids out-id]
  (p:apply-closure-with dispatch/default-compiler
                        closure-id args-id arg-ids out-id))

(defn- primitive-application-messages
  [compile* operator context-id arg-ids out-id network]
  (if-let [activate (operator-value/operator-compiler-activate operator)]
    (activate compile* network context-id arg-ids out-id)
    (if-let [activate (h/application-activate operator)]
      (activate network context-id arg-ids out-id)
      [])))

(defn application-scope
  "Select one compatible lexical application context and combine provenance."
  [values]
  (let [scoped (filterv scope-source/scope-value? values)
        chains (set (map scope-source/context-chain scoped))]
    (when (and (seq scoped) (= 1 (count chains)))
      (let [chain (first chains)
            rank #(let [i (.lastIndexOf ^java.util.List chain
                                        (scope-source/source-scope %))]
                    (if (neg? i) -1 i))
            candidate (apply max-key rank scoped)]
        {:candidate candidate
         :dependencies (apply set/union
                              (map scope-source/dependencies scoped))}))))

(defn- scoped-result
  "Refine an already-scoped result without turning ordinary values into scopes."
  [{:keys [candidate dependencies]} result]
  (if (scope-source/scope-value? result)
    (scope-source/add-dependencies result dependencies)
    result))

(defn- scope-result-message
  [scope result-id m]
  (if (= result-id (msg/message-id m))
    (message result-id (scoped-result scope (msg/message-value m)))
    m))

(defn- scope-activation-result
  [scope result-id result]
  (if-not scope
    result
    (cond
      (map? result) (update result :messages
                            #(mapv (partial scope-result-message scope result-id)
                                   (or % [])))
      (sequential? result) (mapv (partial scope-result-message scope result-id)
                                 result)
      :else result)))

(defn application-messages-with
  [compile* application-id operator-id args-id scheduled-arg-ids context-id out-id
   network]
  (let [application-info (h/strongest-or-nothing network application-id)
        operator-answer (h/strongest-or-nothing network operator-id)
        operator (scope-source/unwrap operator-answer)
        scope-arg-ids (if (closure-value/closure-info? operator)
                        (take (count (closure-value/closure-inputs operator))
                              scheduled-arg-ids)
                        scheduled-arg-ids)
        argument-values (mapv #(h/strongest-or-nothing network %) scope-arg-ids)
        scope (application-scope (into [operator-answer] argument-values))
        result (cond
      (value/unusable? application-info)
      []

      (not (application-value/application-info? application-info))
      []

      (value/unusable? operator)
      []

      (value/contradiction? operator)
      []

      (operator-value/operator-closure? operator)
      (primitive-application-messages compile*
                                      operator
                                      context-id
                                      scheduled-arg-ids
                                      out-id
                                      network)

      (h/application-activate operator)
      (primitive-application-messages compile*
                                      operator
                                      context-id
                                      scheduled-arg-ids
                                      out-id
                                      network)

      :else
      (closure-application-messages-with compile*
                                         operator-id
                                         args-id
                                         scheduled-arg-ids
                                         out-id
                                         network))]
    (scope-activation-result scope out-id result)))

(defn application-messages
  [application-id operator-id args-id scheduled-arg-ids context-id out-id network]
  (application-messages-with dispatch/default-compiler
                             application-id operator-id args-id scheduled-arg-ids
                             context-id out-id network))

(defn- application-base-id
  [application-id role]
  (h/stable-node-id :compiler-2 :application-base application-id role))

(defn- base-reader-declaration-key
  [application-id role]
  [:application-base-reader application-id role])

(defn- base-reader-declared?
  [network application-id role]
  ((requiring-resolve
    'propagators.compiler-2.runtime.topology-effects/declared?)
   network
   (base-reader-declaration-key application-id role)))

(defn- declare-base-reader
  [network application-id role source-id base-id]
  (let [declaration-key (base-reader-declaration-key application-id role)]
  ((requiring-resolve
    'propagators.compiler-2.runtime.topology-effects/declare-once)
   network
   declaration-key
   base-id
   #(layered/declare-layer-reader %
                                  declaration-key
                                  scope-source/base-layer
                                  source-id
                                  base-id))))

(defn- source-specs
  [application-id operator-id arg-ids]
  (into [{:role :operator
          :source-id operator-id
          :base-id (application-base-id application-id :operator)}]
        (map-indexed
         (fn [index arg-id]
           {:role [:arg index]
            :source-id arg-id
            :base-id (application-base-id application-id [:arg index])})
         arg-ids)))

(defn- addressable-source?
  [network source-id]
  (layered/layer-addressable? (h/strongest-or-nothing network source-id)
                              scope-source/base-layer))

(defn- pending-base-readers
  [network application-id specs]
  (filterv (fn [{:keys [role source-id]}]
             (and (addressable-source? network source-id)
                  (nil? (layered/layer-parent-id network
                                                 source-id
                                                 scope-source/base-layer))
                  (not (base-reader-declared? network application-id role))))
           specs))

(defn- merge-activation-results
  [left right]
  {:effects (into (vec (:effects left)) (:effects right))
   :messages (into (vec (:messages left)) (:messages right))})

(defn- declare-pending-base-readers
  [network application-id specs]
  (reduce (fn [result {:keys [role source-id base-id]}]
            (merge-activation-results
             result
             (declare-base-reader network
                                  application-id
                                  role
                                  source-id
                                  base-id)))
          {:effects [] :messages []}
          specs))

(defn- evaluation-id
  [network {:keys [source-id base-id]}]
  (or (layered/layer-parent-id network source-id scope-source/base-layer)
      (when (addressable-source? network source-id) base-id)
      source-id))

(defn p:apply-application-with
  "Evaluate one retained compiler-2 application object.

  The application object is declaration data. This propagator owns executable
  lowering at evaluation time: primitive operators produce messages directly,
  and closure values delegate to the closure application path."
  [compile* application-id operator-id args-id arg-ids context-id out-id]
  (let [arg-ids (vec arg-ids)
        specs (source-specs application-id operator-id arg-ids)
        possible-base-ids (mapv :base-id specs)
        activate (fn [_inputs _outputs network]
                   (let [pending (pending-base-readers network
                                                       application-id
                                                       specs)]
                     (if (seq pending)
                       (declare-pending-base-readers network
                                                     application-id
                                                     pending)
                       (let [evaluation-ids (mapv #(evaluation-id network %) specs)]
                         (application-messages-with compile*
                                                    application-id
                                                    (first evaluation-ids)
                                                    args-id
                                                    (subvec evaluation-ids 1)
                                                    context-id
                                                    out-id
                                                    network)))))
        inputs (into [application-id operator-id args-id context-id]
                     (concat arg-ids possible-base-ids))]
    (fn [network]
      (let [network* (reduce h/ensure-cell network (conj inputs out-id))
            [prop-id n] ((prop/construct-propagator :compiler-2/apply-application
                                                    activate inputs [out-id])
                         network*)]
        [prop-id
         (net/update-net-dict-entry n
                                    apply-application-props-key
                                    (fnil conj #{})
                                    prop-id)]))))

(defn p:apply-application
  [application-id operator-id args-id arg-ids context-id out-id]
  (p:apply-application-with dispatch/default-compiler
                            application-id operator-id args-id arg-ids
                            context-id out-id))

(defn- declare-child-environment
  [network parent-env-id parent-env child-env-id]
  (let [runtime-parent-id (h/stable-node-id :compiler-2
                                            :execute-sub-env
                                            parent-env-id
                                            child-env-id
                                            :parent)
        [imported _] (env/import-environment network runtime-parent-id parent-env)
        [props declared] ((env/p:scope-frame runtime-parent-id child-env-id)
                          (h/ensure-cell imported child-env-id))]
    [props declared]))

(defn- compile-expr
  [compile* expr child-env network seed props]
  (compile*
   {:net network
    :env child-env
    :seed seed
    :path []
    :props (vec props)
    :applications []
    :compiler compile*}
   expr))

(defn execute-sub-env-messages-with
  [compile* parent-env-id expr-id child-env-id out-id network]
  (let [expr (h/strongest-or-nothing network expr-id)
        parent-env (h/strongest-or-nothing network parent-env-id)]
    (if (or (value/unusable? expr)
            (value/unusable? parent-env))
      []
      (let [[env-props with-child]
            (declare-child-environment network
                                       parent-env-id
                                       parent-env
                                       child-env-id)
            [state result] (compile-expr
                                compile*
                                expr
                                child-env-id
                                with-child
                                [:compiler-2/execute-sub-env
                                 parent-env-id
                                 expr-id
                                 child-env-id
                                 out-id]
                                env-props)
                result-id (env/binding-id result)
                after-body (run-activation-network (:net state)
                                                   []
                                                   (:props state))
                result-answer (h/strongest-or-nothing after-body result-id)
                addressed-id (when (scope-source/scope-value? result-answer)
                               (scope-source/binding-address result-answer))
                value-id (or (when (and (ids/node-id? addressed-id)
                                         (contains? (net/net-env after-body)
                                                    addressed-id))
                                addressed-id)
                             (layered/layer-parent-id after-body
                                                      result-id
                                                      scope-source/base-layer)
                             result-id)
                result-value (h/strongest-or-nothing after-body value-id)
                result-content (cell-content-or-nothing after-body value-id)
                output-content (if (value/unusable? result-content)
                                 result-value
                                 result-content)]
            (cond-> (topology-effects/network-diff network
                                                   after-body
                                                   (:props state))
              (not (value/unusable? output-content))
              (update :messages conj (message out-id output-content)))))))

(defn execute-sub-env-messages
  [parent-env-id expr-id child-env-id out-id network]
  (execute-sub-env-messages-with dispatch/default-compiler
                                 parent-env-id expr-id child-env-id out-id
                                 network))

(defn p:execute-sub-env-with
  [compile* parent-env-id expr-id watch-ids child-env-id out-id]
  (let [watch-ids (vec watch-ids)
        inputs (into [parent-env-id expr-id] watch-ids)
        outputs [child-env-id out-id]
        activate (fn [_inputs _outputs network]
                   (execute-sub-env-messages-with compile*
                                                  parent-env-id
                                                  expr-id
                                                  child-env-id
                                                  out-id
                                                  network))]
    (fn [network]
      (let [network* (reduce h/ensure-cell network (into inputs outputs))
            [prop-id n] ((prop/construct-propagator :compiler-2/execute-sub-env
                                                    activate inputs outputs)
                         network*)]
        [prop-id
         (net/update-net-dict-entry n
                                    execute-sub-env-props-key
                                    (fnil conj #{})
                                    prop-id)]))))

(defn p:execute-sub-env
  "Compile and run one expression in a child compiler-2 env.

  `watch-ids` is the minimal v1 reactivity hook: pass external cells that should
  re-trigger this transient execution when their strongest values change.
  "
  ([parent-env-id expr-id out-id]
   (p:execute-sub-env parent-env-id expr-id [] (ids/new-node-id) out-id))
  ([parent-env-id expr-id child-env-id out-id]
   (p:execute-sub-env parent-env-id expr-id [] child-env-id out-id))
  ([parent-env-id expr-id watch-ids child-env-id out-id]
   (p:execute-sub-env-with dispatch/default-compiler
                           parent-env-id expr-id watch-ids child-env-id out-id)))
