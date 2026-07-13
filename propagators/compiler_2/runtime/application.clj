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
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.helpers.task-queue :as tq]
            [propagators.ids :as ids]
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

(defn closure-body-env
  [lexical-env inputs output-targets input-ids]
  (let [base-env (reduce (fn [scoped-env [sym id]]
                           (if sym
                             (env/bind-local scoped-env sym (env/cell-binding id))
                             scoped-env))
                         (env/sub-env lexical-env)
                         output-targets)]
    (reduce
     (fn [scoped-env [sym id]]
       (env/bind-local scoped-env sym (env/cell-binding id)))
     base-env
     (map vector inputs input-ids))))

(defn- compile-body
  [compile* body body-env state]
  (compile* (assoc state :env body-env :compiler compile*) body))

(defn prepare-closure-frame
  "Compile a closure body against an already-bound frame environment.

  Returns declaration data only; callers choose transient execution or an
  outer-network topology diff."
  ([network closure-info frame-env compile-state]
   (prepare-closure-frame dispatch/compile-expression
                          network closure-info frame-env compile-state))
  ([compile* network closure-info frame-env compile-state]
   (let [[state result]
         (compile-body compile*
                       (closure-value/closure-body closure-info)
                       frame-env
                       (merge {:net network
                               :env frame-env
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
                  activation-env (closure-body-env lexical-env
                                           inputs
                                           (mapv (fn [[sym _id] inner-id]
                                                   [sym inner-id])
                                                 output-targets
                                                 output-inners)
                                           inner-inputs)
                  prepared (prepare-closure-frame
                            compile*
                            %
                            closure-info
                            activation-env
                            {:seed [:compiler-2/apply-closure
                                    (closure-value/closure-scope closure-info)
                                    arg-ids
                                    output-ids]})
                  result-id (:result-id prepared)
                  body-net (run-activation-network (:net prepared)
                                                   inner-inputs
                                                   (:props prepared))]
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
  (closure-application-messages-with dispatch/compile-expression
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
  (p:apply-closure-with dispatch/compile-expression
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

(defn- scope-message
  [{:keys [candidate dependencies]} m]
  (message (msg/message-id m)
           (scope-source/scope-value
            (scope-source/source-scope candidate)
            nil
            (scope-source/context-chain candidate)
            (msg/message-value m)
            dependencies)))

(defn- scope-activation-result
  [scope result]
  (if-not scope
    result
    (cond
      (map? result) (update result :messages
                            #(mapv (partial scope-message scope) (or % [])))
      (sequential? result) (mapv (partial scope-message scope) result)
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
    (scope-activation-result scope result)))

(defn application-messages
  [application-id operator-id args-id scheduled-arg-ids context-id out-id network]
  (application-messages-with dispatch/compile-expression
                             application-id operator-id args-id scheduled-arg-ids
                             context-id out-id network))

(defn p:apply-application-with
  "Evaluate one retained compiler-2 application object.

  The application object is declaration data. This propagator owns executable
  lowering at evaluation time: primitive operators produce messages directly,
  and closure values delegate to the closure application path."
  [compile* application-id operator-id args-id arg-ids context-id out-id]
  (let [arg-ids (vec arg-ids)
        activate (fn [_inputs _outputs network]
                   (application-messages-with compile*
                                              application-id
                                              operator-id
                                              args-id
                                              arg-ids
                                              context-id
                                              out-id
                                              network))
        inputs (into [application-id operator-id args-id context-id] arg-ids)]
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
  (p:apply-application-with dispatch/compile-expression
                            application-id operator-id args-id arg-ids
                            context-id out-id))

(defn- install-child-env-cell
  [network parent-env-id child-env-id]
  (reduce h/ensure-cell network [parent-env-id child-env-id]))

(defn- compile-expr
  [compile* expr child-env network seed]
  (compile*
   {:net network
    :env child-env
    :seed seed
    :path []
    :props []
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
      (let [with-child (install-child-env-cell network
                                               parent-env-id
                                               child-env-id)
            child-env (env/extend-env parent-env [:env/child child-env-id])
            with-child (h/seed-cell with-child child-env-id child-env)]
        (if (value/unusable? child-env)
          []
          (let [[state result] (compile-expr
                                compile*
                                expr
                                child-env
                                with-child
                                [:compiler-2/execute-sub-env
                                 parent-env-id
                                 expr-id
                                 child-env-id
                                 out-id])
                result-id (env/binding-id result)
                after-body (run-activation-network (:net state)
                                                   []
                                                   (:props state))
                result-value (h/strongest-or-nothing after-body result-id)
                result-content (cell-content-or-nothing after-body result-id)
                output-content (if (value/unusable? result-content)
                                 result-value
                                 result-content)]
            (cond-> []
              (and (contains? (net/net-env network) child-env-id)
                   (not (value/unusable? child-env)))
              (conj (message child-env-id child-env))

              (and result-id
                   (contains? (net/net-env network) result-id)
                   (not (value/unusable? result-content)))
              (conj (message result-id result-content))

              (not (value/unusable? output-content))
              (conj (message out-id output-content)))))))))

(defn execute-sub-env-messages
  [parent-env-id expr-id child-env-id out-id network]
  (execute-sub-env-messages-with dispatch/compile-expression
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
   (p:execute-sub-env-with dispatch/compile-expression
                           parent-env-id expr-id watch-ids child-env-id out-id)))


