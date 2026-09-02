(ns propagators.runtime.session.program
  "Program compilation and rebuild logic for compiler-2 runtime."
  (:require [propagators.runtime.session.block-model :as block-model]
            [propagators.runtime.boundary :as boundary]
            [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.operators :as runtime-ops]
            [propagators.runtime.session.program.source :as source]
            [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.semantic-graph :as semantic-repl]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.closure-value :as closure-value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.operators.block-premise :as block-premise]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.compiler.lowering.application :as compiler-app]
            [propagators.compiler.model.env :as cenv]
            [propagators.compiler.main :as compiler]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.scope-source :as scope-source]
            [propagators.infra.ids :as ids]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.stdlib.prop :as stdlib-prop]
            [propagators.infra.semantic-trace :as semantic-trace]))

(def default-xr-client-id state/default-xr-client-id)
(def empty-state state/empty-state)
(def empty-graph state/empty-graph)
(def runtime-base-net state/runtime-base-net)
(def runtime-compiler-env state/runtime-compiler-env)
(def runtime-graph-id state/runtime-graph-id)
(def boundary-outbox-id state/boundary-outbox-id)
(def perform-boundary-effects effects/perform-boundary-effects)
(def block-text block-model/block-text)
(def source-blocks block-model/source-blocks)
(def all-blocks block-model/all-blocks)
(def block-by-index block-model/block-by-index)
(def read-source-forms source/read-source-forms)
(def top-level-declaration? source/top-level-declaration?)
(def trace-source? source/trace-source?)
(def trace-form? source/trace-form?)
(def normalize-trace-source source/normalize-trace-source)
(def auto-output-source source/auto-output-source)

(declare runtime-compiler)

(defn runtime-application-installer
  [application-id operator-id args-id arg-ids context-id out-id]
  (compiler-app/p:apply-application-with runtime-compiler
                                         application-id
                                         operator-id
                                         args-id
                                         arg-ids
                                         context-id
                                         out-id))

(defn runtime-compiler
  [compiler-state expr]
  (compiler/default-compiler
   (assoc compiler-state :application-installer runtime-application-installer)
   expr))

(defn runtime-compile-options
  [opts]
  (assoc opts
         :compiler (or (:compiler opts) runtime-compiler)
         :application-installer runtime-application-installer))

(declare runtime-env
         settle-application-props)

(defn- expose-application-boundary-outputs
  [program-net]
  (net/update-net-dict-entry
   program-net
   compiler-app/application-extra-output-ids-key
   (fnil conj #{})
   (boundary-outbox-id)))

(defn compiled-state
  ([source]
   (compiled-state source {}))
  ([source options]
   (if (map? options)
     (let [graph-id (runtime-graph-id)
        base-state (assoc (empty-state) :runtime/options options)
        base-net (-> (:program/net base-state)
                     expose-application-boundary-outputs
                     (nb/ensure-cell (boundary-outbox-id))
                     (nb/install-cell graph-id
                                      (semantic-trace/graph-union (empty-graph))
                                      (semantic-trace/graph-union (empty-graph))))
        environment (runtime-env base-state
                                 base-net
                                 (:program/env base-state)
                                 graph-id
                                 default-xr-client-id
                                 [:compiled-source source])
        compiled (compiler/compile-source source
                                          (:env environment)
                                          (runtime-compile-options
                                           {:net (:net environment)
                                            :seed [:runtime/xr source]}))
        network0 (nb/run-propagators (:net compiled) (:props compiled))
        [tasks network1] (core/eval-cells
                          [(message graph-id
                                    (semantic-repl/compiled-semantic-graph
                                     compiled
                                     network0))]
                          network0)
        network2 (core/run-tasks tasks network1)
        network (settle-application-props network2 (:props compiled))
        graph (assoc (semantic-repl/compiled-semantic-graph compiled network)
                     :source source)]
       {:source source
     :compiled compiled
     :compiled-network network
     :network net/empty-net
     :program/net network
     :program/env (:env compiled)
     :program/graph graph
     :program/results {}
     :program/epoch 0
     :block-order []
     :next-order 0
     :tuis {}
     :xr {:launched {}}
        :graph graph})
     (throw (ex-info "runtime compile options must be a map"
                     {:options options})))))

(defn compile-source!
  [session source]
  (doseq [{:keys [stop]} (vals (:traces @session))]
    (when stop (stop)))
  (let [options (or (:runtime/options @session) {})
        state (perform-boundary-effects
               (assoc (compiled-state source options) :traces {}))]
    (reset! session state)
    (:graph state)))

(defn compiled-result-value
  [compiled network]
  (let [result-id (:cell compiled)
        result (scope-source/unwrap
                (net/network-cell-strongest network result-id))]
    (cond
      (value/unusable? result) value/nothing
      (net/network? result) value/nothing
      :else result)))

(defn install-result-premise
  [compiled program-net context]
  (let [raw-id (:cell compiled)
        raw-value (scope-source/unwrap
                   (net/network-cell-strongest program-net raw-id))
        out-id (state/stable-node-id :compiler-2 :block-premise
                                     (:premise/id context) :result)
        contexts (conj (block-premise/binding-contexts program-net raw-id)
                       context)]
    (if (definition/public-definition-cell? program-net raw-id)
      [compiled program-net]
      (let [program-net (block-premise/record-binding-contexts
                         program-net raw-id contexts)]
        (if (or (net/network? raw-value)
            (closure-value/closure-info? raw-value)
            (operator-value/operator-closure? raw-value))
          [compiled program-net]
          (let [program-net (-> program-net
                                (nb/ensure-cell out-id)
                                (block-premise/record-binding-contexts out-id contexts))
                [prop-id installed]
                ((block-premise/p:block-premise
                  [:block-result (:premise/id context)] raw-id contexts out-id)
                 program-net)]
            [(assoc compiled :cell out-id)
             (nb/run-propagators installed [prop-id])]))))))

(defn namespace-graph
  [graph prefix]
  (letfn [(rename [node-id]
            (if (keyword? node-id)
              (keyword (namespace node-id)
                       (str prefix "/" (name node-id)))
              [prefix node-id]))]
    (let [rename-alias (fn [v]
                         (cond
                           (nil? v) #{}
                           (set? v) (set (map rename v))
                           (sequential? v) (set (map rename v))
                           :else #{(rename v)}))]
    (-> graph
        (update :nodes update-keys rename)
        (update :values update-keys rename)
        (update :edges (fn [edges]
                         (mapv (fn [[from to]]
                                 [(rename from) (rename to)])
                               edges)))
        (update :expansions (fn [expansions]
                              (into {}
                                    (map (fn [[node-id expansion]]
                                           [(rename node-id)
                                            (namespace-graph expansion prefix)]))
                                    expansions)))
        (update :node-aliases (fn [aliases]
                                (into {}
                                      (map (fn [[k v]] [k (rename-alias v)]))
                                      aliases)))))))

(defn seed-program-block-cell [program-net runtime-net id]
  (let [v (net/network-cell-strongest runtime-net id)]
    (if (or (value/unusable? v)
            (semantic-trace/semantic-trace-graph? v))
      (nb/ensure-cell program-net id)
      (nb/install-cell program-net id v v))))

(defn install-instance-slots
  [n {:keys [instance-id blocks-id]}]
  (let [[_cell-id prop-ids n]
        (obj/install-slot-access n :instance/blocks instance-id blocks-id)]
    (nb/run-propagators n prop-ids)))

(defn install-block-slots
  [n {:keys [block-id index-id text-id display-id next-id]}]
  (let [[_index-cell index-props n]
        (obj/install-slot-access n :block/index block-id index-id)
        [_text-cell text-props n]
        (obj/install-slot-access n :block/text block-id text-id)
        [_display-cell display-props n]
        (obj/install-slot-access n :block/display block-id display-id)
        [_next-cell next-props n]
        (obj/install-slot-access n :cdr block-id next-id)]
    (nb/run-propagators n
                        (into [] cat [index-props
                                     text-props
                                     display-props
                                     next-props]))))

(defn seed-program-instances [state program-net]
  (reduce-kv
   (fn [n _client-id {:keys [instance-id blocks-id]}]
     (-> n
         (seed-program-block-cell (:network state) instance-id)
         (seed-program-block-cell (:network state) blocks-id)
         (install-instance-slots {:instance-id instance-id
                                  :blocks-id blocks-id})))
   program-net
   (:tuis state)))

(defn seed-program-blocks [state program-net]
  (reduce
   (fn [n block]
     (let [n0 (reduce #(seed-program-block-cell %1 (:network state) %2)
                      n
                      [(:block-id block) (:index-id block)
                       (:next-id block) (:text-id block)
                       (:display-id block)])]
       (install-block-slots
        (if (:text-current-source? block)
          (nb/install-cell n0
                           (:text-id block)
                           (:text-current block)
                           (:text-current block))
          n0)
        block)))
   program-net
   (all-blocks state)))

(defn- core-runtime-bindings
  [graph-id]
  [['block-at (runtime-ops/block-at-operator (boundary-outbox-id))]
   ['be:block-at (runtime-ops/be-block-at-operator (boundary-outbox-id))]
   ['be:event-block-at (runtime-ops/be-event-block-at-operator)]
   ['instance (runtime-ops/instance-operator)]
   ['trace-target (runtime-ops/trace-target-operator)]
   ['trace (runtime-ops/trace-operator graph-id (boundary-outbox-id))]
   ['runtime:list-text-events (runtime-ops/list-text-events-operator)]
   ['load-primitive-environment
    (runtime-ops/load-primitive-environment-operator (boundary-outbox-id))]
   ['load-lain (runtime-ops/load-lain-operator (boundary-outbox-id))]
   ['save-environment
    (runtime-ops/save-environment-operator (boundary-outbox-id))]
   ['translate (runtime-ops/translate-operator)]])

(defn- external-runtime-bindings
  [runtime-state graph-id]
  (let [options (:runtime/options runtime-state)
        groups (or (:operator-groups options) [])
        providers (or (:operator-providers options) {})
        context {:graph-id graph-id
                 :boundary-outbox-id (boundary-outbox-id)}]
    (reduce (fn [bindings group]
              (let [provider (get providers group)]
                (if (fn? provider)
                  (into bindings (provider context))
                  (throw (ex-info "unsupported operator group"
                                  {:group group
                                   :supported-groups (set (keys providers))})))))
            []
            groups)))

(defn- static-runtime-bindings
  [runtime-state graph-id]
  (into (core-runtime-bindings graph-id)
        (external-runtime-bindings runtime-state graph-id)))

(defn- dynamic-runtime-bindings
  [runtime-state current-client-id]
  (let [instance-id (get-in runtime-state [:tuis current-client-id :instance-id])
        versioned? (= :versioned-premise
                      (get-in runtime-state [:tuis current-client-id :mode]))]
    (cond-> (mapv (fn [[client-id {:keys [instance-id]}]]
                    [(symbol client-id) (cenv/cell-binding instance-id)])
                  (:tuis runtime-state))
      instance-id
      (into [['block (if versioned?
                       (runtime-ops/block-cell-operator instance-id)
                       (runtime-ops/block-target-operator (boundary-outbox-id)
                                                          instance-id))]
             ['be:block (runtime-ops/be-block-target-operator
                         (boundary-outbox-id) instance-id)]
             ['load-blocks
              (runtime-ops/load-blocks-operator (boundary-outbox-id)
                                                current-client-id)]
             ['save-blocks
              (runtime-ops/save-blocks-operator (boundary-outbox-id)
                                                current-client-id)]
             ['% (cenv/cell-binding instance-id)]]))))

(defn runtime-env
  ([runtime-state network base-env graph-id current-client-id scope-key]
   (runtime-env runtime-state network base-env graph-id current-client-id
                scope-key {}))
  ([runtime-state network base-env graph-id current-client-id scope-key
    {:keys [fixed-scope?]}]
   (let [dynamic (dynamic-runtime-bindings runtime-state current-client-id)]
     (if-not (ids/node-id? base-env)
       (let [root-id (state/stable-node-id :compiler-2 :runtime-env :root)
             initial (reduce (fn [environment [sym binding]]
                               (cenv/bind-at environment sym binding 0))
                             base-env
                             (into dynamic
                                   (static-runtime-bindings runtime-state graph-id)))
             [network env-id] (cenv/import-environment network root-id initial)]
         {:net network :env env-id :props []})
       (let [child-id (state/stable-node-id :compiler-2 :runtime-env scope-key)
             scope-installer (if fixed-scope?
                               (cenv/p:scope-frame base-env child-id)
                               (cenv/p:sub-env base-env child-id))
             [scope-props network] (scope-installer
                                    (nb/ensure-cell network base-env))
             declared (cenv/declare-bindings network child-id child-id dynamic)
             props (into (vec scope-props) (:props declared))]
         {:net (nb/run-propagators (:net declared) props)
          :env child-id
          :props props})))))

(defn retained-application-props
  [program-net]
  (vec (get (net/net-dict-or-empty program-net)
            compiler-app/apply-application-props-key
            #{})))

(defn settle-application-props
  [program-net current-props]
  (let [props (vec (distinct (concat (retained-application-props program-net)
                                     current-props)))]
    (-> program-net
        (nb/run-propagators props)
        (nb/run-propagators props))))

(defn refresh-semantic-graph
  "Project the current compiled block receipts on demand for tracing."
  [state]
  (let [compiled (keep :compiled (vals (:program/results state)))
        aggregate {:props (vec (distinct (mapcat :props compiled)))
                   :applications (vec (distinct (mapcat :applications compiled)))
                   :cell (some :cell (reverse (vec compiled)))
                   :env (:program/env state)}
        graph (semantic-trace/graph-union
               (if (seq compiled)
                 (semantic-repl/compiled-semantic-graph
                  aggregate (:program/net state))
                 (empty-graph)))
        graph-id (runtime-graph-id)
        network (-> (:program/net state)
                    (nb/ensure-cell graph-id)
                    (nb/seed-cell graph-id graph))
        network (nb/run-propagators
                 network (nb/neighbor-propagator-ids network graph-id))]
    (assoc state :program/net network :program/graph graph :graph graph)))

(defn compile-program-form
  ([state block epoch source]
   (compile-program-form state block epoch source {}))
  ([state block epoch source
    {:keys [fixed-scope? premise-context reuse-existing-bindings?
            application-settler semantic-graph-projector]
     :or {reuse-existing-bindings? true
          application-settler settle-application-props
          semantic-graph-projector semantic-repl/compiled-semantic-graph}
     :as version-options}]
   (try
    (let [source (normalize-trace-source source)
          source (auto-output-source state block source)
          needs-live-graph? (trace-form? source)
          top-level-trace? (trace-source? source)
          graph-id (runtime-graph-id)
          program-net-input (-> (:program/net state)
                                expose-application-boundary-outputs
                                (nb/ensure-cell (boundary-outbox-id))
                                (nb/install-cell graph-id
                                                 (:graph state)
                                                 (:graph state)))
          environment (runtime-env state
                                   program-net-input
                                   (:program/env state)
                                   graph-id
                                   (:client-id block)
                                   [:block (:order block) (:epoch block)]
                                   {:fixed-scope? fixed-scope?})
          compiled (compiler/compile-source
                    source
                    (:env environment)
                    (runtime-compile-options
                     {:net (:net environment)
                      :compiler (:compiler version-options)
                      :block/premise-context premise-context
                      :seed [:runtime/block (:order block) (:epoch block)]
                      :reuse-existing-bindings? reuse-existing-bindings?}))
          program-net0 (nb/run-propagators (:net compiled) (:props compiled))
          program-net2 (if (and needs-live-graph?
                                (not top-level-trace?))
                         (let [graph* (namespace-graph
                                       (semantic-repl/compiled-semantic-graph
                                        compiled
                                        program-net0)
                                       (str (:client-id block) "-"
                                            (:order block)))
                               graph (assoc (semantic-trace/graph-union
                                             (:graph state)
                                             graph*)
                                            :source source)
                               [tasks program-net1]
                               (core/eval-cells [(message graph-id graph)]
                                                (nb/ensure-cell program-net0
                                                                graph-id))]
                           (core/run-tasks tasks program-net1))
                         program-net0)
          program-net (application-settler program-net2 (:props compiled))
          [compiled program-net]
          (if premise-context
            (install-result-premise compiled program-net premise-context)
            [compiled program-net])
          graph (if top-level-trace?
                  (assoc (:graph state) :source source)
                  (let [graph* (namespace-graph
                                (semantic-graph-projector compiled program-net)
                                (str (:client-id block) "-" (:order block)))]
                    (assoc (semantic-trace/graph-union
                            (:graph state)
                            graph*)
                           :source source)))
          result (compiled-result-value compiled program-net)
          result-key [(:client-id block) (:index block)]]
      (-> state
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :program/graph graph
                 :compiled compiled
                 :compiled-network program-net
                 :graph graph
                 :source source)
          (assoc-in [:program/results result-key]
                    {:result result :compiled compiled})))
    (catch Throwable t
      (assoc-in state
                [:program/results [(:client-id block) (:index block)]]
                {:error (or (ex-message t) (str (class t)))
                 :class (str (class t))
                 :data (ex-data t)})))))

(defn rebuild-block
  [state epoch block]
  (let [source (block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source))
            (trace-source? source))
      state
      (compile-program-form state block epoch source))))

(defn block-compile-error?
  [state block]
  (some? (get-in state
                 [:program/results [(:client-id block) (:index block)] :error])))

(defn retry-errored-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (if (block-compile-error? s block)
              (rebuild-block s epoch block)
              s))
          state
          source-blocks))

(defn compiled-has-unresolved-application?
  [compiled program-net]
  (boolean
   (some (fn [app-id]
           (let [application (net/network-cell-strongest program-net app-id)
                 operator-id (obj/slot-value
                              application
                              compiler/application-operator-cell-slot)]
             (or (nil? operator-id)
                 (value/unusable?
                  (net/network-cell-strongest program-net operator-id)))))
         (:applications compiled))))

(defn- repair-application-operator
  [state app-id]
  (let [program-net (:program/net state)
        application (net/network-cell-strongest program-net app-id)
        operator-ast (obj/slot-value
                      application
                      compiler/application-operator-ast-slot)
        operator-id (obj/slot-value
                     application
                     compiler/application-operator-cell-slot)
        sym (when (= :symbol (ast/type operator-ast))
              (ast/name operator-ast))
        resolved-id (when sym
                      (cenv/resolve-binding-id program-net
                                               (:program/env state)
                                               sym))]
    (if (and operator-id
             resolved-id
             (not= operator-id resolved-id)
             (value/unusable?
              (net/network-cell-strongest program-net operator-id))
             (not (value/unusable?
                   (net/network-cell-strongest program-net resolved-id))))
      (let [[prop-id installed]
            ((stdlib-prop/id resolved-id operator-id) program-net)]
        (assoc state :program/net (nb/run-propagators installed [prop-id])))
      state)))

(defn repair-unresolved-application-operators
  [state]
  (let [app-ids (->> (:program/results state)
                     vals
                     (keep :compiled)
                     (mapcat :applications)
                     distinct)]
    (reduce repair-application-operator state app-ids)))

(defn retry-expression-blocks
  [state _epoch _source-blocks]
  (repair-unresolved-application-operators state))

(defn rebuild-program-state
  [state]
  (let [epoch (inc (or (:program/epoch state) 0))
        source-blocks (source-blocks state)
        base (assoc state
                    :program/net (nb/install-cell
                                  (net/assoc-net-dict-entry
                                   (nb/ensure-cell
                                    (seed-program-blocks
                                     state
                                     (seed-program-instances state
                                                             (runtime-base-net)))
                                    (boundary-outbox-id))
                                   :program/epoch
                                   epoch)
                                  (runtime-graph-id)
                                  (semantic-trace/graph-union (empty-graph))
                                  (semantic-trace/graph-union (empty-graph)))
                    :program/env (runtime-compiler-env)
                    :program/graph (empty-graph)
                    :program/results {}
                    :program/epoch epoch
                    :compiled nil
                    :compiled-network net/empty-net
                    :graph (empty-graph)
                    :source nil)]
    (as-> (reduce (fn [s block]
                    (rebuild-block s epoch block))
                  base
                  source-blocks) s
      (retry-errored-blocks s epoch source-blocks)
      (retry-expression-blocks s epoch source-blocks)
      (reduce (fn [s block]
                (let [source (block-text s block)]
                  (if (and (string? source) (trace-source? source))
                    (compile-program-form s block epoch source)
                    s)))
              s
              source-blocks))))
