(ns propagators.runtime.session.program.rebuild
  "Transactional rebuild path for compiler-2 runtime source blocks."
  (:require [propagators.runtime.session.program :as program]
            [propagators.runtime.session.program.topology :as topology]
            [propagators.runtime.inspection.temperature :as temperature]
            [propagators.runtime.inspection.semantic-graph :as semantic-repl]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.lowering.application :as compiler-app]
            [propagators.compiler.main :as compiler]
            [propagators.infra.core :as core]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.semantic-trace :as semantic-trace]))

(def seed-appended-block-topology-state
  topology/seed-appended-block-topology-state)

(defn- expose-application-boundary-outputs
  [program-net]
  (net/update-net-dict-entry
   program-net
   compiler-app/application-extra-output-ids-key
   (fnil conj #{})
   (program/boundary-outbox-id)))

(defn- result-key
  [block]
  [(:client-id block) (:index block)])

(defn- compile-topology-form
  [state block epoch source]
  (try
    (let [source (program/normalize-trace-source source)
          source (program/auto-output-source state block source)
          graph-id (program/runtime-graph-id)
          program-net-input (-> (:program/net state)
                                expose-application-boundary-outputs
                                (nb/install-cell graph-id
                                                 (:graph state)
                                                 (:graph state)))
          environment (program/runtime-env
                       state
                       program-net-input
                       (:program/env state)
                       graph-id
                       (:client-id block)
                       [:block (:order block) (:epoch block)])
          compiled (compiler/compile-source
                    source
                    (:env environment)
                    (program/runtime-compile-options
                     {:net (:net environment)
                      :seed [:runtime/block (:order block) (:epoch block)]
                      :reuse-existing-bindings? true}))
          [state program-net] (temperature/run-propagators
                               state
                               :propagation/compile-topology
                               (:net compiled)
                               (:props compiled))]
      (-> state
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :compiled compiled
                 :compiled-network program-net
                 :source source)
          (assoc-in [:program/results (result-key block)]
                    {:compiled compiled
                     :source source
                     :block block
                     :top-level-trace? (program/trace-source? source)})))
    (catch Throwable t
      (assoc-in state
                [:program/results (result-key block)]
                {:error (or (ex-message t) (str (class t)))
                 :class (str (class t))
                 :data (ex-data t)}))))

(defn- rebuild-topology-block
  [state epoch block]
  (let [source (program/block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source))
            (program/trace-source? source))
      state
      (compile-topology-form state block epoch source))))

(defn- block-error?
  [state block]
  (some? (get-in state [:program/results (result-key block) :error])))

(defn- retry-errored-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (if (block-error? s block)
              (rebuild-topology-block s epoch block)
              s))
          state
          source-blocks))

(defn- retry-expression-blocks
  [state _epoch _source-blocks]
  (program/repair-unresolved-application-operators state))

(defn- compile-trace-source-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (let [source (program/block-text s block)]
              (if (and (string? source) (program/trace-source? source))
                (compile-topology-form s block epoch source)
                s)))
          state
          source-blocks))

(defn- base-rebuild-state
  [state epoch]
  (assoc state
         :program/net (nb/install-cell
                       (net/assoc-net-dict-entry
                        (nb/ensure-cell
                         (topology/seed-program-topology
                          state
                          (program/runtime-base-net))
                         (program/boundary-outbox-id))
                        :program/epoch
                        epoch)
                       (program/runtime-graph-id)
                       (semantic-trace/graph-union (program/empty-graph))
                       (semantic-trace/graph-union (program/empty-graph)))
         :program/env (program/runtime-compiler-env)
         :program/graph (program/empty-graph)
         :program/results {}
         :program/epoch epoch
         :compiled nil
         :compiled-network net/empty-net
         :graph (program/empty-graph)
         :source nil))

(defn- compiled-entry
  [state block]
  (get-in state [:program/results (result-key block)]))

(defn- entry-graph
  [entry program-net]
  (when (and (:compiled entry)
             (not (:top-level-trace? entry)))
    (program/namespace-graph
     (semantic-repl/compiled-application-semantic-graph (:compiled entry)
                                                        program-net)
     (str (-> entry :block :client-id) "-"
          (-> entry :block :order)))))

(defn- project-program-graph
  [state source-blocks]
  (let [program-net (:program/net state)]
    (reduce (fn [graph block]
              (let [entry (compiled-entry state block)]
                (cond
                  (:error entry)
                  graph

                  (:top-level-trace? entry)
                  (assoc graph :source (:source entry))

                  :else
                  (if-let [graph* (entry-graph entry program-net)]
                    (assoc (semantic-trace/graph-union graph graph*)
                           :source (:source entry))
                    graph))))
            (program/empty-graph)
            source-blocks)))

(defn- publish-graph
  [state graph]
  (let [graph-id (program/runtime-graph-id)
        [tasks n1] (core/eval-cells [(message graph-id graph)]
                                    (nb/ensure-cell (:program/net state)
                                                    graph-id))
        [state' n2] (temperature/run-tasks state
                                           :propagation/publish-graph
                                           tasks
                                           n1)]
    (assoc state'
           :program/net n2
           :program/graph graph
           :graph graph)))

(defn- settle-state
  [state]
  (assoc state :program/net (program/settle-application-props
                             (:program/net state)
                             [])))

(defn- elapsed-ms
  [started]
  (/ (double (- (System/nanoTime) started)) 1000000.0))

(defn- put-profile
  [state phase ms]
  (assoc-in state [:runtime/last-incremental-profile phase] ms))

(defn- timed-state
  [state phase f]
  (let [started (System/nanoTime)
        state' (f state)]
    (put-profile state' phase (elapsed-ms started))))

(defn- project-results
  [state source-blocks]
  (reduce (fn [s block]
            (let [entry (compiled-entry s block)]
              (if-let [compiled (:compiled entry)]
                (assoc-in s
                          [:program/results (result-key block) :result]
                          (program/compiled-result-value compiled
                                                         (:program/net s)))
                s)))
          state
          source-blocks))

(defn transactional-rebuild-program-state
  [state]
  (let [epoch (inc (or (:program/epoch state) 0))
        source-blocks (program/source-blocks state)
        compiled (as-> (reduce (fn [s block]
                                  (rebuild-topology-block s epoch block))
                                (base-rebuild-state state epoch)
                                source-blocks) s
                   (retry-errored-blocks s epoch source-blocks)
                   (retry-expression-blocks s epoch source-blocks)
                   (compile-trace-source-blocks s epoch source-blocks))
        graph (project-program-graph compiled source-blocks)]
    (as-> compiled s
      (publish-graph s graph)
      (settle-state s)
      (publish-graph s (project-program-graph s source-blocks))
      (settle-state s)
      (project-results s source-blocks)
      (assoc s :compiled-network (:program/net s)))))

(defn- be-block-watch-source?
  [source]
  (try
    (let [forms (program/read-source-forms source)]
      (boolean
       (some (fn [form]
               (and (seq? form)
                    (= '-> (first form))
                    (seq? (nth form 2 nil))
                    (#{'be:block 'be:block-at}
                     (first (nth form 2)))))
             forms)))
    (catch Throwable _
      false)))

(defn- compile-effect-only-form
  [state block epoch source]
  (try
    (let [source (program/normalize-trace-source source)
          graph-id (program/runtime-graph-id)
          program-net-input (-> (:program/net state)
                                expose-application-boundary-outputs
                                (nb/ensure-cell (program/boundary-outbox-id))
                                (nb/install-cell graph-id
                                                 (:graph state)
                                                 (:graph state)))
          environment (program/runtime-env
                       state
                       program-net-input
                       (:program/env state)
                       graph-id
                       (:client-id block)
                       [:effect-only (:order block) (:epoch block)])
          compiled (compiler/compile-source
                    source
                    (:env environment)
                    (program/runtime-compile-options
                     {:net (:net environment)
                      :seed [:runtime/block (:order block) (:epoch block)]
                      :reuse-existing-bindings? true}))
          [state program-net] (temperature/run-propagators
                               state
                               :propagation/effect-only-compile
                               (:net compiled)
                               (:props compiled))]
      (-> state
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :compiled compiled
                 :compiled-network program-net
                 :source source)
          (assoc-in [:program/results (result-key block)]
                    {:compiled compiled
                     :source source
                     :block block
                     :result (program/compiled-result-value compiled
                                                            program-net)})))
    (catch Throwable t
      (assoc-in state
                [:program/results (result-key block)]
                {:error (or (ex-message t) (str (class t)))
                 :class (str (class t))
                 :data (ex-data t)}))))

(defn incremental-block-state
  [state block]
  (let [source (program/block-text state block)]
    (when (and (string? source)
               (not (value/unusable? source))
               (some? (:order block))
               (not (program/trace-source? source)))
      (let [epoch (inc (or (:program/epoch state) 0))
            state (assoc state
                         :program/net (net/assoc-net-dict-entry
                                       (nb/ensure-cell
                                        (:program/net state)
                                        (program/boundary-outbox-id))
                                       :program/epoch
                                       epoch)
                         :runtime/last-incremental-profile {})
            block (assoc block :epoch (:epoch block))
            compiled0 (timed-state
                       (assoc state :program/epoch epoch)
                       :compile
                       #(if (be-block-watch-source? source)
                          (compile-effect-only-form % block epoch source)
                          (program/compile-program-form % block epoch source)))
            entry (compiled-entry compiled0 block)
            compiled (if (:error entry)
                       compiled0
                       (program/repair-unresolved-application-operators
                        compiled0))]
        (when-not (:error entry)
          (let [settled (if (be-block-watch-source? source)
                          compiled
                          (-> compiled
                              (timed-state :publish-graph
                                           #(publish-graph %
                                                           (:graph compiled)))
                              (timed-state :settle settle-state)))]
            (assoc settled
                   :compiled-network (:program/net settled)
                   :runtime/incremental-installs
                   (inc (long (or (:runtime/incremental-installs settled)
                                  0))))))))))

(defn- transaction-supported?
  [state]
  (let [source-update-symbols '#{behavior-event premise-input premise-retract}]
    (letfn [(source-uses-symbols? [source syms]
              (try
                (boolean
                 (some syms
                       (mapcat #(tree-seq coll? seq %)
                               (program/read-source-forms source))))
                (catch Throwable _
                  false)))
            (unsupported-block? [block]
              (let [source (program/block-text state block)]
                (or (program/trace-source? source)
                    (pos? (long (or (:epoch block) 0)))
                    (source-uses-symbols? source source-update-symbols))))]
      (not-any? unsupported-block? (program/source-blocks state)))))

(defn rebuild-program-state
  [state]
  (try
    (if (transaction-supported? state)
      (transactional-rebuild-program-state state)
      (program/rebuild-program-state state))
    (catch Throwable _
      (program/rebuild-program-state state))))
