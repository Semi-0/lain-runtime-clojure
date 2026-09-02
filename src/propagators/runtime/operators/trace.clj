(ns propagators.runtime.operators.trace
  "Trace compiler-2 runtime operators."
  (:require [propagators.runtime.boundary :as boundary]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.semantic-trace :as semantic-trace]))

(defn- effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn- program-epoch
  [network]
  (or (:program/epoch (net/net-dict-or-empty network))
      0))

(defn- explicit-request-source?
  [source-value]
  (or (symbol? source-value)
      (string? source-value)
      (seq? source-value)
      (and (map? source-value)
           (not (net/network? source-value))
           (not (event/event-projection? source-value))
           (not (semantic-trace/semantic-trace-graph? source-value)))))

(defn- trace-request-source
  [source-id source-value direction]
  (if (explicit-request-source? source-value)
    (semantic-trace/trace-request source-value direction)
    {:node source-id :direction direction}))

(defn- trace-messages
  [network graph-id outbox-id source-id direction-id target-id]
  (let [direction (if direction-id
                    (net/network-cell-strongest network direction-id)
                    :upstream)
        graph (net/network-cell-strongest network graph-id)
        source-value (net/network-cell-strongest network source-id)
        request (trace-request-source source-id source-value direction)]
    (if (or (value/unusable? graph)
            (value/unusable? direction)
            (not (keyword? direction)))
      []
      (let [effect-id [:trace/subscribe target-id]]
        [(message outbox-id
                  (obj/compound-object
                   {(effect-slot-key effect-id)
                    (boundary/xr-trace-subscribe-request effect-id
                                                         request
                                                         target-id
                                                         (program-epoch network))}))]))))

(defn trace-operator [graph-id outbox-id]
  (operator-value/operator-closure
   {:name 'trace
    :output-selector (fn [arg-ids fallback-id]
                       (or (last (vec arg-ids)) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[source-id direction-or-out maybe-out] (vec arg-ids)
                      target-id (or maybe-out direction-or-out out-id)
                      direction-id (when maybe-out direction-or-out)]
                  (when-not (#{2 3} (count arg-ids))
                    (throw (ex-info "trace expects source, optional direction, and output"
                                    {:arg-ids arg-ids})))
                  (trace-messages network
                                  graph-id
                                  outbox-id
                                  source-id
                                  direction-id
                                  target-id)))}))
