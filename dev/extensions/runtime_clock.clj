(ns extensions.runtime-clock
  "Loadable compiler-2 wall-clock primitive declarations."
  (:require [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.operators.block-premise :as premise]
            [propagators.runtime.boundary :as boundary]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

(defn- clock-request-messages
  [network interval-id target-id]
  (let [interval-ms (net/network-cell-strongest network interval-id)]
    (if (value/unusable? interval-ms)
      []
      (do
        (when-not (and (integer? interval-ms) (pos? interval-ms))
          (throw (ex-info "clock-in interval must be a positive integer"
                          {:interval-ms interval-ms})))
        (let [contexts (premise/binding-contexts network target-id)
              effect-id [:clock/subscribe target-id interval-ms]]
          [(message
            (runtime-ids/boundary-outbox-id)
            (obj/compound-object
             {(runtime-ids/effect-slot-key effect-id)
              (boundary/clock-subscribe-request
               effect-id target-id interval-ms contexts)}))])))))

(defn- install-clock
  [network arg-ids fallback-id]
  (let [arg-ids (vec arg-ids)]
    (when-not (#{1 2} (count arg-ids))
      (throw (ex-info "clock-in expects interval-ms and optional output"
                      {:arg-ids arg-ids})))
    (let [interval-id (first arg-ids)
          target-id (or (nth arg-ids 1 nil) fallback-id)
          outbox-id (runtime-ids/boundary-outbox-id)
          marked (-> network
                     (nb/ensure-cell target-id)
                     (event/mark-protocol-cell target-id))
          [prop-id installed]
          ((prop/construct-propagator
            (runtime-ids/stable-node-id :clock-in interval-id target-id)
            :runtime/clock-in
            (fn [_inputs _outputs current]
              (clock-request-messages current interval-id target-id))
            [interval-id] [outbox-id])
           marked)]
      [installed [prop-id] target-id])))

(defn clock-in-operator
  []
  (operator-value/operator-closure
   {:name 'clock-in
    :output-selector
    (fn [arg-ids fallback-id]
      (or (nth (vec arg-ids) 1 nil) fallback-id))
    :static-installer install-clock}))

(defn primitive-bindings
  []
  [['clock-in (clock-in-operator)]])
