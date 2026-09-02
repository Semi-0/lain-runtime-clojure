(ns propagators.runtime.operators.session.effects
  "TUI block write/display effect compiler-2 operators."
  (:require [propagators.runtime.operators.session.common :as common]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]))

(def effect-slot-key common/effect-slot-key)
(def block-at-text-id common/block-at-text-id)
(def block-at-display-id common/block-at-display-id)
(def tui-write-effect-request common/tui-write-effect-request)
(def tui-display-effect-request common/tui-display-effect-request)
(def effect-tick common/effect-tick)
(def supported-display-result common/supported-display-result)
(def event-display-result common/event-display-result)

(defn- write-block-messages
  [network outbox-id text-id source-id read-block?]
  (let [source-value (when text-id
                       (net/network-cell-strongest network text-id))
        target-value (net/network-cell-strongest network source-id)
        write-message (when (and text-id
                                 (not (value/nothing? target-value)))
                        (let [epoch (effect-tick network)
                              effect-id [:tui/write-block
                                         text-id
                                         epoch
                                         (hash target-value)]]
                          (message outbox-id
                                   (obj/compound-object
                                    {(effect-slot-key effect-id)
                                     (tui-write-effect-request effect-id
                                                               text-id
                                                               target-value
                                                               epoch)}))))]
    (cond-> []
      (and read-block? text-id)
      (conj (message source-id source-value))

      write-message
      (conj write-message))))

(defn- display-write-messages
  [network outbox-id display-id source-id]
  (let [target-value (when source-id
                       (net/network-cell-strongest network source-id))
        supported (when (and display-id source-id)
                    (supported-display-result network display-id source-id))]
    (if supported
      supported
      (let [tick (effect-tick network)
            write-message
            (when (and display-id source-id
                       (not (value/unusable? target-value)))
              (let [effect-id [:tui/write-display display-id tick
                               (hash target-value)]]
                (message outbox-id
                         (obj/compound-object
                          {(effect-slot-key effect-id)
                           (tui-display-effect-request effect-id display-id
                                                       target-value tick)}))))]
        (cond-> [] write-message (conj write-message))))))

(defn block-at-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'block-at
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 2 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[instance-id index-id maybe-out] (vec arg-ids)
                      target-id (or maybe-out out-id)
                      text-id (block-at-text-id network instance-id index-id)]
                  (when-not (#{2 3} (count arg-ids))
                    (throw (ex-info "block-at expects instance, index, and optional output"
                                    {:arg-ids arg-ids})))
                  (write-block-messages network outbox-id text-id target-id
                                        (nil? maybe-out))))}))

(defn be-block-at-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'be:block-at
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 2 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[instance-id index-id source-id] (vec arg-ids)
                      source-id (or source-id out-id)
                      display-id (when (and instance-id index-id)
                                   (block-at-display-id network
                                                        instance-id
                                                        index-id))]
                  (when-not (= 3 (count arg-ids))
                    (throw (ex-info "be:block-at expects instance, index, and source"
                                    {:arg-ids arg-ids})))
                  (display-write-messages network outbox-id display-id source-id)))}))

(defn be-event-block-at-operator []
  (operator-value/operator-closure
   {:name 'be:event-block-at
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 2 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[instance-id index-id source-id] (vec arg-ids)
                      source-id (or source-id out-id)
                      display-id (when (and instance-id index-id)
                                   (block-at-display-id network
                                                        instance-id
                                                        index-id))]
                  (when-not (= 3 (count arg-ids))
                    (throw (ex-info
                            "be:event-block-at expects instance, index, and source"
                            {:arg-ids arg-ids})))
                  (if (and display-id source-id)
                    (event-display-result display-id source-id)
                    [])))}))

(defn be-block-target-operator [outbox-id instance-id]
  (operator-value/operator-closure
   {:name 'be:block
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 1 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[index-id maybe-source-id] (vec arg-ids)
                      source-id (or maybe-source-id out-id)
                      display-id (when (and instance-id index-id)
                                   (block-at-display-id network
                                                        instance-id
                                                        index-id))]
                  (when-not (#{1 2} (count arg-ids))
                    (throw (ex-info "be:block expects index and optional source"
                                    {:arg-ids arg-ids})))
                  (display-write-messages network outbox-id display-id source-id)))}))
