(ns propagators.runtime.operators.list-text
  "Linked-list text projection operators for runtime demos."
  (:require [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]))

(defn- strongest
  [network id]
  (if (and id (contains? (net/net-env network) id))
    (net/network-cell-strongest network id)
    value/nothing))

(defn- source-slot
  [coll slot]
  (cond
    (obj/accessor-network? coll)
    (obj/accessor-source-slot-value coll slot)

    (net/net? coll)
    (obj/slot-value coll slot)

    :else
    value/nothing))

(defn- source-slot-present?
  [coll slot]
  (cond
    (obj/accessor-network? coll)
    (obj/accessor-source-slot-present? coll slot)

    (net/net? coll)
    (not (value/unusable? (obj/slot-value coll slot)))

    :else
    false))

(defn- linked-list-prefixes
  [items]
  (loop [coll items
         prefix ""
         tick 0
         acc []]
    (cond
      (value/contradiction? coll)
      value/contradiction

      (value/unusable? coll)
      acc

      (not (source-slot-present? coll :car))
      acc

      :else
      (let [line (source-slot coll :car)
            rest (source-slot coll :cdr)]
        (cond
          (or (value/contradiction? line)
              (value/contradiction? rest))
          value/contradiction

          (value/unusable? line)
          acc

          :else
          (let [current (str prefix line)
                acc' (conj acc [tick current])]
            (cond
              (or (value/unusable? rest)
                  (= :compiler-2/list-empty rest))
              acc'

              (source-slot-present? rest :car)
              (recur rest current (inc tick) acc')

              :else
              (conj acc' [(inc tick) (str current rest)]))))))))

(defn- list-text-event-messages
  [network items-id out-id]
  (let [items (strongest network items-id)
        prefixes (linked-list-prefixes items)]
    (cond
      (value/contradiction? prefixes)
      [(message out-id value/contradiction)]

      (empty? prefixes)
      []

      :else
      (mapv (fn [[tick text]]
              (message out-id
                       (event/active-event out-id out-id tick text)))
            prefixes))))

(defn list-text-events-operator []
  (operator-value/propagator-operator
   {:name 'runtime:list-text-events
    :output-selector (fn [arg-ids fallback-id]
                       [(or (nth (vec arg-ids) 1 nil) fallback-id)])
    :input-selector (fn [arg-ids _fallback-id _context-id]
                      (let [[items-id _out-id] (vec arg-ids)]
                        (when-not (and items-id (<= 1 (count arg-ids) 2))
                          (throw (ex-info "runtime:list-text-events expects items and optional output"
                                          {:arg-ids arg-ids})))
                        [items-id]))
    :activate (fn [network inputs outputs _context-id]
                (list-text-event-messages network (first inputs) (first outputs)))}))
