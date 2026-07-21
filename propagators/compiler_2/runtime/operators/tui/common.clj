(ns propagators.compiler-2.runtime.operators.tui.common
  "Shared TUI operator target lookup and effect request helpers."
  (:require [propagators.compiler-2.runtime.boundary :as boundary]
            [propagators.compiler-2.runtime.ids :as runtime-ids]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms.distributed :as tms]
            [propagators.compiler-2.operators.block-premise :as premise]
            [propagators.gur.flat :as fvm]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]))

(defn effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn effect-tick
  [network]
  (let [dict (net/net-dict-or-empty network)
        program-epoch (long (or (:program/epoch dict) 0))
        commit-tick (long (or (:runtime/commit-tick dict) 0))]
    (+ (* program-epoch 1000000000) commit-tick)))

(defn declared-slot-parent-id
  [network block-id slot-key]
  (some->> (get (obj/accessor-declarations-for network block-id) slot-key)
           keys
           (sort-by pr-str)
           first))

(defn instance-block-head-id
  [network instance-id]
  (let [instance-value (net/network-cell-strongest network instance-id)
        instance-id (if (ids/node-id? instance-value)
                      instance-value
                      instance-id)]
    (when-let [blocks-id (declared-slot-parent-id network
                                                  instance-id
                                                  :instance/blocks)]
      (net/network-cell-strongest network blocks-id))))

(defn block-at-slot-id
  [network instance-id index-id slot-key]
  (let [wanted-index (net/network-cell-strongest network index-id)
        first-block-id (instance-block-head-id network instance-id)]
    (loop [block-id first-block-id
           seen #{}]
      (when (and (ids/node-id? block-id)
                 (not (contains? seen block-id)))
        (let [index-cell-id (declared-slot-parent-id network
                                                     block-id
                                                     :block/index)
              slot-cell-id (declared-slot-parent-id network block-id slot-key)
              next-cell-id (declared-slot-parent-id network block-id :cdr)
              block-index (when index-cell-id
                            (net/network-cell-strongest network index-cell-id))]
          (if (= wanted-index block-index)
            slot-cell-id
            (recur (when next-cell-id
                     (net/network-cell-strongest network next-cell-id))
                   (conj seen block-id))))))))

(defn block-at-text-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/text))

(defn block-at-display-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/display))

(defn tui-write-effect-request
  [effect-id text-id payload epoch]
  (boundary/tui-write-effect-request effect-id text-id payload epoch))

(defn tui-display-effect-request
  [effect-id display-id payload tick]
  (boundary/tui-display-effect-request effect-id display-id payload tick))

(defn supported-display-result
  "Connect a premise-supported source directly to a TUI block display cell.

  Returns nil for raw sources so explicit legacy display effects retain their
  existing outbox behavior."
  [network display-id source-id]
  (let [contexts (vec (premise/binding-contexts network source-id))]
    (when (seq contexts)
      (let [state-ids (mapv :premise/state-cell contexts)
            inputs (into [source-id] state-ids)
            prop-id (runtime-ids/stable-node-id
                     :tui :block-display display-id source-id)
            activate
            (fn [_inputs _outputs current]
              (let [source-content (net/network-cell-content current source-id)
                    update
                    (if (tms/distributed-value? source-content)
                      (tms/distributed-forward-update source-content)
                      (premise/support-update
                       [:tui/block-display display-id source-id]
                       (net/network-cell-strongest current source-id)
                       source-content
                       (mapv #(net/network-cell-content current %) state-ids)
                       contexts))]
                (if update [(message display-id update)] [])))]
        {:effects [(fvm/declare-prop prop-id
                                     :runtime/tui-block-display
                                     inputs [display-id] activate)]
         :messages []}))))

(defn trace-target-value
  [label source-id]
  {:trace/target true
   :trace/symbol label
   :node source-id
   :label label})
