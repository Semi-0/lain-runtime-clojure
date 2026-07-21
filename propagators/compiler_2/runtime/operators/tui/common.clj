(ns propagators.compiler-2.runtime.operators.tui.common
  "Shared TUI operator target lookup and effect request helpers."
  (:require [propagators.compiler-2.runtime.boundary :as boundary]
            [propagators.compiler-2.runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
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

(defn scope-distributed-claims
  "Give claims copied into a shared boundary cell a source-local identity.

  Retained applications may reuse an internal claim ID in distinct result
  cells.  Those claims are valid in isolation, but must not collide when their
  histories meet in one TUI display cell."
  [scope source-content contexts]
  (let [context-supports
        (mapv (fn [{:premise/keys [id state-cell]}]
                (tms/support id
                             [:compiler-2/block-premise state-cell]
                             :block-premise))
              contexts)]
    (tms/distributed-content
     (reduce-kv
      (fn [slots slot fact]
        (if (tms/claim? fact)
          (let [claim-id [scope (tms/claim-id fact)]]
            (assoc slots
                   (tms/claim-slot-key claim-id)
                   (tms/claim claim-id
                              (tms/proposition fact)
                              (tms/claim-value fact)
                              (into (tms/support-objects fact)
                                    context-supports))))
          (assoc slots slot fact)))
      {}
      (tms/distributed-slots source-content)))))

(defn forward-distributed-display-update
  "Forward distributed claims together with the latest premise-state facts.

  A source cell can retain the claim produced by an old block version while
  that version's current active/retracted state lives in a separate context
  cell.  Forwarding only the source would leave the display's copied premise
  state stale."
  [claim-scope source-content state-contents contexts]
  (let [forwarded (scope-distributed-claims claim-scope source-content contexts)
        state-update (tms/distributed-state-update
                      (into [source-content] state-contents))]
    (cond
      (value/contradiction? state-update) state-update
      state-update (tms/merge-distributed-content forwarded state-update)
      :else forwarded)))

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
                    state-contents
                    (mapv #(net/network-cell-content current %) state-ids)
                    update
                    (if (tms/distributed-value? source-content)
                      (forward-distributed-display-update
                       [:tui/block-display display-id source-id]
                       source-content
                       state-contents
                       contexts)
                      (premise/support-update
                       [:tui/block-display display-id source-id]
                       (net/network-cell-strongest current source-id)
                       source-content
                       state-contents
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
