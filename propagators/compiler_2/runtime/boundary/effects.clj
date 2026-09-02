(ns propagators.compiler-2.runtime.boundary.effects
  "Boundary effect delivery for compiler-2 runtime."
  (:require [propagators.compiler-2.runtime.session.block-model :as block-model]
            [propagators.compiler-2.runtime.boundary :as boundary]
            [propagators.compiler-2.runtime.boundary.display :as display]
            [propagators.compiler-2.runtime.inspection.graph-projection :as graphp]
            [propagators.compiler-2.runtime.inspection.retraction :as retraction]
            [propagators.compiler-2.runtime.session.state :as state]
            [propagators.compiler-2.runtime.inspection.temperature :as temperature]
            [propagators.compiler-2.runtime.inspection.semantic-graph :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(def boundary-outbox-id state/boundary-outbox-id)
(def receipt-slot-key state/receipt-slot-key)
(def xr-receipt state/xr-receipt)
(def record-widget-register graphp/record-widget-register)
(def block-by-text-id block-model/block-by-text-id)
(def block-by-display-id block-model/block-by-display-id)
(def update-block block-model/update-block)

(defn assoc-block-current-text
  [state block payload tick]
  (update-block state
                (:client-id block)
                (:index block)
                #(assoc % :text-current payload
                          :text-effect-tick tick)))

(defn write-block-value
  [state block epoch payload]
  (let [epoch (long (or epoch 0))
        current-tick (long (or (:text-effect-tick block) Long/MIN_VALUE))]
    (if (< epoch current-tick)
      state
      (let [state (assoc-block-current-text state block payload epoch)
        current (net/network-cell-strongest (:network state) (:text-id block))]
        (if (or (= current payload)
                (and (not (value/unusable? current))
                     (not= current payload)))
          state
          (let [[tasks n1] (core/eval-cells [(message (:text-id block) payload)]
                                            (:network state))
                [state' n2] (temperature/run-tasks state
                                                   :effects/tui-write-block
                                                   tasks
                                                   n1)]
            (assoc state' :network n2)))))))

(defn outbox-effects
  [program-net]
  (if-not (contains? (net/net-env program-net) (boundary-outbox-id))
    []
    (let [outbox (net/network-cell-strongest program-net (boundary-outbox-id))]
      (if (value/unusable? outbox)
        []
        (->> (obj/public-slot-keys outbox)
             (keep (fn [slot-key]
                     (let [request (obj/slot-value outbox slot-key)]
                       (when (:boundary/effect request)
                         request))))
             vec)))))

(defn receipt-message
  [request status]
  (message (:boundary/receipt-id request)
           (obj/compound-object
            {(receipt-slot-key (:boundary/id request))
             (xr-receipt request status)})))

(defn environment-receipt-message
  [request receipt]
  (message (:boundary/receipt-id request)
           (obj/compound-object
            {(receipt-slot-key (:boundary/id request))
             (boundary/environment-receipt
              request (:status receipt) (dissoc receipt :status))})))

(defn environment-request?
  [request]
  (= :environment (:boundary/port request)))

(declare collapse-boundary-effects drain-environment-effects)

(defn undelivered-environment-effects
  [state]
  (->> (outbox-effects (:program/net state))
       collapse-boundary-effects
       (filter environment-request?)
       (filter (fn [request]
                 (let [delivery (get-in state
                                        [:environment/effects
                                         (:boundary/id request)])]
                   (or (nil? delivery)
                       (and (not= :processing (:status delivery))
                            (not (contains? (:receipt-ids delivery)
                                            (:boundary/receipt-id request))))))))
       vec))

(defn- deliver-environment-receipt
  [state request receipt]
  (let [receipt-message (environment-receipt-message request receipt)
        [tasks program-net]
        (core/eval-cells [receipt-message] (:program/net state))
        [state program-net]
        (temperature/run-tasks state
                               :effects/environment-receipt
                               tasks
                               program-net)]
    (assoc state :program/net program-net)))

(defn- perform-environment-request
  [state request]
  (let [effect-id (:boundary/id request)
        existing (get-in state [:environment/effects effect-id])]
    (if existing
      (let [same-payload? (= (:boundary/payload request)
                             (get-in existing [:request :boundary/payload]))
            receipt (if same-payload?
                      (assoc (:receipt existing) :replayed? true)
                      {:status :failed
                       :diagnostics [{:message "environment effect id collision"
                                      :effect-id effect-id}]})]
        (-> state
            (update-in [:environment/effects effect-id :receipt-ids]
                       (fnil conj #{}) (:boundary/receipt-id request))
            (deliver-environment-receipt request receipt)))
      (let [processing
            (assoc-in state [:environment/effects effect-id]
                      {:status :processing
                       :request request
                       :receipt-ids #{(:boundary/receipt-id request)}})]
        (try
          (let [perform (requiring-resolve
                         'propagators.compiler-2.runtime.session.environment-io/perform-request)
                {next-state :state receipt :receipt}
                (perform processing request drain-environment-effects)]
            (-> next-state
                (assoc-in [:environment/effects effect-id]
                          {:status (:status receipt)
                           :request request
                           :receipt receipt
                           :receipt-ids #{(:boundary/receipt-id request)}})
                (deliver-environment-receipt request receipt)))
          (catch Throwable t
            (let [receipt {:status :failed
                           :diagnostics [{:message (ex-message t)
                                          :data (ex-data t)
                                          :class (some-> t class .getName)}]}]
              (-> processing
                  (assoc-in [:environment/effects effect-id]
                            {:status :failed
                             :request request
                             :receipt receipt
                             :receipt-ids #{(:boundary/receipt-id request)}})
                  (deliver-environment-receipt request receipt)))))))))

(defn drain-environment-effects
  "Run newly declared environment effects until none remain.

  The ledger is written before evaluation, so a nested load cannot re-enter its
  own retained outbox request.  This loop is deliberately outside the kernel
  scheduler."
  [state]
  (loop [current state
         round 0]
    (let [requests (undelivered-environment-effects current)]
      (cond
        (empty? requests)
        current

        (>= round 256)
        (throw (ex-info "environment effect round limit exceeded"
                        {:maximum-rounds 256
                         :pending (mapv :boundary/id requests)}))

        :else
        (recur (reduce perform-environment-request current requests)
               (inc round))))))

(defn record-xr-launch
  [state request]
  (if (get-in state [:xr :launched (:boundary/id request)])
    state
    (let [receipt (receipt-message request :delivered)
          [tasks program-net] (core/eval-cells [receipt] (:program/net state))]
      (-> state
          (assoc :program/net program-net)
          (update-in [:xr :launched]
                     (fnil assoc {})
                     (:boundary/id request)
                     {:request request
                      :receipt (:value receipt)})
          (update-in [:xr :effects]
                     (fnil conj [])
                     request)
          (assoc :program/pending-after-effects tasks)))))

(defn record-trace-subscription
  [state boundary-request]
  (let [{trace-request :request target-id :target-id}
        (:boundary/payload boundary-request)
        subscription-id (:boundary/id boundary-request)]
    (assoc-in state
              [:trace/subscriptions subscription-id]
              {:id subscription-id
               :request trace-request
               :target-id target-id
               :created-epoch (:boundary/epoch boundary-request)})))

(defn record-tui-write
  [state request]
  (let [text-id (get-in request [:boundary/target :text-id])
        payload (:boundary/payload request)]
    (if-let [block (block-by-text-id state text-id)]
      (if (some? (:order block))
        state
        (-> state
            (write-block-value block (:boundary/epoch request) payload)
            (update-in [:tui :effects] (fnil conj []) request)))
      state)))

(defn write-block-display-value
  [state block tick payload]
  (let [display-id (:display-id block)
        update (display/update-value display-id display-id tick payload)
        [tasks n1] (core/eval-cells [(message (:display-id block) update)]
                                    (:network state))
        [state' n2] (temperature/run-tasks state
                                           :effects/tui-write-display
                                           tasks
                                           n1)]
    (assoc state' :network n2)))

(defn record-tui-display
  [state request]
  (let [display-id (get-in request [:boundary/target :display-id])
        payload (:boundary/payload request)
        tick (or (:boundary/tick request)
                 (:boundary/epoch request)
                 0)]
    (if-let [block (block-by-display-id state display-id)]
      (-> state
          (write-block-display-value block tick payload)
          (update-in [:tui :effects] (fnil conj []) request))
      state)))

(defn record-clock-subscription
  [state request]
  (let [subscription-id (:boundary/id request)
        target-id (get-in request [:boundary/target :cell-id])
        {:keys [interval-ms contexts]} (:boundary/payload request)
        context (first contexts)
        source-id (if context
                    [:clock/source (:client/id context) (:block/id context)]
                    [:clock/source target-id])]
    (assoc-in state [:clock/subscriptions subscription-id]
              {:clock/id subscription-id
               :clock/source-id source-id
               :clock/target-id target-id
               :clock/interval-ms interval-ms
               :clock/contexts (vec contexts)
               :clock/request request})))

(defn graph-score
  [request]
  (let [payload (:boundary/payload request)
        graph (if (semantic-trace/semantic-trace-graph? payload)
                payload
                (:graph payload))]
    (+ (count (:nodes graph))
       (count (:edges graph))
       (count (:values graph)))))

(defn delivery-key
  [request]
  (case [(:boundary/port request) (:boundary/kind request)]
    [:inspection :inspection/profile-next-commit]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/id request)]

    [:clock :clock/subscribe]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/id request)]

    [:tui :tui/write-display]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/epoch request)]

    [:tui :tui/write-block]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/epoch request)]

    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/receipt-id request)
     (:boundary/epoch request)]))

(defn prefer-latest-boundary-request?
  [request]
  (= [(:boundary/port request) (:boundary/kind request)]
     [:xr :xr/launch-trace]))

(defn richer-boundary-request
  [old request]
  (if (> (graph-score old) (graph-score request))
    old
    request))

(defn usable-boundary-request
  [old request]
  (let [old-unusable? (value/unusable? (:boundary/payload old))
        request-unusable? (value/unusable? (:boundary/payload request))]
    (cond
      (and old-unusable? (not request-unusable?)) request
      (and request-unusable? (not old-unusable?)) old
      :else nil)))

(defn better-boundary-request
  [old request]
  (let [usable (when old
                 (usable-boundary-request old request))]
    (cond
      (nil? old)
      request

      usable
      usable

      (prefer-latest-boundary-request? request)
      (richer-boundary-request old request)

      (>= (graph-score old) (graph-score request))
      old

      :else
      request)))

(defn collapse-boundary-effects
  [requests]
  (->> requests
       (reduce (fn [acc request]
                 (update acc
                         (delivery-key request)
                         #(better-boundary-request % request)))
               {})
       vals))

(defn perform-boundary-effects
  [state]
  (let [started (System/nanoTime)
        state (drain-environment-effects state)
        requests (->> (outbox-effects (:program/net state))
                      collapse-boundary-effects
                      (remove environment-request?))
        state' (reduce (fn [s request]
                         (case [(:boundary/port request) (:boundary/kind request)]
                           [:xr :xr/launch-trace] (record-xr-launch s request)
                           [:xr :xr/trace-subscribe] (record-trace-subscription s request)
                           [:xr :xr/widget-register] (record-widget-register s request)
                           [:tui :tui/write-block] (record-tui-write s request)
                           [:tui :tui/write-display] (record-tui-display s request)
                           [:clock :clock/subscribe]
                           (record-clock-subscription s request)
                           [:inspection :inspection/profile-next-commit]
                           (retraction/record-probe s request)
                           s))
                       state
                       requests)]
    (temperature/record state'
                        :effects/boundary
                        (count requests)
                        (temperature/elapsed-ms started))))

(defn refresh-program-graph
  [state]
  (if (and (:compiled state) (:program/net state))
    (let [fresh (semantic-repl/compiled-semantic-graph (:compiled state)
                                                       (:program/net state))
          graph (semantic-trace/graph-union (:graph state) fresh)]
      (assoc state
             :graph graph
             :program/graph graph
             :compiled-network (:program/net state)))
    state))

(defn run-runtime-cycle
  "Apply already-committed model state through propagation effects.

  Public for tests; command handlers should keep using the higher-level TUI/XR
  operations unless they are deliberately testing the runtime cycle boundary."
  [state]
  (perform-boundary-effects (refresh-program-graph state)))
