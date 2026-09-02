(ns propagators.runtime.inspection.trace.subscriptions
  "External latest-result trace subscriptions for XR."
  (:require [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.session.program :as program]
            [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.temperature :as temperature]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.semantic-trace :as semantic-trace]))

(def mutate-session! state/mutate-session!)

(defonce worker-executor
  (state/daemon-executor "xr-trace-subscriptions"))

(defn runtime-epoch
  [state]
  (max (long (or (:program/epoch state) 0))
       (long (or (:runtime/commit-tick state) 0))))

(defn result-event
  [subscription-id target-id epoch graph]
  (event/active-event target-id subscription-id epoch graph))

(defn result-epoch
  [result]
  (:epoch result))

(defn result-graph
  [result]
  (:graph result))

(defn- stale-result?
  [state subscription-id epoch]
  (let [current (get-in state [:trace/results subscription-id])
        current-epoch (long (or (result-epoch current) Long/MIN_VALUE))]
    (<= (long epoch) current-epoch)))

(defn- unchanged-result?
  [state subscription-id graph]
  (= graph (result-graph (get-in state [:trace/results subscription-id]))))

(defn publish-result-state
  [state {:keys [subscription epoch graph]}]
  (let [subscription-id (:id subscription)]
    (cond
      (not (get-in state [:trace/subscriptions subscription-id]))
      (update state :trace/stale-results (fnil inc 0))

      (stale-result? state subscription-id epoch)
      (update state :trace/stale-results (fnil inc 0))

      :else
      (let [trace-update (result-event subscription-id
                                       (:target-id subscription)
                                       epoch
                                       graph)
            unchanged? (unchanged-result? state subscription-id graph)
            xr-traces (or (:xr/traces state)
                          (get-in state [:xr :traces]))
            [tasks n1] (core/eval-cells
                        [(message (:target-id subscription) trace-update)]
                        (:program/net state))
            [state' n2] (temperature/run-tasks state
                                               :propagation/trace-result
                                               tasks
                                               n1)
            n3 (program/settle-application-props n2 [])]
        (-> state'
            (assoc :program/net n3)
            (assoc-in [:trace/results subscription-id]
                      {:subscription-id subscription-id
                       :epoch epoch
                       :graph graph
                       :event trace-update})
            (update :trace/published-results (fnil inc 0))
            (cond-> unchanged?
              (update :trace/unchanged-results (fnil inc 0)))
            effects/perform-boundary-effects
            (cond-> (seq xr-traces)
              (assoc :xr/traces xr-traces)
              (seq xr-traces)
              (assoc-in [:xr :traces] xr-traces)))))))

(defn publish-result!
  [session result]
  (mutate-session! session #(publish-result-state % result)))

(defn- subscription-snapshot
  [state subscription]
  {:subscription subscription
   :epoch (runtime-epoch state)
   :graph (:graph state)})

(defn- needs-refresh?
  [state subscription]
  (let [epoch (runtime-epoch state)
        scheduled (long (or (:scheduled-epoch subscription) Long/MIN_VALUE))]
    (< scheduled epoch)))

(defn- mark-scheduled
  [state subscription]
  (assoc-in state
            [:trace/subscriptions (:id subscription) :scheduled-epoch]
            (runtime-epoch state)))

(defn- schedule-snapshots-state
  [state]
  (let [subscriptions (vals (:trace/subscriptions state))
        pending (filter #(needs-refresh? state %) subscriptions)]
    [(reduce mark-scheduled state pending)
     (mapv #(subscription-snapshot state %) pending)]))

(defn- compute-result
  [{:keys [subscription epoch graph]}]
  {:subscription subscription
   :epoch epoch
   :graph (semantic-trace/trace-graph graph
                                      (:request subscription))})

(defn schedule-refreshes!
  [session]
  (let [snapshots (atom [])]
    (mutate-session!
     session
     (fn [state]
       (let [[state' snapshots'] (schedule-snapshots-state state)]
         (reset! snapshots snapshots')
         state')))
    (doseq [snapshot @snapshots]
      (.submit worker-executor
               ^Runnable
                 (fn []
                   (try
                     (publish-result! session (compute-result snapshot))
                     (catch Throwable t
                       (state/record-runtime-error!
                        session
                        {:phase :xr/trace-subscription}
                        t))))))
    (count @snapshots)))

(defn refresh-now!
  [session]
  (let [snapshots (atom [])]
    (mutate-session!
     session
     (fn [state]
       (let [[state' snapshots'] (schedule-snapshots-state state)]
         (reset! snapshots snapshots')
         state')))
    (doseq [snapshot @snapshots]
      (try
        (publish-result! session (compute-result snapshot))
        (catch Throwable t
          (state/record-runtime-error!
           session
           {:phase :xr/trace-subscription}
           t))))
    (count @snapshots)))
