(ns propagators.runtime.session.clock
  "Runtime-owned scheduling for declarative `clock-in` subscriptions."
  (:require [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.session.input :as input]
            [propagators.runtime.session.state :as state]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.datastructures.tms.distributed :as tms]
            [propagators.infra.network :as net])
  (:import [java.util.concurrent TimeUnit]))

(defonce ^:private session-runners (atom {}))

(defn- context-active?
  [program-net context]
  (let [content (net/network-cell-content program-net
                                          (:premise/state-cell context))]
    (and (tms/distributed-value? content)
         (contains? (-> content tms/distributed-slots tms/tms-view
                        tms/active-premises)
                    (:premise/id context)))))

(defn subscription-active?
  [runtime-state subscription]
  (let [contexts (:clock/contexts subscription)]
    (or (empty? contexts)
        (every? #(context-active? (:program/net runtime-state) %) contexts))))

(defn clock-update
  [subscription epoch wallclock-ms]
  (event/active-event (:clock/target-id subscription)
                      (or (:clock/source-id subscription)
                          (:clock/id subscription))
                      epoch
                      wallclock-ms))

(defn clock-retraction
  [subscription epoch]
  (event/retraction-event (:clock/target-id subscription)
                          (or (:clock/source-id subscription)
                              (:clock/id subscription))
                          epoch))

(defn- epoch-key
  [subscription]
  (or (:clock/source-id subscription) (:clock/id subscription)))

(defn- apply-clock-update
  [runtime-state subscription update]
  (let [tick (input/next-runtime-commit-tick runtime-state)]
    (-> runtime-state
        (assoc :runtime/commit-tick tick)
        (input/assoc-program-commit-tick tick)
        (assoc-in [:clock/epochs (epoch-key subscription)]
                  (event/timestamp update))
        (input/apply-program-updates
         [{:cell-id (event/input-id update) :update update}])
        effects/run-runtime-cycle)))

(defn tick-clock!
  ([session subscription-id]
   (tick-clock! session subscription-id #(System/currentTimeMillis)))
  ([session subscription-id now]
   (state/mutate-session!
    session
    (fn [runtime-state]
      (if-let [subscription
               (get-in runtime-state [:clock/subscriptions subscription-id])]
        (if (subscription-active? runtime-state subscription)
          (let [epoch (inc (long (get-in runtime-state
                                         [:clock/epochs (epoch-key subscription)]
                                         0)))]
            (apply-clock-update runtime-state subscription
                                (clock-update subscription epoch (long (now)))))
          runtime-state)
        runtime-state)))))

(defn- start-runner
  [session subscription]
  (let [subscription-id (:clock/id subscription)
        interval-ms (long (:clock/interval-ms subscription))
        executor (state/daemon-executor
                  (str "compiler-2-clock-" (hash subscription-id)))]
    (.scheduleWithFixedDelay
     executor
     #(try
        (tick-clock! session subscription-id)
        (catch Throwable t
          (state/record-runtime-error!
           session {:clock/id subscription-id :phase :clock/tick} t)))
     0 interval-ms TimeUnit/MILLISECONDS)
    {:clock/id subscription-id
     :clock/interval-ms interval-ms
     :stop #(do (.shutdownNow executor) nil)}))

(defn- stop-runner! [runner]
  (when-let [stop (:stop runner)]
    (stop)))

(defn- retract-subscription!
  [session subscription-id]
  (state/mutate-session!
   session
   (fn [runtime-state]
     (if-let [subscription
              (get-in runtime-state [:clock/subscriptions subscription-id])]
       (let [epoch (inc (long (get-in runtime-state
                                      [:clock/epochs (epoch-key subscription)]
                                      0)))]
         (apply-clock-update runtime-state subscription
                             (clock-retraction subscription epoch)))
       runtime-state))))

(defn schedule-subscriptions!
  "Reconcile process resources with active declarative clock subscriptions."
  [session]
  (state/ensure-session-state! session)
  (let [runtime-state @session
        desired (into {}
                      (filter (fn [[_ subscription]]
                                (subscription-active? runtime-state subscription)))
                      (:clock/subscriptions runtime-state))
        current (get @session-runners session {})
        removed (remove (comp (set (keys desired)) key) current)
        added (remove (comp (set (keys current)) key) desired)]
    (doseq [[_ runner] removed]
      (stop-runner! runner))
    (doseq [[id _] removed]
      (retract-subscription! session id))
    (let [kept (apply dissoc current (map first removed))
          started (into {} (map (fn [[id subscription]]
                                  [id (start-runner session subscription)]))
                        added)
          next-runners (merge kept started)]
      (swap! session-runners
             (fn [all]
               (if (seq next-runners)
                 (assoc all session next-runners)
                 (dissoc all session))))
      {:active (count next-runners)
       :started (count started)
       :stopped (count removed)})))

(defn stop-all!
  [session]
  (doseq [[_ runner] (get @session-runners session {})]
    (stop-runner! runner))
  (swap! session-runners dissoc session)
  nil)
