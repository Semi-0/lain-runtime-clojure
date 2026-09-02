(ns propagators.runtime.inspection.temperature
  "Runtime temperature telemetry sampled at commit/propagation/effect boundaries."
  (:require [propagators.infra.core :as core]
            [propagators.infra.helpers.task-queue :as tq]
            [propagators.infra.network-builder :as nb]))

(def max-samples 1000)

(defn now-ms
  []
  (System/currentTimeMillis))

(defn elapsed-ms
  [started]
  (/ (double (- (System/nanoTime) started)) 1000000.0))

(defn task-count
  [tasks]
  (cond
    (nil? tasks) 0
    (tq/task-queue? tasks) (count (:task-queue/q tasks))
    (set? tasks) (count tasks)
    (sequential? tasks) (count tasks)
    :else 1))

(defn append-sample
  [samples sample]
  (let [samples* (conj (vec samples) sample)
        overflow (- (count samples*) max-samples)]
    (if (pos? overflow)
      (subvec samples* overflow)
      samples*)))

(defn record
  [state phase queued ms]
  (update-in state
             [:runtime :temperature :samples]
             append-sample
             {:phase phase
              :queued queued
              :ms ms
              :at (now-ms)}))

(defn run-tasks
  [state phase tasks network]
  (let [queued (task-count tasks)
        started (System/nanoTime)
        network' (core/run-tasks tasks network)]
    [(record state phase queued (elapsed-ms started)) network']))

(defn run-propagators
  [state phase network prop-ids]
  (let [queued (task-count prop-ids)
        started (System/nanoTime)
        network' (nb/run-propagators network prop-ids)]
    [(record state phase queued (elapsed-ms started)) network']))

(defn- percentile
  [p xs]
  (let [xs (vec (sort xs))]
    (when (seq xs)
      (nth xs (min (dec (count xs))
                   (long (Math/ceil (* p (count xs)))))))))

(defn- average
  [xs]
  (when (seq xs)
    (/ (reduce + xs) (double (count xs)))))

(defn summarize
  [samples]
  (let [samples (vec samples)]
    {:sample-count (count samples)
     :phases
     (into {}
           (map (fn [[phase phase-samples]]
                  (let [queued (map :queued phase-samples)
                        times (map :ms phase-samples)]
                    [phase
                     {:samples (count phase-samples)
                      :queue/avg (average queued)
                      :queue/p95 (percentile 0.95 queued)
                      :queue/max (when (seq queued) (apply max queued))
                      :ms/avg (average times)
                      :ms/p95 (percentile 0.95 times)
                      :ms/max (when (seq times) (apply max times))}])))
           (group-by :phase samples))}))

(defn drain-summary!
  [session]
  (locking session
    (let [summary (summarize (get-in @session [:runtime :temperature :samples]))]
      (swap! session assoc-in [:runtime :temperature :samples] [])
      summary)))
