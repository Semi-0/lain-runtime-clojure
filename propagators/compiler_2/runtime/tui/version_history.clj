(ns propagators.compiler-2.runtime.tui.version-history
  "Pure append-only version history and idempotency decisions.")

(defn versions [block]
  (vec (:version-history block)))

(defn current-record [block]
  (peek (versions block)))

(defn current-version [block]
  (:version (current-record block)))

(defn next-version [block]
  (if-some [version (current-version block)]
    (inc version)
    0))

(defn commit-record
  [{:keys [commit-id client-id index expected-version source context retracts
           definitions diagnostics topology]}]
  {:commit-id commit-id
   :client-id client-id
   :index index
   :expected-version expected-version
   :version (:premise/epoch context)
   :source source
   :premise-id (:premise/id context)
   :premise-state-cell (:premise/state-cell context)
   :retracts retracts
   :topology topology
   :definitions (vec definitions)
   :diagnostics (vec diagnostics)})

(defn commit-by-id
  [block commit-id]
  (some #(when (= commit-id (:commit-id %)) %) (versions block)))

(defn same-request?
  [record {:keys [commit-id client-id index expected-version text]}]
  (= [(:commit-id record)
      (:client-id record)
      (:index record)
      (:expected-version record)
      (:source record)]
     [commit-id client-id index expected-version text]))

(defn decision
  [block request]
  (if-let [record (commit-by-id block (:commit-id request))]
    (if (same-request? record request)
      {:decision :replay :record record}
      {:decision :collision :record record})
    (if (= (current-version block) (:expected-version request))
      {:decision :append :version (next-version block)}
      {:decision :stale
       :expected (:expected-version request)
       :current (current-version block)})))

(defn append-version
  [block record]
  (-> block
      (update :version-history (fnil conj []) record)
      (assoc :epoch (:version record)
             :text-current (:source record)
             :text-current-source? true)))
