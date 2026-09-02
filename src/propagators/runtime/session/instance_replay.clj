(ns propagators.runtime.session.instance-replay
  "Export and atomically replay versioned compiler-2 runtime instances."
  (:require [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.annotations :as annotations]
            [propagators.runtime.session.clients :as tui-session]
            [propagators.runtime.session.versioned-commit :as versioned-commit]
            [propagators.infra.datastructures.tms.distributed :as tms]
            [propagators.infra.network :as net]
            [propagators.infra.propagator :as prop]))

(def schema "compiler-2/versioned-instance")
(def schema-version 1)

(defn versioned-client?
  [tui]
  (= :versioned-premise (:mode tui)))

(defn client-manifest
  [tui]
  {:client-id (:client-id tui)
   :mode :versioned-premise})

(defn versioned-clients
  [runtime-state]
  (->> (:tuis runtime-state)
       vals
       (filter versioned-client?)
       (sort-by :client-id)
       (mapv client-manifest)))

(defn topology-counts
  [network]
  (let [nodes (vals (net/net-env network))]
    {:cells (count (remove prop/prop? nodes))
     :propagators (count (filter prop/prop? nodes))}))

(defn tms-summary
  [content]
  (when (tms/distributed-value? content)
    (let [view (tms/tms-view (tms/distributed-slots content))]
      {:active-premises (mapv pr-str (sort-by pr-str
                                              (tms/active-premises view)))
       :active-claim-count (count (tms/active-claims view))
       :inactive-claim-count (count (:tms/inactive-claims view))})))

(defn display-snapshot
  [runtime-state client-id block]
  (let [display-id (:display-id block)
        program-net (:program/net runtime-state)
        content (when (contains? (net/net-env program-net) display-id)
                  (net/network-cell-content program-net display-id))]
    (when-let [summary (tms-summary content)]
      (merge {:client-id client-id
              :index (:index block)
              :display-id (pr-str display-id)
              :strongest (annotations/project-value
                          (tui-session/block-view-value runtime-state block))}
             summary))))

(defn display-snapshots
  [runtime-state]
  (->> (:tuis runtime-state)
       vals
       (filter versioned-client?)
       (mapcat (fn [tui]
                 (keep #(display-snapshot runtime-state (:client-id tui) %)
                       (:blocks tui))))
       vec))

(def compact-effect-keys
  [:boundary/id :boundary/port :boundary/kind :boundary/target
   :boundary/payload :boundary/epoch])

(defn compact-effect
  [request]
  (select-keys request compact-effect-keys))

(defn snapshot
  [runtime-state]
  {:views (mapv #(tui-session/read-tui-view
                  runtime-state {:client-id (:client-id %)})
                (versioned-clients runtime-state))
   :runtime-errors (vec (:runtime/errors runtime-state))
   :external-inputs {:count (count (:runtime/inputs runtime-state))
                     :inputs (vec (:runtime/inputs runtime-state))}
   :outbox {:effects (mapv compact-effect
                           (effects/outbox-effects (:program/net runtime-state)))}
   :displays (display-snapshots runtime-state)
   :topology (topology-counts (:program/net runtime-state))})

(defn export-instance
  [runtime-state]
  (let [runtime-state (or runtime-state (state/empty-state))]
    {:schema schema
     :schema-version schema-version
     :clients (versioned-clients runtime-state)
     :commits (vec (:versioned/commit-log runtime-state))
     :snapshot (snapshot runtime-state)}))

(defn normalize-mode
  [mode]
  (if (string? mode) (keyword mode) mode))

(defn normalize-client
  [{:keys [client-id mode]}]
  {:client-id client-id
   :mode (normalize-mode mode)})

(defn normalize-commit
  [commit]
  (into {}
        (map (fn [k] [k (get commit k)]))
        versioned-commit/replay-commit-keys))

(defn validate-clients!
  [clients]
  (when-not (vector? clients)
    (throw (ex-info "instance clients must be an array" {:clients clients})))
  (let [clients (mapv normalize-client clients)
        ids (mapv :client-id clients)]
    (when-not (= (count ids) (count (set ids)))
      (throw (ex-info "instance clients must be unique" {:client-ids ids})))
    (doseq [{:keys [client-id mode] :as client} clients]
      (tui-session/validate-client-id! client-id)
      (when-not (= :versioned-premise mode)
        (throw (ex-info "instance client must use versioned-premise mode"
                        {:client client}))))
    clients))

(defn validate-commits!
  [commits client-ids]
  (when-not (vector? commits)
    (throw (ex-info "instance commits must be an array" {:commits commits})))
  (mapv
   (fn [commit]
     (let [commit (normalize-commit commit)]
       (versioned-commit/validate-request! commit)
       (when-not (contains? client-ids (:client-id commit))
         (throw (ex-info "instance commit references an unregistered client"
                         {:commit commit})))
       (when-not (or (nil? (:expected-version commit))
                     (integer? (:expected-version commit)))
         (throw (ex-info "expected-version must be an integer or null"
                         {:commit commit})))
       commit))
   commits))

(defn validate-manifest!
  [manifest]
  (when-not (map? manifest)
    (throw (ex-info "instance manifest must be an object" {:manifest manifest})))
  (when-not (= schema (:schema manifest))
    (throw (ex-info "unsupported instance schema"
                    {:expected schema :actual (:schema manifest)})))
  (when-not (= schema-version (:schema-version manifest))
    (throw (ex-info "unsupported instance schema version"
                    {:expected schema-version
                     :actual (:schema-version manifest)})))
  (let [clients (validate-clients! (:clients manifest))
        commits (validate-commits! (:commits manifest)
                                   (set (map :client-id clients)))]
    {:schema schema
     :schema-version schema-version
     :clients clients
     :commits commits}))

(defn versioned-history-empty?
  [runtime-state]
  (and (empty? (:versioned/commit-log runtime-state))
       (every? (fn [client]
                 (every? (comp empty? :version-history)
                         (get-in runtime-state
                                 [:tuis (:client-id client) :blocks])))
               (versioned-clients runtime-state))))

(defn client-set
  [clients]
  (set (map (juxt :client-id :mode) clients)))

(defn compatible-empty-clients?
  [current-clients manifest-clients]
  (every? (client-set manifest-clients)
          (client-set current-clients)))

(defn register-client!
  [candidate {:keys [client-id mode]}]
  (if-let [existing (get-in @candidate [:tuis client-id])]
    (when-not (= mode (:mode existing))
      (throw (ex-info "instance client mode collision"
                      {:client-id client-id
                       :existing-mode (:mode existing)
                       :manifest-mode mode})))
    (tui-session/register-tui! candidate {:client-id client-id :mode mode})))

(defn replay-step!
  [candidate commit]
  (tui-session/ensure-block-index! candidate (:client-id commit) (:index commit))
  (let [receipt (versioned-commit/commit-version! candidate commit)
        compiled-result (get-in @candidate
                                [:program/results
                                 [(:client-id commit) (:index commit)]
                                 :result])]
    {:receipt receipt
     :compiled-result (annotations/project-value compiled-result)
     :view (tui-session/read-tui-view @candidate
                                     {:client-id (:client-id commit)})}))

(defn exact-history?
  [runtime-state manifest]
  (and (= (vec (:versioned/commit-log runtime-state)) (:commits manifest))
       (= (client-set (versioned-clients runtime-state))
          (client-set (:clients manifest)))))

(defn import-instance!
  [session manifest]
  (let [manifest (validate-manifest! manifest)]
    (locking session
      (let [current (or @session (state/empty-state))]
        (cond
          (exact-history? current manifest)
          {:status :replayed
           :commit-count (count (:commits manifest))
           :manifest (export-instance current)}

          (not (versioned-history-empty? current))
          (throw (ex-info "instance history diverges from manifest"
                          {:current-commits (:versioned/commit-log current)
                           :manifest-commits (:commits manifest)}))

          (not (compatible-empty-clients? (versioned-clients current)
                                          (:clients manifest)))
          (throw (ex-info "empty instance clients diverge from manifest"
                          {:current-clients (versioned-clients current)
                           :manifest-clients (:clients manifest)}))

          :else
          (let [candidate (atom current)]
            (doseq [client (:clients manifest)]
              (register-client! candidate client))
            (let [steps (mapv #(replay-step! candidate %) (:commits manifest))
                  imported @candidate]
              (reset! session imported)
              {:status :imported
               :commit-count (count (:commits manifest))
               :steps steps
               :manifest (export-instance imported)})))))))
