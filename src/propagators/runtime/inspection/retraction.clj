(ns propagators.runtime.inspection.retraction
  "One-shot activation observation for premise-versioned block commits."
  (:require [propagators.infra.cells.cell :as cell]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.compiler.basis :as basis]
            [propagators.compiler.model.application-value :as application]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.runtime.session.block-model :as block-model]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.tms.distributed :as tms]
            [propagators.infra.graph :as graph]
            [propagators.infra.helpers.task-queue :as task-queue]
            [propagators.infra.network :as net]
            [propagators.infra.propagator :as prop]
            [clojure.string :as str]))

(def max-events 10000)

(defn- cell-value
  [network id]
  (let [entry (get (net/net-env network) id)]
    (if (cell/cell? entry) (cell/cell-strongest entry) ::missing)))

(defn- values-changed?
  [before after]
  (not (value/cell-value-equal? before after)))

(defn- topology-size
  [network]
  {:nodes (count (net/net-graph network))
   :entries (count (net/net-env network))})

(defn- activation-event
  [before after prop-id tasks elapsed-ns error]
  (let [node (graph/get-node (net/net-graph before) prop-id)
        propagator (get (net/net-env before) prop-id)
        inputs (vec (graph/node-input-ids node))
        outputs (vec (graph/node-output-ids node))
        changed (filterv #(values-changed? (cell-value before %)
                                           (cell-value after %))
                         outputs)]
    (cond-> {:propagator/id prop-id
             :propagator/name (when (prop/prop? propagator)
                                (prop/prop-name propagator))
             :input-ids inputs
             :output-ids outputs
             :output-changed-ids changed
             :topology-before (topology-size before)
             :topology-after (topology-size after)
             :queued-after (if (task-queue/task-queue? tasks)
                             (count (:task-queue/q tasks))
                             (count tasks))
             :elapsed-ns elapsed-ns}
      error (assoc :error (str (class error))))))

(defn capture-commit
  "Run `f` unchanged while observing propagator activations on this thread."
  [before-network f]
  (let [owner (Thread/currentThread)
        original core/eval-propagator
        events (atom [])
        totals (atom {:activations 0 :elapsed-ns 0 :truncated? false})
        observed
        (fn [prop-id tasks network]
          (if-not (identical? owner (Thread/currentThread))
            (original prop-id tasks network)
            (let [started (System/nanoTime)
                  outcome (try
                            {:result (original prop-id tasks network)}
                            (catch Throwable t {:error t}))
                  elapsed (- (System/nanoTime) started)
                  error (:error outcome)
                  [next-tasks next-network] (or (:result outcome)
                                                [tasks network])
                  event (activation-event network next-network prop-id
                                          next-tasks elapsed error)]
              (swap! totals (fn [summary]
                              (-> summary
                                  (update :activations inc)
                                  (update :elapsed-ns + elapsed)
                                  (assoc :truncated?
                                         (>= (:activations summary)
                                             max-events)))))
              (when (< (count @events) max-events)
                (swap! events conj event))
              (if error
                (throw error)
                [next-tasks next-network]))))]
    (with-redefs [core/eval-propagator observed]
      {:result (f)
       :profile (assoc @totals
                       :events @events
                       :before-network before-network)})))

(defn- premise-active?
  [network premise-id state-cell]
  (let [content (when (contains? (net/net-env network) state-cell)
                  (net/network-cell-content network state-cell))]
    (boolean
     (and (tms/distributed-value? content)
          (contains? (-> content tms/distributed-slots tms/tms-view
                         tms/active-premises)
                     premise-id)))))

(defn- context-row
  [network source {:premise/keys [id state-cell epoch]}]
  {:premise/id id
   :premise/state-cell state-cell
   :premise/epoch epoch
   :premise/active? (premise-active? network id state-cell)
   :source source})

(defn- record-context
  [network record]
  (context-row network
               {:kind :block-version
                :client-id (:client-id record)
                :block-index (:index record)
                :version (:version record)}
               {:premise/id (:premise-id record)
                :premise/state-cell (:premise-state-cell record)
                :premise/epoch (:version record)}))

(defn- application-row
  [network record ordinal application-id]
  (let [info (cell-value network application-id)]
    {:application/id application-id
     :application/ordinal ordinal
     :source {:kind :block-version
              :client-id (:client-id record)
              :block-index (:index record)
              :version (:version record)}
     :active? (:premise/active? (record-context network record))
     :operator-ast (when (application/application-info? info)
                     (obj/slot-value info application/application-operator-ast-slot))
     :operator-cell (when (application/application-info? info)
                      (obj/slot-value info application/application-operator-cell-slot))
     :arg-cells (when (application/application-info? info)
                  (obj/slot-value info application/application-arg-cells-slot))
     :output-cell (when (application/application-info? info)
                    (obj/slot-value info application/application-output-slot))
     :lowering (when (application/application-info? info)
                 (obj/slot-value info application/application-lowering-slot))}))

(defn- block-application-rows
  [network records]
  (mapv identity
        (mapcat (fn [record]
                  (map-indexed
                   #(application-row network record %1 %2)
                   (get-in record [:topology :application-ids])))
                records)))

(defn- candidate-application-id
  [call-id candidate-id]
  (basis/stable-node-id :compiler-2 :versioned-definition-call
                        call-id candidate-id :application))

(defn- candidate-application-rows
  [network]
  (let [candidates (net/network-dict-entry network definition/candidates-key)]
    (mapv
     (fn [[[call-id candidate-id] _call]]
       (let [candidate (get candidates candidate-id)
             context (:candidate/context candidate)]
         {:application/id (candidate-application-id call-id candidate-id)
          :source {:kind :definition-candidate
                   :name (:candidate/name candidate)
                   :block-id (:candidate/block-id candidate)
                   :version (:candidate/version candidate)}
          :active? (and context
                        (premise-active? network
                                         (:premise/id context)
                                         (:premise/state-cell context)))
          :operator-ast (:candidate/name candidate)}))
     (net/network-dict-entry network definition/calls-key))))

(defn- all-contexts
  [network records]
  (let [block-contexts (map #(record-context network %) records)
        candidate-contexts
        (map (fn [candidate]
               (context-row network
                            {:kind :definition-candidate
                             :name (:candidate/name candidate)
                             :version (:candidate/version candidate)}
                            (:candidate/context candidate)))
             (vals (net/network-dict-entry network definition/candidates-key)))]
    (into {} (map (juxt :premise/state-cell identity))
          (concat block-contexts candidate-contexts))))

(defn- changed-cell-ids
  [before after]
  (let [ids (into (set (keys (net/net-env before)))
                  (keys (net/net-env after)))]
    (into #{} (filter #(values-changed? (cell-value before %)
                                        (cell-value after %))) ids)))

(defn- enrich-event
  [owners contexts changed event]
  (let [owner (some owners (:input-ids event))
        premises (keep contexts (:input-ids event))]
    (cond-> (assoc event
                   :transaction-changed-inputs
                   (filterv changed (:input-ids event))
                   :premises (vec premises))
      owner (assoc :owner owner))))

(defn- inactive-event?
  [{:keys [owner premises]}]
  (or (and owner (false? (:active? owner)))
      (and (seq premises)
           (not-every? :premise/active? premises))))

(defn- productive-event?
  [{:keys [output-changed-ids topology-before topology-after]}]
  (or (seq output-changed-ids)
      (not= topology-before topology-after)))

(defn classify-report
  [report]
  (let [events (get-in report [:activations :events])
        inactive (filterv inactive-event? events)]
    (assoc report
           :inactive-wakes inactive
           :hibernation-candidates
           (filterv #(not (productive-event? %)) inactive)
           :unowned-activations (filterv #(nil? (:owner %)) events)
           :activation-counts (frequencies (map :propagator/name events)))))

(defn report-summary
  [report]
  (let [target (:target report)]
    (str/join
     "\n"
     [(str "retraction profile " (:client-id target) "/block "
           (:block-index target) "/version " (get-in report [:commit :version]))
      (str "activations: " (get-in report [:activations :activations] 0))
      (str "inactive wakes: " (count (:inactive-wakes report)))
      (str "hibernation candidates: "
           (count (:hibernation-candidates report)))
      (str "details truncated: "
           (boolean (get-in report [:activations :truncated?])))])))

(defn build-report
  [state request receipt {:keys [before-network events] :as profile}]
  (let [network (:program/net state)
        block (block-model/block-by-index state (:client-id request)
                                          (:index request))
        records (vec (:version-history block))
        applications (into (block-application-rows network records)
                           (candidate-application-rows network))
        owners (into {} (map (juxt :application/id identity)) applications)
        contexts (all-contexts network records)
        changed (changed-cell-ids before-network network)]
    (let [report
          (classify-report
           {:inspection/type :retraction-profile
            :target {:client-id (:client-id request)
                     :block-index (:index request)}
            :commit (select-keys receipt [:commit-id :version :premise-id])
            :source (:text request)
            :applications applications
            :activations
            (-> profile
                (dissoc :events :before-network)
                (assoc :changed-cell-ids (vec changed)
                       :events (mapv #(enrich-event owners contexts changed %)
                                     events)))})]
      (assoc report :summary (report-summary report)))))

(defn- context-active?
  [network {:premise/keys [id state-cell]}]
  (premise-active? network id state-cell))

(defn probe-active?
  [state probe]
  (let [contexts (:inspection/contexts probe)]
    (or (empty? contexts)
        (every? #(context-active? (:program/net state) %) contexts))))

(defn prune-probes
  [state]
  (update state :inspection/probes
          (fn [probes]
            (into {} (filter (comp #(probe-active? state %) val)) probes))))

(defn matching-probes
  [state {:keys [client-id index]}]
  (->> (:inspection/probes state)
       vals
       (filter #(and (= client-id (:inspection/client-id %))
                     (= index (:inspection/block-index %))))
       vec))

(defn consume-probes
  [state probes]
  (update state :inspection/probes
          #(apply dissoc % (map :inspection/id probes))))

(defn record-probe
  [state request]
  (let [probe-id (:boundary/id request)]
    (if (contains? (:inspection/seen-requests state) probe-id)
      state
      (let [{:keys [instance-id block-index contexts]} (:boundary/payload request)
            client-id (some (fn [[client-id tui]]
                              (when (= instance-id (:instance-id tui)) client-id))
                            (:tuis state))
            report-cell (get-in request [:boundary/target :cell-id])
            valid? (and client-id
                        (block-model/block-by-index state client-id block-index))]
        (cond-> (update state :inspection/seen-requests (fnil conj #{}) probe-id)
          valid?
          (assoc-in [:inspection/probes probe-id]
                    {:inspection/id probe-id
                     :inspection/client-id client-id
                     :inspection/block-index block-index
                     :inspection/report-cell report-cell
                     :inspection/contexts (vec contexts)}))))))
