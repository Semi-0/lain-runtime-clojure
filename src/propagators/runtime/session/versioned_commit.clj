(ns propagators.runtime.session.versioned-commit
  "Atomic append-only block-version commits for premise-aware TUI clients."
  (:require [clojure.string :as str]
            [propagators.runtime.session.block-model :as block-model]
            [propagators.runtime.session.block-compiler :as block-compiler]
            [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.session.input :as input]
            [propagators.runtime.session.program :as program]
            [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.temperature :as temperature]
            [propagators.runtime.inspection.retraction :as retraction]
            [propagators.runtime.session.clients :as tui-session]
            [propagators.runtime.session.version-history :as history]
            [propagators.compiler.operators.block-premise :as premise]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.infra.cells.value :as value]
            [propagators.infra.core :as core]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network-builder :as nb])
  (:import [java.util UUID]))

(defn validate-request!
  [{:keys [commit-id client-id index text] :as request}]
  (when-not (and (string? commit-id) (not (str/blank? commit-id)))
    (throw (ex-info "commit-version expects commit-id" {:request request})))
  (try
    (UUID/fromString commit-id)
    (catch IllegalArgumentException _
      (throw (ex-info "commit-id must be a UUID" {:commit-id commit-id}))))
  (tui-session/validate-client-id! client-id)
  (when-not (integer? index)
    (throw (ex-info "commit-version expects integer index" {:index index})))
  (when-not (string? text)
    (throw (ex-info "commit-version expects source text" {:text text})))
  request)

(defn- versioned-block
  [runtime-state {:keys [client-id index]}]
  (let [tui (get-in runtime-state [:tuis client-id])
        block (block-model/block-by-index runtime-state client-id index)]
    (when-not (= :versioned-premise (:mode tui))
      (throw (ex-info "commit-version requires a versioned-premise TUI"
                      {:client-id client-id :mode (:mode tui)})))
    (or block
        (throw (ex-info "block not found" {:client-id client-id
                                            :index index})))))

(defn- receipt
  [record & {:as extra}]
  (merge {:status :committed
          :commit-id (:commit-id record)
          :client-id (:client-id record)
          :index (:index record)
          :version (:version record)
          :premise-id (:premise-id record)}
         extra))

(def replay-commit-keys
  [:commit-id :client-id :index :expected-version :text])

(defn replay-commit
  [request]
  (into {}
        (map (fn [k] [k (get request k)]))
        replay-commit-keys))

(defn- committed-request
  [runtime-state commit-id]
  (some #(history/commit-by-id % commit-id)
        (block-model/all-blocks runtime-state)))

(defn- reject-decision!
  [decision request]
  (case (:decision decision)
    :collision
    (throw (ex-info "commit-id collision"
                    {:commit-id (:commit-id request)
                     :existing (:record decision)}))

    :stale
    (throw (ex-info "stale block version"
                    {:client-id (:client-id request)
                     :index (:index request)
                     :expected-version (:expected decision)
                     :current-version (:current decision)}))))

(defn- assign-commit-order
  [runtime-state client-id index]
  (if (some? (:order (block-model/block-by-index runtime-state client-id index)))
    runtime-state
    (block-model/assign-source-order runtime-state client-id index)))

(defn- ensure-next-block
  [runtime-state client-id index]
  (if (block-model/block-by-index runtime-state client-id (inc index))
    runtime-state
    (let [candidate (atom runtime-state)]
      (tui-session/ensure-block-index! candidate client-id (inc index))
      @candidate)))

(defn- update-runtime-text
  [runtime-state block text]
  (let [[tasks network] (core/eval-cells
                         [(message (:text-id block) text)]
                         (:network runtime-state))
        [runtime-state network]
     (temperature/run-tasks runtime-state
                               :propagation/tui-version-commit
                               tasks network)]
    (assoc runtime-state :network network)))

(defn semantic-source-record
  "Project one successful version commit into the portable source ledger."
  [block record]
  {:order (:order block)
   :client-id (:client-id record)
   :block-id (:block-id block)
   :index (:index record)
   :version (:version record)
   :source (:source record)
   :premise-id (:premise-id record)
   :premise-state-cell (:premise-state-cell record)
   :definitions
   (mapv #(select-keys % [:candidate/id :candidate/definition-id
                          :candidate/name :candidate/signature
                          :candidate/context :candidate/explicit])
         (:definitions record))})

(defn append-semantic-source-record
  [runtime-state block record]
  (update runtime-state :environment/source-ledger
          (fnil conj []) (semantic-source-record block record)))

(defn- advance-runtime-commit-tick
  [runtime-state]
  (let [tick (input/next-runtime-commit-tick runtime-state)]
    (-> runtime-state
        (assoc :runtime/commit-tick tick)
        (input/assoc-program-commit-tick tick))))

(defn- refresh-subscribed-semantic-graph
  [runtime-state]
  (if (seq (:trace/subscriptions runtime-state))
    (program/refresh-semantic-graph runtime-state)
    runtime-state))

(defn- compile-version
  [runtime-state block context source]
  (let [runtime-state (if (or (program/trace-form? source)
                              (seq (:trace/subscriptions runtime-state)))
                        (program/refresh-semantic-graph runtime-state)
                        runtime-state)
        runtime-state (-> runtime-state
                          (update :program/net nb/ensure-cell
                                  (:premise/state-cell context))
                          (input/apply-program-updates
                           [{:cell-id (:premise/state-cell context)
                             :update (premise/active-update context)}]
                           block-compiler/settle-current-props))
        compiled (program/compile-program-form
                  runtime-state
                  (assoc block :epoch (:premise/epoch context))
                  (:premise/epoch context)
                  source
                  {:compiler block-compiler/default-compiler
                   :fixed-scope? true
                   :premise-context context
                   :block/premise-context context
                   :application-settler block-compiler/settle-current-props
                   :semantic-graph-projector
                   block-compiler/deferred-semantic-graph
                   :reuse-existing-bindings? false})
        result-key [(:client-id block) (:index block)]
        error (get-in compiled [:program/results result-key :error])]
    (when error
      (throw (ex-info "block version compilation failed"
                      (get-in compiled [:program/results result-key]))))
    compiled))

(defn- record-context [record]
  (when record
    {:premise/id (:premise-id record)
     :premise/epoch (:version record)
     :premise/state-cell (:premise-state-cell record)}))

(defn- candidate-context-updates [candidates active? next-version]
  (mapv (fn [candidate]
          (let [context (:candidate/context candidate)]
            {:cell-id (:premise/state-cell context)
             :update (if active?
                       (premise/active-update context)
                       (premise/retract-update context next-version))}))
        candidates))

(defn- explicit-context-updates [candidates active? next-version]
  (->> candidates
       (keep :candidate/explicit-context)
       (mapv (fn [context]
               {:cell-id (:premise/state-cell context)
                :update (if active?
                          (premise/active-update context)
                          (premise/retract-update context next-version))}))))

(defn- lifecycle-updates
  [network context previous-record block-id next-version]
  (let [new-candidates
        (definition/candidates-for-block-version network block-id next-version)
        old-candidates
        (if previous-record
          (definition/candidates-for-block-version
           network block-id (:version previous-record))
          [])]
    {:candidates new-candidates
     :updates
     (vec
      (concat
       (when-let [previous-context (record-context previous-record)]
         [{:cell-id (:premise/state-cell previous-context)
           :update (premise/retract-update previous-context next-version)}])
       (candidate-context-updates old-candidates false next-version)
       (explicit-context-updates old-candidates false next-version)
       [{:cell-id (:premise/state-cell context)
         :update (premise/active-update context)}]
       (candidate-context-updates new-candidates true next-version)
       (explicit-context-updates new-candidates true next-version)))}))

(defn commit-version-state
  "Pure state transition except for compiler construction; boundary effects are
  deliberately excluded so callers can publish the state first."
  [runtime-state request]
  (validate-request! request)
  (let [block (versioned-block runtime-state request)
        existing (committed-request runtime-state (:commit-id request))
        decision (if existing
                   (if (history/same-request? existing request)
                     {:decision :replay :record existing}
                     {:decision :collision :record existing})
                   (history/decision block request))]
    (case (:decision decision)
      :replay {:state runtime-state
               :receipt (receipt (:record decision) :replayed? true)}

      :collision (reject-decision! decision request)
      :stale (reject-decision! decision request)

      :append
      (let [runtime-state (assign-commit-order runtime-state
                                               (:client-id request)
                                               (:index request))
            runtime-state (ensure-next-block runtime-state
                                             (:client-id request)
                                             (:index request))
            block (assoc (versioned-block runtime-state request)
                         :client-id (:client-id request))
            version (:version decision)
            context (premise/premise-context (:client-id block)
                                             (:block-id block)
                                             version)
            previous (history/current-record block)
            compiled (compile-version runtime-state block context (:text request))
            lifecycle (lifecycle-updates (:program/net compiled) context previous
                                         (:block-id block) version)
            compiled (input/apply-program-updates
                      compiled (:updates lifecycle)
                      block-compiler/settle-current-props)
            compilation (:compiled compiled)
            record (history/commit-record
                    {:commit-id (:commit-id request)
                     :client-id (:client-id request)
                     :index (:index request)
                     :expected-version (:expected-version request)
                     :source (:text request)
                     :context context
                     :retracts (:premise-id previous)
                     :topology {:propagator-ids (vec (:props compilation))
                                :application-ids
                                (vec (:applications compilation))
                                :result-cell (:cell compilation)}
                     :definitions (:candidates lifecycle)})
            committed (-> compiled
                          (block-model/update-block
                           (:client-id request) (:index request)
                           #(history/append-version % record))
                          (update :versioned/commit-log
                                  (fnil conj [])
                                  (replay-commit request))
                          (append-semantic-source-record block record)
                          (update-runtime-text block (:text request))
                          advance-runtime-commit-tick
                          refresh-subscribed-semantic-graph)]
        {:state committed
         :receipt (receipt record)}))))

(defn- trailing-blank?
  [runtime-state client-id]
  (let [block (peek (:blocks (tui-session/read-tui-view
                             runtime-state {:client-id client-id})))]
    (and block
         (nil? (:source block))
         (value/nothing? (:value block))
         (not (:referenced? block)))))

(defn- publish-profile-reports
  [runtime-state request receipt probes profile]
  (let [report (retraction/build-report runtime-state request receipt profile)
        updates (mapv (fn [probe]
                        {:cell-id (:inspection/report-cell probe)
                         :update (assoc report :probe/id (:inspection/id probe))})
                      probes)
        consumed (retraction/consume-probes runtime-state probes)]
    (try
      (input/apply-program-updates consumed updates
                                   block-compiler/settle-current-props)
      (catch Throwable t
        (state/append-runtime-error
         consumed
         (state/runtime-error-entry {:phase :inspection/report-publication}
                                    t))))))

(defn commit-version!
  [session request]
  (let [result
        (locking session
          (state/ensure-session-state! session)
          (let [runtime-state (retraction/prune-probes @session)
                probes (retraction/matching-probes runtime-state request)
                captured (if (seq probes)
                           (retraction/capture-commit
                            (:program/net runtime-state)
                            #(commit-version-state runtime-state request))
                           {:result (commit-version-state runtime-state request)})
                {next-state :state receipt :receipt :as result}
                (:result captured)
                profile (:profile captured)
                next-state (if (and profile
                                    (not (:replayed? receipt)))
                             (publish-profile-reports next-state request receipt
                                                      probes profile)
                             next-state)]
            (when-not (:replayed? receipt)
              ;; Publish history/topology before crossing the effect boundary.
              (reset! session next-state)
              (reset! session (effects/perform-boundary-effects @session)))
            result))]
    (when (and (= :committed (get-in result [:receipt :status]))
               (not (get-in result [:receipt :replayed?]))
               (not (trailing-blank? @session (:client-id request))))
      (tui-session/append-tui-block! session
                                     {:client-id (:client-id request)
                                      :rebuild? false}))
    (:receipt result)))
