(ns propagators.runtime.session.clients
  "TUI session and block operations for compiler-2 runtime."
  (:require [clojure.string :as str]
            [propagators.runtime.session.block-model :as block-model]
            [propagators.runtime.session.input :as input]
            [propagators.runtime.session.program :as program]
            [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.temperature :as temperature]
            [propagators.runtime.inspection.annotations :as annotations]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.infra.datastructures.tms :as tms]
            [propagators.infra.gur.flat :as fvm]
            [propagators.infra.core :as core]
            [propagators.infra.ids :as ids]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]))

(def stable-node-id state/stable-node-id)
(def ensure-session-state! state/ensure-session-state!)
(def require-state state/require-state)
(def valid-client-id? block-model/valid-client-id?)
(def block-by-index block-model/block-by-index)
(def update-block block-model/update-block)
(def assign-source-order block-model/assign-source-order)
(def block-text block-model/block-text)
(def read-source-forms program/read-source-forms)
(def install-block-slots program/install-block-slots)
(def install-instance-slots program/install-instance-slots)
(def rebuild-program! input/rebuild-program!)
(def install-block-incremental! input/install-block-incremental!)
(def seed-appended-block-topology! input/seed-appended-block-topology!)

(declare append-tui-block! prepare-block-targets! read-tui-view)

(defn validate-client-id!
  [client-id]
  (when-not client-id
    (throw (ex-info "missing client-id" {})))
  (when-not (valid-client-id? client-id)
    (throw (ex-info "invalid client-id for compiler env binding"
                    {:client-id client-id}))))

(defn create-tui
  ([client-id] (create-tui client-id :legacy))
  ([client-id mode]
   (let [view-id (stable-node-id :tui client-id :view)
         instance-id (stable-node-id :tui client-id :instance)
         blocks-id (stable-node-id :tui client-id :blocks)]
     {:client-id client-id, :mode mode, :view-id view-id, :instance-id instance-id,
      :blocks-id blocks-id, :head-id nil, :tail-id nil, :next-index 0,
      :client-count 0, :blocks []})))

(defn install-tui-instance
  [network {:keys [view-id instance-id blocks-id]}]
  (-> network
      (nb/ensure-cell view-id)
      (nb/install-cell instance-id instance-id instance-id)
      (nb/ensure-cell blocks-id)
      (install-instance-slots {:instance-id instance-id
                               :blocks-id blocks-id})))

(defn ensure-tui!
  ([session client-id] (ensure-tui! session client-id nil))
  ([session client-id requested-mode]
   (validate-client-id! client-id)
   (let [state (ensure-session-state! session)]
     (if-let [tui (get-in state [:tuis client-id])]
       (do
         (when (and requested-mode (not= requested-mode (:mode tui :legacy)))
           (throw (ex-info "TUI client mode mismatch"
                           {:client-id client-id
                            :existing-mode (:mode tui :legacy)
                            :requested-mode requested-mode})))
         tui)
        (let [tui (create-tui client-id (or requested-mode :legacy))
              n (install-tui-instance (:network state) tui)]
          (swap! session #(-> %
                              (assoc :network n)
                              (assoc-in [:tuis client-id] tui)))
          tui)))))

(defn register-tui!
  [session {:keys [client-id mode]}]
  (let [mode (or mode :legacy)
        tui (ensure-tui! session client-id mode)]
    (swap! session update-in [:tuis client-id :client-count] (fnil inc 0))
    (when (and (= :versioned-premise mode) (empty? (:blocks tui)))
      (append-tui-block! session {:client-id client-id :rebuild? false}))
    (let [tui (get-in @session [:tuis client-id])]
      {:client-id (:client-id tui), :view-id (pr-str (:view-id tui)),
       :mode (:mode tui :legacy)
       :next-index (:next-index tui)})))

(defn tui!
  [session client-id]
  (ensure-tui! session client-id)
  (get-in @session [:tuis client-id]))

(defn append-tui-block!
  [session {:keys [client-id text rebuild?] :or {rebuild? true} :as command}]
  (let [tui (tui! session client-id)
        index (:next-index tui)
        has-text? (contains? command :text)
        block {:block-id (ids/new-node-id), :index-id (ids/new-node-id),
               :text-id (ids/new-node-id), :display-id (ids/new-node-id),
               :next-id (ids/new-node-id), :index index, :epoch 0}
        block (cond-> block
                has-text? (assoc :text-current text
                                 :text-current-source? true))
        state @session
        previous-tail (peek (:blocks tui))
        n0 (-> (:network state)
               (nb/install-cell (:block-id block))
               (nb/install-cell (:index-id block) index index)
               ((if has-text?
                  #(nb/install-cell % (:text-id block) text text)
                  #(nb/install-cell % (:text-id block))))
               (nb/install-cell (:display-id block))
               (nb/install-cell (:next-id block)))
        n1 (install-block-slots n0 block)
        messages (cond-> []
                   (nil? (:head-id tui)) (conj (message (:view-id tui)
                                                        (:block-id block))
                                                   (message (:blocks-id tui)
                                                            (:block-id block)))
                   (:tail-id tui) (conj (message (:next-id (peek (:blocks tui)))
                                                 (:block-id block))))
        [tasks n2] (core/eval-cells messages n1)
        [state' n3] (temperature/run-tasks state
                                           :propagation/tui-append
                                           tasks
                                           n2)
        tui' (-> tui
                 (assoc :head-id (or (:head-id tui) (:block-id block))
                        :tail-id (:block-id block))
                 (update :next-index inc)
                 (update :blocks conj block))]
    (swap! session #(-> %
                        (merge (select-keys state' [:runtime]))
                        (assoc :network n3)
                        (assoc-in [:tuis client-id] tui')))
    (seed-appended-block-topology! session client-id
                                   (assoc block :client-id client-id)
                                   previous-tail)
    (when has-text?
      (prepare-block-targets! session client-id index text)
      (swap! session assign-source-order client-id index))
    (when rebuild?
      (if has-text?
        (let [block' (assoc (block-by-index @session client-id index)
                            :client-id client-id)]
          (install-block-incremental! session block'))
        (rebuild-program! session)))
    {:client-id client-id, :index index,
     :block-id (pr-str (:block-id block)),
     :text-id (pr-str (:text-id block)),
     :display-id (pr-str (:display-id block))}))

(defn next-view-block-index
  [view]
  (or (some->> (:blocks view)
               (map :index)
               seq
               (apply max)
               inc)
      0))

(defn input-view-block-index
  [view]
  (let [blocks (:blocks view)
        last-block (peek blocks)]
    (if (and last-block
             (= value/nothing (:value last-block))
             (not (:referenced? last-block)))
      (:index last-block)
      (next-view-block-index view))))

(defn block-targets
  [source]
  (try
    (let [forms (read-source-forms source)]
      (into []
            (keep (fn [x]
                    (when (seq? x)
                      (case (first x)
                        block (let [[_ index] x]
                                (when (integer? index)
                                  {:index index :strict-past? true}))
                        be:block (let [[_ index] x]
                                   (when (integer? index)
                                     {:index index :strict-past? true}))
                        block-at (let [[_ _ index] x]
                                   (when (integer? index)
                                     {:index index :strict-past? false}))
                        be:block-at (let [[_ _ index] x]
                                      (when (integer? index)
                                        {:index index :strict-past? false}))
                        nil))))
            (mapcat #(tree-seq coll? seq %) forms)))
    (catch Throwable _
      [])))

(defn block-target-indexes
  [source]
  (set (map :index (block-targets source))))

(defn empty-block? [state block]
  (let [v (block-text state block)]
    (or (value/nothing? v)
        (and (string? v) (str/blank? v)))))

(defn ensure-block-index! [session client-id index]
  (loop []
    (let [tui (tui! session client-id)]
      (when (<= (:next-index tui) index)
        (append-tui-block! session {:client-id client-id
                                    :rebuild? false})
        (recur)))))

(defn prepare-block-targets!
  [session client-id source-index source]
  (doseq [{:keys [index strict-past?]} (sort-by :index (block-targets source))]
    (let [state @session
          target-block (block-by-index state client-id index)]
      (cond
        (and target-block
             strict-past?
             (< index source-index)
             (not (empty-block? state target-block)))
        (throw (ex-info "block target points to a non-empty past block"
                        {:client-id client-id
                         :source-index source-index
                         :target-index index}))

        (nil? target-block)
        (ensure-block-index! session client-id index)))))

(defn edit-tui-block!
  [session {:keys [client-id index text]}]
  (let [tui (tui! session client-id)
        block0 (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block0
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (prepare-block-targets! session client-id index text)
    (let [block block0
          new-epoch (inc (or (:epoch block) 0))
          _ (swap! session update-block client-id index
                   #(assoc % :epoch new-epoch
                             :text-current text
                             :text-current-source? true))
          [tasks n1] (core/eval-cells [(message (:text-id block) text)]
                                      (:network @session))
          [state' n2] (temperature/run-tasks @session
                                             :propagation/tui-edit
                                             tasks
                                             n1)]
      (swap! session #(-> %
                          (merge (select-keys state' [:runtime]))
                          (assoc :network n2)))
      (when-not (some? (:order block))
        (swap! session assign-source-order client-id index))
      (rebuild-program! session)
      {:client-id client-id, :index index})))

(defn submit-tui-block!
  [session {:keys [client-id text]}]
  (let [view (read-tui-view @session {:client-id client-id})
        current-index (input-view-block-index view)
        has-current? (= current-index (some-> (:blocks view) peek :index))]
    (when-not has-current?
      (append-tui-block! session {:client-id client-id :rebuild? false}))
    (append-tui-block! session {:client-id client-id :rebuild? false})
    (edit-tui-block! session {:client-id client-id
                              :index current-index
                              :text text})
    (let [view (read-tui-view @session {:client-id client-id})]
      (when-not (and (seq (:blocks view))
                     (= value/nothing (-> view :blocks peek :value))
                     (not (-> view :blocks peek :referenced?)))
        (append-tui-block! session {:client-id client-id :rebuild? false})))
    (read-tui-view @session {:client-id client-id})))

(defn block-value
  [state block]
  (block-text state block))

(defn block-display-network
  "Choose the network that owns the block's strongest display value.

  Compiler output is declarative topology in :program/net. Legacy boundary
  display writes still land in :network, so they remain the fallback."
  [state block]
  (let [client-id (:client-id (block-model/block-by-display-id
                               state (:display-id block)))
        versioned? (= :versioned-premise
                      (get-in state [:tuis client-id :mode]))
        program-net (:program/net state)
        display-id (:display-id block)
        program-content (when (and program-net
                                   (contains? (net/net-env program-net)
                                              display-id))
                          (net/network-cell-content program-net display-id))]
    (if (and versioned?
             program-net
             (not (value/nothing? program-content)))
      program-net
      (:network state))))

(defn block-display-value
  [state block]
  (net/network-cell-strongest (block-display-network state block)
                              (:display-id block)))

(defn block-display-content
  [state block]
  (net/network-cell-content (block-display-network state block)
                            (:display-id block)))

(defn block-view-value
  [state block]
  (let [display-content (when (nil? (:order block))
                          (block-display-content state block))]
    (if (and display-content (not (value/nothing? display-content)))
      display-content
      (block-value state block))))

(defn block-view-content
  [state block]
  (let [display-content (when (nil? (:order block))
                          (block-display-content state block))]
    (if (and display-content (not (value/nothing? display-content)))
      display-content
      (block-value state block))))

(defn referenced-block-indexes
  [state blocks]
  (reduce (fn [indexes block]
            (let [v (block-value state block)]
              (if (string? v)
                (into indexes (block-target-indexes v))
                indexes)))
          #{}
          blocks))

(defn project-tui-view
  [state {:keys [client-id]}]
  (let [tui (get-in (require-state state) [:tuis client-id])]
    (when-not tui
      (throw (ex-info "tui client not found" {:client-id client-id})))
    (let [referenced-indexes (referenced-block-indexes state (:blocks tui))
          current-versions (into {}
                                 (map (fn [block]
                                        [(:block-id block)
                                         (some-> block :version-history peek :version)]))
                                 (:blocks tui))
          diagnostics (vals (get (net/network-dict-entry
                                  (:program/net state) fvm/name-bindings-key)
                                 definition/diagnostic-scope {}))]
      {:client-id client-id
       :mode (:mode tui :legacy)
       :view-id (pr-str (:view-id tui))
       :focus (:focus tui)
       :changed-cells (mapv pr-str (:runtime/changed-cells state))
       :changed-node-ids (mapv pr-str (:runtime/changed-node-ids state))
       :errors (vec (:runtime/errors state))
       :blocks (mapv (fn [block]
                       (let [raw-value (block-view-content state block)]
                         (annotations/annotate-block-view
                          {:index (:index block)
                           :version (some-> block :version-history peek :version)
                           :source (some-> block :version-history peek :source)
                           :block-id (pr-str (:block-id block))
                           :text-id (pr-str (:text-id block))
                           :display-id (pr-str (:display-id block))
                           :warnings (->> diagnostics
                                          (mapcat identity)
                                          (filter
                                           (fn [diagnostic]
                                             (and
                                              (= (:definition-version diagnostic)
                                                 (get current-versions
                                                      (first (:definition-id diagnostic))))
                                              (every? (fn [[block-id version]]
                                                        (= version
                                                           (get current-versions block-id)))
                                                      (:caller-versions diagnostic)))))
                                          (filter #(contains? (:block-ids %)
                                                              (:block-id block)))
                                          vec)
                           :referenced? (contains? referenced-indexes
                                                   (:index block))
                           :value (annotations/project-value
                                   (block-view-value state block))}
                          raw-value)))
                     (:blocks tui))})))

(defn read-tui-view
  [state command]
  (project-tui-view state command))

(defn read-agent-blocks
  [state {:keys [client-id indexes]}]
  (let [view (project-tui-view state {:client-id client-id})
        wanted (when indexes (set indexes))]
    (cond-> view
      wanted (update :blocks #(vec (filter (fn [block]
                                             (contains? wanted (:index block)))
                                           %))))))

(defn read-agent-block
  [state {:keys [client-id index] :as command}]
  (when-not (integer? index)
    (throw (ex-info "agent block read expects integer index"
                    {:command command})))
  (let [blocks (:blocks (read-agent-blocks state {:client-id client-id
                                                  :indexes [index]}))
        projected (or (first blocks)
                      (throw (ex-info "block not found"
                                      {:client-id client-id :index index})))]
    (if-not (:detail? command)
      projected
      (let [block (block-by-index state client-id index)
            record (peek (vec (:version-history block)))
            result-cell (get-in record [:topology :result-cell])
            content (if (and result-cell
                             (contains? (net/net-env (:program/net state))
                                        result-cell))
                      (net/network-cell-content (:program/net state) result-cell)
                      (block-view-content state block))
            tms-view (when (tms/distributed-value? content)
                       (tms/tms-view (tms/distributed-slots content)))
            premise-content
            (when-let [cell (:premise-state-cell record)]
              (when (contains? (net/net-env (:program/net state)) cell)
                (net/network-cell-content (:program/net state) cell)))
            premise-view (when (tms/distributed-value? premise-content)
                           (tms/tms-view
                            (tms/distributed-slots premise-content)))
            candidates (mapv #(select-keys %
                                            [:candidate/id
                                             :candidate/definition-id
                                             :candidate/name
                                             :candidate/version
                                             :candidate/signature
                                             :candidate/context])
                             (:definitions record))]
        (assoc projected
               :premise-id (:premise-id record)
               :premise-state-cell (some-> (:premise-state-cell record) pr-str)
               :premise-active?
               (boolean
                (and premise-view
                     (contains? (tms/active-premises premise-view)
                                (:premise-id record))))
               :definition-candidates candidates
               :result-cell (some-> record :topology :result-cell pr-str)
               :application-ids
               (mapv pr-str (get-in record [:topology :application-ids]))
               :propagator-ids
               (mapv pr-str (get-in record [:topology :propagator-ids]))
               :tms {:active-claim-count
                     (if tms-view (count (tms/active-claims tms-view)) 0)
                     :inactive-claim-count
                     (if tms-view
                       (count (:tms/inactive-claims tms-view))
                       0)
                     :active-premises
                     (if tms-view (vec (tms/active-premises tms-view)) [])})))))

(defn focus-block!
  [session {:keys [client-id index request-id] :as command}]
  (validate-client-id! client-id)
  (when-not (integer? index)
    (throw (ex-info "focus-block expects an integer index" {:command command})))
  (locking session
    (ensure-session-state! session)
    (when-not (block-by-index @session client-id index)
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (let [sequence (inc (long (or (:tui/focus-seq @session) 0)))
          focus {:index index
                 :sequence sequence
                 :request-id request-id}]
      (swap! session #(-> %
                          (assoc :tui/focus-seq sequence)
                          (assoc-in [:tuis client-id :focus] focus)))
      focus)))

(defn send-agent-block!
  [session {:keys [client-id text mode] :or {mode :submit} :as command}]
  (when-not (string? text)
    (throw (ex-info "agent block send expects text"
                    {:command command})))
  (tui! session client-id)
  (case mode
    :append (append-tui-block! session {:client-id client-id
                                        :text text})
    "append" (append-tui-block! session {:client-id client-id
                                         :text text})
    :submit (submit-tui-block! session {:client-id client-id
                                        :text text})
    "submit" (submit-tui-block! session {:client-id client-id
                                         :text text})
    (throw (ex-info "unknown agent block send mode"
                    {:mode mode
                     :command command}))))

(defn unregister-tui!
  [session {:keys [client-id]}]
  (let [remaining (atom nil)]
    (swap! session
           (fn [state]
             (let [count (get-in state [:tuis client-id :client-count] 0)
                   next-count (max 0 (dec count))
                   versioned? (= :versioned-premise
                                  (get-in state [:tuis client-id :mode]))]
               (reset! remaining next-count)
               (if (or versioned? (pos? next-count))
                 (assoc-in state [:tuis client-id :client-count] next-count)
                 (update state :tuis dissoc client-id)))))
    {:client-id client-id, :remaining-clients @remaining,
     :unregistered true}))
