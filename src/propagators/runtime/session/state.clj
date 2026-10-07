(ns propagators.runtime.session.state
  "Shared compiler-2 runtime state primitives."
  (:require [propagators.runtime.boundary :as boundary]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.runtime.inspection.semantic-support :as demo]
            [propagators.runtime.session.extension :as extension]
            [propagators.infra.cells.cell-protocol :as cell-protocol]
            [propagators.compiler.compiler.basis :as compiler-helpers]
            [propagators.compiler.model.env :as compiler-env]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.network-cache :as network-cache]
            [propagators.infra.runner :as runner])
  (:import [java.util.concurrent Executors]))

(defn empty-graph []
  {:nodes {} :edges [] :values {} :expansions {}})

(defn new-session
  ([]
   (new-session {}))
  ([options]
   (if (map? options)
     (atom {:runtime/options options})
     (throw (ex-info "runtime session options must be a map"
                     {:options options})))))

(def default-xr-client-id runtime-ids/default-xr-client-id)

(def external-source-client-id runtime-ids/external-source-client-id)

(defn stable-node-id [& parts]
  (apply runtime-ids/stable-node-id parts))

(defn runtime-graph-id []
  (runtime-ids/runtime-graph-id))

(defn boundary-outbox-id []
  (runtime-ids/boundary-outbox-id))

(defn receipt-slot-key [effect-id]
  (runtime-ids/receipt-slot-key effect-id))

(defn effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn xr-receipt
  [request status]
  (boundary/xr-receipt request status))

(defn daemon-executor
  [name]
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r name)
         (.setDaemon true))))))

(defn install-runtime-protocols
  [n]
  (cell-protocol/prefer-direct-standard-protocols n))

(defn runtime-base-net []
  (install-runtime-protocols net/empty-net))

(defn runtime-root
  [network]
  (let [env-id (stable-node-id :compiler-2 :runtime :root-environment)
        bindings ((requiring-resolve
                   'propagators.compiler.main/tms-bindings))
        declared (compiler-env/declare-root network env-id bindings)]
    (assoc declared
           :net (runner/completed-network
                 (runner/run-network (:props declared) (:net declared))))))

(defn runtime-compiler-env []
  (:env (runtime-root (runtime-base-net))))

(defn empty-state []
  (let [root (runtime-root (runtime-base-net))]
   {:network (install-runtime-protocols net/empty-net)
   :program/net (:net root)
   :program/env (:env root)
   :program/props (:props root)
   :program/graph {:nodes {} :edges [] :values {} :expansions {}}
   :program/results {}
   :program/epoch 0
   :runtime/commit-tick 0
   :runtime/full-rebuild-fallbacks 0
   :runtime/errors []
   :versioned/commit-log []
   :environment/effects {}
   :environment/handlers (extension/default-handler-registry)
   :session/extensions {}
   :environment/primitive-imports []
   :environment/source-ledger []
   :environment/load-stack []
   :environment/checkpoints {}
   :clock/subscriptions {}
   :clock/epochs {}
   :inspection/probes {}
   :inspection/seen-requests #{}
   :tui/focus-seq 0
   :block-order []
   :next-order 0
   :traces {}
   :xr {:launched {}}
   :tuis {}}))

(defn- initialized-state?
  [state]
  (and (map? state)
       (net/network? (:network state))
       (net/network? (:program/net state))
       (ids/node-id? (:program/env state))))

(defn- repair-partial-state
  [state]
  (let [base (assoc (empty-state)
                    :runtime/options
                    (or (:runtime/options state) {}))]
    (cond-> base
      (seq (get-in state [:runtime :temperature :samples]))
      (assoc-in [:runtime :temperature :samples]
                (get-in state [:runtime :temperature :samples]))

      (seq (:runtime/network-cache-stats state))
      (assoc :runtime/network-cache-stats
             (:runtime/network-cache-stats state)))))

(defn ensure-session-state! [session]
  (when-not (initialized-state? @session)
    (reset! session (repair-partial-state @session)))
  @session)

(defn- preserve-xr-traces
  [old-state new-state]
  (let [xr-traces (or (:xr/traces old-state)
                      (get-in old-state [:xr :traces]))]
    (if (and (seq xr-traces)
             (empty? (or (:xr/traces new-state)
                         (get-in new-state [:xr :traces]))))
      (-> new-state
          (assoc :xr/traces xr-traces)
          (assoc-in [:xr :traces] xr-traces))
      new-state)))

(defn mutate-session!
  [session f]
  (locking session
    (network-cache/with-cache
      (let [state @session
            state' (->> (f state)
                        (preserve-xr-traces state))
            state'' (assoc state'
                           :runtime/network-cache-stats
                           (network-cache/stats))]
        (reset! session state'')
        state''))))

(defn require-state
  [state]
  (when-not state
    (throw (ex-info "no compiled source in runtime session" {})))
  state)

(defn runtime-error-entry
  [source throwable]
  (cond-> {:source source
           :message (ex-message throwable)
           :class (some-> throwable class .getName)}
    (ex-data throwable) (assoc :data (ex-data throwable))))

(defn append-runtime-error
  [state entry]
  (update state :runtime/errors
          (fn [errors]
            (->> (conj (vec errors) (assoc entry :at (System/currentTimeMillis)))
                 (take-last 20)
                 vec))))

(defn record-runtime-error
  [state source throwable]
  (append-runtime-error state (runtime-error-entry source throwable)))

(defn run-session-activation
  [state source activate]
  (try
    (activate state)
    (catch Throwable throwable
      (record-runtime-error state source throwable))))

(defn record-runtime-error!
  [session source throwable]
  (locking session
    (ensure-session-state! session)
    (swap! session append-runtime-error
           (runtime-error-entry source throwable))))

(defn labels
  [state]
  (let [compiled (:compiled state)
        program-net (:program/net state)]
    (if (and compiled program-net)
      (demo/compiled-labels compiled program-net)
      {})))
