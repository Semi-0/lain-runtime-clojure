(ns propagators.compiler-2.runtime.inspection.trace.session
  "Installed semantic trace sessions for compiler-2 runtime."
  (:require [propagators.compiler-2.runtime.inspection.cells :as cells]
            [propagators.compiler-2.runtime.session.state :as state]
            [propagators.compiler-2.runtime.inspection.temperature :as temperature]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.core :as core]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.semantic-trace :as semantic-trace])
  (:import [java.util.concurrent TimeUnit]))

(def require-state state/require-state)
(def daemon-executor state/daemon-executor)
(def mutate-session! state/mutate-session!)
(def resolve-cell-row cells/resolve-cell-row)
(def trace-cell-id-for-label cells/trace-cell-id-for-label)

(defn trace-via-propagator
  ([graph request]
   (trace-via-propagator graph request (:direction request)))
  ([graph source direction]
   (let [source-id (ids/new-node-id)
         direction-id (ids/new-node-id)
         request-id (ids/new-node-id)
         graph-id (ids/new-node-id)
         out-id (ids/new-node-id)
         n0 (-> net/empty-net
                (nb/install-cell source-id source source)
                (nb/install-cell direction-id direction direction)
                (nb/install-cell request-id)
                (nb/install-cell graph-id graph graph)
                (nb/install-cell out-id))
         [request-prop-id n1] ((semantic-trace/p:trace-request
                                source-id direction-id request-id)
                               n0)
         [trace-prop-id n2] ((semantic-trace/p:semantic-trace request-id graph-id out-id) n1)
         n3 (nb/run-propagators n2 [request-prop-id trace-prop-id])]
     (net/network-cell-strongest n3 out-id))))

(defn resolved-trace-request
  [state request]
  (cond
    (:node request)
    request

    (:label request)
    request

    :else
    (assoc request :label (:label (resolve-cell-row state request)))))

(defn semantic-trace
  [state request]
  (let [state (require-state state)
        request* (resolved-trace-request state request)]
    (trace-via-propagator (:graph state) request*)))

(defn semantic-expansion
  [state request]
  (let [graph (:graph (require-state state))]
    (or (semantic-repl/expansion graph request)
        (throw (ex-info "semantic expansion not found"
                        (select-keys request [:node :label]))))))

(defn installed-trace-graph
  [trace]
  (net/network-cell-strongest (:network trace) (:out-id trace)))

(defn tick-trace!
  [session trace-id]
  (mutate-session!
   session
   (fn [state]
     (if-let [trace (get-in state [:traces trace-id])]
       (let [next-epoch (inc (:epoch trace))
             [tasks n1] (core/eval-cells
                         [(message (:epoch-id trace)
                                   (semantic-trace/epoch next-epoch))]
                         (:network trace))
             [state' n2] (temperature/run-tasks state
                                                :propagation/trace-tick
                                                tasks
                                                n1)]
         (assoc-in state'
                   [:traces trace-id]
                   (assoc trace
                          :epoch next-epoch
                          :network n2)))
       state))))

(defn install-semantic-trace!
  [session request]
  (let [state (require-state @session)
        request* (resolved-trace-request state request)
        interval-ms (long (or (:interval-ms request*) 5000))
        trace-id (str (random-uuid))
        request-id (ids/new-node-id)
        graph-id (ids/new-node-id)
        epoch-id (ids/new-node-id)
        out-id (ids/new-node-id)
        n0 (-> net/empty-net
               (nb/install-cell request-id request* request*)
               (nb/install-cell graph-id
                                (semantic-trace/graph-union (:graph state))
                                (semantic-trace/graph-union (:graph state)))
               (nb/install-cell epoch-id (semantic-trace/epoch 0) (semantic-trace/epoch 0))
               (nb/install-cell out-id))
        [prop-id n1] ((semantic-trace/p:semantic-trace request-id graph-id epoch-id out-id) n0)
        [state' n2] (temperature/run-propagators state
                                                 :propagation/trace-install
                                                 n1
                                                 [prop-id])
        executor (daemon-executor (str "semantic-trace-clock-" trace-id))
        stop #(do (.shutdownNow executor) nil)
        trace {:trace-id trace-id
               :request request*
               :interval-ms interval-ms
               :network n2
               :prop-id prop-id
               :request-id request-id
               :graph-id graph-id
               :epoch-id epoch-id
               :out-id out-id
               :epoch 0
               :stop stop}]
    (swap! session #(-> %
                        (merge (select-keys state' [:runtime]))
                        (assoc-in [:traces trace-id] trace)))
    (.scheduleAtFixedRate executor
                          #(tick-trace! session trace-id)
                          interval-ms
                          interval-ms
                          TimeUnit/MILLISECONDS)
    {:trace-id trace-id
     :interval-ms interval-ms
     :graph (installed-trace-graph trace)}))

(defn read-installed-trace
  [state {:keys [trace-id]}]
  (let [trace (get-in (require-state state) [:traces trace-id])]
    (when-not trace
      (throw (ex-info "trace not found" {:trace-id trace-id})))
    {:trace-id trace-id
     :epoch (:epoch trace)
     :graph (installed-trace-graph trace)}))

(defn stop-installed-trace!
  [session {:keys [trace-id]}]
  (let [trace (get-in @session [:traces trace-id])]
    (when-not trace
      (throw (ex-info "trace not found" {:trace-id trace-id})))
    ((:stop trace))
    (swap! session update :traces dissoc trace-id)
    {:trace-id trace-id
     :stopped true}))
