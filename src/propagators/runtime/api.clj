(ns propagators.runtime.api
  "Stable public runtime session and inspection entrypoints."
  (:require [propagators.runtime.session.commands :as commands]
            [propagators.runtime.session.program :as program]
            [propagators.runtime.session.state :as state]))

(defn create-session
  [options]
  (state/new-session options))

(defn load-program
  [session source]
  (if (string? source)
    {:session session
     :graph (program/compile-source! session source)}
    (throw (ex-info "runtime source must be a string"
                    {:source source}))))

(defn dispatch-command
  [session command]
  (if (and (map? command) (keyword? (:op command)))
    (commands/handle-command! session command)
    (throw (ex-info "runtime command must contain a keyword :op"
                    {:command command}))))

(defn- stable-edge
  [edge]
  (if (and (sequential? edge) (= 2 (count edge)))
    {:from (first edge) :to (second edge)}
    (throw (ex-info "semantic graph edge must contain two node IDs"
                    {:edge edge}))))

(defn- stable-graph
  [graph]
  (let [nodes (or (:nodes graph) {})
        values (or (:values graph) {})
        aliases (or (:node-aliases graph) (:aliases graph) {})]
    {:schema-version 1
     :nodes (into {}
                  (map (fn [[node-id label]]
                         [node-id {:label (str label)
                                   :value (get values node-id)}]))
                  nodes)
     :edges (mapv stable-edge (or (:edges graph) []))
     :aliases aliases}))

(defn inspect
  [session request]
  (if (not (map? request))
    (throw (ex-info "runtime inspection request must be a map"
                    {:request request}))
    (if (= :semantic-graph (:kind request))
      (stable-graph (:graph (state/require-state @session)))
      (if (keyword? (:op request))
        (dispatch-command session request)
        (throw (ex-info "unsupported runtime inspection request"
                        {:request request}))))))
