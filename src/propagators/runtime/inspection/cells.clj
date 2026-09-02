(ns propagators.runtime.inspection.cells
  "Cell listing and lookup helpers for compiler-2 runtime."
  (:require [propagators.runtime.session.state :as state]
            [propagators.runtime.inspection.semantic-graph :as semantic-repl]
            [propagators.infra.cells.cell :as cell]
            [propagators.compiler.model.env :as cenv]
            [propagators.infra.network :as net]))

(def require-state state/require-state)
(def labels state/labels)

(defn cell-row
  [state [id entry]]
  (when (cell/cell? entry)
    {:cell-id (pr-str id)
     :label (get (labels state) id)
     :strongest (semantic-repl/display-cell-value (cell/cell-strongest entry))}))

(defn list-cells
  [state]
  (->> (net/net-env (:program/net (require-state state)))
       (keep (partial cell-row state))
       (sort-by (juxt #(or (:label %) "") :cell-id))
       vec))

(defn matching-label?
  [requested actual]
  (and requested actual (= (str requested) (str actual))))

(defn resolve-cell-row
  [state {:keys [cell-id label]}]
  (let [cells (list-cells state)]
    (or (some #(when (= cell-id (:cell-id %)) %) cells)
        (some #(when (matching-label? label (:label %)) %) cells)
        (throw (ex-info "cell not found" {:cell-id cell-id :label label})))))

(defn trace-cell-id-for-label
  [state label]
  (or (cenv/resolve-binding-id (:program/net state)
                               (:program/env state)
                               (symbol (str label)))
      (some (fn [[id entry]]
              (when (and (cell/cell? entry)
                         (matching-label? label (get (labels state) id)))
                id))
            (net/net-env (:program/net state)))))

(defn read-cell
  [state request]
  (resolve-cell-row (require-state state) request))
