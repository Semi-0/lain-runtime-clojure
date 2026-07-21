(ns propagators.compiler-2.runtime.tui.block-model
  "Pure TUI block-model helpers for the compiler-2 runtime."
  (:require [propagators.network :as net]))

(defn valid-client-id? [client-id]
  (and (string? client-id)
       (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" client-id))))

(defn all-blocks [state]
  (mapcat (fn [[client-id tui]]
            (map #(assoc % :client-id client-id) (:blocks tui)))
          (:tuis state)))

(defn block-by-index
  [state client-id index]
  (some #(when (= index (:index %)) %)
        (get-in state [:tuis client-id :blocks])))

(defn update-block
  [state client-id index f]
  (update-in state [:tuis client-id :blocks]
             (fn [blocks]
               (mapv #(if (= index (:index %)) (f %) %) blocks))))

(defn block-by-text-id
  [state text-id]
  (some #(when (= text-id (:text-id %)) %)
        (all-blocks state)))

(defn block-by-display-id
  [state display-id]
  (some #(when (= display-id (:display-id %)) %)
        (all-blocks state)))

(defn next-global-order
  [state]
  (or (:next-order state) 0))

(defn assign-source-order
  [state client-id index]
  (let [order (next-global-order state)]
    (-> state
        (update-block client-id index
                      #(assoc % :order order))
        (update :block-order (fnil conj []) {:client-id client-id
                                             :index index})
        (assoc :next-order (inc order)))))

(defn source-blocks
  [state]
  (->> (concat
        (keep (fn [{:keys [client-id index]}]
                (when-let [block (block-by-index state client-id index)]
                  (when (some? (:order block))
                    (assoc block :client-id client-id))))
              (:block-order state))
        (:external-sources state))
       (sort-by :order)
       vec))

(defn block-text
  [state block]
  (if-let [record (peek (vec (:version-history block)))]
    (:source record)
    (if (contains? block :source)
      (:source block)
      (if (contains? block :text-current)
        (:text-current block)
        (net/network-cell-strongest (:network state) (:text-id block))))))
