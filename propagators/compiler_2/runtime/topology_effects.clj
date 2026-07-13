(ns propagators.compiler-2.runtime.topology-effects
  "Translate an additively compiled network fragment into bounded effects."
  (:require [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.gur.flat :as fvm]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(defn- cell-entry? [[_ entry]] (cell/cell? entry))
(defn- prop-entry? [[_ entry]] (prop/prop? entry))

(defn- existing-content [network id]
  (let [entry (get (net/net-env network) id)]
    (when (cell/cell? entry) (cell/cell-content entry))))

(defn new-cell-effects [base compiled]
  (->> (net/net-env compiled)
       (filter cell-entry?)
       (keep (fn [[id _]]
               (when-not (contains? (net/net-env base) id)
                 (fvm/declare-cell id))))
       vec))

(defn changed-cell-messages [base compiled]
  (->> (net/net-env compiled)
       (filter cell-entry?)
       (keep (fn [[id entry]]
               (let [content (cell/cell-content entry)]
                 (when (and (not (value/nothing? content))
                            (not= content (existing-content base id)))
                   (message id content)))))
       vec))

(defn- declare-prop-effect [compiled prop-id entry]
  (if-let [node (get (net/net-graph compiled) prop-id)]
    (fvm/declare-prop prop-id
                      (vec (:inputs node))
                      (vec (:outputs node))
                      (prop/prop-f entry))
    (throw (ex-info "compiled topology prop has no graph node"
                    {:prop-id prop-id}))))

(defn prop-effects [base compiled prop-ids]
  (->> prop-ids
       distinct
       (keep (fn [prop-id]
               (let [entry (get (net/net-env compiled) prop-id)]
                 (when (and (prop-entry? [prop-id entry])
                            (not (contains? (net/net-env base) prop-id)))
                   (declare-prop-effect compiled prop-id entry)))))
       vec))

(defn network-diff
  [base compiled prop-ids]
  {:effects (into (new-cell-effects base compiled)
                  (prop-effects base compiled prop-ids))
   :messages (changed-cell-messages base compiled)})


