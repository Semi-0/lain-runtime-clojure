(ns propagators.compiler-2.runtime.topology-effects
  "Translate an additively compiled network fragment into bounded effects."
  (:require [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.gur.flat :as fvm]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def declaration-scope :compiler-2/runtime-declarations)

(defn declared?
  "True when one delayed topology declaration has already been committed."
  [network declaration-key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [declaration-scope declaration-key])))

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
                      (prop/prop-name entry)
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

(defn declare-once
  "Return bounded effects for one delayed topology declaration.

  `build` receives the current network and returns a compiled network fragment
  plus the propagator ids that belong to the declaration. The marker is
  committed by the same runtime that commits the topology effects."
  [network declaration-key marker-id build]
  (if (declared? network declaration-key)
    {:effects [] :messages []}
    (let [{compiled :net prop-ids :props} (build network)]
      (update (network-diff network compiled prop-ids)
              :effects conj
              (fvm/bind-name declaration-scope declaration-key marker-id)))))
