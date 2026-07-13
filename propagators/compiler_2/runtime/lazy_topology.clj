(ns propagators.compiler-2.runtime.lazy-topology
  "Lazy topology installers for compiler-2 special forms."
  (:require [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.compiler-2.compiler.dispatch :as compiler-dispatch]
            [propagators.compiler-2.model.env :as env]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.gur.flat :as fvm]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def when-scope [:compiler-2 :lazy-topology :when])

(defn- when-installed?
  [network when-key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [when-scope when-key])))

(defn- cell-entry?
  [[_id entry]]
  (cell/cell? entry))

(defn- prop-entry?
  [[_id entry]]
  (prop/prop? entry))

(defn- existing-content
  [network id]
  (let [entry (get (net/net-env network) id)]
    (when (cell/cell? entry)
      (cell/cell-content entry))))

(defn- changed-cell-message
  [base-network id entry]
  (let [content (cell/cell-content entry)]
    (when (and (not (value/nothing? content))
               (not= content (existing-content base-network id)))
      (message id content))))

(defn- declare-prop-effect
  [compiled-network prop-id entry]
  (if-let [node (get (net/net-graph compiled-network) prop-id)]
    (fvm/declare-prop prop-id
                      (vec (:inputs node))
                      (vec (:outputs node))
                      (prop/prop-f entry))
    (throw (ex-info "compiled lazy topology prop has no graph node"
                    {:prop-id prop-id}))))

(defn- new-cell-effects
  [base-network compiled-network]
  (->> (net/net-env compiled-network)
       (filter cell-entry?)
       (keep (fn [[id _entry]]
               (when-not (contains? (net/net-env base-network) id)
                 (fvm/declare-cell id))))
       vec))

(defn- changed-cell-messages
  [base-network compiled-network]
  (->> (net/net-env compiled-network)
       (filter cell-entry?)
       (keep (fn [[id entry]]
               (changed-cell-message base-network id entry)))
       vec))

(defn- prop-effects
  [base-network compiled-network prop-ids]
  (->> prop-ids
       distinct
       (keep (fn [prop-id]
               (let [entry (get (net/net-env compiled-network) prop-id)]
                 (when (and (prop/prop? entry)
                            (not (contains? (net/net-env base-network) prop-id)))
                   (declare-prop-effect compiled-network prop-id entry)))))
       vec))

(defn- compile-body-state
  [compile* base-network captured-state body]
  (let [body-state (-> captured-state
                       (assoc :net base-network
                              :path (conj (:path captured-state) :body)
                              :props []
                              :applications []))
        [state' _binding] (compile* body-state body)]
    state'))

(defn- body-activation-result
  [compile* base-network captured-state body marker-effect]
  (let [compiled-state (compile-body-state compile* base-network captured-state body)
        compiled-network (:net compiled-state)]
    (-> (topology-effects/network-diff base-network
                                       compiled-network
                                       (:props compiled-state))
        (update :effects #(into [marker-effect] %)))))

(defn install-when-topology-with
  [compile* state condition-id body]
  (let [[state' result-binding] (h/new-cell state :when-result)
        result-id (env/binding-id result-binding)
        guard-id (or (env/lexical-value-address (:net state') condition-id)
                     condition-id)
        prop-id (h/node-id state' :when-prop)
        when-key [:compiler-2 :when (:seed state') (:path state') result-id]
        captured-state state'
        activate (fn [_inputs _outputs network]
                   (let [condition (h/strongest-or-nothing network guard-id)]
                     (cond
                       (value/nothing? condition)
                       []

                       (value/contradiction? condition)
                       [(message result-id value/contradiction)]

                       (when-installed? network when-key)
                       []

                       :else
                       (body-activation-result
                        compile*
                        network
                        captured-state
                        body
                        (fvm/bind-name when-scope when-key result-id)))))
        [installed-id network'] ((prop/construct-propagator prop-id
                                                            :compiler-2/lazy-when
                                                            activate
                                                            [guard-id]
                                                            [result-id])
                                 (:net state'))]
    [(-> state'
         (assoc :net network')
         (h/add-props [installed-id]))
     result-binding]))

(defn install-when-topology
  [state condition-id body]
  (install-when-topology-with compiler-dispatch/default-compiler
                              state condition-id body))
