(ns propagators.compiler-2.runtime.retained-application
  "Retained compiler-2 application installer.

  Closure values are lowered into retained closure frames. Primitive/operator
  values still use the compatibility activation path while their installers are
  migrated.
  "
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as application]
            [propagators.compiler-2.runtime.closure-frame :as closure-frame]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.gur.flat :as fvm]
            [propagators.ids :as ids]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def retained-application-scope [:compiler-2 :retained-application])
(def retained-application-props-key :compiler-2/retained-application-props)

(defn- application-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [retained-application-scope key])))

(defn- declare-closure-frame
  [compile* network application-id closure-id closure arg-ids out-id]
  (when-let [{:keys [input-ids targets]}
             (application/closure-call-plan closure arg-ids out-id)]
    (let [key [application-id closure-id arg-ids out-id]
          frame-id (h/stable-node-id :compiler-2/retained-application key :env)
          [env-props prepared-network]
          (application/declare-closure-environment
           network
           (closure-value/closure-env closure)
           frame-id
           (closure-value/closure-inputs closure)
           targets
           input-ids)
          [frame-prop compiled]
          ((closure-frame/p:apply-closure-with compile* closure-id frame-id)
           prepared-network)]
      (-> (topology-effects/network-diff network compiled
                                         (conj (vec env-props) frame-prop))
          (update :effects
                  #(into [(fvm/bind-name retained-application-scope key frame-id)]
                         %))))))

(defn- addressed-id
  "Select the canonical cell named by a scoped lexical candidate."
  [network id]
  (let [candidate (h/strongest-or-nothing network id)
        address (when (scope-source/scope-value? candidate)
                  (scope-source/binding-address candidate))]
    (if (and (ids/node-id? address)
             (contains? (net/net-env network) address))
      address
      id)))

(defn- candidate-addresses
  "Return every callable address retained by one lexical operator cell."
  [network id]
  (->> (scope-source/content-candidates
        (net/network-cell-content network id))
       (keep scope-source/binding-address)
       (filter ids/node-id?)
       (filter #(contains? (net/net-env network) %))
       distinct
       vec))

(defn- operator-ids
  [network operator-id]
  (let [addresses (candidate-addresses network operator-id)]
    (if (seq addresses)
      addresses
      [(addressed-id network operator-id)])))

(defn- activation-parts
  [result]
  (if (map? result)
    {:effects (vec (:effects result))
     :messages (vec (:messages result))}
    {:effects []
     :messages (vec (or result []))}))

(defn- merge-activation-results
  [results]
  (let [{:keys [effects messages]}
        (reduce (fn [combined result]
                  (let [parts (activation-parts result)]
                    {:effects (into (:effects combined) (:effects parts))
                     :messages (into (:messages combined) (:messages parts))}))
                {:effects [] :messages []}
                results)]
    (if (seq effects)
      {:effects effects :messages messages}
      messages)))

(defn- application-messages-for-operator
  [compile* application-id operator-id args-id arg-ids context-id out-id network]
  (let [app-info (h/strongest-or-nothing network application-id)
        answer (h/strongest-or-nothing network operator-id)
        operator answer
        key [application-id operator-id arg-ids out-id]]
    (cond
      (or (value/unusable? app-info)
          (not (application-value/application-info? app-info)))
      []

      (value/unusable? answer)
      []

      (and (closure-value/closure-info? operator)
           (not (scope-source/scope-value? answer)))
      (if (application-installed? network key)
        []
        (or (declare-closure-frame compile*
                                   network
                                   application-id
                                   operator-id
                                   operator
                                   arg-ids
                                   out-id)
            []))

      :else
        (application/application-messages-with compile*
                                               application-id
                                               operator-id
                                               args-id
                                               arg-ids
                                               context-id
                                               out-id
                                               network))))

(defn application-messages-with
  [compile* application-id operator-id args-id arg-ids context-id out-id network]
  (let [arg-ids (mapv (partial addressed-id network) arg-ids)]
    (merge-activation-results
     (mapv #(application-messages-for-operator compile*
                                               application-id
                                               %
                                               args-id
                                               arg-ids
                                               context-id
                                               out-id
                                               network)
           (operator-ids network operator-id)))))

(defn application-messages
  [application-id operator-id args-id arg-ids context-id out-id network]
  (application-messages-with dispatch/compile-expression
                             application-id operator-id args-id arg-ids
                             context-id out-id network))

(defn p:apply-application-with
  [compile* application-id operator-id args-id arg-ids context-id out-id]
  (let [arg-ids (vec arg-ids)
        activate (fn [_inputs _outputs network]
                   (application-messages-with compile*
                                              application-id
                                              operator-id
                                              args-id
                                              arg-ids
                                              context-id
                                              out-id
                                              network))
        inputs (into [application-id operator-id args-id context-id] arg-ids)]
    (fn [network]
      (let [network* (reduce h/ensure-cell network (conj inputs out-id))
            [prop-id n] ((prop/construct-propagator :compiler-2/retained-application
                                                    activate inputs [out-id])
                         network*)]
        [prop-id
         (net/update-net-dict-entry n
                                    retained-application-props-key
                                    (fnil conj #{})
                                    prop-id)]))))

(defn p:apply-application
  [application-id operator-id args-id arg-ids context-id out-id]
  (p:apply-application-with dispatch/compile-expression
                            application-id operator-id args-id arg-ids
                            context-id out-id))
