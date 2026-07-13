(ns propagators.compiler-2.runtime.retained-application
  "Retained compiler-2 application installer.

  Closure values are lowered into retained closure frames. Primitive/operator
  values still use the compatibility activation path while their installers are
  migrated.
  "
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as application]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.gur.flat :as fvm]
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
          frame-env (application/closure-body-env
                     (closure-value/closure-env closure)
                     (closure-value/closure-inputs closure)
                     targets
                     input-ids)
          prepared-network (h/seed-cell network frame-id frame-env)
          prepared (application/prepare-closure-frame
                    compile*
                    prepared-network
                    closure
                    frame-env
                    {:seed [:compiler-2/retained-application key]
                     :application/cell-declarer :retained-frame})]
      (-> (topology-effects/network-diff network
                                         (:net prepared)
                                         (:props prepared))
          (update :effects
                  #(into [(fvm/bind-name retained-application-scope key frame-id)]
                         %))))))

(defn application-messages-with
  [compile* application-id operator-id args-id arg-ids context-id out-id network]
  (let [app-info (h/strongest-or-nothing network application-id)
        answer (h/strongest-or-nothing network operator-id)
        operator (scope-source/unwrap answer)
        arg-ids (vec arg-ids)
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


