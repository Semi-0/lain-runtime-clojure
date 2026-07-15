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
            [propagators.compiler-2.model.context :as context]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.gur.flat :as fvm]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def retained-application-scope [:compiler-2 :retained-application])
(def retained-application-props-key :compiler-2/retained-application-props)

(defn- application-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [retained-application-scope key])))

(defn- closure-lexical-env
  [network closure context-id]
  (let [captured (closure-value/closure-env closure)]
    (if (contains? (net/net-env network) captured)
      captured
      (some-> (h/strongest-or-nothing network context-id)
              context/lexical-env))))

(defn- declare-closure-frame
  [compile* network application-id closure-id closure arg-ids context-id out-id]
  (when-let [{:keys [input-ids targets]}
             (application/closure-call-plan closure arg-ids out-id)]
    (let [lexical-env (closure-lexical-env network closure context-id)
          key [application-id closure-id arg-ids out-id]
          frame-id (h/stable-node-id :compiler-2/retained-application key :env)
          [env-props prepared-network]
          (application/declare-closure-environment
           network
           lexical-env
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

      (closure-value/closure-info? operator)
      (if (application-installed? network key)
        []
        (or (declare-closure-frame compile*
                                   network
                                   application-id
                                   operator-id
                                   operator
                                   arg-ids
                                   context-id
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
  (application-messages-for-operator compile* application-id operator-id args-id
                                     arg-ids context-id out-id network))

(defn application-messages
  [application-id operator-id args-id arg-ids context-id out-id network]
  (application-messages-with dispatch/default-compiler
                             application-id operator-id args-id arg-ids
                             context-id out-id network))

(defn- application-activation
  [compile* application-id operator-id args-id arg-ids context-id out-id]
  (fn [_inputs _outputs network]
    (application-messages-with compile* application-id operator-id args-id
                               arg-ids context-id out-id network)))

(defn p:apply-application-with
  [compile* application-id operator-id args-id arg-ids context-id out-id]
  (let [arg-ids (vec arg-ids)
        inputs (into [application-id operator-id args-id context-id] arg-ids)]
    (fn [network]
      (let [activate (application-activation compile* application-id operator-id
                                             args-id arg-ids context-id out-id)
            network* (reduce h/ensure-cell network (conj inputs out-id))
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
  (p:apply-application-with dispatch/default-compiler
                            application-id operator-id args-id arg-ids
                            context-id out-id))
