(ns propagators.runtime.session.extension
  "Session capability bundles for compiler bindings and boundary handlers."
  (:require [propagators.compiler.model.env :as env]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.runtime.operators.environment :as operators]
            [propagators.infra.ids :as ids]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.runner :as runner]))

(defprotocol SessionExtension
  (extension-id [extension])
  (extension-bindings [extension])
  (extension-effects [extension])
  (extension-declaration [extension]))

(deftype ExtensionBundle [id bindings effects declaration]
  SessionExtension
  (extension-id [_]
    id)
  (extension-bindings [_]
    bindings)
  (extension-effects [_]
    effects)
  (extension-declaration [_]
    declaration))

(defprotocol EffectHandler
  (handle-effect [handler services session request]))

(deftype FixedHandler [handler-symbol implementation]
  EffectHandler
  (handle-effect [_ services session request]
    (implementation services session request)))

(defn fixed-handler
  [handler-symbol]
  (let [resolved (requiring-resolve handler-symbol)
        implementation (var-get resolved)]
    (cond
      (ifn? implementation)
      (FixedHandler. handler-symbol implementation)

      :else
      (throw
       (ex-info "Effect handler is not callable"
                {:handler-symbol handler-symbol})))))

(defn freeze-bindings
  [bindings]
  (cond
    (and (ifn? bindings) (not (sequential? bindings)))
    (throw (ex-info "Session extension bindings must be declared values"
                    {:bindings bindings}))

    (and (seqable? bindings) (not (map? bindings)) (not (set? bindings)))
    (mapv
     (fn [binding]
       (let [pair (vec binding)
             symbol (first pair)]
         (cond
           (and (= 2 (count pair)) (symbol? symbol))
           [symbol (second pair)]

           :else
           (throw (ex-info "Invalid session extension binding"
                           {:binding binding})))))
     bindings)

    :else
    (throw (ex-info "Session extension bindings must be seqable"
                    {:bindings bindings}))))

(defn- validate-effect
  [effect]
  (cond
    (not (symbol? (:effect/symbol effect)))
    (throw (ex-info "Effect specification requires a binding symbol"
                    {:effect effect}))

    (not (set? (:effect/arities effect)))
    (throw (ex-info "Effect specification requires an arity set"
                    {:effect effect}))

    (not (ifn? (:effect/normalize effect)))
    (throw (ex-info "Effect specification requires a normalizer"
                    {:effect effect}))

    (not (ifn? (:effect/identity-parts effect)))
    (throw (ex-info "Effect specification requires an identity function"
                    {:effect effect}))

    :else
    effect))

(defn freeze-effect
  [effect]
  (into {} (validate-effect effect)))

(defn extension-bundle
  [{:keys [id bindings effects]}]
  (cond
    (nil? id)
    (throw (ex-info "Session extension requires an id" {}))

    (not (sequential? effects))
    (throw (ex-info "Session extension effects must be sequential"
                    {:extension-id id :effects effects}))

    :else
    (let [bindings (freeze-bindings bindings)
          effects (mapv freeze-effect effects)
          declaration {:extension/id id
                       :extension/bindings bindings
                       :extension/effects effects}]
      (ExtensionBundle. id bindings effects declaration))))

(defn effect-registration
  [effect]
  (let [effect (validate-effect effect)
        port (:boundary/port effect)
        kind (:boundary/kind effect)
        handler-symbol (:effect/handler-symbol effect)]
    (cond
      (nil? port)
      (throw (ex-info "Effect specification requires a boundary port"
                      {:effect effect}))

      (nil? kind)
      (throw (ex-info "Effect specification requires a boundary kind"
                      {:effect effect}))

      (not (qualified-symbol? handler-symbol))
      (throw (ex-info "Effect handler must be a qualified symbol"
                      {:effect effect}))

      :else
      {:key [port kind]
       :id [port kind handler-symbol]
       :handler (fixed-handler handler-symbol)})))

(defn- registrations
  [effects]
  (mapv effect-registration effects))

(defn- add-registration
  [registry {:keys [key id] :as proposed}]
  (let [existing (get registry key)]
    (cond
      (nil? existing)
      (assoc registry key proposed)

      (= (:id existing) id)
      registry

      :else
      (throw
       (ex-info "Session effect handler key is already registered"
                {:handler-key key
                 :existing-handler-id (:id existing)
                 :proposed-handler-id id})))))

(defn register-effects
  [registry effects]
  (reduce add-registration (or registry {}) (registrations effects)))

(defn- effect-binding
  [context effect]
  (let [outbox-id (:outbox-id context)]
    (cond
      (nil? outbox-id)
      (throw (ex-info "Effect binding requires a boundary outbox"
                      {:effect (:effect/symbol effect)}))

      :else
      [(:effect/symbol effect)
       (operators/effect-operator
        (assoc effect
               :effect/outbox-id outbox-id
               :effect/context context))])))

(defn program-bindings
  [extension context]
  (let [ordinary (extension-bindings extension)]
    (cond
      (not (sequential? ordinary))
      (throw (ex-info "Session extension bindings must be sequential"
                      {:extension-id (extension-id extension)
                       :bindings ordinary}))

      :else
      (let [effects (mapv #(effect-binding context (validate-effect %))
                          (extension-effects extension))]
        (into (vec ordinary) effects)))))

(defn- duplicate-symbols
  [bindings]
  (->> bindings
       (map first)
       frequencies
       (keep (fn [[symbol count]]
               (when (> count 1) symbol)))
       vec))

(defn- validate-bindings
  [extension bindings]
  (let [invalid (remove #(and (vector? %) (= 2 (count %))) bindings)]
    (cond
      (seq invalid)
      (throw (ex-info "Session extension bindings must be ordered pairs"
                      {:extension-id (extension-id extension)
                       :invalid-bindings (vec invalid)}))

      :else
      (let [duplicates (duplicate-symbols bindings)]
        (cond
          (seq duplicates)
          (throw
           (ex-info "Session extension contains duplicate binding symbols"
                    {:extension-id (extension-id extension)
                     :duplicate-symbols duplicates}))

          :else
          bindings)))))

(defn- live-parent
  [network parent extension-id]
  (cond
    (ids/node-id? parent)
    {:net (nb/ensure-cell network parent)
     :env parent
     :props []}

    :else
    (throw (ex-info "Session extension requires a live environment cell"
                    {:extension-id extension-id :environment parent}))))

(defn install-extension-bindings
  [network parent-id child-id bindings]
  (let [[scope-props scoped]
        ((env/p:scope-frame parent-id child-id (set (map first bindings)))
         network)
        declared (env/declare-bindings scoped child-id bindings)]
    {:net (:net declared)
     :env child-id
     :props (into (vec scope-props) (:props declared))}))

(defn install-session-extension
  [session extension context]
  (let [id (extension-id extension)
        declaration (extension-declaration extension)
        existing (get-in session [:session/extensions id])]
    (cond
      (= declaration (:declaration existing))
      session

      (some? existing)
      (throw (ex-info "Session extension id is already installed"
                      {:extension-id id
                       :existing (:declaration existing)
                       :proposed declaration}))

      :else
      (let [bindings (validate-bindings
                      extension
                      (program-bindings extension context))
            effects (extension-effects extension)
            registrations (registrations effects)
            handlers (reduce add-registration
                             (or (:environment/handlers session) {})
                             registrations)
            parent (live-parent (:program/net session) (:program/env session) id)
            child-id (runtime-ids/stable-node-id
                      :compiler-2 :session-extension id)
            installed (install-extension-bindings
                       (:net parent) (:env parent) child-id bindings)
            props (into (vec (:props parent)) (:props installed))
            network (runner/completed-network
                     (runner/run-network props (:net installed)))
            receipt {:status :installed
                     :declaration declaration
                     :environment child-id
                     :binding-symbols (mapv first bindings)
                     :handler-ids (mapv :id registrations)}]
        (-> session
            (assoc :program/net network
                   :program/env child-id
                   :environment/handlers handlers)
            (assoc-in [:session/extensions id] receipt))))))

(def core-extension
  (extension-bundle
   {:id :compiler-2/core-session-effects
    :bindings []
    :effects [(operators/load-primitive-environment-effect)
              (operators/load-lain-effect)
              (operators/save-environment-effect)]}))

(def client-extension
  (extension-bundle
   {:id :compiler-2/client-session-effects
    :bindings []
    :effects [(operators/load-blocks-effect)
              (operators/save-blocks-effect)]}))

(defn default-handler-registry
  []
  (-> {}
      (register-effects (extension-effects core-extension))
      (register-effects (extension-effects client-extension))))
