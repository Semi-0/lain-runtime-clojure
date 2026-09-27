(ns propagators.runtime.operators.environment
  "Ordinary bootstrap operators for live environment and .lain IO.

  These operators only declare boundary requests.  They deliberately know
  nothing about files, sessions, compilation, or effect execution."
  (:require [propagators.compiler.model.operator-value :as operator-value]
            [propagators.runtime.boundary :as boundary]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.infra.cells.value :as value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]))

(defn- argument-values
  [network arg-ids]
  (mapv #(net/network-cell-strongest network %) arg-ids))

(defn effect-request-message
  [outbox-id effect-id kind payload receipt-id]
  (message outbox-id
           (obj/compound-object
            {(runtime-ids/effect-slot-key effect-id)
             (boundary/environment-effect-request
              effect-id kind payload receipt-id)})))

(defn effect-failure-message
  [receipt-id effect-id kind failure]
  (let [request (boundary/environment-effect-request
                 effect-id kind nil receipt-id)]
    (message receipt-id
             (obj/compound-object
              {(runtime-ids/receipt-slot-key effect-id)
               (boundary/environment-receipt
                request :failed
                {:diagnostics
                 [(select-keys failure [:reason :message :data])]})}))))

(defn- invalid-invocation
  [reason message data]
  {:status :invalid
   :reason reason
   :message message
   :data data})

(defn normalize-effect-invocation
  [{:effect/keys [symbol arities normalize] :as effect} context values]
  (cond
    (not (contains? arities (count values)))
    (invalid-invocation
     :invalid-arity
     (str symbol " received the wrong number of arguments")
     {:operator symbol :expected arities :actual (count values)})

    (some value/unusable? values)
    {:status :wait}

    :else
    (let [normalized (normalize context values)]
      (cond
        (= :ready (:status normalized))
        normalized

        (= :invalid (:status normalized))
        normalized

        :else
        (throw
         (ex-info "Effect normalizer returned an unsupported result"
                  {:effect (select-keys effect
                                        [:effect/symbol :boundary/kind])
                   :result normalized}))))))

(defn- valid
  [payload]
  {:status :ready :payload payload})

(defn- invalid
  [reason message data]
  (invalid-invocation reason message data))

(defn effect-operator
  "Build a named propagator operator whose only result is a boundary request.

  `normalize` validates and normalizes the strongest argument values.
  `identity-parts` chooses the idempotency identity independently of delivery."
  [{:effect/keys [symbol identity-parts outbox-id context]
    :boundary/keys [kind]
    :as effect}]
  (operator-value/operator-closure
   {:name symbol
    :output-selector (fn [_arg-ids fallback-id] fallback-id)
    :activate
    (fn [network _context-id arg-ids receipt-id]
      (let [arg-ids (vec arg-ids)
            values (argument-values network arg-ids)
            invocation (normalize-effect-invocation effect context values)]
        (case (:status invocation)
          :wait
          []

          :invalid
          (let [effect-id [kind :invalid arg-ids]]
            [(effect-failure-message
              receipt-id effect-id kind invocation)])

          :ready
          (let [payload (:payload invocation)
                effect-id (into [kind] (identity-parts payload))]
            [(effect-request-message
              outbox-id effect-id kind payload receipt-id)])

          (throw
           (ex-info "Unknown effect invocation status"
                    {:operator symbol :invocation invocation})))))}))

(defn load-primitive-environment-effect
  []
  {:effect/symbol 'load-primitive-environment
   :boundary/kind :environment/load-primitive-environment
   :boundary/port :environment
   :effect/arities #{2 3}
   :effect/handler-symbol
   'propagators.runtime.session.environment-io/load-primitive-environment
   :effect/normalize
   (fn [_context [file entry revision]]
     (cond
       (not (string? file))
       (invalid :invalid-file "load-primitive-environment expects a file path"
                {:file file})

       (not (qualified-keyword? entry))
       (invalid :invalid-entry "primitive entry must be a namespaced keyword"
                {:entry entry})

       :else
       (valid {:file file :entry entry :revision (or revision 0)})))
   :effect/identity-parts (juxt :file :entry :revision)})

(defn load-primitive-environment-operator
  [outbox-id]
  (effect-operator (assoc (load-primitive-environment-effect)
                          :effect/outbox-id outbox-id)))

(defn load-lain-effect
  []
  {:effect/symbol 'load-lain
   :boundary/kind :environment/load-lain
   :boundary/port :environment
   :effect/arities #{1 2}
   :effect/handler-symbol
   'propagators.runtime.session.environment-io/load-lain
   :effect/normalize
   (fn [_context [file revision]]
     (cond
       (string? file)
       (valid {:file file :revision (or revision 0)})

       :else
       (invalid :invalid-file "load-lain expects a file path" {:file file})))
   :effect/identity-parts (juxt :file :revision)})

(defn load-lain-operator
  [outbox-id]
  (effect-operator (assoc (load-lain-effect) :effect/outbox-id outbox-id)))

(defn save-environment-effect
  []
  {:effect/symbol 'save-environment
   :boundary/kind :environment/save-environment
   :boundary/port :environment
   :effect/arities #{3}
   :effect/handler-symbol
   'propagators.runtime.session.environment-io/save-environment
   :effect/normalize
   (fn [_context [file mode checkpoint-id]]
     (cond
       (not (string? file))
       (invalid :invalid-file "save-environment expects a file path" {:file file})

       (not (#{:preserve-premises :commit-supported} mode))
       (invalid :invalid-mode "unsupported environment save mode" {:mode mode})

       :else
       (valid {:file file :mode mode :checkpoint-id checkpoint-id})))
   :effect/identity-parts (juxt :file :mode :checkpoint-id)})

(defn save-environment-operator
  [outbox-id]
  (effect-operator (assoc (save-environment-effect)
                          :effect/outbox-id outbox-id)))

(defn load-blocks-effect
  []
  {:effect/symbol 'load-blocks
   :boundary/kind :environment/load-blocks
   :boundary/port :environment
   :effect/arities #{1 2}
   :effect/handler-symbol
   'propagators.runtime.session.environment-io/load-blocks
   :effect/normalize
   (fn [{:keys [client-id]} [file revision]]
     (cond
       (string? file)
       (valid {:file file :revision (or revision 0) :client-id client-id})

       :else
       (invalid :invalid-file "load-blocks expects a file path" {:file file})))
   :effect/identity-parts (juxt :client-id :file :revision)})

(defn load-blocks-operator
  [outbox-id client-id]
  (effect-operator (assoc (load-blocks-effect)
                          :effect/outbox-id outbox-id
                          :effect/context {:client-id client-id})))

(defn save-blocks-effect
  []
  {:effect/symbol 'save-blocks
   :boundary/kind :environment/save-blocks
   :boundary/port :environment
   :effect/arities #{4}
   :effect/handler-symbol
   'propagators.runtime.session.environment-io/save-blocks
   :effect/normalize
   (fn [{:keys [client-id]} [file mode selection checkpoint-id]]
     (cond
       (not (string? file))
       (invalid :invalid-file "save-blocks expects a file path" {:file file})

       (not (#{:source :preserve-premises :commit-supported} mode))
       (invalid :invalid-mode "unsupported block save mode" {:mode mode})

       (not (or (= :all selection)
                (and (vector? selection) (every? integer? selection))))
       (invalid :invalid-selection
                "block selection must be :all or a vector of indexes"
                {:selection selection})

       :else
       (valid {:file file
               :mode mode
               :selection selection
               :checkpoint-id checkpoint-id
               :client-id client-id})))
   :effect/identity-parts
   (juxt :client-id :file :mode :selection :checkpoint-id)})

(defn save-blocks-operator
  [outbox-id client-id]
  (effect-operator (assoc (save-blocks-effect)
                          :effect/outbox-id outbox-id
                          :effect/context {:client-id client-id})))
