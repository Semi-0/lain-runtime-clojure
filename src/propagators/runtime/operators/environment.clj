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

(defn- request-message
  [outbox-id effect-id kind payload receipt-id]
  (message outbox-id
           (obj/compound-object
            {(runtime-ids/effect-slot-key effect-id)
             (boundary/environment-effect-request
              effect-id kind payload receipt-id)})))

(defn- usable-arguments?
  [values]
  (not-any? value/unusable? values))

(defn effect-operator
  "Build a named propagator operator whose only result is a boundary request.

  `arguments->payload` validates and normalizes the strongest argument values.
  `identity-parts` chooses the idempotency identity independently of delivery."
  [{:keys [name kind arities arguments->payload identity-parts outbox-id]}]
  (operator-value/operator-closure
   {:name name
    :output-selector (fn [_arg-ids fallback-id] fallback-id)
    :activate
    (fn [network _context-id arg-ids receipt-id]
      (let [arg-ids (vec arg-ids)
            values (argument-values network arg-ids)]
        (when-not (contains? arities (count arg-ids))
          (throw (ex-info (str name " received the wrong number of arguments")
                          {:operator name
                           :expected arities
                           :arg-ids arg-ids})))
        (if-not (usable-arguments? values)
          []
          (let [payload (arguments->payload values)
                effect-id (into [kind] (identity-parts payload))]
            [(request-message outbox-id effect-id kind payload receipt-id)]))))}))

(defn load-primitive-environment-operator
  [outbox-id]
  (effect-operator
   {:name 'load-primitive-environment
    :kind :environment/load-primitive-environment
    :arities #{2 3}
    :outbox-id outbox-id
    :arguments->payload
    (fn [[file entry revision]]
      (when-not (string? file)
        (throw (ex-info "load-primitive-environment expects a file path"
                        {:file file})))
      (when-not (qualified-keyword? entry)
        (throw (ex-info "primitive entry must be a namespaced keyword"
                        {:entry entry})))
      {:file file :entry entry :revision (or revision 0)})
    :identity-parts (juxt :file :entry :revision)}))

(defn load-lain-operator
  [outbox-id]
  (effect-operator
   {:name 'load-lain
    :kind :environment/load-lain
    :arities #{1 2}
    :outbox-id outbox-id
    :arguments->payload
    (fn [[file revision]]
      (when-not (string? file)
        (throw (ex-info "load-lain expects a file path" {:file file})))
      {:file file :revision (or revision 0)})
    :identity-parts (juxt :file :revision)}))

(defn save-environment-operator
  [outbox-id]
  (effect-operator
   {:name 'save-environment
    :kind :environment/save-environment
    :arities #{3}
    :outbox-id outbox-id
    :arguments->payload
    (fn [[file mode checkpoint-id]]
      (when-not (string? file)
        (throw (ex-info "save-environment expects a file path" {:file file})))
      (when-not (#{:preserve-premises :commit-supported} mode)
        (throw (ex-info "unsupported environment save mode" {:mode mode})))
      {:file file :mode mode :checkpoint-id checkpoint-id})
    :identity-parts (juxt :file :mode :checkpoint-id)}))

(defn load-blocks-operator
  [outbox-id client-id]
  (effect-operator
   {:name 'load-blocks
    :kind :environment/load-blocks
    :arities #{1 2}
    :outbox-id outbox-id
    :arguments->payload
    (fn [[file revision]]
      (when-not (string? file)
        (throw (ex-info "load-blocks expects a file path" {:file file})))
      {:file file :revision (or revision 0) :client-id client-id})
    :identity-parts (juxt :client-id :file :revision)}))

(defn save-blocks-operator
  [outbox-id client-id]
  (effect-operator
   {:name 'save-blocks
    :kind :environment/save-blocks
    :arities #{4}
    :outbox-id outbox-id
    :arguments->payload
    (fn [[file mode selection checkpoint-id]]
      (when-not (string? file)
        (throw (ex-info "save-blocks expects a file path" {:file file})))
      (when-not (#{:source :preserve-premises :commit-supported} mode)
        (throw (ex-info "unsupported block save mode" {:mode mode})))
      (when-not (or (= :all selection)
                    (and (vector? selection) (every? integer? selection)))
        (throw (ex-info "block selection must be :all or a vector of indexes"
                        {:selection selection})))
      {:file file
       :mode mode
       :selection selection
       :checkpoint-id checkpoint-id
       :client-id client-id})
    :identity-parts (juxt :client-id :file :mode :selection :checkpoint-id)}))
