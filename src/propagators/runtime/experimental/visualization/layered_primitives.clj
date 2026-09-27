(ns propagators.runtime.experimental.visualization.layered-primitives
  "Opt-in primitive composition; no compiler or layered-runtime changes."
  (:require [clojure.set :as set]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator]
            [propagators.runtime.session.extension :as extension]
            [propagators.infra.datastructures.dependency :as dependency]
            [propagators.infra.datastructures.scope-source :as scope]
            [propagators.infra.datastructures.tms.core :as tms]
            [propagators.infra.ids :as ids]
            [propagators.infra.layered :as layered]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]
            [propagators.infra.stdlib.arithmetic.base :as base]
            [propagators.infra.stdlib.prop :as standard]))

(defn- input-value
  "Expose the arithmetic base and dependency layer, without source write-back."
  [v]
  (let [unwrapped (tms/distributed-base-value (scope/unwrap v))]
    (cond
      (value/unusable? unwrapped) unwrapped
      :else
      (dependency/dependency-value
       (dependency/unwrap unwrapped)
       (set/union (dependency/sources unwrapped)
                  (if (scope/scope-value? v) (scope/dependencies v) #{}))))))

(def sources-closure
  {:net net/empty-net
   :f (fn [_closure-net inputs outputs network]
        (let [union-sources
              (prop/primitive-propagator
               ::union-sources
               (fn [_current & arguments]
                 (apply set/union #{} (map dependency/sources arguments))))]
          (second ((apply union-sources (concat inputs outputs)) network))))})

(defn- install-procedure
  [network base-closure]
  (let [[procedure base-id sources-id] (repeatedly 3 ids/new-node-id)
        prepared (-> network
                     (nb/install-cell base-id base-closure base-closure)
                     (nb/install-cell sources-id sources-closure sources-closure))
        base-layer (layered/install-layered-procedure!
                    prepared procedure :base base-id)
        sources-layer (layered/install-layered-procedure!
                       (:net base-layer) procedure dependency/sources-layer sources-id)]
    {:net (:net sources-layer) :procedure procedure}))

(defn- output-messages
  [network arguments layered-result output]
  ;; Readiness is owned by the enclosing concrete-propagator, not this adapter.
  (let [result (net/network-cell-strongest network layered-result)
        contents (mapv #(net/network-cell-content network %) arguments)
        payload (dependency/dependency-value
                 (dependency/unwrap result) (dependency/sources result))
        update (tms/distributed-result-update [::result output] payload contents)]
    [(message output (if (some? update) update payload))]))

(defn install-call
  "Declare input adapters -> layered application -> support-preserving output.
  Argument adapters are one-way. All persistent state lives in ordinary cells."
  [network arguments output base-closure]
  (let [arguments (vec arguments)]
    (when-not (= 2 (count arguments))
      (throw (ex-info "Tracked arithmetic expects two arguments"
                      {:arguments arguments})))
    (let [{:keys [net procedure]} (install-procedure network base-closure)
          adapted (vec (repeatedly 2 ids/new-node-id))
          result (ids/new-node-id)
          prepared (reduce nb/ensure-cell net (concat arguments adapted [output result]))
          installers (concat
                      (map (fn [source target]
                             ((prop/primitive-propagator ::input input-value) source target))
                           arguments adapted)
                      [(layered/p:apply-layered ::apply procedure adapted result)
                       (prop/construct-propagator
                        ::output
                        (prop/concrete-propagator
                         (fn [_inputs _outputs current]
                           (output-messages current arguments result output)))
                        (conj arguments result) [output])])
          [installed props]
          (reduce (fn [[current props] installer]
                    (let [[id next] (installer current)]
                      [next (conj props id)]))
                  [prepared []] installers)]
      [installed props output])))

(def tracked-plus
  (operator/operator-closure
   {:name ::tracked-plus
    :install (fn [network arguments output]
               (install-call network arguments output base/plus-closure))}))

(def tracked-or
  (operator/operator-closure
   {:name ::tracked-or
    :install (fn [network arguments output]
               (install-call network arguments output
                             (base/arithmetic-base-closure standard/or)))}))

(def session-extension
  (extension/extension-bundle
   {:id ::primitives
    :bindings [['tracked+ tracked-plus] ['tracked-or tracked-or]]
    :effects []}))
