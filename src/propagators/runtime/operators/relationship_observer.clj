(ns propagators.runtime.operators.relationship-observer
  "Compiler-2 direct installers for native relationship observers."
  (:require [propagators.compiler.common.cps :as cps]
            [clojure.set :as set]
            [propagators.compiler.compiler.basis :as h]
            [propagators.compiler.compiler.dispatch :as compiler-dispatch]
            [propagators.compiler.model.env :as cenv]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.network :as net]
            [propagators.infra.propagator :as prop]
            [propagators.compiler.relationship-dataflow :as dataflow]
            [propagators.infra.relationship-observer :as observer]))

(defn- compile-form
  [state form role]
  (let [compile* (compiler-dispatch/state-compiler state)
        [state' binding] (compile* (h/child state role) form)]
    [(assoc state' :path (:path state)) binding]))

(defn- propagator-ids
  [network]
  (into #{}
        (keep (fn [[id entry]]
                (when (prop/prop? entry) id)))
        (net/net-env network)))

(defn- observed-and-output-cells
  [state operand-forms]
  (when-not (<= 2 (count operand-forms))
    (throw (ex-info "relationship:roots expects observed cells followed by one output cell"
                    {:operand-forms operand-forms})))
  (let [[compiled bindings]
        (reduce (fn [[current bindings] [index form]]
                  (let [[next-state binding]
                        (compile-form current form [:relationship-roots index])]
                    [next-state (conj bindings binding)]))
                [state []]
                (map-indexed vector operand-forms))
        ids (mapv cenv/binding-id bindings)]
    (when-not (every? some? ids)
      (throw (ex-info "relationship:roots operands must compile to cells"
                      {:operand-forms operand-forms})))
    [compiled (vec (butlast ids)) (last ids)]))

(defn roots-operator
  []
  (operator-value/operator-closure
   {:name 'relationship:roots
    :compiler-operands
    (fn [_compile-k state operand-forms _out-id k]
      (let [[next-state binding]
            (do
      (let [[state' observed-ids output-id]
            (observed-and-output-cells state operand-forms)
            before (propagator-ids (:net state'))
            [_ installed]
            ((observer/p:observe-roots observed-ids output-id) (:net state'))
            introduced (set/difference (propagator-ids installed) before)]
        [(-> state'
             (assoc :net installed)
             (h/add-props (sort-by pr-str introduced)))
         (cenv/cell-binding output-id)]))]
        (cps/continue k next-state binding)))}))

(defn dataflow-operator []
  (operator-value/operator-closure
   {:name 'relationship:dataflow
    :compiler-operands
    (fn [_compile-k state forms _out-id k]
      (let [[next-state binding]
            (do
      (when-not (= 2 (count forms))
        (throw (ex-info "relationship:dataflow expects input and output cells"
                        {:operand-forms forms})))
      (let [[compiled [input-id] output-id] (observed-and-output-cells state forms)
            [id installed] ((dataflow/p:dataflow input-id output-id) (:net compiled))]
        [(-> compiled (assoc :net installed) (h/add-props [id]))
         (cenv/cell-binding output-id)]))]
        (cps/continue k next-state binding)))}))
