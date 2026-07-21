(ns propagators.compiler-2.runtime.tui.block-compiler
  "Block-specific term rewriting over the canonical CPS compiler.

  Applications are rewritten into ordinary dependency-declaration terms. The
  declaration watches the compiled application's raw result and premise cells,
  records dependency metadata on that result, and returns the raw binding."
  (:require [meander.epsilon :as m]
            [propagators.compiler-2.cps-core :as compiler]
            [propagators.compiler-2.language.ast :as ast]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.model.env :as env]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.compiler-2.operators.block-premise :as premise]
            [propagators.compiler-2.operators.versioned-definition :as definition]
            [propagators.compiler-common.cps :as cps]
            [propagators.datastructures.compound-object :as obj]
            [propagators.network :as net]
            [propagators.network-builder :as nb]))

(def dependency-term-name :compiler-2/block-application-dependence)
(def definition-term-name :compiler-2/block-versioned-definition)

(defn settle-current-props
  "The compiler already ran current topology to equilibrium; do not replay it."
  [network _prop-ids]
  network)

(defn deferred-semantic-graph
  "Ordinary versioned commits retain topology receipts and project on trace."
  [_compiled _network]
  {:nodes {}
   :node-aliases {}
   :values {}
   :node-ui {}
   :expansions {}
   :edges []})

(defn- application-contexts
  [network application context]
  (let [operator-id (obj/slot-value
                     application application-value/application-operator-cell-slot)
        arg-ids (obj/slot-value
                 application application-value/application-arg-cells-slot)]
    (reduce into #{context}
            (map #(premise/binding-contexts network %)
                 (into [operator-id] arg-ids)))))

(defn- declare-application-dependence
  [state application-id binding context]
  (let [network (:net state)
        binding-id (env/binding-id binding)
        application (net/network-cell-strongest network application-id)
        contexts (application-contexts network application context)
        dependence-id (premise/application-dependence-cell-id application-id)
        network (nb/ensure-cell network dependence-id)
        [prop-id installed]
        ((premise/p:application-dependence
          application-id binding-id contexts dependence-id)
         network)]
    (-> state
        (assoc :net (premise/record-application-dependence
                     installed binding-id application-id contexts dependence-id))
        (update :props conj prop-id))))

(defn- record-term-context
  [state binding context]
  (update state :net premise/record-binding-contexts
          (env/binding-id binding) #{context}))

(defn- compile-dependency-term
  [context compile-k state operand-forms _unused-out-id k]
  (when-not (= 1 (count operand-forms))
    (throw (ex-info "block application dependence expects one application term"
                    {:operands operand-forms})))
  (let [application-count (count (:applications state))]
    (cps/call
     compile-k state (first operand-forms)
     (fn [state binding]
       (let [application-id (when (< application-count
                                      (count (:applications state)))
                              (peek (:applications state)))
             state (if application-id
                     (declare-application-dependence
                      state application-id binding context)
                     (record-term-context state binding context))]
         (cps/continue k state binding))))))

(defn dependency-term-operator
  [context]
  (operator-value/operator-closure
   {:name dependency-term-name
    :direct-compiler (partial compile-dependency-term context)}))

(defn- compile-definition-term
  [name signature explicit compile-k state operand-forms _out-id k]
  (when-not (= 1 (count operand-forms))
    (throw (ex-info "versioned definition expects one candidate term"
                    {:name name :operands operand-forms})))
  (cps/call
   compile-k state (first operand-forms)
   (fn [state binding]
     (let [[state public]
           (definition/declare-candidate (:compiler state) state name binding
                                         signature explicit)]
       (cps/continue k state public)))))

(defn definition-term-operator [name signature explicit]
  (operator-value/operator-closure
   {:name definition-term-name
    :direct-compiler
    (partial compile-definition-term name signature explicit)}))

(defn- named-literal-operator? [expr name]
  (and (= :apply (ast/type expr))
       (let [operator (ast/operator expr)]
         (and (= :literal (ast/type operator))
              (= name
                 (obj/slot-value (ast/value operator)
                                 operator-value/name-slot))))))

(defn dependency-term? [expr]
  (named-literal-operator? expr dependency-term-name))

(defn definition-term? [expr]
  (named-literal-operator? expr definition-term-name))

(declare rewrite-expr*)

(defn- rewrite-application
  [context expr]
  (if (dependency-term? expr)
    expr
    (let [application (apply ast/app
                             (rewrite-expr* context false (ast/operator expr))
                             (map #(rewrite-expr* context false %) (ast/args expr)))]
      (ast/app (ast/lit (dependency-term-operator context)) application))))

(defn- callable-signature [inputs outputs implicit?]
  {:inputs (count inputs)
   :outputs (count outputs)
   :input-names (vec inputs)
   :output-names (vec outputs)
   :implicit? implicit?})

(defn- explicit-premise [body]
  (when (and (= :apply (ast/type body))
             (= :symbol (ast/type (ast/operator body)))
             (#{'premise-closure 'distributed-premise-closure}
              (ast/name (ast/operator body)))
             (= 3 (count (ast/args body))))
    {:operator (ast/name (ast/operator body))
     :premise-form (second (ast/args body))
     :epoch-form (nth (ast/args body) 2)}))

(defn- network-signature [expr]
  (case (ast/type expr)
    :network (callable-signature (ast/inputs expr) [] true)
    :compound (callable-signature (ast/inputs expr) (ast/output expr) false)
    nil))

(defn- body-signature [body]
  (or (network-signature body)
      (when (explicit-premise body)
        (network-signature (first (ast/args body))))))

(defn- definition-term [name signature explicit candidate]
  (ast/app (ast/lit (definition-term-operator name signature explicit))
           candidate))

(defn- rewrite-definition [context expr]
  (let [name (ast/name expr)]
    (case (ast/type expr)
      :def-net
      (definition-term
       name
       (callable-signature (ast/inputs expr) (ast/output expr) false)
       nil
       (ast/compound {:inputs (ast/inputs expr) :output (ast/output expr)}
                     (rewrite-expr* context false (ast/body expr))))

      :def-cell
      (definition-term
       name
       (callable-signature (ast/inputs expr) [] true)
       nil
       (ast/network (ast/inputs expr)
                    (rewrite-expr* context false (ast/body expr))))

      :def-constraint
      (let [inputs (ast/inputs expr)
            body (rewrite-expr* context false (ast/body expr))
            result (if-let [last-input (peek inputs)]
                     (ast/sequence* body (ast/sym last-input))
                     body)]
        (definition-term name (callable-signature inputs [] true) nil
                         (ast/network inputs result)))

      :def
      (if-let [body (ast/body expr)]
        (let [explicit (explicit-premise body)]
          (definition-term name
                           (or (body-signature body)
                               {:inputs 0 :outputs 0 :implicit? true
                                :scalar? true})
                           explicit
                           (rewrite-expr* context false body)))
        ;; Storage declarations remain ordinary cells.
        expr))))

(defn rewrite-expr* [context block-level? expr]
  (m/match (ast/type expr)
    :apply (if (or (dependency-term? expr) (definition-term? expr))
             expr
             (rewrite-application context expr))
    :sequence (apply ast/sequence*
                     (map #(rewrite-expr* context block-level? %) (ast/body expr)))
    :let-cell (ast/let-cell (ast/names expr)
                            (rewrite-expr* context block-level? (ast/body expr)))
    :let (ast/let* (mapv (fn [[name value]]
                           [name (rewrite-expr* context block-level? value)])
                         (ast/bindings expr))
                    (rewrite-expr* context block-level? (ast/body expr)))
    :when-topology (ast/when-topology
                    (rewrite-expr* context block-level? (ast/condition expr))
                    (rewrite-expr* context block-level? (ast/body expr)))
    :network (ast/network (ast/inputs expr)
                          (rewrite-expr* context false (ast/body expr)))
    :compound (ast/compound {:inputs (ast/inputs expr)
                             :output (ast/output expr)}
                            (rewrite-expr* context false (ast/body expr)))
    :def-net (if block-level? (rewrite-definition context expr) expr)
    :def-constraint (if block-level? (rewrite-definition context expr) expr)
    :def (if block-level? (rewrite-definition context expr) expr)
    :def-cell (if block-level? (rewrite-definition context expr) expr)
    _ expr))

(defn rewrite-expr
  "Idempotently rewrite block-level definitions and application dependencies."
  [context expr]
  (rewrite-expr* context true expr))

(defn default-compiler
  [state expr]
  (if-let [context (:block/premise-context state)]
    (compiler/default-compiler state (rewrite-expr context expr))
    (compiler/default-compiler state expr)))
