(ns propagators.runtime.session.block-compiler
  "Block-specific term rewriting over the canonical CPS compiler.

  Applications are rewritten into ordinary dependency-declaration terms. The
  declaration watches the compiled application's raw result and premise cells,
  records dependency metadata on that result, and returns the raw binding."
  (:require [meander.epsilon :as m]
            [propagators.compiler.cps-core :as compiler]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.closure-value :as closure-value]
            [propagators.compiler.model.env :as env]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.operators.block-premise :as premise]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.compiler.lowering.application :as application]
            [propagators.compiler.common.cps :as cps]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]))

(def dependency-term-name :compiler-2/block-application-dependence)
(def supported-input-term-name :compiler-2/block-supported-input)
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

(defn- application-input-ids
  [network topology]
  (let [operator-id (:operator-id topology)
        operator
        (cond
          (contains? (net/net-env network) operator-id)
          (net/network-cell-strongest network operator-id)

          :else
          nil)
        declaration (operator-value/operator-declaration operator)
        argument-ids (:argument-ids topology)]
    (cond
      (closure-value/closure-info? declaration)
      (vec (take (count (closure-value/closure-inputs declaration))
                 argument-ids))

      :else
      argument-ids)))

(defn- application-contexts
  [network topology context]
  (let [operator-id (:operator-id topology)
        input-ids (application-input-ids network topology)]
    (reduce into #{context}
            (map #(premise/binding-contexts network %)
                 (into [operator-id] input-ids)))))

(defn- declare-application-dependence
  [state topology binding context]
  (let [network (:net state)
        application-id (:application-id topology)
        binding-id (env/binding-id binding)
        contexts (application-contexts network topology context)
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
     (let [application-id
           (cond
             (< application-count (count (:applications state)))
             (peek (:applications state))

             :else
             nil)
           topology
           (cond
             application-id
             (or (application/application-topology (:net state) application-id)
                 (throw
                  (ex-info "Compiled application topology is unavailable"
                           {:application-id application-id})))

             :else
             nil)
           state (cond
                   topology
                   (declare-application-dependence
                    state topology binding context)

                   :else
                   (record-term-context state binding context))]
       (cps/continue k state binding))))))

(defn dependency-term-operator
  [context]
  (operator-value/operator-closure
   {:name dependency-term-name
    :direct-compiler (partial compile-dependency-term context)}))

(defn- declare-supported-input
  [state binding context]
  (let [binding-id (env/binding-id binding)
        contexts (into #{context} (premise/binding-contexts (:net state)
                                                            binding-id))
        claim-id [:block-supported-input (:seed state) (:path state)]
        out-id (premise/application-dependence-cell-id claim-id)
        network (nb/ensure-cell (:net state) out-id)
        [prop-id installed]
        ((premise/p:block-premise claim-id binding-id contexts out-id) network)]
    [(-> state
         (assoc :net (premise/record-binding-contexts installed out-id contexts))
         (update :props conj prop-id))
     (env/cell-binding out-id)]))

(defn- compile-supported-input
  [context compile-k state operand-forms _unused-out-id k]
  (when-not (= 1 (count operand-forms))
    (throw (ex-info "block supported input expects one expression"
                    {:operands operand-forms})))
  (cps/call
   compile-k state (first operand-forms)
   (fn [state binding]
     (let [[state' supported] (declare-supported-input state binding context)]
       (cps/continue k state' supported)))))

(defn supported-input-term-operator [context]
  (operator-value/operator-closure
   {:name supported-input-term-name
    :direct-compiler (partial compile-supported-input context)}))

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

(defn- named-literal-operator?
  [expr name]
  (cond
    (not= :apply (ast/type expr))
    false

    (not= :literal (ast/type (ast/operator expr)))
    false

    :else
    (= name (operator-value/operator-name
             (ast/value (ast/operator expr))))))

(defn dependency-term? [expr]
  (named-literal-operator? expr dependency-term-name))

(defn definition-term? [expr]
  (named-literal-operator? expr definition-term-name))

(defn supported-input-term? [expr]
  (named-literal-operator? expr supported-input-term-name))

(declare rewrite-expr*)

(defn- named-symbol-application? [expr names]
  (and (= :apply (ast/type expr))
       (= :symbol (ast/type (ast/operator expr)))
       (contains? names (ast/name (ast/operator expr)))))

(defn- block-cell-application? [expr]
  (named-symbol-application? expr '#{block block-at be:block be:block-at}))

(defn- block-write-application? [expr]
  (named-symbol-application? expr '#{be:block be:block-at}))

(defn- block-transport? [expr]
  (and (named-symbol-application? expr '#{-> <->})
       (block-cell-application? (peek (vec (ast/args expr))))))

(defn- supported-input-term [context expr]
  (if (supported-input-term? expr)
    expr
    (ast/app (ast/lit (supported-input-term-operator context)) expr)))

(defn- rewrite-application-args [context expr]
  (let [args (mapv #(rewrite-expr* context false %) (ast/args expr))]
    (cond
      (and (block-transport? expr) (<= 2 (count args)))
      (update args (- (count args) 2) #(supported-input-term context %))

      (and (block-write-application? expr) (seq args))
      (update args (dec (count args)) #(supported-input-term context %))

      :else
      args)))

(defn- rewrite-application
  [context expr]
  (if (or (dependency-term? expr) (supported-input-term? expr))
    expr
    (let [application (apply ast/app
                             (rewrite-expr* context false (ast/operator expr))
                             (rewrite-application-args context expr))]
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
