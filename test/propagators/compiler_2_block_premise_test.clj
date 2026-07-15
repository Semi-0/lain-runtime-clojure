(ns propagators.compiler-2-block-premise-test
  (:require [clojure.test :refer [deftest is]]
            [graph.compiler-2-runtime.block-compiler :as block-compiler]
            [graph.compiler-2-runtime.state :as runtime-state]
            [propagators.cells.value :as value]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.language.ast :as ast]
            [propagators.compiler-2.language.parser :as parser]
            [propagators.compiler-2.operators.block-premise :as premise]
            [propagators.core :as core]
            [propagators.datastructures.event :as event]
            [propagators.datastructures.tms.distributed :as tms]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]))

(deftest application-dependence-is-an-idempotent-term-rewrite
  (let [context (premise/premise-context :block 0)
        rewritten (block-compiler/rewrite-expr
                   context (parser/parse-string "(+ 1 (* 2 3))"))
        application (first (ast/args rewritten))
        nested (second (ast/args application))]
    (is (block-compiler/dependency-term? rewritten))
    (is (= '+ (ast/name (ast/operator application))))
    (is (block-compiler/dependency-term? nested))
    (is (= rewritten (block-compiler/rewrite-expr context rewritten)))))

(defn run-message
  [network m]
  (let [[tasks network] (core/eval-cells [m] network)]
    (core/run-tasks tasks network)))

(defn active-premises
  [network id]
  (-> network
      (net/network-cell-content id)
      tms/distributed-slots
      tms/tms-view
      tms/active-premises))

(defn semantic-value
  [network id]
  (-> (h/strongest-or-nothing network id)
      tms/strongest-distributed-value
      tms/distributed-base-value))

(deftest block-premise-gate-is-reactive-and-support-preserving
  (let [value-id (ids/new-node-id)
        state-id (ids/new-node-id)
        out-id (ids/new-node-id)
        context {:premise/id [:block :v0]
                 :premise/epoch 0
                 :premise/state-cell state-id}
        upstream (tms/distributed-input-update :upstream 4 :upstream/p 0 :test)
        n0 (-> (runtime-state/runtime-base-net)
               (nb/install-cell value-id upstream upstream)
               (nb/install-cell state-id
                                (premise/active-update context)
                                (premise/active-update context))
               (nb/ensure-cell out-id))
        [prop-id n1] ((premise/p:block-premise :claim value-id [context] out-id)
                      n0)
        n2 (nb/run-propagators n1 [prop-id])]
    (is (= 4 (semantic-value n2 out-id)))
    (is (= #{:upstream/p [:block :v0]} (active-premises n2 out-id)))
    (is (not (event/event-content? (net/network-cell-content n2 out-id))))
    (let [n3 (run-message n2 (message state-id
                                      (premise/retract-update context 1)))]
      (is (value/nothing? (semantic-value n3 out-id)))
      (is (= #{:upstream/p} (active-premises n3 out-id))
          "monotone upstream evidence remains even when the gate is inactive"))))

(deftest duplicate-gate-activation-is-idempotent
  (let [context (premise/premise-context :block 0)
        value-id (ids/new-node-id)
        out-id (ids/new-node-id)
        n0 (-> (runtime-state/runtime-base-net)
               (nb/install-cell value-id 7 7)
               (nb/install-cell (:premise/state-cell context)
                                (premise/active-update context)
                                (premise/active-update context))
               (nb/ensure-cell out-id))
        [prop-id n1] ((premise/p:block-premise :claim value-id [context] out-id)
                      n0)
        n2 (nb/run-propagators n1 [prop-id])
        before (net/network-cell-content n2 out-id)
        n3 (nb/run-propagators n2 [prop-id])]
    (is (= before (net/network-cell-content n3 out-id)))
    (is (= #{(:premise/id context)} (active-premises n3 out-id)))))
