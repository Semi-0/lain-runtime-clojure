(ns propagators.runtime.semantic-application-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler.cps-core :as compiler]
            [propagators.runtime.inspection.semantic-graph :as semantic]
            [propagators.infra.network-builder :as nb]))

(deftest returned-member-boundaries-are-inspectable-without-mutation
  (let [compiled (compiler/compile-source
                  "(let-cell [a b] ((network (a b) (-> 2 a) (-> 3 b) (list a b)) a b) b)")
        network (nb/run-propagators (:net compiled) (:props compiled))
        graph (semantic/compiled-semantic-graph compiled network)
        edges (set (semantic/edge-labels graph))]
    (is (contains? edges ["call network [a b]" "a"]))
    (is (contains? edges ["call network [a b]" "b"]))
    (is (not-any? #{"slot a" "slot b"} (vals (:nodes graph))))
    (is (= graph (semantic/compiled-semantic-graph compiled network)))))

(deftest scalar-return-keeps-its-primary-output
  (let [compiled (compiler/compile-source
                  "(let [] (define inc (network (x) (+ x 1))) (inc 4))")
        network (nb/run-propagators (:net compiled) (:props compiled))
        graph (semantic/compiled-application-semantic-graph compiled network)
        edges (set (semantic/edge-labels graph))]
    (is (contains? edges ["4" "call inc"]))
    (is (contains? edges ["call inc" "result"]))
    (is (= graph (semantic/compiled-application-semantic-graph compiled network)))))
