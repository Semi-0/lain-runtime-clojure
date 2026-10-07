(ns propagators.runtime.session.block-compiler-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.block-compiler :as block-compiler]
            [propagators.compiler.language.parser :as parser]
            [propagators.compiler.operators.block-premise :as premise]
            [propagators.compiler.operators.versioned-definition :as definition]
            [propagators.infra.network :as net]))

(defn request [id text]
  {:commit-id id :client-id "A" :index 0
   :expected-version nil :text text})

(deftest definition-and-application-rewriting-is-idempotent
  (let [context (premise/premise-context :block 0)
        forms ["(define f (network [x out] (-> x out) (list out)))"
               "(define f (network [x] (+ x 1)))"
               "(define same (network [x y] (<-> x y) (list x y)))"
               "(+ 1 2)"]]
    (doseq [source forms
            :let [once (block-compiler/rewrite-expr
                        context (parser/parse-string source))]]
      (is (= once (block-compiler/rewrite-expr context once)) source))))

(deftest closure-body-definitions-are-not-global-version-candidates
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (request "00000000-0000-0000-0000-000000000001"
              "(define outer (network () (define inner 1) 2))"))
    (let [candidates (vals (net/network-dict-entry
                            (:program/net @session) definition/candidates-key))]
      (is (= ['outer] (mapv :candidate/name candidates))))))

(defn assert-callable-signature
  [source expected]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"
                                    :mode :versioned-premise})
    (runtime/commit-version!
     session
     (request "00000000-0000-0000-0000-000000000001" source))
    (let [signature (-> (net/network-dict-entry
                         (:program/net @session) definition/candidates-key)
                        vals first :candidate/signature)]
      (is (= expected (select-keys signature (keys expected)))))))

(deftest def-net-lowering-records-signature
  (assert-callable-signature
   "(define f (network [x out] (-> x out) (list out)))"
   {:inputs 2 :outputs 0 :implicit? true}))

(deftest cell-expression-lowering-records-signature
  (assert-callable-signature
   "(define f (network [x] (+ x 1)))"
   {:inputs 1 :outputs 0 :implicit? true}))

(deftest constraint-lowering-records-signature
  (assert-callable-signature
   "(define f (network [x y] (<-> x y) (list x y)))"
   {:inputs 2 :outputs 0 :implicit? true}))
