(ns propagators.compiler-2.runtime.tui.block-compiler-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.tui.block-compiler :as block-compiler]
            [propagators.compiler-2.language.parser :as parser]
            [propagators.compiler-2.operators.block-premise :as premise]
            [propagators.compiler-2.operators.versioned-definition :as definition]
            [propagators.network :as net]))

(defn request [id text]
  {:commit-id id :client-id "A" :index 0
   :expected-version nil :text text})

(deftest definition-and-application-rewriting-is-idempotent
  (let [context (premise/premise-context :block 0)
        forms ["(def-net f [x] [out] (-> x out))"
               "(def f (cell-expr [x] (+ x 1)))"
               "(def-constraint same [x y] (<-> x y))"
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
              (str "(def outer (network [] [out] "
                   "(def inner 1) (-> 2 out)))")))
    (let [candidates (vals (net/network-dict-entry
                            (:program/net @session) definition/candidates-key))]
      (is (= ['outer] (mapv :candidate/name candidates))))))

(deftest callable-definition-lowerings-record-signatures
  (doseq [[source expected]
          [["(def-net f [x] [out] (-> x out))"
            {:inputs 1 :outputs 1 :implicit? false}]
           ["(def f (cell-expr [x] (+ x 1)))"
            {:inputs 1 :outputs 0 :implicit? true}]
           ["(def-constraint f [x y] (<-> x y))"
            {:inputs 2 :outputs 0 :implicit? true}]]]
    (testing source
      (let [session (runtime/new-session)]
        (runtime/register-tui! session {:client-id "A"
                                        :mode :versioned-premise})
        (runtime/commit-version!
         session
         (request "00000000-0000-0000-0000-000000000001" source))
        (let [signature (-> (net/network-dict-entry
                             (:program/net @session) definition/candidates-key)
                            vals first :candidate/signature)]
          (is (= expected (select-keys signature (keys expected)))))))))
