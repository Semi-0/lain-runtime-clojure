(ns propagators.runtime.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.runtime.api :as api]))

(deftest runtime-api-loads-and-inspects-program
  (let [session (api/create-session {})]
    (api/load-program session "42")
    (let [graph (api/inspect session {:kind :semantic-graph})]
      (is (= 1 (:schema-version graph)))
      (is (map? (:nodes graph)))
      (is (vector? (:edges graph)))
      (is (map? (:aliases graph))))))

(deftest runtime-api-rejects-invalid-input
  (testing "session options must be a map"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"options must be a map"
                          (api/create-session []))))
  (testing "program source must be a string"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"source must be a string"
                          (api/load-program (api/create-session {}) 42))))
  (testing "commands require an operation"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"keyword :op"
                          (api/dispatch-command (api/create-session {}) {}))))
  (testing "unknown inspection requests are explicit"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"unsupported runtime inspection"
                          (api/inspect (api/create-session {})
                                       {:kind :unknown})))))

(deftest unknown-operator-group-is-explicit
  (let [session (api/create-session
                 {:operator-groups [:unknown]
                  :operator-providers {}})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"unsupported operator group"
                          (api/load-program session "42")))))
