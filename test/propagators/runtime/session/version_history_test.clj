(ns propagators.runtime.session.version-history-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.runtime.session.version-history :as history]
            [propagators.compiler.operators.block-premise :as premise]))

(def block-id :block/a)

(defn request
  [commit-id expected-version text]
  {:commit-id commit-id
   :client-id "A"
   :index 0
   :expected-version expected-version
   :text text})

(defn record-for
  [commit-id expected-version text]
  (let [version (if-some [v expected-version] (inc v) 0)]
    (history/commit-record
     {:commit-id commit-id
      :client-id "A"
      :index 0
      :expected-version expected-version
      :source text
      :context (premise/premise-context block-id version)
      :retracts (when-some [v expected-version]
                  (premise/premise-id block-id v))})))

(deftest append-only-version-decisions
  (let [empty-block {:block-id block-id :version-history []}
        r0 (record-for "c0" nil "(+ 1 2)")
        b0 (history/append-version empty-block r0)
        r1 (record-for "c1" 0 "(+ 2 3)")
        b1 (history/append-version b0 r1)]
    (is (= {:decision :append :version 0}
           (history/decision empty-block (request "c0" nil "(+ 1 2)"))))
    (is (= :replay (:decision (history/decision b1
                                                 (request "c0" nil "(+ 1 2)")))))
    (is (= :collision (:decision (history/decision b1
                                                    (request "c0" nil "different")))))
    (is (= :stale (:decision (history/decision b1
                                                (request "c2" 0 "(+ 3 4)")))))
    (is (= 1 (history/current-version b1)))
    (is (= [r0 r1] (:version-history b1)))
    (is (= "(+ 2 3)" (:text-current b1)))
    (is (= 1 (:epoch b1)))))

(deftest identical-source-can-be-an-intentional-new-version
  (let [r0 (record-for "c0" nil "42")
        b0 (history/append-version {:block-id block-id} r0)]
    (is (= {:decision :append :version 1}
           (history/decision b0 (request "c1" 0 "42"))))))
