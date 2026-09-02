(ns propagators.runtime.retraction-inspection-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [extensions.runtime-inspection :as inspection]
            [propagators.compiler.model.env :as env]
            [propagators.runtime :as runtime]
            [propagators.infra.network :as net]))

(defn- commit-request
  [commit-id client-id index expected-version text]
  {:op :tui/commit-version
   :commit-id commit-id
   :client-id client-id
   :index index
   :expected-version expected-version
   :text text})

(defn- command!
  [session request]
  (let [response (runtime/handle-command! session request)]
    (is (:ok response) (pr-str response))
    response))

(deftest classifier-keeps-structured-details-and-builds-a-summary
  (let [raw {:inspection/type :retraction-profile
             :target {:client-id "A" :block-index 1}
             :commit {:version 2}
             :activations
             {:activations 2
              :events [{:propagator/id :old
                        :propagator/name :compiler-2/retained-application
                        :owner {:active? false}
                        :premises []
                        :output-changed-ids []
                        :topology-before {:nodes 1}
                        :topology-after {:nodes 1}}
                       {:propagator/id :current
                        :propagator/name :primitive
                        :owner {:active? true}
                        :premises []
                        :output-changed-ids [:out]
                        :topology-before {:nodes 1}
                        :topology-after {:nodes 1}}]}}
        report (inspection/classify-retraction raw)]
    (is (= [:old] (mapv :propagator/id (:inactive-wakes report))))
    (is (= [:old] (mapv :propagator/id
                        (:hibernation-candidates report))))
    (is (str/includes? (inspection/report-summary report)
                       "inactive wakes: 1"))))

(deftest loaded-propagators-profile-the-next-matching-versioned-commit
  (let [session (runtime/new-session)]
    (command! session {:op :tui/register :client-id "A"
                       :mode :versioned-premise})
    (command! session {:op :tui/register :client-id "inspector"
                       :mode :versioned-premise})
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000301" "A" 0 nil
               (str "(load-primitive-environment "
                    "\"modules/runtime/dev/extensions/runtime_inspection.clj\" "
                    ":extensions.runtime-inspection/primitive-bindings 0)")))
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000302" "A" 1 nil
               "(load-lain \"modules/tui/dev/examples/lain/runtime-inspection.lain\" 0)"))
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000303" "A" 2 nil
               "(def-net f [x] [out] (-> (+ x 1) out))"))
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000304" "A" 3 nil
               "(let-cell [out] (f 1 out) out)"))
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000305" "inspector" 0 nil
               "(def-cells inspection-report inspection-summary)"))
    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000306" "inspector" 1 nil
               (str "(inspect:profile-next-commit "
                    "(instance A) 3 inspection-report)")))
    (is (= 1 (count (:inspection/probes @session))))

    (testing "an unrelated commit does not consume the one-shot probe"
      (command! session
                (commit-request
                 "00000000-0000-0000-0000-000000000307" "A" 4 nil
                 "(+ 2 3)"))
      (is (= 1 (count (:inspection/probes @session)))))

    (testing "replays, stale commits, and failed commits do not consume it"
      (let [replay (runtime/handle-command!
                    session
                    (commit-request
                     "00000000-0000-0000-0000-000000000304" "A" 3 nil
                     "(let-cell [out] (f 1 out) out)"))]
        (is (:ok replay) (pr-str replay))
        (is (true? (get-in replay [:result :replayed?]))))
      (is (false? (:ok (runtime/handle-command!
                        session
                        (commit-request
                         "00000000-0000-0000-0000-00000000030a"
                         "A" 3 9
                         "(let-cell [out] (f 9 out) out)")))))
      (is (false? (:ok (runtime/handle-command!
                        session
                        (commit-request
                         "00000000-0000-0000-0000-00000000030b"
                         "A" 3 0 "(")))))
      (is (= 1 (count (:inspection/probes @session)))))

    (command! session
              (commit-request
               "00000000-0000-0000-0000-000000000308" "A" 3 0
               "(let-cell [out] (f 2 out) out)"))
    (is (empty? (:inspection/probes @session)))
    (let [network (:program/net @session)
          report-id (env/resolve-binding-id network (:program/env @session)
                                            'inspection-report)
          report (net/network-cell-strongest network report-id)
          summary (:summary report)]
      (is (= :retraction-profile (:inspection/type report)))
      (is (= 1 (get-in report [:commit :version])))
      (is (pos? (get-in report [:activations :activations])))
      (is (seq (:inactive-wakes report)))
      (is (seq (:hibernation-candidates report)))
      (is (string? summary))
      (is (str/includes? summary "retraction profile A/block 3/version 1")))

    (testing "the Lain compound can summarize an existing report"
      (command! session
                (commit-request
                 "00000000-0000-0000-0000-000000000309" "inspector" 2 nil
                 (str "(-> (inspect:retraction-summary inspection-report) "
                      "inspection-summary)")))
      (let [network (:program/net @session)
            summary-id (env/resolve-binding-id network (:program/env @session)
                                               'inspection-summary)
            summary (net/network-cell-strongest network summary-id)]
        (is (string? summary))
        (is (str/includes? summary
                           "retraction profile A/block 3/version 1"))))))
