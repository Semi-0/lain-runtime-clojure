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
                        :propagator/name :compiler-2/application
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
