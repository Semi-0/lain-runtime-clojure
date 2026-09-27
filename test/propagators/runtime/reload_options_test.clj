(ns propagators.runtime.reload-options-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.file-loader :as file-loader]))

(deftest full-reload-preserves-injected-runtime-options
  (let [options {:operator-groups []
                 :operator-providers {}
                 :command-handlers {:test/ping (fn [_ _] {:status :ok})}}
        session (runtime/new-session options)]
    (try
      (file-loader/replace-session-from-source! session "(+ 1 2)")
      (is (= options (:runtime/options @session)))
      (file-loader/replace-session-from-source! session "(+ 3 4)")
      (is (= options (:runtime/options @session)))
      (finally
        (runtime/stop-clocks! session)))))
