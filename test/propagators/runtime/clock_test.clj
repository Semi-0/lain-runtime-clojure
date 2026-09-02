(ns propagators.runtime.clock-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.infra.cells.value :as value]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.clock :as clock]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.network :as net]))

(defn- request [commit-id index expected-version text]
  {:op :tui/commit-version
   :commit-id commit-id
   :client-id "clock-test"
   :index index
   :expected-version expected-version
   :text text})

(defn- wait-until
  [pred]
  (loop [remaining 100]
    (cond
      (pred) true
      (zero? remaining) false
      :else (do (Thread/sleep 10)
                (recur (dec remaining))))))

(deftest clock-update-is-a-source-aware-event
  (let [subscription {:clock/id :clock-a :clock/target-id :out}
        update (clock/clock-update subscription 7 123456)]
    (is (event/event-fact? update))
    (is (= :out (event/input-id update)))
    (is (= :clock-a (event/source update)))
    (is (= 7 (event/timestamp update)))
    (is (= 123456 (event/event-value update)))))

(deftest loaded-clock-is-visible-reactive-and-stops-on-block-retraction
  (let [session (runtime/new-session)
        command #(runtime/handle-command! session %)]
    (try
      (command {:op :tui/register
                :client-id "clock-test"
                :mode :versioned-premise})
      (is (true? (:ok (command
                       (request
                        "00000000-0000-0000-0000-000000000201" 0 nil
                        (str "(load-primitive-environment "
                             "\"modules/runtime/dev/extensions/runtime_clock.clj\" "
                             ":extensions.runtime-clock/primitive-bindings 0)"))))))
      (is (true? (:ok (command
                       (request
                        "00000000-0000-0000-0000-000000000202" 1 nil
                        "(clock-in 20)")))))
      (is (wait-until
           #(number? (get-in (runtime/read-tui-view
                              @session {:client-id "clock-test"})
                             [:blocks 2 :value]))))
      (let [first-value (get-in (runtime/read-tui-view
                                 @session {:client-id "clock-test"})
                                [:blocks 2 :value])]
        (is (wait-until
             #(let [latest (get-in (runtime/read-tui-view
                                    @session {:client-id "clock-test"})
                                   [:blocks 2 :value])]
                (and (number? latest) (not= first-value latest))))))
      (let [[subscription-id subscription]
            (first (:clock/subscriptions @session))]
        (is (seq (:clock/contexts subscription)))
        (is (true? (clock/subscription-active? @session subscription)))
        (is (true? (:ok (command
                         (request
                          "00000000-0000-0000-0000-000000000203" 1 0
                          "(def stopped)")))))
        (is (false? (clock/subscription-active? @session subscription)))
        (let [epoch-key (or (:clock/source-id subscription) subscription-id)
              epoch-after-retraction (get-in @session
                                             [:clock/epochs epoch-key])]
          (Thread/sleep 80)
          (is (= epoch-after-retraction
                 (get-in @session [:clock/epochs epoch-key])))
          (is (value/nothing?
               (net/network-cell-strongest
                (:program/net @session) (:clock/target-id subscription))))))
      (finally
        (runtime/stop-clocks! session)))))

(deftest editing-clock-interval-retracts-the-previous-event-source
  (let [session (runtime/new-session)
        command #(runtime/handle-command! session %)]
    (try
      (command {:op :tui/register
                :client-id "clock-test"
                :mode :versioned-premise})
      (command (request
                "00000000-0000-0000-0000-000000000211" 0 nil
                (str "(load-primitive-environment "
                     "\"modules/runtime/dev/extensions/runtime_clock.clj\" "
                     ":extensions.runtime-clock/primitive-bindings 0)")))
      (command (request
                "00000000-0000-0000-0000-000000000212" 1 nil
                "(clock-in 20)"))
      (is (wait-until #(number? (get-in (runtime/read-tui-view
                                         @session {:client-id "clock-test"})
                                        [:blocks 2 :value]))))
      (is (true? (:ok (command
                       (request
                        "00000000-0000-0000-0000-000000000213" 1 0
                        "(clock-in 30)")))))
      (is (wait-until
           #(number? (get-in (runtime/read-tui-view
                              @session {:client-id "clock-test"})
                             [:blocks 2 :value]))))
      (is (= 1
             (count (filter event/active?
                            (event/latest-facts
                             (net/network-cell-content
                              (:program/net @session)
                              (:display-id
                               (get-in @session [:tuis "clock-test" :blocks 2]))))))))
      (finally
        (runtime/stop-clocks! session)))))
