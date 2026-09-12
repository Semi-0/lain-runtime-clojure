(ns propagators.runtime.clock-test
  "Reliable clock contracts plus a deprecated event-to-TUI diagnostic."
  (:require [clojure.test :refer [deftest is]]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.clock :as clock]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.datastructures.tms.distributed :as tms]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]))

(defn- request [commit-id index expected-version text]
  {:op :tui/commit-version
   :commit-id commit-id
   :client-id "clock-test"
   :index index
   :expected-version expected-version
   :text text})

(defn- install-clock!
  [session interval]
  (let [command #(runtime/handle-command! session %)]
    (command {:op :tui/register
              :client-id "clock-test"
              :mode :versioned-premise})
    (command
     (request
      "00000000-0000-0000-0000-000000000201" 0 nil
      (str "(load-primitive-environment "
           "\"dev/extensions/runtime_clock.clj\" "
           ":extensions.runtime-clock/primitive-bindings 0)")))
    (command
     (request
      "00000000-0000-0000-0000-000000000202" 1 nil
      (str "(clock-in " interval ")")))))

(defn- first-subscription
  [session]
  (first (:clock/subscriptions @session)))

(deftest clock-update-is-a-source-aware-event
  (let [subscription {:clock/id :clock-a :clock/target-id :out}
        update (clock/clock-update subscription 7 123456)]
    (is (event/event-fact? update))
    (is (= :out (event/input-id update)))
    (is (= :clock-a (event/source update)))
    (is (= 7 (event/timestamp update)))
    (is (= 123456 (event/event-value update)))))

(defn ^:deprecated loaded-clock-display-result
  "Reproduce the unsupported flat-GUR event-to-TUI display crossing.

  The intended result is 1000. The current clock prototype returns nothing
  because topology lowering does not preserve its dictionary-backed event-cell
  declaration. This function is diagnostic evidence, not a release gate."
  []
  (let [session (runtime/new-session)]
    (try
      (install-clock! session 20)
      (runtime/stop-clocks! session)
      (let [[subscription-id _subscription] (first-subscription session)]
        (clock/tick-clock! session subscription-id (constantly 1000))
        (get-in (runtime/read-tui-view
                 @session {:client-id "clock-test"})
                [:blocks 2 :value]))
      (finally
        (runtime/stop-clocks! session)))))

(defn ^:deprecated loaded-clock-retraction-result
  "Exercise the full experimental clock loader and block-retraction path.

  Kept as an opt-in diagnostic because compiling the prototype exceeds the
  three-second per-test limit."
  []
  (let [session (runtime/new-session)
        command #(runtime/handle-command! session %)]
    (try
      (install-clock! session 20)
      (let [[subscription-id subscription] (first-subscription session)]
        (command
         (request
          "00000000-0000-0000-0000-000000000203" 1 0
          "(def stopped)"))
        (let [epoch-key (or (:clock/source-id subscription) subscription-id)
              epoch-after-retraction (get-in @session
                                             [:clock/epochs epoch-key])]
          (clock/tick-clock! session subscription-id (constantly 1001))
          {:active? (clock/subscription-active? @session subscription)
           :epoch-stable?
           (= epoch-after-retraction
              (get-in @session [:clock/epochs epoch-key]))
           :target
           (net/network-cell-strongest
            (:program/net @session) (:clock/target-id subscription))}))
      (finally
        (runtime/stop-clocks! session)))))

(deftest retracted-premise-disables-clock-subscription
  (let [state-id (ids/new-node-id)
        premise-id :clock-test/premise
        context {:premise/id premise-id
                 :premise/state-cell state-id}
        subscription {:clock/id :clock-test/subscription
                      :clock/target-id (ids/new-node-id)
                      :clock/contexts [context]}
        active-net
        (-> net/empty-net
            (nb/install-cell state-id)
            (nb/seed-cell
             state-id
             (tms/distributed-premise-update premise-id 0 true)))
        retracted-net
        (nb/seed-cell
         active-net state-id
         (tms/distributed-premise-update premise-id 1 false))]
    (is (true? (clock/subscription-active?
                {:program/net active-net} subscription)))
    (is (false? (clock/subscription-active?
                 {:program/net retracted-net} subscription)))))
