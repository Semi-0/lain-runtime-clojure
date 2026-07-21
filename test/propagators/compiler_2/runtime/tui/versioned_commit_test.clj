(ns propagators.compiler-2.runtime.tui.versioned-commit-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.tui.block-model :as block-model]
            [propagators.compiler-2.runtime.session.program :as program]
            [propagators.cells.value :as value]
            [propagators.compiler-2.operators.block-premise :as premise]
            [propagators.compiler-2.operators.versioned-definition :as definition]
            [propagators.compiler-2.runtime.application :as compiler-app]
            [propagators.datastructures.event :as event]
            [propagators.gur.flat :as fvm]
            [propagators.ids :as ids]
            [propagators.datastructures.tms.distributed :as tms]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.semantic-trace :as semantic-trace]))

(def c0 "00000000-0000-0000-0000-000000000001")
(def c1 "00000000-0000-0000-0000-000000000002")
(def c2 "00000000-0000-0000-0000-000000000003")

(defn request
  [commit-id expected-version text]
  {:commit-id commit-id
   :client-id "A"
   :index 0
   :expected-version expected-version
   :text text})

(defn block-request
  [commit-id index expected-version text]
  (assoc (request commit-id expected-version text) :index index))

(defn semantic-result
  [runtime-state client-id index]
  (let [compiled (get-in runtime-state
                         [:program/results [client-id index] :compiled])]
    (-> (net/network-cell-content (:program/net runtime-state) (:cell compiled))
        tms/strongest-distributed-value
        tms/distributed-base-value)))

(defn topology-size
  [runtime-state]
  (let [environment (vals (net/net-env (:program/net runtime-state)))]
    {:cells (count (remove prop/prop? environment))
     :propagators (count (filter prop/prop? environment))}))

(deftest commit-is-idempotent-and-history-is-monotone
  (let [session (runtime/new-session)]
    (is (= :versioned-premise
           (:mode (runtime/register-tui!
                   session {:client-id "A" :mode :versioned-premise}))))
    (is (= 1 (count (get-in @session [:tuis "A" :blocks]))))
    (let [receipt0 (runtime/commit-version! session (request c0 nil "42"))
          state0 @session
          topology0 (topology-size state0)
          ids0 (set (keys (net/net-env (:program/net state0))))
          replay (runtime/commit-version! session (request c0 nil "42"))]
      (is (= 0 (:version receipt0)))
      (is (:replayed? replay))
      (is (= topology0 (topology-size @session)))
      (is (= 3 (count (get-in @session [:tuis "A" :blocks]))))
      (is (= ["42" 42 value/nothing]
             (mapv :value (:blocks (runtime/read-tui-view
                                    @session {:client-id "A"})))))
      (let [receipt (-> state0 (block-model/block-by-index "A" 0)
                        :version-history first :topology)]
        (is (vector? (:propagator-ids receipt)))
        (is (vector? (:application-ids receipt)))
        (is (some? (:result-cell receipt))))
      (testing "a new ID intentionally creates a new identical-source version"
        (let [receipt1 (runtime/commit-version! session (request c1 0 "42"))
              state1 @session
              block (block-model/block-by-index state1 "A" 0)
              old-state-cell (-> block :version-history first :premise-state-cell)
              old-state (net/network-cell-content (:program/net state1)
                                                  old-state-cell)]
          (is (= 1 (:version receipt1)))
          (is (= 3 (count (get-in state1 [:tuis "A" :blocks]))))
          (is (= [0 1] (mapv :version (:version-history block))))
          (is (= c0 (-> block :version-history first :commit-id)))
          (is (every? #(contains? (net/net-env (:program/net state1)) %) ids0))
          (is (not (contains? (-> old-state tms/distributed-slots
                                  tms/tms-view tms/active-premises)
                              (:premise-id (first (:version-history block)))))))))))

(deftest conflict-collision-and-compilation-failure-do-not-commit
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version! session (request c0 nil "1"))
    (let [history-before (get-in @session [:tuis "A" :blocks 0 :version-history])]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"collision"
                            (runtime/commit-version! session
                                                     (request c0 nil "2"))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stale"
                            (runtime/commit-version! session
                                                     (request c1 nil "2"))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"compilation failed"
                            (runtime/commit-version! session
                                                     (request c2 0 "("))))
      (is (= history-before
             (get-in @session [:tuis "A" :blocks 0 :version-history]))))))

(deftest legacy-and-versioned-clients-coexist-without-event-version-facts
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "legacy"})
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version! session (request c0 nil "7"))
    (let [compiled (get-in @session [:program/results ["A" 0] :compiled])
          content (net/network-cell-content (:program/net @session)
                                            (:cell compiled))]
      (is (= :legacy (get-in @session [:tuis "legacy" :mode])))
      (is (= :versioned-premise (get-in @session [:tuis "A" :mode])))
      (is (tms/distributed-value? content))
      (is (not (event/event-content? content))))))

(deftest premise-identity-embeds-the-logical-client
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/register-tui! session {:client-id "B" :mode :versioned-premise})
    (runtime/commit-version! session (request c0 nil "1"))
    (runtime/commit-version! session
                             (assoc (request c1 nil "2") :client-id "B"))
    (let [a-record (-> (block-model/block-by-index @session "A" 0)
                       :version-history first)
          b-record (-> (block-model/block-by-index @session "B" 0)
                       :version-history first)]
      (is (= ["A" (:block-id (block-model/block-by-index @session "A" 0)) 0]
             (:premise-id a-record)))
      (is (= ["B" (:block-id (block-model/block-by-index @session "B" 0)) 0]
             (:premise-id b-record)))
      (is (not= (:premise-id a-record) (:premise-id b-record)))
      (is (not= (:premise-state-cell a-record)
                (:premise-state-cell b-record))))))

(deftest versioned-commit-does-not-settle-unrelated-retained-applications
  (let [session (runtime/new-session)
        activations (atom 0)
        prop-id (ids/new-node-id)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version! session (request c0 nil "1"))
    (let [[_ network]
          ((prop/construct-propagator
            prop-id :test/unrelated-retained-application
            (fn [_inputs _outputs _network]
              (swap! activations inc)
              [])
            [] [])
           (:program/net @session))
          network (net/update-net-dict-entry
                   network compiler-app/apply-application-props-key
                   (fnil conj #{}) prop-id)]
      (swap! session assoc :program/net network))
    (runtime/commit-version! session (request c1 0 "2"))
    (is (zero? @activations))))

(deftest explicit-trace-refreshes-deferred-versioned-semantic-graph
  (let [session (runtime/new-session)
        sources ["(def-cells a b c d)"
                 "(<-> (- 3 1) a)"
                 "(def g)"
                 "(trace a :upstream g)"]]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (doseq [[index source] (map-indexed vector sources)]
      (runtime/commit-version!
       session
       (block-request (str (java.util.UUID/randomUUID)) index nil source)))
    (let [graph (net/network-cell-strongest
                 (:program/net @session) (program/runtime-graph-id))]
      (is (semantic-trace/semantic-trace-graph? graph))
      (is (seq (:edges graph))))))

(deftest command-protocol-replays-and-versioned-unregister-retains-history
  (let [session (runtime/new-session)
        registered (runtime/handle-command!
                    session {:op :tui/register :client-id "A"
                             :mode :versioned-premise})
        command (assoc (request c0 nil "9") :op :tui/commit-version)
        first-response (runtime/handle-command! session command)
        replay-response (runtime/handle-command! session command)]
    (is (:ok registered))
    (is (:ok first-response))
    (is (true? (get-in replay-response [:result :replayed?])))
    (runtime/unregister-tui! session {:client-id "A"})
    (is (= 1 (count (get-in @session
                            [:tuis "A" :blocks 0 :version-history]))))
    (is (= 0 (get-in @session [:tuis "A" :client-count])))))

(deftest definition-and-caller-premises-retract-and-recommit
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version! session
                             (block-request c0 0 nil "(def x 4)"))
    (runtime/commit-version! session
                             (block-request c1 1 nil "(+ x 1)"))
    (is (= 5 (semantic-result @session "A" 1)))
    (is (= 3 (count (net/network-dict-entry
                     (:program/net @session)
                     premise/application-dependence-key))))
    (is (= 3 (count (filter #(and (prop/prop? %)
                                   (= :compiler-2/application-premise
                                      (prop/prop-name %)))
                            (vals (net/net-env (:program/net @session))))))
        "caller, output transport, and next-block display carry premises")
    (let [caller-content (let [compiled (get-in @session
                                                [:program/results ["A" 1]
                                                 :compiled])]
                           (net/network-cell-content (:program/net @session)
                                                     (:cell compiled)))
          supports (-> caller-content tms/distributed-slots tms/tms-view
                       tms/active-premises)]
      (is (= 2 (count supports)) "definition and caller both support the claim"))
    (runtime/commit-version!
     session
     (block-request "00000000-0000-0000-0000-000000000004"
                    0 0 "(def x 6)"))
    (is (= 7 (semantic-result @session "A" 1))
        "a stable scalar binding reactivates the retained caller")
    (runtime/commit-version!
     session
     (block-request "00000000-0000-0000-0000-000000000005"
                    1 0 "(+ x 1)"))
    (is (= 7 (semantic-result @session "A" 1)))))

(deftest closures-use-retained-application-and-compound-results-stay-raw
  (testing "a closure definition remains raw while its scalar call is premised"
    (let [session (runtime/new-session)]
      (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
      (runtime/commit-version!
       session
       (block-request c0 0 nil
                      "(def inc (network [a] [b] (-> (+ a 1) b)))"))
      (let [closure-cell (get-in @session
                                 [:program/results ["A" 0] :compiled :cell])]
        (is (not (tms/distributed-value?
                  (net/network-cell-content (:program/net @session)
                                            closure-cell)))))
      (runtime/commit-version!
       session
       (block-request c1 1 nil "(let-cell [out] (inc 4 out) out)"))
      (is (= 5 (semantic-result @session "A" 1)))))
  (testing "list topology is raw and carries premise metadata separately"
    (let [session (runtime/new-session)]
      (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
      (runtime/commit-version! session (request c0 nil "(list 1 2)"))
      (let [result-id (get-in @session
                              [:program/results ["A" 0] :compiled :cell])]
      (is (net/network?
             (net/network-cell-strongest (:program/net @session) result-id)))
        (is (seq (premise/binding-contexts
                  (:program/net @session) result-id)))))))

(deftest edited-network-definition-reactivates-existing-application
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil
                    "(def-net inc [a] [b] (-> (+ a 1) b))"))
    (runtime/commit-version!
     session
     (block-request c1 1 nil "(let-cell [out] (inc 4 out) out)"))
    (let [ids-before (set (keys (net/net-env (:program/net @session))))]
      (is (= 5 (semantic-result @session "A" 1)))
      (runtime/commit-version!
       session
       (block-request "00000000-0000-0000-0000-000000000004"
                      0 0 "(def-net inc [a] [b] (-> (+ a 2) b))"))
      (is (= 6 (semantic-result @session "A" 1)))
      (is (every? #(contains? (net/net-env (:program/net @session)) %)
                  ids-before)))))

(deftest edited-retained-application-retracts-its-old-next-block-display
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil
                    "(def-net add-17 [x] [out] (+ x 17))"))
    (runtime/commit-version!
     session
     (block-request c1 1 nil
                    "(let-cell [out] (add-17 20 out) out)"))
    (is (= 37 (get-in (runtime/read-tui-view @session {:client-id "A"})
                      [:blocks 2 :value])))
    (runtime/commit-version!
     session
     (block-request c2 1 0
                    "(let-cell [out] (add-17 60 out) out)"))
    (let [state @session
          application-block (block-model/block-by-index state "A" 1)
          display-block (block-model/block-by-index state "A" 2)
          [old-record new-record] (:version-history application-block)
          display-content (net/network-cell-content
                           (:program/net state) (:display-id display-block))
          display-view (-> display-content tms/distributed-slots tms/tms-view)]
      (is (= 77 (get-in (runtime/read-tui-view state {:client-id "A"})
                        [:blocks 2 :value])))
      (is (tms/distributed-value? display-content))
      (is (= 1 (count (tms/active-claims display-view))))
      (is (= 1 (count (:tms/inactive-claims display-view))))
      (is (contains? (tms/active-premises display-view)
                     (:premise-id new-record)))
      (is (not (contains? (tms/active-premises display-view)
                          (:premise-id old-record)))))))

(deftest block-is-a-cell-target-for-the-ordinary-sync-operator
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil "(-> (+ 20 30) (block 1))"))
    (is (= 50 (get-in (runtime/read-tui-view @session {:client-id "A"})
                      [:blocks 1 :value])))
    (is (some #(and (prop/prop? %)
                    (= :runtime/tui-block (prop/prop-name %)))
              (vals (net/net-env (:program/net @session)))))
    (runtime/commit-version!
     session
     (block-request c1 0 0 "(-> (+ 20 40) (block 1))"))
    (let [state @session
          source-block (block-model/block-by-index state "A" 0)
          display-block (block-model/block-by-index state "A" 1)
          [old-record new-record] (:version-history source-block)
          display-content (net/network-cell-content
                           (:program/net state) (:display-id display-block))
          display-view (-> display-content tms/distributed-slots tms/tms-view)]
      (is (= 60 (get-in (runtime/read-tui-view state {:client-id "A"})
                        [:blocks 1 :value])))
      (is (= 1 (count (tms/active-claims display-view))))
      (is (contains? (tms/active-premises display-view)
                     (:premise-id new-record)))
      (is (not (contains? (tms/active-premises display-view)
                          (:premise-id old-record)))))))

(deftest declared-closure-output-to-block-follows-caller-version
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil
                    "(def-net add-17 [x] [out] (+ x 17))"))
    (runtime/commit-version!
     session
     (block-request c1 1 nil "(add-17 20 (block 2))"))
    (is (= 37 (get-in (runtime/read-tui-view @session {:client-id "A"})
                      [:blocks 2 :value])))
    (runtime/commit-version!
     session
     (block-request c2 1 0 "(add-17 60 (block 2))"))
    (let [state @session
          caller-block (block-model/block-by-index state "A" 1)
          display-block (block-model/block-by-index state "A" 2)
          [old-record new-record] (:version-history caller-block)
          display-content (net/network-cell-content
                           (:program/net state) (:display-id display-block))
          display-view (-> display-content tms/distributed-slots tms/tms-view)]
      (is (= 77 (get-in (runtime/read-tui-view state {:client-id "A"})
                        [:blocks 2 :value])))
      (is (= #{77}
             (set (map tms/claim-value
                       (vals (tms/active-claims display-view))))))
      (is (contains? (tms/active-premises display-view)
                     (:premise-id new-record)))
      (is (not (contains? (tms/active-premises display-view)
                          (:premise-id old-record)))))))

(deftest signature-repair-uses-stable-placeholders-and-visible-warnings
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil "(def-net f [a] [b] (-> (+ a 1) b))"))
    (runtime/commit-version!
     session
     (block-request c1 1 nil "(let-cell [out] (f 4 out) out)"))
    (runtime/commit-version!
     session
     (block-request "00000000-0000-0000-0000-000000000004" 0 0
                    (str "(def-net f [a missing] [b extra] "
                         "(-> (+ a 2) b) (-> (+ a 3) extra))")))
    (let [network (:program/net @session)
          metadata (get (net/network-dict-entry network fvm/name-bindings-key)
                        definition/call-metadata-scope)
          warnings (mapcat :warnings
                           (:blocks (runtime/read-tui-view
                                     @session {:client-id "A"})))
          placeholders (->> (vals metadata)
                            (mapcat :placeholder-ids)
                            set)]
      (is (= 6 (semantic-result @session "A" 1)))
      (is (= #{:missing-inputs :missing-outputs}
             (set (map :warning warnings))))
      (is (= 2 (count placeholders)))
      (is (every? #(contains? (net/net-env network) %) placeholders))
      (runtime/commit-version!
       session
       (block-request "00000000-0000-0000-0000-000000000005" 1 0
                      "(let-cell [out extra] (f 4 0 out extra) out)"))
      (is (empty? (mapcat :warnings
                          (:blocks (runtime/read-tui-view
                                    @session {:client-id "A"}))))
          "a compatible caller version hides retained historical warnings"))))

(deftest explicit-premise-definition-records-and-retracts-its-context
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A" :mode :versioned-premise})
    (runtime/commit-version!
     session
     (block-request c0 0 nil
                    (str "(def op (premise-closure "
                         "(network [x] [out] (-> x out)) :p 0))")))
    (let [first-candidate (-> @session
                              (block-model/block-by-index "A" 0)
                              :version-history first :definitions first)]
      (is (= :p (get-in first-candidate [:candidate/explicit :premise-id])))
      (runtime/commit-version!
       session
       (block-request "00000000-0000-0000-0000-000000000004" 0 0
                      (str "(def op (premise-closure "
                           "(network [x] [out] (-> x out)) :q 1))")))
      (let [network (:program/net @session)
            old-state (net/network-cell-content
                       network
                       (get-in first-candidate
                               [:candidate/explicit-context :premise/state-cell]))
            active (-> old-state tms/distributed-slots tms/tms-view
                       tms/active-premises)]
        (is (not (contains? active :p)))))))
