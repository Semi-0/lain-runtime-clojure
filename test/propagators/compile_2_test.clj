(ns propagators.compile-2-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [propagators.cells.cell-protocol :as protocol]
            [propagators.cells.value :as value]
            [propagators.closure :as closure]
            [propagators.compile :as compile]
            [propagators.compiler-2.runtime.application :as compiler-app]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.language.ast :as ast]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.model.env :as env]
            [propagators.compiler-2.compiler.basis :as h
             :refer [behavior-env
                     default-env
                     dependency-env]]
            [propagators.compiler-2.main :as main]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.compiler-2.language.parser :as parser]
            [propagators.compiler-2.runtime.retained-application :as retained-app]
            [propagators.compiler-2.operators.behavior
             :refer [behavior-tms-env]]
            [propagators.core :as core]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.behavior-algebra :as hist]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.dependency :as dependency]
            [propagators.datastructures.event :as event]
            [propagators.datastructures.reducer-cell :as reducer]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.datastructures.tms :as tms]
            [propagators.ids :as ids]
            [propagators.layered :as layered]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]))

(defn- run-compiled
  [compiled]
  (nb/run-propagators (:net compiled) (:props compiled)))

(defn- strongest
  [n id]
  (net/network-cell-strongest n id))

(defn- layer-strongest
  [n object-id layer-name]
  (let [layer-id (ids/new-node-id)
        n0 (nb/install-cell n layer-id)
        [prop-id n1] ((layered/p:layer layer-name layer-id object-id) n0)
        n2 (nb/run-propagators n1 [prop-id])]
    (strongest n2 layer-id)))

(defn- prop-count
  [n]
  (count (filter prop/prop? (vals (net/net-env n)))))

(defn- seed-and-run
  [n id v]
  (let [[tasks n'] (core/eval-cell id (message id v) n)]
    (core/run-tasks tasks n')))

(defn- seeded-cell
  [n v]
  (let [id (ids/new-node-id)]
    [id (nb/seed-cell (nb/install-cell n id) id v)]))

(defn- behavior-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-event-protocol))
      (compile/install-and-run (protocol/install-behavior-protocol))))

(defn- behavior-tms-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-event-protocol))
      (compile/install-and-run (protocol/install-behavior-protocol))
      (compile/install-and-run (protocol/install-tms-distributed-protocol))))

(defn- scope-source-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-scope-source-protocol))))

(defn- tms-distributed-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-tms-distributed-protocol))))

(defn- behavior-view
  [records source-keys]
  (behavior/behavior-value
   {:history (hist/records->history records)
    :source-keys source-keys
    :reducer behavior/event-history-reducer-id}))

(defn- behavior-cell
  [n v]
  (let [id (ids/new-node-id)]
    [id (nb/install-cell n id v (behavior/strongest-value v))]))

(defn- event-cell
  [n & facts]
  (let [id (ids/new-node-id)
        content (reduce event/merge-content value/nothing facts)]
    [id (nb/install-cell n id content (event/strongest-value content))]))

(defn- event-active-values
  [n id]
  (event/active-values (net/network-cell-content n id)))

(defn- event-active-value-list
  [n id]
  (vec (vals (event-active-values n id))))

(defn- run-event-update
  [n id fact]
  (let [[tasks n'] (core/eval-cell id (message id fact) n)]
    (core/run-tasks tasks n')))

(defn- behavior-record-map
  [record]
  (cond
    (hist/point-record? record)
    {:at (obj/slot-value record :at)
     :value (obj/slot-value record :value)}

    (hist/interval-record? record)
    {:from (obj/slot-value record :from)
     :to (obj/slot-value record :to)
     :value (obj/slot-value record :value)}))

(defn- behavior-records
  [v]
  (mapv behavior-record-map
        (behavior/history-records (scope-source/unwrap v))))

(defn- behavior-current-value
  [n id]
  (let [v (scope-source/unwrap (strongest n id))]
    (if (value/unusable? v)
      v
      (behavior/base-value v))))

(defn- distributed-current-value
  [n id]
  (let [v (strongest n id)]
    (if (value/unusable? v)
      v
      (tms/distributed-base-value v))))

(defn- distributed-slot-keys
  [n id]
  (set (keys (tms/distributed-slots (net/network-cell-content n id)))))

(defn- distributed-behavior-current-value
  [n id]
  (let [v (strongest n id)]
    (if (value/unusable? v)
      v
      (behavior/base-value
       (behavior/strongest-value (tms/distributed-base-value v))))))

(defn- distributed-behavior-records
  [n id]
  (let [v (strongest n id)]
    (if (value/unusable? v)
      v
      (behavior-records (tms/distributed-base-value v)))))

(defn- scoped-base
  [v]
  (scope-source/base-value v))

(defn- scoped-candidate-count
  [content]
  (if (scope-source/scope-content? content)
    (count (scope-source/content-candidates content))
    0))

(defn- install-empty-cells
  [n ids]
  (reduce nb/install-cell n ids))

(defn- installed-prop-ids
  [installed-id]
  (if (sequential? installed-id)
    (vec installed-id)
    [installed-id]))

(defn- read-slot
  [n coll-id slot-key]
  (let [slot-id (ids/new-node-id)
        [prop-id n1] ((obj/p:slot slot-key slot-id coll-id)
                      (nb/ensure-cell n slot-id))
        n2 (nb/run-propagators n1 [prop-id])]
    [(strongest n2 slot-id) n2]))

(defn- bi-sync-chain-source
  [length]
  (let [cells (map #(str "c" %) (range (inc length)))
        links (map #(format "(<-> c%d c%d)" % (inc %)) (range length))]
    (str "(let-cell [" (str/join " " cells) "] "
         "(<-> c0 1) "
         (str/join " " links)
         " c" length ")")))

(defn- seed-behavior-message
  [n id v]
  (core/eval-cell id (message id v) n))

(defn- parse
  [source]
  (parser/parse-string source))

(defn- compile-source
  ([source]
   (main/compile-source source))
  ([source env]
   (main/compile-source source env))
  ([source env opts]
   (main/compile-source source env opts)))

(defn- reducer-test-node
  [& parts]
  (h/stable-node-id (into [:compile-2-test :execute-sub-env] parts)))

(defn- latest-merge-net
  []
  (let [content-id (reducer-test-node :merge :content)
        update-id (reducer-test-node :merge :update)
        out-id (reducer-test-node :merge :out)
        n0 (-> net/empty-net
               (nb/install-cell content-id)
               (nb/install-cell update-id)
               (nb/install-cell out-id))
        [_ n1] ((prop/construct-propagator
                 (fn [_inputs _outputs network]
                   (let [content (strongest network content-id)
                         update (strongest network update-id)
                         content* (if (value/nothing? content) {} content)
                         update* (if (value/nothing? update) {} update)]
                     [(message out-id (merge content* update*))]))
                 [content-id update-id]
                 [out-id])
                n0)]
    (-> n1
        (net/assoc-net-dict-entry :content content-id)
        (net/assoc-net-dict-entry :update update-id)
        (net/assoc-net-dict-entry :out out-id))))

(defn- latest-strongest-net
  []
  (let [slots-id (reducer-test-node :strongest :slots)
        out-id (reducer-test-node :strongest :out)
        epoch-id (reducer-test-node :strongest :epoch)
        n0 (-> net/empty-net
               (nb/install-cell slots-id)
               (nb/install-cell out-id)
               (nb/install-cell epoch-id))
        [_ n1] ((prop/construct-propagator
                 (fn [_inputs _outputs network]
                   (let [slots (strongest network slots-id)
                         latest (last (sort-by key (or slots {})))]
                     [(message out-id (if latest (val latest) value/nothing))
                      (message epoch-id [:latest (when latest (key latest))])]))
                 [slots-id]
                 [out-id epoch-id])
                n0)]
    (-> n1
        (net/assoc-net-dict-entry :slots slots-id)
        (net/assoc-net-dict-entry :out out-id)
        (net/assoc-net-dict-entry :epoch epoch-id))))

(def ^:private execute-merge-net (latest-merge-net))
(def ^:private execute-strongest-net (latest-strongest-net))
(def ^:private execute-reducer-id :compile-2-test/behavior)

(defn- reducer-storage-cell
  [n]
  (let [id (ids/new-node-id)
        v (reducer/reducer-cell execute-reducer-id
                                execute-merge-net
                                execute-strongest-net)]
    [id (nb/install-cell n id v (reducer/strongest v))]))

(defn- reducer-emit-operator
  []
  (with-meta
    (fn [network [value-id storage-id] _out-id]
      (let [[prop-id network']
            ((reducer/p:reducer-slot execute-reducer-id
                                     execute-merge-net
                                     execute-strongest-net
                                     [:event value-id]
                                     value-id
                                     storage-id)
             network)]
        [network' [prop-id] storage-id]))
    {h/output-selector-key
     (fn [[_value-id storage-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [value-id storage-id] _out-id]
       (let [raw (strongest network value-id)
             v (if (reducer/reduced-value? raw)
                 (reducer/reduced-result raw)
                 raw)]
         (if (value/unusable? v)
           []
           [(message storage-id
                     (reducer/reducer-slot-update execute-reducer-id
                                                  execute-merge-net
                                                  execute-strongest-net
                                                  [:event value-id]
                                                  v))])))}))

(defn- tms-claim-operator
  [claim-id proposition supports]
  (with-meta
    (fn [network [value-id storage-id] _out-id]
      (let [[prop-id network']
            ((tms/p:tms-claim tms/reducer-id
                              claim-id
                              proposition
                              supports
                              value-id
                              storage-id)
             network)]
        [network' [prop-id] storage-id]))
    {h/output-selector-key
     (fn [[_value-id storage-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [value-id storage-id] _out-id]
       (let [v (strongest network value-id)]
         (if (value/unusable? v)
           []
           [(message storage-id
                     (tms/claim-update
                      (tms/claim claim-id proposition v supports)))])))}))

(defn- tms-premise-operator
  [premise epoch]
  (with-meta
    (fn [network [active-id storage-id] _out-id]
      (let [[prop-id network']
            ((tms/p:tms-premise tms/reducer-id
                                premise
                                epoch
                                active-id
                                storage-id)
             network)]
        [network' [prop-id] storage-id]))
    {h/output-selector-key
     (fn [[_active-id storage-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [active-id storage-id] _out-id]
       (let [v (strongest network active-id)]
         (if (value/unusable? v)
           []
           [(message storage-id
                     (tms/premise-update tms/reducer-id premise epoch v))])))}))

(defn- tms-premise-source-operator
  [epoch]
  (with-meta
    (fn [network [premise-id active-id storage-id] _out-id]
      (let [[prop-id network']
            ((tms/p:tms-premise-source tms/reducer-id
                                       premise-id
                                       epoch
                                       active-id
                                       storage-id)
             network)]
        [network' [prop-id] storage-id]))
    {h/output-selector-key
     (fn [[_premise-id _active-id storage-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [premise-id active-id storage-id] _out-id]
       (let [premise (strongest network premise-id)
             active (strongest network active-id)]
         (if (or (value/unusable? premise)
                 (value/unusable? active))
           []
           [(message storage-id
                     (tms/premise-update tms/reducer-id
                                          premise
                                          epoch
                                          active))])))}))

(defn- tms-premise-epoch-operator
  [active?]
  (with-meta
    (fn [network [_premise-id _epoch-id storage-id] _out-id]
      [network [] storage-id])
    {h/output-selector-key
     (fn [[_premise-id _epoch-id storage-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [premise-id epoch-id storage-id] _out-id]
       (let [premise (strongest network premise-id)
             epoch (strongest network epoch-id)]
         (if (or (value/unusable? premise)
                 (value/unusable? epoch))
           []
           [(message storage-id
                     (tms/premise-update tms/reducer-id
                                         premise
                                          epoch
                                          active?))])))}))

(defn- tms-insert-pair-messages
  [network storage-id value-id premise-id]
  (let [value (strongest network value-id)
        premise (strongest network premise-id)]
    (cond
      (or (value/contradiction? value)
          (value/contradiction? premise))
      [(message storage-id value/contradiction)]

      (or (value/nothing? value)
          (value/nothing? premise))
      []

      :else
      [(message storage-id
                (tms/premise-update tms/reducer-id premise 0 true))
       (message storage-id
                (tms/claim-update
                 (tms/claim [:insert premise]
                            :answer
                            value
                            [(tms/support premise
                                          :compiler-2
                                          :insert)])))])))

(defn- tms-insert-fact-operator
  []
  (with-meta
    (fn [network [_storage-id _value-id _premise-id] _out-id]
      [network [] _storage-id])
    {h/output-selector-key
     (fn [[storage-id _value-id _premise-id] fallback-id]
       (or storage-id fallback-id))
     h/application-activate-key
     (fn [network _context-id [storage-id value-id premise-id] _out-id]
       (tms-insert-pair-messages network
                                 storage-id
                                 value-id
                                 premise-id))}))

(defn- execute-sub-env-ast
  [expr parent-env & watch-syms]
  (apply ast/app
         (ast/sym 'execute-sub-env)
         (ast/lit expr)
         (ast/lit parent-env)
         (map ast/sym watch-syms)))

(deftest compile-2-compiles-primitive-application
  (testing "application returns a fresh result cell"
    (let [compiled (compile-source "(+ 1 2)")
          result-net (run-compiled compiled)]
      (is (= 3 (strongest result-net (:cell compiled))))
      (is (= (:cell compiled) (main/compiled-result (:net compiled))))
      (is (= (:props compiled) (main/compiled-props (:net compiled)))))))

(deftest compiler-2-operator-closures-replace-primitive-metadata
  (testing "default primitive operators are explicit operator closures"
    (let [plus (env/lookup (default-env) '+)
          a-id (ids/new-node-id)
          b-id (ids/new-node-id)
          out-id (ids/new-node-id)
          n0 (-> net/empty-net
                 (nb/install-cell a-id 1 1)
                 (nb/install-cell b-id 2 2))
          [n1 prop-ids installed-out-id] ((operator-value/operator-install plus)
                                          n0
                                          [a-id b-id]
                                          out-id)
          n2 (nb/run-propagators n1 prop-ids)]
      (is (operator-value/operator-closure? plus))
      (is (= {:arg-ids [a-id b-id]
              :inputs [a-id b-id]
              :outputs [out-id]
              :out-id out-id
              :context-id nil}
             (operator-value/operator-call plus [a-id b-id] out-id nil)))
      (is (= out-id installed-out-id))
      (is (= 3 (strongest n2 out-id)))))
  (testing "operator closure activation and output selection are explicit slots"
    (let [switch (env/lookup (default-env) 'switch)
          value-id (ids/new-node-id)
          condition-id (ids/new-node-id)
          explicit-out-id (ids/new-node-id)
          fallback-id (ids/new-node-id)
          n0 (-> net/empty-net
                 (nb/install-cell value-id 9 9)
                 (nb/install-cell condition-id true true))
          messages ((h/application-activate switch)
                    n0
                    nil
                    [value-id condition-id explicit-out-id]
                    fallback-id)]
      (is (operator-value/operator-closure? switch))
      (is (= explicit-out-id
             (h/output-id switch
                          [value-id condition-id explicit-out-id]
                          fallback-id)))
      (is (= [9] (mapv :value messages)))))
  (testing "legacy metadata operators remain a compatibility fallback"
    (let [out-id (ids/new-node-id)
          legacy (with-meta
                   (fn [network _arg-ids out-id] [network [] out-id])
                   {h/output-selector-key
                    (fn [_arg-ids fallback-id] [:selected fallback-id])
                    h/application-activate-key
                    (fn [_network _context-id _arg-ids out-id]
                      [(message out-id :activated)])})]
      (is (= [:selected out-id] (h/output-id legacy [] out-id)))
      (is (= [:activated]
             (mapv :value ((h/application-activate legacy)
                           net/empty-net
                           nil
                           []
                           out-id)))))))

(deftest compiler-2-env-bound-operator-closures-are-callable
  (let [compiled (compile-source "(let-cell [out]
                                    (switch (+ 1 2) true out)
                                    out)")
        result-net (run-compiled compiled)]
    (is (= 3 (strongest result-net (:cell compiled))))))

(deftest compiler-2-default-env-uses-distributed-tms-premise-closure
  (let [compiler-env (default-env)]
    (doseq [op ['premise-input
                'premise-believe
                'tms-closure
                'premise-closure]]
      (testing op
        (let [operator (env/lookup compiler-env op)]
          (is (operator-value/operator-closure? operator))
          (is (nil? (-> operator meta h/application-activate-key))))))))

(deftest compiler-2-behavior-env-uses-operator-closures
  (let [compiler-env (behavior-tms-env)]
    (doseq [op ['behavior-event
                'behavior-add-event
                'behavior-empty-state
                'behavior-retain-last
                'behavior
                'behavior-cell
                'latest]]
      (testing op
        (let [operator (env/lookup compiler-env op)]
          (is (operator-value/operator-closure? operator))
          (is (nil? (-> operator meta h/application-activate-key))))))))

(deftest compiler-2-main-compiles-with-default-behavior-tms-env
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [out]
                     (def value :yes)
                     (def premise :from-main-entry)
                     (def epoch 0)
                     (premise-input value premise epoch out)
                     out)"
                  {:net (tms-distributed-protocol-net)})
        n (run-compiled compiled)]
    (is (= :yes (distributed-current-value n (:cell compiled))))
    (is (contains? (distributed-slot-keys n (:cell compiled))
                   (tms/premise-slot-key :from-main-entry 0)))))

(deftest compiler-2-static-tms-operator-does-not-watch-its-output
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [epoch out]
                     (def premise :static/premise)
                     (premise-retract premise epoch out)
                     out)")
        out-id (env/binding-id (env/lookup (:env compiled) 'out))
        epoch-id (env/binding-id (env/lookup (:env compiled) 'epoch))
        [prop-id] (filter (fn [id]
                           (= "premise-retract"
                              (prop/prop-name
                               (net/network-env-lookup (:net compiled) id))))
                         (:props compiled))
        inputs (:inputs (get (net/net-graph (:net compiled)) prop-id))]
    (is prop-id)
    (is (contains? inputs epoch-id))
    (is (not (contains? inputs out-id)))
    (is (empty? (net/network-dict-entry
                 (:net compiled)
                 retained-app/retained-application-props-key)))))

(deftest compiler-2-main-can-define-behavior-producing-network
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [a b out]
                     (def-net make-point [t v] [out]
                       (behavior-point t v out))
                     (make-point 6 2 a)
                     (make-point 6 7 b)
                     (<-> (be:+ a b) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 9 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 9}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-main-can-build-behavior-with-compiler-closure-reducer
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events out]
                     (def-net retain-event [acc update] [out]
                       (behavior-add-event acc update out))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior events retain-event (behavior-empty-state) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 3 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 2}
            {:at 8 :value 3}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-merge-can-use-low-level-operators
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events out]
                     (def-net retain-event-low [acc update] [out]
                       (let-cell [known tick value next]
                         (behavior-state-events acc known)
                         (behavior-update-tick update tick)
                         (behavior-update-value update value)
                         (behavior-assoc-event known tick value next)
                         (behavior-state-from-events next out)))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior events retain-event-low (behavior-empty-state) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 3 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 2}
            {:at 8 :value 3}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-cell-can-use-slot-based-merge-closure
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out]
                     (def-net retain-latest-slot [acc next] [out]
                       (let-cell [events* slot value next-events full]
                         (behavior-state-events acc events*)
                         (p:slot :slot slot next)
                         (p:slot :value value next)
                         (behavior-assoc-event events* slot value next-events)
                         (behavior-state-from-events next-events full)
                         (behavior-retain-last full 1 out)))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior-cell events (behavior-empty-state) retain-latest-slot retained)
                     (<-> (be:+ retained retained) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 6 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-closure-reducer-can-retain-latest-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out]
                     (def-net retain-latest [acc update] [out]
                       (let-cell [full]
                         (behavior-add-event acc update full)
                         (behavior-retain-last full 1 out)))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior-event 10 5 events)
                     (behavior events retain-latest (behavior-empty-state) retained)
                     (<-> (be:+ retained retained) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-closure-reducer-can-retain-window-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out]
                     (def-net retain-window [acc update] [out]
                       (let-cell [full]
                         (behavior-add-event acc update full)
                         (behavior-retain-last full 2 out)))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior-event 10 5 events)
                     (behavior events retain-window (behavior-empty-state) retained)
                     (<-> (be:+ retained retained) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}
            {:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-cell-can-retain-window-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out]
                     (def-net retain-window [acc next] [out]
                       (let-cell [full]
                         (behavior-add-event acc next full)
                         (behavior-retain-last full 2 out)))
                     (behavior-event 6 2 events)
                     (behavior-event 8 3 events)
                     (behavior-event 10 5 events)
                     (behavior-cell events (behavior-empty-state) retain-window retained)
                     (<-> (be:+ retained retained) out)
                     out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}
            {:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-behavior-syntax-latest-and-last-are-behaviors
  (let [source "(let-cell [events retained out]
                  (def-net retain-all [acc next] [out]
                    (behavior-add-event acc next out))
                  (behavior-event 6 2 events)
                  (behavior-event 8 3 events)
                  (behavior-event 10 5 events)
                  (be:behavior-cell events (behavior-empty-state) retain-all retained)
                  %s
                  out)"
        compiled (main/compile-source-with-behavior-tms
                  (format source "(<-> (be:+ (latest retained) (latest retained)) out)")
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))
    (let [compiled (main/compile-source-with-behavior-tms
                    (format source "(last retained 1 out)")
                    {:net (behavior-tms-protocol-net)})
          n (run-compiled compiled)]
      (is (= 3 (behavior-current-value n (:cell compiled))))
      (is (= [{:at 8 :value 3}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))))

(deftest compiler-2-behavior-prefixed-projections-are-behaviors
  (let [source "(let-cell [events retained out]
                  (def-net retain-all [acc next] [out]
                    (behavior-add-event acc next out))
                  (behavior-event 6 2 events)
                  (behavior-event 8 3 events)
                  (behavior-event 10 5 events)
                  (behavior-cell events (behavior-empty-state) retain-all retained)
                  %s
                  out)"
        compile-projection (fn [body]
                             (let [compiled (main/compile-source-with-behavior-tms
                                             (format source body)
                                             {:net (behavior-tms-protocol-net)})]
                               [compiled (run-compiled compiled)]))]
    (let [[compiled n] (compile-projection
                        "(<-> (be:+ (be:latest retained) (be:latest retained)) out)")]
      (is (= 10 (behavior-current-value n (:cell compiled))))
      (is (= [{:at 10 :value 10}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))
    (let [[compiled n] (compile-projection "(be:last retained 1 out)")]
      (is (= 3 (behavior-current-value n (:cell compiled))))
      (is (= [{:at 8 :value 3}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))
    (let [[compiled n] (compile-projection "(be:history retained 1 3 out)")]
      (is (= 5 (behavior-current-value n (:cell compiled))))
      (is (= [{:at 8 :value 3}
              {:at 10 :value 5}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))))

(deftest compiler-2-behavior-prefixed-constructor-builds-behavior
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained]
                     (def-net retain-all [acc next] [out]
                       (behavior-add-event acc next out))
                     (behavior-event 6 2 events)
                     (behavior-event 10 5 events)
                     (be:behavior events retain-all (behavior-empty-state) retained)
                     (be:latest retained))"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 5 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 10 :value 5}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-be-latest-zero-arg-builds-empty-latest-behavior
  (let [compiled (main/compile-source-with-behavior-tms
                  "(be:latest)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)
        content (net/network-cell-content n (:cell compiled))]
    (is (= value/nothing (behavior-current-value n (:cell compiled))))
    (is (= [] (behavior-records content)))))

(deftest compiler-2-behavior-declaration-sugar-retains-latest-events
  (testing "def-behavior creates the behavior view and sibling event source"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(def-behavior a)"
                    {:net (behavior-tms-protocol-net)})]
      (is (some? (env/lookup (:env compiled) 'a)))
      (is (some? (env/lookup (:env compiled) 'a-events)))))
  (testing "define-behaviors wires each event source into a latest-retaining behavior"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-cell [out]
                       (define-behaviors a b c)
                       (behavior-event 1 10 a-events)
                       (behavior-event 1 4 b-events)
                       (behavior-event 1 3 c-events)
                       (<-> (be:- (be:+ a b) c) out)
                       out)"
                    {:net (behavior-tms-protocol-net)})
          n (run-compiled compiled)]
      (is (= 11 (behavior-current-value n (:cell compiled))))
      (is (= [{:at 1 :value 10}]
             (behavior-records
              (net/network-cell-content
               n
               (-> compiled :env (env/lookup 'a) env/binding-id)))))))
  (testing "let-behaviour scopes behavior views and sibling event sources"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-behaviour [a b]
                       (behavior-event 1 2 a-events)
                       (behavior-event 1 3 b-events)
                       (be:+ a b))"
                    {:net (behavior-tms-protocol-net)})
          n (run-compiled compiled)]
      (is (= 5 (behavior-current-value n (:cell compiled)))))))

(deftest compiler-2-behavior-syntax-history-slices-return-behaviors
  (let [base-source "(let-cell [events retained out]
                       (def-net retain-all [acc next] [out]
                         (behavior-add-event acc next out))
                       (behavior-event 6 2 events)
                       (behavior-event 8 3 events)
                       (behavior-event 10 5 events)
                       (behavior-cell events (behavior-empty-state) retain-all retained)
                       %s
                       out)"
        compile-slice (fn [body]
                        (let [compiled (main/compile-source-with-behavior-tms
                                        (format base-source body)
                                        {:net (behavior-tms-protocol-net)})]
                          [compiled (run-compiled compiled)]))]
    (let [[compiled n] (compile-slice "(history retained 0 2 out)")]
      (is (= [{:at 6 :value 2}
              {:at 8 :value 3}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))
    (let [[compiled n] (compile-slice "(history-take retained 2 out)")]
      (is (= [{:at 6 :value 2}
              {:at 8 :value 3}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))
    (let [[compiled n] (compile-slice "(history-drop retained 1 out)")]
      (is (= [{:at 8 :value 3}
              {:at 10 :value 5}]
             (behavior-records (net/network-cell-content n (:cell compiled))))))
    (let [[compiled n] (compile-slice "(history-split-at retained 2 out)")
          split (net/network-cell-strongest n (:cell compiled))]
      (is (= [{:at 6 :value 2}
              {:at 8 :value 3}]
             (behavior-records (obj/slot-value split :left))))
      (is (= [{:at 10 :value 5}]
             (behavior-records (obj/slot-value split :right)))))))

(deftest compile-2-exposes-compound-cons-car-cdr
  (let [explicit (compile-source "(let-cell [pair head tail]
                                    (p:cons 1 2 pair)
                                    (p:car head pair)
                                    (p:cdr tail pair)
                                    (+ head tail))")
        explicit-net (run-compiled explicit)
        sugar (compile-source "(let-cell []
                                 (def pair (cons 1 2))
                                 (+ (car pair) (cdr pair)))")
        sugar-net (run-compiled sugar)]
    (is (= 3 (strongest explicit-net (:cell explicit))))
    (is (= 3 (strongest sugar-net (:cell sugar))))))

(deftest compile-2-list-builds-cons-chain
  (let [compiled (compile-source "(let-cell [xs tail]
                                    (def xs (list 1 2 3))
                                    (p:cdr tail xs)
                                    (+ (p:car xs) (p:car tail)))")
        n (run-compiled compiled)]
    (is (= 3 (strongest n (:cell compiled))))))

(deftest compile-2-cdr-gated-list-gur-hop-chain
  (testing "compiler-2 structural GUR should use cdr presence as the lazy hop guard"
    (let [compiled
          (compile-source "(let-cell [xs node1 tail hop1 out first rest second]
                            (def-net inc-list [xs] [out]
                              (let-cell [head rest mapped-head mapped-rest]
                                (p:car head xs)
                                (p:cdr rest xs)
                                (-> (+ head 1) mapped-head)
                                (p:cons mapped-head mapped-rest out)
                                (when rest
                                  (inc-list rest mapped-rest))))
                            (p:cons 2 tail node1)
                            (p:cons 1 node1 xs)
                            (inc-list xs hop1)
                            (inc-list hop1 out)
                            (p:car first out)
                            (p:cdr rest out)
                            (p:car second rest)
                            (+ (* first 10) second))")
          n (run-compiled compiled)]
      (is (= 34 (strongest n (:cell compiled)))))))

(defn- compile-2-map-chain-source
  [depth]
  (let [value-count 5
        node-syms (mapv #(symbol (str "node" %)) (range 1 value-count))
        hop-syms (mapv #(symbol (str "hop" %)) (range 1 depth))
        value-syms (mapv #(symbol (str "v" %)) (range value-count))
        rest-syms (mapv #(symbol (str "rest" %)) (range (dec value-count)))
        cells (vec (concat ['xs 'tail]
                           node-syms
                           hop-syms
                           ['out]
                           value-syms
                           rest-syms))
        double-list
        '(def-net double-list [xs] [out]
           (let-cell [head rest mapped-head mapped-rest]
             (p:car head xs)
             (p:cdr rest xs)
             (-> (* head 2) mapped-head)
             (p:cons mapped-head mapped-rest out)
             (when rest
               (double-list rest mapped-rest))))
        cons-forms
        (mapv (fn [coll tail]
                (list 'p:cons 1 tail coll))
              (into ['xs] node-syms)
              (conj node-syms 'tail))
        chain-forms
        (mapv (fn [in out]
                (list 'double-list in out))
              (into ['xs] hop-syms)
              (conj hop-syms 'out))
        read-forms
        (mapcat
         (fn [idx]
           (let [current (if (zero? idx)
                           'out
                           (rest-syms (dec idx)))
                 car-form (list 'p:car (value-syms idx) current)]
             (if (< idx (dec value-count))
               [car-form (list 'p:cdr (rest-syms idx) current)]
               [car-form])))
         (range value-count))
        result-form (cons '+ value-syms)]
    (pr-str (cons 'let-cell
                  (cons cells
                        (concat [double-list]
                                cons-forms
                                chain-forms
                                read-forms
                                [result-form]))))))

(deftest compile-2-cdr-gated-list-map-chain-depths
  (testing "compiler-2 map chains match the accumulating GUR hop depths"
    (doseq [depth [5 10 15]]
      (let [compiled (compile-source (compile-2-map-chain-source depth))
            n (run-compiled compiled)
            expected (* 5 (long (Math/pow 2 depth)))]
        (is (= expected (strongest n (:cell compiled)))
            (str "map-chain depth " depth))))))

(deftest compile-2-exposes-generic-slot
  (let [compiled (compile-source "(let-cell [obj]
                                    (p:slot :x 7 obj)
                                    (+ (p:slot :x obj) 1))")
        n (run-compiled compiled)]
    (is (= 8 (strongest n (:cell compiled))))))

(deftest compile-2-exposes-generic-slot-in-network-closure
  (let [compiled (compile-source "(let-cell [obj out]
                                    (def-net read-x [coll] [out]
                                      (p:slot :x coll))
                                    (p:slot :x 6 obj)
                                    (read-x obj out)
                                    (+ out 1))")
        n (run-compiled compiled)]
    (is (= 7 (strongest n (:cell compiled))))))

(deftest compile-2-exposes-generic-slot-write-in-network-closure
  (let [compiled (compile-source "(let-cell [obj]
                                    (def-net make-x [v] [out]
                                      (p:slot :x v out))
                                    (make-x 6 obj)
                                    (+ (p:slot :x obj) 1))")
        n (run-compiled compiled)]
    (is (= 7 (strongest n (:cell compiled))))))

(deftest compile-2-retains-primitive-application-ir
  (testing "primitive applications keep an inspectable application object"
    (let [compiled (compile-source "(+ 1 2)")
          [app-id] (main/compiled-applications (:net compiled))
          app-info (strongest (:net compiled) app-id)
          operator-ast (obj/slot-value app-info
                                       main/application-operator-ast-slot)]
      (is (= [app-id] (:applications compiled)))
      (is (application-value/application-info? app-info))
      (is (= :primitive
             (obj/slot-value app-info main/application-lowering-slot)))
      (is (= :symbol (ast/type operator-ast)))
      (is (= '+ (ast/name operator-ast)))
      (is (= 2 (count (obj/slot-value app-info
                                      main/application-arg-cells-slot))))
      (is (= (:cell compiled)
             (obj/slot-value app-info main/application-output-slot))))))

(deftest compile-2-retains-nested-primitive-application-ir
  (testing "nested primitive calls are retained as separate application records"
    (let [compiled (compile-source "(+ 1 (- 4 2))")
          app-ids (main/compiled-applications (:net compiled))
          operators (->> app-ids
                         (map #(strongest (:net compiled) %))
                         (map #(obj/slot-value
                                %
                                main/application-operator-ast-slot))
                         (map ast/name)
                         set)
          result-net (run-compiled compiled)]
      (is (= 2 (count app-ids)))
      (is (= #{'+ '-} operators))
      (is (= 3 (strongest result-net (:cell compiled)))))))

(deftest compile-2-dependency-env-emits-dependency-values
  (testing "default env remains raw while dependency env wraps arithmetic results"
    (let [raw-compiled (compile-source "(+ 1 2)")
          raw (run-compiled raw-compiled)
          compiled (compile-source "(+ 1 2)" (dependency-env) {})
          result-net (run-compiled compiled)
          result (strongest result-net (:cell compiled))
          sources (dependency/sources result)]
      (is (= 3 (strongest raw (:cell raw-compiled))))
      (is (dependency/dependency-value? result))
      (is (= 3 (dependency/base-value result)))
      (is (= 1 (count sources)))
      (is (= #{:compiler-2/application}
             (set (map :dependency/type sources)))))))

(deftest compile-2-default-arithmetic-preserves-operand-dependencies
  (testing "raw operands stay raw, but dependency-bearing operands are not unwrapped away"
    (let [[a-id base-net] (seeded-cell net/empty-net
                                       (dependency/dependency-value
                                        10
                                        #{:outer-source}))
          env (env/bind (default-env) 'a (env/cell-binding a-id) 0)
          compiled (compile-source "(+ a 5)" env {:net base-net})
          result (strongest (run-compiled compiled) (:cell compiled))]
      (is (dependency/dependency-value? result))
      (is (= 15 (dependency/base-value result)))
      (is (= #{:outer-source} (dependency/sources result))))))

(deftest compile-2-behavior-env-merges-same-timestamp-values
  (testing "compiled behavior arithmetic joins retained point histories"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 9 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:at 6 :value 9}]
             (behavior-records out-content))))))

(deftest compile-2-default-arithmetic-uses-behavior-current-values
  (testing "plain arithmetic is current-value arithmetic, not history join"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(+ a b)" env {:net n2})
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled))))
      (is (not (behavior/behavior-value?
                (net/network-cell-content result-net (:cell compiled))))))))

(deftest compile-2-default-arithmetic-lifts-event-values
  (testing "plain arithmetic over event cells emits derived event facts"
    (let [[a-id n1] (event-cell (behavior-protocol-net)
                                (event/active-event :a :slider-a 1 10))
          [b-id n2] (event-cell n1 (event/active-event :b :slider-b 1 4))
          [c-id n3] (event-cell n2 (event/active-event :c :slider-c 1 3))
          d-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0)
                  (env/bind 'c (env/cell-binding c-id) 0)
                  (env/bind 'd (env/cell-binding d-id) 0))
          compiled (compile-source "(-> (- (+ a c) b) d)"
                                   env
                                   {:net (nb/install-cell n3 d-id)})
          result-net (run-compiled compiled)]
      (is (= [9] (vec (vals (event-active-values result-net d-id)))))
      (is (event/event-content? (net/network-cell-content result-net d-id))))))

(deftest compile-2-event-arithmetic-uses-newer-events-and-retractions
  (testing "newer source events dominate older inputs"
    (let [[a-id n1] (event-cell (behavior-protocol-net)
                                (event/active-event :a :slider-a 1 10)
                                (event/active-event :a :slider-a 2 20))
          [b-id n2] (event-cell n1 (event/active-event :b :slider-b 1 4))
          [c-id n3] (event-cell n2 (event/active-event :c :slider-c 1 3))
          d-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0)
                  (env/bind 'c (env/cell-binding c-id) 0)
                  (env/bind 'd (env/cell-binding d-id) 0))
          compiled (compile-source "(-> (- (+ a c) b) d)"
                                   env
                                   {:net (nb/install-cell n3 d-id)})
          result-net (run-compiled compiled)]
      (is (= [19] (vec (vals (event-active-values result-net d-id)))))))

  (testing "source retractions clear derived output"
    (let [[a-id n1] (event-cell (behavior-protocol-net)
                                (event/active-event :a :slider-a 1 10)
                                (event/retraction-event :a :slider-a 2))
          [b-id n2] (event-cell n1 (event/active-event :b :slider-b 1 4))
          [c-id n3] (event-cell n2 (event/active-event :c :slider-c 1 3))
          d-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0)
                  (env/bind 'c (env/cell-binding c-id) 0)
                  (env/bind 'd (env/cell-binding d-id) 0))
          compiled (compile-source "(-> (- (+ a c) b) d)"
                                   env
                                   {:net (nb/install-cell n3 d-id)})
          result-net (run-compiled compiled)]
      (is (= {} (event-active-values result-net d-id))))))

(defn- nested-plus-source
  [sym length]
  (reduce (fn [expr _] (format "(+ %s 1)" expr))
          (name sym)
          (range length)))

(defn- panel-update-all
  [n epoch values]
  (reduce (fn [network [input-id cell-id value]]
            (run-event-update network
                              cell-id
                              (event/active-event input-id
                                                  :panel
                                                  epoch
                                                  value)))
          n
          values))

(deftest compile-2-event-reactivity-propagates-through-long-arithmetic-chain
  (let [chain-length 50
        [x-id n1] (event-cell (behavior-protocol-net)
                              (event/active-event :x :panel 1 0))
        out-id (ids/new-node-id)
        env (-> (default-env)
                (env/bind 'x (env/cell-binding x-id) 0)
                (env/bind 'out (env/cell-binding out-id) 0))
        compiled (compile-source (format "(-> %s out)"
                                         (nested-plus-source 'x chain-length))
                                 env
                                 {:net (nb/install-cell n1 out-id)})
        initial-net (run-compiled compiled)
        updated-net (run-event-update initial-net
                                      x-id
                                      (event/active-event :x :panel 2 10))]
    (is (= [chain-length] (event-active-value-list initial-net out-id)))
    (is (= [(+ 10 chain-length)]
           (event-active-value-list updated-net out-id)))))

(deftest compile-2-event-reactivity-propagates-through-complex-arithmetic-dag
  (let [[a-id n1] (event-cell (behavior-protocol-net)
                              (event/active-event :a :panel 1 2))
        [b-id n2] (event-cell n1 (event/active-event :b :panel 1 3))
        [c-id n3] (event-cell n2 (event/active-event :c :panel 1 11))
        [d-id n4] (event-cell n3 (event/active-event :d :panel 1 5))
        out-id (ids/new-node-id)
        env (-> (default-env)
                (env/bind 'a (env/cell-binding a-id) 0)
                (env/bind 'b (env/cell-binding b-id) 0)
                (env/bind 'c (env/cell-binding c-id) 0)
                (env/bind 'd (env/cell-binding d-id) 0)
                (env/bind 'out (env/cell-binding out-id) 0))
        source "(-> (+ (* (+ a b) (- c d))
                       (- (* a c) (+ b d)))
                    out)"
        compiled (compile-source source
                                 env
                                 {:net (nb/install-cell n4 out-id)})
        initial-net (run-compiled compiled)
        updated-net (panel-update-all initial-net
                                      2
                                      [[:a a-id 2]
                                       [:b b-id 7]
                                       [:c c-id 11]
                                       [:d d-id 5]])]
    (is (= [44] (event-active-value-list initial-net out-id)))
    (is (= [64] (event-active-value-list updated-net out-id)))))

(deftest compile-2-event-reactivity-propagates-through-switch
  (let [[x-id n1] (event-cell (behavior-protocol-net)
                              (event/active-event :x :panel 1 5))
        out-id (ids/new-node-id)
        env (-> (default-env)
                (env/bind 'x (env/cell-binding x-id) 0)
                (env/bind 'out (env/cell-binding out-id) 0))
        compiled (compile-source "(switch (+ x 1) true out)"
                                 env
                                 {:net (nb/install-cell n1 out-id)})
        initial-net (run-compiled compiled)
        updated-net (run-event-update initial-net
                                      x-id
                                      (event/active-event :x :panel 2 8))]
    (is (= [6] (event-active-value-list initial-net out-id)))
    (is (= [9] (event-active-value-list updated-net out-id)))))

(deftest compile-2-event-reactivity-uses-event-valued-switch-condition
  (let [[x-id n1] (event-cell (behavior-protocol-net)
                              (event/active-event :x :panel 1 5))
        [enabled-id n2] (event-cell n1
                                    (event/active-event :enabled
                                                        :panel
                                                        1
                                                        false))
        out-id (ids/new-node-id)
        env (-> (default-env)
                (env/bind 'x (env/cell-binding x-id) 0)
                (env/bind 'enabled (env/cell-binding enabled-id) 0)
                (env/bind 'out (env/cell-binding out-id) 0))
        compiled (compile-source "(switch (+ x 1) enabled out)"
                                 env
                                 {:net (nb/install-cell n2 out-id)})
        initially-disabled-net (run-compiled compiled)
        enabled-net (run-event-update initially-disabled-net
                                      enabled-id
                                      (event/active-event :enabled
                                                          :panel
                                                          2
                                                          true))]
    (is (= [] (event-active-value-list initially-disabled-net out-id)))
    (is (= [6] (event-active-value-list enabled-net out-id)))))

(deftest compile-2-event-reactivity-propagates-through-bi-sync-chain
  (testing "events flow from the left side through a <-> chain"
    (let [[a-id n1] (event-cell (behavior-protocol-net)
                                (event/active-event :a :panel 1 5))
          b-id (ids/new-node-id)
          c-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0)
                  (env/bind 'c (env/cell-binding c-id) 0))
          compiled (compile-source "(<-> a b c)"
                                   env
                                   {:net (-> n1
                                             (nb/install-cell b-id)
                                             (nb/install-cell c-id))})
          initial-net (run-compiled compiled)
          updated-net (run-event-update initial-net
                                        a-id
                                        (event/active-event :a :panel 2 8))]
      (is (= [5] (event-active-value-list initial-net c-id)))
      (is (= [8] (event-active-value-list updated-net c-id)))))

  (testing "events flow from the right side through a <-> chain"
    (let [a-id (ids/new-node-id)
          b-id (ids/new-node-id)
          [c-id n1] (event-cell (behavior-protocol-net)
                                (event/active-event :c :panel 1 12))
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0)
                  (env/bind 'c (env/cell-binding c-id) 0))
          compiled (compile-source "(<-> a b c)"
                                   env
                                   {:net (-> n1
                                             (nb/install-cell a-id)
                                             (nb/install-cell b-id))})
          result-net (run-compiled compiled)]
      (is (= [12] (event-active-value-list result-net a-id))))))

(deftest compile-2-behavior-env-does-not-imply-point-continuation
  (testing "compiled behavior arithmetic does not join different point timestamps"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)]
      (is (= value/nothing
             (strongest result-net (:cell compiled)))))))

(deftest compile-2-behavior-env-synchronizes-interval-overlap
  (testing "compiled behavior arithmetic emits only the common interval"
    (let [left (behavior-view [(hist/interval-record 0 10 2)] #{[:a 0]})
          right (behavior-view [(hist/interval-record 5 12 7)] #{[:b 5]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 9 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:from 5 :to 10 :value 9}]
             (behavior-records out-content))))))

(deftest compile-2-behavior-env-reacts-to-late-shared-timestamp
  (testing "a compiled behavior application updates when inputs gain a new shared tick"
    (let [left-6 (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right-6 (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
          left-6-8 (behavior-view [(hist/point-record 6 2)
                                   (hist/point-record 8 3)]
                                  #{[:a 6] [:a 8]})
          right-6-8 (behavior-view [(hist/point-record 6 7)
                                    (hist/point-record 8 10)]
                                   #{[:b 6] [:b 8]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left-6)
          [b-id n2] (behavior-cell n1 right-6)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          n3 (run-compiled compiled)
          [_left-tasks n4] (seed-behavior-message n3 a-id left-6-8)
          [right-tasks n5] (seed-behavior-message n4 b-id right-6-8)
          result-net (core/run-tasks right-tasks n5)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 13 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:at 6 :value 9}
              {:at 8 :value 13}]
             (behavior-records out-content))))))

(defn- propagator-inputs-writing-to
  [n out-id]
  (->> (net/net-graph n)
       (keep (fn [[id node]]
               (when (and (prop/prop? (get (net/net-env n) id))
                          (contains? (:outputs node) out-id))
                 (:inputs node))))))

(deftest compile-2-parser-supports-network-closure-marker
  (testing ":: parses into the internal network marker and requires explicit params"
    (let [parsed (parse "(:: [x] (+ x 1))")]
      (is (net/network? parsed))
      (is (= :network (ast/type parsed)))
      (is (= :network (obj/slot-value parsed ast/type-slot)))
      (is (= '[x] (ast/inputs parsed)))
      (is (= :apply (ast/type (ast/body parsed)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #":: params must be a vector"
                          (parse "(:: (+ x 1))")))))

(deftest compile-2-parser-supports-first-slice-network-syntax
  (testing "network and def-net parse output vectors over the existing closure path"
    (let [network-ast (parse "(network [x] [out] (+ x 1))")
          def-net-ast (parse "(def-net pair [x] [same next] (+ x 1))")]
      (is (= :compound (ast/type network-ast)))
      (is (= '[x] (ast/inputs network-ast)))
      (is (= '[out] (ast/output network-ast)))
      (is (= :def-net (ast/type def-net-ast)))
      (is (= 'pair (ast/name def-net-ast)))
      (is (= '[same next] (ast/output def-net-ast))))))

(deftest compile-2-parser-supports-def-and-def-cell
  (testing "def binds an expression, def-cell binds free cells, expressions, and zero-output closures"
    (let [def-ast (parse "(def answer (+ 1 2))")
          free-def-ast (parse "(def signal)")
          free-def-cell-ast (parse "(def-cell signal)")
          def-cells-ast (parse "(def-cells a b)")
          expr-def-cell-ast (parse "(def-cell inc (cell [x] (+ x 1)))")
          closure-cell-ast (parse "(def-cell inc [x] (+ x 1))")]
      (is (= :def (ast/type def-ast)))
      (is (= 'answer (ast/name def-ast)))
      (is (= :apply (ast/type (ast/body def-ast))))
      (is (= :def (ast/type free-def-ast)))
      (is (= 'signal (ast/name free-def-ast)))
      (is (nil? (ast/body free-def-ast)))
      (is (= :def (ast/type free-def-cell-ast)))
      (is (= 'signal (ast/name free-def-cell-ast)))
      (is (nil? (ast/body free-def-cell-ast)))
      (is (= :sequence (ast/type def-cells-ast)))
      (is (= :def (ast/type expr-def-cell-ast)))
      (is (= :network (ast/type (ast/body expr-def-cell-ast))))
      (is (= :def-cell (ast/type closure-cell-ast)))
      (is (= '[x] (ast/inputs closure-cell-ast)))
      (is (= :apply (ast/type (ast/body closure-cell-ast)))))))

(deftest compile-2-parser-supports-behavior-sugar
  (testing "behavior declaration aliases lower to ordinary compiler-2 sequences"
    (doseq [source ["(def-behavior a)"
                   "(def-behaviour a)"
                   "(def-behaviors a b c)"
                   "(define-behaviors a b c)"
                   "(let-behaviour [a b] (+ a b))"
                   "(let-behavior [a b] (+ a b))"]]
      (is (#{:sequence :let-cell} (ast/type (parse source)))
          source))))

(deftest compile-2-parser-supports-let-conditionals-and-def-constraint
  (testing "new immediate syntax parses onto compiler-2 AST"
    (let [let-ast (parse "(let [x 1 y (+ x 2)] y)")
          if-ast (parse "(if true 1 2)")
          when-ast (parse "(when ready (<-> 1 out))")
          cond-ast (parse "(cond [false 1 else 2])")
          constraint-ast (parse "(def-constraint same [a b] (<-> a b))")]
      (is (= :let (ast/type let-ast)))
      (is (= ['x 'y] (mapv first (ast/bindings let-ast))))
      (is (= :apply (ast/type if-ast)))
      (is (= 'if (ast/name (ast/operator if-ast))))
      (is (= :when-topology (ast/type when-ast)))
      (is (= 'ready (ast/name (ast/condition when-ast))))
      (is (= :apply (ast/type (ast/body when-ast))))
      (is (= :apply (ast/type cond-ast)))
      (is (= 'if (ast/name (ast/operator cond-ast))))
      (is (= :def-constraint (ast/type constraint-ast)))
      (is (= 'same (ast/name constraint-ast)))
      (is (= '[a b] (ast/inputs constraint-ast)))))
  (testing "invalid def-constraint syntax reports parser errors"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"def-constraint name must be a symbol"
                          (parse "(def-constraint 1 [a] a)")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"def-constraint inputs must be a vector"
                          (parse "(def-constraint c a a)")))))

(deftest compile-2-ast-accessors-accept-old-map-asts
  (testing "old AST maps normalize into slot-backed AST objects"
    (let [expr {:ast/type :apply
                :ast/operator {:ast/type :symbol :ast/name '+}
                :ast/args [{:ast/type :literal :ast/value 1}
                           {:ast/type :literal :ast/value 2}]}
          compiled (main/compile-expr expr)
          result-net (run-compiled compiled)]
      (is (= :apply (ast/type expr)))
      (is (= '+ (ast/name (ast/operator expr))))
      (is (= 3 (strongest result-net (:cell compiled)))))))

(deftest compile-2-network-closure-is-data-only
  (testing "closure declaration emits closure info, not a runtime Closure function"
    (let [compiled (compile-source "(:: [x] (+ x 1))")
          closure-info (strongest (:net compiled) (:cell compiled))]
      (is (closure-value/closure-info? closure-info))
      (is (not (closure/closure? closure-info)))
      (is (nil? (obj/slot-value closure-info main/closure-runtime-slot)))
      (is (= '[x] (obj/slot-value closure-info main/closure-inputs-slot)))
      (let [output (obj/slot-value closure-info main/closure-output-slot)
            body (obj/slot-value closure-info main/closure-body-slot)]
        (is (= 1 (count output)))
        (is (closure-value/implicit-return-symbol? (first output)))
        (is (= :apply (ast/type body)))
        (is (= '->
               (ast/name (ast/operator body))))))))

(deftest compile-2-implicit-return-closure-rewrites-final-body-form
  (testing "implicit return is ordinary output syntax over only the last body form"
    (let [compiled (compile-source "(:: [x] (-> 1 x) (+ x 1))")
          closure-info (strongest (:net compiled) (:cell compiled))
          [hidden] (obj/slot-value closure-info main/closure-output-slot)
          body (obj/slot-value closure-info main/closure-body-slot)
          forms (ast/body body)
          first-form (first forms)
          return-form (second forms)]
      (is (closure-value/implicit-return-symbol? hidden))
      (is (= :sequence (ast/type body)))
      (is (= 2 (count forms)))
      (is (= '->
             (ast/name (ast/operator first-form))))
      (is (= 'x
             (ast/name (last (ast/args first-form)))))
      (is (= '->
             (ast/name (ast/operator return-form))))
      (is (= '+
             (ast/name (ast/operator (first (ast/args return-form))))))
      (is (= hidden
             (ast/name (last (ast/args return-form))))))))

(deftest compile-2-closure-declaration-alone-does-not-evaluate-body
  (testing "declaring a network closure only installs closure data/slot topology"
    (let [compiled (compile-source "(:: [x] (+ x 1))")
          result-net (run-compiled compiled)]
      (is (= 1 (count (:props compiled))))
      (is (empty? (net/network-dict-entry result-net
                                          compiler-app/apply-application-props-key))))))

(deftest compile-2-application-installs-application-propagator
  (testing "network closure calls are evaluated by retained compiler-2 p:apply-application"
    (let [compiled (compile-source "((:: [x] (+ x 1)) 4)")
          apply-props (net/network-dict-entry
                       (:net compiled)
                       retained-app/retained-application-props-key)
          [app-id] (main/compiled-applications (:net compiled))
          app-info (strongest (:net compiled) app-id)
          result-net (run-compiled compiled)]
      (is (= 1 (count apply-props)))
      (is (application-value/application-info? app-info))
      (is (= :closure-cell
             (obj/slot-value app-info main/application-lowering-slot)))
      (is (contains? (set (:props compiled)) (first apply-props)))
      (is (= 5 (strongest result-net (:cell compiled)))))))

(deftest compile-2-presence-when-delays-body-topology
  (testing "when compiles the condition immediately and installs body topology only after a usable value"
    (let [expr (ast/sequence*
                (parse "(def-cells trigger out)")
                (parse "(when trigger (-> 1 out))")
                (parse "out"))
          compiled (main/compile-expr expr)
          trigger-id (:binding/id (env/lookup (:env compiled) 'trigger))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          n0 (run-compiled compiled)
          before-props (prop-count n0)]
      (is (= value/nothing (strongest n0 out-id)))
      (let [n1 (seed-and-run n0 trigger-id false)
            after-props (prop-count n1)]
        (is (= 1 (strongest n1 out-id)))
        (is (< before-props after-props))
        (is (= after-props
               (prop-count (seed-and-run n1 trigger-id :still-present))))))))

(deftest compile-2-forward-sync-propagates-false
  (testing "false is a usable value, so -> must not treat it as missing"
    (let [compiled (compile-source "(let-cell [out]
                                      (-> false out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= false (strongest result-net (:cell compiled))))))
  (testing "false also propagates through a closure output"
    (let [compiled (compile-source "(let-cell [out]
                                      (def-net f [x] [out]
                                        (-> (<= x 1) out))
                                      (f 2 out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= false (strongest result-net (:cell compiled)))))))

(deftest compile-2-named-closures-capture-copied-live-env
  (testing "a named closure's copied env contains its own binding"
    (let [compiled (compile-source "(def-net self [n] [out]
                                      (when n (self n out)))")
          self-id (:binding/id (env/lookup (:env compiled) 'self))
          closure-info (strongest (:net compiled) self-id)
          closure-env (closure-value/closure-env closure-info)]
      (is (= self-id
             (:binding/id (env/lookup closure-env 'self))))))
  (testing "same-scope later declarations propagate into the copied env"
    (let [compiled (compile-source "(let-cell [result]
                                      (def-net first [x] [out]
                                        (later x out))
                                      (def-net later [x] [out]
                                        (-> (+ x 1) out))
                                      first)")
          result-net (run-compiled compiled)
          first-id (:binding/id (env/lookup (:env compiled) 'first))
          later-id (:binding/id (env/lookup (:env compiled) 'later))
          closure-info (strongest result-net first-id)
          closure-env (closure-value/closure-env closure-info)]
      (is (= later-id
             (:binding/id (env/lookup closure-env 'later)))))))

(deftest compile-2-presence-when-supports-direct-recursive-style
  (testing "ordinary self-application inside presence-gated topology can terminate"
    (let [compiled (compile-source "(let-cell [out]
                                      (def-net down [n] [out]
                                        (let-cell [base? recur? a]
                                          (-> (<= n 1) base?)
                                          (-> (not base?) recur?)
                                          (when (switch true base?)
                                            (-> n out))
                                          (when (switch true recur?)
                                            (down (- n 1) a)
                                            (-> a out))))
                                      (down 4 out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= 1 (strongest result-net (:cell compiled)))))))

(deftest compile-2-presence-when-supports-fib-style-gur
  (testing "fib uses only closure self-application plus switch-gated when bodies"
    (doseq [[n expected] [[0 0] [1 1] [5 5] [6 8]]]
      (let [compiled (compile-source
                      (format "(let-cell [out]
                                 (def-net fib [n] [out]
                                   (let-cell [base? recur? a b]
                                     (-> (<= n 1) base?)
                                     (-> (not base?) recur?)
                                     (when (switch true base?)
                                       (-> n out))
                                     (when (switch true recur?)
                                       (fib (- n 1) a)
                                       (fib (- n 2) b)
                                       (-> (+ a b) out))))
                                 (fib %d out)
                                 out)"
                              n))
            result-net (run-compiled compiled)]
        (is (= expected (strongest result-net (:cell compiled)))
            (str "fib " n))))))

(deftest compile-2-supports-first-slice-network-and-def-net
  (testing "network output cells are explicit application applicants"
    (let [anonymous (compile-source "(let-cell [out]
                                       ((network [x] [out] (+ x 1)) 4 out)
                                       out)")
          named (compile-source "(let-cell [out]
                                   (def-net inc [x] [out] (+ x 1))
                                   (inc 5 out)
                                   out)")]
      (is (= 5 (scoped-base
                (strongest (run-compiled anonymous) (:cell anonymous)))))
      (is (= 6 (scoped-base
                (strongest (run-compiled named) (:cell named))))))))

(deftest compile-2-supports-def-and-def-cell
  (testing "def creates named cells, def-cell declares free cells, and def-cell names cell-producing expressions"
    (let [named-value (compile-source "(def answer (+ 1 2))")
          named-value-net (run-compiled named-value)
          answer-id (:binding/id (env/lookup (:env named-value) 'answer))
          free-def (compile-source "(def signal)")
          signal-id (:binding/id (env/lookup (:env free-def) 'signal))
          free-def-cell (compile-source "(def-cell signal)")
          free-def-cell-id (:binding/id (env/lookup (:env free-def-cell) 'signal))
          free-def-cells (compile-source "(def-cells a b)")
          a-id (:binding/id (env/lookup (:env free-def-cells) 'a))
          b-id (:binding/id (env/lookup (:env free-def-cells) 'b))
          expr-cell (compile-source "(let-cell [out]
                                       (def-cell inc (cell [x] (+ x 1)))
                                       (<-> (inc 4) out)
                                       out)")
          named-cell (compile-source "(let-cell [out]
                                        (def-cell inc [x] (+ x 1))
                                        (<-> (inc 4) out)
                                        out)")
          expr-cell-net (run-compiled expr-cell)
          named-cell-net (run-compiled named-cell)]
      (is (= (:cell named-value) answer-id))
      (is (= 3 (strongest named-value-net answer-id)))
      (is (= (:cell free-def) signal-id))
      (is (= value/nothing (strongest (:net free-def) signal-id)))
      (is (= (:cell free-def-cell) free-def-cell-id))
      (is (= value/nothing (strongest (:net free-def-cell) free-def-cell-id)))
      (is (some? a-id))
      (is (some? b-id))
      (is (= 5 (strongest expr-cell-net (:cell expr-cell))))
      (is (= 5 (strongest named-cell-net (:cell named-cell)))))))

(deftest compile-2-supports-let-conditionals-predicates-and-constraints
  (testing "let binds expression results through ordinary cells"
    (let [compiled (compile-source "(let [x 1
                                          y (+ x 2)]
                                      (+ y 3))")]
      (is (= 6 (strongest (run-compiled compiled) (:cell compiled))))))
  (testing "if and cond route value-level choices"
    (let [if-compiled (compile-source "(if false 1 2)")
          cond-compiled (compile-source "(cond [false 1
                                                true 2
                                                else 3])")]
      (is (= 2 (strongest (run-compiled if-compiled) (:cell if-compiled))))
      (is (= 2 (strongest (run-compiled cond-compiled) (:cell cond-compiled))))))
  (testing "branch writes only the selected output"
    (let [compiled (compile-source "(let-cell [then-out else-out]
                                      (branch false 1 then-out 2 else-out)
                                      else-out)")
          n (run-compiled compiled)]
      (is (= 2 (strongest n (:cell compiled))))))
  (testing "predicate operators project concrete values"
    (let [compiled (compile-source "(let-cell [a b c]
                                      (number? 3 a)
                                      (string? \"x\" b)
                                      (boolean? false c)
                                      (+ (if a 1 0)
                                         (+ (if b 10 0)
                                            (if c 100 0))))")]
      (is (= 111 (strongest (run-compiled compiled) (:cell compiled))))))
  (testing "def-constraint applications use each applicant as both input and output"
    (let [forward (compile-source "(let-cell [a b]
                                    (def-constraint same [x y]
                                      (<-> x y))
                                    (same a b)
                                    (-> 3 a)
                                    b)")
          reverse (compile-source "(let-cell [a b]
                                    (def-constraint same [x y]
                                      (<-> x y))
                                    (same a b)
                                    (-> 4 b)
                                    a)")
          reused (compile-source "(let-cell [a b c d]
                                   (def-constraint same [x y]
                                     (<-> x y))
                                   (same a b)
                                   (same c d)
                                   (-> 3 a)
                                   (-> 8 c)
                                   (+ b d))")
          lexical (compile-source "(let-cell [x y]
                                    (def bias 2)
                                    (def-constraint add-bias [a out]
                                      (<-> (+ a bias) out))
                                    (add-bias x y)
                                    (-> 5 x)
                                    y)")]
      (is (= 3 (strongest (run-compiled forward) (:cell forward))))
      (is (= 4 (strongest (run-compiled reverse) (:cell reverse))))
      (is (= 11 (strongest (run-compiled reused) (:cell reused))))
      (is (= 7 (strongest (run-compiled lexical) (:cell lexical)))))))

(deftest compile-2-network-requires-explicit-output-applicant
  (testing "declared-output network calls do not synthesize hidden output cells"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"network application requires explicit output cells"
         (compile-source "((network [x] [out] (+ x 1)) 4)")))))

(deftest compile-2-cell-expression-returns-body-result
  (testing "cell is the zero-output closure form for expression results"
    (let [compiled (compile-source "((cell [x] (+ x 1)) 4)")
          result-net (run-compiled compiled)]
      (is (= 5 (strongest result-net (:cell compiled)))))))

(deftest compile-2-network-and-def-net-support-multiple-explicit-outputs
  (testing "multi-output applications write to explicit output cells"
    (let [anonymous (compile-source
                     "(let-cell [same next]
                        ((network [x] [same next]
                           (<-> x same)
                           (<-> (+ x 1) next))
                         4 same next)
                        next)")
          named (compile-source
                 "(let-cell [same next]
                    (def-net pair [x] [same next]
                      (<-> x same)
                      (<-> (+ x 1) next))
                    (pair 5 same next)
                    next)")
          anonymous-net (run-compiled anonymous)
          named-net (run-compiled named)]
      (is (= 5 (strongest anonymous-net (:cell anonymous))))
      (is (= 6 (strongest named-net (:cell named)))))))

(deftest compile-2-multi-output-survives-nested-closure-application
  (testing "an outer closure can route explicit output cells through an inner network"
    (let [compiled (compile-source
                    "(let-cell [same next]
                       (def-net pair [x] [same next]
                         (<-> x same)
                         (<-> (+ x 1) next))
                       (def-net outer [x] [same next]
                         (pair x same next))
                       (outer 8 same next)
                       next)")
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled)))))))

(deftest compile-2-multi-output-late-bound-closure-application
  (testing "a declared-output application installed before the operator closure uses explicit output cells"
    (let [compiled (compile-source "(let-cell [some-net same next]
                                      (some-net 2 same next)
                                      next)")
          some-net-id (:binding/id (env/lookup (:env compiled) 'some-net))
          same-id (:binding/id (env/lookup (:env compiled) 'same))
          next-id (:binding/id (env/lookup (:env compiled) 'next))
          closure-compiled (compile-source
                            "(network [x] [same next]
                               (<-> x same)
                               (<-> (+ x 1) next))")
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 some-net-id closure-value)
          n2 (nb/run-propagators n1
                                 (nb/neighbor-propagator-ids n1 some-net-id))]
      (is (= value/nothing (strongest n0 next-id)))
      (is (= 2 (strongest n2 same-id)))
      (is (= 3 (strongest n2 next-id))))))

(deftest compile-2-application-output-adapter-is-not-materializing
  (testing "closure application projects result cells without a materialization helper"
    (let [source (slurp "propagators/compiler_2/runtime/application.clj")
          direct (compile-source "((:: [x] (+ x 1)) 4)")
          late (compile-source "(let-cell [some-net out]
                                 (some-net 4 out)
                                 out)")
          some-net-id (:binding/id (env/lookup (:env late) 'some-net))
          out-id (:binding/id (env/lookup (:env late) 'out))
          closure-compiled (compile-source "(network [x] [out]
                                             (<-> x out))")
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n0 (run-compiled late)
          n1 (nb/seed-cell n0 some-net-id closure-value)
          n2 (nb/run-propagators n1
                                 (nb/neighbor-propagator-ids n1 some-net-id))]
      (is (not (str/includes? source "materialize-slot-object")))
      (is (= 5 (strongest (run-compiled direct) (:cell direct))))
      (is (= 4 (strongest n2 out-id))))))

(deftest compile-2-bi-sync-chain-100
  (testing "compiler-2 handles a 100-hop <-> chain"
    (let [compiled (compile-source (bi-sync-chain-source 100))
          result-net (run-compiled compiled)]
      (is (= 1 (strongest result-net (:cell compiled)))))))

(deftest compile-2-env-lookup-uses-nearest-scope-source-shadowing
  (testing "a child frame binding shadows a parent frame binding"
    (let [parent-id (ids/new-node-id)
          child-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'x (env/cell-binding parent-id) 0)
                  env/enter-scope
                  (env/bind 'x (env/cell-binding child-id)))
          binding (env/lookup env 'x)]
      (is (= :cell (:binding/type binding)))
      (is (= child-id (:binding/id binding))))))

(deftest compile-2-env-extension-builds-frame-parent-and-chain
  (testing "child frames keep parent links and extend the parent chain"
    (let [parent (default-env)
          child (env/sub-env parent)
          sibling (env/sub-env parent)]
      (is (= parent (obj/slot-value child env/env-parent-key)))
      (is (= (conj (env/scope-chain parent) (env/scope-id child))
             (env/scope-chain child)))
      (is (= #{} (env/local-bindings child)))
      (is (not= (env/scope-id child) (env/scope-id sibling))))))

(deftest compile-2-p-sub-env-uses-stable-child-scope
  (testing "one child env cell keeps the same derived scope across parent updates"
    (let [late-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          child-env-id (ids/new-node-id)
          parent-env (default-env)
          parent-env-with-late (env/bind parent-env
                                         'late
                                         (env/cell-binding late-id)
                                         0)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id child-env-id])
                 (nb/seed-cell parent-env-id parent-env))
          [sub-prop n1] ((env/p:sub-env parent-env-id child-env-id) n0)
          n2 (nb/run-propagators n1 (installed-prop-ids sub-prop))
          [first-scope n2*] (read-slot n2 child-env-id env/env-scope-key)
          [first-bindings n2**] (read-slot n2* child-env-id env/env-local-bindings-key)
          n3 (nb/seed-cell n2 parent-env-id parent-env-with-late)
          n4 (nb/run-propagators n3
                                 (nb/neighbor-propagator-ids n3 parent-env-id))
          [updated-scope n4*] (read-slot n4 child-env-id env/env-scope-key)
          [updated-chain _] (read-slot n4* child-env-id env/env-scope-chain-key)]
      (is (= [:env/child child-env-id] first-scope))
      (is (= #{} first-bindings))
      (is (= first-scope updated-scope))
      (is (= (conj (env/scope-chain parent-env)
                   [:env/child child-env-id])
             updated-chain)))))

(deftest compile-2-lexical-access-emits-frame-scope-candidates
  (testing "lexical access retains declarations and selects the nearest candidate"
    (let [parent-id (ids/new-node-id)
          child-id (ids/new-node-id)
          env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          lexical-env (-> (default-env)
                          (env/bind 'x (env/cell-binding parent-id) 0)
                          env/enter-scope
                          (env/bind 'x (env/cell-binding child-id)))
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [env-id out-id])
                 (nb/seed-cell env-id lexical-env))
          [access-prop n1] ((env/p:lexical-access 'x env-id out-id) n0)
          n2 (nb/run-propagators n1 (installed-prop-ids access-prop))
          content (net/network-cell-content n2 out-id)
          selected (strongest n2 out-id)]
      (is (scope-source/scope-content? content))
      (is (= 2 (scoped-candidate-count content)))
      (is (scope-source/scope-value? selected))
      (is (= child-id (:binding/id (scoped-base selected)))))))

(deftest compile-2-lexical-access-conflict-belongs-to-cell-strongest
  (testing "equal-nearest candidates still contradict in scope-source cell semantics"
    (let [left-id (ids/new-node-id)
          right-id (ids/new-node-id)
          source-id (ids/new-node-id)
          chain-id (ids/new-node-id)
          left-value-id (ids/new-node-id)
          right-value-id (ids/new-node-id)
          out-id (ids/new-node-id)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [source-id
                                       chain-id
                                       left-value-id
                                       right-value-id
                                       out-id])
                 (nb/seed-cell source-id :child)
                 (nb/seed-cell chain-id [:root :child])
                 (nb/seed-cell left-value-id (env/cell-binding left-id))
                 (nb/seed-cell right-value-id (env/cell-binding right-id)))
          [left-prop n1] ((scope-source/p:scope-value source-id
                                                      chain-id
                                                      left-value-id
                                                      out-id)
                          n0)
          [right-prop n2] ((scope-source/p:scope-value source-id
                                                       chain-id
                                                       right-value-id
                                                       out-id)
                           n1)]
      (is (= value/contradiction
             (strongest (nb/run-propagators n2 [left-prop right-prop])
                        out-id))))))

(deftest compile-2-network-env-ops-build-scoped-compound-env
  (testing "scope propagators receive parent env one-way and bind locals into a fresh child env"
    (let [parent-x-id (ids/new-node-id)
          local-x-id (ids/new-node-id)
          parent-y-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          inherited-env-id (ids/new-node-id)
          local-binding-id (ids/new-node-id)
          scoped-env-id (ids/new-node-id)
          parent-env (-> (default-env)
                         (env/bind 'x
                                   (env/cell-binding parent-x-id)
                                   0)
                         (env/bind 'y
                                   (env/cell-binding parent-y-id)
                                   0))
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id
                                       inherited-env-id
                                       local-binding-id
                                       scoped-env-id])
                 (nb/seed-cell parent-env-id parent-env)
                 (nb/seed-cell local-binding-id
                               (env/cell-binding local-x-id)))
          [sub-prop n1] ((env/p:sub-env parent-env-id inherited-env-id) n0)
          [bind-prop n2] ((env/p:bind-local
                           'x
                           inherited-env-id
                           local-binding-id
                           scoped-env-id)
                          n1)
          n3 (nb/run-propagators n2
                                 (into (installed-prop-ids sub-prop)
                                       (installed-prop-ids bind-prop)))
          inherited-x-id (ids/new-node-id)
          scoped-x-id (ids/new-node-id)
          scoped-y-id (ids/new-node-id)
          [inherited-access n4] ((env/p:lexical-access 'x
                                                       inherited-env-id
                                                       inherited-x-id)
                                 n3)
          [scoped-x-access n5] ((env/p:lexical-access 'x
                                                     scoped-env-id
                                                     scoped-x-id)
                               n4)
          [scoped-y-access n6] ((env/p:lexical-access 'y
                                                     scoped-env-id
                                                     scoped-y-id)
                               n5)
          n7 (nb/run-propagators n6
                                 (into (into (installed-prop-ids inherited-access)
                                             (installed-prop-ids scoped-x-access))
                                       (installed-prop-ids scoped-y-access)))
          n8 (nb/seed-cell n7 parent-y-id :parent-y-ready)]
      (is (= parent-x-id (:binding/id (scoped-base (strongest n7 inherited-x-id)))))
      (is (= local-x-id (:binding/id (scoped-base (strongest n7 scoped-x-id)))))
      (is (= 1 (scoped-candidate-count (net/network-cell-content n7 scoped-x-id))))
      (is (= local-x-id (:binding/id (scoped-base (strongest n8 scoped-x-id)))))
      (is (= 1 (scoped-candidate-count (net/network-cell-content n8 scoped-x-id))))
      (is (= parent-y-id (:binding/id (scoped-base (strongest n8 scoped-y-id))))))))

(deftest compile-2-lexical-access-sees-late-parent-binding
  (testing "a lexical accessor installed before a parent binding payload wakes after the payload arrives"
    (let [late-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          parent-binding-id (ids/new-node-id)
          bound-parent-env-id (ids/new-node-id)
          child-env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          parent-env (default-env)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id
                                       parent-binding-id
                                       bound-parent-env-id
                                       child-env-id
                                       out-id])
                 (nb/seed-cell parent-env-id parent-env))
          [bind-prop n1] ((env/p:bind-local
                           'late
                           parent-env-id
                           parent-binding-id
                           bound-parent-env-id)
                          n0)
          [sub-prop n2] ((env/p:sub-env bound-parent-env-id child-env-id) n1)
          [access-prop n3] ((env/p:lexical-access 'late child-env-id out-id)
                            n2)
          n4 (nb/run-propagators n3
                                 (into (into (installed-prop-ids bind-prop)
                                             (installed-prop-ids sub-prop))
                                       (installed-prop-ids access-prop)))
          n5 (nb/seed-cell n4 parent-binding-id (env/cell-binding late-id))
          n6 (nb/run-propagators n5
                                 (nb/neighbor-propagator-ids n5 parent-binding-id))
          selected (strongest n6 out-id)]
      (is (= value/nothing (strongest n4 out-id)))
      (is (scope-source/scope-value? selected))
      (is (= late-id (:binding/id (scoped-base selected)))))))

(deftest compile-2-lexical-access-child-binding-survives-late-parent-update
  (testing "child lexical content remains nearest after a parent binding arrives later"
    (let [parent-x-id (ids/new-node-id)
          child-x-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          inherited-env-id (ids/new-node-id)
          child-binding-id (ids/new-node-id)
          scoped-env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          parent-env (default-env)
          parent-env-later (env/bind parent-env
                                     'x
                                     (env/cell-binding parent-x-id)
                                     0)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id
                                       inherited-env-id
                                       child-binding-id
                                       scoped-env-id
                                       out-id])
                 (nb/seed-cell parent-env-id parent-env)
                 (nb/seed-cell child-binding-id
                               (env/cell-binding child-x-id)))
          [sub-prop n1] ((env/p:sub-env parent-env-id inherited-env-id) n0)
          [bind-prop n2] ((env/p:bind-local
                           'x
                           inherited-env-id
                           child-binding-id
                           scoped-env-id)
                          n1)
          [access-prop n3] ((env/p:lexical-access 'x scoped-env-id out-id)
                            n2)
          n4 (nb/run-propagators n3
                                 (into (into (installed-prop-ids sub-prop)
                                             (installed-prop-ids bind-prop))
                                       (installed-prop-ids access-prop)))
          n5 (nb/seed-cell n4 parent-env-id parent-env-later)
          n6 (nb/run-propagators n5
                                 (nb/neighbor-propagator-ids n5 parent-env-id))
          before-update (strongest n4 out-id)
          after-update (strongest n6 out-id)
          before-content (net/network-cell-content n4 out-id)
          after-content (net/network-cell-content n6 out-id)]
      (is (scope-source/scope-value? before-update))
      (is (= child-x-id (:binding/id (scoped-base before-update))))
      (is (= 1 (scoped-candidate-count before-content)))
      (is (scope-source/scope-value? after-update))
      (is (= 1 (scoped-candidate-count after-content)))
      (is (= child-x-id (:binding/id (scoped-base after-update)))))))

(deftest compile-2-lexical-access-does-not-leak-parent-before-child-value
  (testing "child declaration metadata blocks parent traversal before the child value arrives"
    (let [parent-x-id (ids/new-node-id)
          child-x-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          inherited-env-id (ids/new-node-id)
          child-binding-id (ids/new-node-id)
          scoped-env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          parent-env (env/bind (default-env)
                               'x
                               (env/cell-binding parent-x-id)
                               0)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id
                                       inherited-env-id
                                       child-binding-id
                                       scoped-env-id
                                       out-id])
                 (nb/seed-cell parent-env-id parent-env))
          [sub-prop n1] ((env/p:sub-env parent-env-id inherited-env-id) n0)
          [bind-prop n2] ((env/p:bind-local
                           'x
                           inherited-env-id
                           child-binding-id
                           scoped-env-id)
                          n1)
          [access-prop n3] ((env/p:lexical-access 'x scoped-env-id out-id)
                            n2)
          n4 (nb/run-propagators n3
                                 (into (into (installed-prop-ids sub-prop)
                                             (installed-prop-ids bind-prop))
                                       (installed-prop-ids access-prop)))
          n5 (nb/seed-cell n4 child-binding-id (env/cell-binding child-x-id))
          n6 (nb/run-propagators n5
                                 (nb/neighbor-propagator-ids n5 child-binding-id))
          selected (strongest n6 out-id)]
      (is (= value/nothing (strongest n4 out-id)))
      (is (= 0 (scoped-candidate-count (net/network-cell-content n4 out-id))))
      (is (scope-source/scope-value? selected))
      (is (= child-x-id (:binding/id (scoped-base selected))))
      (is (= 1 (scoped-candidate-count (net/network-cell-content n6 out-id)))))))

(deftest compile-2-local-first-lexical-access-emits-raw-nearest-binding
  (testing "local-first access is for compiler dispatch, so it returns a raw binding"
    (let [parent-x-id (ids/new-node-id)
          child-x-id (ids/new-node-id)
          env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          lexical-env (-> (default-env)
                          (env/bind 'x (env/cell-binding parent-x-id) 0)
                          env/enter-scope
                          (env/bind 'x (env/cell-binding child-x-id)))
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [env-id out-id])
                 (nb/seed-cell env-id lexical-env))
          [access-prop n1] ((env/p:lexical-access-local-first 'x env-id out-id)
                            n0)
          n2 (nb/run-propagators n1 (installed-prop-ids access-prop))]
      (is (= (env/cell-binding child-x-id)
             (strongest n2 out-id)))
      (is (not (scope-source/scope-value? (strongest n2 out-id)))))))

(deftest compile-2-local-first-lexical-access-blocks-parent-before-local-value
  (testing "a declared local frame wins even while its binding value is pending"
    (let [parent-x-id (ids/new-node-id)
          child-x-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          inherited-env-id (ids/new-node-id)
          child-binding-id (ids/new-node-id)
          scoped-env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          parent-env (env/bind (default-env)
                               'x
                               (env/cell-binding parent-x-id)
                               0)
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [parent-env-id
                                       inherited-env-id
                                       child-binding-id
                                       scoped-env-id
                                       out-id])
                 (nb/seed-cell parent-env-id parent-env))
          [sub-prop n1] ((env/p:sub-env parent-env-id inherited-env-id) n0)
          [bind-prop n2] ((env/p:bind-local
                           'x
                           inherited-env-id
                           child-binding-id
                           scoped-env-id)
                          n1)
          [access-prop n3] ((env/p:lexical-access-local-first
                             'x
                             scoped-env-id
                             out-id)
                            n2)
          n4 (nb/run-propagators n3
                                 (into (into (installed-prop-ids sub-prop)
                                             (installed-prop-ids bind-prop))
                                       (installed-prop-ids access-prop)))
          n5 (nb/seed-cell n4 child-binding-id (env/cell-binding child-x-id))
          n6 (nb/run-propagators n5
                                 (nb/neighbor-propagator-ids n5 child-binding-id))]
      (is (= value/nothing (strongest n4 out-id)))
      (is (= (env/cell-binding child-x-id)
             (strongest n6 out-id))))))

(deftest compile-2-lexical-compound-uses-env-slot-not-hidden-captures
  (testing "compound declarations attach lexical env through slots, not hidden application inputs"
    (let [[bias-id base-net] (seeded-cell net/empty-net 10)
          env (env/bind (default-env) 'bias (env/cell-binding bias-id) 0)
          compiled (compile-source
                    "(let-cell [add-bias]
                       (<-> add-bias
                            (:: [x]
                              (+ x bias)))
                       (add-bias 5))"
                    env
                    {:net base-net})
          apply-inputs (propagator-inputs-writing-to (:net compiled)
                                                     (:cell compiled))
          result-net (run-compiled compiled)
          declarations (map #(obj/accessor-declarations-for result-net %)
                            (keys (net/net-env result-net)))]
      (is (some #(contains? % main/closure-env-slot) declarations))
      (is (not-any? #(contains? % bias-id) apply-inputs))
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-lexical-argument-shadows-parent-binding
  (testing "input bindings use a nearer scope source than inherited env bindings"
    (let [[outer-x-id base-net] (seeded-cell net/empty-net 100)
          env (env/bind (default-env) 'x (env/cell-binding outer-x-id) 0)
          compiled (compile-source
                    "(let-cell [inc-local]
                       (<-> inc-local
                            (:: [x]
                              (+ x 1)))
                       (inc-local 5))"
                    env
                    {:net base-net})
          result-net (run-compiled compiled)]
      (is (= 6 (strongest result-net (:cell compiled)))))))

(deftest compile-2-inner-local-does-not-write-parent-except-output
  (testing "a local cell that shadows a parent symbol stays local unless routed to the compound output"
    (let [[outer-x-id base-net] (seeded-cell net/empty-net 100)
          env (env/bind (default-env) 'x (env/cell-binding outer-x-id) 0)
          compiled (compile-source
                    "(let-cell [use-local-x]
                       (<-> use-local-x
                            (:: []
                              (let-cell [x]
                                (<-> 7 x)
                                x)))
                       (use-local-x))"
                    env
                    {:net base-net})
          result-net (run-compiled compiled)]
      (is (= 7 (strongest result-net (:cell compiled))))
      (is (= 100 (strongest result-net outer-x-id))))))

(deftest compile-2-escaped-closure-preserves-lexical-env-through-output
  (testing "a returned closure carries its lexical environment through the declared output"
    (let [compiled (compile-source
                    "(let-cell [make-adder]
                       (<-> make-adder
                            (:: [bias]
                              (:: [x]
                                (+ x bias))))
                       ((make-adder 10) 5))")
          result-net (run-compiled compiled)]
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-dependency-env-uses-active-closure-application-context
  (testing "nested closure arithmetic records the later inner application context"
    (let [compiled (compile-source
                    "(let-cell [make-adder]
                       (<-> make-adder
                            (:: [bias]
                              (:: [x]
                                (+ x bias))))
                       ((make-adder 10) 5))"
                    (dependency-env)
                    {})
          result-net (run-compiled compiled)
          result (strongest result-net (:cell compiled))
          sources (dependency/sources result)
          [source] (seq sources)]
      (is (dependency/dependency-value? result))
      (is (= 15 (dependency/base-value result)))
      (is (= 1 (count sources)))
      (is (= :compiler-2/application (:dependency/type source)))
      (is (= :symbol (ast/type (:context/operator source))))
      (is (= '+ (ast/name (:context/operator source)))))))

(deftest compile-2-dependency-env-unions-operand-dependencies
  (testing "operand dependencies are preserved while the result gets the active context"
    (let [[a-id base-net] (seeded-cell net/empty-net
                                       (dependency/dependency-value
                                        10
                                        #{:outer-source}))
          env (env/bind (dependency-env) 'a (env/cell-binding a-id) 0)
          compiled (compile-source
                    "(let-cell [add-a]
                       (<-> add-a
                            (:: [x]
                              (+ a x)))
                       (add-a 5))"
                    env
                    {:net base-net})
          result-net (run-compiled compiled)
          result (strongest result-net (:cell compiled))
          sources (dependency/sources result)
          context-sources (filter #(and (map? %)
                                         (= :compiler-2/application
                                            (:dependency/type %)))
                                  sources)]
      (is (dependency/dependency-value? result))
      (is (= 15 (dependency/base-value result)))
      (is (contains? sources :outer-source))
      (is (= 1 (count context-sources)))
      (is (= :symbol (ast/type (:context/operator (first context-sources)))))
      (is (= '+ (ast/name (:context/operator (first context-sources))))))))

(deftest compile-2-supports-multiple-nested-compounds-in-one-compound
  (testing "an outer compound can define and apply nested compound propagators"
    (let [compiled (compile-source
                    "(let-cell [outer]
                       (<-> outer
                            (:: [x]
                              (let-cell [inc scale-after-inc]
                                (<-> inc
                                     (:: [y]
                                       (+ y 1)))
                                (<-> scale-after-inc
                                     (:: [y]
                                       (let-cell [double]
                                         (<-> double
                                              (:: [v]
                                                (* v 2)))
                                         (double (inc y)))))
                                (+ (inc x) (scale-after-inc x)))))
                       (outer 4))")
          result-net (run-compiled compiled)]
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-multiple-compound-declarations-inside-one-compound
  (testing "one compound can declare several local compound propagators and apply them over its arguments"
    (let [compiled (compile-source
                    "(let-cell [pipeline]
                       (<-> pipeline
                            (:: [a b]
                              (let-cell [add2 mul2 inc]
                                (<-> add2
                                     (:: [x y]
                                       (+ x y)))
                                (<-> mul2
                                     (:: [x y]
                                       (* x y)))
                                (<-> inc
                                     (:: [x]
                                       (+ x 1)))
                                (+ (add2 a b)
                                   (mul2 (inc a) b)))))
                       (pipeline 3 4))")
          result-net (run-compiled compiled)]
      (is (= 23 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-bi-sync-operator
  (testing "<-> is an ordinary application with its own result cell"
    (let [[a-id n1] (seeded-cell net/empty-net 42)
          b-id (ids/new-node-id)
          n2 (nb/install-cell n1 b-id)
          env (-> (default-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(<-> a b)" env {:net n2})
          [app-id] (main/compiled-applications (:net compiled))
          app-info (strongest (:net compiled) app-id)
          result-net (run-compiled compiled)]
      (is (= (:cell compiled)
             (obj/slot-value app-info main/application-output-slot)))
      (is (not= b-id (:cell compiled)))
      (is (= 42 (strongest result-net b-id)))
      (is (= 42 (layer-strongest result-net
                                 (:cell compiled)
                                 scope-source/base-layer))))))

(deftest compile-2-supports-forward-sync-operator
  (testing "-> installs one-way sync and returns the output cell"
    (let [compiled (compile-source "(let-cell [out]
                                      (-> 42 out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= 42 (layer-strongest result-net
                                 (:cell compiled)
                                 scope-source/base-layer))))))

(deftest compile-2-supports-forward-sync-chain
  (testing "-> installs a one-way chain and returns the last cell"
    (let [compiled (compile-source "(let-cell [a b c]
                                      (-> 42 a b c)
                                      c)")
          result-net (run-compiled compiled)]
      (is (= 42 (layer-strongest result-net
                                 (:cell compiled)
                                 scope-source/base-layer))))))

(deftest compile-2-supports-bi-sync-chain
  (testing "<-> installs adjacent bidirectional links and returns the last cell"
    (let [compiled (compile-source "(let-cell [a b c]
                                      (<-> a b c)
                                      (<-> c 42)
                                      a)")
          result-net (run-compiled compiled)]
      (is (= 42 (layer-strongest result-net
                                 (:cell compiled)
                                 scope-source/base-layer))))))

(deftest compile-2-supports-switch-operator
  (testing "default env includes switch"
    (let [compiled (compile-source "(switch 9 true)")
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled))))))
  (testing "switch also supports an explicit output cell"
    (let [compiled (compile-source "(let-cell [out]
                                      (switch 9 true out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled)))))))

(deftest compile-2-switch-preserves-distributed-tms-through-forward-sync
  (let [compiled (compile-source "(let-cell [a gated out]
                                    (def value 2)
                                    (def premise :switch/source)
                                    (def epoch 0)
                                    (premise-input value premise epoch a)
                                    (switch a true gated)
                                    (-> (+ gated 3) out)
                                    out)"
                                 (default-env)
                                 {:net (tms-distributed-protocol-net)})
        result-net (run-compiled compiled)]
    (is (= 5 (distributed-current-value result-net (:cell compiled))))
    (is (contains? (distributed-slot-keys result-net (:cell compiled))
                   (tms/premise-slot-key :switch/source 0)))))

(deftest compiler-2-behavior-tms-env-supports-switch-and-forward-sync
  (testing "explicit-output switch gates behavior content"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-cell [events retained gated out]
                       (def-net retain-latest [acc next] [out]
                         (let-cell [full]
                           (behavior-add-event acc next full)
                           (behavior-retain-last full 1 out)))
                       (behavior-event 6 2 events)
                       (behavior-cell events (behavior-empty-state) retain-latest retained)
                       (switch retained true gated)
                       (-> (be:+ gated gated) out)
                       out)"
                    {:net (behavior-tms-protocol-net)})
          result-net (run-compiled compiled)]
      (is (= 4 (behavior-current-value result-net (:cell compiled))))))
  (testing "expression-style switch gates behavior content"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-cell [events retained out]
                       (def-net retain-latest [acc next] [out]
                         (let-cell [full]
                           (behavior-add-event acc next full)
                           (behavior-retain-last full 1 out)))
                       (behavior-event 6 2 events)
                       (behavior-cell events (behavior-empty-state) retain-latest retained)
                       (def gated (switch retained true))
                       (-> (be:+ gated gated) out)
                       out)"
                    {:net (behavior-tms-protocol-net)})
          result-net (run-compiled compiled)]
      (is (= 4 (behavior-current-value result-net (:cell compiled)))))))

(deftest compile-2-propagator-emits-runnable-network-value
  (testing "source AST/env cells can produce a compiled network cell"
    (let [[x-id n1] (seeded-cell net/empty-net 4)
          expr-id (ids/new-node-id)
          env-id (ids/new-node-id)
          compiled-id (ids/new-node-id)
          expr (parse "(+ x 1)")
          env (env/bind (default-env) 'x (env/cell-binding x-id) 0)
          n2 (-> n1
                 (nb/install-cell expr-id)
                 (nb/install-cell env-id)
                 (nb/install-cell compiled-id)
                 (nb/seed-cell expr-id expr)
                 (nb/seed-cell env-id env))
          [compile-prop n3] ((main/p:compile-expr expr-id env-id compiled-id) n2)
          outer-net (nb/run-propagators n3 [compile-prop])
          compiled-net (strongest outer-net compiled-id)
          result-net (nb/run-propagators compiled-net
                                         (main/compiled-props compiled-net))]
      (is (= 5 (strongest result-net
                          (main/compiled-result compiled-net)))))))

(deftest compile-2-supports-late-input-partial-evaluation
  (testing "compiled applications can run before inputs exist and produce output later"
    (let [compiled (compile-source "(+ a 2)")
          a-id (:binding/id (env/lookup (:env compiled) 'a))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 a-id 5)
          n2 (nb/run-propagators n1 (nb/neighbor-propagator-ids n1 a-id))]
      (is (= value/nothing (strongest n0 (:cell compiled))))
      (is (= 7 (strongest n2 (:cell compiled)))))))

(deftest compile-2-supports-naming-application-result-with-sync
  (testing "application result cells can be synced into named output cells"
    (let [compiled (compile-source "(let-cell [out]
                                      (<-> out (+ a 2))
                                      out)")
          a-id (:binding/id (env/lookup (:env compiled) 'a))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 a-id 5)
          n2 (nb/run-propagators n1 (nb/neighbor-propagator-ids n1 a-id))]
      (is (= out-id (:cell compiled)))
      (is (= value/nothing (strongest n0 out-id)))
      (is (= 7 (strongest n2 out-id))))))

(deftest compile-2-supports-late-compound-definition
  (testing "an unresolved operator cell uses the same application propagator when it later receives a closure"
    (let [compiled (compile-source "(let-cell [some-net out]
                                      (some-net 2 out)
                                      out)")
          some-net-id (:binding/id (env/lookup (:env compiled) 'some-net))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          closure-compiled
          (compile-source
           "(network [x] [out]
              (<-> (+ x 1) out))")
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 some-net-id closure-value)
          n2 (nb/run-propagators n1
                                 (nb/neighbor-propagator-ids n1 some-net-id))]
      (is (= value/nothing (strongest n0 out-id)))
      (is (= 3 (strongest n2 out-id))))))

(deftest compile-2-def-net-shadows-unresolved-operator-cell
  (testing "a later def-net gets a fresh binding and does not mutate an earlier unresolved application"
    (let [a-def (compile-source "(def a)")
          early (compile-source "(inc 1 a)"
                                (:env a-def)
                                {:net (:net a-def)})
          inc-id (:binding/id (env/lookup (:env early) 'inc))
          a-id (:binding/id (env/lookup (:env early) 'a))
          n0 (nb/run-propagators (:net early) (:props early))
          late (compile-source "(def-net inc [x] [out]
                                  (<-> (+ x 1) out))"
                               (:env early)
                               {:net n0})
          n1 (nb/run-propagators (:net late) (:props late))]
      (is (not= inc-id (:binding/id (env/lookup (:env late) 'inc))))
      (is (= value/nothing (strongest n0 a-id)))
      (is (= value/nothing (strongest n1 a-id))))))

(deftest compile-2-application-before-closure-waits-for-later-input-fire
  (testing "an application can exist before the operator closure and evaluate on a later input update"
    (let [compiled (compile-source "(let-cell [some-net out]
                                      (some-net a out)
                                      out)")
          some-net-id (:binding/id (env/lookup (:env compiled) 'some-net))
          a-id (:binding/id (env/lookup (:env compiled) 'a))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          closure-compiled (compile-source "(network [x] [out]
                                             (<-> (+ x 1) out))")
          closure-info (strongest (:net closure-compiled)
                                  (:cell closure-compiled))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 some-net-id closure-info)
          n2 (nb/run-propagators n1
                                 (nb/neighbor-propagator-ids n1 some-net-id))
          n3 (nb/seed-cell n2 a-id 8)
          n4 (nb/run-propagators n3
                                 (nb/neighbor-propagator-ids n3 a-id))]
      (is (= value/nothing (strongest n0 out-id)))
      (is (= value/nothing (strongest n2 out-id)))
      (is (= 9 (strongest n4 out-id))))))

(deftest execute-sub-env-builds-compound-child-env-and-reads-parent
  (let [x-id (ids/new-node-id)
        parent-env (env/bind (default-env) 'x (env/cell-binding x-id) 0)
        expr (execute-sub-env-ast (parse "x") parent-env)
        compiled (main/compile-expr expr (default-env)
                                    {:net (nb/install-cell net/empty-net
                                                           x-id
                                                           41
                                                           41)})
        result-net (run-compiled compiled)]
    (is (= 41 (strongest result-net (:cell compiled))))))

(deftest execute-sub-env-default-env-supports-switch-and-forward-sync
  (let [x-id (ids/new-node-id)
        parent-env (env/bind (default-env) 'x (env/cell-binding x-id) 0)
        outer-env (env/bind (default-env) 'x (env/cell-binding x-id) 0)
        expr (execute-sub-env-ast
              (parse "(let-cell [gated out]
                        (switch x true gated)
                        (-> gated out)
                        out)")
              parent-env
              'x)
        compiled (main/compile-expr expr
                                    outer-env
                                    {:net (nb/install-cell net/empty-net
                                                           x-id
                                                           41
                                                           41)})
        result-net (run-compiled compiled)]
    (is (= 41 (strongest result-net (:cell compiled))))))

(deftest execute-sub-env-behavior-tms-env-supports-switch-and-forward-sync
  (let [parent-env (behavior-tms-env)
        expr (execute-sub-env-ast
              (parse "(let-cell [events retained gated out]
                        (def-net retain-latest [acc next] [out]
                          (let-cell [full]
                            (behavior-add-event acc next full)
                            (behavior-retain-last full 1 out)))
                        (behavior-event 6 2 events)
                        (behavior-cell events
                                       (behavior-empty-state)
                                       retain-latest
                                       retained)
                        (switch retained true gated)
                        (-> (be:+ gated gated) out)
                        out)")
              parent-env)
        compiled (main/compile-expr-with-behavior-tms
                  expr
                  {:net (behavior-tms-protocol-net)})
        result-net (run-compiled compiled)]
    (is (= 4 (behavior-current-value result-net (:cell compiled))))))

(deftest execute-sub-env-uses-parent-env-reducer-storage
  (let [value-id (ids/new-node-id)
        [storage-id n1] (reducer-storage-cell net/empty-net)
        parent-env (-> (default-env)
                       (env/bind 'emit (reducer-emit-operator) 0)
                       (env/bind 'v (env/cell-binding value-id) 0)
                       (env/bind 'store (env/cell-binding storage-id) 0))
        outer-env (env/bind (default-env) 'v (env/cell-binding value-id) 0)
        expr (execute-sub-env-ast (parse "(emit v store)") parent-env 'v)
        compiled (main/compile-expr expr outer-env
                                    {:net (nb/install-cell n1 value-id 10 10)})
        result-net (run-compiled compiled)
        out (strongest result-net (:cell compiled))
        stored (strongest result-net storage-id)]
    (is (reducer/reduced-value? out))
    (is (= 10 (reducer/reduced-result out)))
    (is (= 10 (reducer/reduced-result stored)))))

(deftest execute-sub-env-reacts-to-later-reducer-slot-update
  (let [[input-id n1] (reducer-storage-cell net/empty-net)
        [storage-id n2] (reducer-storage-cell n1)
        parent-env (-> (default-env)
                       (env/bind 'emit (reducer-emit-operator) 0)
                       (env/bind 'v (env/cell-binding input-id) 0)
                       (env/bind 'store (env/cell-binding storage-id) 0))
        outer-env (env/bind (default-env) 'v (env/cell-binding input-id) 0)
        expr (execute-sub-env-ast (parse "(emit v store)") parent-env 'v)
        initial-input (reducer/reducer-slot-update execute-reducer-id
                                                   execute-merge-net
                                                   execute-strongest-net
                                                   [:input 0]
                                                   10)
        later-input (reducer/reducer-slot-update execute-reducer-id
                                                 execute-merge-net
                                                 execute-strongest-net
                                                 [:input 1]
                                                 20)
        [_tasks n3] (core/eval-cell input-id (message input-id initial-input) n2)
        compiled (main/compile-expr expr outer-env {:net n3})
        n6 (run-compiled compiled)
        [tasks n7] (core/eval-cell input-id (message input-id later-input) n6)
        n8 (core/run-tasks tasks n7)]
    (is (= 10 (reducer/reduced-result (strongest n6 (:cell compiled)))))
    (is (= 20 (reducer/reduced-result (strongest n8 (:cell compiled)))))
    (is (= 20 (reducer/reduced-result (strongest n8 storage-id))))))

(deftest execute-sub-env-can-emit-tms-facts-through-parent-storage
  (let [value-id (ids/new-node-id)
        premise-id (ids/new-node-id)
        active-id (ids/new-node-id)
        inactive-id (ids/new-node-id)
        tms-id (ids/new-node-id)
        tms-cell (tms/tms-cell)
        premise-value :p-from-cell
        parent-env (-> (default-env)
                       (env/bind 'claim (tms-claim-operator :c1 :answer
                                                            [(tms/support premise-value
                                                                          :child
                                                                          :derived)])
                                 0)
                       (env/bind 'premise-source
                                 (tms-premise-source-operator 0)
                                 0)
                       (env/bind 'premise-source-later
                                 (tms-premise-source-operator 1)
                                 0)
                       (env/bind 'premise-id (env/cell-binding premise-id) 0)
                       (env/bind 'value (env/cell-binding value-id) 0)
                       (env/bind 'active (env/cell-binding active-id) 0)
                       (env/bind 'inactive (env/cell-binding inactive-id) 0)
                       (env/bind 'tms (env/cell-binding tms-id) 0))
        outer-env (-> (default-env)
                      (env/bind 'premise-id (env/cell-binding premise-id) 0)
                      (env/bind 'value (env/cell-binding value-id) 0)
                      (env/bind 'active (env/cell-binding active-id) 0)
                      (env/bind 'inactive (env/cell-binding inactive-id) 0))
        expr (parse "(let-cell []
                       (premise-source premise-id active tms)
                       (premise-source-later premise-id inactive tms)
                       (claim value tms))")
        outer-expr (execute-sub-env-ast expr
                                        parent-env
                                        'premise-id
                                        'value
                                        'active
                                        'inactive)
        compiled (main/compile-expr
                  outer-expr
                  outer-env
                  {:net (-> net/empty-net
                            (nb/install-cell premise-id
                                             premise-value
                                             premise-value)
                            (nb/install-cell value-id :yes :yes)
                            (nb/install-cell active-id true true)
                            (nb/install-cell inactive-id)
                            (nb/install-cell tms-id
                                             tms-cell
                                             (reducer/strongest tms-cell)))})
        result-net (run-compiled compiled)
        view (reducer/reduced-result (strongest result-net tms-id))
        [inactive-tasks n1] (core/eval-cell inactive-id
                                            (message inactive-id false)
                                            result-net)
        inactive-net (core/run-tasks inactive-tasks n1)
        inactive-view (reducer/reduced-result (strongest inactive-net tms-id))]
    (is (= #{premise-value} (tms/active-premises view)))
    (is (= :yes (tms/proposition-value view :answer)))
    (is (value/nothing? (tms/proposition-value inactive-view :answer)))
    (is (= #{(tms/claim-slot-key :c1)
             (tms/premise-slot-key premise-value 0)
             (tms/premise-slot-key premise-value 1)
             (tms/latest-premise-slot-key premise-value)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content inactive-net tms-id))))))))

(deftest compiler-2-tms-premise-epoch-primitives-chain-belief-and-retraction
  (let [value-id (ids/new-node-id)
        premise-id (ids/new-node-id)
        believe-epoch-id (ids/new-node-id)
        retract-epoch-id (ids/new-node-id)
        tms-id (ids/new-node-id)
        tms-cell (tms/tms-cell)
        premise-value :p-from-cell
        env (-> (default-env)
                (env/bind 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (env/bind 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (env/bind 'claim (tms-claim-operator :c1 :answer
                                                     [(tms/support premise-value
                                                                   :compiler-2
                                                                   :derived)])
                          0)
                (env/bind 'premise-id (env/cell-binding premise-id) 0)
                (env/bind 'believe-epoch (env/cell-binding believe-epoch-id) 0)
                (env/bind 'retract-epoch (env/cell-binding retract-epoch-id) 0)
                (env/bind 'value (env/cell-binding value-id) 0)
                (env/bind 'tms (env/cell-binding tms-id) 0))
        expr (parse "(let-cell []
                       (believe-premise premise-id believe-epoch tms)
                       (retract-premise premise-id retract-epoch tms)
                       (claim value tms))")
        compiled (main/compile-expr
                  expr
                  env
                  {:net (-> net/empty-net
                            (nb/install-cell premise-id
                                             premise-value
                                             premise-value)
                            (nb/install-cell believe-epoch-id 0 0)
                            (nb/install-cell retract-epoch-id)
                            (nb/install-cell value-id :yes :yes)
                            (nb/install-cell tms-id
                                             tms-cell
                                             (reducer/strongest tms-cell)))})
        believed-net (run-compiled compiled)
        believed-view (reducer/reduced-result (strongest believed-net tms-id))
        [retract-tasks n1] (core/eval-cell retract-epoch-id
                                           (message retract-epoch-id 1)
                                           believed-net)
        retracted-net (core/run-tasks retract-tasks n1)
        retracted-view (reducer/reduced-result (strongest retracted-net tms-id))]
    (is (= #{premise-value} (tms/active-premises believed-view)))
    (is (= :yes (tms/proposition-value believed-view :answer)))
    (is (value/nothing? (tms/proposition-value retracted-view :answer)))
    (is (= #{(tms/claim-slot-key :c1)
             (tms/premise-slot-key premise-value 0)
             (tms/premise-slot-key premise-value 1)
             (tms/latest-premise-slot-key premise-value)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content retracted-net tms-id))))))))

(deftest compiler-2-tms-insert-uses-compiler-compound-pair
  (let [env (env/bind (default-env)
                      'tms-insert
                      (tms-insert-fact-operator)
                      0)
        compiled (compile-source "(let-cell []
                                    (def value :yes)
                                    (def premise :from-pair)
                                    (def tms)
                                    (def pair (cons value premise))
                                    (tms-insert tms (car pair) (cdr pair))
                                    tms)"
                                 env)
        n0 (run-compiled compiled)
        view (reducer/reduced-result (strongest n0 (:cell compiled)))]
    (is (= #{:from-pair} (tms/active-premises view)))
    (is (= :yes (tms/proposition-value view :answer)))
    (is (= #{(tms/claim-slot-key [:insert :from-pair])
             (tms/premise-slot-key :from-pair 0)
             (tms/latest-premise-slot-key :from-pair)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n0 (:cell compiled)))))))))

(deftest compiler-2-tms-closure-premise-output-switches-applied-definition
  (let [base-env (-> (default-env)
                     (env/bind 'premise-out
                               (tms-insert-fact-operator)
                               0)
                     (env/bind 'believe-premise
                               (tms-premise-epoch-operator true)
                               0)
                     (env/bind 'retract-premise
                               (tms-premise-epoch-operator false)
                               0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [one-out ten-out]
                       (def-net apply-out [f x] [out]
                         (f x out))
                       (def-net plus-one [x] [out]
                         (<-> (+ x 1) out))
                       (def-net plus-ten [x] [out]
                         (<-> (+ x 10) out))
                       (def x 5)
                       (def p-one :definition/plus-one)
                       (def p-ten :definition/plus-ten)
                       (def one-believe 0)
                       (def ten-believe 0)
                       (def tms)
                       (apply-out plus-one x one-out)
                       (apply-out plus-ten x ten-out)
                       (premise-out tms one-out p-one)
                       (premise-out tms ten-out p-ten)
                       (believe-premise p-one one-believe tms)
                       (believe-premise p-ten ten-believe tms)
                       tms)"
                    base-env
                    net/empty-net)
        env0 (:env setup)
        tms-id (env/binding-id (env/lookup env0 'tms))
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [one-retracted n1] (compile-step
                            "(let-cell []
                               (def one-retract 1)
                               (retract-premise p-one one-retract tms)
                               tms)"
                            env0
                            n0)
        view1 (reducer/reduced-result (strongest n1 tms-id))
        [one-brought n2] (compile-step
                          "(let-cell []
                             (def one-bring 2)
                             (believe-premise p-one one-bring tms)
                             tms)"
                          (:env one-retracted)
                          n1)
        view2 (reducer/reduced-result (strongest n2 tms-id))
        [_ten-retracted n3] (compile-step
                             "(let-cell []
                                (def ten-retract 3)
                                (retract-premise p-ten ten-retract tms)
                                tms)"
                             (:env one-brought)
                             n2)
        view3 (reducer/reduced-result (strongest n3 tms-id))]
    (is (= value/contradiction (tms/proposition-value view0 :answer)))
    (is (= 15 (tms/proposition-value view1 :answer)))
    (is (= value/contradiction (tms/proposition-value view2 :answer)))
    (is (= 6 (tms/proposition-value view3 :answer)))
    (is (= #{(tms/claim-slot-key [:insert :definition/plus-one])
             (tms/claim-slot-key [:insert :definition/plus-ten])
             (tms/premise-slot-key :definition/plus-one 0)
             (tms/premise-slot-key :definition/plus-one 1)
             (tms/premise-slot-key :definition/plus-one 2)
             (tms/premise-slot-key :definition/plus-ten 0)
             (tms/premise-slot-key :definition/plus-ten 3)
             (tms/latest-premise-slot-key :definition/plus-one)
             (tms/latest-premise-slot-key :definition/plus-ten)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n3 tms-id))))))))

(deftest legacy-compiler-2-premise-closure-sugars-premise-marked-network
  (let [base-env (-> (h/legacy-central-tms-env)
                     (env/bind 'believe-premise
                               (tms-premise-epoch-operator true)
                               0)
                     (env/bind 'retract-premise
                               (tms-premise-epoch-operator false)
                               0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [one-out ten-out]
                       (def-net plus-one [x] [out]
                         (<-> (+ x 1) out))
                       (def-net plus-ten [x] [out]
                         (<-> (+ x 10) out))
                       (def x 5)
                       (def p-one :definition/plus-one)
                       (def p-ten :definition/plus-ten)
                       (def one-believe 0)
                       (def ten-believe 0)
                       (def tms)
                       (def apply-one
                         (premise-closure
                           (network [f x] [out]
                             (f x out))
                           p-one
                           tms))
                       (def apply-ten
                         (premise-closure
                           (network [f x] [out]
                             (f x out))
                           p-ten
                           tms))
                       (apply-one plus-one x one-out)
                       (apply-ten plus-ten x ten-out)
                       (believe-premise p-one one-believe tms)
                       (believe-premise p-ten ten-believe tms)
                       tms)"
                    base-env
                    net/empty-net)
        env0 (:env setup)
        tms-id (env/binding-id (env/lookup env0 'tms))
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [one-retracted n1] (compile-step
                            "(let-cell []
                               (def one-retract 1)
                               (retract-premise p-one one-retract tms)
                               tms)"
                            env0
                            n0)
        view1 (reducer/reduced-result (strongest n1 tms-id))
        [_ten-retracted n2] (compile-step
                             "(let-cell []
                                (def ten-retract 2)
                                (retract-premise p-ten ten-retract tms)
                                tms)"
                             (:env one-retracted)
                             n1)
        view2 (reducer/reduced-result (strongest n2 tms-id))]
    (is (= value/contradiction (tms/proposition-value view0 :answer)))
    (is (= 15 (tms/proposition-value view1 :answer)))
    (is (value/nothing? (tms/proposition-value view2 :answer)))
    (is (= #{:definition/plus-one :definition/plus-ten}
           (set (keep (fn [slot-key]
                        (when (= :tms/claim (first slot-key))
                          (second (second slot-key))))
                      (keys (reducer/reducer-slots
                             (net/network-cell-content n2 tms-id)))))))))

(deftest compiler-2-tms-multiple-premises-retract-and-bring-in
  (let [value-id (ids/new-node-id)
        p1-id (ids/new-node-id)
        p2-id (ids/new-node-id)
        p1-believe-id (ids/new-node-id)
        p1-retract-id (ids/new-node-id)
        p1-bring-id (ids/new-node-id)
        p2-believe-id (ids/new-node-id)
        p2-retract-id (ids/new-node-id)
        tms-id (ids/new-node-id)
        tms-cell (tms/tms-cell)
        p1 :premise/a
        p2 :premise/b
        env (-> (default-env)
                (env/bind 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (env/bind 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (env/bind 'claim (tms-claim-operator :c1 :answer
                                                     [(tms/support p1
                                                                   :compiler-2
                                                                   :source-a)
                                                      (tms/support p2
                                                                   :compiler-2
                                                                   :source-b)])
                          0)
                (env/bind 'p1 (env/cell-binding p1-id) 0)
                (env/bind 'p2 (env/cell-binding p2-id) 0)
                (env/bind 'p1-believe (env/cell-binding p1-believe-id) 0)
                (env/bind 'p1-retract (env/cell-binding p1-retract-id) 0)
                (env/bind 'p1-bring (env/cell-binding p1-bring-id) 0)
                (env/bind 'p2-believe (env/cell-binding p2-believe-id) 0)
                (env/bind 'p2-retract (env/cell-binding p2-retract-id) 0)
                (env/bind 'value (env/cell-binding value-id) 0)
                (env/bind 'tms (env/cell-binding tms-id) 0))
        expr (parse "(let-cell []
                       (believe-premise p1 p1-believe tms)
                       (retract-premise p1 p1-retract tms)
                       (believe-premise p1 p1-bring tms)
                       (believe-premise p2 p2-believe tms)
                       (retract-premise p2 p2-retract tms)
                       (claim value tms))")
        compiled (main/compile-expr
                  expr
                  env
                  {:net (-> net/empty-net
                            (nb/install-cell p1-id p1 p1)
                            (nb/install-cell p2-id p2 p2)
                            (nb/install-cell p1-believe-id 0 0)
                            (nb/install-cell p1-retract-id)
                            (nb/install-cell p1-bring-id)
                            (nb/install-cell p2-believe-id 0 0)
                            (nb/install-cell p2-retract-id)
                            (nb/install-cell value-id :yes :yes)
                            (nb/install-cell tms-id
                                             tms-cell
                                             (reducer/strongest tms-cell)))})
        n0 (run-compiled compiled)
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [p1-retract-tasks n1] (core/eval-cell p1-retract-id
                                               (message p1-retract-id 1)
                                               n0)
        n2 (core/run-tasks p1-retract-tasks n1)
        view1 (reducer/reduced-result (strongest n2 tms-id))
        [p1-bring-tasks n3] (core/eval-cell p1-bring-id
                                            (message p1-bring-id 2)
                                            n2)
        n4 (core/run-tasks p1-bring-tasks n3)
        view2 (reducer/reduced-result (strongest n4 tms-id))
        [p2-retract-tasks n5] (core/eval-cell p2-retract-id
                                               (message p2-retract-id 3)
                                               n4)
        n6 (core/run-tasks p2-retract-tasks n5)
        view3 (reducer/reduced-result (strongest n6 tms-id))]
    (is (= :yes (tms/proposition-value view0 :answer)))
    (is (value/nothing? (tms/proposition-value view1 :answer)))
    (is (= :yes (tms/proposition-value view2 :answer)))
    (is (value/nothing? (tms/proposition-value view3 :answer)))
    (is (= #{(tms/claim-slot-key :c1)
             (tms/premise-slot-key p1 0)
             (tms/premise-slot-key p1 1)
             (tms/premise-slot-key p1 2)
             (tms/premise-slot-key p2 0)
             (tms/premise-slot-key p2 3)
             (tms/latest-premise-slot-key p1)
             (tms/latest-premise-slot-key p2)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n6 tms-id))))))))

(deftest compiler-2-tms-tracks-arithmetic-propagator-chain
  (let [a-id (ids/new-node-id)
        b-id (ids/new-node-id)
        c-id (ids/new-node-id)
        d-id (ids/new-node-id)
        f-id (ids/new-node-id)
        pa-id (ids/new-node-id)
        pb-id (ids/new-node-id)
        pc-id (ids/new-node-id)
        pd-id (ids/new-node-id)
        pa-believe-id (ids/new-node-id)
        pb-believe-id (ids/new-node-id)
        pc-believe-id (ids/new-node-id)
        pd-believe-id (ids/new-node-id)
        pa-retract-id (ids/new-node-id)
        pa-bring-id (ids/new-node-id)
        tms-id (ids/new-node-id)
        tms-cell (tms/tms-cell)
        pa :premise/a
        pb :premise/b
        pc :premise/c
        pd :premise/d
        env (-> (default-env)
                (env/bind 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (env/bind 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (env/bind 'claim (tms-claim-operator :chain :computed
                                                     [(tms/support pa :chain :a)
                                                      (tms/support pb :chain :b)
                                                      (tms/support pc :chain :c)
                                                      (tms/support pd :chain :d)])
                          0)
                (env/bind 'a (env/cell-binding a-id) 0)
                (env/bind 'b (env/cell-binding b-id) 0)
                (env/bind 'c (env/cell-binding c-id) 0)
                (env/bind 'd (env/cell-binding d-id) 0)
                (env/bind 'f (env/cell-binding f-id) 0)
                (env/bind 'pa (env/cell-binding pa-id) 0)
                (env/bind 'pb (env/cell-binding pb-id) 0)
                (env/bind 'pc (env/cell-binding pc-id) 0)
                (env/bind 'pd (env/cell-binding pd-id) 0)
                (env/bind 'pa-believe (env/cell-binding pa-believe-id) 0)
                (env/bind 'pb-believe (env/cell-binding pb-believe-id) 0)
                (env/bind 'pc-believe (env/cell-binding pc-believe-id) 0)
                (env/bind 'pd-believe (env/cell-binding pd-believe-id) 0)
                (env/bind 'pa-retract (env/cell-binding pa-retract-id) 0)
                (env/bind 'pa-bring (env/cell-binding pa-bring-id) 0)
                (env/bind 'tms (env/cell-binding tms-id) 0))
        expr (parse "(let-cell [e]
                       (<-> (* (+ (- a b) c) d) e)
                       (<-> e f)
                       (believe-premise pa pa-believe tms)
                       (believe-premise pb pb-believe tms)
                       (believe-premise pc pc-believe tms)
                       (believe-premise pd pd-believe tms)
                       (retract-premise pa pa-retract tms)
                       (believe-premise pa pa-bring tms)
                       (claim f tms))")
        compiled (main/compile-expr
                  expr
                  env
                  {:net (-> net/empty-net
                            (nb/install-cell a-id 8 8)
                            (nb/install-cell b-id 3 3)
                            (nb/install-cell c-id 2 2)
                            (nb/install-cell d-id 4 4)
                            (nb/install-cell f-id)
                            (nb/install-cell pa-id pa pa)
                            (nb/install-cell pb-id pb pb)
                            (nb/install-cell pc-id pc pc)
                            (nb/install-cell pd-id pd pd)
                            (nb/install-cell pa-believe-id 0 0)
                            (nb/install-cell pb-believe-id 0 0)
                            (nb/install-cell pc-believe-id 0 0)
                            (nb/install-cell pd-believe-id 0 0)
                            (nb/install-cell pa-retract-id)
                            (nb/install-cell pa-bring-id)
                            (nb/install-cell tms-id
                                             tms-cell
                                             (reducer/strongest tms-cell)))})
        n0 (run-compiled compiled)
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [retract-tasks n1] (core/eval-cell pa-retract-id
                                            (message pa-retract-id 1)
                                            n0)
        n2 (core/run-tasks retract-tasks n1)
        view1 (reducer/reduced-result (strongest n2 tms-id))
        [bring-tasks n3] (core/eval-cell pa-bring-id
                                         (message pa-bring-id 2)
                                         n2)
        n4 (core/run-tasks bring-tasks n3)
        view2 (reducer/reduced-result (strongest n4 tms-id))]
    (is (= 28 (strongest n0 f-id)))
    (is (= 28 (tms/proposition-value view0 :computed)))
    (is (value/nothing? (tms/proposition-value view1 :computed)))
    (is (= 28 (tms/proposition-value view2 :computed)))
    (is (= #{(tms/claim-slot-key :chain)
             (tms/premise-slot-key pa 0)
             (tms/premise-slot-key pa 1)
             (tms/premise-slot-key pa 2)
             (tms/premise-slot-key pb 0)
             (tms/premise-slot-key pc 0)
             (tms/premise-slot-key pd 0)
             (tms/latest-premise-slot-key pa)
             (tms/latest-premise-slot-key pb)
             (tms/latest-premise-slot-key pc)
             (tms/latest-premise-slot-key pd)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n4 tms-id))))))))

(deftest compiler-2-tms-conflicting-chain-claims-retract-and-switch
  (let [base-env (-> (default-env)
                     (env/bind 'believe-premise
                               (tms-premise-epoch-operator true)
                               0)
                     (env/bind 'retract-premise
                               (tms-premise-epoch-operator false)
                               0)
                     (env/bind 'claim-left
                               (tms-claim-operator :left
                                                   :shared
                                                   [(tms/support :premise/left
                                                                 :chain
                                                                 :left)])
                               0)
                     (env/bind 'claim-right
                               (tms-claim-operator :right
                                                   :shared
                                                   [(tms/support :premise/right
                                                                 :chain
                                                                 :right)])
                               0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [e-left f-left e-right f-right]
                       (def a 8)
                       (def b 3)
                       (def c 2)
                       (def d 4)
                       (def p-left :premise/left)
                       (def p-right :premise/right)
                       (def left-believe 0)
                       (def right-believe 0)
                       (def tms)
                       (<-> (* (+ (- a b) c) d) e-left)
                       (<-> e-left f-left)
                       (<-> (* (+ (- a c) b) d) e-right)
                       (<-> e-right f-right)
                       (believe-premise p-left left-believe tms)
                       (believe-premise p-right right-believe tms)
                       (claim-left f-left tms)
                       (claim-right f-right tms)
                       tms)"
                    base-env
                    net/empty-net)
        env0 (:env setup)
        id-of (fn [sym] (env/binding-id (env/lookup env0 sym)))
        tms-id (id-of 'tms)
        f-left-id (id-of 'f-left)
        f-right-id (id-of 'f-right)
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [left-retracted n1] (compile-step
                             "(let-cell []
                                (def left-retract 1)
                                (retract-premise p-left left-retract tms)
                                tms)"
                             env0
                             n0)
        view1 (reducer/reduced-result (strongest n1 tms-id))
        [left-brought n2] (compile-step
                           "(let-cell []
                              (def left-bring 2)
                              (believe-premise p-left left-bring tms)
                              tms)"
                           (:env left-retracted)
                           n1)
        view2 (reducer/reduced-result (strongest n2 tms-id))
        [right-retracted n3] (compile-step
                              "(let-cell []
                                 (def right-retract 3)
                                 (retract-premise p-right right-retract tms)
                                 tms)"
                              (:env left-brought)
                              n2)
        view3 (reducer/reduced-result (strongest n3 tms-id))
        [right-brought n4] (compile-step
                            "(let-cell []
                               (def right-bring 4)
                               (believe-premise p-right right-bring tms)
                               tms)"
                            (:env right-retracted)
                            n3)
        view4 (reducer/reduced-result (strongest n4 tms-id))
        [left-retracted-again n5] (compile-step
                                   "(let-cell []
                                      (def left-retract-2 5)
                                      (retract-premise p-left left-retract-2 tms)
                                      tms)"
                                   (:env right-brought)
                                   n4)
        view5 (reducer/reduced-result (strongest n5 tms-id))
        [left-brought-again n6] (compile-step
                                 "(let-cell []
                                    (def left-bring-2 6)
                                    (believe-premise p-left left-bring-2 tms)
                                    tms)"
                                 (:env left-retracted-again)
                                 n5)
        view6 (reducer/reduced-result (strongest n6 tms-id))
        [_right-retracted-again n7] (compile-step
                                     "(let-cell []
                                        (def right-retract-2 7)
                                        (retract-premise p-right right-retract-2 tms)
                                        tms)"
                                     (:env left-brought-again)
                                     n6)
        view7 (reducer/reduced-result (strongest n7 tms-id))]
    (is (= 28 (strongest n0 f-left-id)))
    (is (= 36 (strongest n0 f-right-id)))
    (is (= value/contradiction (tms/proposition-value view0 :shared)))
    (is (= 36 (tms/proposition-value view1 :shared)))
    (is (= value/contradiction (tms/proposition-value view2 :shared)))
    (is (= 28 (tms/proposition-value view3 :shared)))
    (is (= value/contradiction (tms/proposition-value view4 :shared)))
    (is (= 36 (tms/proposition-value view5 :shared)))
    (is (= value/contradiction (tms/proposition-value view6 :shared)))
    (is (= 28 (tms/proposition-value view7 :shared)))
    (is (= #{(tms/claim-slot-key :left)
             (tms/claim-slot-key :right)
             (tms/premise-slot-key :premise/left 0)
             (tms/premise-slot-key :premise/left 1)
             (tms/premise-slot-key :premise/left 2)
             (tms/premise-slot-key :premise/left 5)
             (tms/premise-slot-key :premise/left 6)
             (tms/premise-slot-key :premise/right 0)
             (tms/premise-slot-key :premise/right 3)
             (tms/premise-slot-key :premise/right 4)
             (tms/premise-slot-key :premise/right 7)
             (tms/latest-premise-slot-key :premise/left)
             (tms/latest-premise-slot-key :premise/right)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n7 tms-id))))))))

(deftest compiler-2-distributed-tms-premises-flow-through-chain
  (let [compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [a b c d e f]
                       (def va 8)
                       (def vb 3)
                       (def vc 2)
                       (def vd 4)
                       (def pa :premise/a)
                       (def pb :premise/b)
                       (def pc :premise/c)
                       (def pd :premise/d)
                       (def pa0 0)
                       (def pb0 0)
                       (def pc0 0)
                       (def pd0 0)
                       (premise-input va pa pa0 a)
                       (premise-input vb pb pb0 b)
                       (premise-input vc pc pc0 c)
                       (premise-input vd pd pd0 d)
                       (<-> (* (+ (- a b) c) d) e)
                       (<-> e f)
                       f)"
                    (default-env)
                    (tms-distributed-protocol-net))
        env0 (:env setup)
        id-of (fn [sym] (env/binding-id (env/lookup env0 sym)))
        a-id (id-of 'a)
        d-id (id-of 'd)
        e-id (id-of 'e)
        f-id (id-of 'f)
        [a-retracted n1] (compile-step
                          "(let-cell []
                             (def pa1 1)
                             (premise-retract pa pa1 a)
                             f)"
                          env0
                          n0)
        [a-brought n2] (compile-step
                        "(let-cell []
                           (def pa2 2)
                           (premise-believe pa pa2 a)
                           f)"
                        (:env a-retracted)
                        n1)
        [d-retracted n3] (compile-step
                          "(let-cell []
                             (def pd3 3)
                             (premise-retract pd pd3 d)
                             f)"
                          (:env a-brought)
                          n2)
        [_d-brought n4] (compile-step
                         "(let-cell []
                            (def pd4 4)
                            (premise-believe pd pd4 d)
                            f)"
                         (:env d-retracted)
                         n3)]
    (is (= 28 (distributed-current-value n0 f-id)))
    (is (contains? (distributed-slot-keys n0 f-id)
                   (tms/premise-slot-key :premise/a 0)))
    (is (value/nothing? (strongest n1 e-id)))
    (is (value/nothing? (strongest n1 f-id)))
    (is (contains? (distributed-slot-keys n1 f-id)
                   (tms/premise-slot-key :premise/a 1)))
    (is (= 28 (distributed-current-value n2 f-id)))
    (is (contains? (distributed-slot-keys n2 f-id)
                   (tms/premise-slot-key :premise/a 2)))
    (is (value/nothing? (strongest n3 f-id)))
    (is (contains? (distributed-slot-keys n3 f-id)
                   (tms/premise-slot-key :premise/d 3)))
    (is (= 28 (distributed-current-value n4 f-id)))
    (is (contains? (distributed-slot-keys n4 f-id)
                   (tms/premise-slot-key :premise/d 4)))
    (is (= #{a-id d-id}
           #{(env/binding-id (env/lookup (:env d-retracted) 'a))
             (env/binding-id (env/lookup (:env d-retracted) 'd))}))))

(deftest compiler-2-distributed-tms-wraps-network-declaration-closure
  (let [compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [a b c d f]
                       (def va 8)
                       (def vb 3)
                       (def vc 2)
                       (def vd 4)
                       (def pa :premise/a)
                       (def pb :premise/b)
                       (def pc :premise/c)
                       (def pd :premise/d)
                       (def pa0 0)
                       (def pb0 0)
                       (def pc0 0)
                       (def pd0 0)
                       (premise-input va pa pa0 a)
                       (premise-input vb pb pb0 b)
                       (premise-input vc pc pc0 c)
                       (premise-input vd pd pd0 d)
                       (def-net chain [a b c d] [out]
                         (* (+ (- a b) c) d))
                       (def tms-chain (tms-closure chain))
                       (tms-chain a b c d f)
                       f)"
                    (default-env)
                    (tms-distributed-protocol-net))
        env0 (:env setup)
        f-id (env/binding-id (env/lookup env0 'f))
        [a-retracted n1] (compile-step
                          "(let-cell []
                             (def pa1 1)
                             (premise-retract pa pa1 a)
                             f)"
                          env0
                          n0)
        [a-brought n2] (compile-step
                        "(let-cell []
                           (def pa2 2)
                           (premise-believe pa pa2 a)
                           f)"
                        (:env a-retracted)
                        n1)]
    (is (= 28 (distributed-current-value n0 f-id)))
    (is (value/nothing? (strongest n1 f-id)))
    (is (contains? (distributed-slot-keys n1 f-id)
                   (tms/premise-slot-key :premise/a 1)))
    (is (= 28 (distributed-current-value n2 f-id)))
    (is (contains? (distributed-slot-keys n2 f-id)
                   (tms/premise-slot-key :premise/a 2)))))

(deftest compiler-2-redefined-premise-closure-operator-switches-by-premise
  (let [compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [out]
                       (def-net plus-one [x] [out]
                         (<-> (+ x 1) out))
                       (def-net plus-ten [x] [out]
                         (<-> (+ x 10) out))
                       (def x 5)
                       (def p-one :definition/plus-one)
                       (def p-ten :definition/plus-ten)
                       (def e0 0)
                       (def op
                         (premise-closure
                           (network [f x] [out]
                             (f x out))
                           p-one
                           e0))
                       (op plus-one x out)
                       (def op
                         (premise-closure
                           (network [f x] [out]
                             (f x out))
                           p-ten
                           e0))
                       (op plus-ten x out)
                       out)"
                    (default-env)
                    (tms-distributed-protocol-net))
        env0 (:env setup)
        out-id (env/resolve-binding-id n0 env0 'out)
        [one-retracted n1] (compile-step
                          "(let-cell []
                             (def one-retract 1)
                             (premise-retract p-one one-retract out)
                             out)"
                          env0
                          n0)
        [one-brought n2] (compile-step
                         "(let-cell []
                            (def one-bring 2)
                            (premise-believe p-one one-bring out)
                            out)"
                         (:env one-retracted)
                         n1)
        [_ten-retracted n3] (compile-step
                             "(let-cell []
                                (def ten-retract 3)
                                (premise-retract p-ten ten-retract out)
                                out)"
                             (:env one-brought)
                             n2)]
    (is (value/contradiction? (strongest n0 out-id)))
    (is (seq (value/contradiction-provenance (strongest n0 out-id))))
    (is (= 15 (distributed-current-value n1 out-id)))
    (is (value/contradiction? (strongest n2 out-id)))
    (is (= 6 (distributed-current-value n3 out-id)))
    (is (contains? (distributed-slot-keys n3 out-id)
                   (tms/premise-slot-key :definition/plus-one 2)))
    (is (contains? (distributed-slot-keys n3 out-id)
                   (tms/premise-slot-key :definition/plus-ten 3)))))

(deftest compiler-2-distributed-premise-closure-marks-network-output
  (let [compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [x out]
                       (def vx 5)
                       (def px :premise/input)
                       (def pd :premise/definition)
                       (def e0 0)
                       (premise-input vx px e0 x)
                       (def-net inc [x] [out]
                         (<-> (+ x 1) out))
                       (def apply-inc
                         (premise-closure
                           (network [f x] [out]
                             (f x out))
                           pd
                           e0))
                       (apply-inc inc x out)
                       out)"
                    (default-env)
                    (tms-distributed-protocol-net))
        env0 (:env setup)
        out-id (env/binding-id (env/lookup env0 'out))
        [definition-retracted n1] (compile-step
                                   "(let-cell []
                                      (def pd1 1)
                                      (premise-retract pd pd1 out)
                                      out)"
                                   env0
                                   n0)
        [definition-brought n2] (compile-step
                                 "(let-cell []
                                    (def pd2 2)
                                    (premise-believe pd pd2 out)
                                    out)"
                                 (:env definition-retracted)
                                 n1)
        [_input-retracted n3] (compile-step
                               "(let-cell []
                                  (def px3 3)
                                  (premise-retract px px3 x)
                                  out)"
                               (:env definition-brought)
                               n2)]
    (is (= 6 (distributed-current-value n0 out-id)))
    (is (contains? (distributed-slot-keys n0 out-id)
                   (tms/premise-slot-key :premise/input 0)))
    (is (contains? (distributed-slot-keys n0 out-id)
                   (tms/premise-slot-key :premise/definition 0)))
    (is (value/nothing? (strongest n1 out-id)))
    (is (contains? (distributed-slot-keys n1 out-id)
                   (tms/premise-slot-key :premise/definition 1)))
    (is (= 6 (distributed-current-value n2 out-id)))
    (is (contains? (distributed-slot-keys n2 out-id)
                   (tms/premise-slot-key :premise/definition 2)))
    (is (value/nothing? (strongest n3 out-id)))
    (is (contains? (distributed-slot-keys n3 out-id)
                   (tms/premise-slot-key :premise/input 3)))))

(deftest compiler-2-distributed-tms-composes-with-behavior-arithmetic
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:left 6]})
        right (behavior-view [(hist/point-record 6 7)] #{[:right 6]})
        [left-id n1] (behavior-cell (behavior-tms-protocol-net) left)
        [right-id n2] (behavior-cell n1 right)
        env (-> (behavior-tms-env)
                (env/bind 'left-source (env/cell-binding left-id) 0)
                (env/bind 'right-source (env/cell-binding right-id) 0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [a b out]
                       (def p-left :premise/left)
                       (def p-right :premise/right)
                       (def p-left0 0)
                       (def p-right0 0)
                       (premise-content-input left-source p-left p-left0 a)
                       (premise-content-input right-source p-right p-right0 b)
                       (<-> (be:+ a b) out)
                       out)"
                    env
                    n2)
        env0 (:env setup)
        out-id (env/binding-id (env/lookup env0 'out))
        [left-retracted n3] (compile-step
                             "(let-cell []
                                (def p-left1 1)
                                (premise-retract p-left p-left1 a)
                                out)"
                             env0
                             n0)
        [left-brought n4] (compile-step
                           "(let-cell []
                              (def p-left2 2)
                              (premise-believe p-left p-left2 a)
                              out)"
                           (:env left-retracted)
                           n3)]
    (is (= 9 (distributed-behavior-current-value n0 out-id)))
    (is (= [{:at 6 :value 9}]
           (distributed-behavior-records n0 out-id)))
    (is (value/nothing? (strongest n3 out-id)))
    (is (contains? (distributed-slot-keys n3 out-id)
                   (tms/premise-slot-key :premise/left 1)))
    (is (= 9 (distributed-behavior-current-value n4 out-id)))
    (is (contains? (distributed-slot-keys n4 out-id)
                   (tms/premise-slot-key :premise/left 2)))))

(deftest execute-sub-env-reuses-behavior-arithmetic-point-join
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left)
        [b-id n2] (behavior-cell n1 right)
        parent-env (-> (behavior-env)
                       (env/bind 'a (env/cell-binding a-id) 0)
                       (env/bind 'b (env/cell-binding b-id) 0))
        outer-env (-> (default-env)
                      (env/bind 'a (env/cell-binding a-id) 0)
                      (env/bind 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (main/compile-expr expr outer-env {:net n2})
        result-net (run-compiled compiled)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}]
           (behavior-records out-content)))))

(deftest lexical-binding-access-preserves-and-refines-provenance
  (let [answer-id (ids/new-node-id)
        bound-id (ids/new-node-id)
        out-id (ids/new-node-id)
        token {:provenance/type :lexical-access
               :lookup/key :binding-test
               :scope/source :child
               :scope/chain [:root :child]}
        answer (scope-source/scope-value
                :child nil [:root :child] (env/cell-binding bound-id) #{token})
        n0 (-> (scope-source-protocol-net)
               (nb/install-cell answer-id)
               (nb/install-cell bound-id)
               (nb/install-cell out-id)
               (nb/seed-cell answer-id answer)
               (nb/seed-cell bound-id 12))
        [props n1] ((env/p:access-binding answer-id out-id) n0)
        n2 (nb/run-propagators n1 props)
        selected (strongest n2 out-id)]
    (is (= 12 (scope-source/base-value selected)))
    (is (= #{token} (scope-source/dependencies selected)))))

(deftest compiler-lexical-value-fast-path-keeps-live-scope-dependency
  (let [compiled (compile-source "(let-cell [x] x)")
        frame (get-in (net/network-dict-entry (:net compiled)
                                              env/lexical-topology-key)
                      [:frames (:env compiled)])
        bound-id (first (get-in frame [:bindings 'x]))
        prop-names (->> (vals (net/net-env (:net compiled)))
                        (filter prop/prop?)
                        (map prop/prop-name)
                        set)
        waiting (run-compiled compiled)
        with-value (nb/seed-cell waiting bound-id 12)
        settled (nb/run-propagators
                 with-value
                 (nb/neighbor-propagator-ids with-value bound-id))
        selected (strongest settled (:cell compiled))
        dependency (first (scope-source/dependencies selected))]
    (is (ids/node-id? (:scope/source-id frame)))
    (is (ids/node-id? (:scope/chain-id frame)))
    (is (ids/node-id? bound-id))
    (is (not (contains? prop-names :lexical-access/binding-candidates)))
    (is (not (contains? prop-names :lexical-access/access-binding)))
    (is (scope-source/scope-value?
         (strongest waiting (:cell compiled))))
    (is (= value/nothing
           (scope-source/base-value (strongest waiting (:cell compiled)))))
    (is (scope-source/scope-value? selected))
    (is (= 12 (scope-source/base-value selected)))
    (is (= :lexical-access (:provenance/type dependency)))
    (is (= (:scope/source dependency)
           (peek (:scope/chain dependency))))))

(deftest compiler-lexical-value-fallback-has-the-same-result-shape
  (let [bound-id (ids/new-node-id)
        base-net (nb/seed-cell (nb/install-cell net/empty-net bound-id)
                               bound-id
                               12)
        lexical-env (env/bind (default-env)
                              'x
                              (env/cell-binding bound-id)
                              0)
        compiled (compile-source "x" lexical-env {:net base-net})
        selected (strongest (run-compiled compiled) (:cell compiled))
        dependency (first (scope-source/dependencies selected))]
    (is (scope-source/scope-value? selected))
    (is (= 12 (scope-source/base-value selected)))
    (is (= :lexical-access (:provenance/type dependency)))))

(deftest execute-sub-env-reuses-behavior-arithmetic-point-non-continuation
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left)
        [b-id n2] (behavior-cell n1 right)
        parent-env (-> (behavior-env)
                       (env/bind 'a (env/cell-binding a-id) 0)
                       (env/bind 'b (env/cell-binding b-id) 0))
        outer-env (-> (default-env)
                      (env/bind 'a (env/cell-binding a-id) 0)
                      (env/bind 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (main/compile-expr expr outer-env {:net n2})
        result-net (run-compiled compiled)]
    (is (= value/nothing (strongest result-net (:cell compiled))))))

(deftest execute-sub-env-reuses-behavior-arithmetic-late-shared-timestamp
  (let [left-6 (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right-6 (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
        left-6-8 (behavior-view [(hist/point-record 6 2)
                                 (hist/point-record 8 3)]
                                #{[:a 6] [:a 8]})
        right-6-8 (behavior-view [(hist/point-record 6 7)
                                  (hist/point-record 8 10)]
                                 #{[:b 6] [:b 8]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left-6)
        [b-id n2] (behavior-cell n1 right-6)
        parent-env (-> (behavior-env)
                       (env/bind 'a (env/cell-binding a-id) 0)
                       (env/bind 'b (env/cell-binding b-id) 0))
        outer-env (-> (default-env)
                      (env/bind 'a (env/cell-binding a-id) 0)
                      (env/bind 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (main/compile-expr expr outer-env {:net n2})
        n4 (run-compiled compiled)
        [_left-tasks n5] (seed-behavior-message n4 a-id left-6-8)
        [right-tasks n6] (seed-behavior-message n5 b-id right-6-8)
        result-net (core/run-tasks right-tasks n6)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value n4 (:cell compiled))))
    (is (= 13 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}
            {:at 8 :value 13}]
           (behavior-records out-content)))))

(deftest compiler-2-application-can-execute-sub-env-behavior-arithmetic
  (let [left-6 (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right-6 (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
        left-6-8 (behavior-view [(hist/point-record 6 2)
                                 (hist/point-record 8 3)]
                                #{[:a 6] [:a 8]})
        right-6-8 (behavior-view [(hist/point-record 6 7)
                                  (hist/point-record 8 10)]
                                 #{[:b 6] [:b 8]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left-6)
        [b-id n2] (behavior-cell n1 right-6)
        inner-env (-> (behavior-env)
                      (env/bind 'a (env/cell-binding a-id) 0)
                      (env/bind 'b (env/cell-binding b-id) 0))
        outer-env (-> (default-env)
                      (env/bind 'a (env/cell-binding a-id) 0)
                      (env/bind 'b (env/cell-binding b-id) 0))
        outer-expr (execute-sub-env-ast (parse "(be:+ a b)") inner-env 'a 'b)
        compiled (main/compile-expr outer-expr outer-env {:net n2})
        n3 (run-compiled compiled)
        [_left-tasks n4] (seed-behavior-message n3 a-id left-6-8)
        [right-tasks n5] (seed-behavior-message n4 b-id right-6-8)
        result-net (core/run-tasks right-tasks n5)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value n3 (:cell compiled))))
    (is (= 13 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}
            {:at 8 :value 13}]
           (behavior-records out-content)))))
