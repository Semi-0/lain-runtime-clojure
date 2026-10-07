(ns propagators.runtime.compile-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [propagators.infra.cells.cell-protocol :as protocol]
            [propagators.infra.cells.value :as value]
            [propagators.infra.closure :as closure]
            [propagators.compiler.lowering.application :as compiler-app]
            [propagators.runtime.session.program.source :as program-source]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.closure-value :as closure-value]
            [propagators.compiler.model.env :as env]
            [propagators.compiler.compiler.basis :as h]
            [propagators.compiler.cps-core :as cps-core]
            [propagators.compiler.main :as main]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.language.parser :as parser]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.behavior :as behavior]
            [propagators.infra.datastructures.behavior-algebra :as hist]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.dependency :as dependency]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.datastructures.reducer-cell :as reducer]
            [propagators.infra.datastructures.scope-source :as scope-source]
            [propagators.infra.datastructures.tms :as tms]
            [propagators.infra.ids :as ids]
            [propagators.infra.gur :as gur]
            [propagators.infra.layered :as layered]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

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
    (nb/run-propagators n' tasks)))

(defn- seeded-cell
  [n v]
  (let [id (ids/new-node-id)]
    [id (nb/seed-cell (nb/install-cell n id) id v)]))

(defn- behavior-protocol-net
  []
  (protocol/prefer-direct-standard-protocols net/empty-net))

(defn- behavior-tms-protocol-net
  []
  (protocol/prefer-direct-standard-protocols net/empty-net))

(defn- scope-source-protocol-net
  []
  (protocol/prefer-direct-standard-protocols net/empty-net))

(defn- tms-distributed-protocol-net
  []
  (protocol/prefer-direct-standard-protocols net/empty-net))

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
    (nb/run-propagators n' tasks)))

(defn- with-binding
  ([bindings sym candidate]
   (with-binding bindings sym candidate nil))
  ([bindings sym candidate _depth]
   (let [candidate
         (if (and (fn? candidate) (h/application-activate candidate))
           (compiler-app/primitive-callable
            [:compile-2-test sym]
            (h/stable-node-id :compile-2-test :operator sym)
            (fn [network arg-ids out-id _context-id]
              (let [selected (h/output-id candidate arg-ids out-id)
                    inputs (vec (remove #{selected} arg-ids))
                    prop-id (h/stable-node-id :compile-2-test
                                              :operator-activation
                                              sym arg-ids selected)
                    prepared (reduce nb/ensure-cell network
                                     (conj (vec arg-ids) selected))
                    [installed-id installed]
                    ((prop/construct-propagator
                      prop-id
                      [:compile-2-test sym]
                      (prop/concrete-propagator
                       (fn [_inputs _outputs current-net]
                         ((h/application-activate candidate)
                          current-net nil arg-ids selected)))
                      inputs
                      [selected])
                     prepared)]
                [installed [installed-id] selected]))
            {:test/operator sym})
           candidate)]
     (conj (into [] (remove #(= sym (first %))) bindings)
           [sym candidate]))))

(defn- binding-value
  [bindings sym]
  (some (fn [[name candidate]]
          (when (= name sym) candidate))
        bindings))

(defn- live-env
  [network bindings]
  (let [env-id (ids/new-node-id)
        declared (env/declare-root network env-id bindings)]
    {:env env-id
     :net (nb/run-propagators (:net declared) (:props declared))}))

(defn- selected-env
  [available symbols]
  (mapv
   (fn [sym]
     (let [binding (binding-value available sym)]
       (if binding
         [sym binding]
         (throw (ex-info "Default operator is unavailable"
                         {:symbol sym})))))
   symbols))

(defn- selected-default-env
  [& symbols]
  (selected-env (h/default-bindings) symbols))

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
  (let [v (scope-source/unwrap (strongest n id))]
    (if (value/unusable? v)
      v
      (tms/distributed-base-value v))))

(defn- addressed-cell-id
  [n id]
  (let [candidate (strongest n id)
        address (when (scope-source/scope-value? candidate)
                  (scope-source/binding-address candidate))]
    (if (and (ids/node-id? address)
             (contains? (net/net-env n) address))
      address
      id)))

(defn- distributed-slot-keys
  [n id]
  (-> (net/network-cell-content n (addressed-cell-id n id))
      tms/distributed-slots
      keys
      set))

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
   (compile-source source env {}))
  ([source env opts]
   (if (ids/node-id? env)
     (main/compile-source source env opts)
     (cps-core/compile-expr-with-bindings
      (parse source) env opts))))

(defn- compile-expr
  ([expr]
   (main/compile-expr expr))
  ([expr bindings]
   (compile-expr expr bindings {}))
  ([expr bindings opts]
   (if (ids/node-id? bindings)
     (main/compile-expr expr bindings opts)
     (cps-core/compile-expr-with-bindings expr bindings opts))))

(defn- compiled-binding-id
  [compiled sym]
  (let [n (:net compiled)
        topology (net/network-dict-entry n env/lexical-topology-key)
        result-frame (some (fn [[frame-id frame]]
                             (when (some #{(:cell compiled)}
                                         (mapcat identity
                                                 (vals (:bindings frame))))
                               frame-id))
                           (:frames topology))
        topology-ids (distinct
                      (mapcat #(get-in % [:bindings sym])
                              (vals (:frames topology))))]
    (or (env/resolve-binding-id n (:env compiled) sym)
        (when result-frame
          (env/resolve-binding-id n result-frame sym))
        (when (= 1 (count topology-ids))
          (first topology-ids)))))

(defn- compiled-result-env
  [compiled]
  (some (fn [[frame-id frame]]
          (when (some #{(:cell compiled)}
                      (mapcat identity (vals (:bindings frame))))
            frame-id))
        (:frames (net/network-dict-entry (:net compiled)
                                         env/lexical-topology-key))))

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
  [expr _parent-env & watch-syms]
  (apply ast/app
         (ast/sym 'execute-sub-env)
         (ast/lit expr)
         (ast/sym 'parent-env)
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
    (let [plus (binding-value (h/default-bindings) '+)
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
    (let [switch (binding-value (h/default-bindings) 'switch)
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
  (let [compiler-env (h/default-bindings)]
    (doseq [op ['premise-input
                'premise-believe
                'tms-closure
                'premise-closure]]
      (testing op
        (let [operator (binding-value compiler-env op)]
          (is (operator-value/operator-closure? operator))
          (is (nil? (-> operator meta h/application-activate-key))))))))

(deftest compiler-2-behavior-env-uses-operator-closures
  (let [compiler-env (h/behavior-tms-bindings)]
    (doseq [op ['behavior-event
                'behavior-add-event
                'behavior-empty-state
                'behavior-retain-last
                'behavior
                'behavior-cell
                'latest]]
      (testing op
        (let [operator (binding-value compiler-env op)]
          (is (operator-value/operator-closure? operator))
          (is (nil? (-> operator meta h/application-activate-key))))))))

(deftest compiler-2-main-compiles-with-default-behavior-tms-env
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [out]
                     (premise-input :yes :from-main-entry 0 out)
                     out)")
        n (run-compiled compiled)]
    (is (= :yes (distributed-current-value n (:cell compiled))))
    (is (contains? (distributed-slot-keys n (:cell compiled))
                   (tms/premise-slot-key :from-main-entry 0)))))

(deftest compiler-2-known-tms-operator-declares-primitive-topology
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [out] (define premise :static/premise) (define epoch 0) (premise-retract premise epoch out) out)")
        applications (compiler-app/application-topologies (:net compiled))
        n (run-compiled compiled)]
    (is (= 1 (count applications)))
    (is (every? #(= :gur.flat/application (first %))
                (map :application-id applications)))
    (is (contains? (distributed-slot-keys n (:cell compiled))
                   (tms/premise-slot-key :static/premise 0)))))

;; Deferred compiler-2 behavior-history arithmetic integration.
;; See propagators.infra/doc/compiler-2-progress-and-priorities.md.
#_(deftest compiler-2-main-can-define-behavior-producing-network
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [a b out] (define make-point (network [t v out] (-> (behavior-point t v out) out) (list out))) (make-point 6 2 a) (make-point 6 7 b) (<-> (be:+ a b) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 9 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 9}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-main-can-build-behavior-with-compiler-closure-reducer
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events out] (define retain-event (network [acc update out] (-> (behavior-add-event acc update out) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior events retain-event (behavior-empty-state) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 3 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 2}
            {:at 8 :value 3}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-merge-can-use-low-level-operators
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events out] (define retain-event-low (network [acc update out] (-> (let-cell [known tick value next] (behavior-state-events acc known) (behavior-update-tick update tick) (behavior-update-value update value) (behavior-assoc-event known tick value next) (behavior-state-from-events next out)) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior events retain-event-low (behavior-empty-state) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 3 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 6 :value 2}
            {:at 8 :value 3}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-cell-can-use-slot-based-merge-closure
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out] (define retain-latest-slot (network [acc next out] (-> (let-cell [events* slot value next-events full] (behavior-state-events acc events*) (p:slot :slot slot next) (p:slot :value value next) (behavior-assoc-event events* slot value next-events) (behavior-state-from-events next-events full) (behavior-retain-last full 1 out)) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-cell events (behavior-empty-state) retain-latest-slot retained) (<-> (be:+ retained retained) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 6 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-closure-reducer-can-retain-latest-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out] (define retain-latest (network [acc update out] (-> (let-cell [full] (behavior-add-event acc update full) (behavior-retain-last full 1 out)) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (behavior events retain-latest (behavior-empty-state) retained) (<-> (be:+ retained retained) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-closure-reducer-can-retain-window-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out] (define retain-window (network [acc update out] (-> (let-cell [full] (behavior-add-event acc update full) (behavior-retain-last full 2 out)) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (behavior events retain-window (behavior-empty-state) retained) (<-> (be:+ retained retained) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}
            {:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-cell-can-retain-window-in-chain
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained out] (define retain-window (network [acc next out] (-> (let-cell [full] (behavior-add-event acc next full) (behavior-retain-last full 2 out)) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (behavior-cell events (behavior-empty-state) retain-window retained) (<-> (be:+ retained retained) out) out)"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 10 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 8 :value 6}
            {:at 10 :value 10}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

#_(deftest compiler-2-behavior-syntax-latest-and-last-are-behaviors
  (let [source "(let-cell [events retained out] (define retain-all (network [acc next out] (-> (behavior-add-event acc next out) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (be:behavior-cell events (behavior-empty-state) retain-all retained) %s out)"
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

#_(deftest compiler-2-behavior-prefixed-projections-are-behaviors
  (let [source "(let-cell [events retained out] (define retain-all (network [acc next out] (-> (behavior-add-event acc next out) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (behavior-cell events (behavior-empty-state) retain-all retained) %s out)"
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

#_(deftest compiler-2-behavior-prefixed-constructor-builds-behavior
  (let [compiled (main/compile-source-with-behavior-tms
                  "(let-cell [events retained] (define retain-all (network [acc next out] (-> (behavior-add-event acc next out) out) (list out))) (behavior-event 6 2 events) (behavior-event 10 5 events) (be:behavior events retain-all (behavior-empty-state) retained) (be:latest retained))"
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)]
    (is (= 5 (behavior-current-value n (:cell compiled))))
    (is (= [{:at 10 :value 5}]
           (behavior-records (net/network-cell-content n (:cell compiled)))))))

(deftest compiler-2-be-latest-zero-arg-builds-empty-latest-behavior
  (let [compiled (compile-source
                  "(be:latest)"
                  (selected-env (h/behavior-tms-bindings) ['be:latest])
                  {:net (behavior-tms-protocol-net)})
        n (run-compiled compiled)
        content (net/network-cell-content n (:cell compiled))]
    (is (= value/nothing (behavior-current-value n (:cell compiled))))
    (is (= [] (behavior-records content)))))

#_(deftest compiler-2-behavior-syntax-history-slices-return-behaviors
  (let [base-source "(let-cell [events retained out] (define retain-all (network [acc next out] (-> (behavior-add-event acc next out) out) (list out))) (behavior-event 6 2 events) (behavior-event 8 3 events) (behavior-event 10 5 events) (behavior-cell events (behavior-empty-state) retain-all retained) %s out)"
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
        sugar (compile-source "(let-cell [] (define pair (cons 1 2)) (+ (car pair) (cdr pair)))")
        sugar-net (run-compiled sugar)]
    (is (= 3 (strongest explicit-net (:cell explicit))))
    (is (= 3 (strongest sugar-net (:cell sugar))))))

(deftest compile-2-list-builds-cons-chain
  (let [compiled (compile-source "(let-cell [xs tail] (define xs (list 1 2 3)) (p:cdr tail xs) (+ (p:car xs) (p:car tail)))")
        n (run-compiled compiled)]
    (is (= 3 (strongest n (:cell compiled))))))

(deftest compile-2-cdr-gated-list-gur-hop-chain
  (testing "compiler-2 structural GUR should use cdr presence as the lazy hop guard"
    (let [compiled
          (compile-source "(let-cell [xs node1 tail out first rest second] (define inc-list (network [xs out] (-> (let-cell [head rest mapped-head mapped-rest] (p:car head xs) (p:cdr rest xs) (-> (+ head 1) mapped-head) (p:cons mapped-head mapped-rest out) (when rest (inc-list rest mapped-rest))) out) (list out))) (p:cons 2 tail node1) (p:cons 1 node1 xs) (inc-list xs out) (p:car first out) (p:cdr rest out) (p:car second rest) (+ (* first 10) second))"
                          (selected-default-env
                           'p:car 'p:cdr 'p:cons '+ '* '->))
          n (run-compiled compiled)]
      (is (= 23 (strongest n (:cell compiled)))))))

(defn- compile-2-map-chain-source
  [depth]
  (let [value-count 2
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
        '(define double-list (network [xs out] (-> (let-cell [head rest mapped-head mapped-rest] (p:car head xs) (p:cdr rest xs) (-> (* head 2) mapped-head) (p:cons mapped-head mapped-rest out) (when rest (double-list rest mapped-rest))) out) (list out)))
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

(deftest compile-2-cdr-gated-list-map-chain
  (testing "compiler-2 composes two cdr-gated map hops"
    (let [depth 2
          compiled (compile-source
                    (compile-2-map-chain-source depth)
                    (selected-default-env
                     'p:car 'p:cdr 'p:cons '+ '* '-> 'list))
          n (run-compiled compiled)
          expected (* 2 (long (Math/pow 2 depth)))]
      (is (= expected (strongest n (:cell compiled)))))))

(deftest compile-2-exposes-generic-slot
  (let [compiled (compile-source "(let-cell [obj]
                                    (p:slot :x 7 obj)
                                    (+ (p:slot :x obj) 1))")
        n (run-compiled compiled)]
    (is (= 8 (strongest n (:cell compiled))))))

(deftest compile-2-exposes-generic-slot-in-network-closure
  (let [compiled (compile-source "(let-cell [obj out] (define read-x (network [coll out] (-> (p:slot :x coll) out) (list out))) (p:slot :x 6 obj) (read-x obj out) (+ out 1))")
        n (run-compiled compiled)]
    (is (= 7 (strongest n (:cell compiled))))))

(deftest compile-2-exposes-generic-slot-write-in-network-closure
  (let [compiled (compile-source "(let-cell [obj] (define make-x (network [v out] (-> (p:slot :x v out) out) (list out))) (make-x 6 obj) (+ (p:slot :x obj) 1))")
        n (run-compiled compiled)]
    (is (= 7 (strongest n (:cell compiled))))))

(deftest compile-2-retains-primitive-application-topology
  (testing "primitive applications keep an inspectable named topology"
    (let [compiled (compile-source "(+ 1 2)")
          applications (compiler-app/application-topologies (:net compiled))
          [{:keys [application-id operator-id argument-ids result-id]}]
          applications]
      (is (= 1 (count applications)))
      (is (= :gur.flat/application (first application-id)))
      (is (ids/node-id? operator-id))
      (is (= 2 (count argument-ids)))
      (is (= (:cell compiled) result-id))
      (is (= 3 (strongest (run-compiled compiled) (:cell compiled)))))))

(deftest compile-2-retains-nested-primitive-application-topology
  (testing "nested primitive calls are retained as separate named propagators.infra"
    (let [compiled (compile-source "(+ 1 (- 4 2))")
          applications (compiler-app/application-topologies (:net compiled))
          result-net (run-compiled compiled)]
      (is (= 2 (count applications)))
      (is (= 2 (count (set (map :application-id applications)))))
      (is (= 3 (strongest result-net (:cell compiled)))))))

(deftest compile-2-dependency-env-emits-dependency-values
  (testing "default env remains raw while dependency env wraps arithmetic results"
    (let [raw-compiled (compile-source "(+ 1 2)")
          raw (run-compiled raw-compiled)
          compiled (compile-source "(+ 1 2)" (h/dependency-bindings) {})
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
          env (with-binding (h/default-bindings) 'a (env/cell-binding a-id) 0)
          compiled (compile-source "(+ a 5)" env {:net base-net})
          result (strongest (run-compiled compiled) (:cell compiled))]
      (is (dependency/dependency-value? result))
      (is (= 15 (dependency/base-value result)))
      (is (= #{:outer-source} (dependency/sources result))))))

#_(deftest compile-2-behavior-env-merges-same-timestamp-values
  (testing "compiled behavior arithmetic joins retained point histories"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (h/behavior-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 9 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:at 6 :value 9}]
             (behavior-records out-content))))))

#_(deftest compile-2-default-arithmetic-uses-behavior-current-values
  (testing "plain arithmetic is current-value arithmetic, not history join"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (h/behavior-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
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
          env (-> (selected-default-env '+ '- '->)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0)
                  (with-binding 'c (env/cell-binding c-id) 0)
                  (with-binding 'd (env/cell-binding d-id) 0))
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
          env (-> (selected-default-env '+ '- '->)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0)
                  (with-binding 'c (env/cell-binding c-id) 0)
                  (with-binding 'd (env/cell-binding d-id) 0))
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
          env (-> (selected-default-env '+ '- '->)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0)
                  (with-binding 'c (env/cell-binding c-id) 0)
                  (with-binding 'd (env/cell-binding d-id) 0))
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
        env (-> (selected-default-env '+ '->)
                (with-binding 'x (env/cell-binding x-id) 0)
                (with-binding 'out (env/cell-binding out-id) 0))
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
        env (-> (selected-default-env '+ '- '* '->)
                (with-binding 'a (env/cell-binding a-id) 0)
                (with-binding 'b (env/cell-binding b-id) 0)
                (with-binding 'c (env/cell-binding c-id) 0)
                (with-binding 'd (env/cell-binding d-id) 0)
                (with-binding 'out (env/cell-binding out-id) 0))
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
        env (-> (selected-default-env '+ 'switch)
                (with-binding 'x (env/cell-binding x-id) 0)
                (with-binding 'out (env/cell-binding out-id) 0))
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
        env (-> (selected-default-env '+ 'switch)
                (with-binding 'x (env/cell-binding x-id) 0)
                (with-binding 'enabled (env/cell-binding enabled-id) 0)
                (with-binding 'out (env/cell-binding out-id) 0))
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
          env (-> (selected-default-env '<->)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0)
                  (with-binding 'c (env/cell-binding c-id) 0))
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
          env (-> (selected-default-env '<->)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0)
                  (with-binding 'c (env/cell-binding c-id) 0))
          compiled (compile-source "(<-> a b c)"
                                   env
                                   {:net (-> n1
                                             (nb/install-cell a-id)
                                             (nb/install-cell b-id))})
          result-net (run-compiled compiled)]
      (is (= [12] (event-active-value-list result-net a-id))))))

#_(deftest compile-2-behavior-env-does-not-imply-point-continuation
  (testing "compiled behavior arithmetic does not join different point timestamps"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (h/behavior-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)]
      (is (= value/nothing
             (strongest result-net (:cell compiled)))))))

#_(deftest compile-2-behavior-env-synchronizes-interval-overlap
  (testing "compiled behavior arithmetic emits only the common interval"
    (let [left (behavior-view [(hist/interval-record 0 10 2)] #{[:a 0]})
          right (behavior-view [(hist/interval-record 5 12 7)] #{[:b 5]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (h/behavior-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          result-net (run-compiled compiled)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 9 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:from 5 :to 10 :value 9}]
             (behavior-records out-content))))))

#_(deftest compile-2-behavior-env-reacts-to-late-shared-timestamp
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
          env (-> (h/behavior-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(be:+ a b)" env {:net n2})
          n3 (run-compiled compiled)
          [_left-tasks n4] (seed-behavior-message n3 a-id left-6-8)
          [right-tasks n5] (seed-behavior-message n4 b-id right-6-8)
          result-net (nb/run-propagators n5 right-tasks)
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

(deftest compile-2-parser-supports-implicit-output-closures
  (doseq [source ["(network (x) (+ x 1))" "(network [x] (+ x 1))"]]
    (let [expression (parse source)]
      (is (net/network? expression))
      (is (= :network (ast/type expression)))
      (is (= '[x] (ast/inputs expression)))
      (is (nil? (ast/output expression)))
      (is (= :apply (ast/type (ast/body expression))))))
  (is (thrown? clojure.lang.ExceptionInfo (parse "(cell-expr [x] x)")))
  (is (thrown? clojure.lang.ExceptionInfo (parse "(:: [x] x)"))))

(deftest compile-2-parser-supports-functional-network-syntax
  (let [network (parse "(network [x out] (-> (+ x 1) out) (list out))")
        definition (parse "(define pair (network [x same next] (list same next)))")]
    (is (= :network (ast/type network)))
    (is (= '[x out] (ast/inputs network)))
    (is (nil? (ast/output network)))
    (is (= :def (ast/type definition)))
    (is (= 'pair (ast/name definition)))
    (is (= '[x same next] (ast/inputs (ast/body definition)))))
  (is (thrown? clojure.lang.ExceptionInfo (parse "(network [x] [out] x)"))))

(deftest compile-2-parser-supports-definition-receipts
  (let [defined (parse "(define answer (+ 1 2))")
        waiting (parse "(define signal)")
        literal-nil (parse "(define signal nil)")]
    (is (= :def (ast/type defined)))
    (is (= 'answer (ast/name defined)))
    (is (= :apply (ast/type (ast/body defined))))
    (is (nil? (ast/body waiting)))
    (is (= :literal (ast/type (ast/body literal-nil))))
    (is (nil? (ast/value (ast/body literal-nil)))))
  (doseq [source ["(def x 1)" "(def-cell x)" "(def-cells x y)"]]
    (is (thrown? clojure.lang.ExceptionInfo (parse source))))
  (is (thrown? clojure.lang.ExceptionInfo (parse "(define inc [x] (+ x 1))"))))

(deftest compile-2-parser-has-no-retired-special-forms
  (testing "retired behavior spellings and tombstones are ordinary applications"
    (doseq [source ["(def-behavior a)"
                    "(def-behaviour a)"
                    "(def-behaviors a b c)"
                    "(def-behaviours a b c)"
                    "(define-behaviors a b c)"
                    "(define-behaviours a b c)"
                    "(let-behavior [a b] (+ a b))"
                    "(let-behaviour [a b] (+ a b))"
                    "(app-> f x)"
                    "(let-network [x] x)"
                    "(let-compound [x] x)"]]
      (is (= :apply (ast/type (parse source))) source)))
  (testing "retired behavior spellings are not runtime declarations"
    (doseq [source ["(def-behavior a)"
                    "(def-behaviours a b)"
                    "(define-behaviors a b)"]]
      (is (false? (program-source/top-level-declaration? source)) source)))
  (testing "be:/ is no longer rewritten by the reader"
    (is (thrown? Exception (parser/read-form "(be:/ a b)")))
    (is (thrown? Exception (program-source/read-source-forms "(be:/ a b)")))
    (is (= 'be:divide
           (ast/name (ast/operator (parse "(be:divide a b)")))))))

(deftest compiler-2-primitive-behavior-env-has-no-arithmetic-bindings
  (let [compiler-env (h/behavior-bindings)]
    (doseq [op ['be:+ 'be:- 'be:* 'be:divide]]
      (is (nil? (binding-value compiler-env op)) op))))

(deftest compile-2-parser-supports-let-conditionals-and-def-constraint
  (testing "new immediate syntax parses onto compiler-2 AST"
    (let [let-ast (parse "(let [x 1 y (+ x 2)] y)")
          if-ast (parse "(if true 1 2)")
          when-ast (parse "(when ready (<-> 1 out))")
          cond-ast (parse "(cond [false 1 else 2])")
          constraint-ast (parse "(define same (network [a b] (<-> a b) (list a b)))")]
      (is (= :let (ast/type let-ast)))
      (is (= ['x 'y] (mapv first (ast/bindings let-ast))))
      (is (= :apply (ast/type if-ast)))
      (is (= 'if (ast/name (ast/operator if-ast))))
      (is (= :when-topology (ast/type when-ast)))
      (is (= 'ready (ast/name (ast/condition when-ast))))
      (is (= :apply (ast/type (ast/body when-ast))))
      (is (= :apply (ast/type cond-ast)))
      (is (= 'if (ast/name (ast/operator cond-ast))))
      (is (= :def (ast/type constraint-ast)))
      (is (= 'same (ast/name constraint-ast)))
      (is (= '[a b] (ast/inputs (ast/body constraint-ast))))))
  (testing "invalid def-constraint syntax reports parser errors"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"define expects"
                          (parse "(define 1 (network [a] a (list a)))")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"was removed"
                          (parse "(def-constraint c a a)")))))

(deftest compile-2-ast-accessors-accept-old-map-asts
  (testing "old AST maps normalize into slot-backed AST objects"
    (let [expr {:ast/type :apply
                :ast/operator {:ast/type :symbol :ast/name '+}
                :ast/args [{:ast/type :literal :ast/value 1}
                           {:ast/type :literal :ast/value 2}]}
          compiled (compile-expr expr)
          result-net (run-compiled compiled)]
      (is (= :apply (ast/type expr)))
      (is (= '+ (ast/name (ast/operator expr))))
      (is (= 3 (strongest result-net (:cell compiled)))))))

(deftest compile-2-network-closure-is-data-only
  (testing "closure declaration emits closure info, not a runtime Closure function"
    (let [compiled (compile-source "(network [x] (+ x 1))")
          callable (strongest (:net compiled) (:cell compiled))
          closure-info (compiler-app/callable-declaration callable)]
      (is (compiler-app/compiler-callable? callable))
      (is (closure-value/closure-info? closure-info))
      (is (not (closure/closure? callable)))
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
    (let [compiled (compile-source "(network [x] (-> 1 x) (+ x 1))")
          callable (strongest (:net compiled) (:cell compiled))
          closure-info (compiler-app/callable-declaration callable)
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
    (let [compiled (compile-source "(network [x] (+ x 1))")
          result-net (run-compiled compiled)
          callable (strongest result-net (:cell compiled))]
      (is (compiler-app/compiler-callable? callable))
      (is (empty? (:applications compiled)))
      (is (empty? (compiler-app/application-topologies result-net)))
      (is (empty? (main/compiled-applications result-net))))))

(deftest compile-2-application-installs-application-propagator
  (testing "network closure calls install named flat-GUR application topology"
    (let [compiled (compile-source "((network [x] (+ x 1)) 4)")
          applications (compiler-app/application-topologies (:net compiled))
          [{:keys [application-id]}] applications
          apply-prop-id (gur/stable-node-id [application-id :apply-prop])
          result-net (run-compiled compiled)]
      (is (= 1 (count applications)))
      (is (= [application-id] (:applications compiled)))
      (is (contains? (set (:props compiled)) apply-prop-id))
      (is (= 5 (strongest result-net (:cell compiled)))))))

(deftest compile-2-presence-when-delays-body-topology
  (testing "when compiles the condition immediately and installs body topology only after a usable value"
    (let [expr (ast/sequence*
                (parse "(define trigger)")
                (parse "(define out)")
                (parse "(when trigger (-> 1 out))")
                (parse "out"))
          compiled (compile-expr expr)
          trigger-id (compiled-binding-id compiled 'trigger)
          out-id (compiled-binding-id compiled 'out)
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
    (let [compiled (compile-source "(let-cell [out] (define f (network [x out] (-> (<= x 1) out) (list out))) (f 2 out) out)")
          result-net (run-compiled compiled)]
      (is (= false (strongest result-net (:cell compiled)))))))

(deftest compile-2-def-consumes-let-cell-reservation
  (let [compiled (compile-source "(let-cell [x] (define x 12) x)")
        topology (net/network-dict-entry (:net compiled)
                                         env/lexical-topology-key)
        [frame-id frame] (first (filter (fn [[_ frame]]
                                         (some #{(:cell compiled)}
                                               (get-in frame [:bindings 'x])))
                                       (:frames topology)))
        binding-ids (get-in frame [:bindings 'x])
        binding-id (first binding-ids)
        result-net (run-compiled compiled)]
    (is (= 1 (count binding-ids)))
    (is (= binding-id (:cell compiled)))
    (is (nil? (env/reserved-binding-id (:net compiled) frame-id 'x)))
    (is (= 12 (strongest result-net binding-id)))))

(deftest compile-2-named-closures-capture-copied-live-env
  (testing "a named closure's copied env contains its own binding"
    (let [compiled (compile-source "(define self (network [n out] (-> (when n (self n out)) out) (list out)))")
          self-id (compiled-binding-id compiled 'self)
          callable (strongest (run-compiled compiled) self-id)
          closure-env (get callable compiler-app/captured-environment-key)
          binding-ids (get-in (net/network-dict-entry (:net compiled)
                                                      env/lexical-topology-key)
                              [:frames closure-env :bindings 'self])]
      (is (= 1 (count binding-ids)))
      (is (nil? (env/reserved-binding-id (:net compiled) closure-env 'self)))
      (is (= self-id
             (env/resolve-binding-id (:net compiled) closure-env 'self)))))
  (testing "same-scope later declarations propagate into the copied env"
    (let [compiled (compile-source "(let-cell [result] (define first (network [x out] (-> (later x out) out) (list out))) (define later (network [x out] (-> (+ x 1) out) (list out))) first)")
          result-net (run-compiled compiled)
          first-id (compiled-binding-id compiled 'first)
          later-id (compiled-binding-id compiled 'later)
          callable (strongest result-net first-id)
          closure-env (get callable compiler-app/captured-environment-key)]
      (is (= later-id
             (env/resolve-binding-id result-net closure-env 'later))))))

(deftest compile-2-presence-when-supports-direct-recursive-style
  (testing "ordinary self-application inside presence-gated topology can terminate"
    (let [compiled (compile-source "(let-cell [out] (define down (network [n out] (-> (let-cell [base? recur? a] (-> (<= n 1) base?) (-> (not base?) recur?) (when (switch true base?) (-> n out)) (when (switch true recur?) (down (- n 1) a) (-> a out))) out) (list out))) (down 4 out) out)")
          result-net (run-compiled compiled)]
      (is (= 1 (strongest result-net (:cell compiled)))))))

(deftest compile-2-presence-when-supports-fib-style-gur
  (testing "fib uses only closure self-application plus switch-gated when bodies"
    (doseq [[n expected] [[0 0] [1 1] [5 5]]]
      (let [compiled (compile-source
                      (format "(let-cell [out] (define fib (network [n out] (-> (let-cell [base? recur? a b] (-> (<= n 1) base?) (-> (not base?) recur?) (when (switch true base?) (-> n out)) (when (switch true recur?) (fib (- n 1) a) (fib (- n 2) b) (-> (+ a b) out))) out) (list out))) (fib %d out) out)"
                              n)
                      (selected-default-env '<= 'not 'switch '- '+ '-> 'list))
            result-net (run-compiled compiled)]
        (is (= expected (strongest result-net (:cell compiled)))
            (str "fib " n))))))

(deftest compile-2-supports-first-slice-network-and-def-net
  (testing "network output cells are explicit application applicants"
    (let [anonymous (compile-source "(let-cell [out] ((network [x out] (-> (+ x 1) out) (list out)) 4 out) out)")
          named (compile-source "(let-cell [out] (define inc (network [x out] (-> (+ x 1) out) (list out))) (inc 5 out) out)")]
      (is (= 5 (strongest (run-compiled anonymous) (:cell anonymous))))
      (is (= 6 (strongest (run-compiled named) (:cell named)))))))

(deftest compile-2-supports-def-and-def-cell
  (testing "def creates named cells, def-cell declares free cells, and def-cell names cell-producing expressions"
    (let [named-value (compile-source "(define answer (+ 1 2))")
          named-value-net (run-compiled named-value)
          answer-id (compiled-binding-id named-value 'answer)
          free-def (compile-source "(define signal)")
          signal-id (compiled-binding-id free-def 'signal)
          free-def-cell (compile-source "(define signal)")
          free-def-cell-id (compiled-binding-id free-def-cell 'signal)
          free-def-cells (compile-expr (ast/sequence* (parse "(define a)") (parse "(define b)")))
          a-id (compiled-binding-id free-def-cells 'a)
          b-id (compiled-binding-id free-def-cells 'b)
          expr-cell (compile-source "(let-cell [out] (define inc (network [x] (+ x 1))) (<-> (inc 4) out) out)")
          named-cell (compile-source "(let-cell [out] (define inc (network [x] (+ x 1))) (<-> (inc 4) out) out)")
          expr-cell-net (run-compiled expr-cell)
          named-cell-net (run-compiled named-cell)]
      (is (= answer-id (:binding/target (strongest named-value-net (:cell named-value)))))
      (is (= 3 (strongest named-value-net answer-id)))
      (is (= signal-id (:binding/target (strongest (:net free-def) (:cell free-def)))))
      (is (= value/nothing (strongest (:net free-def) signal-id)))
      (is (= free-def-cell-id (:binding/target (strongest (:net free-def-cell) (:cell free-def-cell)))))
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
    (let [forward (compile-source "(let-cell [a b] (define same (network [x y] (<-> x y) (list x y))) (same a b) (-> 3 a) b)")
          reverse (compile-source "(let-cell [a b] (define same (network [x y] (<-> x y) (list x y))) (same a b) (-> 4 b) a)")
          reused (compile-source "(let-cell [a b c d] (define same (network [x y] (<-> x y) (list x y))) (same a b) (same c d) (-> 3 a) (-> 8 c) (+ b d))")
          lexical (compile-source "(let-cell [x y] (define bias 2) (define add-bias (network [a out] (<-> (+ a bias) out) (list a out))) (add-bias x y) (-> 5 x) y)")
          returned (compile-source "(let-cell [a b] (define same (network [x y] (<-> x y) (list x y))) (-> 9 a) (same a b))")
          empty (compile-source "(let-cell [] (define constant (network [] 12)) (constant))")]
      (is (= 3 (strongest (run-compiled forward) (:cell forward))))
      (is (= 4 (strongest (run-compiled reverse) (:cell reverse))))
      (is (= 11 (strongest (run-compiled reused) (:cell reused))))
      (is (= 7 (strongest (run-compiled lexical) (:cell lexical))))
      (is (net/network? (strongest (run-compiled returned) (:cell returned))))
      (is (= 12 (strongest (run-compiled empty) (:cell empty)))))))

(deftest compile-2-network-requires-explicit-output-applicant
  (testing "declared-output network calls do not synthesize hidden output cells"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Closure application has invalid arity"
         (run-compiled
          (compile-source "((network [x out] (-> (+ x 1) out) (list out)) 4)"))))))

(deftest compile-2-cell-expression-returns-body-result
  (testing "cell-expr is the zero-output closure form for expression results"
    (let [compiled (compile-source "((network [x] (+ x 1)) 4)")
          result-net (run-compiled compiled)]
      (is (= 5 (strongest result-net (:cell compiled)))))))

(deftest compile-2-network-and-def-net-support-multiple-explicit-outputs
  (testing "multi-output applications write to explicit output cells"
    (let [anonymous (compile-source
                     "(let-cell [same next] ((network [x same next] (<-> x same) (<-> (+ x 1) next) (list same next)) 4 same next) next)")
          named (compile-source
                 "(let-cell [same next] (define pair (network [x same next] (<-> x same) (<-> (+ x 1) next) (list same next))) (pair 5 same next) next)")
          anonymous-net (run-compiled anonymous)
          named-net (run-compiled named)]
      (is (= 5 (strongest anonymous-net (:cell anonymous))))
      (is (= 6 (strongest named-net (:cell named)))))))

(deftest compile-2-multi-output-survives-nested-closure-application
  (testing "an outer closure can route explicit output cells through an inner network"
    (let [compiled (compile-source
                    "(let-cell [same next] (define pair (network [x same next] (<-> x same) (<-> (+ x 1) next) (list same next))) (define outer (network [x same next] (pair x same next) (list same next))) (outer 8 same next) next)")
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled)))))))

(deftest compile-2-multi-output-late-bound-closure-application
  (testing "a declared-output application installed before the operator closure uses explicit output cells"
    (let [compiled (compile-source "(let-cell [some-net same next]
                                      (some-net 2 same next)
                                      next)")
          some-net-id (compiled-binding-id compiled 'some-net)
          same-id (compiled-binding-id compiled 'same)
          next-id (compiled-binding-id compiled 'next)
          n0 (run-compiled compiled)
          closure-compiled (compile-source
                            "(network [x same next] (<-> x same) (<-> (+ x 1) next) (list same next))"
                            (:env compiled)
                            {:net n0 :seed [:late-multi-output]})
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n1 (nb/seed-cell (:net closure-compiled)
                           some-net-id
                           closure-value)
          n2 (nb/run-propagators
              n1
              (into (:props closure-compiled)
                    (nb/neighbor-propagator-ids n1 some-net-id)))]
      (is (= value/nothing (strongest n0 next-id)))
      (is (= 2 (strongest n2 same-id)))
      (is (= 3 (strongest n2 next-id))))))

(deftest compile-2-application-output-adapter-is-not-materializing
  (testing "closure application projects result cells without a materialization helper"
    (let [source (slurp (io/resource "propagators/compiler/lowering/application.clj"))
          direct (compile-source "((network [x] (+ x 1)) 4)")
          late (compile-source "(let-cell [some-net out]
                                 (some-net 4 out)
                                 out)")
          some-net-id (compiled-binding-id late 'some-net)
          out-id (compiled-binding-id late 'out)
          n0 (run-compiled late)
          closure-compiled (compile-source "(network [x out] (<-> x out) (list out))"
                                           (:env late)
                                           {:net n0
                                            :seed [:late-output-adapter]})
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n1 (nb/seed-cell (:net closure-compiled)
                           some-net-id
                           closure-value)
          n2 (nb/run-propagators
              n1
              (into (:props closure-compiled)
                    (nb/neighbor-propagator-ids n1 some-net-id)))]
      (is (not (str/includes? source "materialize-slot-object")))
      (is (= 5 (strongest (run-compiled direct) (:cell direct))))
      (is (= 4 (strongest n2 out-id))))))

(deftest compile-2-bi-sync-chain-10
  (testing "compiler-2 handles a 10-hop <-> chain"
    (let [compiled (compile-source
                    (bi-sync-chain-source 10)
                    (selected-default-env '<->))
          result-net (run-compiled compiled)]
      (is (= 1 (strongest result-net (:cell compiled)))))))

;; Live compound-environment coverage lives in compiler_2_live_environment_test.clj.

(deftest compile-2-lexical-compound-uses-env-slot-not-hidden-captures
  (testing "compound declarations retain the live env id as closure data"
    (let [[bias-id base-net] (seeded-cell net/empty-net 10)
          env (with-binding (h/default-bindings) 'bias (env/cell-binding bias-id) 0)
          compiled (compile-source
                    "(let-cell [add-bias] (<-> add-bias (network [x] (+ x bias))) (add-bias 5))"
                    env
                    {:net base-net})
          apply-inputs (propagator-inputs-writing-to (:net compiled)
                                                     (:cell compiled))
          result-net (run-compiled compiled)
          closure (strongest result-net (compiled-binding-id compiled 'add-bias))]
      (is (ids/node-id?
           (get closure compiler-app/captured-environment-key)))
      (is (not-any? #(contains? % bias-id) apply-inputs))
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-lexical-argument-shadows-parent-binding
  (testing "input bindings use a nearer scope source than inherited env bindings"
    (let [[outer-x-id base-net] (seeded-cell net/empty-net 100)
          env (with-binding (h/default-bindings) 'x (env/cell-binding outer-x-id) 0)
          compiled (compile-source
                    "(let-cell [inc-local] (<-> inc-local (network [x] (+ x 1))) (inc-local 5))"
                    env
                    {:net base-net})
          result-net (run-compiled compiled)]
      (is (= 6 (strongest result-net (:cell compiled)))))))

(deftest compile-2-inner-local-does-not-write-parent-except-output
  (testing "a local cell that shadows a parent symbol stays local unless routed to the compound output"
    (let [[outer-x-id base-net] (seeded-cell net/empty-net 100)
          env (with-binding (h/default-bindings) 'x (env/cell-binding outer-x-id) 0)
          compiled (compile-source
                    "(let-cell [use-local-x] (<-> use-local-x (network [] (let-cell [x] (<-> 7 x) x))) (use-local-x))"
                    env
                    {:net base-net})
          result-net (run-compiled compiled)]
      (is (= 7 (strongest result-net (:cell compiled))))
      (is (= 100 (strongest result-net outer-x-id))))))

(deftest compile-2-escaped-closure-preserves-lexical-env-through-output
  (testing "a returned closure carries its lexical environment through the declared output"
    (let [compiled (compile-source
                    "(let-cell [make-adder] (<-> make-adder (network [bias] (network [x] (+ x bias)))) ((make-adder 10) 5))")
          result-net (run-compiled compiled)]
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-dependency-env-uses-active-closure-application-context
  (testing "nested closure arithmetic records the later inner application context"
    (let [compiled (compile-source
                    "(let-cell [make-adder] (<-> make-adder (network [bias] (network [x] (+ x bias)))) ((make-adder 10) 5))"
                    (h/dependency-bindings)
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
          env (with-binding (h/dependency-bindings) 'a (env/cell-binding a-id) 0)
          compiled (compile-source
                    "(let-cell [add-a] (<-> add-a (network [x] (+ a x))) (add-a 5))"
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
  (testing "an outer compound can define and apply nested compound propagators.infra"
    (let [compiled (compile-source
                    "(let-cell [outer] (<-> outer (network [x] (let-cell [inc scale-after-inc] (<-> inc (network [y] (+ y 1))) (<-> scale-after-inc (network [y] (let-cell [double] (<-> double (network [v] (* v 2))) (double (inc y))))) (+ (inc x) (scale-after-inc x))))) (outer 4))")
          result-net (run-compiled compiled)]
      (is (= 15 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-multiple-compound-declarations-inside-one-compound
  (testing "one compound can declare several local compound propagators.infra and apply them over its arguments"
    (let [compiled (compile-source
                    "(let-cell [pipeline] (<-> pipeline (network [a b] (let-cell [add2 mul2 inc] (<-> add2 (network [x y] (+ x y))) (<-> mul2 (network [x y] (* x y))) (<-> inc (network [x] (+ x 1))) (+ (add2 a b) (mul2 (inc a) b))))) (pipeline 3 4))")
          result-net (run-compiled compiled)]
      (is (= 23 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-bi-sync-operator
  (testing "<-> returns its declared output cell"
    (let [[a-id n1] (seeded-cell net/empty-net 42)
          b-id (ids/new-node-id)
          n2 (nb/install-cell n1 b-id)
          env (-> (h/default-bindings)
                  (with-binding 'a (env/cell-binding a-id) 0)
                  (with-binding 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(<-> a b)" env {:net n2})
          applications (compiler-app/application-topologies (:net compiled))
          result-net (run-compiled compiled)]
      (is (= 1 (count applications)))
      (is (= 42 (strongest result-net b-id)))
      (is (= 42 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-forward-sync-operator
  (testing "-> installs one-way sync and returns the output cell"
    (let [compiled (compile-source "(let-cell [out]
                                      (-> 42 out)
                                      out)")
          result-net (run-compiled compiled)]
      (is (= 42 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-forward-sync-chain
  (testing "-> installs a one-way chain and returns the last cell"
    (let [compiled (compile-source "(let-cell [a b c]
                                      (-> 42 a b c)
                                      c)")
          result-net (run-compiled compiled)]
      (is (= 42 (strongest result-net (:cell compiled)))))))

(deftest compile-2-supports-bi-sync-chain
  (testing "<-> installs adjacent bidirectional links and returns the last cell"
    (let [compiled (compile-source "(let-cell [a b c]
                                      (<-> a b c)
                                      (<-> c 42)
                                      a)")
          result-net (run-compiled compiled)]
      (is (= 42 (strongest result-net (:cell compiled)))))))

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
  (let [compiled (compile-source "(let-cell [a gated out] (define value 2) (define premise :switch/source) (define epoch 0) (premise-input value premise epoch a) (switch a true gated) (-> gated out) out)"
                                 (selected-default-env
                                  'premise-input
                                  'switch
                                  '->)
                                 {:net (tms-distributed-protocol-net)})
        result-net (run-compiled compiled)]
    (is (= 2 (distributed-current-value result-net (:cell compiled))))
    (is (contains? (distributed-slot-keys result-net (:cell compiled))
                   (tms/premise-slot-key :switch/source 0)))))

#_(deftest compiler-2-behavior-tms-env-supports-switch-and-forward-sync
  (testing "explicit-output switch gates behavior content"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-cell [events retained gated out] (define retain-latest (network [acc next out] (-> (let-cell [full] (behavior-add-event acc next full) (behavior-retain-last full 1 out)) out) (list out))) (behavior-event 6 2 events) (behavior-cell events (behavior-empty-state) retain-latest retained) (switch retained true gated) (-> (be:+ gated gated) out) out)"
                    {:net (behavior-tms-protocol-net)})
          result-net (run-compiled compiled)]
      (is (= 4 (behavior-current-value result-net (:cell compiled))))))
  (testing "expression-style switch gates behavior content"
    (let [compiled (main/compile-source-with-behavior-tms
                    "(let-cell [events retained out] (define retain-latest (network [acc next out] (-> (let-cell [full] (behavior-add-event acc next full) (behavior-retain-last full 1 out)) out) (list out))) (behavior-event 6 2 events) (behavior-cell events (behavior-empty-state) retain-latest retained) (define gated (switch retained true)) (-> (be:+ gated gated) out) out)"
                    {:net (behavior-tms-protocol-net)})
          result-net (run-compiled compiled)]
      (is (= 4 (behavior-current-value result-net (:cell compiled)))))))

(deftest compile-2-propagator-emits-runnable-network-value
  (testing "source AST/env cells can produce a compiled network cell"
    (let [[x-id n1] (seeded-cell net/empty-net 4)
          expr-id (ids/new-node-id)
          compiled-id (ids/new-node-id)
          expr (parse "(+ x 1)")
          bindings (with-binding (h/default-bindings)
                                 'x (env/cell-binding x-id) 0)
          declared (live-env n1 bindings)
          env-id (:env declared)
          n2 (-> (:net declared)
                 (nb/install-cell expr-id)
                 (nb/install-cell compiled-id)
                 (nb/seed-cell expr-id expr))
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
          a-id (compiled-binding-id compiled 'a)
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
          a-id (compiled-binding-id compiled 'a)
          out-id (compiled-binding-id compiled 'out)
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
          some-net-id (compiled-binding-id compiled 'some-net)
          out-id (compiled-binding-id compiled 'out)
          n0 (run-compiled compiled)
          closure-compiled
          (compile-source
           "(network [x out] (<-> (+ x 1) out) (list out))"
           (h/default-bindings)
           {:net n0})
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n1 (nb/seed-cell (:net closure-compiled) some-net-id closure-value)
          n2 (nb/run-propagators n1
                                 (into (:props closure-compiled)
                                       (nb/neighbor-propagator-ids
                                        n1 some-net-id)))]
      (is (= value/nothing (strongest n0 out-id)))
      (is (= 3 (strongest n2 out-id))))))

(deftest compile-2-definition-refines-unresolved-operator-cell
  (testing "a later definition refines the waiting operator and activates the retained application"
    (let [a-def (compile-source "(define a)")
          early (compile-source "(inc 1 a)"
                                (:env a-def)
                                {:net (:net a-def)})
          inc-id (compiled-binding-id early 'inc)
          a-id (compiled-binding-id early 'a)
          n0 (nb/run-propagators (:net early) (:props early))
          late (compile-source "(define inc (network [x out] (<-> (+ x 1) out) (list out)))"
                               (:env early)
                               {:net n0})
          n1 (nb/run-propagators (:net late) (:props late))]
      (is (= inc-id (compiled-binding-id late 'inc)))
      (is (= value/nothing (strongest n0 a-id)))
      (is (= 2 (strongest n1 a-id))))))

(deftest compile-2-application-before-closure-waits-for-later-input-fire
  (testing "an application can exist before the operator closure and evaluate on a later input update"
    (let [compiled (compile-source "(let-cell [some-net out]
                                      (some-net a out)
                                      out)")
          some-net-id (compiled-binding-id compiled 'some-net)
          a-id (compiled-binding-id compiled 'a)
          out-id (compiled-binding-id compiled 'out)
          n0 (run-compiled compiled)
          closure-compiled (compile-source "(network [x out] (<-> (+ x 1) out) (list out))"
                                           (:env compiled)
                                           {:net n0
                                            :seed [:late-input-fire]})
          closure-info (strongest (:net closure-compiled)
                                  (:cell closure-compiled))
          n1 (nb/seed-cell (:net closure-compiled)
                           some-net-id
                           closure-info)
          n2 (nb/run-propagators
              n1
              (into (:props closure-compiled)
                    (nb/neighbor-propagator-ids n1 some-net-id)))
          n3 (nb/seed-cell n2 a-id 8)
          n4 (nb/run-propagators n3
                                 (nb/neighbor-propagator-ids n3 a-id))]
      (is (= value/nothing (strongest n0 out-id)))
      (is (= value/nothing (strongest n2 out-id)))
      (is (= 9 (strongest n4 out-id))))))

(deftest execute-sub-env-builds-compound-child-env-and-reads-parent
  (let [x-id (ids/new-node-id)
        initial (nb/install-cell net/empty-net x-id 41 41)
        parent (live-env initial
                         (with-binding (h/default-bindings)
                                       'x (env/cell-binding x-id) 0))
        expr (execute-sub-env-ast (parse "x") (:env parent))
        outer (with-binding (h/default-bindings)
                            'parent-env (env/cell-binding (:env parent)))
        compiled (compile-expr expr outer {:net (:net parent)})
        result-net (run-compiled compiled)]
    (is (= 41 (strongest result-net (:cell compiled))))))

(deftest execute-sub-env-default-env-supports-switch-and-forward-sync
  (let [x-id (ids/new-node-id)
        initial (nb/install-cell net/empty-net x-id 41 41)
        parent (live-env initial
                         (with-binding (h/default-bindings)
                                       'x (env/cell-binding x-id) 0))
        outer-env (-> (h/default-bindings)
                      (with-binding 'x (env/cell-binding x-id) 0)
                      (with-binding 'parent-env
                                    (env/cell-binding (:env parent))))
        expr (execute-sub-env-ast
              (parse "(let-cell [gated out]
                        (switch x true gated)
                        (-> gated out)
                        out)")
              (:env parent)
              'x)
        compiled (compile-expr expr
                                    outer-env
                                    {:net (:net parent)})
        result-net (run-compiled compiled)]
    (is (= 41 (strongest result-net (:cell compiled))))))

#_(deftest execute-sub-env-behavior-tms-env-supports-switch-and-forward-sync
  (let [parent-env (h/behavior-tms-bindings)
        expr (execute-sub-env-ast
              (parse "(let-cell [events retained gated out] (define retain-latest (network [acc next out] (-> (let-cell [full] (behavior-add-event acc next full) (behavior-retain-last full 1 out)) out) (list out))) (behavior-event 6 2 events) (behavior-cell events (behavior-empty-state) retain-latest retained) (switch retained true gated) (-> (be:+ gated gated) out) out)")
              parent-env)
        compiled (main/compile-expr-with-behavior-tms
                  expr
                  {:net (behavior-tms-protocol-net)})
        result-net (run-compiled compiled)]
    (is (= 4 (behavior-current-value result-net (:cell compiled))))))

(deftest execute-sub-env-uses-parent-env-reducer-storage
  (let [value-id (ids/new-node-id)
        [storage-id n1] (reducer-storage-cell net/empty-net)
        parent-env (-> (h/default-bindings)
                       (with-binding 'emit (reducer-emit-operator) 0)
                       (with-binding 'v (env/cell-binding value-id) 0)
                       (with-binding 'store (env/cell-binding storage-id) 0))
        parent (live-env (nb/install-cell n1 value-id 10 10) parent-env)
        outer-env (-> (h/default-bindings)
                      (with-binding 'v (env/cell-binding value-id) 0)
                      (with-binding 'parent-env
                                    (env/cell-binding (:env parent))))
        expr (execute-sub-env-ast (parse "(emit v store)") (:env parent) 'v)
        compiled (compile-expr expr outer-env
                               {:net (:net parent)})
        result-net (run-compiled compiled)
        out (strongest result-net (:cell compiled))
        stored (strongest result-net storage-id)]
    (is (reducer/reduced-value? out))
    (is (= 10 (reducer/reduced-result out)))
    (is (= 10 (reducer/reduced-result stored)))))

(deftest execute-sub-env-reacts-to-later-reducer-slot-update
  (let [[input-id n1] (reducer-storage-cell net/empty-net)
        [storage-id n2] (reducer-storage-cell n1)
        parent-env (-> (h/default-bindings)
                       (with-binding 'emit (reducer-emit-operator) 0)
                       (with-binding 'v (env/cell-binding input-id) 0)
                       (with-binding 'store (env/cell-binding storage-id) 0))
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
        parent (live-env n3 parent-env)
        outer-env (-> (h/default-bindings)
                      (with-binding 'v (env/cell-binding input-id) 0)
                      (with-binding 'parent-env
                                    (env/cell-binding (:env parent))))
        expr (execute-sub-env-ast (parse "(emit v store)") (:env parent) 'v)
        compiled (compile-expr expr outer-env {:net (:net parent)})
        n6 (run-compiled compiled)
        [tasks n7] (core/eval-cell input-id (message input-id later-input) n6)
        n8 (nb/run-propagators n7 tasks)]
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
        parent-env (-> (h/default-bindings)
                       (with-binding 'claim (tms-claim-operator :c1 :answer
                                                            [(tms/support premise-value
                                                                          :child
                                                                          :derived)])
                                 0)
                       (with-binding 'premise-source
                                 (tms-premise-source-operator 0)
                                 0)
                       (with-binding 'premise-source-later
                                 (tms-premise-source-operator 1)
                                 0)
                       (with-binding 'premise-id (env/cell-binding premise-id) 0)
                       (with-binding 'value (env/cell-binding value-id) 0)
                       (with-binding 'active (env/cell-binding active-id) 0)
                       (with-binding 'inactive (env/cell-binding inactive-id) 0)
                       (with-binding 'tms (env/cell-binding tms-id) 0))
        outer-bindings (-> (h/default-bindings)
                           (with-binding 'premise-id
                                         (env/cell-binding premise-id) 0)
                           (with-binding 'value (env/cell-binding value-id) 0)
                           (with-binding 'active (env/cell-binding active-id) 0)
                           (with-binding 'inactive
                                         (env/cell-binding inactive-id) 0))
        initial (-> net/empty-net
                    (nb/install-cell premise-id premise-value premise-value)
                    (nb/install-cell value-id :yes :yes)
                    (nb/install-cell active-id true true)
                    (nb/install-cell inactive-id)
                    (nb/install-cell tms-id
                                     tms-cell
                                     (reducer/strongest tms-cell)))
        parent (live-env initial parent-env)
        outer-env (with-binding outer-bindings
                                'parent-env
                                (env/cell-binding (:env parent)))
        expr (parse "(let-cell []
                       (premise-source premise-id active tms)
                       (premise-source-later premise-id inactive tms)
                       (claim value tms))")
        outer-expr (execute-sub-env-ast expr
                                        (:env parent)
                                        'premise-id
                                        'value
                                        'active
                                        'inactive)
        compiled (compile-expr
                  outer-expr
                  outer-env
                  {:net (:net parent)})
        result-net (run-compiled compiled)
        view (reducer/reduced-result (strongest result-net tms-id))
        [inactive-tasks n1] (core/eval-cell inactive-id
                                            (message inactive-id false)
                                            result-net)
        inactive-net (nb/run-propagators n1 inactive-tasks)
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
        env (-> (h/default-bindings)
                (with-binding 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (with-binding 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (with-binding 'claim (tms-claim-operator :c1 :answer
                                                     [(tms/support premise-value
                                                                   :compiler-2
                                                                   :derived)])
                          0)
                (with-binding 'premise-id (env/cell-binding premise-id) 0)
                (with-binding 'believe-epoch (env/cell-binding believe-epoch-id) 0)
                (with-binding 'retract-epoch (env/cell-binding retract-epoch-id) 0)
                (with-binding 'value (env/cell-binding value-id) 0)
                (with-binding 'tms (env/cell-binding tms-id) 0))
        expr (parse "(let-cell []
                       (believe-premise premise-id believe-epoch tms)
                       (retract-premise premise-id retract-epoch tms)
                       (claim value tms))")
        compiled (compile-expr
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
        retracted-net (nb/run-propagators n1 retract-tasks)
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
  (let [env (with-binding (h/default-bindings)
                      'tms-insert
                      (tms-insert-fact-operator)
                      0)
        compiled (compile-source "(let-cell [] (define value :yes) (define premise :from-pair) (define tms) (define pair (cons value premise)) (tms-insert tms (car pair) (cdr pair)) tms)"
                                 env)
        n0 (run-compiled compiled)
        result-id (addressed-cell-id n0 (:cell compiled))
        view (reducer/reduced-result (strongest n0 result-id))]
    (is (= #{:from-pair} (tms/active-premises view)))
    (is (= :yes (tms/proposition-value view :answer)))
    (is (= #{(tms/claim-slot-key [:insert :from-pair])
             (tms/premise-slot-key :from-pair 0)
             (tms/latest-premise-slot-key :from-pair)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n0 result-id))))))))

(deftest compiler-2-premise-output-conflicts-preserve-evidence
  (let [base-env (-> (selected-default-env)
                     (with-binding 'premise-out
                               (tms-insert-fact-operator)
                               0)
                     (with-binding 'believe-premise
                               (tms-premise-epoch-operator true)
                               0)
                     (with-binding 'retract-premise
                               (tms-premise-epoch-operator false)
                               0))
        setup (compile-source
                    "(let-cell [] (define p-one :definition/plus-one) (define p-ten :definition/plus-ten) (define one-believe 0) (define ten-believe 0) (define tms) (premise-out tms 6 p-one) (premise-out tms 15 p-ten) (believe-premise p-one one-believe tms) (believe-premise p-ten ten-believe tms) tms)"
                    base-env
                    {:net net/empty-net})
        n0 (run-compiled setup)
        env0 (compiled-result-env setup)
        tms-id (env/resolve-binding-id n0 env0 'tms)
        view0 (reducer/reduced-result (strongest n0 tms-id))]
    (is (= value/contradiction (tms/proposition-value view0 :answer)))
    (is (= #{(tms/claim-slot-key [:insert :definition/plus-one])
             (tms/claim-slot-key [:insert :definition/plus-ten])
             (tms/premise-slot-key :definition/plus-one 0)
             (tms/premise-slot-key :definition/plus-ten 0)
             (tms/latest-premise-slot-key :definition/plus-one)
             (tms/latest-premise-slot-key :definition/plus-ten)}
           (set (keys (reducer/reducer-slots
                       (net/network-cell-content n0 tms-id))))))))

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
        env (-> (h/default-bindings)
                (with-binding 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (with-binding 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (with-binding 'claim (tms-claim-operator :c1 :answer
                                                     [(tms/support p1
                                                                   :compiler-2
                                                                   :source-a)
                                                      (tms/support p2
                                                                   :compiler-2
                                                                   :source-b)])
                          0)
                (with-binding 'p1 (env/cell-binding p1-id) 0)
                (with-binding 'p2 (env/cell-binding p2-id) 0)
                (with-binding 'p1-believe (env/cell-binding p1-believe-id) 0)
                (with-binding 'p1-retract (env/cell-binding p1-retract-id) 0)
                (with-binding 'p1-bring (env/cell-binding p1-bring-id) 0)
                (with-binding 'p2-believe (env/cell-binding p2-believe-id) 0)
                (with-binding 'p2-retract (env/cell-binding p2-retract-id) 0)
                (with-binding 'value (env/cell-binding value-id) 0)
                (with-binding 'tms (env/cell-binding tms-id) 0))
        expr (parse "(let-cell []
                       (believe-premise p1 p1-believe tms)
                       (retract-premise p1 p1-retract tms)
                       (believe-premise p1 p1-bring tms)
                       (believe-premise p2 p2-believe tms)
                       (retract-premise p2 p2-retract tms)
                       (claim value tms))")
        compiled (compile-expr
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
        n2 (nb/run-propagators n1 p1-retract-tasks)
        view1 (reducer/reduced-result (strongest n2 tms-id))
        [p1-bring-tasks n3] (core/eval-cell p1-bring-id
                                            (message p1-bring-id 2)
                                            n2)
        n4 (nb/run-propagators n3 p1-bring-tasks)
        view2 (reducer/reduced-result (strongest n4 tms-id))
        [p2-retract-tasks n5] (core/eval-cell p2-retract-id
                                               (message p2-retract-id 3)
                                               n4)
        n6 (nb/run-propagators n5 p2-retract-tasks)
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
        env (-> (h/default-bindings)
                (with-binding 'believe-premise
                          (tms-premise-epoch-operator true)
                          0)
                (with-binding 'retract-premise
                          (tms-premise-epoch-operator false)
                          0)
                (with-binding 'claim (tms-claim-operator :chain :computed
                                                     [(tms/support pa :chain :a)
                                                      (tms/support pb :chain :b)
                                                      (tms/support pc :chain :c)
                                                      (tms/support pd :chain :d)])
                          0)
                (with-binding 'a (env/cell-binding a-id) 0)
                (with-binding 'b (env/cell-binding b-id) 0)
                (with-binding 'c (env/cell-binding c-id) 0)
                (with-binding 'd (env/cell-binding d-id) 0)
                (with-binding 'f (env/cell-binding f-id) 0)
                (with-binding 'pa (env/cell-binding pa-id) 0)
                (with-binding 'pb (env/cell-binding pb-id) 0)
                (with-binding 'pc (env/cell-binding pc-id) 0)
                (with-binding 'pd (env/cell-binding pd-id) 0)
                (with-binding 'pa-believe (env/cell-binding pa-believe-id) 0)
                (with-binding 'pb-believe (env/cell-binding pb-believe-id) 0)
                (with-binding 'pc-believe (env/cell-binding pc-believe-id) 0)
                (with-binding 'pd-believe (env/cell-binding pd-believe-id) 0)
                (with-binding 'pa-retract (env/cell-binding pa-retract-id) 0)
                (with-binding 'pa-bring (env/cell-binding pa-bring-id) 0)
                (with-binding 'tms (env/cell-binding tms-id) 0))
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
        compiled (compile-expr
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
        n2 (nb/run-propagators n1 retract-tasks)
        view1 (reducer/reduced-result (strongest n2 tms-id))
        [bring-tasks n3] (core/eval-cell pa-bring-id
                                         (message pa-bring-id 2)
                                         n2)
        n4 (nb/run-propagators n3 bring-tasks)
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

(deftest compiler-2-tms-conflicting-claims-retract-and-switch
  (let [base-env (-> (selected-default-env)
                     (with-binding 'believe-premise
                               (tms-premise-epoch-operator true)
                               0)
                     (with-binding 'retract-premise
                               (tms-premise-epoch-operator false)
                               0)
                     (with-binding 'claim-left
                               (tms-claim-operator :left
                                                   :shared
                                                   [(tms/support :premise/left
                                                                 :claim
                                                                 :left)])
                               0)
                     (with-binding 'claim-right
                               (tms-claim-operator :right
                                                   :shared
                                                   [(tms/support :premise/right
                                                                 :claim
                                                                 :right)])
                               0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [] (define p-left :premise/left) (define p-right :premise/right) (define tms) (believe-premise p-left 0 tms) (believe-premise p-right 0 tms) (claim-left 28 tms) (claim-right 36 tms) tms)"
                    base-env
                    net/empty-net)
        env0 (compiled-result-env setup)
        tms-id (env/resolve-binding-id n0 env0 'tms)
        view0 (reducer/reduced-result (strongest n0 tms-id))
        [_retracted n1] (compile-step
                         "(let-cell []
                            (retract-premise p-left 1 tms)
                            tms)"
                         env0
                         n0)
        view1 (reducer/reduced-result (strongest n1 tms-id))]
    (is (= value/contradiction (tms/proposition-value view0 :shared)))
    (is (= 36 (tms/proposition-value view1 :shared)))
    (is (contains? (set (keys (reducer/reducer-slots
                               (net/network-cell-content n1 tms-id))))
                   (tms/premise-slot-key :premise/left 1)))))

(deftest compiler-2-distributed-tms-premises-flow-through-chain
  (let [compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [a f] (define pa :premise/a) (define pa0 0) (premise-input 8 pa pa0 a) (-> a f) f)"
                    (selected-default-env
                     'premise-input 'premise-retract '->)
                    (tms-distributed-protocol-net))
        env0 (compiled-result-env setup)
        id-of (fn [sym] (env/resolve-binding-id n0 env0 sym))
        a-id (id-of 'a)
        f-id (id-of 'f)
        [_a-retracted n1] (compile-step
                          "(let-cell []
                             (premise-retract pa 1 a)
                             f)"
                          env0
                          n0)]
    (is (= 8 (distributed-current-value n0 f-id)))
    (is (contains? (distributed-slot-keys n0 f-id)
                   (tms/premise-slot-key :premise/a 0)))
    (is (value/nothing? (strongest n1 f-id)))
    (is (contains? (distributed-slot-keys n1 f-id)
                   (tms/premise-slot-key :premise/a 1)))
    (is (ids/node-id? a-id))))

(deftest compiler-2-distributed-tms-wraps-network-declaration-closure
  (let [setup
        (compile-source
         "(let-cell [a out] (define value 8) (define premise :premise/a) (define epoch 0) (premise-input value premise epoch a) (define identity (network [a] a)) (define tms-identity (tms-closure identity)) (-> (tms-identity a) out) out)"
         (h/default-bindings)
         (tms-distributed-protocol-net))
        n0 (run-compiled setup)
        env0 (compiled-result-env setup)
        out-id (env/resolve-binding-id n0 env0 'out)]
    (is (= 8 (distributed-current-value n0 out-id)))
    (is (contains? (distributed-slot-keys n0 out-id)
                   (tms/premise-slot-key :premise/a 0)))))

(deftest compiler-2-shadowed-premise-closures-preserve-both-claims
  (let [compiled (compile-source
                  "(let-cell [out] (define identity (network (x) x))
                     (define op (premise-closure (network (f x) (f x)) :definition/plus-one 0))
                     (-> (op identity 6) out)
                     (let [op (premise-closure (network (f x) (f x)) :definition/plus-ten 0)]
                       (-> (op identity 15) out)) out)"
                  (h/default-bindings) (tms-distributed-protocol-net))
        network (run-compiled compiled)
        out-id (:cell compiled)]
    (is (value/contradiction? (distributed-current-value network out-id)))
    (is (contains? (distributed-slot-keys network out-id) (tms/premise-slot-key :definition/plus-one 0)))
    (is (contains? (distributed-slot-keys network out-id) (tms/premise-slot-key :definition/plus-ten 0)))))

(deftest compiler-2-distributed-premise-closure-marks-network-output
  (let [compiled (compile-source
                  "(let-cell [x out] (premise-input 5 :premise/input 0 x)
                     (define inc (network (x) (+ x 1)))
                     (define apply-inc (premise-closure (network (f x) (f x)) :premise/definition 0))
                     (-> (apply-inc inc x) out) out)"
                  (h/default-bindings) (tms-distributed-protocol-net))
        network (run-compiled compiled)
        out-id (:cell compiled)]
    (is (= 6 (distributed-current-value network out-id)))
    (is (contains? (distributed-slot-keys network out-id) (tms/premise-slot-key :premise/input 0)))
    (is (contains? (distributed-slot-keys network out-id) (tms/premise-slot-key :premise/definition 0)))))

#_(deftest compiler-2-distributed-tms-composes-with-behavior-arithmetic
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:left 6]})
        right (behavior-view [(hist/point-record 6 7)] #{[:right 6]})
        [left-id n1] (behavior-cell (behavior-tms-protocol-net) left)
        [right-id n2] (behavior-cell n1 right)
        env (-> (h/behavior-tms-bindings)
                (with-binding 'left-source (env/cell-binding left-id) 0)
                (with-binding 'right-source (env/cell-binding right-id) 0))
        compile-step (fn [source env network]
                       (let [compiled (compile-source source env {:net network})]
                         [compiled (run-compiled compiled)]))
        [setup n0] (compile-step
                    "(let-cell [a b out] (define p-left :premise/left) (define p-right :premise/right) (define p-left0 0) (define p-right0 0) (premise-content-input left-source p-left p-left0 a) (premise-content-input right-source p-right p-right0 b) (<-> (be:+ a b) out) out)"
                    env
                    n2)
        env0 (compiled-result-env setup)
        out-id (env/binding-id (binding-value env0 'out))
        [left-retracted n3] (compile-step
                             "(let-cell [] (define p-left1 1) (premise-retract p-left p-left1 a) out)"
                             env0
                             n0)
        [left-brought n4] (compile-step
                           "(let-cell [] (define p-left2 2) (premise-believe p-left p-left2 a) out)"
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

#_(deftest execute-sub-env-reuses-behavior-arithmetic-point-join
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left)
        [b-id n2] (behavior-cell n1 right)
        parent-env (-> (h/behavior-bindings)
                       (with-binding 'a (env/cell-binding a-id) 0)
                       (with-binding 'b (env/cell-binding b-id) 0))
        outer-env (-> (h/default-bindings)
                      (with-binding 'a (env/cell-binding a-id) 0)
                      (with-binding 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (compile-expr expr outer-env {:net n2})
        result-net (run-compiled compiled)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}]
           (behavior-records out-content)))))

(deftest compiler-symbol-fast-path-exposes-the-canonical-cell
  (let [compiled (compile-source "(let-cell [x] x)")
        topology (net/network-dict-entry (:net compiled)
                                         env/lexical-topology-key)
        [frame-id frame] (first (filter (fn [[_ frame]]
                                         (some #{(:cell compiled)}
                                               (get-in frame [:bindings 'x])))
                                       (:frames topology)))
        bound-id (first (get-in frame [:bindings 'x]))
        prop-names (->> (vals (net/net-env (:net compiled)))
                        (filter prop/prop?)
                        (map prop/prop-name)
                        set)
        waiting (run-compiled compiled)
        with-value (nb/seed-cell waiting bound-id 12)
        settled (nb/run-propagators
                 with-value
                 (nb/neighbor-propagator-ids with-value bound-id))]
    (is (ids/node-id? (:scope/source-id frame)))
    (is (ids/node-id? (:scope/chain-id frame)))
    (is (ids/node-id? bound-id))
    (is (= bound-id (:cell compiled)))
    (is (= bound-id
           (env/reserved-binding-id (:net compiled) frame-id 'x)))
    (is (not (contains? prop-names :lexical-access/binding-candidates)))
    (is (not (contains? prop-names :lexical-access/binding-value)))
    (is (not (contains? prop-names :lexical-access/access-binding)))
    (is (= value/nothing (strongest waiting (:cell compiled))))
    (is (= 12 (strongest settled (:cell compiled))))))

(deftest compiler-imported-env-records-direct-lexical-addresses
  (let [compiled (compile-source "(+ 1 2)")
        frame (get-in (net/network-dict-entry (:net compiled)
                                              env/lexical-topology-key)
                      [:frames (:env compiled)])
        prop-names (->> (vals (net/net-env (:net compiled)))
                        (filter prop/prop?)
                        (map prop/prop-name)
                        set)]
    (is (= 1 (count (get-in frame [:bindings '+]))))
    (is (ids/node-id? (:scope/source-id frame)))
    (is (ids/node-id? (:scope/chain-id frame)))
    (is (not (contains? prop-names
                        [:compiler-2/lexical-projection :grouped])))
    (is (= 3 (strongest (run-compiled compiled) (:cell compiled))))))

(deftest topology-lookup-and-binding-dereference-remain-separate
  (let [bound-id (ids/new-node-id)
        env-id (ids/new-node-id)
        binding-answer-id (ids/new-node-id)
        value-answer-id (ids/new-node-id)
        lexical-env (with-binding (h/default-bindings)
                              'x
                              (env/cell-binding bound-id)
                              0)
        base (-> (scope-source-protocol-net)
                 (install-empty-cells [bound-id env-id binding-answer-id
                                       value-answer-id])
                 (nb/seed-cell bound-id 12))
        declared (env/declare-root base env-id lexical-env)
        n0 (:net declared)
        [access-props n1]
        ((env/p:lexical-access-local-first 'x env-id binding-answer-id) n0)
        [value-props n2] ((env/p:binding-value binding-answer-id value-answer-id)
                          n1)
        settled (nb/run-propagators
                 n2
                 (into (vec (:props declared))
                       (concat (installed-prop-ids access-props)
                               (installed-prop-ids value-props))))]
    (is (= (env/cell-binding bound-id)
           (strongest settled binding-answer-id)))
    (is (= 12 (strongest settled value-answer-id)))
    (is (not (scope-source/scope-value?
              (strongest settled value-answer-id))))))

#_(deftest execute-sub-env-reuses-behavior-arithmetic-point-non-continuation
  (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
        right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
        [a-id n1] (behavior-cell (behavior-protocol-net) left)
        [b-id n2] (behavior-cell n1 right)
        parent-env (-> (h/behavior-bindings)
                       (with-binding 'a (env/cell-binding a-id) 0)
                       (with-binding 'b (env/cell-binding b-id) 0))
        outer-env (-> (h/default-bindings)
                      (with-binding 'a (env/cell-binding a-id) 0)
                      (with-binding 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (compile-expr expr outer-env {:net n2})
        result-net (run-compiled compiled)]
    (is (= value/nothing (strongest result-net (:cell compiled))))))

#_(deftest execute-sub-env-reuses-behavior-arithmetic-late-shared-timestamp
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
        parent-env (-> (h/behavior-bindings)
                       (with-binding 'a (env/cell-binding a-id) 0)
                       (with-binding 'b (env/cell-binding b-id) 0))
        outer-env (-> (h/default-bindings)
                      (with-binding 'a (env/cell-binding a-id) 0)
                      (with-binding 'b (env/cell-binding b-id) 0))
        expr (execute-sub-env-ast (parse "(be:+ a b)") parent-env 'a 'b)
        compiled (compile-expr expr outer-env {:net n2})
        n4 (run-compiled compiled)
        [_left-tasks n5] (seed-behavior-message n4 a-id left-6-8)
        [right-tasks n6] (seed-behavior-message n5 b-id right-6-8)
        result-net (nb/run-propagators n6 right-tasks)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value n4 (:cell compiled))))
    (is (= 13 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}
            {:at 8 :value 13}]
           (behavior-records out-content)))))

#_(deftest compiler-2-application-can-execute-sub-env-behavior-arithmetic
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
        inner-env (-> (h/behavior-bindings)
                      (with-binding 'a (env/cell-binding a-id) 0)
                      (with-binding 'b (env/cell-binding b-id) 0))
        outer-env (-> (h/default-bindings)
                      (with-binding 'a (env/cell-binding a-id) 0)
                      (with-binding 'b (env/cell-binding b-id) 0))
        outer-expr (execute-sub-env-ast (parse "(be:+ a b)") inner-env 'a 'b)
        compiled (compile-expr outer-expr outer-env {:net n2})
        n3 (run-compiled compiled)
        [_left-tasks n4] (seed-behavior-message n3 a-id left-6-8)
        [right-tasks n5] (seed-behavior-message n4 b-id right-6-8)
        result-net (nb/run-propagators n5 right-tasks)
        out-content (net/network-cell-content result-net (:cell compiled))]
    (is (= 9 (behavior-current-value n3 (:cell compiled))))
    (is (= 13 (behavior-current-value result-net (:cell compiled))))
    (is (= [{:at 6 :value 9}
            {:at 8 :value 13}]
           (behavior-records out-content)))))
