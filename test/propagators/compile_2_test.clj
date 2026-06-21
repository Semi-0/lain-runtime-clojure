(ns propagators.compile-2-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.cells.cell-protocol :as protocol]
            [propagators.cells.value :as value]
            [propagators.closure :as closure]
            [propagators.compile :as compile]
            [propagators.compiler-2.application :as compiler-app]
            [propagators.compiler-2.application-value :as application-value]
            [propagators.compiler-2.ast :as ast]
            [propagators.compiler-2.closure-value :as closure-value]
            [propagators.compiler-2.env :as env]
            [propagators.compiler-2.helpers :refer [behavior-env
                                                    default-env
                                                    dependency-env]]
            [propagators.compiler-2.main :as main]
            [propagators.compiler-2.parser :as parser]
            [propagators.core :as core]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.behavior-algebra :as hist]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.dependency :as dependency]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.ids :as ids]
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

(defn- seeded-cell
  [n v]
  (let [id (ids/new-node-id)]
    [id (nb/seed-cell (nb/install-cell n id) id v)]))

(defn- behavior-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-behavior-protocol))))

(defn- scope-source-protocol-net
  []
  (-> net/empty-net
      (compile/install-and-run (protocol/install-cell-protocol))
      (compile/install-and-run (protocol/install-scope-source-protocol))))

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
  (mapv behavior-record-map (behavior/history-records v)))

(defn- behavior-current-value
  [n id]
  (let [v (strongest n id)]
    (if (value/unusable? v)
      v
      (behavior/base-value v))))

(defn- scoped-base
  [v]
  (scope-source/base-value v))

(defn- install-empty-cells
  [n ids]
  (reduce nb/install-cell n ids))

(defn- seed-behavior-message
  [n id v]
  (core/eval-cell id (message id v) n))

(defn- parse
  [source]
  (parser/parse-string source))

(defn- compile-source
  ([source]
   (main/compile-source source))
  ([source env opts]
   (main/compile-source source env opts)))

(deftest compile-2-compiles-primitive-application
  (testing "application returns a fresh result cell"
    (let [compiled (compile-source "(+ 1 2)")
          result-net (run-compiled compiled)]
      (is (= 3 (strongest result-net (:cell compiled))))
      (is (= (:cell compiled) (main/compiled-result (:net compiled))))
      (is (= (:props compiled) (main/compiled-props (:net compiled)))))))

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

(deftest compile-2-behavior-env-merges-same-timestamp-values
  (testing "compiled behavior arithmetic joins retained point histories"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 6 7)] #{[:b 6]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(+ a b)" env {:net n2})
          result-net (run-compiled compiled)
          out-content (net/network-cell-content result-net (:cell compiled))]
      (is (= 9 (behavior-current-value result-net (:cell compiled))))
      (is (= [{:at 6 :value 9}]
             (behavior-records out-content))))))

(deftest compile-2-behavior-env-does-not-imply-point-continuation
  (testing "compiled behavior arithmetic does not join different point timestamps"
    (let [left (behavior-view [(hist/point-record 6 2)] #{[:a 6]})
          right (behavior-view [(hist/point-record 7 7)] #{[:b 7]})
          [a-id n1] (behavior-cell (behavior-protocol-net) left)
          [b-id n2] (behavior-cell n1 right)
          env (-> (behavior-env)
                  (env/bind 'a (env/cell-binding a-id) 0)
                  (env/bind 'b (env/cell-binding b-id) 0))
          compiled (compile-source "(+ a b)" env {:net n2})
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
          compiled (compile-source "(+ a b)" env {:net n2})
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
          compiled (compile-source "(+ a b)" env {:net n2})
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
      (is (= :apply
             (ast/type (obj/slot-value closure-info
                                       main/closure-body-slot)))))))

(deftest compile-2-closure-declaration-alone-does-not-evaluate-body
  (testing "declaring a network closure only installs closure data/slot topology"
    (let [compiled (compile-source "(:: [x] (+ x 1))")
          result-net (run-compiled compiled)]
      (is (= 1 (count (:props compiled))))
      (is (empty? (net/network-dict-entry result-net
                                          compiler-app/apply-application-props-key))))))

(deftest compile-2-application-installs-application-propagator
  (testing "network closure calls are evaluated by compiler-2 p:apply-application"
    (let [compiled (compile-source "((:: [x] (+ x 1)) 4)")
          apply-props (net/network-dict-entry
                       (:net compiled)
                       compiler-app/apply-application-props-key)
          [app-id] (main/compiled-applications (:net compiled))
          app-info (strongest (:net compiled) app-id)
          result-net (run-compiled compiled)]
      (is (= 1 (count apply-props)))
      (is (application-value/application-info? app-info))
      (is (= :closure-cell
             (obj/slot-value app-info main/application-lowering-slot)))
      (is (contains? (set (:props compiled)) (first apply-props)))
      (is (= 5 (strongest result-net (:cell compiled)))))))

(deftest compile-2-env-lookup-uses-nearest-scope-source-shadowing
  (testing "a child binding with a nearer scope source is selected from the compound env"
    (let [parent-id (ids/new-node-id)
          child-id (ids/new-node-id)
          env (-> (default-env)
                  (env/bind 'x (env/cell-binding parent-id) 0)
                  env/enter-scope
                  (env/bind 'x (env/cell-binding child-id)))
          binding (env/lookup env 'x)]
      (is (= :cell (:binding/type binding)))
      (is (= child-id (:binding/id binding))))))

(deftest compile-2-lexical-access-preserves-scope-source-content
  (testing "lexical access copies all scoped candidates and lets cell strongest choose"
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
          n2 (nb/run-propagators n1 [access-prop])
          content (net/network-cell-content n2 out-id)
          selected (strongest n2 out-id)]
      (is (scope-source/scope-content? content))
      (is (= 2 (count (scope-source/content-candidates content))))
      (is (scope-source/scope-value? selected))
      (is (= child-id (:binding/id (scoped-base selected)))))))

(deftest compile-2-lexical-access-conflict-belongs-to-cell-strongest
  (testing "equal-nearest scoped candidates are received intact and strongest contradicts"
    (let [left-id (ids/new-node-id)
          right-id (ids/new-node-id)
          env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          chain [:root :child]
          left (scope-source/scope-value :child
                                         :child
                                         chain
                                         (env/cell-binding left-id))
          right (scope-source/scope-value :child
                                          :child
                                          chain
                                          (env/cell-binding right-id))
          env-value (obj/compound-object
                     {'x (scope-source/merge-content left right)})
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [env-id out-id])
                 (nb/seed-cell env-id env-value))
          [access-prop n1] ((env/p:lexical-access 'x env-id out-id) n0)
          n2 (nb/run-propagators n1 [access-prop])
          content (net/network-cell-content n2 out-id)]
      (is (= 2 (count (scope-source/content-candidates content))))
      (is (= value/contradiction (strongest n2 out-id))))))

(deftest compile-2-lexical-access-retains-non-ancestor-candidates
  (testing "non-ancestor candidates remain content but are not strongest"
    (let [env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          unrelated (scope-source/scope-value :other
                                              :child
                                              [:root :child]
                                              (env/cell-binding
                                               (ids/new-node-id)))
          env-value (obj/compound-object {'x unrelated})
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [env-id out-id])
                 (nb/seed-cell env-id env-value))
          [access-prop n1] ((env/p:lexical-access 'x env-id out-id) n0)
          n2 (nb/run-propagators n1 [access-prop])
          content (net/network-cell-content n2 out-id)]
      (is (= 1 (count (scope-source/content-candidates content))))
      (is (= value/nothing (strongest n2 out-id))))))

(deftest compile-2-lexical-access-reads-accessor-backed-env-slot
  (testing "lexical access can read an accessor source slot without materializing env"
    (let [binding-id (ids/new-node-id)
          env-id (ids/new-node-id)
          out-id (ids/new-node-id)
          candidate (scope-source/scope-value :root
                                              :root
                                              [:root]
                                              (env/cell-binding binding-id))
          accessor-env (obj/as-accessor-network {'x candidate})
          n0 (-> (scope-source-protocol-net)
                 (install-empty-cells [env-id out-id])
                 (nb/seed-cell env-id accessor-env))
          [access-prop n1] ((env/p:lexical-access 'x env-id out-id) n0)
          n2 (nb/run-propagators n1 [access-prop])
          selected (strongest n2 out-id)]
      (is (scope-source/scope-value? selected))
      (is (= binding-id (:binding/id (scoped-base selected)))))))

(deftest compile-2-network-env-ops-build-scoped-compound-env
  (testing "scope propagators receive parent env one-way and bind locals into a fresh child env"
    (let [parent-x-id (ids/new-node-id)
          local-x-id (ids/new-node-id)
          parent-y-id (ids/new-node-id)
          parent-env-id (ids/new-node-id)
          inherited-env-id (ids/new-node-id)
          local-binding-id (ids/new-node-id)
          scoped-env-id (ids/new-node-id)
          parent-env (env/bind (default-env)
                               'x
                               (env/cell-binding parent-x-id)
                               0)
          parent-env-with-y (env/bind parent-env
                                      'y
                                      (env/cell-binding parent-y-id)
                                      0)
          n0 (-> (nb/install-cells [parent-env-id
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
          n3 (nb/run-propagators n2 [sub-prop bind-prop])
          inherited-env (strongest n3 inherited-env-id)
          scoped-env (strongest n3 scoped-env-id)
          n4 (nb/seed-cell n3 parent-env-id parent-env-with-y)
          n5 (nb/run-propagators n4
                                 (nb/neighbor-propagator-ids n4 parent-env-id))
          scoped-env-after-parent-update (strongest n5 scoped-env-id)]
      (is (= parent-x-id (:binding/id (env/lookup inherited-env 'x))))
      (is (= local-x-id (:binding/id (env/lookup scoped-env 'x))))
      (is (= local-x-id
             (:binding/id (env/lookup scoped-env-after-parent-update 'x))))
      (is (= parent-y-id
             (:binding/id (env/lookup scoped-env-after-parent-update 'y)))))))

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
  (testing "<-> installs bidirectional sync and returns the second cell"
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
      (is (= b-id (:cell compiled)))
      (is (= b-id (obj/slot-value app-info main/application-output-slot)))
      (is (= 42 (strongest result-net b-id))))))

(deftest compile-2-supports-switch-operator
  (testing "default env includes switch"
    (let [compiled (compile-source "(switch 9 true)")
          result-net (run-compiled compiled)]
      (is (= 9 (strongest result-net (:cell compiled)))))))

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
                                      (<-> out (some-net 2))
                                      out)")
          some-net-id (:binding/id (env/lookup (:env compiled) 'some-net))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          closure-compiled
          (compile-source
           "(compound [x] out
              (+ x 1))")
          closure-value (strongest (:net closure-compiled)
                                   (:cell closure-compiled))
          n0 (run-compiled compiled)
          n1 (nb/seed-cell n0 some-net-id closure-value)
          n2 (nb/run-propagators n1
                                 (nb/neighbor-propagator-ids n1 some-net-id))]
      (is (= value/nothing (strongest n0 out-id)))
      (is (= 3 (strongest n2 out-id))))))

(deftest compile-2-application-before-closure-waits-for-later-input-fire
  (testing "an application can exist before the operator closure and evaluate on a later input update"
    (let [compiled (compile-source "(let-cell [some-net out]
                                      (<-> out (some-net a))
                                      out)")
          some-net-id (:binding/id (env/lookup (:env compiled) 'some-net))
          a-id (:binding/id (env/lookup (:env compiled) 'a))
          out-id (:binding/id (env/lookup (:env compiled) 'out))
          closure-compiled (compile-source "(compound [x] out
                                             (+ x 1))")
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
