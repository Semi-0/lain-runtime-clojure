(ns propagators.runtime.network-observation-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.model.env :as compiler-env]
            [propagators.compiler.lowering.activation :as activation]
            [propagators.runtime.boundary.effects :as effects]
            [propagators.compiler.lowering.one-time-network :as one-time]
            [propagators.compiler.operators.network-observation :as observation]
            [propagators.runtime.session.environment-io :as environment-io]
            [propagators.runtime.session.state :as runtime-state]
            [propagators.infra.gur :as gur]
            [propagators.infra.ids :as ids]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

(defn- id-network
  []
  (let [a (ids/new-node-id)
        b (ids/new-node-id)
        p (ids/new-node-id)
        prepared (reduce nb/ensure-cell net/empty-net [a b])
        [_ installed]
        ((prop/construct-propagator
          p
          :test/id
          (prop/concrete-propagator
           (fn [_inputs _outputs network]
             [(message b (net/network-cell-strongest network a))]))
          [a]
          [b])
         prepared)]
    {:net installed :a a :b b :p p}))

(defn- apply-observer
  [source operator node-id]
  (let [snapshot-id (ids/new-node-id)
        node-value-id (ids/new-node-id)
        out-id (ids/new-node-id)
        working
        (-> source
            (nb/install-cell
             snapshot-id
             (observation/frozen-snapshot source)
             (observation/frozen-snapshot source))
            (nb/install-cell node-value-id node-id node-id)
            (nb/ensure-cell out-id))
        installer (operator-value/operator-install operator)
        [installed prop-ids _selected]
        (installer working [snapshot-id node-value-id] out-id)
        completed (activation/run-network installed
                                          [snapshot-id node-value-id]
                                          prop-ids)]
    {:net completed
     :out-id out-id
     :value
     (one-time/project-trace
      (one-time/compound-projection)
      completed
      out-id)}))

(deftest neighbors-observes-cells-and-propagators
  (let [{:keys [net a b p]} (id-network)
        cell-result
        (apply-observer net (observation/neighbors-operator) a)
        prop-result
        (apply-observer net (observation/neighbors-operator) p)]
    (is (= {:node/id a
            :node/inputs []
            :node/outputs [p]}
           (:value cell-result)))
    (is (= {:node/id p
            :node/inputs [a]
            :node/outputs [b]}
           (:value prop-result)))))

(deftest trace-neighbors-observes-extended-output-edges
  (let [{:keys [net b p]} (id-network)
        member (ids/new-node-id)
        extended (nb/extend-propagator-outputs net p [member])
        before (:value (apply-observer net (observation/neighbors-operator) p))
        after (:value (apply-observer extended (observation/neighbors-operator) p))
        reciprocal (:value (apply-observer extended
                             (observation/neighbors-operator) member))]
    ;; This is the same compound-valued primitive used by the Lain tracer.
    (is (= [b] (:node/outputs before)))
    (is (= #{b member} (set (:node/outputs after))))
    (is (= [p] (:node/inputs reciprocal)))
    (is (value/nothing? (net/network-cell-strongest extended member)))
    (is (= #{b} (:outputs (get (net/net-graph net) p))))
    (is (= (net/net-relationship net) (net/net-relationship extended)))))

(deftest neighbors-orders-identities-deterministically
  (let [root (ids/new-node-id)
        targets [(ids/new-node-id)
                 (ids/new-node-id)
                 (ids/new-node-id)]
        prepared (reduce nb/ensure-cell net/empty-net (into [root] targets))
        installed
        (reduce
         (fn [network [position target]]
           (second
            ((prop/construct-propagator
              target
              [:test/reader position]
              (fn [_ _ _] [])
              [root]
              [])
             network)))
         prepared
         (map-indexed vector targets))
        observed
        (:value
         (apply-observer
          installed
          (observation/neighbors-operator)
          root))]
    (is (= (vec (sort-by pr-str targets))
           (:node/outputs observed)))))

(deftest node-info-is-a-compound-projection
  (let [{:keys [net a p]} (id-network)
        observed
        (:value
         (apply-observer
          net
          (observation/node-info-operator)
          p))
        unnamed
        (:value
         (apply-observer
          net
          (observation/node-info-operator)
          a))]
    (is (= p (:node/id observed)))
    (is (= :propagator (:node/kind observed)))
    (is (= :test/id (:node/name observed)))
    (is (= value/nothing (:node/name unnamed)))))

(deftest unknown-node-produces-contradiction
  (let [{:keys [net]} (id-network)
        {:keys [net out-id]}
        (apply-observer
         net
         (observation/neighbors-operator)
         (ids/new-node-id))]
    (is (= value/contradiction
           (net/network-cell-strongest net out-id)))))

(deftest one-time-run-declares-flat-gur-only-in-temporary-net
  (let [closure-id (ids/new-node-id)
        context-id (ids/new-node-id)
        argument-id (ids/new-node-id)
        outer-result-id (ids/new-node-id)
        closure
        (gur/recursive-closure
         :test/identity
         (fn [{:keys [stable-id]} invocation-ids out-id]
           (let [[_context-id _snapshot-id value-id] invocation-ids]
             (gur/declare-prop
              (stable-id :identity)
              [value-id]
              [out-id]
              (prop/concrete-propagator
               (fn [_inputs _outputs network]
                 [(message
                   out-id
                   (net/network-cell-strongest
                    network
                    value-id))]))))))
        active
        (-> net/empty-net
            (nb/install-cell closure-id closure closure)
            (nb/install-cell context-id :context :context)
            (nb/install-cell argument-id 42 42)
            (nb/ensure-cell outer-result-id))
        before-count (count (net/net-env active))
        result
        (one-time/run-flat-gur-once
         active
         context-id
         closure-id
         [argument-id]
         (one-time/strongest-projection)
         outer-result-id)]
    (is (= 42 (:value result)))
    (is (:temporary-net-disposed? result))
    (is (> (count (net/net-env (:temporary-net result)))
           before-count))
    (is (= before-count (count (net/net-env active))))
    (is (= value/nothing
           (net/network-cell-strongest active outer-result-id)))))

(deftest trace-program-loads-through-existing-environment-api
  (let [module
        (.getCanonicalPath
         (io/file
          (io/resource "propagators/compiler/lowering/one_time_network.clj")))

        source
        (.getCanonicalPath
         (io/file
          (io/resource "propagators/runtime/lain/network_trace.lain")))

        imported
        (environment-io/install-primitive-environment
         (runtime-state/empty-state)
         {:file module
          :entry
          :propagators.compiler.lowering.one-time-network/trace-environment
          :revision 0})

        loaded
        (environment-io/load-lain
         {:drain-environment-effects
          effects/drain-environment-effects}
         (:state imported)
         {:boundary/kind :environment/load-lain
          :boundary/payload
          {:file source :revision 0}})]
    (is (= :loaded (get-in imported [:receipt :status])))
    (is (= :loaded (get-in loaded [:receipt :status])))
    (is (= 5 (get-in loaded [:receipt :installed-form-count])))
    (let [loaded-state (:state loaded)
          program-net (:program/net loaded-state)
          program-env (:program/env loaded-state)
          closure-id
          (compiler-env/resolve-binding-id
           program-net program-env 'trace-breadth-first)]
      (is (ids/node-id? closure-id))
      (is (gur/recursive-closure?
           (net/network-cell-strongest program-net closure-id))))))

(deftest lain-tracer-executes-in-a-disposed-flat-network
  (let [module (.getCanonicalPath
                (io/file (io/resource "propagators/compiler/lowering/one_time_network.clj")))
        source (.getCanonicalPath
                (io/file (io/resource "propagators/runtime/lain/network_trace.lain")))
        imported (environment-io/install-primitive-environment
                  (runtime-state/empty-state)
                  {:file module
                   :entry :propagators.compiler.lowering.one-time-network/trace-environment
                   :revision 0})
        loaded (environment-io/load-lain
                {:drain-environment-effects effects/drain-environment-effects}
                (:state imported)
                {:boundary/kind :environment/load-lain
                 :boundary/payload {:file source :revision 0}})
        state (:state loaded)
        closure-id (compiler-env/resolve-binding-id (:program/net state)
                                                    (:program/env state) 'trace-breadth-first)
        root (ids/new-node-id)
        context-id (ids/new-node-id)
        root-argument (ids/new-node-id)
        direction (ids/new-node-id)
        result-id (ids/new-node-id)
        active (-> (:program/net state)
                   (nb/install-cell root 42 42)
                   (nb/install-cell context-id :context :context)
                   (nb/install-cell root-argument root root)
                   (nb/install-cell direction :both :both))
        result (one-time/run-flat-gur-once active context-id closure-id
                                          [root-argument direction]
                                          (one-time/compound-projection) result-id)]
    (is (= root (get-in result [:value :trace/root])))
    (is (= [root] (get-in result [:value :trace/nodes])))
    (is (= [] (get-in result [:value :trace/edges])))
    (is (:temporary-net-disposed? result))
    (is (not (contains? (net/net-env active) result-id)))))
