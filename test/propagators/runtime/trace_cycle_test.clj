(ns propagators.runtime.trace-cycle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [propagators.compiler.lowering.one-time-network :as one-time]
            [propagators.compiler.model.env :as env]
            [propagators.compiler.operators.network-observation :as observation]
            [propagators.runtime.boundary.effects :as effects]
            [propagators.runtime.session.environment-io :as environment-io]
            [propagators.runtime.session.state :as runtime-state]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

(def ^:dynamic *trace-state* nil)

(defn- resource-path [path]
  (.getCanonicalPath (io/file (io/resource path))))

(use-fixtures :once
  (fn [run-tests]
    (let [imported (environment-io/install-primitive-environment
                    (runtime-state/empty-state)
                    {:file (resource-path "propagators/compiler/lowering/one_time_network.clj")
                     :entry :propagators.compiler.lowering.one-time-network/trace-environment
                     :revision 0})
          loaded (environment-io/load-lain
                   {:drain-environment-effects effects/drain-environment-effects}
                   (:state imported)
                   {:boundary/kind :environment/load-lain
                    :boundary/payload
                    {:file (resource-path "propagators/runtime/lain/network_trace.lain")
                     :revision 0}})]
      (binding [*trace-state* (:state loaded)] (run-tests)))))

(deftest cyclic-trace-expands-each-node-once-and-reaches-quiescence
  (let [state *trace-state*
        closure-id (env/resolve-binding-id (:program/net state) (:program/env state)
                                          'trace-breadth-first)
        [root p context arg direction out] (repeatedly 6 ids/new-node-id)
        seeded (-> (:program/net state) (nb/install-cell root 42 42)
                   (nb/install-cell context :context :context)
                   (nb/install-cell arg root root)
                   (nb/install-cell direction :both :both))
        [_ active] ((prop/construct-propagator p :test/noop
                      (prop/concrete-propagator (fn [_ _ _] [])) [root] []) seeded)
        queries (atom [])
        freeze observation/frozen-snapshot
        result
        (with-redefs [observation/frozen-snapshot
                      (fn [network]
                        (let [snapshot (freeze network)]
                          (reify observation/NetworkSnapshot
                            (snapshot-neighbors [_ id]
                              (swap! queries conj id)
                              (observation/snapshot-neighbors snapshot id))
                            (snapshot-node-info [_ id]
                              (observation/snapshot-node-info snapshot id)))))]
          (one-time/run-flat-gur-once active context closure-id [arg direction]
                                      (one-time/compound-projection) out))]
    (is (= #{root p} (set (get-in result [:value :trace/nodes]))))
    (is (= 2 (count (get-in result [:value :trace/nodes]))))
    (is (= #{[root p] [p root]} (set (get-in result [:value :trace/edges]))))
    (is (= {root 1 p 1} (frequencies @queries)))
    (is (:temporary-net-disposed? result))
    (is (not (contains? (net/net-env active) out)))
    (is (= 42 (net/network-cell-strongest active root)))))
