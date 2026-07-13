(ns propagators.compiler-2.runtime.lexical-application
  "Retained closure application selected from a lexical scope cell."
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as application]
            [propagators.compiler-2.runtime.closure-frame :as closure-frame]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.gur.flat :as fvm]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def candidate-scope [:compiler-2 :lexical-closure])
(def raw-scope [:compiler-2 :selected-closure])

(defn- candidate-key [call-key {:keys [candidate dependencies]}]
  [call-key
   (scope-source/source-scope candidate)
   (scope-source/context-chain candidate)
   (sort-by pr-str dependencies)])

(defn- candidate-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [candidate-scope key])))

(defn- raw-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [raw-scope key])))

(defn- private-id [key role]
  (h/stable-node-id :lexical-closure key role))

(defn- scoped-projector [{:keys [candidate dependencies]}]
  (prop/primitive-propagator
   (fn [v]
     (if (value/unusable? v)
       value/nothing
       (scope-source/scope-value
        (scope-source/source-scope candidate)
        nil
        (scope-source/context-chain candidate)
        (scope-source/unwrap v)
        dependencies)))))

(defn- p:project-output [scope private-id public-id]
  ((scoped-projector scope) private-id public-id))

(defn- declare-raw-closure
  [compile* network key closure arg-ids out-id]
  (when-let [{:keys [input-ids targets]}
             (application/closure-call-plan closure arg-ids out-id)]
    (let [base-network network
          private-closure (private-id key :closure)
          frame-id (private-id key :env)
          network (h/seed-cell network private-closure closure)
          [env-props prepared-network]
          (application/declare-closure-environment
           network
           (closure-value/closure-env closure)
           frame-id
           (closure-value/closure-inputs closure)
           targets
           input-ids)
          [frame-prop compiled]
          ((closure-frame/p:apply-closure-with compile* private-closure frame-id)
           prepared-network)]
      (-> (topology-effects/network-diff base-network compiled
                                         (conj (vec env-props) frame-prop))
          (update :effects
                  #(into [(fvm/bind-name raw-scope key frame-id)] %))))))

(defn- declare-candidate
  [compile* network scope key closure arg-ids out-id]
  (when-let [{:keys [input-ids targets]}
             (application/closure-call-plan closure arg-ids out-id)]
    (let [private-closure (private-id key :closure)
          frame-id (private-id key :env)
          private-outputs (mapv #(private-id key [:output %])
                                (range (count targets)))
          private-targets (mapv (fn [[sym _] private-id] [sym private-id])
                                targets
                                private-outputs)
          prepared (-> network
                       (h/seed-cell private-closure closure)
                       (#(reduce h/ensure-cell
                                 %
                                 (concat private-outputs
                                         (map second targets)))))
          [env-props prepared]
          (application/declare-closure-environment
           prepared
           (closure-value/closure-env closure)
           frame-id
           (closure-value/closure-inputs closure)
           private-targets
           input-ids)
          [frame-prop with-frame]
          ((closure-frame/p:apply-closure-with compile* private-closure frame-id)
           prepared)
          [projection-props compiled]
          (reduce (fn [[props n] [private-id [_ public-id]]]
                    (let [[p n'] ((p:project-output scope private-id public-id) n)]
                      [(conj props p) n']))
                  [[] with-frame]
                  (map vector private-outputs targets))]
      (-> (topology-effects/network-diff
           network compiled (into (conj (vec env-props) frame-prop)
                                  projection-props))
          (update :effects
                  #(into [(fvm/bind-name candidate-scope key frame-id)] %))))))

(defn p:apply-lexical-closure-with
  "Select a scoped closure and retain its existing compiled closure frame."
  [compile* closure-id arg-ids out-id]
  (let [arg-ids (vec arg-ids)
        call-key [closure-id arg-ids out-id]
        activate
        (fn [_ _ network]
          (let [answer (h/strongest-or-nothing network closure-id)
                closure (scope-source/unwrap answer)]
            (cond
              (value/unusable? answer) []

              (and (not (scope-source/scope-value? answer))
                   (closure-value/closure-info? closure))
              (let [key [call-key :raw]]
                (if (raw-installed? network key)
                  []
                  (or (declare-raw-closure compile*
                                           network
                                           key
                                           closure
                                           arg-ids
                                           out-id)
                      [])))

              (not (scope-source/scope-value? answer)) []

              (not (closure-value/closure-info? closure)) []
              :else
              (let [input-count (count (closure-value/closure-inputs closure))
                    input-values (mapv #(h/strongest-or-nothing network %)
                                       (take input-count arg-ids))
                    scope (application/application-scope
                           (into [answer] input-values))
                    key (some->> scope (candidate-key call-key))]
                (cond
                  (nil? scope) []
                  (candidate-installed? network key) []
                  :else (or (declare-candidate compile*
                                               network
                                               scope
                                               key
                                               closure
                                               arg-ids
                                               out-id)
                            []))))))]
    (prop/construct-propagator
     (h/stable-node-id :compiler-2/lexical-closure
                       call-key
                       :prop)
     :compiler-2/lexical-closure
     activate
     (into [closure-id] arg-ids)
     [out-id])))

(defn p:apply-lexical-closure
  [closure-id arg-ids out-id]
  (p:apply-lexical-closure-with dispatch/compile-expression
                                closure-id arg-ids out-id))
