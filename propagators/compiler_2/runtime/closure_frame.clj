(ns propagators.compiler-2.runtime.closure-frame
  "Retained closure application from only a closure cell and an environment cell."
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as application]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.model.env :as env]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.datastructures.compound-object :as obj]
            [propagators.gur.flat :as fvm]
            [propagators.ids :as ids]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def frame-scope [:compiler-2 :closure-frame])

(defn frame-key [closure-id env-id]
  [closure-id env-id])

(defn frame-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [frame-scope key])))

(defn- materialized-frame?
  [network env-id]
  (let [frame (h/strongest-or-nothing network env-id)]
    (and (net/network? frame)
         (not (obj/accessor-network? frame)))))

(defn- local-declarations
  [frame]
  (keep (fn [sym]
          (when-let [id (some-> (env/lookup frame sym) env/binding-id)]
            [sym id]))
        (env/local-bindings frame)))

(defn- declare-live-frame
  [network key parent-id frame]
  (let [frame-id (h/stable-node-id :compiler-2 :closure-frame key :live-env)
        declarations (vec (local-declarations frame))
        [frame-props framed]
        ((env/p:scope-frame parent-id frame-id (map first declarations))
         (h/ensure-cell network frame-id))]
    (reduce
     (fn [[n props] [sym binding-id]]
       (let [[new-props declared]
             ((env/p:declare-canonical-local sym frame-id binding-id) n)]
         [declared (into props new-props)]))
     [framed (vec frame-props)]
     declarations)))

(defn- prepare-frame-environment
  [network key env-id]
  (if (materialized-frame? network env-id)
    (let [frame (h/strongest-or-nothing network env-id)
          parent-id (obj/slot-value frame env/env-parent-key)]
      (if (ids/node-id? parent-id)
        (let [[declared props] (declare-live-frame network key parent-id frame)]
          [declared
           (h/stable-node-id :compiler-2 :closure-frame key :live-env)
           props])
        (let [imported-id (h/stable-node-id :compiler-2 :closure-frame key
                                            :imported-env)
              [imported _] (env/import-environment network imported-id frame)]
          [imported imported-id []])))
    [network env-id []]))

(defn prepare-frame-topology
  "Compile one known raw closure against its addressed frame environment."
  [compile* network closure-id env-id closure]
  (let [key (frame-key closure-id env-id)
        [prepared-network frame-env-id frame-props]
        (prepare-frame-environment network key env-id)
        prepared (application/prepare-closure-frame
                  compile*
                  prepared-network
                  closure
                  frame-env-id
                  {:seed [:compiler-2/closure-frame key]
                   :application/cell-declarer :retained-frame})
        props (into frame-props (:props prepared))]
    {:key key
     :marker (fvm/bind-name frame-scope key env-id)
     :net (:net prepared)
     :props props}))

(defn declare-frame-topology
  [compile* network closure-id env-id closure]
  (let [{:keys [marker net props]}
        (prepare-frame-topology compile* network closure-id env-id closure)]
    (update (topology-effects/network-diff network net props)
            :effects #(into [marker] %))))

(defn p:apply-closure-with
  "Declare a closure body into the outer network using a pre-bound frame env."
  [compile* closure-id env-id]
  (let [key (frame-key closure-id env-id)
        activate
        (fn [_ _ network]
          (let [closure-answer (h/strongest-or-nothing network closure-id)
                closure closure-answer]
            (cond
              (value/unusable? closure-answer) []
              (not (closure-value/closure-info? closure)) []
              (frame-installed? network key) []
              :else (declare-frame-topology compile* network closure-id env-id
                                             closure))))]
    (prop/construct-propagator
     (h/stable-node-id :compiler-2/closure-frame key :prop)
     :compiler-2/closure-frame
     activate
     [closure-id]
     [])))

(defn p:apply-closure
  [closure-id env-id]
  (p:apply-closure-with dispatch/default-compiler closure-id env-id))
