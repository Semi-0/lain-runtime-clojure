(ns propagators.compiler-2.runtime.closure-frame
  "Retained closure application from only a closure cell and an environment cell."
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as application]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.compiler.dispatch :as dispatch]
            [propagators.compiler-2.compiler.basis :as h]
            [propagators.compiler-2.runtime.topology-effects :as topology-effects]
            [propagators.gur.flat :as fvm]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def frame-scope [:compiler-2 :closure-frame])

(defn frame-key [closure-id env-id]
  [closure-id env-id])

(defn frame-installed? [network key]
  (boolean
   (get-in (net/network-dict-entry network fvm/name-bindings-key)
           [frame-scope key])))

(defn- compile-frame [compile* network closure-id env-id closure frame-env]
  (let [key (frame-key closure-id env-id)
        prepared (application/prepare-closure-frame
                  compile*
                  network
                  closure
                  frame-env
                  {:seed [:compiler-2/closure-frame key]
                   :application/cell-declarer :retained-frame})
        diff (topology-effects/network-diff network
                                            (:net prepared)
                                            (:props prepared))]
    (update diff :effects
            #(into [(fvm/bind-name frame-scope key env-id)] %))))

(defn p:apply-closure-with
  "Declare a closure body into the outer network using a pre-bound frame env."
  [compile* closure-id env-id]
  (let [key (frame-key closure-id env-id)
        activate
        (fn [_ _ network]
          (let [closure (h/strongest-or-nothing network closure-id)
                frame-env (h/strongest-or-nothing network env-id)]
            (cond
              (or (value/unusable? closure)
                  (value/unusable? frame-env)) []
              (not (closure-value/closure-info? closure)) []
              (frame-installed? network key) []
              :else (compile-frame compile* network closure-id env-id
                                   closure frame-env))))]
    (prop/construct-propagator
     (h/stable-node-id :compiler-2/closure-frame key :prop)
     activate
     [closure-id env-id]
     [])))

(defn p:apply-closure
  [closure-id env-id]
  (p:apply-closure-with dispatch/compile-expression closure-id env-id))


