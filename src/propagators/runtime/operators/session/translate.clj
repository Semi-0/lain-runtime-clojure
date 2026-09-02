(ns propagators.runtime.operators.session.translate
  "Small translation demo compiler-2 operator."
  (:require [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]))

(def ^:private jp->en
  {"猫" "cat"
   "犬" "dog"
   "水" "water"})

(def ^:private en->jp
  (into {} (map (fn [[jp en]] [en jp]) jp->en)))

(defn translate-messages
  [network jp-id en-id]
  (let [jp (net/network-cell-strongest network jp-id)
        en (net/network-cell-strongest network en-id)
        en' (when (string? jp) (get jp->en jp))
        jp' (when (string? en) (get en->jp en))]
    (cond-> []
      en' (conj (message en-id en'))
      jp' (conj (message jp-id jp')))))

(defn translate-operator []
  (operator-value/operator-closure
   {:name 'translate
    :output-selector (fn [arg-ids fallback-id]
                       (or (second (vec arg-ids)) fallback-id))
    :activate (fn [network _context-id arg-ids _out-id]
                (let [[jp-id en-id] (vec arg-ids)]
                  (when-not (and jp-id en-id (= 2 (count arg-ids)))
                    (throw (ex-info "translate expects Japanese and English cells"
                                    {:arg-ids arg-ids})))
                  (translate-messages network jp-id en-id)))}))
