(ns propagators.compiler-2.runtime.operators.session.targets
  "TUI target and instance compiler-2 operators."
  (:require [propagators.compiler-2.compiler.basis :as basis]
            [propagators.compiler-2.runtime.ids :as runtime-ids]
            [propagators.compiler-2.runtime.operators.session.common :as common]
            [propagators.cells.value :as value]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def effect-slot-key common/effect-slot-key)
(def block-at-text-id common/block-at-text-id)
(def block-at-display-id common/block-at-display-id)
(def tui-write-effect-request common/tui-write-effect-request)
(def trace-target-value common/trace-target-value)
(def effect-tick common/effect-tick)

(defn- block-target-messages
  [network outbox-id instance-id index-id target-id]
  (let [text-id (and instance-id
                     index-id
                     (block-at-text-id network instance-id index-id))
        source-value (when text-id
                       (net/network-cell-strongest network text-id))
        target-value (net/network-cell-strongest network target-id)
        write-message (when (and text-id
                                 (not (value/nothing? target-value)))
                        (let [epoch (effect-tick network)
                              effect-id [:tui/write-block
                                         text-id
                                         epoch
                                         (hash target-value)]]
                          (message outbox-id
                                   (obj/compound-object
                                    {(effect-slot-key effect-id)
                                     (tui-write-effect-request effect-id
                                                               text-id
                                                               target-value
                                                               epoch)}))))]
    (cond-> []
      text-id
      (conj (message target-id source-value))

      write-message
      (conj write-message))))

(defn block-target-operator
  [outbox-id instance-id]
  (operator-value/operator-closure
   {:name 'block
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 1 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[index-id maybe-out] (vec arg-ids)
                      target-id (or maybe-out out-id)]
                  (when-not (#{1 2} (count arg-ids))
                    (throw (ex-info "block expects index and optional output"
                                    {:arg-ids arg-ids})))
                  (block-target-messages network
                                         outbox-id
                                         instance-id
                                         index-id
                                         target-id)))}))

(defn p:block
  "Declare a persistent proxy from `out-id` to one TUI block display cell."
  [instance-id index-id out-id]
  (fn [network]
    (let [display-id (block-at-display-id network instance-id index-id)]
      (when-not display-id
        (throw (ex-info "block index does not resolve to a display cell"
                        {:instance-id instance-id
                         :index-id index-id
                         :out-id out-id})))
      ((prop/construct-propagator
        (runtime-ids/stable-node-id :tui :block instance-id index-id out-id)
        :runtime/tui-block
        (fn [_inputs _outputs current]
          (basis/mono-sync-messages current out-id display-id))
        [index-id out-id]
        [display-id])
       network))))

(defn block-cell-operator
  "Compile `(block n)` as syntax sugar for `(p:block session n out)`."
  [instance-id]
  (operator-value/operator-closure
   {:name 'block
    :static-installer
    (fn [network arg-ids fallback-id]
      (when-not (= 1 (count arg-ids))
        (throw (ex-info "block expects one index"
                        {:arg-ids arg-ids})))
      (let [index-id (first arg-ids)
            [prop-id installed] ((p:block instance-id index-id fallback-id)
                                 network)]
        [installed [prop-id] fallback-id]))}))

(defn trace-target-operator []
  (operator-value/operator-closure
   {:name 'trace-target
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 2 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[label-id source-id maybe-out] (vec arg-ids)
                      target-id (or maybe-out out-id)
                      label (net/network-cell-strongest network label-id)]
                  (when-not (and label-id source-id (#{2 3} (count arg-ids)))
                    (throw (ex-info "trace-target expects label, source, and optional output"
                                    {:arg-ids arg-ids})))
                  (if (value/unusable? label)
                    []
                    [(message target-id
                              (trace-target-value label source-id))])))}))

(defn instance-operator []
  (operator-value/operator-closure
   {:name 'instance
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 1 nil) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[instance-id maybe-out] (vec arg-ids)
                      target-id (or maybe-out out-id)
                      source-value (net/network-cell-strongest network instance-id)
                      target-value (net/network-cell-strongest network target-id)]
                  (when-not (#{1 2} (count arg-ids))
                    (throw (ex-info "instance expects instance and optional output"
                                    {:arg-ids arg-ids})))
                  (cond-> []
                    (not (value/unusable? source-value))
                    (conj (message target-id source-value))

                    (not (value/unusable? target-value))
                    (conj (message instance-id target-value)))))}))
