(ns propagators.compiler-2.runtime.inspection.graph-projection
  "Runtime semantic graph projection helpers."
  (:require [clojure.set :as set]
            [propagators.compiler-2.runtime.session.state :as state]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.event :as event]
            [propagators.graph :as graph]
            [propagators.network :as net]
            [propagators.propagator :as prop]
            [propagators.semantic-trace :as semantic-trace]))

(def labels state/labels)
(def stable-node-id state/stable-node-id)

(defn display-widget-value
  [v]
  (when-not (value/unusable? v)
    (cond
      (or (event/event-content? v)
          (event/event-fact? v)
          (event/event-projection? v))
      (let [values (vals (event/active-values v))]
        (cond
          (empty? values) nil
          (= 1 (count values)) (first values)
          :else (vec values)))

      :else
      (semantic-repl/display-cell-value v))))

(defn widget-node-id
  [widget-id]
  (stable-node-id :xr :widget widget-id))

(defn graph-cell-node-id
  [graph cell-id]
  (or (first (sort-by pr-str (get-in graph [:node-aliases cell-id])))
      (stable-node-id :xr :widget-cell cell-id)))

(defn graph-cell-node-ids
  [graph cell-id]
  (let [aliases (get-in graph [:node-aliases cell-id])]
    (if (seq aliases)
      aliases
      #{})))

(defn ensure-graph-cell-node
  [graph label cell-id]
  (let [node-id (graph-cell-node-id graph cell-id)]
    (-> graph
        (assoc-in [:nodes node-id] (or label "cell"))
        (update-in [:node-aliases cell-id] (fnil conj #{}) node-id))))

(defn graph-cell-value?
  [v]
  (cond
    (value/unusable? v)
    false

    (semantic-trace/semantic-trace-graph? v)
    false

    (and (contains? (obj/public-slot-keys v) behavior/base-layer)
         (contains? (obj/public-slot-keys v) behavior/summary-layer))
    true

    (behavior/behavior-value? v)
    true

    (net/network? v)
    false

    :else
    true))

(defn- assoc-graph-cell-value*
  [state labels* cell-id]
  (let [label (get labels* cell-id)
        graph0 (ensure-graph-cell-node (:graph state) label cell-id)
        node-id (graph-cell-node-id graph0 cell-id)
        strongest (net/network-cell-strongest (:program/net state) cell-id)
        graph1 (if (graph-cell-value? strongest)
                 (assoc-in graph0 [:values node-id] strongest)
                 (update graph0 :values dissoc node-id))]
    (assoc state
           :graph graph1
           :program/graph graph1)))

(defn assoc-graph-cell-value
  [state cell-id]
  (assoc-graph-cell-value* state (labels state) cell-id))

(defn assoc-graph-cell-values
  [state cell-ids]
  (let [labels* (labels state)]
    (reduce #(assoc-graph-cell-value* %1 labels* %2) state cell-ids)))

(defn- assoc-existing-graph-cell-values
  [state cell->node-ids]
  (let [program-net (:program/net state)
        graph (:graph state)
        graph' (reduce
                (fn [g [cell-id node-ids]]
                  (let [strongest (net/network-cell-strongest program-net cell-id)]
                    (reduce (fn [g* node-id]
                              (if (graph-cell-value? strongest)
                                (assoc-in g* [:values node-id] strongest)
                                (update g* :values dissoc node-id)))
                            g
                            node-ids)))
                graph
                cell->node-ids)]
    (assoc state
           :graph graph'
           :program/graph graph')))

(defn program-strongest-snapshot
  ([program-net]
   (program-strongest-snapshot program-net nil))
  ([program-net cell-ids]
  (into {}
        (keep (fn [[id entry]]
                (when (and (cell/cell? entry)
                           (or (nil? cell-ids)
                               (contains? cell-ids id)))
                  [id (cell/cell-strongest entry)])))
        (net/net-env program-net))))

(def ^:private missing-cell ::missing-cell)

(defn changed-cell-ids
  [before after]
  (->> (set/union (set (keys before)) (set (keys after)))
       (filter (fn [id]
                 (not (value/cell-value-equal?
                       (get before id missing-cell)
                       (get after id missing-cell)))))
       (sort-by pr-str)
       vec))

(defn- cell-id?
  [program-net id]
  (cell/cell? (get (net/net-env program-net) id)))

(defn- prop-id?
  [program-net id]
  (prop/prop? (get (net/net-env program-net) id)))

(defn downstream-cell-ids
  "Cells that can be affected by messages starting at `start-cell-ids`.

  This is a runtime projection helper, not scheduler state. It only narrows
  before/after comparison for UI change reporting."
  [program-net start-cell-ids]
  (let [g (net/net-graph program-net)]
    (loop [frontier (vec start-cell-ids)
           seen #{}
           cells #{}]
      (if-let [id (first frontier)]
        (if (contains? seen id)
          (recur (subvec frontier 1) seen cells)
          (let [node (get g id)
                next-ids (if node (graph/node-output-ids node) #{})
                cells' (if (cell-id? program-net id) (conj cells id) cells)
                props (filter #(prop-id? program-net %) next-ids)
                prop-outputs (mapcat (fn [prop-id]
                                        (graph/node-output-ids
                                         (graph/get-node g prop-id)))
                                      props)
                next-cells (filter #(cell-id? program-net %) prop-outputs)]
            (recur (into (subvec frontier 1) next-cells)
                   (conj seen id)
                   cells')))
        cells))))

(defn record-runtime-transaction
  [state before-program-net]
  (let [candidates (seq (:runtime/changed-candidate-cells state))
        label-aware? (or (seq (get-in state [:xr :launched]))
                         (seq (:trace/subscriptions state)))
        tracked (or (some-> candidates set)
                    (set (keys (get-in state [:graph :node-aliases]))))
        tracked (when (seq tracked) tracked)
        before (if before-program-net
                 (program-strongest-snapshot before-program-net tracked)
                 {})
        after (program-strongest-snapshot (:program/net state) tracked)
        graph (:graph state)
        changed (changed-cell-ids before after)
        pairs (keep (fn [cell-id]
                      (let [node-ids (seq (graph-cell-node-ids graph cell-id))]
                        (when node-ids
                          [cell-id node-ids])))
                    changed)
        cells (mapv first pairs)
        nodes (->> pairs
                   (mapcat second)
                   distinct
                   vec)]
    (-> (if label-aware?
          (assoc-graph-cell-values state cells)
          (assoc-existing-graph-cell-values state pairs))
        (assoc :runtime/changed-cells cells
               :runtime/changed-node-ids nodes)
        (dissoc :runtime/changed-candidate-cells))))

(defn widget-channel-view
  [state {:keys [widget/channel widget/view-cell widget/event-cell widget/view-value]}]
  (let [labels (labels state)]
    {:channel (str channel)
     :view-cell view-cell
     :view-cell-id (pr-str view-cell)
     :view-label (get labels view-cell)
     :event-cell event-cell
     :event-cell-id (pr-str event-cell)
     :event-label (get labels event-cell)
     :current (display-widget-value view-value)}))

(defn widget-ui
  [widget-type widget-id channels]
  {:kind "widget"
   :type (name widget-type)
   :widget-id (str widget-id)
   :channels (mapv #(select-keys %
                                 [:channel
                                  :view-label
                                  :event-label
                                  :current
                                  :epoch])
                   channels)})

(defn add-widget-to-graph
  [state widget-type widget-id channels]
  (let [node-id (widget-node-id widget-id)
        graph0 (:graph state)
        graph1 (assoc-in graph0 [:nodes node-id] (str widget-id))
        graph2 (assoc-in graph1 [:node-ui node-id] (widget-ui widget-type
                                                              widget-id
                                                              channels))
        graph3 (reduce
                (fn [graph {:keys [view-cell event-cell view-label event-label]}]
                  (let [view-node (graph-cell-node-id graph view-cell)
                        event-node (graph-cell-node-id graph event-cell)]
                    (-> graph
                        (ensure-graph-cell-node view-label view-cell)
                        (ensure-graph-cell-node event-label event-cell)
                        (update :edges (fnil into [])
                                [[view-node node-id]
                                 [node-id event-node]]))))
                graph2
                channels)]
    (assoc state
           :graph graph3
           :program/graph graph3)))

(defn record-widget-register
  [state request]
  (if (get-in state [:xr :widget-registrations (:boundary/id request)])
    state
    (let [{:widget/keys [type id channels]} (:boundary/payload request)
          widget-id (str id)
          channels* (mapv #(widget-channel-view state %) channels)
          channel-map (into {}
                            (map (fn [channel]
                                   [(:channel channel) channel]))
                            channels*)]
      (-> state
          (assoc-in [:xr :widget-registrations (:boundary/id request)] true)
          (assoc-in [:xr :widgets widget-id]
                    {:id widget-id
                     :type (name type)
                     :channels channel-map
                     :epoch (:boundary/epoch request)})
          (update-in [:xr :effects] (fnil conj []) request)
          (add-widget-to-graph type widget-id channels*)))))
