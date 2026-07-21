(ns propagators.compiler-2.runtime.tui.annotations
  "Display annotations for projected TUI block values."
  (:require [clojure.string :as str]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.event :as event]
            [propagators.datastructures.tms :as tms]
            [propagators.ids :as ids]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(defn- behavior-projection?
  [v]
  (and (contains? (obj/public-slot-keys v) behavior/base-layer)
       (contains? (obj/public-slot-keys v) behavior/summary-layer)))

(defn project-value
  [v]
  (cond
    (value/unusable? v)
    v

    (semantic-trace/semantic-trace-graph? v)
    v

    (behavior-projection? v)
    (project-value (behavior/base-value v))

    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (if (value/unusable? current)
        current
        (project-value current)))

    (event/event-projection? v)
    (let [slot-keys (obj/public-slot-keys v)]
      (if (= 1 (count slot-keys))
        (obj/slot-value v (first slot-keys))
        (into {}
              (map (fn [slot-key]
                     [slot-key (obj/slot-value v slot-key)]))
              slot-keys)))

    (or (event/event-content? v)
        (event/event-fact? v))
    (project-value (event/strongest-value v))

    (tms/distributed-value? v)
    (let [projected (tms/strongest-distributed-value v)]
      (if (value/unusable? projected)
        projected
        (tms/distributed-base-value projected)))

    (net/network? v)
    (or (semantic-repl/display-cell-value v) "network")

    :else
    v))

(defn- sorted-pr
  [xs]
  (->> xs (sort-by pr-str) vec))

(defn- behavior-latest-time
  [v]
  (cond
    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (when-not (value/unusable? current)
        (behavior/summary-latest-time current)))

    (behavior-projection? v)
    (behavior/summary-latest-time v)))

(defn- behavior-annotation
  [v]
  (let [identities (behavior/identity-set v)]
    (when (seq identities)
      {:kind :behavior
       :identities (sorted-pr identities)
       :latest-time (behavior-latest-time v)})))

(declare value-annotations)

(defn- behavior-base-value
  [v]
  (cond
    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (when-not (value/unusable? current)
        (behavior/base-value current)))

    (behavior-projection? v)
    (behavior/base-value v)))

(defn- behavior-annotations
  [v]
  (let [self (behavior-annotation v)
        base (behavior-base-value v)
        nested (when base (value-annotations base))
        nested-behavior (first (filter #(= :behavior (:kind %)) nested))
        redundant-wrapper? (and self
                                nested-behavior
                                (= (:identities self)
                                   (:identities nested-behavior)))]
    (cond-> []
      (and self (not redundant-wrapper?)) (conj self)
      (seq nested) (into nested))))

(defn- tms-annotation
  [v]
  (let [projected (tms/strongest-distributed-value v)]
    (when-not (value/unusable? projected)
      (let [view (tms/tms-view (tms/distributed-slots projected))
            entry (get (tms/propositions view) tms/distributed-proposition)]
        {:kind :tms
         :status (:tms/status entry)
         :claims (sorted-pr (:tms/claims entry))
         :active-premises (sorted-pr (tms/active-premises view))}))))

(defn- tms-annotations
  [v]
  (let [projected (tms/strongest-distributed-value v)
        base (when-not (value/unusable? projected)
               (tms/distributed-base-value projected))
        annotation (tms-annotation v)]
    (cond-> []
      annotation (conj annotation)
      base (into (value-annotations base)))))

(defn- event-annotation
  [v]
  (let [facts (event/active-facts v)]
    (when (seq facts)
      {:kind :event
       :facts (mapv (fn [fact]
                      {:input-id (event/input-id fact)
                       :source (event/source fact)
                       :timestamp (event/timestamp fact)
                       :source-state (event/source-state fact)})
                    facts)})))

(defn value-annotations
  [v]
  (cond
    (value/unusable? v) []
    (or (event/event-content? v)
        (event/event-fact? v)
        (event/event-projection? v)) (if-let [annotation (event-annotation v)]
                                       [annotation]
                                       [])
    (behavior/behavior-value? v) (behavior-annotations v)
    (behavior-projection? v) (behavior-annotations v)
    (tms/distributed-value? v) (tms-annotations v)
    :else []))

(def max-annotation-token-length 48)

(defn- abbreviate
  [s]
  (if (> (count s) max-annotation-token-length)
    (str (subs s 0 (- max-annotation-token-length 3)) "...")
    s))

(defn- trace-subscription-identity?
  [x]
  (and (vector? x)
       (= :xr/trace-subscription (first x))))

(defn- primitive-event-identity?
  [x]
  (and (vector? x)
       (= :compiler-2/primitive (first x))))

(defn- derived-event-source?
  [x]
  (and (vector? x)
       (= :event/derived (first x))))

(defn- node-token
  [x]
  (when (ids/node-id? x)
    (str "node:" (subs (str (:uuid x)) 0 8))))

(defn- annotation-token
  [x]
  (cond
    (trace-subscription-identity? x) "xr-trace"
    (primitive-event-identity? x) "primitive"
    (derived-event-source? x) "derived"
    (ids/node-id? x) (node-token x)
    (keyword? x) (name x)
    (symbol? x) (name x)
    (string? x) x
    :else (abbreviate (pr-str x))))

(defn- evidence-timestamp-token
  [{:keys [input-id source timestamp]}]
  (str (annotation-token input-id)
       "/"
       (annotation-token source)
       "@"
       (annotation-token timestamp)))

(defn- event-timestamp-token
  [x]
  (if (and (set? x)
           (every? #(and (map? %)
                         (contains? % :input-id)
                         (contains? % :source)
                         (contains? % :timestamp))
                   x))
    (let [evidence (map evidence-timestamp-token
                        (sort-by (juxt (comp pr-str :input-id)
                                       (comp pr-str :source))
                                 x))]
      (if (= 1 (count evidence))
        (str "joined{" (first evidence) "}")
        (str "joined{\n"
             (str/join "\n" (map #(str "  " %) evidence))
             "\n}")))
    (annotation-token x)))

(defn- behavior-annotation-label
  [{:keys [identities latest-time]}]
  (str "[be:"
       (str/join "," (map annotation-token identities))
       (when (some? latest-time)
         (str " @" latest-time))
       "]"))

(defn- tms-annotation-label
  [{:keys [status claims active-premises]}]
  (str "[tms:"
       (or (some-> status name) "unknown")
       (when (seq claims)
         (str " claims=" (str/join "," (map annotation-token claims))))
       (when (seq active-premises)
         (str " premises=" (str/join "," (map annotation-token active-premises))))
       "]"))

(defn- event-fact-label
  [{:keys [input-id source timestamp source-state]}]
  (str (annotation-token input-id)
       "/"
       (annotation-token source)
       "@"
       (event-timestamp-token timestamp)
       (when (= event/retracted-state source-state)
         ":retracted")))

(defn- event-annotation-label
  [{:keys [facts]}]
  (if (= 1 (count facts))
    (str "[event:" (event-fact-label (first facts)) "]")
    (str "[event:\n"
         (str/join "\n" (map #(str "  " (event-fact-label %)) facts))
         "\n]")))

(defn format-annotations
  [annotations]
  (when (seq annotations)
    (str/join
     " "
     (map (fn [{:keys [kind] :as annotation}]
            (case kind
              :event (event-annotation-label annotation)
              :behavior (behavior-annotation-label annotation)
              :tms (tms-annotation-label annotation)
              (pr-str annotation)))
          annotations))))

(defn annotate-block-view
  [block-view raw-value]
  (let [annotations (value-annotations raw-value)]
    (cond-> block-view
      (seq annotations)
      (assoc :annotations annotations
             :annotation (format-annotations annotations)))))
