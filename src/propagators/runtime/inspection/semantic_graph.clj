(ns propagators.runtime.inspection.semantic-graph
  "Pure semantic graph construction and runtime value annotation."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [propagators.runtime.inspection.semantic-support :as demo]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.closure-value :as closure-value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]))

(def internal-slot-namespaces
  #{"ast" "closure" "env" "generic" "method" "operator"})

(defn- internal-slot-keyword?
  [slot-key]
  (and (keyword? slot-key)
       (contains? internal-slot-namespaces (namespace slot-key))))

(defn- internal-slot?
  [slot-key]
  (or (internal-slot-keyword? slot-key)
      (and (vector? slot-key)
           (internal-slot-keyword? (first slot-key)))))

(defn- declaration-records
  [n]
  (vec
   (for [collection-id (keys (net/net-env n))
         :let [declarations (merge-with merge
                                        (obj/slot-declarations-for n collection-id)
                                        (obj/accessor-declarations-for n collection-id))]
         [slot-key parents] declarations
         :when (not (internal-slot? slot-key))
         parent-id (keys parents)]
     {:collection-id collection-id
      :slot-key slot-key
      :parent-id parent-id})))

(defn- label
  [labels id]
  (or (get labels id) "cell"))

(defn- ensure-node
  [state key node-label]
  (if-let [node-id (get-in state [:key->id key])]
    [state node-id]
    (let [node-id (keyword (str "struct" (:next-id state)))]
      [(-> state
           (update :next-id inc)
           (assoc-in [:key->id key] node-id)
           (assoc-in [:nodes node-id] node-label))
       node-id])))

(defn display-cell-value
  [v]
  (when-not (value/unusable? v)
    (let [s (cond
              (closure-value/closure-info? v) (str ":: " (pr-str (closure-value/closure-inputs v)))
              (net/network? v) "network"
              :else (demo/display-name v))]
      (if (< 200 (count s))
        (str (subs s 0 200) "...")
        s))))

(defn- alias-node-set
  [v]
  (cond
    (nil? v) #{}
    (set? v) v
    (sequential? v) (set v)
    :else #{v}))

(defn- merge-node-aliases
  [& aliases]
  (apply merge-with
         set/union
         (map (fn [alias-map]
                (into {}
                      (map (fn [[k v]] [k (alias-node-set v)]))
                      alias-map))
              aliases)))

(defn- runtime-cell-id
  [key]
  (cond
    (ids/node-id? key) key
    (and (vector? key)
         (= :cell (first key))) (second key)
    :else nil))

(defn- state-node-aliases
  [state]
  (reduce-kv
   (fn [aliases key node-id]
     (if-let [cell-id (runtime-cell-id key)]
       (update aliases cell-id (fnil conj #{}) node-id)
       aliases))
   {}
   (:key->id state)))

(defn- graph-from-state
  [state]
  {:nodes (:nodes state)
   :node-aliases (state-node-aliases state)
   :values (:values state)
   :node-ui (:node-ui state)
   :expansions (:expansions state)
   :edges (vec (distinct (:edges state)))})

(defn- annotate-node
  [state node-id v]
  (if-let [label (display-cell-value v)]
    (assoc-in state [:values node-id] label)
    state))

(defn- ensure-semantic-node
  [state {:keys [key label value]}]
  (let [[state node-id] (demo/semantic-node state key label)]
    [(annotate-node state node-id value) node-id]))

(defn- add-edge
  [state from-key from-label to-key to-label]
  (demo/semantic-edge state from-key from-label to-key to-label))

(defn- structural-graph
  [compiled n]
  (let [labels (demo/compiled-labels compiled n)]
    (graph-from-state
     (reduce
      (fn [state {:keys [collection-id slot-key parent-id]}]
        (let [[state collection-node] (ensure-node state
                                                   [:cell collection-id]
                                                   (label labels collection-id))
              [state slot-node] (ensure-node state
                                             [:slot collection-id slot-key]
                                             (str "slot " (demo/display-name slot-key)))
              [state parent-node] (ensure-node state
                                               [:cell parent-id]
                                               (label labels parent-id))
              state (annotate-node state
                                   parent-node
                                   (net/network-cell-strongest n parent-id))]
          (update state :edges conj [collection-node slot-node] [slot-node parent-node])))
      {:next-id 0
       :key->id {}
       :nodes {}
       :values {}
       :node-ui {}
       :expansions {}
       :edges []}
      (declaration-records n)))))

(defn- output-symbols
  [output]
  (cond
    (symbol? output) [output]
    (vector? output) output
    :else []))

(defn- expr-entry
  [env expr]
  (let [expr (ast/ast expr)]
    (case (ast/type expr)
      :symbol
      (let [sym (ast/name expr)]
        (get env sym {:key [:symbol sym]
                      :label (demo/display-name sym)}))

      :literal
      {:key [:literal (ast/value expr)]
       :label (demo/display-name (ast/value expr))
       :value (ast/value expr)}

      {:key [:expr expr]
       :label (demo/ast-label expr)})))

(declare add-expr-semantics)

(defn- add-sync-semantics
  [state env app-id path args]
  (let [[left-expr right-expr] args
        sync-key [:closure app-id :sync path]
        sync-label "<->"
        [state left] (add-expr-semantics state env app-id (conj path 0) left-expr)
        [state right] (add-expr-semantics state env app-id (conj path 1) right-expr)]
    [(-> state
         (add-edge (:key left) (:label left) sync-key sync-label)
         (add-edge (:key right) (:label right) sync-key sync-label)
         (add-edge sync-key sync-label (:key left) (:label left))
         (add-edge sync-key sync-label (:key right) (:label right)))
     right]))

(defn- add-forward-sync-semantics
  [state env app-id path args]
  (let [[source-expr target-expr] args
        sync-key [:closure app-id :forward-sync path]
        sync-label "->"
        [state source] (add-expr-semantics state env app-id (conj path 0) source-expr)
        [state target] (add-expr-semantics state env app-id (conj path 1) target-expr)]
    [(-> state
         (add-edge (:key source) (:label source) sync-key sync-label)
         (add-edge sync-key sync-label (:key target) (:label target)))
     target]))

(defn- add-operator-semantics
  [state env app-id path operator-label args]
  (let [operator-key [:closure app-id :operator path operator-label]
        [state arg-entries]
        (reduce
         (fn [[state entries] [index arg]]
           (let [[state entry] (add-expr-semantics state env app-id (conj path index) arg)]
             [state (conj entries entry)]))
         [state []]
         (map-indexed vector args))]
    [(reduce
      (fn [state {:keys [key label]}]
        (add-edge state key label operator-key operator-label))
      state
      arg-entries)
     {:key operator-key
      :label operator-label}]))

(defn- add-expr-semantics
  [state env app-id path expr]
  (let [expr (ast/ast expr)]
    (case (ast/type expr)
      :sequence
      (reduce
       (fn [[state _entry] [index form]]
         (add-expr-semantics state env app-id (conj path index) form))
       [state nil]
       (map-indexed vector (ast/body expr)))

      :symbol
      (let [{:keys [key label] :as entry} (expr-entry env expr)
            [state _] (ensure-semantic-node state entry)]
        [state entry])

      :literal
      (let [{:keys [key label] :as entry} (expr-entry env expr)
            [state _] (ensure-semantic-node state entry)]
        [state entry])

      :apply
      (let [operator (ast/operator expr)
            operator-label (demo/ast-label operator)
            args (ast/args expr)]
        (cond
          (= "<->" operator-label)
          (add-sync-semantics state env app-id path args)

          (= "->" operator-label)
          (add-forward-sync-semantics state env app-id path args)

          :else
          (add-operator-semantics state env app-id path operator-label args)))

      (let [{:keys [key label] :as entry} (expr-entry env expr)
            [state _] (ensure-semantic-node state entry)]
        [state entry]))))

(defn- closure-value
  [n {:keys [operator-cell]}]
  (let [v (net/network-cell-strongest n operator-cell)]
    (when (closure-value/closure-info? v)
      v)))

(defn- closure-env
  [n labels {:keys [app-id arg-cells output-id]} closure-info]
  (let [inputs (closure-value/closure-inputs closure-info)
        outputs (output-symbols (closure-value/closure-output closure-info))]
    (merge
     (into {}
           (map (fn [[sym arg-id]]
                  [sym {:key [:closure app-id :input sym]
                        :label (demo/display-name sym)
                        :value (net/network-cell-strongest n arg-id)}]))
           (map vector inputs (take (count inputs) arg-cells)))
     (into {}
           (map (fn [[sym arg-id]]
                  [sym {:key [:cell arg-id]
                        :label (label labels arg-id)
                        :value (net/network-cell-strongest n arg-id)}]))
           (map vector outputs (drop (count inputs) arg-cells))))))

(defn- add-input-bindings
  [state labels {:keys [app-id arg-cells]} closure-info]
  (reduce
   (fn [state [sym arg-id]]
     (add-edge state
               [:cell arg-id]
               (label labels arg-id)
               [:closure app-id :input sym]
               (demo/display-name sym)))
   state
   (map vector (closure-value/closure-inputs closure-info) arg-cells)))

(defn- add-output-bindings
  [state labels {:keys [app-id arg-cells]} closure-info]
  (let [input-count (count (closure-value/closure-inputs closure-info))
        outputs (output-symbols (closure-value/closure-output closure-info))]
    (reduce
     (fn [state [sym arg-id]]
       (add-edge state
                 [:closure app-id :output sym]
                 (demo/display-name sym)
                 [:cell arg-id]
                 (label labels arg-id)))
     state
     (map vector outputs (drop input-count arg-cells)))))

(defn- closure-output-cells
  [{:keys [arg-cells output-id]} closure-info]
  (let [input-count (count (closure-value/closure-inputs closure-info))
        outputs (vec (drop input-count arg-cells))]
    (if (seq outputs)
      outputs
      [output-id])))

(defn- closure-call-label
  [labels {:keys [operator-label operator-cell]}]
  (str "call " (let [label (label labels operator-cell)]
                 (if (= "cell" label) operator-label label))))

(defn- annotate-cell
  [state n labels cell-id]
  (first
   (ensure-semantic-node state {:key [:cell cell-id]
                                :label (label labels cell-id)
                                :value (net/network-cell-strongest n cell-id)})))

(declare expansion-graph)

(defn- add-collapsed-closure-semantics
  [state n labels {:keys [app-id arg-cells] :as app} closure-info]
  (let [input-count (count (closure-value/closure-inputs closure-info))
        input-cells (take input-count arg-cells)
        output-cells (closure-output-cells app closure-info)
        call-key [:call app-id]
        call-label (closure-call-label labels app)
        [state call-node-id] (demo/semantic-node state call-key call-label)
        expansion (expansion-graph n labels app closure-info)]
    (as-> (assoc-in state [:expansions call-node-id] expansion) state
      (reduce (fn [state cell-id]
                (annotate-cell state n labels cell-id))
              state
              (concat input-cells output-cells))
      (reduce (fn [state arg-id]
                (add-edge state
                          [:cell arg-id]
                          (label labels arg-id)
                          call-key
                          call-label))
              state
              input-cells)
      (reduce (fn [state out-id]
                (add-edge state
                          call-key
                          call-label
                          [:cell out-id]
                          (label labels out-id)))
              state
              output-cells))))

(defn- add-inlined-closure-semantics
  [state n labels app closure-info]
  (let [state (add-input-bindings state labels app closure-info)
        env (closure-env n labels app closure-info)
        [state _] (add-expr-semantics state env (:app-id app) [:body]
                                      (closure-value/closure-body closure-info))]
    state))

(defn- expansion-graph
  [n labels app closure-info]
  (graph-from-state
   (add-inlined-closure-semantics {:next-id 0
                                   :key->id {}
                                   :nodes {}
                                   :values {}
                                   :node-ui {}
                                   :expansions {}
                                   :edges []}
                                  n
                                  labels
                                  app
                                  closure-info)))

(defn- application-graph
  [compiled n]
  (let [labels (demo/semantic-base-labels compiled n)
        state (reduce
               (fn [state app]
                 (if-let [closure-info (closure-value n app)]
                   (add-collapsed-closure-semantics state n labels app closure-info)
                   (demo/add-application-semantic state labels app)))
               {:next-id 0
                :key->id {}
                :nodes {}
                :values {}
                :node-ui {}
                :expansions {}
                :edges []}
               (demo/application-records compiled n))]
    (graph-from-state state)))

(defn compiled-semantic-graph
  [compiled network]
  (let [semantic (application-graph compiled network)
        structural (structural-graph compiled network)]
    {:nodes (merge (:nodes semantic) (:nodes structural))
     :node-aliases (merge-node-aliases (:node-aliases structural)
                                       (:node-aliases semantic))
     :values (merge (:values semantic) (:values structural))
     :node-ui (merge (:node-ui semantic) (:node-ui structural))
     :expansions (:expansions semantic)
     :edges (vec (distinct (concat (:edges semantic) (:edges structural))))}))

(defn compiled-application-semantic-graph
  [compiled network]
  (application-graph compiled network))

(defn- parse-node-id
  [node]
  (cond
    (string? node) (try
                     (edn/read-string node)
                     (catch Throwable _ node))
    :else node))

(defn expansion
  [graph {:keys [node label]}]
  (let [node (parse-node-id node)
        node (or node
                 (first (keep (fn [[node-id node-label]]
                                (when (= label node-label)
                                  node-id))
                              (:nodes graph))))]
    (get-in graph [:expansions node])))

(defn semantic-graph
  [source]
  (let [[_compiled-stage expanded-stage] (demo/compiled-progression source)
        compiled (:compiled expanded-stage)
        n (:network expanded-stage)
        graph (compiled-semantic-graph compiled n)]
    (assoc graph :source source)))

(defn edge-labels
  [{:keys [nodes edges]}]
  (mapv (fn [[from to]]
          [(get nodes from) (get nodes to)])
        edges))

(defn value-labels
  [{:keys [nodes values]}]
  (into {}
        (map (fn [[node-id v]]
               [(get nodes node-id) v]))
        values))
