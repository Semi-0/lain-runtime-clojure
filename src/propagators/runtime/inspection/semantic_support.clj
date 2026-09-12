(ns propagators.runtime.inspection.semantic-support
  "Pure compiler and network projection helpers for semantic graphs."
  (:require [propagators.compiler.compiler.declarations :as declarations]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.closure-value :as closure-value]
            [propagators.compiler.model.env :as cenv]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.compiler.lowering.application :as application]
            [propagators.compiler.main :as compiler]
            [propagators.infra.cells.value :as value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.graph :as pgraph]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]))

(def source
  "(let-cell [inc-local]
     (<-> inc-local
          (:: [x]
            (+ x 1)))
     (inc-local 5))")

(defn display-name [x]
  (cond
    (symbol? x) (str x)
    (keyword? x) (if-let [ns (namespace x)]
                   (str ns "/" (name x))
                   (name x))
    :else (pr-str x)))

(defn ast-label [expr]
  (let [expr (ast/ast expr)]
    (case (ast/type expr)
      :symbol (display-name (ast/name expr))
      :literal (display-name (ast/value expr))
      :network "::"
      :compound "::"
      :apply (str "(" (ast-label (ast/operator expr)) " ...)")
      (display-name (ast/type expr)))))

(defn- env-frames [env]
  (take-while #(and (some? %) (not (value/unusable? %)))
              (iterate #(obj/slot-value % cenv/env-parent-key) env)))

(defn environment-labels [env]
  (reduce
   (fn [labels frame]
     (reduce
      (fn [labels slot-key]
        (if (contains? cenv/env-internal-keys slot-key)
          labels
          (if-let [id (cenv/binding-id (cenv/lookup frame slot-key))]
            (if (contains? labels id)
              labels
              (assoc labels id (display-name slot-key)))
            labels)))
      labels
      (obj/public-slot-keys frame)))
   {}
   (env-frames env)))

(defn topology-binding-labels [n]
  (into {}
        (map (fn [[id sym]] [id (display-name sym)]))
        (cenv/binding-names n)))

(defn network-closure-values [n]
  (keep (fn [[id entry]]
          (let [strongest (net/network-cell-strongest n id)]
            (when (closure-value/closure-info? strongest)
              [id strongest])))
        (net/net-env n)))

(defn closure-labels [n]
  (into {}
        (map (fn [[id closure-info]]
               [id (str ":: "
                        (pr-str (closure-value/closure-inputs closure-info)))]))
        (network-closure-values n)))

(defn closure-key [closure-info]
  [(closure-value/closure-inputs closure-info)
   (closure-value/closure-output closure-info)
   (closure-value/closure-body closure-info)
   (closure-value/closure-scope closure-info)])

(defn unique-network-closure-values [n]
  (vals
   (reduce (fn [acc [_ closure-info]]
             (let [k (closure-key closure-info)]
               (if (contains? acc k)
                 acc
                 (assoc acc k closure-info))))
           {}
           (network-closure-values n))))

(defn closure-env-labels [n]
  (apply merge
         (map (fn [[_ closure-info]]
                (environment-labels (closure-value/closure-env closure-info)))
              (network-closure-values n))))

(defn generated-labels [ids prefix]
  (into {}
        (map-indexed (fn [index id]
                       [id (str prefix index)]))
        ids))

(defn simple-cell-value? [v]
  (or (number? v)
      (string? v)
      (boolean? v)
      (symbol? v)
      (keyword? v)
      (nil? v)))

(defn literal-cell-labels [n]
  (into {}
        (keep (fn [[id _entry]]
                (let [v (net/network-cell-strongest n id)]
                  (when (and (not (value/unusable? v))
                             (simple-cell-value? v))
                    [id (display-name v)]))))
        (net/net-env n)))

(defn application-records [compiled n]
  (vec
   (keep
    (fn [app-id]
      (let [topology (application/application-topology n app-id)]
        (cond
          topology
          (let [operator-id (:operator-id topology)
                operator
                (cond
                  (contains? (net/net-env n) operator-id)
                  (net/network-cell-strongest n operator-id)

                  :else
                  nil)
                operator-name (operator-value/operator-name operator)]
            {:app-id app-id
             :operator-label (display-name (or operator-name :application))
             :operator-cell operator-id
             :arg-cells (:argument-ids topology)
             :context-id (:context-id topology)
             :output-id (:result-id topology)
             :lowering :flat-gur})

          :else
          nil)))
    (or (:applications compiled)
        (compiler/compiled-applications (:net compiled))))))

(defn label-entry [id label]
  (when (and id (not (value/unusable? id)))
    [id label]))

(defn application-node-labels [compiled n]
  (into {}
        (mapcat
         (fn [{:keys [app-id operator-label operator-cell args-id context-id output-id]}]
           (keep identity
                 [(label-entry app-id (str "app:" operator-label))
                  (label-entry operator-cell (str "op:" operator-label))
                  (label-entry args-id (str "args:" operator-label))
                  (label-entry context-id (str "ctx:" operator-label))
                  (label-entry output-id (str "out:" operator-label))])))
        (application-records compiled n)))

(defn application-prop-labels [compiled n]
  (let [graph (net/net-graph n)]
    (into {}
          (keep
           (fn [{:keys [app-id operator-label]}]
             (cond
               (contains? graph app-id)
               [app-id (str "prop:" operator-label)]

               :else
               nil))
           (application-records compiled n)))))

(defn slot-prop-labels [n]
  (into {}
        (mapcat (fn [[_collection-id slots]]
                  (mapcat (fn [[slot-key parents]]
                            (keep (fn [[_parent-id {:keys [prop-id]}]]
                                    (when prop-id
                                      [prop-id (str "prop:"
                                                    (display-name slot-key))]))
                                  parents))
                          slots)))
        (net/network-dict-entry n :slot-declarations)))

(defn compiled-labels [compiled n]
  (let [graph (net/net-graph n)
        env-labels (merge (environment-labels (:env compiled))
                          (closure-env-labels n)
                          (topology-binding-labels n))
        props (vec (or (:props compiled)
                       (compiler/compiled-props (:net compiled))))
        applications (vec (or (:applications compiled)
                              (compiler/compiled-applications (:net compiled))))
        generated (generated-labels (keys graph) "cell")]
    (merge generated
           (literal-cell-labels n)
           (generated-labels props "prop")
           (generated-labels applications "app")
           (slot-prop-labels n)
           (application-node-labels compiled n)
           (application-prop-labels compiled n)
           (closure-labels n)
           env-labels
           {(:cell compiled) "result"})))

(defn network-edges [n]
  (let [graph (net/net-graph n)]
    (vec
     (distinct
      (mapcat
       (fn [[id node]]
         (concat
          (map (fn [input] [input id])
               (pgraph/node-input-ids node))
          (map (fn [output] [id output])
               (pgraph/node-output-ids node))))
       graph)))))

(defn network-vijual-graph [compiled n]
  (let [ids (vec (keys (net/net-graph n)))
        id->vijual-id (into {}
                            (map-indexed
                             (fn [index id]
                               [id (keyword (str "n" index))]))
                            ids)
        labels (compiled-labels compiled n)]
    {:id->vijual-id id->vijual-id
     :edges (mapv (fn [[from to]]
                    [(id->vijual-id from) (id->vijual-id to)])
                  (network-edges n))
     :nodes (into {}
                  (keep (fn [[id label]]
                          (when-let [vijual-id (id->vijual-id id)]
                            [vijual-id label])))
                  labels)}))

(defn compiled-progression [source]
  (let [compiled (compiler/compile-source source)
        expanded-net (nb/run-propagators (:net compiled) (:props compiled))]
    [{:title "Stage 1: compiled declaration graph"
      :compiled compiled
      :network (:net compiled)}
     {:title "Stage 2: after propagation evaluates closure application"
      :compiled compiled
      :network expanded-net}]))

(defn closure-body-progression [n]
  (vec
   (map-indexed
    (fn [index closure-info]
      (let [compiled (compiler/compile-expr
                      (closure-value/closure-body closure-info))]
        {:title (str "Closure body " index ": stored subgraph")
         :compiled compiled
         :network (:net compiled)}))
    (unique-network-closure-values n))))

(defn semantic-base-labels
  ([compiled n]
   (semantic-base-labels compiled n {}))
  ([compiled n {:keys [result-label] :or {result-label "result"}}]
   (let [env-labels (merge (environment-labels (:env compiled))
                           (closure-env-labels n)
                           (topology-binding-labels n))
         labels (merge (literal-cell-labels n)
                       (closure-labels n)
                       env-labels)]
     (if (contains? env-labels (:cell compiled))
       labels
       (assoc labels (:cell compiled) result-label)))))

(defn semantic-label [labels id]
  (or (get labels id) "cell"))

(defn semantic-key [key]
  (if (ids/node-id? key)
    [:cell key]
    key))

(defn semantic-node
  [state key label]
  (let [key (semantic-key key)]
    (if-let [id (get-in state [:key->id key])]
      [state id]
      (let [id (keyword (str "sem" (:next-id state)))]
        [(-> state
             (update :next-id inc)
             (assoc-in [:key->id key] id)
             (assoc-in [:nodes id] label))
         id]))))

(defn semantic-edge
  [state from-key from-label to-key to-label]
  (let [[state from-id] (semantic-node state from-key from-label)
        [state to-id] (semantic-node state to-key to-label)]
    (update state :edges conj [from-id to-id])))

(defn add-sync-semantic [state labels {:keys [app-id arg-cells]}]
  (let [[left right] arg-cells
        sync-key [:sync app-id]
        sync-label "<->"
        left-label (semantic-label labels left)
        right-label (semantic-label labels right)]
    (-> state
        (semantic-edge left left-label sync-key sync-label)
        (semantic-edge right right-label sync-key sync-label)
        (semantic-edge sync-key sync-label left left-label)
        (semantic-edge sync-key sync-label right right-label))))

(defn add-forward-sync-semantic [state labels {:keys [app-id arg-cells]}]
  (let [[source target] arg-cells
        sync-key [:forward-sync app-id]
        sync-label "->"
        source-label (semantic-label labels source)
        target-label (semantic-label labels target)]
    (-> state
        (semantic-edge [:cell source] source-label sync-key sync-label)
        (semantic-edge sync-key sync-label [:cell target] target-label))))

(defn add-call-semantic [state labels {:keys [app-id operator-label operator-cell arg-cells output-id]}]
  (let [call-key [:call app-id]
        call-label (str "call " operator-label)
        operator-label (semantic-label labels operator-cell)
        output-label (semantic-label labels output-id)]
    (as-> state state
      (semantic-edge state operator-cell operator-label call-key call-label)
      (reduce (fn [state arg-id]
                (semantic-edge state
                               arg-id
                               (semantic-label labels arg-id)
                               call-key
                               call-label))
              state
              arg-cells)
      (semantic-edge state call-key call-label output-id output-label))))

(defn add-operator-semantic [state labels {:keys [app-id operator-label arg-cells output-id]}]
  (let [operator-key [:operator app-id]
        output-label (semantic-label labels output-id)]
    (as-> state state
      (reduce (fn [state arg-id]
                (semantic-edge state
                               arg-id
                               (semantic-label labels arg-id)
                               operator-key
                               operator-label))
              state
              arg-cells)
      (semantic-edge state operator-key operator-label output-id output-label))))

(defn add-application-node-semantic
  [state labels {:keys [app-id operator-label arg-cells output-id]}]
  (let [app-key [:application app-id]
        app-label (str "app:" operator-label)
        output-label (semantic-label labels output-id)]
    (as-> state state
      (reduce (fn [state arg-id]
                (semantic-edge state
                               arg-id
                               (semantic-label labels arg-id)
                               app-key
                               app-label))
              state
              arg-cells)
      (semantic-edge state app-key app-label output-id output-label))))

(defn add-application-semantic [state labels {:keys [operator-label lowering] :as app}]
  (cond
    (= "<->" operator-label)
    (add-sync-semantic state labels app)

    (= "->" operator-label)
    (add-forward-sync-semantic state labels app)

    (= :closure-cell lowering)
    (add-call-semantic state labels app)

    :else
    (add-operator-semantic state labels app)))

(defn semantic-graph
  ([compiled n]
   (semantic-graph compiled n {}))
  ([compiled n opts]
   (let [labels (semantic-base-labels compiled n opts)
         state (reduce (fn [state app]
                         (add-application-semantic state labels app))
                       {:next-id 0
                        :key->id {}
                        :nodes {}
                        :edges []}
                       (application-records compiled n))]
     {:nodes (:nodes state)
      :edges (vec (distinct (:edges state)))})))

(defn semantic-progression [compiled n]
  (vec
   (cons
    {:title "Semantic main graph: compiler wiring collapsed"
     :graph (semantic-graph compiled n)}
    (map-indexed
     (fn [index closure-info]
       (let [compiled (compiler/compile-expr
                       (declarations/closure-semantic-body closure-info))]
         {:title (str "Semantic closure body " index ": compiler wiring collapsed")
          :graph (semantic-graph compiled (:net compiled) {:result-label "output"})}))
     (unique-network-closure-values n)))))
