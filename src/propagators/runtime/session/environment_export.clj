(ns propagators.runtime.session.environment-export
  "Pure S-expression export of versioned compiler-2 definitions.

  The exporter rebuilds ordinary `define`/`network` forms.  Private
  candidates are invoked through ordinary premise closures; no runtime router,
  generated NodeId, scheduler state, or JVM function is serialized."
  (:require [clojure.string :as str]
            [propagators.runtime.session.program.source :as source]
            [propagators.runtime.session.block-model :as block-model]
            [propagators.infra.datastructures.tms :as tms]
            [propagators.infra.network :as net]))

(def definition-heads '#{define})

(defn definition-form?
  [form]
  (and (seq? form) (contains? definition-heads (first form))))

(defn- form-body
  [form offset]
  (vec (drop offset form)))

(defn- closure-signature [closure]
  (when (and (seq? closure) (= 'network (first closure)))
    (let [[_ inputs & body] closure]
      {:inputs (vec inputs) :outputs [] :implicit? true :body (vec body)})))

(defn- explicit-premise
  [candidate]
  (when (and (seq? candidate)
             (#{'premise-closure 'distributed-premise-closure}
              (first candidate))
             (= 4 (count candidate)))
    {:operator (first candidate)
     :closure (second candidate)
     :premise (nth candidate 2)
     :epoch (nth candidate 3)}))

(defn definition-info [form]
  (when (definition-form? form)
    (let [[head name body] form
          explicit (explicit-premise body)
          signature (closure-signature (or (:closure explicit) body))]
      (cond
        signature
        (merge {:head head :name name :callable? true :candidate body :explicit explicit}
               (select-keys signature [:inputs :outputs :implicit?]))
        (= 2 (count form))
        {:head head :name name :storage? true}
        :else
        {:head head :name name :scalar? true :candidate body}))))

(defn- safe-token
  [x]
  (str/replace (str x) #"[^A-Za-z0-9_]" "_"))

(defn candidate-premise
  [{:keys [client-id index version]} name]
  [:lain/definition client-id index name version])

(defn- private-symbol
  [{:keys [client-id index version]} definition-name role & [position]]
  (symbol (str (clojure.core/name definition-name)
               "__lain_" (safe-token client-id) "_"
               index "_v" version "_" (clojure.core/name role)
               (when (some? position) (str "_" position)))))

(defn- candidate-entry
  [block record form]
  (when-let [info (definition-info form)]
    (when (or (:callable? info) (:scalar? info))
      (merge info
             {:client-id (:client-id block)
              :index (:index block)
              :version (:version record)
              :active-version? (= (:version record)
                                  (:version (peek (:version-history block))))
              :record record
              :form form}))))

(defn block-definition-entries
  [block]
  (vec
   (mapcat
    (fn [record]
      (keep #(candidate-entry block record %)
            (source/read-source-forms (:source record))))
    (:version-history block))))

(defn- current-entry
  [entries]
  (apply max-key :version entries))

(defn- callable-warnings
  [candidate current]
  (cond-> []
    (< (count (:inputs candidate)) (count (:inputs current)))
    (conj {:warning :extra-inputs
           :candidate-version (:version candidate)
           :count (- (count (:inputs current)) (count (:inputs candidate)))})

    (> (count (:inputs candidate)) (count (:inputs current)))
    (conj {:warning :missing-inputs
           :candidate-version (:version candidate)
           :count (- (count (:inputs candidate)) (count (:inputs current)))})

    (< (count (:outputs candidate)) (count (:outputs current)))
    (conj {:warning :missing-outputs
           :candidate-version (:version candidate)
           :count (- (count (:outputs current)) (count (:outputs candidate)))})

    (> (count (:outputs candidate)) (count (:outputs current)))
    (conj {:warning :extra-outputs
           :candidate-version (:version candidate)
           :count (- (count (:outputs candidate)) (count (:outputs current)))})))

(defn rewrite-callable
  [entries]
  (let [current (current-entry entries)
        private-forms
        (mapv (fn [candidate]
                (list 'define
                      (private-symbol candidate (:name candidate) :candidate)
                      (:candidate candidate)))
              entries)]
    {:form (:form current)
     :forms (conj private-forms (:form current))
     :warnings
     (vec
      (concat
       (mapcat #(callable-warnings % current) entries)
       (when (> (count entries) 1)
         [{:warning :preserved-candidates-archived
           :definition (:name current)
           :candidate-versions (mapv :version entries)
           :message
           "Candidates are reloadable private closures; automatic live switching is deferred."}])))}))

(defn rewrite-scalar
  [entries]
  (let [current (current-entry entries)
        private-forms
        (mapv (fn [candidate]
                (list 'define
                      (private-symbol candidate (:name candidate) :candidate)
                      (:candidate candidate)))
              entries)]
    {:form (:form current)
     :forms (conj private-forms (:form current))
     :warnings (if (> (count entries) 1)
                 [{:warning :preserved-candidates-archived
                   :definition (:name current)
                   :candidate-versions (mapv :version entries)
                   :message
                   "Scalar candidates are reloadable; automatic live switching is deferred."}]
                 [])}))

(defn rewrite-definition-group
  [entries]
  (if (:callable? (current-entry entries))
    (rewrite-callable entries)
    (rewrite-scalar entries)))

(defn- premise-active?
  [network context]
  (let [content (when-let [cell (:premise/state-cell context)]
                  (when (contains? (net/net-env network) cell)
                    (net/network-cell-content network cell)))]
    (and (tms/distributed-value? content)
         (contains? (-> content tms/distributed-slots tms/tms-view
                        tms/active-premises)
                    (:premise/id context)))))

(defn candidate-active?
  [network entry]
  (let [candidate (first (:definitions (:record entry)))
        internal (or (:candidate/context candidate)
                     {:premise/id (:premise-id (:record entry))
                      :premise/state-cell (:premise-state-cell (:record entry))})
        explicit (:candidate/explicit-context candidate)]
    (and (premise-active? network internal)
         (or (nil? explicit) (premise-active? network explicit)))))

(defn select-supported
  [network entries]
  (let [active (vec (filter #(candidate-active? network %) entries))]
    (when-not (= 1 (count active))
      (throw (ex-info "definition does not have one uniquely supported candidate"
                      {:definition (:name (first entries))
                       :active-versions (mapv :version active)
                       :candidate-versions (mapv :version entries)})))
    (first active)))

(defn versioned-definition-groups
  [runtime-state blocks]
  (->> blocks
       (map #(assoc % :client-id (:client-id %)))
       (mapcat block-definition-entries)
       (group-by (juxt :client-id :index :name))))

(defn rewrite-versioned-definitions
  [runtime-state blocks mode]
  (let [groups (versioned-definition-groups runtime-state blocks)
        rewritten
        (into {}
              (map (fn [[group entries]]
                     [group
                      (case mode
                        :preserve-premises
                        (rewrite-definition-group entries)

                        :commit-supported
                        (let [form (:form (select-supported
                                          (:program/net runtime-state)
                                          entries))]
                          {:form form
                           :forms [form]
                           :warnings []}))])
                   groups))]
    {:groups rewritten
     :warnings (vec (mapcat :warnings (vals rewritten)))}))

(defn block-premise
  [block record]
  [:lain/block (:client-id block) (:index block) (:version record)])

(defn preserve-expression
  "Wrap one ordinary root expression in its portable block-version support."
  [block record form current-version]
  (let [identity (assoc record
                        :client-id (:client-id block)
                        :index (:index block))
        raw (private-symbol identity 'block :raw-result)
        out (private-symbol identity 'block :result)
        premise (block-premise block record)
        body (cond-> [(list '-> form raw)
                      (list 'premise-input raw premise 0 out)]
               (not= (:version record) current-version)
               (conj (list 'premise-retract premise
                           (inc current-version) out))
               true (conj out))]
    (list* 'let-cell [raw out] body)))
