(ns propagators.compiler-2.runtime.session.environment-io
  "Runtime boundary evaluation for live environments and plain .lain files.

  This is intentionally a boundary service, not a compiler extension.  Its
  inputs are declarative requests emitted by ordinary propagators; loaded
  forms always return through the canonical compiler-2 CPS entrypoint."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [propagators.compiler-2.model.env :as cenv]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.compiler-2.runtime.session.program :as program]
            [propagators.compiler-2.runtime.session.environment-export :as export]
            [propagators.compiler-2.runtime.session.program.source :as source]
            [propagators.compiler-2.runtime.session.state :as state]
            [propagators.compiler-2.runtime.tui.block-model :as block-model]
            [propagators.compiler-2.runtime.tui.versioned-commit :as versioned-commit]
            [propagators.network-builder :as nb])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardCopyOption]
           [java.security MessageDigest]
           [java.util UUID]))

(def max-load-depth 32)

(def transient-heads
  '#{load-primitive-environment load-lain load-blocks
     save-environment save-blocks
     block block-at be:block be:block-at be:event-block-at trace trace-target
     xr-io io:xr})

(defn canonical-file
  [file]
  (.getCanonicalFile (io/file file)))

(defn sha-256
  [^bytes bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn file-digest
  [file]
  (sha-256 (Files/readAllBytes (.toPath (canonical-file file)))))

(defn- source-form-head
  [form]
  (when (seq? form) (first form)))

(defn semantic-form?
  [form]
  (not (contains? transient-heads (source-form-head form))))

(defn- top-level-form-list?
  [form]
  (and (seq? form) (seq form) (every? seq? form)))

(defn lain-forms
  "Read either consecutive normal forms or one outer list of normal forms."
  [text]
  (let [forms (source/read-source-forms text)]
    (if (and (= 1 (count forms))
             (top-level-form-list? (first forms)))
      (vec (first forms))
      forms)))

(defn- ensure-loadable-path!
  [runtime-state canonical]
  (let [path (.getPath canonical)
        stack (vec (:environment/load-stack runtime-state))]
    (when (some #{path} stack)
      (throw (ex-info "cyclic .lain load"
                      {:path path :load-stack stack})))
    (when (>= (count stack) max-load-depth)
      (throw (ex-info "maximum .lain nesting depth exceeded"
                      {:path path
                       :maximum-depth max-load-depth
                       :load-stack stack})))
    path))

(defn- hidden-form-block
  [runtime-state client-id source-text]
  (let [order (block-model/next-global-order runtime-state)]
    {:client-id client-id
     :index order
     :order order
     :epoch 0
     :source source-text}))

(defn compile-hidden-form
  "Compile one hidden source form without replaying earlier program topology."
  [runtime-state client-id form origin]
  (let [source-text (pr-str form)
        block (hidden-form-block runtime-state client-id source-text)
        result-key [client-id (:index block)]
        candidate (-> runtime-state
                      (update :external-sources (fnil conj []) block)
                      (assoc :next-order (inc (:order block)))
                      (program/compile-program-form block 0 source-text))
        failure (get-in candidate [:program/results result-key :error])]
    (if failure
      {:state runtime-state
       :error {:message failure
               :data (get-in candidate [:program/results result-key :data])
               :form form
               :source source-text
               :origin origin}}
      {:state (cond-> candidate
                (semantic-form? form)
                (update :environment/source-ledger
                        (fnil conj [])
                        {:order (:order block)
                         :client-id client-id
                         :origin origin
                         :form form
                         :source source-text}))
       :compiled (:compiled candidate)})))

(defn- failure-receipt
  [canonical digest revision successful failure]
  {:status :failed
   :canonical-path (.getPath canonical)
   :digest digest
   :revision revision
   :installed-form-count successful
   :diagnostics [failure]})

(defn- drain-form-effects
  [runtime-state drain-effects]
  (let [known (set (keys (:environment/effects runtime-state)))
        drained (drain-effects runtime-state)
        failed (->> (:environment/effects drained)
                    (remove (comp known key))
                    (keep (fn [[effect-id delivery]]
                            (when (= :failed (:status delivery))
                              {:effect-id effect-id
                               :receipt (:receipt delivery)})))
                    vec)]
    (if (seq failed)
      {:state drained
       :error {:message "nested environment effect failed"
               :effects failed}}
      {:state drained})))

(defn- diagnostic
  [error form]
  (if (instance? Throwable error)
    {:message (ex-message error)
     :data (ex-data error)
     :class (some-> error class .getName)
     :form form}
    (assoc error :form form)))

(defn load-lain-state
  [runtime-state {:keys [file revision]} drain-effects]
  (let [canonical (canonical-file file)
        path (ensure-loadable-path! runtime-state canonical)
        bytes (Files/readAllBytes (.toPath canonical))
        digest (sha-256 bytes)
        forms (lain-forms (String. bytes StandardCharsets/UTF_8))
        client-id (str "file-" (subs digest 0 12))]
    (loop [current (update runtime-state :environment/load-stack conj path)
           remaining forms
           installed 0]
      (if-let [form (first remaining)]
        (let [{next-state :state failure :error}
              (compile-hidden-form current client-id form path)]
          (if failure
            {:state (update next-state :environment/load-stack pop)
             :receipt (failure-receipt canonical digest revision installed failure)}
            (let [drained
                  (try
                    (drain-form-effects next-state drain-effects)
                    (catch Throwable t
                      {:error t}))]
              (if-let [t (:error drained)]
                {:state (update next-state :environment/load-stack pop)
                 :receipt
                 (failure-receipt
                  canonical digest revision installed
                  (diagnostic t form))}
                (recur (:state drained)
                       (next remaining)
                       (inc installed))))))
        {:state (update current :environment/load-stack pop)
         :receipt {:status :loaded
                   :canonical-path path
                   :digest digest
                   :revision revision
                   :installed-form-count installed
                   :diagnostics []}}))))

(defn- keyword->var
  [entry]
  (let [ns-sym (symbol (namespace entry))
        var-sym (symbol (name entry))]
    (or (ns-resolve ns-sym var-sym)
        (throw (ex-info "primitive environment entry not found"
                        {:entry entry})))))

(defn- validate-binding!
  [[sym operator :as pair]]
  (when-not (and (sequential? pair) (= 2 (count pair)))
    (throw (ex-info "primitive binding must be [symbol operator]"
                    {:binding pair})))
  (when-not (symbol? sym)
    (throw (ex-info "primitive binding name must be a symbol"
                    {:binding pair})))
  (when-not (operator-value/operator-closure? operator)
    (throw (ex-info "primitive binding value must be a compiler-2 operator"
                    {:symbol sym :value operator})))
  [sym operator])

(defn- read-primitive-bindings
  [canonical entry]
  ;; Local primitive modules are explicitly trusted executable Clojure code.
  (load-file (.getPath canonical))
  (let [factory (keyword->var entry)
        bindings (factory)]
    (when-not (sequential? bindings)
      (throw (ex-info "primitive environment entry must return ordered pairs"
                      {:entry entry :value bindings})))
    (mapv validate-binding! bindings)))

(defn install-primitive-environment
  [runtime-state {:keys [file entry revision]}]
  (let [canonical (canonical-file file)
        digest (file-digest canonical)
        bindings (read-primitive-bindings canonical entry)
        parent-id (:program/env runtime-state)
        child-id (state/stable-node-id :live-primitive-environment
                                       (.getPath canonical) entry revision digest)
        [scope-props scoped-net]
        ((cenv/p:scope-frame parent-id child-id (set (map first bindings)))
         (:program/net runtime-state))
        declared (cenv/declare-bindings scoped-net child-id child-id bindings)
        props (into (vec scope-props) (:props declared))
        installed (nb/run-propagators (:net declared) props)
        import-record {:file (.getPath canonical)
                       :entry entry
                       :revision revision
                       :digest digest
                       :environment child-id
                       :binding-count (count bindings)}]
    {:state (-> runtime-state
                (assoc :program/net installed :program/env child-id)
                (update :environment/primitive-imports (fnil conj []) import-record)
                (update :environment/source-ledger
                        (fnil conj [])
                        {:order (block-model/next-global-order runtime-state)
                         :origin (.getPath canonical)
                         :form (list 'load-primitive-environment
                                     (.getPath canonical) entry revision)
                         :source (pr-str
                                  (list 'load-primitive-environment
                                        (.getPath canonical) entry revision))}))
     :receipt {:status :loaded
               :canonical-path (.getPath canonical)
               :digest digest
               :revision revision
               :installed-form-count (count bindings)
               :diagnostics []}}))

(defn- uuid-for
  [& parts]
  (str (UUID/nameUUIDFromBytes
        (.getBytes (pr-str parts) StandardCharsets/UTF_8))))

(defn- current-version
  [block]
  (:version (peek (vec (:version-history block)))))

(defn- next-visible-index
  [runtime-state client-id]
  (let [blocks (get-in runtime-state [:tuis client-id :blocks])
        blank (peek blocks)]
    (if (and blank (nil? (:source blank))
             (empty? (:version-history blank)))
      (:index blank)
      (or (some-> blocks peek :index inc) 0))))

(defn load-blocks-state
  [runtime-state {:keys [file revision client-id]} drain-effects]
  (let [canonical (canonical-file file)
        path (ensure-loadable-path! runtime-state canonical)
        bytes (Files/readAllBytes (.toPath canonical))
        digest (sha-256 bytes)
        forms (lain-forms (String. bytes StandardCharsets/UTF_8))]
    (loop [current (update runtime-state :environment/load-stack conj path)
           remaining forms
           installed 0]
      (if-let [form (first remaining)]
        (let [index (next-visible-index current client-id)
              block (block-model/block-by-index current client-id index)]
          (when-not block
            (throw (ex-info "load-blocks requires a registered versioned client"
                            {:client-id client-id :index index})))
          (let [advanced
                (try
                  (let [{next-state :state}
                        (versioned-commit/commit-version-state
                         current
                         {:commit-id
                          (uuid-for :load-block path revision client-id index installed)
                          :client-id client-id
                          :index index
                          :expected-version (current-version block)
                          :text (pr-str form)})]
                    (drain-form-effects next-state drain-effects))
                  (catch Throwable t
                    {:error t}))]
            (if-let [t (:error advanced)]
              {:state (update current :environment/load-stack pop)
               :receipt (failure-receipt
                         canonical digest revision installed
                         (diagnostic t form))}
              (recur (:state advanced)
                     (next remaining)
                     (inc installed)))))
        {:state (update current :environment/load-stack pop)
         :receipt {:status :loaded
                   :canonical-path path
                   :digest digest
                   :revision revision
                   :installed-form-count installed
                   :diagnostics []}}))))

(defn- selected-blocks
  [runtime-state client-id selection]
  (let [blocks (->> (get-in runtime-state [:tuis client-id :blocks])
                    (filter #(seq (:version-history %)))
                    (map #(assoc % :client-id client-id)))]
    (if (= :all selection)
      (vec blocks)
      (let [wanted (set selection)]
        (vec (filter #(contains? wanted (:index %)) blocks))))))

(defn- block-current-source
  [block]
  (:source (peek (vec (:version-history block)))))

(defn- block-all-sources
  [block]
  (mapv :source (:version-history block)))

(defn- versioned-block-forms
  [runtime-state block mode]
  (let [records (:version-history block)
        groups (:groups (export/rewrite-versioned-definitions
                         runtime-state [block] mode))
        ordered-groups (map second (sort-by (comp pr-str first) groups))
        warnings (vec (mapcat :warnings ordered-groups))
        definition-forms (vec (mapcat #(or (:forms %) [(:form %)])
                                      ordered-groups))
        record-forms
        (case mode
          :preserve-premises
          (let [current (peek records)
                current-version (:version current)
                active (->> (source/read-source-forms (:source current))
                            (remove export/definition-form?)
                            (map #(export/preserve-expression
                                   block current % current-version)))
                archived
                (mapcat
                 (fn [record]
                   (map-indexed
                    (fn [position form]
                      (list 'def
                            (symbol (str "block__lain_"
                                         (:index block) "_v"
                                         (:version record) "_archived_"
                                         position))
                            (list 'cell [] form)))
                    (remove export/definition-form?
                            (source/read-source-forms (:source record)))))
                 (butlast records))]
            (concat archived active))

          :commit-supported
          (->> (source/read-source-forms (:source (peek records)))
               (remove export/definition-form?)))]
    {:forms (vec (concat definition-forms record-forms))
     :warnings (cond-> warnings
                 (and (= :preserve-premises mode) (> (count records) 1))
                 (conj {:warning :preserved-applications-archived
                        :client-id (:client-id block)
                        :index (:index block)
                        :message
                        "Inactive application versions are reloadable closures; automatic reactivation is deferred."}))}))

(defn- environment-source-forms
  [runtime-state mode]
  (let [imports (map (fn [{:keys [file entry revision]}]
                       (list 'load-primitive-environment file entry revision))
                     (:environment/primitive-imports runtime-state))
        blocks (block-model/source-blocks runtime-state)
        projected
        (mapv (fn [block]
                (if (seq (:version-history block))
                  (versioned-block-forms runtime-state block mode)
                  {:forms (->> (block-model/block-text runtime-state block)
                               source/read-source-forms
                               (filter semantic-form?)
                               vec)
                   :warnings []}))
              blocks)]
    {:forms (vec (concat imports (mapcat :forms projected)))
     :warnings (vec (mapcat :warnings projected))}))

(defn- block-source-forms
  [runtime-state client-id mode selection]
  (let [blocks (selected-blocks runtime-state client-id selection)]
    (if (= :source mode)
      (let [sources (vec (keep block-current-source blocks))]
        {:forms (vec (mapcat source/read-source-forms sources))
         :source-text (str (str/join "\n\n" sources) "\n")
         :source-count (count sources)
         :warnings []})
      (let [projected (mapv #(versioned-block-forms runtime-state % mode)
                            blocks)]
        {:forms (vec (mapcat :forms projected))
         :warnings (vec (mapcat :warnings projected))}))))

(defn- forms->text
  [forms]
  (str (str/join "\n\n" (map pr-str forms)) "\n"))

(defn- validate-generated-source
  [runtime-state forms]
  (loop [candidate (assoc (state/empty-state)
                          :environment/primitive-imports
                          (:environment/primitive-imports runtime-state))
         forms forms]
    (if-let [form (first forms)]
      (let [{next-state :state failure :error}
            (compile-hidden-form candidate "save-validation" form :save-validation)]
        (when failure
          (throw (ex-info "generated environment did not recompile" failure)))
        (recur next-state (next forms)))
      true)))

(defn- atomic-write!
  [canonical text]
  (let [target (.toPath canonical)
        parent (or (.getParent target) (.toPath (io/file ".")))
        _ (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))
        temporary (Files/createTempFile parent ".lain-" ".tmp"
                                        (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/write temporary (.getBytes text StandardCharsets/UTF_8)
                   (into-array java.nio.file.OpenOption []))
      (try
        (Files/move temporary target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (catch java.nio.file.AtomicMoveNotSupportedException _
          (Files/move temporary target
                      (into-array StandardCopyOption
                                  [StandardCopyOption/REPLACE_EXISTING]))))
      (finally
        (Files/deleteIfExists temporary)))))

(defn save-state
  [runtime-state {:keys [file mode checkpoint-id client-id selection]
                  :or {selection :all}}]
  (let [canonical (canonical-file file)
        checkpoint [(.getPath canonical) checkpoint-id]
        existing (get-in runtime-state [:environment/checkpoints checkpoint])]
    (if existing
      {:state runtime-state :receipt (assoc existing :status :saved :replayed? true)}
      (let [{:keys [forms warnings source-text source-count]}
            (if client-id
              (block-source-forms runtime-state client-id mode selection)
              (environment-source-forms runtime-state mode))
            _ (when (not= :source mode)
                (validate-generated-source runtime-state forms))
            text (or source-text (forms->text forms))
            _ (atomic-write! canonical text)
            digest (file-digest canonical)
            receipt {:status :saved
                     :canonical-path (.getPath canonical)
                     :digest digest
                     :checkpoint-id checkpoint-id
                     :exported-form-count (or source-count (count forms))
                     :diagnostics warnings}]
        {:state (assoc-in runtime-state
                          [:environment/checkpoints checkpoint]
                          receipt)
         :receipt receipt}))))

(defn perform-request
  "Evaluate one environment boundary request.  `drain-effects` is supplied by
  the boundary driver so nested loads reach equilibrium before the next form."
  [runtime-state request drain-effects]
  (let [payload (:boundary/payload request)]
    (case (:boundary/kind request)
      :environment/load-primitive-environment
      (install-primitive-environment runtime-state payload)

      :environment/load-lain
      (load-lain-state runtime-state payload drain-effects)

      :environment/load-blocks
      (load-blocks-state runtime-state payload drain-effects)

      :environment/save-environment
      (save-state runtime-state payload)

      :environment/save-blocks
      (save-state runtime-state payload)

      (throw (ex-info "unknown environment boundary request"
                      {:request request})))))
