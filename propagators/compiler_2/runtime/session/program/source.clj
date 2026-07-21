(ns propagators.compiler-2.runtime.session.program.source
  "Compiler source normalization helpers for compiler-2 runtime."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [propagators.compiler-2.runtime.tui.block-model :as block-model]
            [propagators.compiler-2.language.parser :as compiler-parser])
  (:import [java.io PushbackReader StringReader]))

(def block-by-index block-model/block-by-index)

(defn top-level-form-head
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (when (seq? form)
        (first form)))
    (catch Throwable _
      nil)))

(def ^:private declaration-heads
  '#{def def-cell def-cells def-net def-constraint
     def-behavior def-behaviour def-behaviors def-behaviours
     define-behaviors define-behaviours behavior behavior-cell})

(def ^:private boundary-effect-heads
  '#{be:block translate xr-io io:xr
     slider-io slider-panel-io io:slider io:slider-panel
     io:slider-panels io:slider-panel-name
     load-primitive-environment load-lain save-environment
     load-blocks save-blocks})

(def ^:private transport-heads '#{-> <->})

(defn- source-form [source]
  (try
    (compiler-parser/read-form source)
    (catch Throwable _ nil)))

(defn top-level-declaration? [source]
  (contains? declaration-heads (top-level-form-head source)))

(defn explicit-output-form?
  "True when a form deliberately targets an external/runtime destination.

  `->` and `<->` are ordinary sync expressions, not declarations.  Their
  destination is nevertheless explicit, so the TUI must not add a second
  implicit next-block destination."
  [source]
  (let [form (source-form source)
        head (when (seq? form) (first form))]
    (or (contains? boundary-effect-heads head)
        (and (contains? '#{block-at be:block-at} head)
             (>= (count form) 4))
        (contains? transport-heads head))))

(defn trace-source? [source]
  (= 'trace (top-level-form-head source)))

(def ^:private source-reader-eof (Object.))

(defn read-source-forms
  [source]
  (let [source (str/replace source
                            #"\(\s*::(?=\s)"
                            (str "(" compiler-parser/network-marker))
        source (str/replace source #"(?<=\(|\s)be:/" "be:divide")
        reader (PushbackReader. (StringReader. source))]
    (loop [forms []]
      (let [form (edn/read {:eof source-reader-eof} reader)]
        (if (identical? source-reader-eof form)
          (do
            (when-not (seq forms)
              (throw (ex-info "empty compiler-2 source" {:source source})))
            forms)
          (recur (conj forms form)))))))

(defn trace-form? [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (boolean
       (some (fn [x]
               (and (seq? x)
                    (= 'trace (first x))))
             (tree-seq coll? seq form))))
    (catch Throwable _
      false)))

(defn normalize-trace-source
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (pr-str
       (walk/postwalk
        (fn [form]
          (if (and (seq? form)
                   (= 'trace (first form))
                   (symbol? (second form)))
            (cons 'trace
                  (cons (list 'trace-target
                              (name (second form))
                              (second form))
                        (nnext form)))
            form))
        form)))
    (catch Throwable _
      source)))

(defn auto-output-display-form
  [_source target-index]
  (format "(be:block-at %% %d __runtime_out)" target-index))

(defn auto-output-source [state block source]
  (if (or (top-level-declaration? source)
          (explicit-output-form? source)
          (nil? (block-by-index state (:client-id block) (inc (:index block)))))
    source
    (let [target-index (inc (:index block))]
      (format "(let-cell [__runtime_out]
                 (-> %s __runtime_out)
                 %s
                 __runtime_out)"
              source
              (auto-output-display-form source target-index)))))
