(ns propagators.runtime.session.file-loader
  "Pure .lain file loading helpers for compiler-2 runtime sessions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.program.source :as source]))

(def default-client-id "file")

(defn- top-level-form-list?
  [form]
  (and (seq? form)
       (seq form)
       (every? seq? form)))

(defn source-forms
  "Read .lain source as compiler-2 top-level forms.

  Files may contain either normal consecutive top-level forms or one outer list
  containing those forms.
  "
  [text]
  (let [forms (source/read-source-forms text)]
    (if (and (= 1 (count forms))
             (top-level-form-list? (first forms)))
      (vec (first forms))
      forms)))

(defn normalized-source
  [text]
  (->> (source-forms text)
       (map pr-str)
       (str/join "\n")))

(defn read-file-source
  [file]
  (slurp (io/file file)))

(defn load-source!
  ([session text] (load-source! session text {}))
  ([session text {:keys [client-id]
                  :or {client-id default-client-id}}]
   (let [source (normalized-source text)
         load-result (runtime/extend-source! session {:source source
                                                      :client-id client-id})]
     (runtime/refresh-trace-subscriptions! session)
     {:session session
      :client-id client-id
      :source source
      :load-result load-result})))

(defn load-file!
  ([session file] (load-file! session file {}))
  ([session file opts]
   (assoc (load-source! session (read-file-source file) opts)
          :file (str (io/file file)))))

(defn compile-file!
  ([session file] (compile-file! session file {}))
  ([session file opts]
   (load-file! session file opts)))

(defn load-session-from-source
  ([text] (load-session-from-source text {}))
  ([text opts]
   (let [session (runtime/new-session)]
     (load-source! session text opts)
     session)))

(defn load-session-from-file
  ([file] (load-session-from-file file {}))
  ([file opts]
   (let [session (runtime/new-session)]
     (load-file! session file opts)
     session)))

(defn compile-file
  ([file] (compile-file file {}))
  ([file opts]
   (load-session-from-file file opts)))

(defn load-server-instance-from-source
  ([text] (load-server-instance-from-source text {}))
  ([text opts]
   (let [session (runtime/new-session)
         loaded (load-source! session text opts)]
     {:session session
      :loaded loaded})))

(defn load-server-instance-from-file
  ([file] (load-server-instance-from-file file {}))
  ([file opts]
   (let [session (runtime/new-session)
         loaded (load-file! session file opts)]
     {:session session
      :loaded loaded})))

(defn load-server-instance
  ([file] (load-server-instance file {}))
  ([file opts]
   (load-server-instance-from-file file opts)))
