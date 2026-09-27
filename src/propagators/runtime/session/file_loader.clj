(ns propagators.runtime.session.file-loader
  "Pure .lain file loading helpers for compiler-2 runtime sessions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [propagators.runtime :as runtime]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.runtime.session.program.source :as source]))

(def default-client-id "file")
(def default-watch-interval-ms 250)

(defn- install-extensions!
  [session extensions]
  (doseq [extension extensions]
    (runtime/install-session-extension!
     session
     extension
     {:outbox-id (runtime-ids/boundary-outbox-id)})))

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
     ;; Deterministic topology IDs can repeat after a complete replacement.
     ;; A public runtime generation distinguishes commands from the old session.
     (swap! session
            (fn [state]
              (if (:environment/generation state)
                state
                (assoc state :environment/generation (str (java.util.UUID/randomUUID))))))
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
     (install-extensions! session (:extensions opts))
     (load-source! session text opts)
     session)))

(defn load-session-from-file
  ([file] (load-session-from-file file {}))
  ([file opts]
   (let [session (runtime/new-session)]
     (install-extensions! session (:extensions opts))
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
         _ (install-extensions! session (:extensions opts))
         loaded (load-source! session text opts)]
     {:session session
      :loaded loaded})))

(defn load-server-instance-from-file
  ([file] (load-server-instance-from-file file {}))
  ([file opts]
   (let [session (runtime/new-session)
         _ (install-extensions! session (:extensions opts))
         loaded (load-file! session file opts)]
     {:session session
      :loaded loaded})))

(defn load-server-instance
  ([file] (load-server-instance file {}))
  ([file opts]
   (load-server-instance-from-file file opts)))

(defn- stop-session-resources!
  [session]
  (runtime/stop-clocks! session)
  (doseq [{:keys [stop]} (vals (:traces @session))]
    (when stop
      (stop)))
  nil)

(defn replace-session-from-source!
  "Compile source in a fresh session and atomically promote it into `session`.

  A failed compilation leaves the live session untouched. Runtime resources
  created for the candidate are always stopped; declarative clock
  subscriptions are restarted against the live session after promotion.
  "
  ([session text] (replace-session-from-source! session text {}))
  ([session text opts]
   (let [candidate (runtime/new-session (:runtime/options @session))]
     (try
       (install-extensions! candidate (:extensions opts))
       (let [loaded (load-source! candidate text opts)
             candidate-state @candidate]
         (stop-session-resources! candidate)
         (locking session
           (let [previous-state @session]
             (stop-session-resources! session)
             (try
               (reset! session candidate-state)
               (runtime/schedule-clock-subscriptions! session)
               (catch Throwable t
                 (runtime/stop-clocks! session)
                 (reset! session previous-state)
                 (runtime/schedule-clock-subscriptions! session)
                 (throw t)))))
         (assoc loaded :session session))
       (catch Throwable t
         (stop-session-resources! candidate)
         (throw t))))))

(defn replace-session-from-file!
  ([session file] (replace-session-from-file! session file {}))
  ([session file opts]
   (assoc (replace-session-from-source! session (read-file-source file) opts)
          :file (str (io/file file)))))

(defn watch-file!
  "Poll a .lain file and replace the complete live environment after each
  successful change. A failed reload is reported through `on-error` and the
  last good environment remains live. Returns a closeable watcher map.
  "
  ([session file] (watch-file! session file {}))
  ([session file {:keys [interval-ms on-reload on-error]
                  :or {interval-ms default-watch-interval-ms
                       on-reload (fn [_] nil)
                       on-error (fn [_] nil)}
                  :as opts}]
   (let [file (io/file file)
         running? (atom true)
         last-source (atom (read-file-source file))
         loader-opts (dissoc opts :interval-ms :on-reload :on-error)
         thread (Thread.
                 (fn []
                   (while @running?
                     (try
                       (Thread/sleep (long interval-ms))
                       (let [source (read-file-source file)]
                         (when-not (= source @last-source)
                           (reset! last-source source)
                           (try
                             (let [loaded (replace-session-from-source!
                                           session source loader-opts)]
                               (on-reload (assoc loaded :file (str file))))
                             (catch Throwable t
                               (on-error t)))))
                       (catch InterruptedException _ nil)
                       (catch Throwable t
                         (on-error t)))))
                 "compiler-2-file-watch")]
     (.setDaemon thread true)
     (.start thread)
     {:file (str file)
      :close (fn []
               (reset! running? false)
               (.interrupt thread))})))
