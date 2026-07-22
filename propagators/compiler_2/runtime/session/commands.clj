(ns propagators.compiler-2.runtime.session.commands
  "Public command dispatch for compiler-2 runtime."
  (:require [propagators.compiler-2.runtime.inspection.cells :as cells]
            [propagators.compiler-2.runtime.session.clock :as clock]
            [propagators.compiler-2.runtime.session.input :as input]
            [propagators.compiler-2.runtime.session.instance-replay :as instance-replay]
            [propagators.compiler-2.runtime.session.program :as program]
            [propagators.compiler-2.runtime.session.state :as state]
            [propagators.compiler-2.runtime.inspection.temperature :as temperature]
            [propagators.compiler-2.runtime.inspection.trace.session :as trace-session]
            [propagators.compiler-2.runtime.inspection.trace.subscriptions :as trace-subscriptions]
            [propagators.compiler-2.runtime.tui.session :as tui-session]
            [propagators.compiler-2.runtime.tui.versioned-commit :as versioned-commit]
            [propagators.compiler-2.runtime.bridge.xr-projection :as xr-projection]))

(def require-state state/require-state)
(def register-tui! tui-session/register-tui!)
(def append-tui-block! tui-session/append-tui-block!)
(def edit-tui-block! tui-session/edit-tui-block!)
(def submit-tui-block! tui-session/submit-tui-block!)
(def read-tui-view tui-session/read-tui-view)
(def unregister-tui! tui-session/unregister-tui!)
(def commit-version! versioned-commit/commit-version!)
(def read-agent-blocks tui-session/read-agent-blocks)
(def read-agent-block tui-session/read-agent-block)
(def send-agent-block! tui-session/send-agent-block!)
(def focus-block! tui-session/focus-block!)
(def compile-source! program/compile-source!)
(def list-cells cells/list-cells)
(def read-cell cells/read-cell)
(def semantic-trace trace-session/semantic-trace)
(def semantic-expansion trace-session/semantic-expansion)
(def install-semantic-trace! trace-session/install-semantic-trace!)
(def read-installed-trace trace-session/read-installed-trace)
(def stop-installed-trace! trace-session/stop-installed-trace!)
(def schedule-trace-refreshes! trace-subscriptions/schedule-refreshes!)
(def read-xr-effects xr-projection/read-xr-effects)
(def summarize-temperature temperature/summarize)
(def export-instance instance-replay/export-instance)
(def import-instance! instance-replay/import-instance!)

(defn handle-command!
  [session {:keys [op source] :as command}]
  (try
    (let [response (locking session
                     {:ok true
                      :result
                      (case op
                        :tui/register (register-tui! session command)
                        :tui/append-block (append-tui-block! session command)
                        :tui/edit-block (edit-tui-block! session command)
                        :tui/submit-block (submit-tui-block! session command)
                        :tui/commit-version (commit-version! session command)
                        :tui/read-view (read-tui-view @session command)
                        :tui/focus-block (focus-block! session command)
                        :tui/unregister (unregister-tui! session command)
                        :instance/export (export-instance @session)
                        :instance/import (import-instance! session
                                                          (:manifest command))
                        :agent/blocks (read-agent-blocks @session command)
                        :agent/block (read-agent-block @session command)
                        :agent/send-block (send-agent-block! session command)
                        :compile/source (compile-source! session source)
                        :cells/list (list-cells @session)
                        :cell/read (read-cell @session command)
                        :semantic/graph (:graph (require-state @session))
                        :semantic/trace (semantic-trace @session command)
                        :semantic/expand (semantic-expansion @session command)
                        :semantic/trace/install (install-semantic-trace! session command)
                        :semantic/trace/read (read-installed-trace @session command)
                        :semantic/trace/stop (stop-installed-trace! session command)
                        :xr/effects (read-xr-effects @session)
                        :runtime/temperature
                        (summarize-temperature
                         (get-in @session [:runtime :temperature :samples]))
                        (throw (ex-info "unknown runtime op" {:op op})))})]
      (schedule-trace-refreshes! session)
      (clock/schedule-subscriptions! session)
      response)
    (catch Throwable t
      ;; Import is transactional, including its diagnostic state.
      (when-not (= :instance/import op)
        (state/record-runtime-error! session {:op op} t))
      {:ok false
       :error (ex-message t)
       :data (ex-data t)})))
