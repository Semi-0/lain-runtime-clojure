(ns propagators.compiler-2.runtime
  "Shared compiler-2 runtime session for socket clients."
  (:require [propagators.compiler-2.runtime.inspection.cells :as cells]
            [propagators.compiler-2.runtime.session.commands :as commands]
            [propagators.compiler-2.runtime.session.clock :as clock]
            [propagators.compiler-2.runtime.boundary.effects :as effects]
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

(def new-session state/new-session)
(def ensure-session-state! state/ensure-session-state!)
(def record-runtime-error! state/record-runtime-error!)
(def default-xr-client-id state/default-xr-client-id)
(def compile-source! program/compile-source!)
(def list-cells cells/list-cells)
(def read-cell cells/read-cell)
(def run-runtime-cycle effects/run-runtime-cycle)
(def commit-runtime-input input/commit-runtime-input)
(def commit-runtime-input! input/commit-runtime-input!)
(def extend-source! input/extend-source!)
(def semantic-trace trace-session/semantic-trace)
(def semantic-expansion trace-session/semantic-expansion)
(def install-semantic-trace! trace-session/install-semantic-trace!)
(def read-installed-trace trace-session/read-installed-trace)
(def stop-installed-trace! trace-session/stop-installed-trace!)
(def schedule-trace-refreshes! trace-subscriptions/schedule-refreshes!)
(def refresh-trace-subscriptions! trace-subscriptions/refresh-now!)
(def summarize-temperature temperature/summarize)
(def drain-temperature! temperature/drain-summary!)
(def register-tui! tui-session/register-tui!)
(def commit-version! versioned-commit/commit-version!)
(def export-instance instance-replay/export-instance)
(def import-instance! instance-replay/import-instance!)
(def schedule-clock-subscriptions! clock/schedule-subscriptions!)
(def stop-clocks! clock/stop-all!)

(defn- with-trace-refresh!
  [session result]
  (refresh-trace-subscriptions! session)
  result)

(defn append-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/append-tui-block! session command)))

(defn edit-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/edit-tui-block! session command)))

(defn submit-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/submit-tui-block! session command)))

(def project-tui-view tui-session/project-tui-view)
(def read-tui-view tui-session/read-tui-view)
(def read-agent-blocks tui-session/read-agent-blocks)
(def read-agent-block tui-session/read-agent-block)
(def send-agent-block! tui-session/send-agent-block!)
(def focus-block! tui-session/focus-block!)
(def unregister-tui! tui-session/unregister-tui!)
(def project-xr-effects xr-projection/project-xr-effects)
(def read-xr-effects xr-projection/read-xr-effects)
(def handle-command! commands/handle-command!)
