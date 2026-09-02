# propagators-runtime

Presentation-independent sessions, commands, program loading, replay,
boundaries, clocks, version history, tracing, and runtime inspection for the
propagator system.

## Public API

```clojure
(require '[propagators.runtime.api :as runtime])

(runtime/create-session options)
(runtime/load-program session source)
(runtime/dispatch-command session command)
(runtime/inspect session request)
```

Semantic graph inspection returns stable data with schema version 1, nodes,
edges, and runtime aliases. Rendering, servers, Charm, web, and XR integration
belong to `propagators-tui`.

## Verify

```bash
clojure -M:test
```

Committed sibling dependencies use pinned Git SHAs. The Repo workspace may
provide local-root overrides without changing this file.
