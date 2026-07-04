# XR Runtime Surface

The XR surface is an experiment in multiple projections over the same
compiler-2 runtime. It does not own truth and does not mutate cells directly.

## Boundary Rule

An XR client may do only two normal runtime actions:

- extend graph topology by sending compiler/runtime declarations;
- send widget events to compiler-declared widget IO propagators.

Everything else is local UI state: camera, selected node, force-layout position,
controller state, and pending command UI.

`xr/send-message` still exists as a compatibility/testing command, but the web
panel no longer exposes it as the normal interaction path. Browser-originated
behavior input should go through widget IO so compiler/runtime code declares
which cells may receive external events.

## Runtime Bridge

`graph.xr-server` serves a small browser UI and a WebSocket endpoint. The
server delegates runtime operations to `graph.xr-runtime`, which wraps the
existing compiler-2 runtime session.

Supported XR operations:

- `xr/trace/install` starts a semantic trace projection;
- `xr/trace/read` reads the current projection for an installed trace;
- `xr/extend-graph` appends compiler-2 source to the XR source list and
  recompiles the accumulated program;
- `xr/widget-event` sends `{widget-id, channel, value}` to a registered widget
  IO source; the runtime assigns epochs and injects behavior events;
- `xr/send-message` sends one value, behavior event, or distributed TMS premise
  fact into a target cell as a compatibility path.

The bridge speaks JSON to the browser. Cell ids are serialized as strings, and
semantic graph values are projected into browser-friendly node/edge records.

## Known Fragility And Projection Boundary

The propagation scheduler is synchronous and intentionally small: a commit
merges cell messages, wakes downstream propagators, and drains the task queue.
The fragile part is the runtime/effect/projection layer around it. `xr-io` is an
effect/output path, not an XR input commit path; it emits an XR launch request
after propagation. If that request carries large trace values, the TUI command
that caused it can appear stuck while runtime projection or JSON delivery runs.

Trace topology must not carry raw runtime cell content. Cell content is merge
evidence and may contain behavior history, TMS facts, closures, or network-like
values. XR and TUI projections may expose only strongest-derived lightweight
summaries. In short:

```text
cell content  -> internal merge/evidence substrate
cell strongest -> readable current truth
XR/TUI value   -> projection-safe summary of strongest
```

The target runtime cycle is:

```text
commit external input -> propagate -> collect boundary effects -> project TUI/XR
```

Both TUI and XR should consume that completed transaction result, including
explicit changed cell/node ids for UI pulses. Browser glow must not depend on
diffing serialized raw cell values.

## Compiler-2 Runtime Operator

The live compiler-2 runtime also binds `xr-io` as a runtime-only operator:

```clojure
(let-cell [g]
  (trace a g)
  (xr-io g receipt)
  receipt)
```

`xr-io` is intentionally not direct socket IO. During propagation it writes a
boundary-effect request into the runtime XR outbox. After propagation
quiesces, the runtime effect period reads the outbox, schedules one
`:xr/launch-trace` delivery per receipt/epoch, and writes a receipt fact back
to the requested receipt cell.

The receipt cell is a compound object keyed by stable receipt slots. This keeps
multiple launches monotone instead of overwriting a plain value.

Runtime clients can inspect delivered XR requests with:

```clojure
{:op :xr/effects}
```

The receipt may be an ordinary expression/local output:

```clojure
(let-cell [g r]
  (trace out g)
  (xr-io g r)
  r)
```

or a runtime-visible cell binding:

```clojure
(def receipt)
(let-cell [g]
  (trace out g)
  (xr-io g receipt)
  receipt)
```

## Web UI

The web side is plain JavaScript modules using The Elm Architecture:

- `model.js` owns plain model data and force-layout state;
- `update.js` is the pure state transition;
- `effects.js` owns WebSocket, animation, and XR session effects;
- `render.js` projects model snapshots into Three.js objects.

The first projection is a normal 3D browser view. WebXR is layered on top after
that view works. Pinch recognition updates selection state first; committed
actions still go back through `xr/extend-graph` or `xr/widget-event`.

The browser view also has a mouse test path for IO. Click a node in the 3D
canvas to select it. Compiler-declared widget nodes render as sliders in the 3D
scene and in the side panel. Dragging a slider sends `xr/widget-event`, not a
raw cell mutation, so message passing can be tested without a headset while
still respecting the widget IO boundary.

## Widget IO

The live compiler-2 runtime binds two XR widget operators:

```clojure
(slider-io "gain" gain gain-events)

(slider-panel-io "mix"
  "a" a a-events
  "b" b b-events
  "c" c c-events)
```

`slider-io` registers one channel named `value`. `slider-panel-io` registers a
multi-channel panel. Each channel has a view cell, used for display/feedback,
and an event-source cell, used for external input. During propagation these
operators emit `:xr/widget-register` boundary effects; the runtime effect stage
records a widget registry and augments the semantic graph with widget metadata.

The browser may only send a widget id, channel, and value. The runtime resolves
that pair through the registry, assigns the next widget epoch, and writes
ordinary behavior event-source updates. For a slider panel, every channel edit
re-emits the latest known values for all channels at the same epoch. That keeps
multi-input behavior arithmetic fresh without changing behavior arithmetic's
existing sparse-history rule that point events do not automatically continue.

The desktop 3D view remains available even when XR is unsupported. It renders
cells as white illuminated spheres and propagators/operators as white
illuminated triangles on a black background. Camera controls in 3D mode use
Three.js `OrbitControls`: left-drag rotates, wheel zooms, and right-drag pans.
The `3D View` / `XR View` toggle changes presentation mode without changing the
runtime graph model. When a cell appears or receives a changed value in a graph
snapshot, it briefly blooms with a white halo and then fades back to normal.
On graph load, the camera frames the current graph center; once the user moves
the camera, manual control takes over. Labels are plain white text without
background rectangles or glow.

Run the integrated runtime with:

```text
clojure -M:wired/server
```

When compiler-2 code installs an `xr-io` propagator and it emits an XR launch
effect, the runtime server starts an XR/browser projection server against the
same runtime session. Connected browser clients subscribe to XR launch effects
and receive the latest launch graph immediately, then receive updated launch
graphs as the runtime changes. Run only the standalone XR projection with:

```text
clojure -M -m graph.xr-server
```

or the shortcut:

```text
clojure -M:wired/xr
```

Then open `http://127.0.0.1:45666/`.

## Future Session Boundary

The current lazy launch path deliberately shares the compiler-2 runtime session
with the XR projection. That is the smallest useful prototype because the XR UI
can immediately inspect the same cells and semantic traces as the TUI.

Longer term, XR should probably run as its own runtime session connected by an
explicit boundary protocol. In that shape, `xr-io` would still emit a boundary
effect from the compiler runtime, but the server would launch or address a
separate XR session and pass only projection facts, receipts, and user commands
across the boundary. That would make ownership clearer:

- compiler-2 runtime owns propagation truth and effect scheduling;
- XR runtime owns 3D/WebXR state, layout, controller state, and view-local
  interaction state;
- communication between them is only graph extension requests, cell messages,
  projection snapshots, and receipts.

This separation should make multi-view experiments safer: XR can restart,
fork, replay, or hold local projection state without mutating or depending on
the compiler runtime's internal session map.
