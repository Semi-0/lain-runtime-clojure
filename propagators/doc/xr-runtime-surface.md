# XR Runtime Surface

The XR surface is an experiment in multiple projections over the same
compiler-2 runtime. It does not own truth and does not mutate cells directly.

## Boundary Rule

An XR client may do only two runtime actions:

- extend graph topology by sending compiler/runtime declarations;
- send an ordinary message to an existing cell.

Everything else is local UI state: camera, selected node, force-layout position,
controller state, and pending command UI.

## Runtime Bridge

`graph.xr-server` serves a small browser UI and a WebSocket endpoint. The
server delegates runtime operations to `graph.xr-runtime`, which wraps the
existing compiler-2 runtime session.

Supported XR operations:

- `xr/trace/install` starts a semantic trace projection;
- `xr/trace/read` reads the current projection for an installed trace;
- `xr/extend-graph` appends compiler-2 source to the XR source list and
  recompiles the accumulated program;
- `xr/send-message` sends one value, behavior event, or distributed TMS premise
  fact into a target cell.

The bridge speaks JSON to the browser. Cell ids are serialized as strings, and
semantic graph values are projected into browser-friendly node/edge records.

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

## Web UI

The web side is plain JavaScript modules using The Elm Architecture:

- `model.js` owns plain model data and force-layout state;
- `update.js` is the pure state transition;
- `effects.js` owns WebSocket, animation, and XR session effects;
- `render.js` projects model snapshots into Three.js objects.

The first projection is a normal 3D browser view. WebXR is layered on top after
that view works. Pinch recognition updates selection state first; committed
actions still go back through `xr/extend-graph` or `xr/send-message`.

The browser view also has a mouse test path for IO. Click a node in the 3D
canvas, enter a JSON scalar or string value, and send it to the selected node.
This uses the same `xr/send-message` runtime command as XR gestures, so message
passing can be tested without a headset.

The desktop 3D view remains available even when XR is unsupported. It renders
cells as white illuminated spheres and propagators/operators as white
illuminated triangles on a black background. Mouse controls in 3D mode are:
left-drag to rotate, wheel to zoom, and right/middle/shift-drag to pan. The
`3D View` / `XR View` toggle changes presentation mode without changing the
runtime graph model. When a cell appears or receives a changed value in a graph
snapshot, it briefly blooms with a white halo and then fades back to normal.
On graph load, the camera frames the current graph center; once the user moves
the camera, manual control takes over. Labels are plain white text without
background rectangles or glow.

Run the prototype with:

```text
clojure -M -m graph.xr-server
```

or the shortcut:

```text
clojure -M:wired/xr
```

Then open `http://127.0.0.1:45666/`.
