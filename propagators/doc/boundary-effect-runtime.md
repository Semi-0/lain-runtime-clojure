# Boundary Effect Runtime

This note extends the core runtime model with a strict boundary between
propagation and communication with external environments such as TUI, XR, trace
renderers, sockets, files, or other clients.

## Rule

Propagation is pure with respect to the outside world.

An effectful propagator must not send data to a socket, mutate a UI, write a
file, or call an external server during activation. It may only add ordinary
cell content that describes a requested boundary effect.

The host runtime is responsible for pulling those requests during an explicit
effect phase.

## Three Runtime Periods

The runtime should run in repeated transactions with three stages:

1. **Commit**

   External input is converted into internal runtime data:

   - graph extension requests become compiler/runtime declarations;
   - UI/XR gestures become cell messages;
   - inbound socket messages become ordinary messages or declarations;
   - no propagator is run yet.

2. **Propagate**

   The scheduler drains propagator tasks until quiescence.

   During this phase propagators may:

   - emit messages to cells;
   - extend internal topology through bounded kernel declaration effects;
   - write boundary-effect requests into designated effect cells or outbox
     stores.

   During this phase propagators must not:

   - talk to XR/TUI/socket clients directly;
   - perform host IO;
   - observe mutable external state.

3. **Effect**

   The host runtime reads boundary-effect content and performs external
   communication.

   Examples:

   - send a trace graph snapshot to XR;
   - update a TUI view;
   - push a WebSocket message;
   - write a diagnostic event.

   Inbound events produced by the outside world during this period are queued
   as the next commit, not merged directly into the currently propagating
   network.

## Effect Content

Boundary effects should be ordinary monotone cell content. A minimal effect
request shape is:

```clojure
{:boundary/effect true
 :boundary/id <stable-effect-id>
 :boundary/port <port-id>
 :boundary/kind <kind>
 :boundary/payload <data>
 :boundary/epoch <monotone-epoch-or-sequence>}
```

`boundary/id` makes the effect idempotent from the runtime's perspective.
`boundary/port` names the external projection, such as `:xr`, `:tui`, or
`:trace`. `boundary/payload` is data only; host callbacks and socket handles are
not stored in cells.

The runtime may keep external acknowledgement state outside the propagator
network. If acknowledgements need to participate in propagation, they must be
converted into ordinary messages during a later commit stage.

## XR And TUI

XR and TUI should use the same boundary model.

Both clients may only request:

- graph extension;
- cell messages.

Both clients receive output only through boundary-effect pulls:

- a trace propagator writes a graph snapshot request to an effect cell;
- an XR/TUI runtime effect handler reads that request and sends it to the
  client;
- the client's later input is queued as a new commit.

This keeps UI projections universal. XR is not special because it is 3D, and
TUI is not special because it is text. They are both external projections over
the same runtime.

## Current XR Runtime Slice

The compiler-2 live runtime currently implements the first XR boundary-effect
slice with `xr-io`:

1. commit: a TUI/runtime block containing `(xr-io traced receipt)` is compiled;
2. propagate: `xr-io` writes `:xr/launch-trace` requests into a hidden XR
   outbox cell;
3. effect: the host runtime reads the outbox, collapses multiple same-epoch
   trace-growth requests to the largest graph, records the launched XR effect,
   and writes a delivered receipt into the requested receipt cell.

The receipt is ordinary cell content, represented as compound-object slots.
Downstream receipt-dependent propagation is still a follow-up: today the
receipt write is visible for inspection, while full acknowledgement propagation
should be modeled as a later commit.

## Relationship To Existing Kernel Effects

`propagators.core/eval-activation-result` currently supports activation returns
with `:messages` and `:effects`. Those effects are kernel declaration effects:
they install cells, install propagators, or bind names inside the network.

Boundary effects are different:

- kernel effects change the internal network and may be applied during
  propagation;
- boundary effects describe external communication and must be pulled by the
  host runtime after propagation quiesces.

Do not overload the existing kernel-effect path for XR/TUI/socket IO.

## Target API Shape

The eventual host API should look like:

```clojure
(commit runtime external-events)
(propagate runtime)
(perform-effects runtime effect-handlers)
```

or as one transaction:

```clojure
(step runtime external-events effect-handlers)
```

`step` returns the updated runtime and any host-level delivery results. It does
not hide the three periods; the separation is the semantic contract.

## Migration Path

1. Keep current scheduler behavior unchanged.
2. Add a boundary-effect datastructure and outbox projection helper.
3. Refactor tracing, TUI pushes, and XR pushes to produce/read boundary effects.
4. Move current direct runtime pushes behind effect handlers.
5. Add tests proving an effectful propagator activation only changes cell
   content during propagation, and that the external send occurs only in the
   effect phase.
