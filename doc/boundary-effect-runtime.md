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

`propagators.infra.core/eval-activation-result` currently supports activation returns
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

## Commit And Effect Are Declarations

Both commits and boundary effects should be data. A propagator declares an
intention; the runtime chooses when and how to evaluate it.

```clojure
{:runtime/op :commit
 :commit/target target-id
 :commit/value value}

{:runtime/op :effect
 :effect/id stable-id
 :effect/port :trace
 :effect/kind :trace/compute
 :effect/payload request
 :effect/result {:commit/target target-id}}
```

A commit is not an immediate cell mutation. It means "merge this value into
this cell at the beginning of the next runtime round." An effect is not an IO
callback stored in the network. It is a data-only request interpreted by a host
handler. If that handler produces an internal result, the result returns as a
commit for a later round.

Keeping handlers outside the declaration makes the boundary locally testable:

- propagator tests assert the declared commit or effect without performing IO;
- runtime tests supply small handler functions and assert the resulting commits;
- integration tests exercise real TUI, XR, trace, socket, or file handlers.

Do not add commit/effect queues to `Net [graph env dict]` initially. `Net`
describes the declarative propagator network. Queue position, runtime phase,
in-flight IO, and round number describe an execution of that network. The
smallest additive execution value is therefore:

```clojure
{:net network
 :tasks task-queue
 :commits commit-queue
 :effects effect-queue
 :in-flight {}
 :round 0
 :phase :idle}
```

If later evidence shows that subnets must retain independent IO mailboxes, this
execution value can itself become the primitive running-net value. That need
does not require changing the existing `Net` constructor now.

## Runtime Fixed Point

The runtime must reach a fixed point across tasks, commits, and synchronous
effects, not merely drain the current propagator task queue.

```text
commit batch
    |
    v
propagate until the task queue is empty
    |
    v
freeze and interpret the resulting effect batch
    |
    v
queue effect results as next-round commits
    |
    +---- repeat while work remains
```

The minimal driver can be assembled from phase functions:

```clojure
(defn run-until-idle
  [machine handlers]
  (loop [machine machine]
    (cond
      (seq (:commits machine))
      (recur (apply-commit-round machine))

      (seq (:tasks machine))
      (recur (propagate-round machine))

      (seq (:effects machine))
      (recur (perform-effect-round machine handlers))

      :else
      (assoc machine :phase :idle))))
```

Each phase consumes a frozen batch. Work produced by that batch belongs to the
following phase or round; it is never injected into the phase currently being
iterated. External input may be accepted into a mailbox while the runtime is
busy, but it is not committed until the current transaction becomes idle.

Three states must remain distinct:

- **propagation equilibrium**: the propagator task queue is empty;
- **runtime idle**: tasks, commits, and synchronous effects are empty;
- **runtime waiting**: locally idle, but one or more asynchronous effects are
  still in flight.

An asynchronous trace or socket handler must not block `run-until-idle`. It
records an in-flight effect and returns. Its later completion enqueues a commit
and wakes a new serialized runtime round. A new user input may already be in the
mailbox, but the runtime applies commit batches in its defined order rather than
mutating the network from the callback thread.

## First Slice: Trace Results Commit To The Runtime

The first implementation should introduce only declarative commits. It should
not introduce the general effect evaluator, commit queue, fixed-point driver, or
change the existing boundary outbox.

The ownership rule is:

```text
trace worker computes a result
  -> tracer validates freshness and declares a commit
  -> runtime evaluates the commit
  -> runtime propagates to equilibrium
  -> runtime performs today's boundary effects
  -> tracer records trace-domain result metadata
```

The trace-result declaration is data only:

```clojure
(defn trace-result-commit
  [subscription-id target-id epoch trace-update]
  {:runtime/op :commit
   :commit/kind :effect-result
   :commit/id [:trace-result subscription-id epoch]
   :commit/source [:trace subscription-id]
   :commit/epoch epoch
   :commit/updates [{:cell-id target-id
                     :update trace-update}]})
```

The tracer owns only trace intention and policy:

- compute the semantic graph;
- reject results from removed subscriptions;
- reject stale epochs;
- suppress or count unchanged results;
- construct `trace-result-commit`;
- record `:trace/results` and trace counters after successful commit evaluation.

The tracer must not call `eval-cells`, run task queues, settle compiler
applications, or perform boundary effects. Those are runtime evaluation
strategies.

The runtime owns commit evaluation:

```clojure
(defn apply-commit
  [state commit]
  (-> state
      (apply-program-updates (:commit/updates commit))
      perform-boundary-effects
      (record-runtime-commit commit)))
```

`apply-program-updates` remains the existing composition of cell-message merge,
task draining, and the temporary application-settling compatibility pass. The
important change is ownership: every producer submits update data to one runtime
function instead of reproducing scheduling steps.

The whole trace publication remains atomic under the existing session lock:

```clojure
(mutate-session!
 session
 (fn [state]
   (let [commit (trace-result-commit subscription-id
                                     target-id
                                     epoch
                                     trace-update)]
     (-> state
         (runtime/apply-commit commit)
         (record-trace-result subscription-id epoch trace-update))))))
```

This composition keeps runtime mechanics out of the tracer without forcing the
runtime to understand trace-specific result maps and counters.

### External Input And Effect Result Are Different Commits

The existing `commit-runtime-input` must not be reused unchanged for trace
results. It represents new information from outside the runtime and therefore:

- advances `:runtime/commit-tick`;
- enters the external-input replay log;
- invalidates or refreshes computations that depend on external input.

A trace result is the completion of work already caused by an earlier runtime
epoch. Its commit should:

- merge into its target through the ordinary scheduler;
- run every downstream propagator to equilibrium;
- perform the resulting boundary effects;
- retain its source effect and epoch for diagnostics and idempotence;
- not enter the external-input replay log;
- not advance the source-input epoch;
- not invalidate the subscription that produced it.

If a trace result were treated as fresh external input, advancing the commit
tick could make the same subscription refresh again. The next result would
advance it again, creating a self-sustaining trace-result loop even when the
semantic graph is unchanged.

The initial API should therefore expose intention-specific constructors over a
shared evaluator:

```clojure
(external-input-commit input)
(effect-result-commit source epoch updates)
(apply-commit state commit)
(commit! session commit)
```

These are functions over maps, not a new protocol or datatype hierarchy.
`commit!` provides serialized session mutation; `apply-commit` evaluates one
already-declared commit. A queue can be added later without changing commit
producers.

### First-Slice Completion Criteria

This slice is complete when:

1. `trace-subscriptions` contains no references to `core/eval-cells`, task
   queues, application settling, or boundary-effect execution.
2. A trace worker returns a data-only commit containing a stable ID, source,
   epoch, target, and update.
3. The runtime is the only layer that merges the target update and drains its
   downstream tasks.
4. Trace result publication remains serialized by `mutate-session!`.
5. A trace-result commit does not change the external-input tick or replay log.
6. Publishing an unchanged trace result cannot schedule itself indefinitely.
7. The focused runtime-server suite remains at 330 assertions with no failures
   or errors.

## Runtime-Server Regression: One Boundary Error, Many Assertions

The compiler-2 runtime-server regression is useful evidence for this model. It
is not a collection of approximately fifty independent semantic mistakes.
Many assertions observe the same broken transition at different UI and trace
surfaces.

The historical comparison is:

- before retained application frames, the focused runtime-server suite passed;
- the regression first appears with the retained-application migration, before
  the later CPS compiler and namespace organization;
- the initial symptom was 55 failed assertions and 2 errors;
- selecting ordinary application explicitly for the live runtime removes both
  errors and 32 failures while preserving retained application as the compiler
  default;
- the remaining 23 failures are trace/display propagation observations.

The first correction belongs at the runtime/compiler boundary: compiler-2 keeps
retained application as its default, while the live runtime explicitly supplies
its ordinary application installer. A runtime compiler wrapper preserves that
selection through delayed closure and watch compilation. This is a declaration
strategy selection error, not an IO-round error.

The remaining trace failure has this verified shape:

```text
trace subscription is registered
  -> semantic trace graph is computed correctly
  -> graph behavior is present in the target program cell
  -> downstream blank TUI display cell still contains nothing
```

The graph algorithm and the expected trace values are therefore not the
failure. The missing transition is publication of an asynchronous result back
through ordinary propagation.

The regressed `trace-subscriptions/publish-result-state` replaced the target
cell with `assoc-net-cell`, then invoked an application-specific settling
helper. Direct replacement changed storage but did not express a cell update to
the scheduler. Consequently, downstream topology such as `be:block-at` was not
guaranteed to run, so it could not declare the TUI display effect.

The compatibility repair publishes the trace result using the normal message
path:

```text
trace worker result
  -> eval-cells message to the target program cell
  -> run all scheduled downstream tasks
  -> perform resulting boundary effects
```

That repair is preferable to a test sleep, direct TUI mutation, or another
trace-specific settling function. It restores the scheduler invariant at the
point where it was bypassed.

The declarative commit/effect model generalizes the same repair:

```text
trace propagator declares :trace/compute effect
  -> runtime handler computes graph asynchronously
  -> handler returns a commit targeting the trace result cell
  -> runtime starts the next commit round
  -> ordinary propagation reaches equilibrium
  -> block-at declares :tui/write-display effect
  -> runtime performs the display effect
```

The 23 assertions then exercise one shared runtime contract rather than needing
23 patches.

Four separate graph-demo failures compare exact visualization labels. They
predate the retained-application regression and should be evaluated separately
as visualization expectation drift; they are not evidence against the runtime
commit/effect model.

## Invariants

The runtime should make these rules executable and testable:

1. Propagators declare messages, kernel declarations, commits, or boundary
   effects; they do not perform host IO.
2. A commit always enters through `eval-cells`; runtime code does not publish a
   value with `assoc-net-cell`.
3. All tasks caused by a commit batch reach equilibrium before effects from
   that round are interpreted.
4. Effect results become commits for a later round, never mutations of the
   currently propagating network.
5. Each stable effect ID is delivered at most once for the same epoch unless
   its handler explicitly defines retry semantics.
6. External inputs are serialized at transaction boundaries.
7. A round limit or repeated-commit detector reports temporal oscillation;
   per-round propagation equilibrium alone does not prove runtime termination.
8. Runtime errors record the phase, round, commit/effect identity, and target
   before the runtime decides whether to retry or stop.

## Garbage Collection Boundary

Garbage collection should run only at a runtime-idle boundary. At that point no
propagator task or synchronous effect can still discover a new internal
reference in the current transaction.

The root set must nevertheless include more than named program cells:

- queued external inputs and commits;
- targets and payload references in unconsumed effects;
- targets retained by asynchronous in-flight effects;
- public/named cells and client subscriptions;
- retained closure and application frames.

Runtime waiting is therefore not automatically safe for collection. It is safe
only when every in-flight effect exposes the internal references that its later
commit may require. Formal commit/effect declarations make those references
inspectable instead of hiding them in callback closures.

## Additive Implementation Plan

1. **Restore the intended declaration strategies.** Keep retained application
   as compiler-2's default. Give the live runtime a compiler wrapper that
   selects ordinary runtime application at every immediate or delayed compile
   entry. Verify that the two runtime errors and the non-trace failures stay
   removed.
2. **Introduce one commit value and evaluator.** Add `effect-result-commit`,
   `apply-commit`, and `commit!`. Implement commit updates with the existing
   `apply-program-updates`; do not change `Net`, the scheduler, or boundary
   effects.
3. **Route trace completion through the runtime evaluator.** The tracer declares
   the commit and keeps trace-domain freshness/bookkeeping. The runtime owns
   cell merge, propagation, application settling, and boundary handling. This
   is the first end-to-end proof because it crosses asynchronous computation,
   program propagation, and TUI effects.
4. **Add the fixed-point driver.** Compose commit, propagation, and effect
   phases until idle. Keep the existing public runtime entrypoints as adapters.
5. **Turn existing outbox requests into effect declarations.** Reuse their
   stable IDs, epochs, ports, kinds, payloads, and receipt targets. Avoid a
   second competing effect format.
6. **Migrate one boundary at a time.** Trace, then TUI display/write, then XR
   launch/widgets. Preserve each existing handler behind the common effect
   evaluator.
7. **Make asynchronous completion enqueue-only.** Worker threads enqueue
   commits; the serialized runtime driver alone mutates the runtime state.
8. **Add idle-boundary GC hooks only after the round semantics pass.** First
   expose roots and lifecycle events; do not implement collection as part of
   the IO repair.

The migration is complete when boundary-specific code declares work and the
runtime driver is the only component that decides when commits, propagation,
and IO evaluation occur.

## Verification Plan

Start with small contract tests rather than treating every UI assertion as a
separate bug:

1. A declared commit does not change its target cell before the next commit
   phase.
2. Applying a commit uses cell merge and schedules every downstream propagator.
3. Propagation reaches task-queue equilibrium before an effect handler runs.
4. A synchronous effect result is applied only as a next-round commit.
5. An asynchronous completion enqueues a commit without mutating the network
   from its worker thread.
6. Inputs arriving during propagation remain queued until the transaction is
   idle.
7. Stable effect IDs prevent duplicate delivery for one epoch.
8. A deliberately self-rescheduling commit reports a round-limit error.
9. Trace completion updates its program target, runs `block-at`, and updates the
   outer TUI display through an effect.
10. Existing trace, shared-client, later-upstream, list, closure, and retained
    application scenarios retain their values and stable IDs.

Then run the existing runtime-server suite as the integration receipt. Its many
trace assertions are valuable because they cover different topologies, but the
unit of repair remains the shared commit/effect boundary.
