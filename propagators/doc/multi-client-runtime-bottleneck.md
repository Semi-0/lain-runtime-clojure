# Multi-Client Runtime Watcher Bottleneck

This note records the current evidence for the slow path when one compiler-2
runtime server is shared by several TUI clients. It also proposes a fix path.

The key result is simple: socket read and TUI rendering are not the bottleneck.
Appending watcher blocks, especially `be:block` watchers, is the cliff.

## Benchmark Added

The benchmark harness is `graph/compiler_2_tui_bench.clj`.

New modes:

```bash
clojure -M:wired/tui-bench multi-client <clients> <blocks> <watchers>
clojure -M:wired/tui-bench multi-client-profile <clients> <blocks> <watchers> <slow-ms> <max-ms>
```

`multi-client-profile` is the useful diagnostic mode. It starts one TCP runtime
server, registers several client ids, appends source blocks, preallocates blank
target blocks with `:rebuild? false`, appends watcher blocks, and reads each
view. It records every request and stops early when one request exceeds
`slow-ms` or the run exceeds `max-ms`.

Each client uses independent names:

```clojure
(def c0x0 0)
(-> (+ 1 c0x0) c0x1)
...
(-> c0x0 (be:block 10))
```

That avoids accidental symbol redefinition between clients while still using
one shared server session and one shared compiler environment.

## Evidence

### 4 clients, 10 blocks, 10 watchers

Command:

```bash
clojure -M:wired/tui-bench multi-client-profile 4 10 10 5000 60000
```

Result:

```clojure
{:clients 4
 :blocks-per-client 10
 :watchers-per-client 10
 :stopped :slow-request
 :elapsed-ms 57863.000208
 :phases
 {:register {:avg-ms 171.57257275000003 :p95-ms 621.606791 :count 4}
  :source-block-append {:avg-ms 143.430440625 :p95-ms 194.750333 :count 40}
  :target-block-append {:avg-ms 36.754492725000006 :p95-ms 40.065458 :count 40}
  :watcher-append {:avg-ms 2379.182876904762 :p95-ms 4973.817209 :count 21}
  :read-view nil}
 :slowest
 [{:phase :watcher :client "c2" :item 0 :ms 5488.222708 :ok true}
  {:phase :watcher :client "c1" :item 9 :ms 4973.817209 :ok true}
  {:phase :watcher :client "c1" :item 8 :ms 4550.099375 :ok true}
  {:phase :watcher :client "c1" :item 7 :ms 4162.993041 :ok true}]
 :completed-requests 105}
```

The run stopped before finishing all watcher appends. The source blocks were
still sub-200ms p95. Blank target allocation was about 40ms p95. Watchers grew
into multi-second requests.

### 10 clients, 10 blocks, 10 watchers

Command:

```bash
clojure -M:wired/tui-bench multi-client-profile 10 10 10 5000 60000
```

Result:

```clojure
{:clients 10
 :blocks-per-client 10
 :watchers-per-client 10
 :stopped :slow-request
 :elapsed-ms 71876.346416
 :phases
 {:register {:avg-ms 80.1982041 :p95-ms 602.976916 :count 10}
  :source-block-append {:avg-ms 354.54147922000004 :p95-ms 842.957666 :count 100}
  :target-block-append {:avg-ms 35.68468873 :p95-ms 38.608084 :count 100}
  :watcher-append {:avg-ms 32043.310416 :p95-ms 32043.310416 :count 1}
  :read-view nil}
 :slowest
 [{:phase :watcher :client "c0" :item 0 :ms 32043.310416 :ok true}
  {:phase :source-block :client "c9" :item 3 :ms 931.441208 :ok true}]
 :completed-requests 211}
```

The first watcher append took about 32 seconds after the 10-client source setup.

### 10 clients, 10 blocks, no watchers

Command:

```bash
clojure -M:wired/tui-bench multi-client-profile 10 10 0 5000 60000
```

Result:

```clojure
{:clients 10
 :blocks-per-client 10
 :watchers-per-client 0
 :stopped nil
 :elapsed-ms 33035.419542
 :phases
 {:register {:avg-ms 69.56784160000001 :p95-ms 549.227208 :count 10}
  :source-block-append {:avg-ms 323.23190002 :p95-ms 727.834625 :count 100}
  :target-block-append nil
  :watcher-append nil
  :read-view {:avg-ms 1.1512957 :p95-ms 1.796583 :count 10}}
 :completed-requests 120}
```

This completed. The shared source append path is not cheap, but view reads are
about 1ms. The server socket and TUI projection are therefore not the visible
multi-second bottleneck.

### Baseline simple wired bench

Command:

```bash
clojure -M:wired/tui-bench
```

Result from the same worktree:

```clojure
{:socket-read-view {:avg-ms 1.2098354500000001 :p95-ms 1.656625}
 :unchanged-update {:avg-ms 0.006379150000000003 :p95-ms 0.005625}
 :raw-render {:avg-ms 0.0091664 :p95-ms 0.018583}}
```

Again, socket read, unchanged TUI update, and raw render are not the culprit.

## What The Evidence Proves

The results eliminate several explanations:

- It is not primarily TCP request overhead. `read-view` stays around 1-2ms.
- It is not text rendering. `raw-render` is tiny in the baseline.
- It is not blank target block allocation when targets are appended with
  `:rebuild? false`; target append p95 is about 40ms.
- It is not only source parsing; source append grows with total shared env size,
  but the severe cliff appears at watcher append.

The bottleneck is watcher installation under a shared runtime graph and env.

The scaling shape is the important proof:

- 10 clients x 10 source blocks, no watchers: finishes in about 33s.
- Add watchers: the first watcher after that setup takes about 32s by itself.
- 4 clients show the same slope earlier: watcher appends climb from about
  1.6s to over 5s before the profile stops.

That points to work triggered by watcher append that scans, settles, or projects
global runtime state instead of only installing the new watcher topology.

## Current Watcher Append Path

The server command path is:

```text
server/request
-> graph.compiler-2-runtime.commands/handle-command!
-> tui-session/append-tui-block!
-> input/install-block-incremental!
-> program-rebuild/incremental-block-state
-> effects/perform-boundary-effects
-> input/replay-runtime-inputs
-> effects/run-runtime-cycle
-> graph-projection/record-runtime-transaction
```

The currently suspicious sub-steps are:

1. `program-rebuild/incremental-block-state` calls `seed-program-topology` over
   `program/all-blocks`. That reseeds the whole TUI block linked-list topology
   into the program net for each incremental block install.

2. `program/compile-program-form` computes semantic graph data for the new
   block. For a simple `be:block` watcher, most users want a display effect,
   not an immediately-expanded semantic graph.

3. `program-rebuild/incremental-block-state` publishes the updated semantic
   graph and calls `settle-state`. `settle-state` delegates to
   `program/settle-application-props`, which includes retained application
   propagators from the whole program net, not just the newly installed watcher.

4. `effects/run-runtime-cycle` refreshes semantic graph state from the last
   compiled form and performs boundary effects. This can re-project graph data
   even when the operation is only a TUI display watcher.

5. `graph-projection/record-runtime-transaction` compares before/after program
   nets to track changed cells. If this scans a large network after every
   append, it turns every watcher install into global work.

The concrete hot spot still needs per-substep timing, but the phase benchmark
already narrows the regression to the watcher installation transaction.

## Proposed Fix

### Step 1: Add internal phase timing

Add an opt-in profiler around `install-block-incremental!`:

```text
seed-program-topology
compile-program-form
publish-graph
settle-state
perform-boundary-effects
replay-runtime-inputs
run-runtime-cycle
record-runtime-transaction
```

The profiler should return timings in the benchmark result, not print logs from
inside runtime code. This keeps the runtime pure enough for tests and makes
benchmark output comparable.

Success: the next profile explains the 32s watcher append as one or two named
substeps, not just `:watcher`.

### Step 2: Stop reseeding all block topology for one new block

`seed-program-topology` should not run over every TUI and every block during a
single append. Block append is monotone. The runtime already knows the new block
and the previous tail.

Add small primitives:

- seed one TUI instance once at registration;
- seed one block's `:block/index`, `:block/text`, `:block/display`, and `:cdr`
  slots when that block is appended;
- seed the previous tail cdr once when the linked list grows;
- keep a program-net-side index from `(client-id, block-index)` to text/display
  ids so `block-at` and `be:block` do not rediscover targets by walking the
  linked list.

This keeps append topology work proportional to the new block, not to all
clients and all prior blocks.

Success: blank target append remains around tens of milliseconds, and watcher
append no longer grows just because unrelated clients have more blocks.

### Step 3: Make `be:block` install a display-effect edge directly

`be:block` is an effectful display watcher. It should not need the same generic
application path as ordinary value computation.

The direct installer can:

- compile the source operand normally;
- resolve the target display id from the block index map;
- install a small propagator from source cell to boundary outbox/display request;
- avoid semantic graph extraction unless a trace/XR block actually requests the
  graph.

This keeps intention separate from strategy:

- intention: "display this behavior/current value in block N";
- strategy: "install a boundary-effect propagator from source to target".

Success: one watcher append should look closer to ordinary block append than to
full semantic graph rebuild.

### Step 4: Partition semantic graph projection

Semantic graph should be maintained as per-block fragments keyed by source
block identity:

```clojure
{:program/graph-fragments {[:client "c0" :block 12] graph-fragment}}
```

Appending a watcher updates only its fragment. A full graph value can still be
projected by unioning fragments when a trace/XR view asks for it. Trace blocks
can subscribe to the union cell, but ordinary TUI display watcher appends should
not eagerly recompute the whole graph.

Success: graph node/edge count can grow, but append latency should not scale
with full graph projection unless the append itself is a graph-producing block.

### Step 5: Narrow retained application settling

`settle-application-props` currently protects correctness by running retained
application props after compile. That is safe, but it is global.

The runtime needs a narrower queue:

- newly installed props for the current block;
- retained props whose inputs changed during this transaction;
- trace/XR props only when graph cell changed;
- boundary-effect props only for the target client/block affected.

Success: late watcher append does not re-run unrelated retained applications
from other clients.

## Success Criteria

Use the same commands:

```bash
clojure -M:wired/tui-bench multi-client-profile 4 10 10 5000 60000
clojure -M:wired/tui-bench multi-client-profile 10 10 10 5000 60000
```

Target results:

- both runs complete without `:stopped`;
- `read-view` remains around 1-2ms;
- target block append remains under 50ms p95;
- watcher append p95 is under 250ms for 4 clients;
- first watcher append for 10 clients is under 1s;
- no full-rebuild fallback is used for covered append scenarios;
- existing correctness suites still pass:

```bash
clojure -M:test graph.vijual.compiler-2-runtime-server-test
clojure -M:test graph.xr-runtime-test
clojure -M:test propagators.compile-2-test
```

## Current Conclusion

The shared-server multi-client model works semantically, but watcher install is
not incrementally scoped yet. The runtime still does too much global work when a
new effectful watcher is appended.

The fix should not be "make the server faster" or "optimize TUI rendering".
The fix is to make watcher append a local topology/effect installation:

```text
new watcher block
-> resolve target display id directly
-> install source-to-display boundary effect propagator
-> update only this block's graph fragment
-> settle only newly affected props
```

That matches the propagator model better: adding a watcher is a monotone
topology extension, not a reason to rescan every client, every block, and every
retained application in the shared runtime.
