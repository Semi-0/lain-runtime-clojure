# Live Runtime Surface

See [Compiler Flat-GUR Runtime Integration](compiler-flat-gur-integration.md)
for the current application-topology boundary and the experimental clock
limitation.

This note describes the current compiler-2 TUI/runtime experiment as a user
interface model. It is not a replacement for the implementation notes in
[`compiler-2.md`](compiler-2.md); it names what the system is becoming.

## The Shape

The runtime is no longer just a REPL that evaluates text and prints a result.
It is a live surface where source blocks, named cells, semantic traces, rendered
graphs, and multiple client views all sit on top of the same propagator
network.

Blocks are runtime cells. A block can contain source text, a normal value,
`nothing`, a contradiction, or a graph value. Compiler-2 code can address those
blocks through `block-at`, so the UI is not outside the language runtime. It is
part of the system being coordinated.

Named definitions are also live cells. `(def a)` creates a free named cell that
later blocks can constrain with relationships such as:

```clojure
(<-> (+ 1 2) a)
```

This is different from assignment in a normal REPL. The name is not just a
mutable variable slot. It is a cell that can participate in propagation and be
connected to other cells over time.

## What Exists Now

Current implemented pieces:

- `def` creates named free cells and can optionally sync a body into them.
- `def-net` declares a named compiler-2 network closure for later use.
- `def-cell` declares a named closure that takes input cells and returns an
  output cell.
- `block-at` reads and writes linked-list TUI block cells from compiler-2 code.
- `instance` resolves another connected TUI instance by client id.
- `trace` returns semantic graph data as an ordinary runtime value.
- TUI blocks render graph values with Vijual stress-majorization layout.
- Trace output blocks update when later source blocks add upstream relations.
- Trace traversal follows semantic nodes backed by the same runtime cell across
  separate source blocks.
- Multiple TUI/socket clients share one runtime session and one growing
  compiler environment.
- Clients can talk to each other through block cells: one client can write a
  value or graph into another client's block with `(block-at (instance other)
  index value)`, and the receiving client sees that update as part of its own
  view.

The important property is that the runtime can inspect and update parts of its
own interactive surface without leaving the propagation model.

## Example

One block can declare a free cell:

```clojure
(def a)
```

Another block can request a live upstream trace:

```clojure
(def g)
(trace a :upstream g)
```

Later blocks can add relationships:

```clojure
(def b)
(<-> b a)
(<-> (+ 1 2) b)
```

The already-rendered trace graph for `a` can expand to show:

```text
1, 2 -> + -> 3 -> b -> a
```

That makes the graph view a reactive value, not a one-shot debugger screenshot.

Because each client instance is also bound in the compiler environment, the
same mechanism can send values across TUI sessions:

```clojure
(let-cell [msg]
  (<-> "hello from A" msg)
  (block-at (instance tui-b) 0 msg)
  msg)
```

This is not a separate chat protocol. It is ordinary propagation into another
client's block cell.

## Why This Is Unusual

Most interactive language systems keep several boundaries separate:

- editor text vs runtime values;
- UI state vs program state;
- assignment vs constraints;
- result display vs dependency graph;
- debugger views vs values that can flow through the program.

Python notebooks, Clojure REPLs, and many live-coding systems usually evaluate
cells and display snapshots. Some tools track dependencies, but the dependency
graph is normally tool metadata. Some constraint or logic systems expose live
cells, but they usually do not make the notebook/TUI surface itself available as
runtime data.

This experiment combines those pieces:

```text
source block -> compiler-2 expression -> propagator cells
             -> semantic graph value -> rendered block
             -> addressable again through block-at
```

The loop is controlled by compiler primitives rather than hidden editor magic.
That is the novel part.

## Current Limits

The system is still a prototype:

- TUI graph rendering is text-based and can be expensive for large graphs.
- `block-at` is powerful but low-level; it exposes linked-list block addressing
  directly.
- Trace graph construction is semantic, not a full provenance or proof system.
- The runtime rebuild path is intentionally simple and recompiles source blocks.
- Conflict handling still relies on the current cell merge/contradiction model.
- There is no policy layer yet for permissions, persistence, or collaborative
  editing.

Those limits are acceptable for now. The point of the prototype is to test the
interaction model: a live, shared, propagator-backed runtime surface.

## Design Principle

Keep the core rule simple:

> UI blocks, named values, traces, and graph views should be ordinary runtime
> cells whenever possible.

When that holds, the system can grow new interaction forms without adding a
separate notebook engine, debugger protocol, or editor-side dependency tracker.
