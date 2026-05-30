# Core Runtime Model

Source files:

- `propagators/graph.clj`
- `propagators/network.clj`
- `propagators/propagator.clj`
- `propagators/core.clj`
- `propagators/compile.clj`
- `propagators/helpers/task_queue.clj`

## Representation

The central experiment is to separate network declaration from network
evaluation. Declaration builds a graph/env/dict value. Evaluation consumes that
value with an explicit task queue and returns a new network value.

That means a network can itself become data. It can be stored in a cell, merged
as partial information, inspected, and passed to compound propagators without
requiring evaluation to be hidden inside construction.

The runtime separates topology from state:

| Piece | Shape | Role |
|-------|-------|------|
| graph | `id -> Node` | wiring: input and output node ids |
| env | `id -> Cell or Propagator` | runtime content and behavior |
| dict | `name -> id` | optional named interface |

`Net` stores all three:

```clojure
(net graph env dict)
```

This is different from a monolithic network object. A caller can construct a
network, inspect it, seed cells, enqueue tasks, and run the scheduler explicitly.

## Cells

A cell has:

```clojure
{:content ...
 :strongest ...}
```

`content` is the raw merged information. `strongest` is the readable value used
by propagator inputs. Most simple values have `content == strongest`, but richer
domains can keep more evidence in content than they expose as strongest.

Examples:

- named-network evidence stores an antichain in content and computes strongest
  lazily
- compound subnet state stores structural state; effectful execution is not part
  of strongest

## Propagators

A propagator stores an activation function:

```clojure
(fn [input-ids output-ids network] messages)
```

Primitive propagators are built with `primitive-propagator`. They read strongest
values from input cells, skip activation if any input is unusable, and return
messages for output cells.

`p:id` is the simplest primitive:

```clojure
(def p:id (primitive-propagator (fn [x] x)))
```

## Installer Shape

Installers mutate the immutable network value by returning a new network:

```clojure
installer = (fn [network] [installed-id network'])
```

`construct-cell` installs a blank graph node plus a cell env entry.
`construct-propagator` installs a graph node, wires edges, and stores the
propagator env entry.

## Scheduler

`run-tasks` drains an explicit FIFO queue of propagator ids:

```text
task queue -> eval-propagator -> messages -> eval-cells -> maybe enqueue outputs
```

`eval-propagator`:

1. reads the propagator node from `graph`
2. reads the propagator function from `env`
3. calls the function with input ids, output ids, and the network
4. merges returned messages into cells

`eval-cell`:

1. merges the update into cell content
2. computes strongest
3. stores the new cell
4. if strongest changed, enqueues downstream propagators

Task queue entries are propagator node ids. Message targets are cell node ids.

## Compiler Surface

`compile-net` lowers a small quoted DSL into the same graph/env representation:

```clojure
'(let-cell [c0 c1]
   (p:id c0 c1))
```

Supported concepts are currently small:

- cell binding
- sequential `do`
- primitive propagator installation such as `p:id`

The compiler is closer to MIT-style install-time expansion for primitive
networks: it adds cells and propagators to one flat graph/env. Compound forms are
not yet part of this compiler surface.

## Assumptions

- graph nodes must be installed before edges are wired
- propagator ports are cell ids
- propagator functions return messages for cells, not propagator nodes
- missing env entries are errors, not soft failures
- port order is not guaranteed when using graph node input/output sets
- callers decide what to enqueue first

## Current Limits

- contradiction handling is still a stub
- dependence tracking is not implemented
- backtracking is not implemented
- richer domains need explicit `cell-merge` and `strongest-value` methods

The design favors explicit data flow over hidden runtime mutation. That makes
tests slightly verbose, but it keeps each scheduler step reproducible.
