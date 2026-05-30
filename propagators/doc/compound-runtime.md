# Compound Runtime

Source files:

- `propagators/closure.clj`
- `propagators/stdlib.clj`
- `propagators/propagator.clj`
- `propagators/core.clj`

## Context

The classic MIT propagator model treats compound propagators as install-time
expansion: the compound body creates cells and propagators directly in the
parent network. Boundary cells are the same cells as the caller sees.

This experiment separates declaration from evaluation. A compound can therefore
hold a declared subnet as data and run that subnet by applying the evaluator to a
network value and a task queue. The subnet run is not a hidden mutation of the
constructor; it is an explicit evaluation step that returns messages for the
outer network.

This repo also experiments with runtime compounds. A runtime compound stores a
closure as data and runs an inner network when the compound propagator activates.
That gives better inspection and hot reload. It is more indirect than flat
expansion, so long stable paths should eventually have a compiled lowering.

## Runtime Compound Model

Current runtime compound activation uses an avatar frame:

1. parent boundary cells exist in the outer graph
2. activation creates or uses avatar cells for inner work
3. topology-only links connect real cells and avatars
4. the inner closure installs or runs inner propagators on avatars
5. inner `run-tasks` starts from avatar inputs
6. changed avatar outputs are diffed back into messages for real boundary cells

The key point is scheduling isolation. Inner work must not enqueue the compound
propagator itself.

## Why Avatars Exist

If inner `run-tasks` starts from real boundary cells, those cells have outgoing
edges to the compound propagator. The compound can schedule itself again while
already running, causing re-entry loops or stack overflow.

Avatars avoid that:

| Seed for inner run | Schedules compound? | Use? |
|--------------------|---------------------|------|
| real boundary cells | yes | no |
| avatar boundary cells | no | yes |

This is a runtime frame, not lexical identity. A real parent cell and its avatar
are different cells connected by explicit synchronization logic.

## Bi-sync Boundary Shape

`stdlib/bi-sync` installs two `p:id` propagators in opposite directions. For a
compound constraint, the same boundary cells should usually appear in both
input and output sets:

```clojure
(install-compound n closure-cell [a b] [a b])
```

Directional wiring such as `[a] -> [b]` is not enough for a symmetric
constraint. If `b` changes and is only an output, the compound is not woken by
that change.

## Current Two-Stage Direction

The intended long-term model is two lowerings for the same propagator language:

| Stage | Purpose | Shape |
|-------|---------|-------|
| runtime compound | inspection, experiments, hot reload | closure-as-data + avatars + inner run |
| compiled compound | stable fast path | expand into one parent graph/env |

Runtime compound is useful while editing or reflecting on the closure. Compiled
compound should be used for stable hot paths once promotion/demotion exists.

Promotion will need:

- semantic equivalence tests between runtime and expanded forms
- generation/version tags on expanded nodes
- demotion or garbage collection when the closure changes
- protection against duplicate wiring in hybrid runtime/compiled graphs

## Historical Performance Notes

The experiment moved through several eras:

1. snapshot inner nets were fast and isolated but not unified with the parent
   graph
2. parent-network execution without avatars exposed re-entry scheduling bugs
3. avatars fixed scheduling but added overhead
4. boundary caching helped some short runs
5. removing the old `closure-out` bookkeeping message made current runtime
   compounds much faster for prototype workloads

The current runtime model is good enough for experiments, but it should not be
treated as the final representation for long stable chains.

## Open Risks

- port order currently comes from sets in graph nodes, so pairing by position is
  order-sensitive and should be made explicit
- runtime compounds are still more expensive and indirect than flat expansion
- scheduling correctness depends on seeding inner work from avatars, not real
  boundary ids
- the API does not yet make promotion to compiled compound explicit
