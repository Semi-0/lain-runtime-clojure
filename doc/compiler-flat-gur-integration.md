# Compiler Flat-GUR Runtime Integration

Status: current architecture, 2026-09-12.

The runtime consumes connected application topology from the pinned
`lain-compiler` dependency. Program compilation no longer supplies a retained
application installer or exposes hidden application-output indexes. Compiler
environments are imported through `import-environment-topology`, and every
returned propagator ID is scheduled.

Block premise compilation, semantic graph projection, session repair, and
retraction inspection obtain operator, argument, context, frame, capture, and
result identities through read-only traversal of
`propagators.compiler.lowering.application/application-topology`.

This runtime port comes from
`Semi-0/datalog-research@3eac0243745cf1a6a21495e4f6e77a5bbef5900d` and depends
on the corresponding `lain-infrastructure` and `lain-compiler` topic commits.

## Clock limitation

The experimental `clock-in` primitive marks its output through a network
dictionary entry. Compiler topology lowering preserves cells, propagators,
names, and messages, but does not preserve arbitrary dictionary updates. A
clock event can therefore arrive in an unmarked cell and fail to cross the
event-to-display path.

The reliable clock contracts remain tested: updates are source-aware events,
and a retracted premise disables its subscription. Full clock loading and
event-to-TUI display remain deprecated diagnostic functions rather than release
tests. A robust fix requires an explicit representation for dictionary-backed
protocol declarations and is outside this application refactor.
