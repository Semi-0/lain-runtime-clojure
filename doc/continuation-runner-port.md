# Continuation runner runtime migration

Source: `Semi-0/datalog-research` revision
`5a1c0be62e44c48510f69316c766ccfcaa5251af`.

This port consumes the infrastructure continuation runner and the compiler's
existing CPS operand contract. Runtime owns sessions, effect execution, extension
installation, full-file environment reload, and pure semantic inspection.
Compiler-aware collection application stays in lain-compiler; browser rendering,
XR projection, and UI assembly stay in wired. No presentation dependency is added
to the runtime.

The visualization extension uses layered primitive propagators. Relationship and
value observations produce declarative data; they do not render it or grant
write-through authority to source references. Existing session and extension
injection APIs remain intact.

Verification: 147 tests, 527 assertions, zero failures/errors using the published
infrastructure/compiler SHA pins in deps.edn. This includes preserving injected
runtime options across two complete source reloads.

## Release blocker

This migration is published on a review branch, not runtime main. Its library
suite passes, but downstream wired integration is not compatible yet.

Three existing wired tests pass at wired revision
`040a46930e5a9ad31fed64c3747c5d90ddcfa2fe` with its original dependency pins
(3 tests, 9 assertions), but each fails one assertion with this migration:

- `block-target-expression-writes-future-block`: expected 7, got nothing.
- `blocks-compile-into-one-growing-env`: after editing inc1, expected 6, got 5.
- `be-block-watch-installs-and-updates-without-full-rebuild`: expected 56, got nothing.

These live in `propagators.tui.graph.vijual.compiler-2-runtime-server-test`.
The old retained-application rescheduling path has been removed. The replacement
uses flat declaration topology and declared dependencies. Investigation must
preserve block-write, live-edit, and behavior-watch semantics without restoring
the retired execution path or adding domain branches to the runner. The precise
cause of each regression is not yet proven. Do not promote this branch on the
basis of the library suite alone.
