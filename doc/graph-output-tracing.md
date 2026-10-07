# Tracing an application with late outputs

Compiler applications expose discovered member ports as output edges of their
flat-GUR application propagator. Runtime `neighbors` reads those edges from a
frozen Net snapshot and returns the same compound representation as for any
other propagator. No callable classification or output-name index is needed to
observe connectivity.

A snapshot taken before a closure or list tail arrives contains the boundary
known at that time. A later snapshot contains the added edges. This change does
not add automatic subscription or scheduling policy for topology observations.
Periodic tracing continues to take explicit, fresh snapshots.

The focused runtime test verifies that extended outputs appear in the compound
neighbor projection, reciprocal cell connections are visible, output values may
remain unavailable, and the original Net and creation relationships are intact.
The existing one-time Lain tracer execution/disposal test remains supported.

## Timing limitation

An exploratory end-to-end Lain trace of a two-node cyclic traversal exceeded the
three-second individual-test limit. The equivalent graph constructed with an
ordinary output boundary also exceeded that limit using the previously
published compiler and infrastructure dependencies. Every attempt was stopped
at the limit; no successful end-to-end result is claimed for that case.

Moving program bootstrap into a separate fixture did not bring the two-node
execution below the limit. That exploratory fixture was removed rather than
relaxing the threshold or changing the tracer algorithm. The accepted tests
verify the graph API, the actual compound-valued `neighbors` primitive, and the
existing one-time execution/disposal case independently. General Lain tracer
performance requires a separate investigation.
