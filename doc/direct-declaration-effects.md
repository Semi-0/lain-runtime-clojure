# Direct compiler declaration effects

The compiler dependency now emits declaration effects as cells and propagators
are installed. Flat GUR commits those effects into the active runtime Net.
The whole-network diff adapter is removed; runtime scheduling and effect
boundaries are unchanged.

All 160 runtime tests passed (585 assertions) against this compiler. Every
individual test finished within three seconds; the cyclic Lain tracer completed
in 1.77 seconds and reached quiescence. Its cycle prevention is unchanged.

The compiler retains an immutable declaration view for compilation. It neither
executes a child runtime network nor exports its private recording buffer.
