# UMVPA M1.03 spatial adapter verification

Scope: `SpatialMeasurementFrames` and `SpatialMeasurementScatter` only. The
measurement core is owned by the adjacent M1.03 handoff.

The frame adapter accepts only supports owned by the source axis's live locus
domain and checks that ownership before reading supports or centers. The typed
adapter rejects foreign supports at compile time. Region
supports use ambient order; `Selection` supports retain selection order. Volume
neighborhoods and centered surface searchlights become lazy frames whose legs
are built during traversal. Frame identity carries the caller's declared key,
revision, parameters, and a canonical support manifest. The manifest includes
measurement ID and ordered support ordinals, while center and label remain
rendition metadata.

Scatter returns a locus `IndexedField` of explicit cells. An unvisited point is
not filled with numeric zero. Its local supports likewise share the domain type
at the API boundary. Local failures retain their measurement IDs and
messages. Successful overlapping results retain contributors, declared weights,
and the positive denominator used by the supplied aggregation algebra. Results
are reduced in canonical measurement-ID order so receipt and floating-point
accumulation do not depend on local completion order. A non-finite overlap
denominator is rejected before the aggregation algebra runs. The supplied
double weighted-mean algebra also rejects a non-finite weighted accumulator;
generic algebras declare their own accumulator validity.

Focused suites added:

- `SpatialMeasurementFramesSuite`: typed owner rejection, region/selection ordering,
  direct identity oracle, support-sensitive frame identity, rendition-insensitive
  identity, volume-neighborhood frame identity, and explicit empty-surface
  rejection.
- `SpatialMeasurementScatterSuite`: direct weighted-mean oracle (2 and 10 with
  weights 1 and 3 yields 8), contributor/denominator receipt, unvisited cells,
  local failures, foreign-support rejection, and denominator-overflow rejection.
  The built-in double weighted mean also has a `Double.MaxValue` accumulator
  overflow regression.

The final owning-module qualification is recorded below.

## Final qualification

On 2026-09-29, the frozen candidate passed `mvpaJVM/test` 173/173,
`mvpaJS/test` 173/173, `mvpaSpatialJVM/test` 27/27, and
`mvpaSpatialJS/test` 26/26 in bounded invocations. `scalafimCompileAll`
passed with no warnings. Independent compiler/measurement/spatial source
review accepted the repaired contracts. Raw logs, source hashes, provider
revisions and execution metadata are recorded in
`/private/tmp/scalafim-umvpa-20260929/qualification.json`.

This qualification applies to the isolated candidate, whose recovered base is
`79a8b7d1fbe07c383bb8929a69973729c22ce64b`, rather than the shared main
checkout. No legacy cutover, native replay certification, uncontaminated
performance benchmark, fresh external fixture regeneration, or full-epic
scientific qualification is claimed. Spatial construction materializes support
metadata; streaming fold itself retains only its caller accumulator. Built-in
weighted arithmetic overflow is rejected with a typed error.
