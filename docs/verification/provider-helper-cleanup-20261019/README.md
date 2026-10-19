# Provider helper cleanup

This slice removes 168 production lines and adds no replacement model, dependency,
or provider revision. The transport wrappers `ExactSpatialMap` and
`CrispSpatialRelation` had no production callers. Their only test now exercises
locus4s `TotalMap` and `Relation` directly, including distinct live owners and the
separation from sampled `SpatialOperator` values.

Selected images remain zero-wrapper refinements of image4s `SelectedSampled`.
Five convenience constructors with no production callers are removed: volume `categorical`/`mask`
and series `continuous`/`categorical`/`mask`. Use the existing checked `create`
constructor with explicit `ValueSemantics` when constructing those roles; the four
existing test call sites now demonstrate this.
The redundant `.selected` identity accessor is removed from volume and series;
provider methods such as `mapValues`, `withSelectionData`, `seriesAt`, and
`scatterVolume` are already accessible directly through their public subtype.
Known callers now use those methods directly.

The checked scalar/series constructors, single-Time-axis admission, exact support
order, live-owner checks, missing-voxel policies, and dense neuroimage admission
remain in place. These enforce domain contracts and will move with neuroim4s.
NeuroStats changes only by removing its two identity-accessor calls; numerical
predicates and algorithms are unchanged. The other neuroim2 parity APIs, active
`SpatialDimSize` and `AxisCode` models, and deferred spatial-routing work remain.

This intentionally changes the source API. No compatibility aliases are retained;
no persisted identifier, storage representation, or scientific computation changes.

The full image suite passed 359 JVM / 329 JS tests, and spatial passed 231 JVM /
206 JS tests: 1,125 executions, zero failures or skips. `scalafimCompileAll`
passed every module on JVM and JS without Scala source warnings. Existing allocation checks
pass with unchanged budgets. No test is removed; selected-image support/order,
Time-axis rejection, mutable-ingress isolation, dense round trips, and parity
fixtures continue to run. The first compile exposed four test-only convenience
constructor calls split over multiple lines; those now use `create` with explicit
categorical/continuous semantics. The failed compile log is retained.

Exact source inputs and commands are recorded in [receipt.json](receipt.json).
Raw logs are retained under the shared Git directory at
`verification/provider-helper-cleanup-20261019/`. This work is local and unpushed;
it does not claim hosted CI or a complete ScalaFIM test-all run.
