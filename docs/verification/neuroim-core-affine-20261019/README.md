# neuroim4s core: affine policy cutover

The first independent core slice is the existing `NeuroAffineSyntax`, moved from
`scalafim.image` to `neuroim4s.image`. Its arithmetic, validation, return types and
allocation behavior are unchanged byte for byte; only the package declaration
changes. image4s remains the sole affine/matrix owner. No wrappers or aliases are
introduced. `neuroim4s-core` depends on image4s geometry, with coherent immutable
image4s/spatial4s/Gale pins, and has no ScalaFIM, surface, atlas or MVPA dependency.

This is part of `eco-neuroim-core`, not completion of that task. The remaining
volume/series admission and IO APIs and retained neuroim2 parity families still
need their own moves. The affine slice is independently extractable and breaks
one existing dependency used by the later volume/series migration.

## Published provider and consumer cutover

The user approved publishing the exact candidate. neuroim4s main is now
`9780bcab6c061284a3c8864d9599b0534c71a97f`, advanced from
`3b7a8441e0fcdfd20428835f8eaec7688b73a9ef` by a non-force fast-forward.
The owned clone's HEAD, tracking ref and GitHub main match exactly and its
worktree is clean. [publication.json](publication.json) records the verified
provider publication and consumer qualification.

The saved [consumer-cutover.patch](consumer-cutover.patch) is now applied:
ScalaFIM's original 88-line implementation is deleted, all ten imports are
migrated, core module dependencies are added on both platforms, explicit
image4s/spatial4s overrides are forwarded, and the immutable neuroim4s revision
is pinned. No compatibility facade remains. Existing integration assertions
and fixtures are preserved. The original [receipt.json](receipt.json) retains
the pre-publication local-override evidence; publication.json records the new
default-pin gate with no local overrides.

A recovery bundle and all raw evidence live in the shared Git directory under
`verification/neuroim-core-affine-20261019/`. ScalaFIM remains local and unpushed.

## Correctness evidence

Standalone neuroim4s tests pass on JVM and JS: 52 existing world/identity tests
and 10 new affine-policy tests. An external consumer package tests analytic
rotated anisotropic grids, reflections, even and odd shapes, explicit/default
target shapes, identity rescaling, immutable inputs and rejected invalid inputs.
The even-shape oracle pins the lower central voxel: source `(1,2,3)` maps to
world `(12,14,42)`, and target `(0,1,1)` retains that position. A geometric
midpoint substitution would fail this test. These are analytic policy checks;
no new R-parity or scientific qualification is claimed.

Affected ScalaFIM suites also pass on JVM and JS: image 359/329, image-view 42/42,
and dataset-zarr 15/7, totalling 794 test executions. They cover the actual
rescaling, deobliquity, viewer, NIfTI admission/export and Zarr consumers. Existing
image allocation budgets are unchanged. These 794 executions passed again against
the published immutable pin without local overrides, with zero skips. The fetched
provider HEAD is the approved commit and each provider resolves once in the build.
Every ScalaFIM module also compiles on
JVM and JS without Scala source warnings. Exact qualified source/build blobs and
commands are in [receipt.json](receipt.json). The workspace-wide impact command could not run because the existing catalog
omits neuroim4s and other top-level entries; no catalog change is made here.
Direct caller audit and affected build gates provide this slice's consumer evidence.
No hosted CI or full ScalaFIM
test-all result is claimed.

## Next substantial tranche

Complete `eco-neuroim-core` as an independent image library, including:

- `NeuroVolume`/`NeuroSeries`, selected-image values/windows, masks, series
  sequences, error ADTs and the necessary checked sample-space admission.
- The retained neuroim2 parity families: KMeans, SpatialFilters/Kernel3D,
  Ops/NeuroStats, ParcelNeighborhoods and the connected-component wrapper.
  Preserve scientific behavior and fixtures; generic provider capabilities
  remain owned by their current providers.
- Optional JVM neuroimage IO admission around image4s NIfTI parsing/encoding.
  Keep anatomical world interpretation, affine/scaling evidence and bounded
  access contracts; do not copy the generic format implementation.
- Known ScalaFIM callers, tests, fixtures and examples, deleting the moved
  originals and using neuroim4s directly without compatibility aliases.

A current source census gives 2,744 lines in the central values/selection/mask
families and 1,932 in the listed parity files, before supporting admission,
SpatialFilters, IO and tests. These are source lines, not a final extraction
closure or a claim that every line should move unchanged. Generic aliases and
helpers encountered during the move must be replaced by their provider owner.

Completion requires an independent JVM/JS volume-and-series consumer without
ScalaFIM/surface/atlas/MVPA dependencies, unchanged live image4s/Ravel ownership,
Time-axis and semantic-role admission, ordering/views, affine/scaling behavior,
parity fixtures, JVM IO bounded-read/allocation gates, and affected downstream
JVM/JS gates. The wider core task remains open until those conditions hold.
