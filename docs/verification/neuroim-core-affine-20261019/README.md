# neuroim4s core: affine policy candidate

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

## Candidate and ready cutover

The provider candidate is `9780bcab6c061284a3c8864d9599b0534c71a97f`, based on
`3b7a8441e0fcdfd20428835f8eaec7688b73a9ef`. Its owned clone is
`/private/tmp/scalafim-neuroim-core-20261019`, branch `eco/core-affine-20261019`.
The last observed GitHub main was exactly that parent. Nothing has been pushed.
A recovery bundle and raw evidence live in the shared Git directory under
`verification/neuroim-core-affine-20261019/`.

[consumer-cutover.patch](consumer-cutover.patch) removes ScalaFIM's original
88-line implementation, migrates all ten explicit imports, adds the core module
dependency on both platforms, forwards explicit image4s/spatial4s overrides and
sets the future immutable neuroim4s revision. Existing integration assertions and
fixtures are preserved. The patch is qualified with the exact local provider
candidate; default remote-pin resolution must be verified after publication.
It is retained for review rather than activating an unpublished GitHub SHA in
the default build. ScalaFIM's source remains at the working published provider
revision until publication is authorized.

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
image allocation budgets are unchanged. Every ScalaFIM module also compiles on
JVM and JS without Scala source warnings. Exact qualified source/build blobs and
commands are in [receipt.json](receipt.json). The workspace-wide impact command could not run because the existing catalog
omits neuroim4s and other top-level entries; no catalog change is made here.
Direct caller audit and affected build gates provide this slice's consumer evidence.
No hosted CI or full ScalaFIM
test-all result is claimed.

## Publication and next action

Publication is separate from local qualification. After approval, refresh GitHub
main, publish only the exact neuroim4s candidate as a non-force fast-forward,
apply the saved patch, then run the consumer gates against the immutable remote
pin without overrides. Commit that ScalaFIM cutover locally. ScalaFIM publication
has not been authorized. Continue the remaining volume/series/IO extraction only
after its dependencies and ownership seams are explicit.
