# Estimate persistence continuation — 10 September 2026

This continuation adds covariance persistence and bounded group reuse. It is not
full V0 interchange, HDF5 or downstream-application qualification.

## Provider publication and baseline

On resumption, image4s main was already published at
`ec56b34806c22e26c28ecbd366ef2e323195fc88` and ScalaFIM had adopted it in
`316a15758686c4693f97c5ac84376a3382a37b95`. Both live remote refs were verified.
The retained 12-file image4s prerequisite patch passes reverse-application against
published source. No duplicate provider branch, commit or push was needed.

The initial pinned-provider regression run passed image 305/284, dataset 76/62
and fit 295/286 JVM/JS tests. No provider override was used. The earlier temporary
provider path is no longer a requirement.

The final isolated candidate is based on ScalaFIM
`faeba828723cb89696f96fdfc481499304f4fe36`, plus the declared estimate/group changes.
Concurrent later design commits are outside that candidate. Its retained [source manifest](continuation-source-manifest.json),
[source patch](continuation-source.patch) and [logs](continuation-test-logs.json.gz)
identify the exact candidate. All 67 changed source/build/test paths match the
working tree; the patch passes reverse-application against that candidate.

The isolated `scalafimCompileAll` gate passed without warnings, with no local
provider overrides. Final focused JVM tests passed estimates 10, estimates-io 9,
group 49 and fit-estimates 11. The latter includes the relocated direct-fit suites
and three physical readback/group tests. Final isolated JS tests passed estimates
10, estimates-io 3, group 48 and fit-estimates 8 (79 JVM / 69 JS in this final
focused set). These counts are separate from the earlier image/dataset/fit runs.

A fresh 64 MiB writer and separate 64 MiB reader again wrote and reopened a
128 MiB numerical payload after directory relocation. The independent Python
checker verified all 16,777,216 values and validity bytes. Both exported reader
and group compile classpaths omit the fitter. See the
[heap/relocation receipt](continuation-heap-and-relocation.json). This retains the
first probe's workload boundary; it does not certify covariance/cohort throughput.

## Scientific behavior

- Named upper-triangle covariance-pair selections preserve caller pair/sample
  order, reject unknown/reversed/duplicate pairs and enforce block capacities.
- NIfTI pair volumes carry an explicit ordered mapping and per-pair UInt8 validity.
  Both normalized shared OLS covariance and voxelwise absolute covariance reopen.
  The current normalized encoding repeats the shared matrix over samples; compact
  shared JSON/TSV matrices remain outstanding.
- `CovarianceAccess.matrix` applies variance scaling once and verifies the requested
  principal matrix through Gale under an explicit consumer order/tolerance budget.
  It rejects unavailable entries and checks normalized U before multiplying,
  including when variance scale is zero. It does not authorize a new contrast or
  inference, or certify every matrix in the file.
- The OLS fixture has X=[t,1], t=0..3, giving inverse(X'X)=[[.2,-.3],[-.3,.7]].
  Its second voxel's residual [2,-2,-2,2] has variance SSE/df=8. Readback independently
  verifies reordered absolute covariance [[5.6,-2.4],[-2.4,1.6]].
- The voxelwise fixture verifies actual NIfTI bytes for ordered pair volumes and
  retains an invalid off-diagonal entry without turning it into zero uncertainty.

## Group boundary

`group` now depends on `estimates`, not `fit`. The old eager direct-fit adapter
and three suites moved to `fit-estimates.FitGroupAdapter`; its production edge
points outward to `group`, without a dependency cycle.

`EstimateGroup.prepare` takes pinned inputs, catalog IDs, a total effect/variance
cell budget, and a required consumer admission capability. Shape or affine equality
alone is not scientific alignment. Repeated participant rows, mixed pooling scopes,
conflicting catalogs and mismatched marginal axes are refused before opening payloads.

Each requested block opens and verifies one unit at a time, closes it on success
or failure, and returns group values plus the exact pinned input/axis receipt.
Missing or invalid cells refuse the complete block without shrinking the cohort.
SE is squared once; zero variance remains incompatible with the existing weighted
group estimator. All verification scans currently repeat for each new block; this
is a resource bound, not a cohort throughput claim.

The physical producer/save/reopen/group regression checks two persisted units.
A separate group JVM runtime test proves the first-level fitter is absent from
its classpath. Portable tests cover ordered slabs, budget checks, resource closure,
invalid cells, cancellation, duplicate participants and refused alignment.

## Remaining plan

Final canonical JSON/TSV schemas/spec migration and golden bundles; compact shared
covariance and pooled-fit persistence; general geometry/gzip; HDF5; migration/removal
of ResultManifestWriter; workflow and PLS Neuro adoption; complete crash/concurrency
and access-performance qualification remain open. No full-plan completion is claimed.
