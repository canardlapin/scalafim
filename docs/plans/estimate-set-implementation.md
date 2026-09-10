# Estimate sets: native implementation placement

**10 September 2026 — source audit and implementation decision.** ScalaFIM owns
the general scientific contract and its implementation. PLS Neuro is a consumer.
The user explicitly permits replacing unused, inadequate APIs; preserving their
shape is not an acceptance requirement. This document proposes the concrete
module split and migration. No runtime code is changed or qualified by this audit.

Scientific target: [estimate-set V0, version 0.2.0](../../../plsneuro/docs/first-level-artifact-spec.md).
Acceptance: [physical conformance and performance plan](../../../plsneuro/docs/verification/first-level-artifact-conformance.md).
Native owner: `bd-01KX6G9B8R86MRBZ9S8K8F5G7V`. Consumer integration: PLS Neuro
W5 `bd-01M1YVQ52TAVZB2SN37DT96EAD`. These are separate stores.

## Recommendation

Add a small **`estimates` module, package `scalafim.estimates`**, for identified
scientific numerical products. It admits effects, statistics and uncertainty
without requiring a ScalaFIM fit object. Put physical storage in `estimates-io`
and producer conversion in `fit-estimates`. Reuse and improve `archive` for
general resource, integrity and transaction mechanics. Keep numeric solvers in
`fit`, raw acquisition access in `dataset`, and orchestration in outer consumers.

The core is called estimates rather than first-level because an imported t map,
a pooled contrast, a group effect and a future non-GLM estimate should share
identity, geometry, validity and access rules. V0 still has a bounded volumetric
scope. This is not a promise to design every later analysis format now.

## What exists, and its limits

The inspected checkout is `main` at
`a2d6f95f2a7703b015c156262699138db1d8b70c`. The following findings refer to that
source, not to every branch or retained candidate.

| Existing surface | Finding | Disposition |
|---|---|---|
| [`ResultArtifacts.scala`](../../modules/fit/shared/src/main/scala/scalafim/fmri/fit/ResultArtifacts.scala) | Typed in-memory parameter/contrast maps, provenance, exclusions, df and shared/voxelwise covariance. `ResultManifest` holds actual vectors/matrices and fit-owned provenance, not immutable references or a shared cohort catalog. | Reuse scientific extraction logic; replace its role as the durable contract. Keep fit result values only where useful to computation/inspection. |
| [`ResultManifestWriter`](../../modules/fit/jvm/src/main/scala/scalafim/fmri/fit/io/ResultManifestWriter.scala) | Writes NIfTI, covariance TSV and a JSON sidecar. HDF5/GDS return explicit unsupported errors. It writes directly to final paths; no collection, digest graph, reopening reader or durable transaction appears here. | Replace with the V0 writer/reader. Do not add another exporter beside this one indefinitely. |
| [`FitImageMaps`](../../modules/fit/shared/src/main/scala/scalafim/fmri/fit/ImageMaps.scala) | Provides useful selected image conversion, but `dense` calls `toDense(0.0)` and the exporter calls it. The output path materializes full maps and gives outside-support cells numeric zero. | Retain useful display conversion; remove it from bounded persistence. Write explicit validity/support with scientific products. |
| [`archive`](../../modules/archive/README.md) | Dependency-light, shared/JVM/JS identities, content digests, typed payload descriptors, resource-safe drivers, read observations and staged publication contracts. | Reuse this foundation and its tests. Add missing generic streaming/commit capabilities here. |
| [`CanonicalArchiveWriter`](../../modules/archive/shared/src/main/scala/scalafim/archive/CanonicalArchiveWriter.scala) | `CanonicalPayload` copies an entire payload into `Vector[Byte]`; the write plan contains all payloads. Publication is an abstract transaction, not proof of filesystem durability. | Refactor the production path around bounded producers/prewritten verified objects. A small eager convenience path may delegate to it. |
| [`CanonicalArchiveManifestCodec`](../../modules/archive/shared/src/main/scala/scalafim/archive/CanonicalArchiveManifestCodec.scala) | Deterministic tagged `NAM1` text, not JSON. It preserves raw scalar bits but is a different wire profile. | Reuse exact identity concepts; do not make NAM1 a hidden required second authority for the JSON/TSV V0 profile. |
| [`JhdfLnaHdf5Store`](../../modules/archive-lna/jvm/src/main/scala/scalafim/archive/io/JhdfLnaHdf5Store.scala) | Real HDF5 implementation for LNA payloads. Eager read/write, LNA-specific shape/role rules, atomic move with replacement. | Reuse proven low-level techniques only after factoring them appropriately. It is not a general bounded estimate store and should not impose LNA on estimates. |
| [`JvmNeuroArchivePublisher`](../../modules/archive-zarr/jvm/src/main/scala/scalafim/archive/zarr/JvmNeuroArchivePublisher.scala) | Real staging, hashes, readback and atomic-directory publication, specialized to canonical BOLD/Zarr. This wrapper has no explicit file/directory fsync or collection-pointer CAS. | Extract useful general mechanics; preserve the BOLD profile. Do not claim its existing tests establish V0 crash durability/concurrency. |
| [`WorkflowTypes`](../../modules/fmri-workflow/shared/src/main/scala/scalafim/fmri/workflow/WorkflowTypes.scala) | `ResultBundleRef` is ID + location + format/layout. It lacks a pinned manifest digest, collection membership and operation capabilities. | Replace result references with immutable estimate-set references; keep output intent distinct from a committed result. |
| [`group.FirstLevel`](../../modules/group/shared/src/main/scala/scalafim/fmri/group/FirstLevel.scala) | Consumes in-memory `TContrastResult`, keyed by subject and contrast strings; checks sample count and builds a dense effects/SE² cube. | Replace the primary group bridge with identified estimate inputs and explicit coverage/admission. Move any retained direct-fit convenience adapter outward. |
| [`pipeline.Artifacts`](../../modules/pipeline/shared/src/main/scala/scalafim/pipeline/Artifacts.scala) | Typed graph references and an in-memory value table. | Keep for execution. It is not persistence and should not own statistical metadata. |
| [`response`](../../modules/response/README.md), `dataset`, archived-response interop | Useful ordered selections, read receipts, resource ownership and time-by-sample response access. | Reuse applicable lower-level contracts; do not relabel estimands as acquisition times or force products into an `FmriDataset`. |

The old sidecar also omits the numeric contrast operators and df carried by the
in-memory contrast maps, and it does not persist an explicit covariance scaling
equation. Adding fields alone will not repair its lifecycle and bounded-access
limitations. Searches of current main found no `FirstLevelCatalog`, result
manifest reader, or general result block sink implementation.

## Proposed module boundaries

Arrows below mean “depends on”; this is a proposed graph, not current `build.sbt`.

```text
estimates       -> archive, image (existing geometry/domain), Gale as needed
estimates-io    -> estimates; image4s through the native image bridge;
                  BIDS4s naming/reference adapters; JVM HDF5 provider
fit-estimates   -> fit, estimates
group           -> estimates, existing group numeric/domain dependencies
fmri-workflow   -> fit-estimates, group, existing study/design composition
PLS Neuro       -> estimates-io, fit-estimates, its own jobs/review/method adapters
```

`estimates` has no dependency on `fit`, `model`, `fmri-workflow`, JavaFX, a
scheduler, or a container-specific library. `fit-estimates` does not depend on
physical IO: it lowers native output metadata/blocks to a typed sink interface.
The outer execution owner acquires a concrete sink and runs that adapter.
Readers therefore need no first-level engine, original session or producer plugin.

Suggested source layout:

```text
modules/estimates/shared/.../scalafim/estimates/
  Identity.scala          dataset/model/unit/product/collection identity
  EstimandCatalog.scala   shared targets and unit-local numeric bindings
  Products.scala          effects/statistics/uncertainty and availability
  Uncertainty.scala       covariance/estimability/df descriptors
  EstimateDomain.scala    geometry/sample identity and typed validity
  EstimateSet.scala       immutable manifest references, coverage, revisions
  EstimateSource.scala    inspect/open/read, limits and owned read results
  EstimateSink.scala      declared layout, block delivery, seal/abort contract
  validation/            semantic and operation-capability checks
modules/estimates-io/shared/.../scalafim/estimates/io/
  schema/                 V0 JSON/TSV codecs and validation
  layout/                 explicit physical/logical axis and locator mappings
modules/estimates-io/jvm/.../scalafim/estimates/io/
  nifti/                  native image4s scientific binding, staging and readback
  hdf5/                   bounded datasets/hyperslabs, explicit encoding profiles
  EstimateSetStore.scala  manifest references and collection publication
modules/fit-estimates/shared/.../scalafim/fmri/fit/estimates/
  FitEstimateProducer.scala
```

These filenames are planning seams, not a requirement for one type per file.
Keep HDF5 implementation dependencies on JVM; introduce a separate HDF5 artifact
only if optional dependency weight or platform qualification warrants it. Shared
models/codecs must compile and test on JVM and JS. Physical implementations claim
only platforms actually implemented and qualified; no fake browser file API.

General staged-object transactions, digest/length verification and local commit
primitives belong in `archive/shared` and `archive/jvm`. The estimate layer decides
which scientific units/products constitute a complete collection and how compatible
concurrent additions merge. The generic transaction enforces exclusive immutable
publication and serialized/CAS pointer replacement. Job retries, scheduling and
user feedback remain outside the artifact model. Eidolon or another orchestrator
may provide the transaction capability, but opening or writing local estimate
sets must not require that application.

Do not extend the generic archive's `CanonicalValue` into the public scientific
API. Typed estimate descriptors validate before serialization. Existing archive
digest constructors need profile-level SHA256/length/path checks. Reusing a type
or resource abstraction does not automatically qualify its wire encoding or IO.

## Replacement and integration order

1. **Reconcile the source baseline.** Current main has canonical image geometry
   but no `SelectedEstimates.scala` or `FirstLevelEstimates.scala`. Those APIs are
   in PLS Neuro's retained ScalaFIM patch on `fd992a0c85001eb50ba2e78d29c497693ca494dc`.
   The inspected patch SHA256 is
   `ed388817ffdcf54bf8a9561eb2d8c34d44f039d82b323b1949b49f269988d73c`.
   Its `PreparedSelectedEstimates.foreachBlock` already supplies a bounded consumer
   callback and explicit completion/cancellation outcomes; it does not implement
   a durable transaction. Forward-port and qualify this scientific work rather
   than rewrite it or restore obsolete image wrappers. The independent incremental
   NIfTI bridge is another retained candidate, absent from current main's NIfTI API.
2. **Implement `estimates` and its wire schema/fixtures.** Include statistic-only,
   two-unit/shared-catalog, deficient-contrast, voxelwise covariance, validity and
   geometry cases. This can proceed independently of the first-level forward-port.
3. **Refactor archive transactions and implement Core-NIfTI IO.** Feed blocks
   directly to the image4s writer; test no-clobber, bounded memory, failures at
   every commit boundary, relocation, checksum verification and fresh-process
   readback. Keep JSON/TSV the authoritative profile metadata.
4. **Add fit adapters and replace old result export.** Bind native descriptions,
   requested/retained products, exact operators, covariance scale and outcomes.
   Convert useful `ResultArtifacts` extraction into these adapters. Migrate the
   old exporter tests to the new contract, then remove the old durable-manifest
   facade and unwired format choices. Do not preserve a parallel legacy writer
   solely for unused compatibility; explicit import is justified only by actual
   retained artifacts that need recovery.
5. **Replace downstream input coupling.** Add group access over pinned estimate
   references and bounded slabs. Current `group` imports `fit` in the direct
   `FirstLevel` bridge; migrate that bridge/tests and remove the compile dependency
   after checking all consumers. Replace workflow result-reference fields and wire
   PLS Neuro W5 save/quit/reopen/reuse and its separate scientific admission.
6. **Qualify HDF5 with the same logical fixtures and access workloads.** Both HDF5
   and NIfTI remain the final product target; HDF5 is not the unimplemented enum
   placeholder in the old exporter. Evaluate full-grid, packed and per-estimand
   forms through actual bounded IO. Cache/transposition policy follows measured
   map, slice, ROI, FIR and cohort access, with precision/identity preserved.

The scientific spec should become a canonical ScalaFIM document when schema work
starts, with a PLS Neuro consumer link and admission requirements. Avoid maintaining
two independently edited copies. This audit leaves its existing location intact.

## Existing work and scope corrections

Reuse native transactional sink/catalog issue `bd-01KX6G9B8R86MRBZ9S8K8F5G7V`.
Its older “compose ResultManifest” and “preserve compatibility layout” directions
are superseded by this user's replacement authorization and V0 contract. Its
older optional-HDF5 description is superseded by the staged dual-encoding target.

Keep the related native work:

- `bd-01M21H6P1908Q5PGSZYWQZHWRN`: incremental NIfTI adoption/scientific binding.
- `bd-01M23Q9Q92SBD5PHZZJYVY9EC3`: reconcile selected-output work with canonical image architecture.
- `bd-01KXYJCH0R2W45SAPT38PCHFJJ`: reusable resource descriptors replacing workflow-owned locations.

The resource-descriptor issue is wider than estimate results. The narrow estimate
reference slice can land without redesigning all BIDS/events/mask resources or
waiting for an external scheduler. Review prerequisite edges before beginning an
implementation slice; preserve real provider dependencies, avoid unrelated global
barriers. PLS Neuro W5 tracks adoption and application behavior, not a duplicate
native implementation.

## Evidence boundary

This is a read-only source/tracker audit plus a planning document. Source findings
were checked against the files linked above and the declared provider patch.
Existing tests were inspected; no test suites were rerun and no new runtime pass,
performance result, native dependency adoption or publication is claimed. General
archive tests are useful regression anchors, not conformance certificates for the
new estimate profile. All implementation stages above remain outstanding.

## First implementation increment — 10 September 2026

A bounded first vertical slice now exists in the working tree. See
[implementation scope and remaining gates](../estimate-sets.md) and
[verification evidence](../verification/estimate-set-increment/heap-and-relocation.json).
This does **not** mark the six-stage replacement plan complete.

- Forward-ported the selected-output fit executor/tests onto canonical image
  values, including OLS readout and runwise selected precision pooling.
- Reconciled the incremental image4s writer and checked dimensional refinement
  onto provider main `18ffdce`, then adopted the native bridge in a development
  build. The existing published output branch is divergent and cannot be pinned
  directly. Provider publication is awaiting explicit user authorization after
  automatic approval review rejected the attempted commit/push; the default pin
  consequently still lacks the APIs this working increment requires.
- Added `estimates`, `estimates-io`, and `fit-estimates`; readers do not import
  `fit`. The development schema and JVM scalar NIfTI implementation support
  verified save/reopen, typed per-estimand validity and bounded block delivery.
- Added archive-owned streamed/prewritten immutable object publication and
  serialized discovery-pointer CAS. Tests cover source failure, no-clobber,
  tampering, owned staging and stale updates, not every crash boundary.
- Qualified a full writer/relocated-reader probe under a 64 MiB JVM heap with a
  128 MiB numerical payload; independent Python verifies all 16,777,216 values
  and every validity byte. This is one workload, not general V0 performance
  certification.

Still outstanding: final canonical wire-schema/spec migration and golden fixture
matrix; pair-axis/voxelwise covariance and pooled-fit persistence; more geometry
bindings and gzip staging; HDF5; migration/removal of the old result exporter;
group/workflow and PLS Neuro W5 adoption; complete crash/concurrency/access
qualification. The old writer remains until the replacement covers its useful
scientific extraction. No application pin, source artifact or unrelated dirty
work has been replaced by this increment.


## Continuation — published provider, covariance and group input

The provider prerequisite has landed independently on image4s main `ec56b348`
and is already pinned by ScalaFIM `316a157`. Live refs and exact retained patch
content agree. Ordinary pinned-provider qualification replaces the prior local
override requirement; no duplicate publication was necessary after user approval.

Joint shared OLS now persists normalized covariance with explicit variance scale.
NIfTI supports named upper-triangle pair volumes and per-pair validity, including
voxelwise absolute covariance. A bounded consumer accessor reconstructs selected
matrices and checks PSD through Gale, without claiming estimability or inference.

The group module no longer imports or depends on `fit`. Its primary durable
bridge accepts pinned units and explicit scientific/spatial admission, then reads
bounded complete-case blocks with pinned receipts and one open unit at a time.
The eager direct-fit adapter and its three suites moved to `fit-estimates`.

These changes do not complete V0 interchange conformance. Final JSON/TSV schemas,
compact shared covariance encoding, pooled persistence, general geometry/gzip,
HDF5, old export replacement, workflow/PLS Neuro wiring and the complete
crash/concurrency/performance matrix remain outstanding. See the current
[scope](../estimate-sets.md) and [evidence](../verification/estimate-set-increment/continuation.md).

## Commit checkpoint assessment — 10 September 2026

The architecture is now exercised end to end: native selected OLS can write
identified effects and uncertainty, a separate reader can reopen them, and the
group layer can consume pinned blocks without the first-level fitter. The
provider prerequisite is resolved. This is a working development implementation,
not the completed replacement or a V0-conforming interchange release.

| Original stage | Status | What remains before that stage is complete |
|---|---|---|
| 1. Reconcile source baseline | Resolved for selected execution and incremental output | Keep future producer changes qualified against the published canonical providers. |
| 2. Scientific core and wire schema | Core implemented; wire profile incomplete | Move the canonical spec into ScalaFIM, finalize JSON/TSV schemas and metadata authority, and add independent golden bundles including deficient-estimability cases. The current discriminator explicitly says `development-1`. |
| 3. Archive transactions and Core-NIfTI | Bounded local path implemented and tested | Complete crash/retry/concurrency qualification, general frame bindings and gzip policy, compact metadata/support storage, and the existing eager archive writer's migration. |
| 4. Fit adapters and old export replacement | Shared full-rank OLS adapter implemented | Persist pooled outputs and retained contributions, compact shared covariance, cover the old exporter's useful scientific extraction, then remove `ResultManifestWriter` and its durable facade. Other fit engines need explicit adapters. |
| 5. Downstream decoupling | Group bridge implemented; application workflow incomplete | Replace workflow `ResultBundleRef` identities with pinned references and qualify PLS Neuro save/quit/reopen/retry/reuse. Consumer method admission remains explicit. |
| 6. HDF5 | Not implemented for estimate sets | Build bounded physical storage/readback and run the same logical fixtures and measured access workloads as NIfTI. Existing LNA HDF5 code is not an estimate backend. |

The next implementation milestone should be **a stable Core-NIfTI profile with
independent conformance fixtures**. Finalize the authoritative metadata and
layout, including compact shared covariance and support/observation tables,
before an application creates durable datasets around the development encoding.
The scanner-only binding must also be expanded and qualified for the intended
registered group-analysis frames; matching array shape is not spatial admission.

After that, extend producer coverage and retire the legacy exporter, wire the
workflow/application path, and qualify HDF5 against the same fixtures. Keep
performance and crash-durability gates explicit throughout. Repeated full hashes
per group block, scalar file seeks and physically repeated shared covariance are
known current costs; bounded memory alone does not make them production-ready.

No percentage-complete estimate is assigned: the remaining format, replacement,
application and physical-conformance work is substantial and uneven in size.
