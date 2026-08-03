# NeuroArchive Next — BIDS-compatible, remotely addressable neuroimaging

Status: implementation plan, not yet a format specification\
Working names: Scala Zarr kernel (`scala-zarr`) and NeuroArchive Zarr profile (`neuroarchive-zarr-0.1`)\
Reviewed against the live ScalaFIM tree and external specifications on 2026-07-20

## Decision

Use **NeuroArchive as the umbrella archive system**. Do not create NZA as a
second semantic format beside LNA.

NeuroArchive already has the difficult conceptual core: transform descriptors,
payload roles and references, validation layers, reversible reconstruction,
lazy scientific views, shared content-addressed bases, and a BIDS-derivative
organization. What it does not yet have is the physical substrate required by
the motivating access problem. Its current R format is an HDF5 file, its R
inverse transforms generally read complete payload datasets before subsetting,
and ScalaFIM's current jHDF reader eagerly loads every payload. The current R
and Scala HDF5 layouts are also not the same wire format.

The plan is therefore **NeuroArchive Next**, with two storage families behind
one archive contract and exact concrete profile IDs:

- **LNA/HDF5** remains the transform-aware family for compact latent products
  and local tools; R LNA v2 and `scalafim-lna-hdf5-0` stay distinct profiles
  until differential fixtures prove an adapter;
- **NeuroArchive Zarr** becomes the canonical lossless and remote-access
  profile for chunked arrays and materialized analytical views.

In the rest of this document, **NZA** is shorthand for that Zarr storage
profile. It is not a new archive ontology, project, or independent manifest.

The focused storage-kernel design is
[`neuroarchive-zarr-core.md`](neuroarchive-zarr-core.md). It is normative for
the Scala implementation boundary: ScalaFIM owns an extraction-ready,
runtime-rank Zarr v3 kernel plus a stricter NeuroArchive profile. Rank four is
a `CanonicalBold` refinement, not a property of chunking or range I/O.
zarr-java remains an independent JVM oracle rather than a runtime dependency or
the public/internal API.

The first useful system is deliberately smaller than the motivating proposal:

- one backend-neutral NeuroArchive manifest with payload descriptors separate
  from payload access;
- one reusable cross-platform Zarr kernel for arbitrary finite-rank arrays,
  initially narrow in dtypes, grids, codecs, stores, and mutation;
- one canonical BOLD payload in Zarr v3;
- a Zarr storage profile that makes axes, scalar calibration, geometry, source
  BIDS identity, layout derivation, and completeness explicit;
- lossless raw-scalar import from NIfTI and a validator-clean BIDS/NIfTI export;
- a Zarr implementation of the existing storage-neutral response-block source,
  connected to dataset selections and fit chunk programs;
- JVM filesystem/HTTP and Scala.js Fetch reads through the same pure planner;
- credentialed S3 through an optional JVM adapter when deployment requires it;
- sharding and chunk shapes chosen by a recorded workload benchmark;
- existing LNA/latent products plus optional masked and visualization layouts,
  all tied to the same canonical content revision.

This gives ScalaFIM a credible answer to `.nii.gz` without making the first
release carry SQL catalogs, Parquet mirrors, virtual-array execution, nonlinear
transform registries, global content-addressed storage, and a multilevel cache.

## What we have and what is missing

| Capability | Current NeuroArchive / ScalaFIM | NeuroArchive Next requirement |
| --- | --- | --- |
| Reversible transforms and reconstruction | Present in R NeuroArchive and typed Scala `archive`/`latent` code | Reuse; do not duplicate in Zarr-specific metadata |
| Transform descriptors, payload roles, validation | Present | Lift common semantics into a backend-neutral manifest |
| BIDS-derivative organization and shared bases | Present, using BIDS-style paths and content-addressed basis artifacts | Preserve, but use parsed BIDS entities rather than filename substring matching |
| Lossless canonical BOLD | Raw payloads are possible, but not the primary checked import/export contract | Required Zarr canonical payload with exact scalar and geometry round trip |
| Random selection | HDF5 supports hyperslabs, but most current R paths read whole datasets before selecting; Scala eagerly materializes all payloads | Selection must compile to bounded chunk/range reads before reconstruction |
| Remote object-store access | Not present in the current readers | HTTP/S3 range access with request-trace evidence |
| Multiple physical layouts | Transform payloads exist, but there is no source-revision-aware layout registry | Canonical, masked, summary, and latent layouts share identity and record derivation |
| Wire interoperability | R LNA v2 expects `/transforms`, `/basis`, and `/scans`; Scala writes `/__lna__/...` and only proves Scala round trips | Declare profiles honestly and add R/Scala/Python differential fixtures |
| Geometry and scalar fidelity | R headers are optional; Scala `LnaShape` carries 3D space plus time and a string header map | Full affine, qform/sform, axis, time, dtype, scale, offset, and units contract |
| Integrity and publication | HDF5 file checksum and atomic replacement exist | Multi-object completion receipt, immutable revision, and checked publication |

So the answer to "do we already have it?" is: **we have the semantic center,
but not yet the remote computational store**. That is a much better starting
point than a greenfield format, and it changes the implementation from format
invention to storage/profile convergence.

## Delivery at a glance

| Stage | Concrete outcome | Gate to continue |
| --- | --- | --- |
| M0: converge and spike | NeuroArchive ADR, exact HDF5 profile IDs, common-manifest fixtures, rank-generic Scala Zarr proof, Python/JVM/JS sharded cross-read, HTTP range traces | Cross-language classification is honest and the owned subset avoids full-object download at multiple ranks |
| M1: canonical local | Lossless local Zarr acquisition, canonical-BOLD refinement/publication, faithful NIfTI/BIDS round trip, Zarr response-block implementation, fit-chunk adapter | Independent value/geometry oracles and JVM/JS module gates pass |
| M2: remote | Production HTTP and deployment-driven S3 sharded reads, bounded concurrency, diagnostics, measured chunk/shard default | Read-amplification, request-count, memory, and throughput gates pass |
| M3: views | Masked matrix and existing LNA products registered as revision-bound derived views | Exact/loss policy prevents unsafe substitution; GLM/ROI parity passes |
| M4: repository | Immutable release index, optional Parquet mirrors, query/permission and materialization control plane | ScalaFIM/Eidolon ownership and publication protocol are settled |
| M5: extensions | Summary pyramids, surfaces/grayordinates, multi-echo profiles, nonlinear assets, broader Zarr capabilities | Each addition has its own schema, workload, and external oracle |

The critical path is M0 -> M1 -> M2. M3 is the first optimization layer, not a
condition for a useful remote store. M4 and M5 must not delay canonical access.

## Why this stance is necessary

### BIDS compatibility, not a claim of BIDS compliance

The current BIDS 1.11.1 specification requires imaging data to be NIfTI and
tabular data to be TSV. A Zarr/Parquet tree is therefore not itself a
BIDS-compliant imaging dataset. BIDS does, however, permit non-compliant
derivatives beneath a valid study dataset.

The NeuroArchive Zarr 0.x profile will use this contract:

1. Raw BIDS remains valid and untouched.
2. The profile may live under `derivatives/neuroarchive-zarr/` or at an
   external URI named by the institutional catalog.
3. Every canonical payload records the source BIDS dataset, relative BIDS
   path, parsed entities, source artifact digest, and conversion version.
4. A NeuroArchive export produces an ordinary BIDS/NIfTI tree and runs the
   BIDS validator.
5. We do not call the profile `BIDS-NGFF` or claim a BIDS extension until there
   is implementation evidence and an actual BIDS Extension Proposal.

This is a dual-format release model: BIDS/NIfTI is the exchange and archival
view; NeuroArchive supplies the remotely addressable computational view.

### OME-NGFF is precedent, not the neuroimaging schema

OME-Zarr 0.5 is useful precedent for Zarr v3, named axes, multiscales, labels,
and physical units. Its released transform model only admits scale and
translation for multiscale levels. That cannot faithfully encode an arbitrary
oblique NIfTI voxel-to-world affine with rotation or shear.

NZA will therefore:

- follow the useful OME axis convention (`t`, optional channel, then `z,y,x`);
- use a `neuroarchive` metadata namespace with an explicit Zarr profile ID,
  rather than placing neuroimaging claims under the `ome` namespace;
- store the complete neuroimaging affine and original qform/sform semantics;
- optionally emit a separate OME-NGFF visualization projection when its
  geometry is actually representable.

The canonical acquisition is never resampled merely to make it look like an
OME image.

### ScalaFIM is a scientific library, not the repository control plane

The live tracker is already correcting an older boundary. ScalaFIM retains
reusable scientific specifications, format profiles, compilers, readers,
writers, and numerical kernels. Eidolon owns persisted application workflow
graphs, scheduling, cache/materialization state, publication, permissions, run
lifecycle, and application provenance.

That split applies here:

| Concern | Owner |
| --- | --- |
| Backend-neutral NeuroArchive manifest, named axes, scalar encoding, affine geometry, layout and derivation descriptors | ScalaFIM |
| NIfTI/BIDS import and export adapters | ScalaFIM JVM adapters |
| Generic Zarr metadata, chunk/shard planning, JVM/Scala.js reading, and create-only writing behind narrow capabilities | ScalaFIM `zarr` kernel |
| NeuroArchive Zarr validation, publication, calibration, and scientific import/export | ScalaFIM `archive-zarr` profile adapter |
| Selection-to-layout cost model and scientific view derivations | ScalaFIM pure contracts/kernels |
| Dataset registry, permissions, immutable release pointer, credentials | Eidolon or repository service |
| Cache tiers, eviction, materialization policy, retries, publication lifecycle | Eidolon/runtime |
| Workbench loading, display caches, progress, cancellation, UI | Workbench |
| Bytes, shards, Parquet objects, manifests | Object store or filesystem |

ScalaFIM must not depend on Eidolon. Eidolon adapts its checked resource
descriptors into ScalaFIM inputs and publishes ScalaFIM outputs.

## Existing ScalaFIM foundation

This plan extends live code rather than starting a second data model.

- [`DatasetBackend`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/DatasetBackend.scala)
  and [`DataSelection`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/DataSelection.scala)
  already define typed timepoint/voxel selection and preserve selected indices
  in a `time x voxel` result.
- [`LatentResponseDatasetBackend`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/LatentResponseDatasetBackend.scala)
  already reconstructs selected timepoints and samples without materializing a
  whole latent response. It is the strongest current proof that selection can
  remain part of the scientific contract.
- [`FitChunkPlan`](../../modules/fit/shared/src/main/scala/scalafim/fmri/fit/FitChunks.scala)
  and `FitChunkProgram` already express ordered voxel-block execution.
- [`ResponseBlockSource`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/ResponseBlockSource.scala)
  now supplies the bounded, storage-neutral selected-read seam, and
  [`NiftiResponseBlockSource`](../../modules/dataset/jvm/src/main/scala/scalafim/dataset/io/NiftiResponseBlockSource.scala)
  proves it with positional reads over uncompressed or atomically staged NIfTI
  data. Zarr should implement this contract, not create another source API.
- standalone [`bids4s`](https://github.com/canardlapin/bids4s) supplies typed
  BIDS names, entities, manifests, queries, sidecar metadata, and JVM project
  loading.
- [`archive`](../../modules/archive/README.md) is already the typed Scala home
  for NeuroArchive manifests, transform descriptors, validation, and
  content-addressed basis artifacts.
- [`latent`](../../modules/latent/README.md) already models coefficient-time
  responses and selective reconstruction. Existing LNA transforms should
  become derived NeuroArchive layouts, not be reinvented as Zarr metadata.

Five limits are material to the next storage profile:

1. The response-block source exists, but current fit preparation still obtains
   selected response data through `DatasetBackend` before executing voxel
   chunks. Remote access needs the fit adapter to drive `ResponseBlockSource`
   per work block without first materializing the selected whole run.
2. The current [`Nifti`](../../modules/image/jvm/src/main/scala/scalafim/image/io/Nifti.scala)
   convenience reader still inflates `.nii.gz` and calibrates eagerly to
   `Double`. A bounded `NiftiResponseBlockSource`, a lightweight uncompressed
   Float64 writer, and JVM/nibabel oracle tests now exist, but faithful
   raw-scalar import/export still does not preserve the full declared qform,
   qfac, form-code, intent, timing, and dtype contract.
3. `LnaArchive` joins a manifest to an in-memory `Map[ArchivePath, Payload]`;
   `JhdfLnaHdf5Store.read` loads all referenced datasets. Large remote arrays
   need descriptors and region access without constructing an eager archive.
4. The Scala jHDF layout is explicitly `scalafim-lna-hdf5-0` under
   `/__lna__/...`, whereas the R LNA v2 contract requires `/transforms`,
   `/basis`, and `/scans`. Both use `LNA R v2.0` vocabulary, but current tests
   do not establish wire compatibility. M0 must resolve the naming and adapter
   boundary rather than quietly treating them as one format.
5. No transport-neutral remote resource descriptor exists in this checkout.
   The archive contract needs a narrow immutable locator/identity value without
   importing credentials, retry state, cache policy, or a future application
   workflow type.

## NeuroArchive Zarr 0.1 scope

### Required

- BOLD acquisitions with three spatial axes and one time axis.
- Canonical array order `[t, z, y, x]`.
- A runtime-rank Zarr kernel beneath the profile; the kernel is tested with
  scalar, 2D, 4D, and 5D arrays and contains no rank-four branches.
- Zarr format 3 metadata restricted to a v3.0-compatible subset.
- Regular chunk grid plus optional `sharding_indexed`.
- One mandatory portable codec chain—little-endian bytes, chunk-local gzip,
  and CRC32C—proven by the Python/JVM/Scala.js conformance suite.
  Blosc/Zstandard with byte shuffle is an optional fast capability only after
  benchmark, deployment, and independent-fixture gates. Bitshuffle remains
  deferred.
- Exact stored scalar values for the supported NIfTI scalar types.
- Explicit scale/offset mapping from stored values to physical values.
- Full voxel-to-world affine plus preserved NIfTI qform/sform source metadata.
- Regular or explicit temporal coordinates, TR, and time units.
- Parsed BIDS acquisition identity and source artifact references.
- A completion receipt that prevents a missing chunk from silently becoming a
  valid fill-valued acquisition.
- JVM local read/write and HTTP read adapters.
- Scala.js Fetch/HTTP Range read support with the same plan/result fixtures.
- Presigned/public S3 through HTTP; credentialed S3 is an optional JVM adapter
  promoted only when the repository deployment needs it.
- Import/export and cross-language conformance fixtures.

### Explicitly deferred

- NeuroArchive profiles for multi-echo, complex, phase, coil, and arbitrary
  channel arrays; the generic kernel must not prevent their ranks.
- Surface, grayordinate, parcel, and point-cloud domains.
- Nonlinear transforms and transform-graph persistence.
- Full 4D visualization pyramids.
- Parquet as the canonical form of BIDS tables.
- SQL catalog and query service.
- Global content-addressed chunk keys or cross-study deduplication.
- Mutable or append-oriented published acquisitions.
- Virtual-array execution and a dataframe-style optimizer.
- Scala.js writing and browser-specific cache policy.
- New custom prediction or temporal-delta codecs.
- A BIDS Extension Proposal.

## Logical model

### Separate logical identity from content revision

An acquisition needs two identities:

- `AcquisitionId`: stable, dataset-scoped scientific identity derived from the
  complete BIDS entity key, not a filename substring and not a hash;
- `ContentRevision`: immutable digest of a particular imported content and
  metadata revision.

Do not use one hash as both concepts. Re-importing a corrected sidecar should
retain the acquisition identity and produce a new content revision.

Every physical layout has its own `LayoutId` and names the `ContentRevision`
from which it was derived. A reader may select a view only when its source
revision, axes, domain, and scalar semantics match the acquisition.

### Separate archive meaning from payload access

The current `LnaArchive(manifest, payloads)` is a useful small, eager value for
portable transform tests. It must not become the only NeuroArchive abstraction
for large data. NeuroArchive Next separates three layers:

```text
NeuroArchiveManifest
  acquisition identity, geometry, scalar semantics, layouts, derivations
          |
          v
PayloadDescriptor
  logical role, axes, shape, dtype, storage profile, immutable resource ref
          |
          v
RegionPayloadSource
  validate metadata, plan regions, read bounded bytes, decode requested data
```

The manifest never contains open HDF5 handles, Zarr library objects,
credentials, cache policy, or eager payload matrices. `LnaManifest` remains a
versioned LNA-profile value and gains an explicit adapter to the common
manifest where the mapping is lawful. Existing `LnaArchive` pipelines remain
available while latent readers migrate to payload-backed reconstruction.

The 0.1 common manifest describes exactly one acquisition content revision.
The M4 release manifest is a separate index over acquisition manifests; it is
not a second place to restate their array or transform semantics.
A multi-run LNA archive therefore projects to one acquisition manifest per
run, each referencing the same immutable LNA artifact and its run-scoped
payloads.

This is the key convergence move: Zarr and HDF5 are storage interpreters for
NeuroArchive payload descriptors, not competing scientific data models.

### Canonical axes

The canonical BOLD array is:

```text
shape:           [time, z, y, x]
dimension_names: ["t", "z", "y", "x"]
```

This is preferable to `[x,y,z,time]` for three reasons:

1. it follows the released OME-NGFF ordering convention;
2. C-order storage leaves `x` fastest and one spatial volume contiguous in the
   same linear order used by current NIfTI/`NeuroVec` code;
3. the public API can still select by named axis, so storage order need not leak
   into scientific code.

The wire schema uses validated runtime axis ADTs. NZA 0.1 will not encode every
axis name in singleton types and tuple-level public signatures. Static wrappers
may type common domains (`VolumeTime`, `MaskedTimeSeries`) without making
arbitrary file validation a compile-time fiction.

### Scalar semantics

The canonical array stores the source scalar values, not eagerly calibrated
float64 values:

```text
stored value  -- scale/offset -->  physical signal value
```

Required metadata includes:

- storage dtype and byte interpretation;
- scale and offset, with NIfTI zero-slope normalized to identity;
- physical computation dtype exposed by the reader;
- units or an explicit unknown/arbitrary-signal marker;
- NaN and non-finite policy for floating inputs.

The planned ScalaFIM response-block adapter applies calibration lazily into the
current `Double`-valued fit boundary. Raw integer and floating bits remain available
for exact import/export and independent verification. Float16 and bfloat16 are
not permitted for a canonical view in 0.1.

### Coordinates and source semantics

NZA 0.1 stores:

- discrete axis names and sizes;
- one selected voxel-to-world affine used by ScalaFIM;
- original qform code and parameters;
- original sform code and matrix;
- the rule used to select the working affine;
- spatial and temporal units;
- regular sampling (`origin`, `step`, `count`) or an explicit time-coordinate
  array;
- slice timing as acquisition metadata, not as the time-axis coordinate;
- source NIfTI version, datatype, bitpix, scaling, and relevant intent fields.

World coordinates for every voxel are derived from the affine and must not be
stored redundantly in 0.1. A masked layout stores dense linear voxel indices;
world coordinates remain a typed derived view.

### Completeness and immutability

Zarr fill values make missing chunks look like data unless publication adds a
stronger rule. The writer therefore produces a checked `ArchiveWriteReceipt`
for the Zarr payload with:

- expected logical chunk count;
- written logical chunk count;
- shard/object inventory;
- metadata digest;
- content revision;
- codec and layout version;
- completion state.

Readers open only a published, complete manifest by default. Import writes to a
staging prefix, validates the receipt and round trip, and only then lets the
repository publish an immutable release pointer. The publication action and
conditional object-store update belong to Eidolon/repository code, not the NZA
array reader.

## Physical organization

One acquisition is the unit of independent storage, permission, validation,
and replacement. A study manifest may index many acquisition stores; it need
not place the whole study under one enormous Zarr hierarchy.

Illustrative layout:

```text
dataset-release/
├── neuroarchive.json
├── acquisitions/
│   └── <acquisition-id>/
│       ├── canonical.zarr/
│       │   ├── zarr.json
│       │   ├── bold/...
│       │   ├── mask/...
│       │   └── coordinates/time/...
│       └── latent.lna.h5          # optional LNA profile
└── views/
    └── <content-revision>/
        ├── masked/...       # optional, reproducible materialization
        └── summaries/...    # optional, reproducible materialization
```

The hierarchy is logical. A manifest may point to acquisition and view stores
at separate URIs. It may also reference an existing `.lna.h5` transform
archive or shared basis without copying it into the Zarr hierarchy.

### Canonical layout

```text
role:        canonical-volume-time
axes:        [t, z, y, x]
dtype:       source scalar dtype
compression: independently decodable per inner chunk
mutability:  write once, then publish
```

Chunk and shard shapes are not normative constants. Initial benchmark
candidates for a 96 x 96 x 72 x 1200 run are:

```text
inner chunks [t,z,y,x]
[1, 16, 64, 64]
[4, 24, 32, 32]
[8, 16, 32, 32]
[16,24, 32, 32]

target shard sizes
64 MiB, 128 MiB, 256 MiB
```

The first candidate favors single volumes and slabs; the last favors short
spatiotemporal blocks. The benchmark, not taste, chooses the default profile.

Prediction may occur only inside one independently decodable inner chunk. NZA
0.1 does not invent a temporal predictor codec; a custom codec would reduce
interoperability before we have evidence that it matters.

### Masked and latent analytical views

The first optional dense analytical materialized view is:

```text
data:        [t, voxel]
voxel_index: [voxel] -> dense x-fastest linear index
candidate chunks: [128 or 256, 8192 or 32768]
```

It is a derived, reproducible materialization, not a second canonical dataset.
It is created only when the GLM/connectivity workload benefit justifies its
storage. It records the source content revision, mask revision, voxel order,
derivation version, and completion receipt.

The planner may use it for time-series/GLM requests and fall back to canonical
chunks otherwise. A missing or stale masked view never makes the acquisition
unreadable.

Existing LNA coefficient/basis representations are another class of derived
view. They retain their transform-specific reconstruction contracts and may
remain HDF5-backed initially. A view descriptor records the canonical content
revision, transform pipeline revision, source mask/domain mapping, and error or
loss contract. It is never silently substituted for canonical data when the
requested operation requires exact stored values.

### Visualization views

The first visualization materialization should contain a mean image, standard
deviation image, mask, anatomical reference, and their spatial pyramids. Do not
duplicate the complete 4D BOLD run at every resolution in the first system.

If a viewer needs a time movie, it reads canonical time windows. A full 4D
pyramid needs an explicit measured use case and storage budget.

### Tables

BIDS TSV/JSON remains the compatibility truth. A Parquet or Arrow projection is
an indexed mirror for predicate pushdown and column selection, not the sole
copy. It must retain:

- source BIDS relative path and content digest;
- exact BIDS entity key;
- typed column schema and missing-value semantics;
- a reversible mapping to BIDS TSV/JSON export.

The open `Frame` Arrow-compatible batch work is the right prerequisite for a
portable table boundary. Do not add a JVM Parquet dependency directly to the
shared `bids` module or block NZA array access on it.

## Proposed ScalaFIM placement

The exact names may change during the boundary RFE, but the dependency shape
should be preserved.

### Evolve `modules/archive`; do not add `modules/nza`

The existing cross-project archive module becomes the semantic umbrella:

- backend-neutral acquisition, axis, scalar, affine, revision, payload, layout,
  and derivation ADTs;
- deterministic checked `NeuroArchiveManifest` codec;
- open `StorageProfileId` values and immutable resource descriptors;
- layout validation, completeness receipts, and cost-model inputs;
- an explicit, tested projection from `LnaManifest` where semantics overlap;
- no credentials, open stores, zarr-java classes, cache state, scheduler types,
  or eager full-run payloads in the common manifest.

Keep the current `scalafim.archive.lna` types and eager `LnaArchive` pipelines
source-compatible. They remain useful for small latent artifacts and portable
transform tests. Add the common contract beside them, then migrate consumers
incrementally; do not turn one breaking rename into a prerequisite for Zarr.

The common archive module keeps its low dependency on `image`. The smallest
transport-neutral resource descriptor should land at the lowest acyclic module
boundary selected by the open resource RFE; BIDS entity mapping remains in an
adapter above `archive`. `archive` must not depend on `dataset`, `pipeline`, or
Eidolon.

### Add `modules/zarr`

A cross-project, extraction-ready Zarr v3 kernel:

- an owned, runtime-rank implementation of the initial Zarr v3 subset in
  [`neuroarchive-zarr-core.md`](neuroarchive-zarr-core.md);
- shared exact metadata parsing, executable-capability validation,
  arbitrary-rank shape/chunk/key algebra, region/point plans, codec programs,
  and indexed-shard logic;
- JVM filesystem/JDK HTTP readers and a deterministic create-only local writer;
- Scala.js Fetch/HTTP Range reading and asynchronous codec execution;
- ordinary Zarr fill semantics and no NeuroArchive, BIDS, geometry,
  publication, or model-fitting concepts;
- zarr-java and Zarr-Python used as differential oracles without their types
  escaping the conformance harness;
- shared negative fixtures plus JVM/Scala.js/Python cross-language and request-
  trace fixtures;
- no S3 SDK, cache eviction, retries, catalogs, or mutation in 0.1.

The module uses the repository package namespace (`scalafim.zarr`) but depends
on no ScalaFIM domain module. That is the extraction seam if the implementation
later deserves an independent `scala-zarr` release.

### Add `modules/archive-zarr`

A cross-project NeuroArchive profile adapter above `zarr` and `archive`:

- `CanonicalBold` refinement requiring rank four and `[t,z,y,x]`;
- profile codec/layout policy, scalar calibration, geometry/timing agreement,
  immutable revisions, completeness receipts, and scientific payload hashes;
- Scala.js opening/reading of already published profile data;
- JVM publication and audit adapters;
- no generic array mechanics duplicated from `zarr`.

The current zarr-developers `zarr-java` 0.1.3 release remains valuable
independent evidence because it supports the relevant v3 features. Its generic
array model is not itself the problem; its monolithic transitive
numeric/cloud/codec surface, JVM-only boundary, mutable API, execution policy,
and released suffix-range edge case make it the wrong runtime foundation. A
temporary test/codec adapter is permitted, but the kernel model, planner,
sharding logic, and public capabilities are owned Scala types.

### `modules/dataset-zarr`

A thin adapter above `dataset`, `archive-zarr`, `image`, and `bids`:

- `ZarrResponseBlockSource` on the JVM;
- BIDS entity/source mapping;
- NIfTI-to-NeuroArchive streaming importer and NeuroArchive-to-NIfTI exporter
  at JVM boundaries;
- `BidsProject`/manifest integration after the resource descriptor is fixed;
- no transform registry or second archive manifest.

### Existing LNA and latent integration

- `LnaHdf5Store` remains an explicit local HDF5 profile; it is not relabeled as
  the Zarr backend.
- R LNA v2 and `scalafim-lna-hdf5-0` get distinct format IDs until a
  differential fixture proves a reader or writer is compatible.
- `LatentArchiveCodec` keeps owning transform-specific encode/decode logic.
- A payload-backed latent reader may reconstruct requested samples/timepoints
  without first building an eager `LnaArchive`, but that is an M3 optimization.
- Existing LNA artifacts may be registered as derived views of a canonical
  Zarr revision without conversion.

### Integration points

- Implement the existing storage-neutral `ResponseBlockSource`; Zarr joins the
  live bounded NIfTI implementation behind the same contract.
- `NeuroArchiveDatasetBackend` implements the current `DatasetBackend` for
  ordinary selections. Fit code consumes the block source when executing a
  `FitChunkProgram`, so it does not load a whole selected run before chunking.
- BIDS discovery remains in `BidsProjectLoader`; the adapter maps parsed BIDS
  entities and metadata into an acquisition manifest before payload import.
- Format selection is resolved by a capability keyed by an open `FormatId`,
  not a closed `Nifti | Zarr` enum embedded in the model.
- Visualization uses a spatial-region source, not a forced conversion of every
  box/slab request into millions of arbitrary voxel indices.

An illustrative shared API is:

```scala
enum NeuroAxisKind:
  case Space, Time, Channel, Element

enum LayoutRole:
  case CanonicalVolumeTime
  case MaskedTimeElement
  case LatentTransform
  case SpatialSummary

opaque type StorageProfileId = String

final case class ScalarEncoding private (
    stored: StoredDType,
    scale: ScaleFactor,
    offset: SignalOffset,
    physical: PhysicalDType,
    unit: SignalUnit
)

final case class NeuroArrayDescriptor[D <: NeuroDomain] private (
    id: ArrayAssetId,
    revision: ContentRevision,
    axes: AxisSchema[D],
    scalar: ScalarEncoding,
    geometry: DomainGeometry[D],
    layouts: Vector[LayoutDescriptor],
    derivation: Option[DerivationDescriptor]
)

trait NeuroRegionSource[D <: NeuroDomain]:
  def descriptor: NeuroArrayDescriptor[D]
  def read(region: NamedRegion[D]): Either[ArchiveAccessError, RegionBlock[D]]
```

This is directional pseudocode, not an accepted public signature.

## Workload and benchmark contract

Chunking is a workload decision. The benchmark must record cold and warm runs
over at least three acquisitions: typical 2-3 mm BOLD, high-resolution BOLD,
and a multi-run study-scale sample.

### Required requests

| Workload | Request |
| --- | --- |
| V1 volume | all spatial samples at one timepoint |
| V2 movie | all spatial samples for 16 consecutive timepoints |
| S1 slab | one orthogonal slab at one timepoint |
| R1 ROI window | 32 x 32 x 16 region for 32 timepoints |
| T1 voxel series | 1, 32, and 1024 voxels for all timepoints |
| G1 GLM block | all timepoints for 8k, 32k, and 64k active voxels |
| M1 mask | full mask and selected mask blocks |
| X1 export | sequential full acquisition read |

### Baselines

- remote `.nii.gz` download/decompress;
- current eager ScalaFIM NIfTI reader on `.nii` and `.nii.gz`;
- independently staged uncompressed NIfTI through a reference reader;
- current R LNA/HDF5 reader and Scala eager HDF5 reader on applicable local
  raw/latent fixtures;
- NZA canonical, unsharded;
- NZA canonical, sharded;
- NZA masked view and registered LNA latent view when M3 exists.

### Measurements

- requested scientific bytes;
- compressed bytes transferred;
- request and range-read counts;
- touched logical chunks and physical shards;
- read amplification;
- decode and rearrangement CPU time;
- p50/p95 wall time;
- peak buffers and allocations;
- stored bytes and physical object count;
- import/export throughput;
- cold-cache and warm-cache results kept separate.

### Provisional go/no-go gates

Correctness gates are absolute:

- every storage profile has an unambiguous format ID; R LNA v2 and
  `scalafim-lna-hdf5-0` are never treated as wire-compatible without an
  independent differential fixture;
- conversion between an LNA manifest and the common NeuroArchive manifest
  preserves transform order, payload roles, shapes, domains, loss metadata,
  and reconstruction behavior;
- exact raw scalar values/bits after NIfTI -> NZA -> independent reader;
- calibrated values match nibabel fixtures and the current ScalaFIM NIfTI
  reader wherever its supported datatype/byte-order/scaling surface overlaps;
- qform/sform/qfac and oblique geometry survive semantic round trip;
- selected timepoint and voxel order matches `ResponseBlockSource` exactly;
- exported BIDS passes the current validator and all sidecar/table joins remain
  unambiguous;
- an incomplete array is rejected, never silently fill-valued.

Performance gates are calibrated against the benchmark corpus, but the first
decision threshold is:

- canonical median read amplification at most 4x over the declared V/S/R
  subset workloads;
- masked-view median amplification at most 1.5x over T1/G1 workloads;
- shard reads use byte ranges and do not fetch an entire shard for one inner
  chunk;
- one touched shard costs at most one index request plus coalesced data ranges;
- whole-acquisition sequential throughput is at least 80% of staged NIfTI;
- canonical stored bytes are no more than 1.5x source `.nii.gz` over the corpus,
  unless a recorded latency win justifies an exception;
- importer peak payload memory is bounded by configured chunks/shards, not run
  size.

Failure of a performance gate means tune or change the physical adapter. It
does not invalidate the common NeuroArchive manifest or scientific contracts.

## Verification strategy

### Cross-language format conformance

Generate checked fixtures with NeuroArchive R and a pinned Python Zarr
implementation:

1. R NeuroArchive writes minimal raw, quant, and basis/embed LNA v2 fixtures;
   Scala classifies them by exact profile and either reads them correctly or
   returns a typed unsupported-profile error. Scala writes its current HDF5
   profile and R performs the reciprocal check. The initial test is allowed to
   document incompatibility; it is not allowed to blur the two wire formats.
2. Python writes Zarr v3 arrays at ranks 0, 1, 2, 4, and 5 with each required
   dtype, codec, edge chunk, and sharding case; JVM and Scala.js read selections
   and full arrays where their declared store capabilities apply.
3. Scala writes the same arbitrary-rank cases; Python reads and verifies
   values, metadata, and axes.
4. Corrupt metadata, shard index, checksum, truncated chunk, missing chunk, and
   stale layout revision cases fail with typed errors.
5. An HTTP test server records JVM and Scala.js requests and proves equivalent
   bounded range access.

Do not accept Scala-writer/Scala-reader or R-writer/R-reader round trips as
independent proof.

### Neuroimaging oracles

Extend the existing JVM `NiftiSuite`, nibabel fixtures, and reslice oracle
before claiming a faithful round trip. The lightweight Float64 writer and
current tests are a baseline, not evidence for the raw-scalar export contract.
Required additions are:

- nibabel-generated qform-only, negative-qfac, sform-precedence,
  singleton-4D, and oblique fixtures;
- big-endian and all supported scalar decoders;
- zero-slope identity and nonzero slope/intercept;
- irregular time coordinates and slice-timing metadata;
- exact mask and dense-linear voxel index mapping;
- R `neuroim2` and nibabel checks of exported NIfTI headers and values;
- BIDS validator checks over the complete exported acquisition and sidecars.

### Platform gates

Shared profile, manifest, axis, layout, and planner tests must pass on JVM and
Scala.js. JVM filesystem/NIfTI, optional S3, and zarr-java oracle adapters are
JVM-specific. HTTP reading and supported codec execution are tested on both
JVM and Scala.js; browser reading is not satisfied by compilation alone. A
feature is not complete until both platforms pass:

```text
sbt zarrJVM/test zarrJS/test
sbt archiveJVM/test archiveJS/test
sbt archiveZarrJVM/test archiveZarrJS/test
sbt datasetZarrJVM/test datasetZarrJS/test
sbt datasetJVM/test datasetJS/test
sbt imageJVM/test imageJS/test
(cd ../bids4s && sbt testAll)
sbt fitJVM/test fitJS/test
```

The actual aliases are added only when the modules exist; `compileAll` and
`testAll` remain the repository gates.

## Delivery milestones

### M0 — freeze the contract and prove the backend

Deliverables:

- an ADR declaring NeuroArchive the umbrella, HDF5 and Zarr as storage
  families with explicit concrete profile IDs, and a separate NZA semantic
  format out of scope;
- exact format IDs and an R LNA v2 versus `scalafim-lna-hdf5-0`
  compatibility matrix backed by raw, quant, and basis/embed fixtures;
- backend-neutral NeuroArchive manifest and NZA 0.1 payload-profile schemas
  with positive and negative fixtures;
- exact BIDS compatibility statement and export contract;
- the owned runtime-rank Scala metadata/compiler, planner, and indexed-shard
  proof described by Z0–Z2 of the focused core design;
- zarr-java 0.1.3 dependency/behavior report as an oracle assessment;
- Python/JVM/Scala.js sharded cross-read spike at multiple ranks;
- equivalent JVM and Scala.js HTTP range-request traces;
- benchmark corpus manifest and workload definitions;
- decision on codec chain and chunk candidates; zarr-java may be retained only
  as an oracle or narrow codec escape hatch.

Acceptance:

- the common manifest represents the overlapping LNA concepts without an open
  store or eager payload, and preserves reconstruction semantics in fixtures;
- an R or Scala HDF5 file is classified by exact profile; unsupported wire
  layouts fail explicitly rather than being accepted by name;
- the same 2D, 4D, and 5D sharded fixtures are readable in Python, JVM, and
  Scala.js;
- one subset request demonstrably avoids full object download;
- no rank-four branch exists in `modules/zarr`;
- no zarr-java type escapes the conformance/codec boundary;
- the boundary is compatible with the generic resource-descriptor RFE;
- a written go/no-go receipt is checked into the repo.

Rollback: stop after the pure kernel/planner and conformance fixtures; retain
the NeuroArchive ADR, compatibility matrix, and workload contract, then test a
narrow external interpreter against the same gates.

### M1 — canonical local acquisition

Deliverables:

- common NeuroArchive ADTs and total manifest codec in `modules/archive`;
- non-breaking LNA-to-common-manifest projection for supported descriptors;
- `modules/zarr` generic create-only writer plus JVM/Scala.js readers;
- `modules/archive-zarr` canonical-BOLD refinement and immutable publication;
- a raw-scalar NIfTI header/data reader and writer sufficient for the declared
  round-trip contract;
- a scalar-calibrating Zarr implementation of `ResponseBlockSource` and an
  adapter from fit chunk programs;
- NeuroArchive-to-NIfTI/BIDS exporter with full required header semantics;
- completion receipts and immutable-open validation.

Acceptance:

- all correctness gates pass across the NIfTI oracle matrix;
- Python/JVM/Scala.js cross-read passes for supported reads; Scala/JVM writer
  output is readable in Python;
- OLS/GLS/LSS chunk programs read NZA blocks without first materializing the
  selected whole run or adding storage branches to numerical kernels;
- existing LNA reconstruction and HDF5-profile behavior remains unchanged;
- JVM/JS `zarr` and `archive-zarr` tests plus existing dataset, image, BIDS,
  and fit suites pass.

Rollback: NIfTI remains the reference backend; the new modules are isolated and
no existing public API is removed.

### M2 — remote and sharded access

Deliverables:

- production HTTP hardening and, if required by deployment, a credentialed S3
  adapter behind the same reader protocol;
- `sharding_indexed` reference profile;
- request coalescing, bounded concurrency, cancellation, and structured
  diagnostics;
- benchmark receipts for every required workload;
- cache-key inputs exposed as immutable values, without implementing cache
  ownership in ScalaFIM.

Acceptance:

- range traces and amplification/throughput gates pass;
- object-store credentials never enter shared descriptors or manifests;
- failures distinguish missing, unauthorized, corrupt, unsupported codec, and
  transient transport cases;
- full-run reads remain bounded and cancellation closes resources.

Rollback: publish unsharded or filesystem-only NZA while retaining the same
logical profile.

### M3 — analytical and latent view integration

Deliverables:

- `t x voxel` view plus checked `voxel_index` mapping;
- deterministic derivation receipt tied to mask and source revisions;
- a NeuroArchive view descriptor for existing LNA transform archives;
- pure layout planner that chooses canonical, masked, or latent views according
  to selection shape and the caller's exact/loss contract;
- payload-backed selective LNA reconstruction where profiling shows that it
  avoids material payload reads;
- GLM, connectivity, ROI, and arbitrary-order selection tests;
- materialization request/receipt interface for Eidolon.

Acceptance:

- masked and canonical reads are numerically and index-order identical;
- existing LNA reconstruction matches its registered view and a lossy view is
  never selected for an exact-value request;
- stale or incomplete materializations are rejected;
- T1/G1 amplification gate passes;
- eviction of the view affects performance only, never scientific identity.

Rollback: remove/recompute the derived view and use canonical chunks; existing
standalone LNA readers remain available.

### M4 — release catalog and table projection

This is a cross-project milestone after the Eidolon/resource boundary settles.

Deliverables:

- immutable dataset release manifest and conditional publication protocol;
- typed acquisition/table index;
- optional Parquet mirrors for participants, sessions, acquisitions, events,
  confounds, and QC;
- query and permission service in the repository layer;
- export mapping back to BIDS TSV/JSON;
- cache/materialization policies in Eidolon.

Acceptance:

- logical identity and content revision are independently queryable;
- a release is immutable and reproducible;
- unchanged objects may be reused without global cross-study deduplication;
- authorization covers manifests and data objects consistently;
- a catalog outage does not change the bytes or manifest of a published
  release.

### M5 — advanced domains and visualization

Only measured consumer needs may promote:

- summary/anatomical pyramids and optional OME projection;
- multi-echo/channel arrays;
- surface, grayordinate, parcel, and sparse operator mappings;
- nonlinear transform assets using `image`/`spatial` domain semantics;
- virtual derivation descriptors.

Each addition needs its own domain schema, parity fixture, and workload gate. It
does not expand NZA 0.1 by implication.

## Candidate evaluation

Impact, effort, and risk use 1 (low) to 5 (high). Confidence measures the
strength of current evidence, not implementation certainty.

| # | Candidate | Impact | Effort | Risk | Confidence | Verdict | Reason |
| ---: | --- | ---: | ---: | ---: | ---: | --- | --- |
| 1 | NeuroArchive as the umbrella contract | 5 | 2 | 2 | 98% | Keep | Reuses the live transform, validation, reconstruction, and shared-basis model. |
| 2 | Independent NZA semantic format | 2 | 5 | 5 | 98% | Reject | Duplicates NeuroArchive identity, descriptors, validation, and provenance. |
| 3 | Backend-neutral manifest separated from payload access | 5 | 4 | 3 | 95% | Keep | The current eager `LnaArchive` shape cannot represent remote region reads. |
| 4 | Preserve LNA/HDF5 as an explicit storage profile | 4 | 2 | 2 | 95% | Keep | Protects current latent work without pretending HDF5 solves object-store access. |
| 5 | R/Scala LNA differential fixtures and exact profile IDs | 5 | 3 | 2 | 98% | Keep | Current HDF5 layouts differ; convergence requires evidence, not shared naming. |
| 6 | Zarr v3 canonical BOLD payload | 5 | 3 | 2 | 95% | Keep | Directly fixes selective access and has a current JVM implementation candidate. |
| 7 | Indexed sharding | 5 | 3 | 3 | 90% | Keep | Standard answer to object/inode explosion; must prove range behavior. |
| 8 | Canonical `[t,z,y,x]` order | 5 | 2 | 2 | 95% | Keep | Aligns OME ordering, C-order volume contiguity, and current NIfTI linearization. |
| 9 | Preserve raw dtype plus scale/offset | 5 | 4 | 3 | 90% | Keep | Saves space and enables faithful export; requires a raw-scalar importer. |
| 10 | Typed BIDS acquisition mapping | 5 | 2 | 2 | 95% | Keep | Existing BIDS/entity code makes filename-free identity practical. |
| 11 | Streaming NIfTI import plus validator-clean export | 5 | 4 | 3 | 95% | Keep | This is the concrete migration and BIDS-compatibility guarantee. |
| 12 | Existing response-block source plus Zarr/fit adapters | 5 | 3 | 3 | 97% | Keep | Connects the live bounded-read seam and typed fit chunks without eager whole-run reads. |
| 13 | Runtime-rank Scala 3 Zarr kernel; zarr-java as oracle | 5 | 4 | 3 | 92% | Keep | Generic dimensionality costs little once planning is axis-loop based; capability breadth stays deliberately narrow and JVM-only dependencies stay out. |
| 14 | Python/Scala Zarr differential fixtures | 5 | 3 | 1 | 98% | Keep | Independent proof is mandatory for a format implementation. |
| 15 | JVM/Scala.js HTTP plus optional credentialed S3 | 5 | 3 | 3 | 90% | Keep | HTTP Range is the common access contract; an SDK adapter follows only from deployment need. |
| 16 | Range-request trace tests | 5 | 2 | 1 | 98% | Keep | Prevents fake remote-random-access claims. |
| 17 | Workload-driven chunk planner inputs | 4 | 3 | 2 | 90% | Keep | Avoids one global chunk folklore; adaptive learning can follow. |
| 18 | Optional masked `t x voxel` view | 5 | 4 | 3 | 90% | Keep | Directly serves GLM/connectivity after canonical correctness lands. |
| 19 | Register existing LNA archives as derived views | 5 | 3 | 3 | 90% | Keep | Turns prior latent work into an asset instead of a parallel data island. |
| 20 | Source-revision and loss provenance for every view | 5 | 2 | 2 | 95% | Keep | Prevents drift and unsafe substitution across physical representations. |
| 21 | Summary/anatomical spatial pyramids | 4 | 3 | 2 | 85% | Maybe | Valuable for the workbench after canonical remote reads are measured. |
| 22 | Full 4D BOLD pyramid | 2 | 5 | 4 | 80% | Reject | Large storage multiplier without a demonstrated first-use case. |
| 23 | Parquet mirrors for metadata tables | 4 | 4 | 3 | 85% | Maybe | Useful at study scale, but not a blocker for array access or BIDS compatibility. |
| 24 | SQL catalog inside ScalaFIM | 2 | 5 | 5 | 98% | Reject | Conflicts with the established Eidolon/repository ownership boundary. |
| 25 | Immutable release manifest | 5 | 4 | 3 | 90% | Keep | Required for reproducibility and completeness; publication lives outside core. |
| 26 | Content-address every Zarr chunk key | 3 | 5 | 5 | 75% | Reject | Breaks ordinary layout/interoperability and complicates sharding updates. |
| 27 | Multi-tier cache implemented in ScalaFIM | 3 | 5 | 5 | 98% | Reject | Cache ownership and eviction belong to Eidolon/runtime. |
| 28 | Virtual-array DSL in the first profile | 3 | 5 | 5 | 85% | Reject | Premature optimizer/execution work before physical correctness is proven. |
| 29 | Claim OME-NGFF conformance for canonical fMRI | 2 | 3 | 5 | 98% | Reject | Released OME transforms cannot encode arbitrary oblique affine geometry. |
| 30 | Start a BIDS Extension Proposal now | 3 | 5 | 5 | 90% | Reject | Standardization should follow two implementations and operational evidence. |

## Prioritized selected work

### Quick wins

1. Record the NeuroArchive umbrella ADR, exact HDF5 profile IDs, BIDS
   compatibility statement, and R/Scala compatibility fixtures.
2. Freeze the backend-neutral manifest and NZA 0.1 profile fixtures, then add
   the multi-rank Python/JVM/Scala.js sharded conformance and HTTP range spike.
3. Record the benchmark corpus and exact workload definitions.

These are low-regret because they remain useful if a platform backend changes.

### Medium-sized improvements

1. Add the generic `zarr` kernel and common manifest beside LNA, then implement
   the canonical-BOLD profile, local writer, and NIfTI round trip.
2. Add sharded HTTP/S3 reads with diagnostics and bounded concurrency.
3. Register existing LNA products as derived views and add the masked
   analytical view only after benchmark evidence.

### Larger strategic bets

1. Publish immutable NeuroArchive releases and Parquet query projections in
   the repository/Eidolon layer.
2. Add workbench summaries and optional OME projections.
3. Extend the domain to grayordinates, surfaces, multi-echo, and nonlinear
   transforms with separate conformance profiles.

## Risks and mitigations

| Risk | Mitigation | Rollback |
| --- | --- | --- |
| Zarr profile is mistaken for BIDS-compliant primary data | State the dual-format contract in metadata, docs, and CLI output; validate exports | Keep raw BIDS as authority |
| A parallel NZA ontology reappears during implementation | Make NeuroArchive identity/layout/derivation types the only semantic manifest; keep Zarr metadata profile-specific | Reject the duplicate manifest at review |
| R LNA v2 and Scala HDF5 are confused because both say LNA | Assign exact profile IDs and differential fixtures before compatibility claims | Keep both readers explicit and separate |
| Common-manifest work destabilizes current latent code | Add adapters beside `LnaManifest`; retain eager `LnaArchive` and existing tests | Revert the adapter without removing LNA |
| Extraction-ready design becomes an excuse for full Zarr scope | Keep dimensionality generic but capability breadth budgeted; enforce stop gates and explicit unsupported-feature tests | Retain the lawful kernel/fixtures and use narrow external adapters for unpromoted features |
| zarr-java oracle behavior is defective or incomplete | Keep Python as the primary independent oracle and classify zarr-java mismatches explicitly | Remove it from the harness without changing the core |
| One layout performs badly for half the workloads | Benchmark canonical candidates; add masked view as optional derived materialization | Fall back to canonical or staged NIfTI |
| Missing chunks silently read as fill | Require a complete published manifest and checked write receipt | Reject the acquisition |
| Raw scaling is ignored by generic clients | Make scalar semantics required profile metadata; provide calibrated API/export | Offer an explicitly derived calibrated-float view |
| Multiple views drift | Tie each view to source/mask revisions and validate domain/order | Evict and rebuild the view |
| Oblique geometry is flattened into scale/translation | Preserve full affine and qform/sform; do not claim OME conformance | Export/use NIfTI geometry |
| Object-store writes race | Write immutable staging prefixes and conditionally publish a manifest pointer | Leave prior release current |
| Cache policy leaks into core | Expose only immutable keys/costs; keep cache service in Eidolon | Run uncached |
| Digests leak cross-study equality | No global dedupe by default; scope access and reuse to authorized releases | Disable reuse across security domains |
| Schema grows into a database | Hold 0.1 to acquisition data and manifest semantics | Add separate catalog service |

## Recommended first three slices

1. **Freeze convergence before refactoring.** Add the NeuroArchive umbrella
   ADR, exact storage-profile IDs, a minimal common-manifest schema, and R/Scala
   raw/quant/basis HDF5 fixtures. Record precisely which current files are and
   are not wire-compatible.
2. **Implement Z0–Z3 as a conformance-first storage kernel.** Produce direct
   and start-indexed 2D, 4D, and 5D fixtures; read them in Python, JVM, and
   Scala.js; trace equivalent HTTP subset reads; and use zarr-java only as a
   third-party JVM differential oracle. Then refine the 4D fixture into
   canonical BOLD above the kernel.
3. **Implement M1 canonical local access and BIDS round trip.** Do not begin the
   masked/latent view planner, Parquet, catalog, or cache until this passes the
   full NIfTI oracle and cross-platform gates.

In parallel, complete or explicitly re-scope
`bd-01KXYHVBTCJ05W4EX3GY80DS1X`, `bd-01KXYJC5CK664MZZ9AH8PC0M3M`, and
`bd-01KXYJCH0R2W45SAPT38PCHFJJ` before M1 publishes resource-bearing APIs, so
the manifest does not fossilize `WorkflowArtifactRef`.

This order removes the three largest unknowns—NeuroArchive convergence,
resource ownership, and the bounded Scala Zarr implementation—before the schema
accumulates expensive commitments.

## Unknowns to resolve in M0

- Which representative institutional datasets may be used for benchmark
  receipts without moving protected data into fixtures?
- Which R LNA v2 concepts map losslessly to the common manifest, and should
  `scalafim-lna-hdf5-0` gain an R adapter or remain a Scala-private profile?
- Does the owned two-phase reader produce exact index-prefix and inner-range
  requests against realistic HTTP stores under the declared amplification
  gate?
- Does the mandatory gzip/CRC32C chain pass Python/JVM/Scala.js cross-read, and
  does optional Blosc/Zstandard justify its native deployment surface?
- What canonical chunk/shard profile wins across the required workload corpus?
- Which NIfTI header fields beyond the current `NiftiHeader` must be retained
  for the promised semantic export?
- Should the Zarr 0.1 profile include a brain mask, or permit a canonical
  acquisition before any mask exists?
- What generic resource descriptor emerges from the live Eidolon boundary RFE?
- Does the first repository deployment require S3 only, or also plain HTTPS
  with no listing capability?

## Evidence and external specifications

Current implementation evidence for the NeuroArchive decision:

- `~/code/neuroarchive/README.md` defines the core as transform-aware HDF5
  archives, validation, lazy reconstruction, and BIDS-style group archives.
- `~/code/neuroarchive/inst/spec/lna-v2-validation.md` requires the R LNA v2
  `/transforms`, `/basis`, and `/scans` structure.
- `~/code/neuroarchive/R/reader.R` defers reconstruction, but current inverse
  paths such as `transform_quant.R` call full dataset reads before ROI/time
  slicing; `h5_read_subset` is not the general execution substrate.
- [`JhdfLnaHdf5Store`](../../modules/archive/jvm/src/main/scala/scalafim/archive/io/JhdfLnaHdf5Store.scala)
  reads all manifest payloads and writes the distinct `/__lna__/...` layout.
- [`LatentArchiveDatasetBackend`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala)
  reconstructs and caches a dense run before applying `DataSelection`.
- [`LatentResponse`](../../modules/latent/shared/src/main/scala/scalafim/latent/LatentResponse.scala)
  already supplies the selective scientific reconstruction contract worth
  preserving behind a payload-backed reader.

- [BIDS 1.11.1 common principles](https://bids-specification.readthedocs.io/en/stable/common-principles.html)
  define the present NIfTI/TSV requirements and permit non-compliant
  derivatives.
- [Zarr v3 core specification](https://zarr-specs.readthedocs.io/en/latest/v3/core/)
  defines N-dimensional arrays (including rank zero), regular chunk grids,
  dimension names, codec chains, partial decode, and store abstractions.
- [Zarr indexed sharding specification](https://zarr-specs.readthedocs.io/en/latest/v3/codecs/sharding-indexed/)
  defines independently addressable inner chunks inside coarser storage
  objects and range-readable shard indexes.
- [OME-Zarr 0.5](https://ngff.openmicroscopy.org/0.5/) is the released Zarr v3
  multiscale precedent and documents its axis order and scale/translation
  limits.
- [zarr-developers/zarr-java](https://github.com/zarr-developers/zarr-java)
  is a JVM differential oracle; its 0.1.3 behavior is evidence, not the
  ScalaFIM storage model.
- [`neuroarchive-zarr-core.md`](neuroarchive-zarr-core.md) records the exact
  runtime-rank kernel subset, Scala 3 algebra, cross-platform read plans,
  create-only writer, NeuroArchive refinement/publication contract, and
  conformance-first delivery sequence.

## Tracker alignment

This parent plan is recorded under `bd-01KXYP6T25X6H8YGMTBBTCPFKR`. The focused
Scala 3 storage-kernel decision began under
`bd-01KXZ04WBM2VP0G1MXB56M17TS`; the runtime-rank/library-profile refinement is
recorded under `bd-01KXZHMD8XSKA18NZWR9TDYDED`.

It does not supersede the out-of-core fit work. It provides the concrete
storage use case for connecting the live scientific response-source seam to
fit execution. The live Eidolon-boundary RFEs remain prerequisites for shared
resource identity, publication, and cache ownership; M0 conformance work may
proceed without changing those APIs.
