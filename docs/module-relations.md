# ScalaFIM Module Relations

This is a quick orientation map for coding agents. Use it before adding a new
type, dependency edge, or convenience helper.

ScalaFIM is not organized by the original R package names. The R packages are
semantic references; ScalaFIM is organized by typed computational layers. Add
code to the lowest module that can own it without depending on a higher-level
workflow.

## Dependency Shape

Live dependency edges are declared in `build.sbt`.

`provider-contract` is an unpublished public-provider composition court. Its Test
configuration retains locus laws and the old foundation's eight provider admission
witnesses; production axis/observation/reindexing laws live in mvpa4s. No production
module depends on this court. The old identity prototype, standalone pin runner,
and standalone locus4s core facades are retired. See the [U6 custody ledger](verification/eco-u6-cleanup-20261022/ownership.md).

`ecosystemBoundaries` checks actual transitive provider Compile classpaths on
JVM and JS, plus retired source authorities and loaded Git-pin coherence. It
runs before compile-all and the bounded headless repository test campaign.

The estimate artifact modules form a separate reader boundary:

```text
estimates     -> archive, standalone neuroim4s-core
estimates-io  -> estimates
mvpa-artifacts -> mvpa4s, estimates, archive
fmri-workflow -> fit, group
```

`fmri-workflow` uses `estimates-io` only in its test configuration for physical
readback tests. A production reader has no dependency on the fitter. The group
bridge now reads pinned estimate inputs; workflow result references remain to
be migrated. See [estimate sets](estimate-sets.md).

```text
Provider libraries (no ScalaFIM backedge)
  neuroim4s-spaces -> spatial4s
  neuroim4s core/IO/transform/routing -> image4s, reframe4s, locus4s, Gale, Ravel
  neurosurf4s core/IO -> spaces, mesh4s, locus4s, spatial4s, image4s-geometry, graph4s, zarr4s
  neurosurf4s optional transport/volume -> reframe4s and neuroim4s core/IO
  neurosurf4s view/raster -> surface core and Intaglio; no volume stack
  neurosurf4s view-link -> view and reframe4s; no volume stack
  neurosurf4s view-volume -> view and optional volume integration
  neurosurf4s JavaFX/Three -> view/raster; toolkit dependencies stay platform-specific
  neuroatlas4s core/IO -> locus4s/JDK; optional volume -> neuroim4s routing/IO
  neuroatlas4s surface/IO -> neurosurf4s core/IO; optional routing -> volume + surface + neurosurf routing
  mvpa4s core -> Gale, locus4s, Multivar, Resample4s
  mvpa4s predictive -> Alder; mvpa4s locus -> locus4s-data
  mvpa4s surface -> mvpa4s locus, neurosurf4s core
  mvpa4s atlas -> mvpa4s locus, neuroatlas4s core

ScalaFIM functional composition
  response -> response-laws, dataset, latent
  hrf -> design -> fit -> group
  dataset + fit + group + bids4s -> fmri-workflow
  archive -> archive-lna/archive-zarr -> interop-archived-response, dataset-zarr
  estimates -> estimates-io; fit + estimates -> fmri-workflow
  mvpa4s + pipeline/fit/dataset/estimates -> mvpa-* integration modules
  connectivity + neurosurf4s-view -> surface-view-connectivity
  neurosurf4s routing + neuroim4s scoped IO -> spatial (functional fields)
  neuroatlas4s optional volume/IO -> atlas/workflow examples (functional composition)
```

Surface modules, their unpublished fixtures, examples and parity tooling now live
in [neurosurf4s](https://github.com/canardlapin/neurosurf4s). Core, IO, scenes,
raster and typed cursor links have no volume-stack dependency; projection and GPU
volume rendering are explicit optional modules. Root keeps the functional
connectivity adapter, mixed field composition and atlas/MVPA integrations.


Graphics is developed in standalone
[`Intaglio`](https://github.com/canardlapin/intaglio). ScalaFIM pins an exact
public source revision and depends directly on the smallest required Intaglio
core or backend project. The completed handoff and verification boundary are
recorded in [`plans/graphics-extraction.md`](plans/graphics-extraction.md).

The local typed dataframe modules formerly incubated here were extracted to the
standalone [`frame4s`](https://github.com/canardlapin/frame4s) repository. They
are no longer part of the ScalaFIM build or internal dependency graph.

`pipeline` depends only on standalone graph4s for validated DAG layering. It
owns generic graph orchestration without forcing workflow dependencies into the computational core;
`fmri-workflow` will add that edge when its orchestration lowering lands.
Reusable BIDS semantics live in standalone
[`bids4s`](https://github.com/canardlapin/bids4s), which has no ScalaFIM
dependency. ScalaFIM pins an immutable bids4s source revision. Only motion JVM,
archived-response JVM interop, `fmri-workflow`, and `dataset-zarr` consume that
boundary; the core `dataset` module does not.

Generic Zarr metadata, stores, planning, codecs, readers, writers, and caches
now live in standalone `zarr4s`, along with its optional Blosc/Zstandard
provider. ScalaFIM consumes the core as a pinned source build. The temporary
adjacent checkout is selected automatically during extraction; an explicit
`-Dscalafim.zarr4s.build=/path/to/zarr4s` selects another local checkout.

## Module Roles

| Module | Owns | Depends On | Do Not Put Here |
| --- | --- | --- | --- |
| `pipeline` | Generic typed pipeline graphs, artifact references, graph4s-delegated deterministic DAG staging, local pure execution, and structured receipts. | standalone graph4s | Neuroimaging algorithms, file IO, external CLI execution, scheduler/runtime implementations, or lower-module convenience helpers. |
| `response` | Axis-safe identities and ordered selections, neutral time/sample schemas, owned row-major `Double` response blocks, effectful read planning, axis-keyed locality capabilities, and read receipts. Source lineage is the provider-owned `locus4s.provenance` DAG, used directly. | Nothing internal; standalone Ravel core and locus4s-provenance; Cats Core and Cats Effect externally. | A second lineage DAG or identifier grammar, general tensors, mutable public buffers, image/surface geometry, dataset hierarchy, archive formats, representation codecs, storage interpreters, or fit policy. |
| `hrf` | HRFs, basis functions, sampling frames, convolution primitives. | Nothing internal. | Design formulas, datasets, or model fitting. |
| `ar` | AR/ARMA whitening plans and pure prewhitening kernels. | standalone Gale | GLM fitting orchestration or dataset IO. |
| `design` | Event models, formulas, baselines, contrasts, and design metadata. | `hrf`, standalone Gale | Dataset execution, numerical fit engines, or renderer-neutral/concrete graphics. |
| `phrf-comparison` | Unpublished comparative-evaluation preparation/scoring and JVM runners/custody. | `ar`, `hrf`, `fit`, standalone Gale | Production workflow orchestration or claims of completed S10 rehearsal/pilot qualification. |
| standalone `neuroim4s-core` / optional `neuroim4s-io` | Volumes, time series, masks, selected images, sampling, orientation, world binding and neuroimaging policy. The local image module is removed. | neuroim4s-spaces; image4s, locus4s, Ravel, Gale, reframe4s; optional image4s NIfTI on JVM | ScalaFIM, atlas, surface, MVPA or rendering dependencies; duplicate provider geometry, axes, finite domains or codecs. |
| `provider-contract` | An unpublished JVM/Scala.js compile contract for direct provider composition; it owns no runtime abstraction. | standalone image4s, image4s-locus, locus4s, Ravel, Gale, and reframe4s | ScalaFIM packages, adapters, wrappers, policies, error algebras, or production runtime code. |
| standalone `neuroim4s-transform` / optional `transform-io` | Toolkit transform formats (ITK/ANTs, FSL FLIRT/FNIRT, AFNI, FreeSurfer, X5), their interpretation as frame-typed world-space transforms, toolkit conversion, writers, and gated numerical inverses of dense warps over the provider solver. | standalone `neuroim4s-core`; standalone image4s-geometry and reframe4s (`lie`, `field`, `resample`); jHDF on JVM | Generic transform algebra or solvers (reframe4s), generic frame identity (`spatial4s`) or anatomical world identity (`neuroim4s-spaces`), platform IO in shared sources (the provider transform boundary checks), or graph routing (`spatial`). |
| standalone `neuroim4s-view` | Renderer-neutral world-space slice views: typed colorizers/layers, orthogonal scene compilation, crosshairs, orientation labels, and panel receipts. | standalone `neuroim4s-core`, standalone Intaglio core | NIfTI IO, mutable toolkit widgets, DOM/JavaFX lifecycle ownership, or concrete renderer command interpretation. |
| `standalone neuroim4s-view-canvas` | Browser Canvas rendering host plus canvas-relative pointer/wheel translation into pure viewer actions. | standalone `neuroim4s-view`, Intaglio Canvas | Image geometry, DOM ownership, application state mutation, or alternate renderer logic. |
| `standalone neuroim4s-view-java2d` | Java2D rendering host plus device-relative event translation and `BufferedImage` convenience rendering. | standalone `neuroim4s-view`, Intaglio Java2D | Image geometry, Swing lifecycle ownership, or alternate renderer logic. |
| `standalone neuroim4s-view-javafx` | JavaFX Canvas rendering host plus device-relative event translation through the toolkit-free graphics context boundary. | standalone `neuroim4s-view`, Intaglio JavaFX | Image geometry, JavaFX application/thread lifecycle ownership, or alternate renderer logic. |
| `threshold` | Spatial inference over statistic maps: locus-backed active/full support, scored candidates, octrees, set scoring, and maxT-style correction. | standalone `neuroim4s-core`, standalone locus4s core; Gale on each platform | Model fitting, group-model definitions, or a second generic region abstraction. |
| `motion` | Rigid poses/traces, FD/DVARS, motion QC, one-pass rigid application over image data. | standalone `neuroim4s-core`, standalone image4s-geometry; standalone bids4s on JVM | Heavy registration engines, NIfTI IO, reports, or GLM nuisance modeling. |
| standalone `neurosurf4s-core` / `io` | Surface anatomy, ordered topology, fields, placement and surface formats over their generic providers. | mesh4s, locus4s, spatial4s, image4s-geometry, graph4s, zarr4s and neuroim4s-spaces only | Volume, registration, UI, atlas, MVPA or ScalaFIM runtime backedges; alternate mesh/domain/frame authorities. |
| standalone `neurosurf4s` optional integrations/display | Typed transport, volume projection, pure scenes/raster, cursor linking and platform backends, with each optional dependency explicit. | core; Intaglio for display; reframe4s for links; neuroim4s volume/IO only for projection; OpenJFX/Three.js only in platform backends | Volume dependencies in core/scenes/raster, JVM UI in shared/JS, atlas or functional response processing. |
| `surface-view-connectivity` | Typed conversion from connectivity edge spaces/vectors to renderer-neutral surface-network inputs and provenance. | standalone neurosurf4s-view, `connectivity` | Estimation/inference, backend objects, or alternate node identity. |
| `spatial` | Functional field identity, lazy demands, caches, staged value execution and backprojection composition around anatomical routing. | neurosurf4s-routing; neuroim4s-routing-io on JVM | Anatomical graph/compiler/IO copies, atlas route catalogs, generic numeric kernels, or another domain owner. |
| standalone `neuroatlas4s` | Atlas catalog/publication, exact volume and bilateral surface realizations, loaders and template route policy in optional modules. | core: locus4s; volume: neuroim4s; surface: neurosurf4s core; mixed routing: both providers | Rendering, functional response processing, generic execution kernels or duplicate membership/domain owners. |
| `archive` | Format-neutral revision/publication envelopes; separately versioned archive, object, and representation identities; exact canonical manifest values and encoding; namespaced payload roles and logical identities; transactional canonical write orchestration; typed payload plans/executors; resource-safe open archives; and archive-native physical receipts. | No internal module; Cats Core and Cats Effect externally. | Physical LNA or Zarr sinks/schemas, dataset selection APIs, scientific reconstruction, untyped manifest values, secretly owned handles, or model-level decoding policy. |
| `archive-lna` | Typed LNA 2 paths, manifests, descriptors, validation, quant/delta payload codecs, shared-basis artifacts and registries, pure manifest normalization, JVM HDF5 stores, and the eager whole-payload driver. | `archive`, standalone `neuroim4s-core`; jHDF on JVM. | Scientific reconstruction, latent encoders, dataset discovery, response interpretation, runtime family assembly, or ownership of the canonical manifest writer. |
| `response-laws` | Typed, framework-neutral JVM/Scala.js checks for response ordering, shape, selected/whole and partition decode consistency, raw-bit persistence, axis-keyed receipt conformance, and provenance derivation. | `response` | Runtime execution, representation mathematics, archive bindings, fixtures, effect interpretation, or ownership of production response types. |
| `latent` | Archive-independent response-representation contracts, typed inspectable decode plans, temporal bases, transport and BOLDZip response semantics, radial/HRBF mathematics, and exact locus-backed active/full-grid selection order. | `response`, standalone `neuroim4s-core`, standalone locus4s core; Gale on each platform. | Archive values or paths, physical execution, checksums, dataset storage backends, model execution, a general tensor API, or another spatial selection algebra. |
| `interop-archived-response` | Typed logical-read lowering between representations and LNA/Zarr archives, LNA pipeline reconstruction and representation-specific latent codecs, archive/latent dataset adapters, first-class narrow persisted envelopes, canonical dense-BOLD response binding, immutable registries, and resource-safe runtime assembly. | `response`, `latent`, `archive`, `archive-lna`, `archive-zarr`, `dataset`; standalone bids4s and zarr4s | New reconstruction mathematics, core dataset query semantics, manifest attribute scraping, global mutable/plugin-loaded registries, central codec facades, or a universal payload/tensor carrier. |
| `dataset` | Pure fMRI descriptions and run queries, semantic acquisition locus domains, explicit synchronous-reader capabilities, checked `OpenedDataset[F]` attachment, ordered run-local selections, segmented reads, and response evidence propagation. | `response`, standalone `neuroim4s-core`, `hrf`, standalone locus4s core; standalone Gale | Archive or concrete representation imports, format dispatch, hidden readers, effect-parameterized model values, storage-format inheritance, parallel study/selection/error algebras, design formulas, fit kernels, or general BIDS project ownership. |
| `fit` | Inspectable fMRI model and fit plans plus numerical fit engines, with explicit synchronous `DatasetSeriesReader` and effectful `OpenedDataset[F]` execution boundaries. Packages remain `scalafim.fmri.model` and `scalafim.fmri.fit`. | `design`, `dataset`, `ar`; standalone Gale | Dataset storage, hidden blocking readers, or group inference. |
| `design-view` | Renderer-neutral design review graphics and raster exports. | `design`; standalone Intaglio core and platform renderers | Design construction, model/fit policy, or application UI ownership. |
| `mvpa4s` (standalone, pinned) | Identified MVPA evidence, axes and reindexing, relational/pattern analyses, measurement and execution contracts; `mvpa4s-testkit` supplies test-scope fixture identities and R parity reference data. | standalone Gale, locus4s core/provenance, Multivar, Multivar inference, Resample4s | Response, pipeline, imaging, surface, atlas, dataset or fMRI-fit types; ScalaFIM dependencies of any kind. |
| `mvpa-fmri` | Typed dataset/response adapters and shared run-local composition of fit-owned trial readouts with MVPA pattern operators, checked common feature axes, trial/run metadata, fold restriction, and local task/result collection. | `fit`, `dataset`, `mvpa4s`, `mvpa4s-predictive`; standalone Multivar and Alder | QR/GLM kernels, classifier numerics, a second feature-set abstraction, workflow scheduling, or platform IO. |
| `connectivity` | Shared connectivity algebra and portable kernels: ordered node axes with scientific provenance, graph4s-projected topology, locus node/edge spaces and masks, parcel time series, explicit vectorization orders, static/dynamic containers, estimator plans, ETS/event-weighted correlation, partial correlation, connectivity-set inference, dynamic stacks, diagnostics, and workflow receipts. | standalone graph4s, standalone locus4s core; Gale on each platform | Dataset backends, atlas/BIDS adapters, plotting, JVM IO, multivar execution bridges, TVGL/SRLC/phase/HMM internals, native optimizer backends, or scheduler/runtime execution. |
| `mvpa-spatial` | Functional threshold policy and fMRI spatial scenario checks; generic locus/mesh/atlas adapters live in optional mvpa4s artifacts. | `mvpa4s-core`, `threshold`; locus, surface and predictive providers in Test scope | Classifier algorithms, atlas loading, or a second searchlight/window model. |
| `group` | Second-level/group GLM, meta-analysis, group contrasts, FDR over subjects-by-samples maps. | standalone `neuroim4s-core`, `design`, `estimates`; standalone Gale | First-level model fitting or thresholding internals. |
| `mvpa-group` | Source-bound subject-coordinate and full joint uncertainty bridges into the existing group model; eager inputs retain covariance and df provenance. | `mvpa4s`, `group` (including adopted estimates contracts) | A second group engine, invented uncertainty, generic covariance storage disguised as anatomical geometry, or unqualified joint inference. |
| `fmri-workflow` | Serializable study specifications, header-derived catalogs, deterministic first-level/group jobs, structural preflight, result references, estimate production, and the eager fit-to-group adapter; generic pipeline lowering is a future orchestration slice. | `fit`, `group`; standalone bids4s | Numeric kernels, concrete file readers/writers, scheduler APIs, open resources, matrices, or captured execution closures. |
| `archive-zarr` | NeuroArchive Zarr 0.1 canonical-BOLD refinement and normalized `neuroarchive-zarr@1` metadata with a typed canonical-response payload role, measured layout profiles, scientific manifests, immutable publication, full-object validation, and cross-platform typed async execution with exact ordered object/range/byte observations. | standalone zarr4s, `archive`; Cats Core and Cats Effect externally | Response interpretation, dataset selection APIs, NIfTI/BIDS IO, catalogs, generic Zarr mechanics, hidden codec runtimes, or nondeterministic receipt aggregation. |
| `dataset-zarr` | JVM NeuroArchive-to-`FmriDataset` composition, regular-timing refinement, ordered selection lowering, Zarr-backed response blocks, streaming raw-scalar NIfTI import, and raw-scalar- and affine-preserving BIDS/NIfTI export within the documented NeuroArchive 0.1 subset. | `dataset`, `archive-zarr`, standalone `neuroim4s-core`; standalone bids4s and zarr4s | Generic array mechanics, fit kernels, catalog policy, synchronous browser facades, or browser file IO. |

The single-layer typed operator core, language-neutral IR, mathematical
contracts, and validation matrix are authoritative in the standalone
[`canardlapin/multivar`](https://github.com/canardlapin/multivar) repository.
ScalaFIM consumes a pinned source revision and owns only application adapters
and neuroimaging-facing integrations.

## Main Vertical Flows

### First-level fMRI GLM

```text
response -> dataset -> fit
                        ^
                        |
                     design -> hrf

bids4s -> motion / fmri-workflow / dataset-zarr
      \-> interop-archived-response JVM

archive-lna + archive-zarr + latent + response + dataset
  -> interop-archived-response

response -> response-laws
  -> reusable checks consumed from module test configurations

fit -> first-level-laws (owns HRF law helpers)
  -> generated cross-stack scientific laws; test-only and non-published

archive-zarr + dataset + image + bids4s
  -> dataset-zarr
```

`response` owns the storage-neutral selected-read contract. Standalone bids4s
describes BIDS files and confounds. `dataset` provides pure scientific descriptions, explicit
synchronous readers, and checked effectful attachment over that contract.
`design`/`hrf` describe regressors. `fit` owns the inspectable plan (including
the retained `scalafim.fmri.model` package) and executes it with Gale and
optionally `ar`.
`interop-archived-response` owns runtime driver/family assembly and scopes a
validated `OpenedDataset[F]`; it does not move effects into the scientific
description or model plan.
LNA composition flows through `interop-archived-response`, which is the
lowest artifact allowed to see the LNA schema, latent representations, and
dataset contracts together. NeuroArchive Zarr dense-dataset composition lives
in `dataset-zarr`; representation-aware Zarr runtime binding lives in
`interop-archived-response`. Downstream consumers read through the neutral
dataset/response contracts and do not dispatch on either archive family.
`fmri-workflow` sits above this flow and `group`: it carries only persistent
catalog/result references and immutable recipes, then delegates IO and numeric
execution to typed interpreters at module boundaries.

### Finite Indexed Spaces

```text
standalone locus4s -> neuroim4s/neurosurf4s/neuroatlas4s/mvpa4s
                  -> ScalaFIM dataset/connectivity/threshold/latent
```

Standalone locus4s is the sole owner of generic finite spaces, points, regions,
ordered selections, exact maps, relations, indexed fields, sections, and their
laws, and callers use its names directly. Partial parcel assignments use
`PartialSurjection` and native `Aggregation`; compact neighborhoods use
`NeighborhoodSystem`, with separate checked centered evidence. There is no process-wide domain registry: each
restore mints an independent owner, owners with equal persistent keys combine
through a checked `DomainAlignment`, and a composition that needs one shared
live owner threads its own `DomainRegistry` value. Derived supports are
ephemeral, and size or display name is never identity. Domain modules add
geometry, metadata, storage, provenance, or algorithm policy through checked
adapters; they do not reproduce the generic algebra. Zarr's package-local
`Geometry.Region` remains an array chunk/slice rectangle, not a spatial ROI,
and is outside this boundary.

### Spatial Data And Transforms

```text
spatial4s -> neuroim4s-spaces -> neurosurf4s-core
image4s + reframe4s -> neuroim4s-core/transform/routing
neurosurf4s-core + neuroim4s-routing -> neurosurf4s-routing
neurosurf4s-routing -> scalafim-spatial (functional fields)
neuroatlas4s optional volume/IO -> ScalaFIM atlas/workflow examples
```

`neuroim4s-spaces` owns shared world declarations. Surface geometry belongs to
neurosurf4s over mesh4s's topology and point fields. The optional neurosurf4s
routing module owns mixed anatomical domains, graph routing, sparse operators,
adjoints and QC; its JVM IO module owns graph ingestion and triplet files.
ScalaFIM spatial retains functional field identity, demand, caches and staged
value execution. Neuroatlas4s policy composes public anatomical contracts;
ScalaFIM has no atlas implementation module.

### Surface Display

```text
neurosurf4s-core -> neurosurf4s-view -> raster / JavaFX / Three.js
                          ^
ScalaFIM connectivity -> surface-view-connectivity
```

Arrows here mean the flow of scientific values. Neurosurf4s view owns the pure
scene plan; backends interpret it. Linked-volume composition is an optional
provider module above both anatomical libraries, not a core/display dependency.
The raster backend remains the deterministic oracle; native and browser lifecycle
qualification are separate from compilation and headless unit checks.

### Inference And Analysis

```text
fit -> group -> threshold
image --------^       ^
dataset -------------+

dataset -> mvpa-fmri -> mvpa4s
neurosurf4s/neuroatlas4s core -> optional mvpa4s surface/atlas adapters
mvpa-spatial -> mvpa4s core + threshold
fit + mvpa4s -> mvpa-fmri
mvpa4s + group -> mvpa-group
standalone multivar-inference (external)
```

`threshold` consumes statistic maps and masks; it is deliberately not a group
modeling module. `mvpa4s` core is spatially agnostic; `mvpa-fmri` converts selected
dataset rows into identified evidence. Optional mvpa4s locus/surface/atlas modules
compose measurements and scatter; `mvpa-spatial` retains fMRI threshold policy.

## Neurotransform And Neurofunctor Boundary

`~/code/neurotransform` and `~/code/neurofunctor` are related, but ScalaFIM
keeps their durable ideas at different layers. The authority is the ledger in
[`plans/image-module-unification-hardening.md`](plans/image-module-unification-hardening.md);
the transform design is [`plans/spatial-transform-parity.md`](plans/spatial-transform-parity.md)
and [`decisions/spatial-transforms.md`](decisions/spatial-transforms.md).

**Providers:**

- **spatial4s** owns nominal dimensions, frames, points, vectors, units and affine maps.
- **image4s-geometry** owns grids and sampling geometry over those same coordinate types.
- **reframe4s** (`lie`, `field`, `resample`, consumed by standalone `neuroim4s-core` and
  `neuroim4s-transform`) owns
  generic transform algebra:
  - `SpatialMap` with frame-checked composition
  - `SmoothIso` and `FramedAffine`
  - `DenseMap` and dense fields
  - `ResamplingPlan` and the interpolation kernels
  - the fixed-point dense-field inverse solver behind `invertNumerically`
  - nonlinear registration and the experimental HalfFlow engine (not
    ScalaFIM dependencies)

**Neuroimaging policy and ScalaFIM modules:**

- **standalone `neuroim4s-core`** owns neuroimaging adapters over those providers: `GridSpec`,
  `SpatialPullbacks`, neuroimaging sampling policy,
  `DenseVectorField` roles, and anatomical orientation. It defines no
  morphism or affine algebra of its own; its `coreBoundaryCheck` rejects
  the retired `SpatialMorphism`/`Affine3DMorphism` family.
  - World-space identity (`WorldSpace`, `FrameCatalog`, `Spaces`,
    `SpaceResolver`) comes from neuroim4s-spaces and the linked-cursor
    `WorldLink` from reframe4s; `neuroim4s.image.world` keeps toolkit
    coordinate conventions (`ToolCoordinates`) and world-frame binding
    (`Placed`). Bare D3 geometry receives a fresh unknown-world
    scope; file admission requires explicit resolved evidence.
- **standalone `neuroim4s-transform`** owns toolkit transform formats (ITK/ANTs, FSL, AFNI,
  FreeSurfer, X5), their interpretation as typed world-space transforms, and
  conversion between toolkits. Gated numerical inverses are qualified against
  native ANTs `InverseWarp` and FSL `invwarp` outputs
  (`NativeInverseOracleSuite`, on the pinned reframe4s revision).
- **standalone `neurosurf4s`** owns surface geometry, volume projection and
  surface-to-surface vertex-map execution in explicit optional modules.
- **standalone `neuroatlas4s-routing`** owns named known-space route descriptors such as
  `SpaceTransforms`, a manifest that populates the `spatial` graph over
  unsampled template domains (STP P7.01); routing is `neurosurf4s.routing.SpatialGraph`'s.
- **standalone `neurosurf4s-routing`** owns typed mixed anatomical domains,
  graph morphisms, routing, compiled sparse operators, adjoints and QC. Its
  optional JVM IO module owns transform-asset graph ingestion.
- **`spatial`** owns functional fields, demands, caches and staged value execution.
  `FunctionalRouting` adds functional/filter/hybrid plugin compilers to the
  anatomical registry. Anatomical compilers reject value execution; the
  restricted coordinate-component seam preserves the full program identity.

There are intentionally two layers:

- **A coordinate map** is a reframe4s `SpatialMap`, which takes target
  coordinates and returns source coordinates. `neurosurf4s-routing` binds these through
  `CoordinateMap.Geometric(ProviderMapBinding)`.
- **`neurosurf4s.routing.Morphism`** is a graph edge: ids, source and target
  domains, cost, inverse quality, route tag, provenance, and an optional
  coordinate map.

**Points:**

- Raw unlabelled triples use provider `spatial4s.Xyz` (neuroimaging point
  conveniences do not establish frame alignment). Surface and atlas code
  do not mint a second coordinate owner.
- Where voxel or world roles matter, prefer `VoxelPoint` and `WorldPoint`.
- Frame-indexed spatial4s `Point[F, D3]` values carry world coordinates through
  transforms and linked viewer cursors; cross-world sharing requires a
  `WorldLink`.

**Rules for agents:**

- Do not duplicate coordinate execution in `spatial` or `atlas`. Compose
  provider maps, and compile sampled operators where the target is a sampled
  domain.
- Dense nonlinear routes execute through reframe4s `DenseMap` pullbacks that
  are built from transform assets.

## Placement Rules

- Put generic topology, traversal, graph laws, indexed numerical operators,
  spectra, embeddings, and graph similarities in standalone graph4s. Its
  optional `graph4s-gale` module owns the numerical boundary. Keep scientific
  connectivity semantics in `connectivity`.
- Put generic finite semantic domains, points, regions, selections, exact maps,
  relations, indexed fields, sections, and reusable laws in standalone
  locus4s, including aggregation (`locus4s.data.Aggregation`), partial quotients
  and neighborhood representation. Functional admission adapters use its public
  constructors directly. Geometry, storage, metadata and algorithm policy remain
  in their domain modules.
- Put primitive matrix/vector/operator math, solver contracts, portable
  eigensolver/SVD/QR/Cholesky/SPD-inverse implementations, and backend adapter
  boundaries in standalone Gale. ScalaFIM domain modules may own typed
  scientific policy and explicit conversions, but must not recreate generic
  decomposition families or expose JVM-only libraries such as Breeze in shared
  APIs.
- Put generic workflow graph algebra in `pipeline`; keep domain execution in the owning computational modules and adapt it upward.
- Put general plotting, scene-description contracts, and concrete renderers in
  standalone Intaglio. ScalaFIM modules may adapt scientific values into that
  public API but must not recreate renderer or grammar internals.
- Put generic image kernels in image4s (or its domain/numeric providers). Put
  neuroimaging admission/policy in `neuroim4s-core`, volume IO in `neuroim4s-io`,
  and toolkit container IO in `neuroim4s-transform-io`.
- Put nonlinear registration state, objectives, deformation geometry, and
  acceptance policy in standalone reframe4s. ScalaFIM retains no registration
  module and depends only on the reframe4s `lie`, `field` and `resample`
  modules, not on its registration or HalfFlow modules.
- Put generic mesh and vertex-domain algorithms in mesh4s and anatomical
  surface policy in neurosurf4s.
- Put surface display state, layers, anatomical cameras, render-plan compilation,
  projection/network visualization primitives, scene documents, and backend
  admission contracts in neurosurf4s-view. Put deterministic pixels in neurosurf4s-view-raster,
  JavaFX and Three.js interpretation in the corresponding provider backends,
  and ScalaFIM connectivity result adaptation in `surface-view-connectivity`.
- Put named atlas descriptors and parcel metadata in neuroatlas4s-core,
  runtime realizations in its optional volume/surface modules, and mixed
  template asset policy in its optional routing/IO modules.
- Put anatomical route compilation, sparse projectors, adjoints and QC in
  neurosurf4s-routing. Functional field caches and staged execution belong in
  ScalaFIM spatial.
- Put dataset selection/storage contracts and narrow dataset-backend adapters in
  `dataset`; put reusable BIDS table/query logic in standalone bids4s, and
  translate BIDS entities at dataset IO boundaries with exact parsed matches
  rather than filename substring filters.
- Put inspectable model descriptions and numeric execution in `fit`; put
  group-level inference in `group`.
- Put cross-module BIDS-to-group study recipes, catalog/job identity, structural
  preflight, result references, and future pipeline lowering in `fmri-workflow`; keep
  payloads, open readers, format implementations, and scheduler types out of it.
- Put nominal primal/dual algebra, semantic duality diagrams, role-specific
  forms and certificates, row measures/centering, singular policies, explicit
  row relationships, direct-sum objectives, sparse-aware preprocessing,
  decomposition artifacts, and language-neutral IR in standalone `multivar`.
  Keep dataset, MVPA, bindings, and scheduler-specific execution in their
  ScalaFIM modules, and image policy in standalone neuroim4s.
- Put perturbation targets, resampling/null actions, Monte Carlo programs,
  latent-unit stability, validity, and evidence provenance in the external
  `multivar-inference` artifact. ScalaFIM may add only domain-specific adapters;
  fitting, workflows, IO, and schedulers remain outside that library.
- Put shared connectivity structures in `connectivity`: validated series axes,
  explicit edge spaces/vectorization orders, static/dynamic containers,
  estimator/preprocessing plan ADTs, portable ETS/event, partial-correlation,
  group-inference, and low-risk dynamic kernels, diagnostics, and workflow
  receipts. Keep Ariadne/R compatibility orders explicit and keep dataset,
  atlas, BIDS, plotting, IO, multivar execution bridges, native optimizer
  backends, phase/HMM/TVGL/SRLC internals, and scheduler-specific execution
  outside this structural core.
- Put dataset/sample adapters for MVPA in `mvpa-fmri`; put spatial feature
  adapters for MVPA in optional `mvpa4s-locus`, `mvpa4s-surface` and `mvpa4s-atlas`;
  keep fMRI threshold policy in `mvpa-spatial` and `mvpa4s` core over plain sample-by-feature
  matrices and linear operators. Put fit-owned time-to-trial readout composition,
  run stacking, and trial/run metadata in `mvpa-fmri` so neither lower module
  depends back on the other.

If a change wants a new dependency edge, stop and check whether the code belongs
in a higher adapter module instead. Lower modules should stay reusable and
workflow-neutral.

## Bounded MVPA pattern persistence

`mvpa-artifacts` owns a finite completed-pattern profile and a JVM adapter to
`archive.io.LocalObjectStore`. It depends on MVPA artifact types and existing
estimate IO, never on `fit` or `mvpa-fmri`. Its reader restores declared axes,
policies, lineage and coverage without the fitter. Matrices are individually
hash-pinned external Float64 leaves. Other profiles and resumable optimizer
checkpoints are explicitly unsupported; there is no codec registry or upload.

The unpublished neurosurf4s `fixturesJVM` project owns the fsLR asset/oracle helpers, resources and qualification suites. neuroatlas4s `routingIoJVM` Test depends on its Compile and Test configurations. This upward dependency is confined to test support: neither provider core nor any production module depends on the fixture project. JVM IO helpers have no JS implementation.

Toolkit transform formats and anatomical interpretation now belong to standalone
`neuroim4s-transform`; optional JVM files/HDF5/X5 belong to
`neuroim4s-transform-io`. Surface, spatial and atlas use those provider APIs
directly. There is no local `transform` module. The unpublished `providerContract`
test configuration retains the four ScenarioHarness registration workflows with
strict compiler checks; its runtime compile contract remains independent of them.

## Anatomical volume routing

`neuroim4s-routing` depends on neuroim4s core/transform and graph4s algorithms.
It admits provider-owned D3 grids and coordinate maps, routes anatomical volume
registrations and assembles one sampling pass. It has no surface, atlas,
response or dataset dependency. Its optional `neuroim4s-routing-io` JVM module
adds scoped NIfTI reads over neuroim4s IO; core and basic IO do not depend on
routing. graph4s owns directed shortest/simple-path algorithms, including
parallel arc payload choices.

The optional `neurosurf4s-routing` module composes volume routing with surface
and hybrid anatomical execution. ScalaFIM spatial retains functional field
identity, demand, caches and value-stage execution. Volume cases contain one
admitted `VolumeGrid`; they do not repeat grid or mask admission.

Surface resampling delegates generic nearest-vertex lookup, positive radial
face lookup and origin-centered sphere projection to `mesh4s-geometry`.
Neurosurf4s owns registered-coordinate interpretation, sparse plan construction,
normalization and the nearest fallback after geometric admission. Graph-cost
geodesics use the owner-preserving `mesh4s-graph4s` adapter over graph4s distances;
neurosurf4s owns anatomical validation and cache policy. No surface or anatomical
routing implementation remains in ScalaFIM.

## Phase D boundary decisions (2026-10-27)

`group` and `mvpa-group` consume `neuroim4s.spaces.SubjectId`; dataset's public
subject constructors delegate to that same type while retaining dataset errors.
The grammar is identical (String.trim followed by nonempty admission), including
Unicode and composite namespace/subject labels. The group runtime no longer
depends on `dataset`.

`pipeline` remains optional: its real fit-workflow contracts exercise typed graph
execution through a Test-only fit dependency. `mvpa-spatial` remains the reduced
MVPA/threshold integration module, with imaging adapters in Test scope. These
boundaries keep graph execution out of fitting and MVPA out of generic
thresholding. `locus-data` and `DomainFactory` were already retired in the U6
provider cutover; current callers use native locus4s registry contracts.
