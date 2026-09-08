# ScalaFIM Module Relations

This is a quick orientation map for coding agents. Use it before adding a new
type, dependency edge, or convenience helper.

ScalaFIM is not organized by the original R package names. The R packages are
semantic references; ScalaFIM is organized by typed computational layers. Add
code to the lowest module that can own it without depending on a higher-level
workflow.

## Dependency Shape

Live dependency edges are declared in `build.sbt`.

```text
response
+-- response-laws   reusable cross-platform law checks; test-support only
+-- dataset         response adapters; response has no internal dependencies

standalone locus4s
+-- locus-data
|   +-- image
|   +-- surface       also depends on image, standalone graph4s
|   +-- atlas         also depends on image, surface, standalone graph4s
|   +-- spatial       also depends on standalone Gale, image, surface
|   +-- dataset       also depends on response, image, hrf
|   +-- mvpa          also depends on fit, dataset, image, surface, atlas
+-- latent            also depends on response, image
+-- threshold         also depends on image
+-- connectivity      also depends on standalone graph4s

standalone graph4s
+-- pipeline
+-- connectivity      also depends on locus-data
+-- surface           also depends on image, locus-data
+-- atlas             also depends on image, surface, locus-data

standalone Gale
+-- ar
+-- design           also depends on hrf
+-- mvpa             also depends on fit, dataset, image, surface, atlas, locus-data
+-- threshold         also depends on image
+-- motion            also depends on image
+-- spatial           also depends on image, surface
+-- model             also depends on design, dataset
+-- fit               also depends on model, ar
+-- group             also depends on image, dataset, design, fit

pipeline

standalone Intaglio core
+-- design                 also depends on hrf, standalone Gale
+-- image-view             also depends on image
|   +-- image-view-canvas  also depends on Intaglio Canvas
|   +-- image-view-java2d  also depends on Intaglio Java2D
|   +-- image-view-javafx  also depends on Intaglio JavaFX
+-- surface-view           also depends on surface
    +-- surface-view-raster
    +-- surface-view-javafx  also depends on external OpenJFX
    +-- surface-view-three
    +-- surface-view-connectivity  also depends on connectivity

hrf
+-- design           also depends on standalone Gale, Intaglio core
    +-- model
        +-- fit
            +-- group

image
+-- image-view        also depends on Intaglio core
+-- archive-lna      also depends on archive
+-- latent           also depends on response, locus-data, and standalone Gale
+-- dataset          also depends on response, hrf, locus-data
+-- surface
|   +-- spatial        also depends on standalone Gale, image
|   +-- atlas          also depends on image
|   +-- surface-view   also depends on Intaglio core
+-- threshold         also depends on standalone Gale
+-- motion            also depends on standalone Gale
+-- group             also depends on standalone Gale, dataset, design, fit

standalone bids4s
+-- motion JVM
+-- interop-archived-response JVM
+-- fmri-workflow
+-- dataset-zarr

bids4s + dataset + model + fit + group
+-- fmri-workflow     outer composition only; no lower module depends back on it

standalone zarr4s
+-- archive-zarr      also depends on archive
    +-- dataset-zarr  also depends on dataset, image, standalone bids4s

archive
+-- archive-lna       also depends on image
+-- archive-zarr      also depends on standalone zarr4s
+-- interop-archived-response
    +-- also depends on response, latent, archive-lna, archive-zarr, and dataset

fit + dataset + image + surface + atlas + locus-data
+-- mvpa              one neuroimaging analysis boundary

standalone Gale + multivar + resample4s + Alder
+-- mvpa
```

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
| `locus-data` | ScalaFIM domain construction, supported parcellations, and one-pass commutative aggregation. | standalone locus4s core and data | Generic finite-domain algebra, neighborhood systems, or laws; image/surface geometry, atlas ontology, lazy execution, IO, or probabilistic membership. |
| `pipeline` | Generic typed pipeline graphs, artifact references, graph4s-delegated deterministic DAG staging, local pure execution, and structured receipts. | standalone graph4s | Neuroimaging algorithms, file IO, external CLI execution, scheduler/runtime implementations, or lower-module convenience helpers. |
| `response` | Axis-safe identities and ordered selections, neutral time/sample schemas, owned row-major `Double` response blocks, effectful read planning, provenance, axis-keyed locality capabilities, and read receipts. | Nothing internal; Cats Core and Cats Effect externally. | General tensors, mutable public buffers, image/surface geometry, dataset hierarchy, archive formats, representation codecs, storage interpreters, or fit policy. |
| `hrf` | HRFs, basis functions, sampling frames, convolution primitives. | Nothing internal. | Design formulas, datasets, or model fitting. |
| `ar` | AR/ARMA whitening plans and pure prewhitening kernels. | standalone Gale | GLM fitting orchestration or dataset IO. |
| `design` | Event models, formulas, baselines, contrasts, design metadata, and renderer-neutral design plot exports. | `hrf`, standalone Gale, standalone Intaglio core | Dataset execution, numerical fit engines, or concrete renderers such as SVG/Java2D/Canvas. |
| `image` | Volumes, masks, exact volume locus domains, locus-backed regions/selections, affine math, low-level coordinate transforms, morphisms, resampling, clustering, and metric searchlight construction. | `locus-data` | Atlas registries, dataset backends, graph-level operator caches, JVM-only image readers in shared code. |
| `image-view` | Renderer-neutral world-space slice views: typed colorizers/layers, orthogonal scene compilation, crosshairs, orientation labels, and panel receipts. | `image`, standalone Intaglio core | NIfTI IO, mutable toolkit widgets, DOM/JavaFX lifecycle ownership, or concrete renderer command interpretation. |
| `image-view-canvas` | Browser Canvas rendering host plus canvas-relative pointer/wheel translation into pure viewer actions. | `image-view`, Intaglio Canvas | Image geometry, DOM ownership, application state mutation, or alternate renderer logic. |
| `image-view-java2d` | Java2D rendering host plus device-relative event translation and `BufferedImage` convenience rendering. | `image-view`, Intaglio Java2D | Image geometry, Swing lifecycle ownership, or alternate renderer logic. |
| `image-view-javafx` | JavaFX Canvas rendering host plus device-relative event translation through the toolkit-free graphics context boundary. | `image-view`, Intaglio JavaFX | Image geometry, JavaFX application/thread lifecycle ownership, or alternate renderer logic. |
| `threshold` | Spatial inference over statistic maps: locus-backed active/full support, scored candidates, octrees, set scoring, and maxT-style correction. | `image`, `locus-data`; Gale on each platform | Model fitting, group-model definitions, or a second generic region abstraction. |
| `motion` | Rigid poses/traces, FD/DVARS, motion QC, one-pass rigid application over image data. | `image`, standalone Gale; standalone bids4s on JVM | Heavy registration engines, NIfTI IO, reports, or GLM nuisance modeling. |
| `surface` | Meshes, exact topology/order locus domains, vertex fields, region-backed surface ROIs, quotient-backed labels, geodesic searchlights, graph4s interop, cross-platform GIFTI ingestion, and JVM FreeSurfer readers. | standalone graph4s, `image`, `locus-data` | Atlas metadata, MVPA plans, or whole spatial graph compilation. |
| `surface-view` | Renderer-neutral surface assets/layers, immutable display state and reducer, anatomical cameras/layouts, render-plan compilation, resource identity, temporal/projection/network primitives, scene documents, backend capabilities, and admission contracts. | `surface`, standalone Intaglio core | JavaFX/Three.js objects, DOM/window lifecycle, connectivity estimation, or platform IO. |
| `surface-view-raster` | Deterministic JVM/Scala.js CPU raster, depth/culling/clipping, compositing, exact picks, and semantic reference receipts. | `surface-view`, Intaglio core | Interactive toolkit lifecycle, platform-specific acceleration, or scientific-data policy. |
| `surface-view-javafx` | JVM JavaFX Scene3D plan interpretation, retained mesh/color-atlas resources, reducer-backed controller, picks, snapshots, and native receipts. | `surface-view`, Intaglio core; external OpenJFX | Shared scientific semantics, application/stage ownership, Scala.js code, or silent fallback for unsupported plans. |
| `surface-view-three` | Scala.js Three.js/WebGL plan interpretation, retained GPU resources, native picks/snapshots, and feature-gated GPU volume projection. | `surface-view`, Intaglio core; host-injected Three.js | DOM/bundler ownership, shared scientific semantics, or an assumption that WebGL2 float targets exist. |
| `surface-view-connectivity` | Typed conversion from connectivity edge spaces/vectors to renderer-neutral surface-network inputs and provenance. | `surface-view`, `connectivity` | Estimation/inference, backend objects, or alternate node identity. |
| `spatial` | Neurofunctor-style domains with locus packages, domain-specific morphism routing, exact/crisp/sampled transport distinctions, selections, route policies, sampled operators, adjoints, provenance, QC, caches, and lazy fields. | standalone Gale, `image`, `surface`, `locus-data` | Low-level image interpolation kernels, atlas-specific route catalogs, or another finite-space/region implementation. |
| `atlas` | Standard atlas descriptors, parcel metadata, typed locus parcellations, parcel/network quotient operations, explicit display order, one-pass reduction, explicit-alignment overlap, graph4s region interop, and transform route descriptors. | standalone graph4s, `image`, `surface`, `locus-data` | Generic spatial operator compilation, low-level transform kernels, or extensional region identity in labels/metadata. |
| `archive` | Format-neutral revision/publication envelopes; separately versioned archive, object, and representation identities; exact canonical manifest values and encoding; namespaced payload roles and logical identities; transactional canonical write orchestration; typed payload plans/executors; resource-safe open archives; and archive-native physical receipts. | No internal module; Cats Core and Cats Effect externally. | Physical LNA or Zarr sinks/schemas, dataset selection APIs, scientific reconstruction, untyped manifest values, secretly owned handles, or model-level decoding policy. |
| `archive-lna` | Typed LNA 2 paths, manifests, descriptors, validation, quant/delta payload codecs, shared-basis artifacts and registries, pure manifest normalization, JVM HDF5 stores, and the eager whole-payload driver. | `archive`, `image`; jHDF on JVM. | Scientific reconstruction, latent encoders, dataset discovery, response interpretation, runtime family assembly, or ownership of the canonical manifest writer. |
| `response-laws` | Typed, framework-neutral JVM/Scala.js checks for response ordering, shape, selected/whole and partition decode consistency, raw-bit persistence, axis-keyed receipt conformance, and provenance derivation. | `response` | Runtime execution, representation mathematics, archive bindings, fixtures, effect interpretation, or ownership of production response types. |
| `latent` | Archive-independent response-representation contracts, typed inspectable decode plans, temporal bases, transport and BOLDZip response semantics, radial/HRBF mathematics, and exact locus-backed active/full-grid selection order. | `response`, `image`, `locus-data`; Gale on each platform. | Archive values or paths, physical execution, checksums, dataset storage backends, model execution, a general tensor API, or another spatial selection algebra. |
| `interop-archived-response` | Typed logical-read lowering between representations and LNA/Zarr archives, LNA pipeline reconstruction and representation-specific latent codecs, archive/latent dataset adapters, first-class narrow persisted envelopes, canonical dense-BOLD response binding, immutable registries, and resource-safe runtime assembly. | `response`, `latent`, `archive`, `archive-lna`, `archive-zarr`, `dataset`; standalone bids4s and zarr4s | New reconstruction mathematics, core dataset query semantics, manifest attribute scraping, global mutable/plugin-loaded registries, central codec facades, or a universal payload/tensor carrier. |
| `dataset` | Pure fMRI descriptions and run queries, semantic acquisition locus domains, explicit synchronous-reader capabilities, checked `OpenedDataset[F]` attachment, ordered run-local selections, segmented reads, and response evidence propagation. | `response`, `image`, `hrf`, `locus-data`; standalone Gale | Archive or concrete representation imports, format dispatch, hidden readers, effect-parameterized model values, storage-format inheritance, parallel study/selection/error algebras, design formulas, fit kernels, or general BIDS project ownership. |
| `model` | Inspectable fMRI model and fit plans: dataset plus design plus fitting configuration. | `design`, `dataset`; standalone Gale | OLS/GLS kernels or backend implementations. |
| `fit` | Numerical fit engines over pure model plans, with explicit synchronous `DatasetSeriesReader` and effectful `OpenedDataset[F]` execution boundaries. | `model`, `ar`; standalone Gale | Model description, dataset storage, hidden blocking readers, or group inference. |
| `mvpa` | One identified-evidence architecture for predictive modelling and relational geometry: exact axes and columns, observation and relation evidence, validation/cross-fit and ordered-pairing designs, measurement frames, typed estimands/results, direct fMRI readout and dataset evidence construction, spatial frame builders, and portable kernels. | `fit`, `dataset`, `image`, `surface`, `atlas`, `locus-data`; standalone Gale, multivar, resample4s, and Alder | Generic linear algebra, generic resampling or learner lifecycles, dataset storage backends, atlas loading, platform IO, duplicate axis/feature-set/result algebras, or hidden materialization. |
| `connectivity` | Shared connectivity algebra and portable kernels: ordered node axes with scientific provenance, graph4s-projected topology, locus node/edge spaces and masks, parcel time series, explicit vectorization orders, static/dynamic containers, estimator plans, ETS/event-weighted correlation, partial correlation, connectivity-set inference, dynamic stacks, diagnostics, and workflow receipts. | standalone graph4s, `locus-data`; Gale on each platform | Dataset backends, atlas/BIDS adapters, plotting, JVM IO, multivar execution bridges, TVGL/SRLC/phase/HMM internals, native optimizer backends, or scheduler/runtime execution. |
| `group` | Second-level/group GLM, meta-analysis, group contrasts, FDR over subjects-by-samples maps. | `image`, `dataset`, `design`, `fit`; standalone Gale | First-level model fitting or thresholding internals. |
| `fmri-workflow` | Serializable study specifications, header-derived catalogs, deterministic first-level/group jobs, structural preflight, and result references; generic pipeline lowering is a future orchestration slice. | `dataset`, `model`, `fit`, `group`; standalone bids4s | Numeric kernels, concrete file readers/writers, scheduler APIs, open resources, matrices, or captured execution closures. |
| `archive-zarr` | NeuroArchive Zarr 0.1 canonical-BOLD refinement and normalized `neuroarchive-zarr@1` metadata with a typed canonical-response payload role, measured layout profiles, scientific manifests, immutable publication, full-object validation, and cross-platform typed async execution with exact ordered object/range/byte observations. | standalone zarr4s, `archive`; Cats Core and Cats Effect externally | Response interpretation, dataset selection APIs, NIfTI/BIDS IO, catalogs, generic Zarr mechanics, hidden codec runtimes, or nondeterministic receipt aggregation. |
| `dataset-zarr` | JVM NeuroArchive-to-`FmriDataset` composition, regular-timing refinement, ordered selection lowering, Zarr-backed response blocks, streaming raw-scalar NIfTI import, and raw-scalar- and affine-preserving BIDS/NIfTI export within the documented NeuroArchive 0.1 subset. | `dataset`, `archive-zarr`, `image`; standalone bids4s and zarr4s | Generic array mechanics, fit kernels, catalog policy, synchronous browser facades, or browser file IO. |

The single-layer typed operator core, language-neutral IR, mathematical
contracts, and validation matrix are authoritative in the standalone
[`canardlapin/multivar`](https://github.com/canardlapin/multivar) repository.
ScalaFIM consumes a pinned source revision and owns only the neuroimaging
evidence, measurement, and execution bindings.

## Main Vertical Flows

### First-level fMRI GLM

```text
response -> dataset -> model -> fit
                        ^
                        |
                     design -> hrf

bids4s -> motion / fmri-workflow / dataset-zarr
      \-> interop-archived-response JVM

archive-lna + archive-zarr + latent + response + dataset
  -> interop-archived-response

response -> response-laws
  -> reusable checks consumed from module test configurations

hrf-laws + fit -> first-level-laws
  -> generated cross-stack scientific laws; test-only and non-published

archive-zarr + dataset + image + bids4s
  -> dataset-zarr
```

`response` owns the storage-neutral selected-read contract. Standalone bids4s
describes BIDS files and confounds. `dataset` provides pure scientific descriptions, explicit
synchronous readers, and checked effectful attachment over that contract.
`design`/`hrf` describe regressors. `model` joins those descriptions into an
inspectable plan. `fit` executes the plan with Gale and optionally `ar`.
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
standalone locus4s -> locus-data -> image/surface/atlas/spatial/dataset
                          |
                          +-----> connectivity/threshold/latent/mvpa
```

Standalone locus4s is the sole owner of generic finite spaces, indices, regions,
ordered selections, exact maps, relations, indexed fields, sections, and their
laws. `locus-data` owns ScalaFIM-specific domain construction, parcellations,
and aggregation. Domain modules add
geometry, metadata, storage, provenance, or algorithm policy through checked
adapters; they do not reproduce the generic algebra. Zarr's package-local
`Geometry.Region` remains an array chunk/slice rectangle, not a spatial ROI,
and is outside this boundary.

### Spatial Data And Transforms

```text
locus-data -> image -> surface
     |          |        |
     |          +------> atlas
     +-----------------> spatial <----- surface
```

`image` owns executable low-level volume geometry. `surface` owns mesh geometry.
`atlas` names standard spaces and attaches metadata to locus parcels.
`spatial` compiles reusable source-to-target operators with provenance and
adjoints.

### Surface Display

```text
image ----------------------> surface-view <---------------- connectivity
                               ^       |
                               |       +--> surface-view-raster  (JVM + JS)
surface -----------------------+       +--> surface-view-javafx  (JVM)
Intaglio core -----------------+       +--> surface-view-three   (Scala.js)
                                       +--> surface-view-connectivity
```

`surface-view` is the only owner of scientific display semantics and emits one
backend-neutral plan. The raster backend is the deterministic oracle. JavaFX
and Three.js retain native resources but may only interpret the plan; the
connectivity adapter converts estimator output without reversing that boundary.

### Inference And Analysis

```text
fit -> group -> threshold
image --------^       ^
dataset -------------+

fit/dataset/image/surface/atlas/locus-data -> mvpa
Gale/multivar/resample4s/Alder ------------> mvpa
standalone multivar-inference (external)
```

`threshold` consumes statistic maps and masks; it is deliberately not a group
modeling module. `mvpa` is the sole neuroimaging multivariate-analysis boundary:
dataset selections and fMRI readouts become identified evidence there, while
image, surface, atlas, and locus objects become typed measurement frames there.
The generic mathematical, resampling, and learner lifecycles remain owned by
their standalone libraries.

## Neurotransform And Neurofunctor Boundary

`~/code/neurotransform` and `~/code/neurofunctor` are related, but ScalaFIM
keeps their durable ideas at different layers.

- `image` owns executable geometric transform kernels: `SpatialMorphism`,
  `IdentityMorphism`, `Affine3DMorphism`, dense displacement/coordinate fields,
  interpolation plans, `GridSpec`, `SpatialPoint`, `ResamplingPlan`, field
  materialization, and local execution-plan compaction/fusion.
- Standalone reframe4s owns nonlinear registration, HalfFlow state, midpoint
  updates, paired flows, acceptance guards, and registration diagnostics.
  ScalaFIM has no dependency on it after extraction.
- `surface` owns surface-specific geometric data, volume-to-surface morphism
  wrappers, and surface-to-surface vertex-map execution.
- `atlas` owns named known-space route descriptors such as `SpaceTransforms`;
  available affine routes can lower to image pullback morphisms.
- `spatial` owns graph/operator semantics: typed domains, graph morphisms,
  routing, compiled sparse operators, adjoints, QC, provenance, caches, and
  lowering of executable identity/affine graph paths into image morphisms.

There are intentionally two morphism layers:

- `scalafim.image.SpatialMorphism` is an executable coordinate map: target
  coordinates in, source coordinates out.
- `scalafim.spatial.Morphism` is a graph edge: ids, source/target domains,
  cost, inverse quality, route tag, provenance, and optional coordinate map.

`scalafim.image.SpatialPoint` is the canonical finite 3D coordinate value across
the spatial stack. `scalafim.surface.Point3D` and `scalafim.atlas.Point3D` are
source-compatible aliases/adapters over that image type, not independent point
records.

Where voxel/world roles matter, prefer the narrower `VoxelPoint` and
`WorldPoint` image types over an unlabelled 3D vector.

Agents should not duplicate coordinate execution in `spatial` when an
`image.SpatialMorphism` or surface primitive can own it. Use the existing typed
lowering from `spatial.MorphismPath` to executable image morphisms for
identity/affine paths, and compiled sampled operators where the target is a
sampled domain. Dense nonlinear routes should wait until the JVM transform IO
descriptors can materialize executable dense morphisms.

## Placement Rules

- Put generic topology, traversal, graph laws, indexed numerical operators,
  spectra, embeddings, and graph similarities in standalone graph4s. Its
  optional `graph4s-gale` module owns the numerical boundary. Keep scientific
  connectivity semantics in `connectivity`.
- Put generic finite semantic domains, indices, regions, selections, exact maps,
  relations, fields, sections, neighborhood systems, and reusable laws in standalone
  locus4s. Put only ScalaFIM domain construction, parcellations, and
  commutative aggregation in `locus-data`. Geometry, storage, metadata, and
  algorithm policy remain in their domain modules.
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
- Put pure image-space kernels in `image`; platform IO goes in `image/jvm`.
- Put nonlinear registration state, objectives, deformation geometry, and
  acceptance policy in standalone reframe4s. ScalaFIM retains no registration
  module or adapter dependency.
- Put mesh and vertex-domain algorithms in `surface`.
- Put surface display state, layers, anatomical cameras, render-plan compilation,
  projection/network visualization primitives, scene documents, and backend
  admission contracts in `surface-view`. Put deterministic pixels in
  `surface-view-raster`, JavaFX Scene3D interpretation in `surface-view-javafx`,
  Three.js/WebGL interpretation in `surface-view-three`, and connectivity result
  adaptation in `surface-view-connectivity`.
- Put named atlas descriptors and parcel metadata in `atlas`.
- Put route compilation, sparse projectors, adjoints, QC, provenance, and
  operator caches in `spatial`.
- Put dataset selection/storage contracts and narrow dataset-backend adapters in
  `dataset`; put reusable BIDS table/query logic in standalone bids4s, and
  translate BIDS entities at dataset IO boundaries with exact parsed matches
  rather than filename substring filters.
- Put inspectable model descriptions in `model`; put numeric execution in
  `fit`; put group-level inference in `group`.
- Put cross-module BIDS-to-group study recipes, catalog/job identity, structural
  preflight, result references, and future pipeline lowering in `fmri-workflow`; keep
  payloads, open readers, format implementations, and scheduler types out of it.
- Put nominal primal/dual algebra, semantic duality diagrams, role-specific
  forms and certificates, row measures/centering, singular policies, explicit
  row relationships, direct-sum objectives, sparse-aware preprocessing,
  decomposition artifacts, and language-neutral IR in standalone `multivar`.
  Keep dataset, image, MVPA, bindings, and scheduler-specific execution in
  their ScalaFIM modules.
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
- Put MVPA dataset evidence construction, fMRI time-to-trial readout
  composition, run stacking, spatial measurement-frame builders, scientific
  designs, estimands, and typed results in `mvpa`. Keep storage, image/surface
  geometry, atlas metadata, generic linear algebra, resampling, and learner
  lifecycles in their existing owners; do not introduce another MVPA adapter
  module or parallel identity algebra.

If a change wants a new dependency edge, stop and check whether the code belongs
in a higher adapter module instead. Lower modules should stay reusable and
workflow-neutral.
