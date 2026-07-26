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
locus-kernel
+-- locus-data
|   +-- image
|   +-- surface       also depends on image, graph
|   +-- atlas         also depends on image, surface, graph
|   +-- spatial       also depends on linalg, image, surface
|   +-- dataset       also depends on image, hrf, archive, latent, bids
|   +-- mvpa-spatial  also depends on mvpa, image, surface, atlas
|   +-- locus-laws    test-support and reference models
+-- graph
+-- latent            also depends on archive
+-- threshold         also depends on image
+-- multivar          also depends on linalg
+-- connectivity      also depends on graph

graph
+-- connectivity      also depends on locus-kernel
+-- graph-linalg      also depends on linalg
+-- surface           also depends on image, locus-data
+-- atlas             also depends on image, surface, locus-data
+-- spatial           also depends on linalg, image, surface, locus-data
+-- pipeline

linalg
+-- linalg-breeze    JVM-only optional Breeze adapter
+-- ar
+-- design           also depends on hrf
+-- mvpa
|   +-- mvpa-dataset  also depends on dataset
+-- multivar
|   +-- multivar-ir
|   +-- inference     also depends on linalg
+-- threshold         also depends on image
+-- motion            also depends on image
+-- spatial           also depends on image, surface
+-- model             also depends on design, dataset
+-- fit               also depends on model, ar
+-- group             also depends on image, dataset, design, fit

pipeline

graphics
+-- graphics-svg
+-- graphics-canvas
+-- graphics-java2d
+-- graphics-javafx
+-- image-view             also depends on image
|   +-- image-view-canvas  also depends on graphics-canvas
|   +-- image-view-java2d  also depends on graphics-java2d
|   +-- image-view-javafx  also depends on graphics-javafx
+-- surface-view           also depends on surface
    +-- surface-view-raster
    +-- surface-view-javafx  also depends on graphics-javafx
    +-- surface-view-three
    +-- surface-view-connectivity  also depends on connectivity

hrf
+-- design           also depends on linalg, graphics
    +-- model
        +-- fit
            +-- group

image
+-- registration
+-- image-view        also depends on graphics
+-- archive
|   +-- latent         also depends on linalg
|   +-- dataset        also depends on hrf, latent, bids
|       +-- mvpa-dataset  also depends on mvpa
+-- surface
|   +-- spatial        also depends on linalg, image
|   +-- atlas          also depends on image
|   +-- mvpa-spatial   also depends on mvpa, image, atlas
|   +-- surface-view   also depends on graphics
+-- threshold         also depends on linalg
+-- motion            also depends on linalg
+-- group             also depends on linalg, dataset, design, fit

bids
+-- dataset           one-way boundary parsing dependency; bids has no internal deps

bids + dataset + model + fit + group
+-- fmri-workflow     outer composition only; no lower module depends back on it

zarr
+-- zarr-codec-blosc-zstd  optional JVM JNI / Scala.js WASM codec provider
+-- archive-zarr      also depends on archive
    +-- dataset-zarr  also depends on dataset, image, bids

fit + mvpa
+-- mvpa-fit          run-local trial-readout and pattern-operator composition
```

The `graphics` subtree has a stricter extraction boundary: core has no internal
dependency, and SVG, Canvas, Java2D, and JavaFX each depend only on core. Image
and design modules consume that public API but are not part of the standalone
artifact family. The frozen artifact matrix and lift-and-shift procedure are in
[`plans/graphics-extraction.md`](plans/graphics-extraction.md); the boundary is
enforced by `GraphicsExtractionGuardSuite` in `graphicsJVM/test`.

The local typed dataframe modules formerly incubated here were extracted to the
standalone [`frame4s`](https://github.com/canardlapin/frame4s) repository. They
are no longer part of the ScalaFIM build or internal dependency graph.

`pipeline` depends only on `graph` for validated DAG layering. It owns generic
graph orchestration without forcing workflow dependencies into the computational core;
`fmri-workflow` will add that edge when its orchestration lowering lands.
`bids` remains free of internal ScalaFIM dependencies and parses/describes BIDS
projects without pulling in dataset, image IO, or modeling dependencies. Cats
Core is confined to internal shared validation accumulation; Cats Effect is
confined to the JVM loader/store interpreter. Dataset IO adapters may consume
`bids` at the boundary when they need BIDS metadata tables or exact entity
parsing.

## Module Roles

| Module | Owns | Depends On | Do Not Put Here |
| --- | --- | --- | --- |
| `locus-kernel` | Semantic finite spaces and points, unordered extensional regions, ordered selections, exact total maps, finite relations, and validated map/relation evidence. | Nothing internal. | Geometry, labels, storage, interpolation, graphs, atlas metadata, or ordered region semantics. |
| `locus-data` | Pure indexed fields and sections, supported parcellations, searchlights, and one-pass commutative aggregation. | `locus-kernel` | Image/surface geometry, atlas ontology, lazy execution, IO, or probabilistic membership. |
| `locus-laws` | Exhaustive finite reference models, reusable law groups, differential adapters, and bounded ScalaCheck supplements. | `locus-data` | Production representations or domain policy. |
| `graph` | Ordered keyed vertex bases, locus-space adaptation, graph-local dense coordinates, validated alignment/permutation values, canonical immutable simple directed/undirected graphs, components, nonnegative-cost paths, cycle witnesses, DAG layers, and topology laws. | `locus-kernel` | Matrices, Laplacians, spectra, connectivity measures, spatial-domain validation, pipeline execution, multigraphs, or loops. |
| `graph-linalg` | Basis-carrying topology/weighted adjacency, incidence, degree/strength, combinatorial/normalized Laplacians, spectra/embeddings, aligned spectral feature maps, and explicitly PSD-tagged similarities over shared linalg contracts. | `graph`, `linalg` | Connectivity estimation or projection policy, scientific measurement semantics, solver implementations, generic multivar kernel ownership, or domain-specific convenience APIs. |
| `linalg` | Primitive vectors, matrices, sparse linear maps, solver contracts, portable reference decompositions, linear solves, projection kernels, and backend adapter boundaries. | Nothing internal. | fMRI, image, dataset, domain-specific spatial concepts, or direct domain-module ownership of eigensolver/SVD/inverse helpers. |
| `linalg-breeze` | JVM-only Breeze-backed adapters for linalg solver contracts and backend differential tests. | `linalg` | Shared APIs, domain-specific algorithms, Scala.js code, or direct Breeze exposure to domain modules. |
| `pipeline` | Generic typed pipeline graphs, artifact references, graph-delegated deterministic DAG staging, local pure execution, and structured receipts. | `graph` | Neuroimaging algorithms, file IO, external CLI execution, scheduler/runtime implementations, or lower-module convenience helpers. |
| `graphics` | Renderer-neutral graphics algebra: grammar-of-graphics plot/layer specs, row-aware typed aesthetics/scales, immutable grid-like grob scene trees (y-up), a phased plot compiler with derived guides and a layout solver, numeric device-scene resolution, and the renderer conformance contract. | Nothing internal. | Java2D/JavaFX/Canvas rendering, device IO, neuroimaging-specific plot exports, or mutable display-list state. |
| `graphics-svg` | Deterministic SVG string serialization of resolved `graphics` device scenes (numeric-only geometry, clip paths, rotation), validated against the shared renderer conformance contract. | `graphics` | Plot compilation, browser Canvas state, Java2D/raster output, device IO, or domain-specific plot exporters. |
| `graphics-canvas` | Scala.js Canvas 2D command compilation and browser-context interpretation, with deterministic command logs validated against the shared renderer conformance contract. | `graphics` | Plot compilation, SVG serialization, JVM raster output, browser DOM ownership, or domain-specific plot exporters. |
| `graphics-java2d` | JVM Java2D command compilation and `Graphics2D` raster interpretation, validated with deterministic commands, shared conformance, and real image assertions. | `graphics` | Plot compilation, SVG/Canvas rendering, Scala.js code, device IO, or domain-specific plot exporters. |
| `graphics-javafx` | JVM JavaFX Canvas command compilation and `GraphicsContext` interpretation behind a toolkit-free drawing contract, validated with deterministic commands and shared conformance. | `graphics` | Plot compilation, SVG/Canvas/Java2D rendering, Scala.js code, toolkit lifecycle ownership (application threads, stages), or domain-specific plot exporters. |
| `hrf` | HRFs, basis functions, sampling frames, convolution primitives. | Nothing internal. | Design formulas, datasets, or model fitting. |
| `ar` | AR/ARMA whitening plans and pure prewhitening kernels. | `linalg` | GLM fitting orchestration or dataset IO. |
| `design` | Event models, formulas, baselines, contrasts, design metadata, and renderer-neutral design plot exports. | `hrf`, `linalg`, `graphics` | Dataset execution, numerical fit engines, or concrete renderers such as SVG/Java2D/Canvas. |
| `image` | Volumes, masks, exact volume locus domains, locus-backed regions/selections, affine math, low-level coordinate transforms, morphisms, resampling, clustering, and metric searchlight construction. | `locus-data` | Atlas registries, dataset backends, graph-level operator caches, JVM-only image readers in shared code. |
| `registration` | Frame-safe inverse pairs, symmetric midpoint deformation, paired diffeomorphic flow construction, topology/inverse guards, and nonlinear registration optimization. | `image` | Generic tensor/field kernels, image IO, atlas catalogs, dataset policy, or registration-specific shortcuts in Gale. |
| `image-view` | Renderer-neutral world-space slice views: typed colorizers/layers, orthogonal scene compilation, crosshairs, orientation labels, and panel receipts. | `image`, `graphics` | NIfTI IO, mutable toolkit widgets, DOM/JavaFX lifecycle ownership, or concrete renderer command interpretation. |
| `image-view-canvas` | Browser Canvas rendering host plus canvas-relative pointer/wheel translation into pure viewer actions. | `image-view`, `graphics-canvas` | Image geometry, DOM ownership, application state mutation, or alternate renderer logic. |
| `image-view-java2d` | Java2D rendering host plus device-relative event translation and `BufferedImage` convenience rendering. | `image-view`, `graphics-java2d` | Image geometry, Swing lifecycle ownership, or alternate renderer logic. |
| `image-view-javafx` | JavaFX Canvas rendering host plus device-relative event translation through the toolkit-free graphics context boundary. | `image-view`, `graphics-javafx` | Image geometry, JavaFX application/thread lifecycle ownership, or alternate renderer logic. |
| `threshold` | Spatial inference over statistic maps: locus-backed active/full support, scored candidates, octrees, set scoring, and maxT-style correction. | `image`, `locus-kernel`; Gale on each platform | Model fitting, group-model definitions, or a second generic region abstraction. |
| `motion` | Rigid poses/traces, FD/DVARS, motion QC, one-pass rigid application over image data. | `image`, `linalg` | Heavy registration engines, NIfTI IO, reports, or GLM nuisance modeling. |
| `surface` | Meshes, exact topology/order locus domains, vertex fields, region-backed surface ROIs, quotient-backed labels, geodesic searchlights, graph interop, and JVM surface readers. | `graph`, `image`, `locus-data` | Atlas metadata, MVPA plans, or whole spatial graph compilation. |
| `surface-view` | Renderer-neutral surface assets/layers, immutable display state and reducer, anatomical cameras/layouts, render-plan compilation, resource identity, temporal/projection/network primitives, scene documents, backend capabilities, and admission contracts. | `surface`, `graphics` | JavaFX/Three.js objects, DOM/window lifecycle, connectivity estimation, or platform IO. |
| `surface-view-raster` | Deterministic JVM/Scala.js CPU raster, depth/culling/clipping, compositing, exact picks, and semantic reference receipts. | `surface-view` | Interactive toolkit lifecycle, platform-specific acceleration, or scientific-data policy. |
| `surface-view-javafx` | JVM JavaFX Scene3D plan interpretation, retained mesh/color-atlas resources, reducer-backed controller, picks, snapshots, and native receipts. | `surface-view`, `graphics-javafx`; external OpenJFX | Shared scientific semantics, application/stage ownership, Scala.js code, or silent fallback for unsupported plans. |
| `surface-view-three` | Scala.js Three.js/WebGL plan interpretation, retained GPU resources, native picks/snapshots, and feature-gated GPU volume projection. | `surface-view`; host-injected Three.js | DOM/bundler ownership, shared scientific semantics, or an assumption that WebGL2 float targets exist. |
| `surface-view-connectivity` | Typed conversion from connectivity edge spaces/vectors to renderer-neutral surface-network inputs and provenance. | `surface-view`, `connectivity` | Estimation/inference, backend objects, or alternate node identity. |
| `spatial` | Neurofunctor-style domains with locus packages, graph-delegated morphism routing, exact/crisp/sampled transport distinctions, selections, route policies, sampled operators, adjoints, provenance, QC, caches, and lazy fields. | `graph`, `linalg`, `image`, `surface`, `locus-data` | Low-level image interpolation kernels, atlas-specific route catalogs, or another finite-space/region implementation. |
| `atlas` | Standard atlas descriptors, parcel metadata, typed locus parcellations, parcel/network quotient operations, explicit display order, one-pass reduction, explicit-alignment overlap, region-graph interop, and transform route descriptors. | `graph`, `image`, `surface`, `locus-data` | Generic spatial operator compilation, low-level transform kernels, or extensional region identity in labels/metadata. |
| `archive` | Latent NeuroArchive-style manifests, transform descriptors, portable archive transforms, JVM HDF5 store. | `image` | Dataset selection APIs or model-level decoding policy. |
| `latent` | Latent-response contracts, temporal bases, archive codecs, transport responses, and exact locus-backed active/full-grid selection order. | `archive`, `locus-kernel`; Gale on each platform | Dataset storage backends, model execution, or another spatial selection algebra. |
| `dataset` | Checked source-blind fMRI views, semantic acquisition locus domains, provenance values, study/run keys and queries, ordered run-local windows and coordinates, segmented cross-run reads, backend contracts, and narrow LNA IO composition. | `image`, `hrf`, `archive`, `latent`, `bids`, `locus-data` | Storage-format inheritance, parallel study/selection/error algebras, design formulas, fit kernels, or general BIDS project ownership. |
| `bids` | Pure BIDS names/entities, checked manifests, queries, TSV tables, fMRIPrep confound selection, and domain diagnostics; JVM resource-safe project/store adapters. | Nothing internal; Cats Core in shared, Cats Effect on JVM. | Dataset execution, image decoding, model fitting, remote-store policy, or hidden runtime execution. |
| `model` | Inspectable fMRI model and fit plans: dataset plus design plus fitting configuration. | `design`, `dataset`, `linalg` | OLS/GLS kernels or backend implementations. |
| `fit` | Numerical fit engines over model plans: dense/runwise OLS, contrasts, residual diagnostics. | `linalg`, `model`, `ar` | Model description, dataset storage, or group inference. |
| `mvpa` | Portable sample-by-feature MVPA contracts, folds, feature-set plans, classifiers, RDM/RSA kernels. | `linalg` | Spatial object adapters or dataset backend logic. |
| `mvpa-fit` | Shared run-local composition of fit-owned trial readouts with MVPA pattern operators, checked common feature axes, trial/run metadata, fold restriction, and local task/result collection. | `fit`, `mvpa` | QR/GLM kernels, classifier numerics, a second feature-set abstraction, workflow scheduling, or platform IO. |
| `multivar` | One layered mathematical lifecycle: semantic `core`, mathematical `contract`, declarative `optimization`, executable `solver`, family-indexed `lifecycle`, fitted `capability`, statistical `family.*` verticals, fold-safe `workflow`, and `validation`; includes locus selection adapters, typed duality diagrams, GLRM, multiblock, paired/canonical/spectral methods, CPCA and kernels. | `linalg`, `locus-kernel` | Flat catch-all APIs, reciprocal family dependencies, formula/model-matrix builders, sample/feature metadata encoders, MVPA ROI adapters, dataset/image IO, language bindings, JVM solver backends, or scheduler-specific execution. |
| `multivar-ir` | Versioned language-neutral records and portable codecs for multivar spaces, operators, certificates, diagrams, alignments, objectives, projection actions, synthesis capabilities, solver guarantees, payload references, and conformance fixtures. | `multivar` | Statistical algorithms, backend storage ownership, Python/R runtime implementations, or platform-specific IO. |
| `inference` | Typed perturbation inference over fitted multivariate structures: invariant targets, resampling designs, lawful null/bootstrap actions, deterministic Monte Carlo ladders, latent units, alignment/stability summaries, validity, evidence, and provenance. | `multivar`, `linalg` | Multivariate fitting, GLM/group contrasts, spatial multiple testing, dataset/image IO, schedulers, or platform-specific random/runtime APIs. |
| `connectivity` | Shared connectivity algebra and portable kernels: graph-backed ordered node axes with scientific provenance, locus node/edge spaces and masks, parcel time series, explicit vectorization orders, static/dynamic containers, estimator plans, ETS/event-weighted correlation, partial correlation, connectivity-set inference, dynamic stacks, diagnostics, and workflow receipts. | `graph`, `locus-kernel`; Gale on each platform | Dataset backends, atlas/BIDS adapters, plotting, JVM IO, multivar execution bridges, TVGL/SRLC/phase/HMM internals, native optimizer backends, or scheduler/runtime execution. |
| `mvpa-dataset` | Typed adapters from `FmriSeries`/`FmriDataset` reads and sample metadata into MVPA pattern sources. | `mvpa`, `dataset` | Classifier algorithms, dataset storage backends, or spatial feature-set construction. |
| `mvpa-spatial` | Thin adapters from locus regions, selections, parcellations, and searchlights plus image/surface/atlas objects into MVPA feature-set plans. | `mvpa`, `image`, `surface`, `atlas`, `locus-data` | Classifier algorithms, atlas loading, or a second searchlight/window model. |
| `group` | Second-level/group GLM, meta-analysis, group contrasts, FDR over subjects-by-samples maps. | `linalg`, `image`, `dataset`, `design`, `fit` | First-level model fitting or thresholding internals. |
| `fmri-workflow` | Serializable study specifications, header-derived catalogs, deterministic first-level/group jobs, structural preflight, and result references; generic pipeline lowering is a future orchestration slice. | `bids`, `dataset`, `model`, `fit`, `group` | Numeric kernels, concrete file readers/writers, scheduler APIs, open resources, matrices, or captured execution closures. |
| `zarr` | Dependency-free Zarr v3 metadata plus read-only v2 lowering, runtime-rank hierarchy and factored slice/gather geometry, direct/sharded planning, backpressured chunk fragments, primitive codecs, portable bounded async reads, revision-scoped bounded object/range caches, store-independent sync/async create-only writers, content receipts, and atomic JVM publication. | Nothing internal. | Neuroimaging semantics, BIDS identity, S3 credentials, persistent cache/prefetch/retention policy, mutation, v2 writing, or hidden execution policy. |
| `zarr-codec-blosc-zstd` | Optional typed Zarr v3 Blosc/Zstandard capability, bounded frame validation, JVM JNI executor, and Scala.js embedded-WASM executor. | `zarr`; external platform codec dependencies | Generic Zarr planning, neuroimaging semantics, or an implied guarantee that Scala.js can encode every Blosc `typesize`. |
| `archive-zarr` | NeuroArchive Zarr 0.1 canonical-BOLD refinement, measured layout profiles, scientific manifests, content identity, immutable publication, and audit. | `zarr`, `archive` | Dataset selection APIs, NIfTI/BIDS IO, catalogs, or generic Zarr mechanics. |
| `dataset-zarr` | JVM NeuroArchive-to-`FmriDataset` composition, regular-timing refinement, ordered selection lowering, Zarr-backed response blocks, streaming raw-scalar NIfTI import, and raw-scalar- and affine-preserving BIDS/NIfTI export within the documented NeuroArchive 0.1 subset. | `dataset`, `archive-zarr`, `image`, `bids` | Generic array mechanics, fit kernels, catalog policy, synchronous browser facades, or browser file IO. |

The binding `multivar` migration target is the
[single-layer typed operator core](plans/multivar-operator-core.md): one directed
operator representation, `secondOrder` and `compress` as the only second-order
and component reductions, `FunctionalFrame` as the latent parameter, and named
methods lowering to one closed `OperatorProgram`. Fitted projection/synthesis
capabilities remain downstream compositions over the frozen frame, while
family-indexed workflows own data-fitted lifecycle stages. Its
production-consumer table is the authoritative ownership map.
The source-level ownership law is the
[multivar package hierarchy](plans/multivar-package-hierarchy.md): packages
expose the order from semantic algebra through contracts, programs, solvers,
lifecycle evidence, family verticals, workflow composition, and validation
without introducing a second runtime model.

## Main Vertical Flows

### First-level fMRI GLM

```text
bids -> dataset -> model -> fit
        ^          ^
        |          |
      archive    design -> hrf
      latent
        ^
        |
archive-zarr -> dataset-zarr
```

`bids` describes files and confounds. `dataset` provides selected data.
`design`/`hrf` describe regressors. `model` joins those descriptions into an
inspectable plan. `fit` executes the plan with `linalg` and optionally `ar`.
LNA composes into `dataset` directly because that module already owns the latent
archive adapter. NeuroArchive Zarr composes in `dataset-zarr`, the lowest module
that can see both `dataset` and `archive-zarr`; both paths expose
`Either[DatasetError, FmriDataset]`, and downstream reads are source-blind.
`fmri-workflow` sits above this flow and `group`: it carries only persistent
catalog/result references and immutable recipes, then delegates IO and numeric
execution to typed interpreters at module boundaries.

### Finite Indexed Spaces

```text
locus-kernel -> locus-data -> image/surface/atlas/spatial/dataset/mvpa-spatial
       |             |
       |             +-----> locus-laws  (test support)
       +-----> graph/connectivity/threshold/latent/multivar
```

`locus-kernel` is the sole owner of generic finite spaces, points, regions,
ordered selections, exact maps, and relations. `locus-data` owns fields,
sections, parcellations, searchlights, and aggregation. Domain modules add
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
graphics ----------------------+       +--> surface-view-three   (Scala.js)
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

dataset -> mvpa-dataset -> mvpa
image/surface/atlas -> mvpa-spatial -> mvpa
fit + mvpa -> mvpa-fit
multivar -> inference
```

`threshold` consumes statistic maps and masks; it is deliberately not a group
modeling module. `mvpa` is spatially agnostic; `mvpa-dataset` converts selected
dataset rows into pattern sources, while `mvpa-spatial` converts spatial objects
into feature sets.

## Neurotransform And Neurofunctor Boundary

`~/code/neurotransform` and `~/code/neurofunctor` are related, but ScalaFIM
keeps their durable ideas at different layers.

- `image` owns executable geometric transform kernels: `SpatialMorphism`,
  `IdentityMorphism`, `Affine3DMorphism`, dense displacement/coordinate fields,
  interpolation plans, `GridSpec`, `SpatialPoint`, `ResamplingPlan`, field
  materialization, and local execution-plan compaction/fusion.
- `registration` owns optimization-time inverse tracking, midpoint updates,
  paired flows, acceptance guards, and registration diagnostics over those image
  kernels.
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

- Put stable-key/local-index separation, canonical simple topology, basis
  alignment, and reusable graph laws in `graph`. Keep numerical projections in
  `graph-linalg`, and keep scientific
  connectivity semantics in `connectivity`.
- Put generic finite semantic domains, points, regions, selections, exact maps,
  and relations in `locus-kernel`; put indexed fields, sections,
  parcellations, searchlights, and commutative aggregation in `locus-data`;
  put reusable reference models and adapter law suites in `locus-laws`.
  Geometry, storage, metadata, and algorithm policy remain in their domain
  modules.
- Put primitive matrix/vector/operator math, solver contracts, portable
  eigensolver/SVD/QR/Cholesky/SPD-inverse reference implementations, and backend
  adapter boundaries in `linalg`. Domain modules should receive typed solver
  capabilities from `linalg`, not define private decomposition families.
  JVM-only libraries such as Breeze must stay behind adapter modules and never
  appear in shared APIs. The current JVM adapter is `linalg-breeze`; see
  `docs/plans/linalg-backend-strategy.md`.
- Put generic workflow graph algebra in `pipeline`; keep domain execution in the owning computational modules and adapt it upward.
- Put renderer-neutral plotting and scene-description contracts in `graphics`; keep concrete rendering backends and domain-specific plot exporters in adapters above it.
- Put deterministic SVG string rendering in `graphics-svg`, browser Canvas 2D rendering in `graphics-canvas`, and JVM raster rendering in `graphics-java2d`; keep future backends in separate adapters rather than broadening `graphics`.
- Put pure image-space kernels in `image`; platform IO goes in `image/jvm`.
- Put nonlinear registration state, objectives, deformation geometry, and
  acceptance policy in `registration`; keep reusable image-field kernels in
  `image` and reusable linear algebra in Gale.
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
  `dataset`; put reusable BIDS table/query logic in `bids`, and translate BIDS
  entities at dataset IO boundaries with exact parsed matches rather than
  filename substring filters.
- Put inspectable model descriptions in `model`; put numeric execution in
  `fit`; put group-level inference in `group`.
- Put cross-module BIDS-to-group study recipes, catalog/job identity, structural
  preflight, result references, and future pipeline lowering in `fmri-workflow`; keep
  payloads, open readers, format implementations, and scheduler types out of it.
- Put nominal primal/dual algebra, semantic duality diagrams, role-specific
  forms and certificates, row measures/centering, singular policies, explicit
  row relationships, direct-sum objectives, sparse-aware preprocessing, and
  decomposition artifacts in `multivar`. Put their versioned language-neutral
  records and conformance corpus in `multivar-ir`; keep neuroimaging adapters,
  bindings, and scheduler-specific execution in higher modules.
- Put perturbation targets, resampling/null actions, Monte Carlo programs,
  latent-unit stability, validity, and evidence provenance in `inference`;
  consume fitted geometry through small `multivar` capabilities and keep
  fitting, workflows, IO, and schedulers outside the module.
- Put shared connectivity structures in `connectivity`: validated series axes,
  explicit edge spaces/vectorization orders, static/dynamic containers,
  estimator/preprocessing plan ADTs, portable ETS/event, partial-correlation,
  group-inference, and low-risk dynamic kernels, diagnostics, and workflow
  receipts. Keep Ariadne/R compatibility orders explicit and keep dataset,
  atlas, BIDS, plotting, IO, multivar execution bridges, native optimizer
  backends, phase/HMM/TVGL/SRLC internals, and scheduler-specific execution
  outside this structural core.
- Put dataset/sample adapters for MVPA in `mvpa-dataset`; put spatial feature
  adapters for MVPA in `mvpa-spatial`; keep `mvpa` over plain sample-by-feature
  matrices and linear operators. Put fit-owned time-to-trial readout composition,
  run stacking, and trial/run metadata in `mvpa-fit` so neither lower module
  depends back on the other.

If a change wants a new dependency edge, stop and check whether the code belongs
in a higher adapter module instead. Lower modules should stay reusable and
workflow-neutral.
