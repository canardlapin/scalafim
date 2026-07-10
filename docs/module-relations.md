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
linalg
+-- ar
+-- design           also depends on hrf
+-- mvpa
|   +-- mvpa-dataset  also depends on dataset
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

hrf
+-- design           also depends on linalg, graphics
    +-- model
        +-- fit
            +-- group

image
+-- archive
|   +-- latent         also depends on linalg
|   +-- dataset        also depends on hrf, latent
|       +-- mvpa-dataset  also depends on mvpa
+-- surface
|   +-- spatial        also depends on linalg, image
|   +-- atlas          also depends on image
|   +-- mvpa-spatial   also depends on mvpa, image, atlas
+-- threshold         also depends on linalg
+-- motion            also depends on linalg
+-- group             also depends on linalg, dataset, design, fit

bids
```

`pipeline` and `bids` are intentionally standalone today. `pipeline` owns
generic graph orchestration without forcing workflow dependencies into the
computational core. `bids` parses/describes BIDS projects without forcing
dataset, image IO, or modeling dependencies into the shared core.

## Module Roles

| Module | Owns | Depends On | Do Not Put Here |
| --- | --- | --- | --- |
| `linalg` | Primitive vectors, matrices, sparse linear maps, linear solves, projection kernels. | Nothing internal. | fMRI, image, dataset, or domain-specific spatial concepts. |
| `pipeline` | Generic typed pipeline graphs, artifact references, deterministic staging, local pure execution, and structured receipts. | Nothing internal. | Neuroimaging algorithms, file IO, external CLI execution, scheduler/runtime implementations, or lower-module convenience helpers. |
| `graphics` | Renderer-neutral graphics algebra: grammar-of-graphics plot/layer specs, row-aware typed aesthetics/scales, immutable grid-like grob scene trees, units, viewports, and graphical parameters. | Nothing internal. | Java2D/JavaFX/Canvas rendering, device IO, neuroimaging-specific plot exports, or mutable display-list state. |
| `graphics-svg` | Deterministic SVG string rendering for `graphics` scene trees, including primitive grobs, graphical params, basic units, and viewport wrappers. | `graphics` | Plot compilation, browser Canvas state, Java2D/raster output, device IO, or domain-specific plot exporters. |
| `graphics-canvas` | Scala.js Canvas 2D command compilation and browser-context interpretation, with deterministic command logs validated against the shared renderer conformance contract. | `graphics` | Plot compilation, SVG serialization, JVM raster output, browser DOM ownership, or domain-specific plot exporters. |
| `graphics-java2d` | JVM Java2D command compilation and `Graphics2D` raster interpretation, validated with deterministic commands, shared conformance, and real image assertions. | `graphics` | Plot compilation, SVG/Canvas rendering, Scala.js code, device IO, or domain-specific plot exporters. |
| `hrf` | HRFs, basis functions, sampling frames, convolution primitives. | Nothing internal. | Design formulas, datasets, or model fitting. |
| `ar` | AR/ARMA whitening plans and pure prewhitening kernels. | `linalg` | GLM fitting orchestration or dataset IO. |
| `design` | Event models, formulas, baselines, contrasts, design metadata, and renderer-neutral design plot exports. | `hrf`, `linalg`, `graphics` | Dataset execution, numerical fit engines, or concrete renderers such as SVG/Java2D/Canvas. |
| `image` | Volumes, masks, spaces, affine math, low-level coordinate transforms, morphisms, resampling, clustering/searchlights. | Nothing internal. | Atlas registries, dataset backends, graph-level operator caches, JVM-only image readers in shared code. |
| `threshold` | Spatial inference over statistic maps: masked fields, octrees, set scoring, maxT-style correction. | `image`, `linalg` | Model fitting or group-model definitions. |
| `motion` | Rigid poses/traces, FD/DVARS, motion QC, one-pass rigid application over image data. | `image`, `linalg` | Heavy registration engines, NIfTI IO, reports, or GLM nuisance modeling. |
| `surface` | Meshes, topology, vertex fields, surface ROIs, geodesics, labels, JVM surface readers. | `image` | Atlas metadata, MVPA plans, or whole spatial graph compilation. |
| `spatial` | Neurofunctor-style domains, morphism graphs, route policies, sampled operators, adjoints, provenance, QC, caches, lazy fields. | `linalg`, `image`, `surface` | Low-level image interpolation kernels or atlas-specific route catalogs. |
| `atlas` | Standard atlas descriptors, region metadata, parcel payloads, coordinate/parcel lookup, transform route descriptors. | `image`, `surface` | Generic spatial operator compilation or low-level transform kernels. |
| `archive` | Latent NeuroArchive-style manifests, transform descriptors, portable archive transforms, JVM HDF5 store. | `image` | Dataset selection APIs or model-level decoding policy. |
| `latent` | Latent-response contracts, temporal bases, archive codecs, transport responses. | `linalg`, `archive` | Dataset storage backends or model execution. |
| `dataset` | Dataset shapes, ids, selections, fMRI series, backend contracts and adapters. | `image`, `hrf`, `archive`, `latent` | Design formulas, fit kernels, or BIDS filesystem walking. |
| `bids` | BIDS names/entities, manifests, queries, TSV tables, fMRIPrep confound selection. | Nothing internal. | Dataset execution, image decoding, or model fitting. |
| `model` | Inspectable fMRI model and fit plans: dataset plus design plus fitting configuration. | `design`, `dataset`, `linalg` | OLS/GLS kernels or backend implementations. |
| `fit` | Numerical fit engines over model plans: dense/runwise OLS, contrasts, residual diagnostics. | `linalg`, `model`, `ar` | Model description, dataset storage, or group inference. |
| `mvpa` | Portable sample-by-feature MVPA contracts, folds, feature-set plans, classifiers, RDM/RSA kernels. | `linalg` | Spatial object adapters or dataset backend logic. |
| `mvpa-dataset` | Typed adapters from `FmriSeries`/`FmriDataset` reads and sample metadata into MVPA pattern sources. | `mvpa`, `dataset` | Classifier algorithms, dataset storage backends, or spatial feature-set construction. |
| `mvpa-spatial` | Thin adapters from image/surface/atlas objects into MVPA feature-set plans. | `mvpa`, `image`, `surface`, `atlas` | Classifier algorithms or atlas loading. |
| `group` | Second-level/group GLM, meta-analysis, group contrasts, FDR over subjects-by-samples maps. | `linalg`, `image`, `dataset`, `design`, `fit` | First-level model fitting or thresholding internals. |

## Main Vertical Flows

### First-level fMRI GLM

```text
bids -> dataset -> model -> fit
        ^          ^
        |          |
      archive    design -> hrf
      latent
```

`bids` describes files and confounds. `dataset` provides selected data.
`design`/`hrf` describe regressors. `model` joins those descriptions into an
inspectable plan. `fit` executes the plan with `linalg` and optionally `ar`.

### Spatial Data And Transforms

```text
image -> surface
  |        |
  +-----> atlas
  +-----> spatial <----- surface
```

`image` owns executable low-level volume geometry. `surface` owns mesh geometry.
`atlas` names standard spaces and parcels. `spatial` compiles reusable
source-to-target operators with provenance and adjoints.

### Inference And Analysis

```text
fit -> group -> threshold
image --------^       ^
dataset -------------+

dataset -> mvpa-dataset -> mvpa
image/surface/atlas -> mvpa-spatial -> mvpa
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

- Put primitive matrix/vector/operator math in `linalg`.
- Put generic workflow graph algebra in `pipeline`; keep domain execution in the owning computational modules and adapt it upward.
- Put renderer-neutral plotting and scene-description contracts in `graphics`; keep concrete rendering backends and domain-specific plot exporters in adapters above it.
- Put deterministic SVG string rendering in `graphics-svg`, browser Canvas 2D rendering in `graphics-canvas`, and JVM raster rendering in `graphics-java2d`; keep future backends in separate adapters rather than broadening `graphics`.
- Put pure image-space kernels in `image`; platform IO goes in `image/jvm`.
- Put mesh and vertex-domain algorithms in `surface`.
- Put named atlas descriptors and parcel metadata in `atlas`.
- Put route compilation, sparse projectors, adjoints, QC, provenance, and
  operator caches in `spatial`.
- Put dataset selection/storage contracts in `dataset`; put file-name discovery
  and BIDS table logic in `bids`.
- Put inspectable model descriptions in `model`; put numeric execution in
  `fit`; put group-level inference in `group`.
- Put dataset/sample adapters for MVPA in `mvpa-dataset`; put spatial feature
  adapters for MVPA in `mvpa-spatial`; keep `mvpa` over plain sample-by-feature
  matrices.

If a change wants a new dependency edge, stop and check whether the code belongs
in a higher adapter module instead. Lower modules should stay reusable and
workflow-neutral.
