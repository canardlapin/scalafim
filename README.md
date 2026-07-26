# scalafim

This repository consolidates the Scala fMRI and neuroimaging libraries into one
cross-compiled sbt build.

## Modules

- `locus-kernel`: dependency-free finite indexed spaces, unordered regions, ordered selections, exact maps, relations, and validated map/relation evidence.
- `locus-data`: indexed fields and sections, supported parcellations, searchlights, and one-pass commutative aggregation over locus spaces.
- `locus-laws`: cross-platform exhaustive/reference models, differential checks, and reusable law suites for locus implementations and adapters.
- `graph`: dependency-free ordered vertex bases, canonical simple graph values, alignment, components, paths, cycles, DAG layers, and reusable topology laws.
- `graph-linalg`: basis-carrying topology/weighted adjacency, incidence, degree/strength, and Laplacian operators over the shared sparse linear-map contracts.
- `linalg`: small primitive array-backed vectors, matrices, and linear solves for portable fitting kernels.
- `pipeline`: generic typed pipeline graphs, artifact references, deterministic staging, local execution, and receipts.
- `graphics`: renderer-neutral grammar-of-graphics core, row-aware typed scales and statistical transforms (count, histogram, summary intervals, density), immutable grid-like scene trees, and plot/layer specifications.
- `graphics-svg`: deterministic SVG renderer for `graphics` scene trees.
- `graphics-canvas`: Scala.js Canvas 2D renderer with deterministic command recording.
- `graphics-java2d`: JVM Java2D raster renderer with deterministic commands and image-level tests.
- `graphics-javafx`: JVM JavaFX Canvas renderer with deterministic commands behind a toolkit-free drawing contract.
- `latent`: typed latent fMRI response contracts, explicit basis/loadings responses, coefficient projection, and locus-backed active/full-grid selections.
- `ar`: AR/ARMA whitening plans, run/censor-aware segment construction, and pure design/data prewhitening.
- `hrf`: hemodynamic response functions, bases, sampling, convolution, and regressors.
- `design`: fMRI event models, formulas, baselines, contrasts, and design matrices.
- `image`: neuroimaging volumes, locus-backed masks/selections and volume domains, metric searchlight construction, affine/dense-field spatial morphisms, statistics, clustering, and image IO.
- `registration`: compact symmetric nonlinear registration algebra with typed inverse pairs, midpoint updates, paired diffeomorphic flows, and topology guards.
- `image-view`: renderer-neutral world-space slice viewing, typed colorizers and layers, orthogonal scene compilation, and interaction receipts.
- `image-view-canvas`: thin Scala.js Canvas host for image-view scenes and device-event translation.
- `image-view-java2d`: thin JVM Java2D host with direct `Graphics2D` and `BufferedImage` rendering.
- `image-view-javafx`: thin JVM JavaFX Canvas host behind the existing toolkit-free drawing contract.
- `threshold`: spatial inference over statistic maps — locus-backed active support, LR-MFT set scoring, maxT/stepdown correction, octree candidates, and thresholding primitives.
- `motion`: fMRI rigid-motion traces, baseline rigid estimation, motion QC metrics, and one-pass motion application over 4D runs.
- `surface`: surface-mesh data structures, exact locus domains, topology, vertex fields, region-backed ROIs, quotient-backed labels, geodesic searchlights, parcel operations, and JVM surface IO.
- `surface-view`: renderer-neutral surface layers, thresholds, layouts, cameras, interaction, projection/network primitives, scene documents, and versioned render plans.
- `surface-view-raster`: deterministic JVM/Scala.js CPU raster, depth, clipping, compositing, and picking oracle.
- `surface-view-javafx`: retained JVM JavaFX Scene3D backend, controller, picking, snapshots, and resource/timing receipts.
- `surface-view-three`: retained Scala.js Three.js/WebGL backend, browser picking/snapshots, and optional GPU volume projection.
- `surface-view-connectivity`: cross-platform adapter from typed connectivity edge spaces into surface network render resources.
- `spatial`: spatial-functor infrastructure — typed domains with locus packages, sampled geometries, exact/crisp/sampled transport, selections, lazy fields, and graph/operator compilation.
- `atlas`: typed standard-atlas metadata plus locus parcellations, registry, transform plans, parcel/network lookup, one-pass reduction, explicit-alignment overlap, and quotient adjacency.
- `archive`: typed Latent NeuroArchive-style manifests, transform descriptors, validation, quant roundtrips, and JVM storage boundaries.
- `bids`: BIDS filename/entity parsing, typed manifests, query semantics, BIDS URIs, event tables, and fMRIPrep confound selections.
- `dataset`: source-blind fMRI views, acquisition locus domains, checked archive/backend attachment, typed study/run indexing, ordered spatial/temporal selections, segmented cross-run reads, and series.
- `model`: fMRI model composition and typed fitting plans/configuration.
- `fit`: portable ordinary least squares kernels over timepoints-by-voxels response blocks.
- `mvpa`: portable MVPA engine primitives, fold plans, ROI feature sets, and RDM/crossnobis kernels.
- `mvpa-fit`: shared composition of fit-owned trial readouts with MVPA pattern operators, run metadata, and leave-one-run-out execution.
- `multivar`: typed duality-diagram core — nominal primal/dual spaces, certified row/column forms, semantic GPCA, explicit partial row alignment, locus selection adapters, direct-sum multiset objectives, sparse-aware operators, decompositions, and pure execution plans.
- `multivar-ir`: versioned language-neutral multivar semantics, portable JSON codecs, numeric payload references, and cross-binding conformance fixtures.
- `inference`: typed perturbation inference over fitted multivariate structures — targets, resampling designs, null actions, Monte Carlo ladders, latent units, stability, validity, and provenance.
- `connectivity`: shared typed connectivity algebra, locus-backed node/edge domains and masks, explicit vectorization orders, static/dynamic containers, and inspectable estimator plans.
- `mvpa-dataset`: typed adapters from dataset series and sample metadata into MVPA pattern sources.
- `mvpa-spatial`: adapters from locus regions, selections, parcellations, and searchlights plus image/surface/atlas objects into MVPA feature-set plans.
- `group`: second-level (group) analysis — group GLM, fixed/random-effects meta-analysis, group contrasts, and FDR over subjects-by-samples effect maps.
- `fmri-workflow`: typed, payload-free study plans and catalogs that compose BIDS ingest, first-level fitting, durable results, group analysis, and scheduler-neutral orchestration.
- `zarr`: dependency-free Scala 3 Zarr v3 kernel with read-only v2 lowering, runtime-rank hierarchy and factored selections, portable bounded async reads, revision-scoped LRU range caches, sync/async create-only writers, and atomic JVM publication.
- `zarr-codec-blosc-zstd`: optional typed Blosc/Zstandard provider using JNI on JVM and embedded WASM on Scala.js.
- `archive-zarr`: NeuroArchive Zarr 0.1 refinement with canonical BOLD semantics, immutable manifests/publication receipts, and measured sharded layout policy.
- `dataset-zarr`: JVM NeuroArchive-to-`FmriDataset` composition, bounded reads, regular-timing refinement, streaming NIfTI import, and BIDS/NIfTI export over NeuroArchive Zarr revisions.

Each module is built for both the JVM and Scala.js with `sbt-crossproject`.

The immutable typed dataframe work formerly incubated here now lives in the
standalone [`frame4s`](https://github.com/canardlapin/frame4s) repository.

See [docs/image-viewer.md](docs/image-viewer.md) for the world-coordinate
contract, slice and layer APIs, interaction reducer, caching receipts, and
platform-host boundaries.

See [docs/surface-viewer.md](docs/surface-viewer.md) for surface layers,
orientation and coordinate contracts, JavaFX/Three.js backends, lifecycle,
serialization, performance gates, and the cross-platform example.

## Common Commands

```sh
sbt compileAll
sbt testAll
sbt locusKernelJVM/test
sbt locusKernelJS/test
sbt locusDataJVM/test
sbt locusDataJS/test
sbt locusLawsJVM/test
sbt locusLawsJS/test
sbt graphJVM/test
sbt graphJS/test
sbt graphLinalgJVM/test
sbt graphLinalgJS/test
sbt linalgJVM/test
sbt linalgJS/test
sbt pipelineJVM/test
sbt pipelineJS/test
sbt graphicsJVM/test
sbt graphicsJS/test
sbt graphicsSvgJVM/test
sbt graphicsSvgJS/test
sbt graphicsCanvasJS/test
sbt graphicsJava2dJVM/test
sbt graphicsJavafxJVM/test
sbt latentJVM/test
sbt latentJS/test
sbt arJVM/test
sbt arJS/test
sbt hrfJVM/test
sbt hrfJS/test
sbt designJVM/test
sbt designJS/test
sbt imageJVM/test
sbt imageJS/test
sbt registrationJVM/test
sbt registrationJS/test
sbt imageViewJVM/test
sbt imageViewJS/test
sbt imageViewCanvasJS/test
sbt imageViewJava2dJVM/test
sbt imageViewJavafxJVM/test
sbt thresholdJVM/test
sbt thresholdJS/test
sbt motionJVM/test
sbt motionJS/test
sbt surfaceJVM/test
sbt surfaceJS/test
sbt surfaceViewJVM/test
sbt surfaceViewJS/test
sbt surfaceViewRasterJVM/test
sbt surfaceViewRasterJS/test
sbt surfaceViewJavafxJVM/test
sbt surfaceViewThreeJS/test
sbt surfaceViewConnectivityJVM/test
sbt surfaceViewConnectivityJS/test
sbt spatialJVM/test
sbt spatialJS/test
sbt atlasJVM/test
sbt atlasJS/test
sbt archiveJVM/test
sbt archiveJS/test
sbt bidsJVM/test
sbt bidsJS/test
sbt datasetJVM/test
sbt datasetJS/test
sbt modelJVM/test
sbt modelJS/test
sbt fitJVM/test
sbt fitJS/test
sbt zarrJVM/test
sbt zarrJS/test
npm ci --prefix modules/zarr-codec-blosc-zstd/js
sbt zarrCodecBloscZstdJVM/test
sbt zarrCodecBloscZstdJS/test
sbt archiveZarrJVM/test
sbt archiveZarrJS/test
sbt datasetZarrJVM/test
sbt datasetZarrJS/test
sbt mvpaJVM/test
sbt mvpaJS/test
sbt mvpaFitJVM/test
sbt mvpaFitJS/test
sbt multivarJVM/test
sbt multivarJS/test
sbt multivarIrJVM/test
sbt multivarIrJS/test
sbt inferenceJVM/test
sbt inferenceJS/test
sbt connectivityJVM/test
sbt connectivityJS/test
sbt mvpaDatasetJVM/test
sbt mvpaDatasetJS/test
sbt mvpaSpatialJVM/test
sbt mvpaSpatialJS/test
sbt groupJVM/test
sbt groupJS/test
sbt fmriWorkflowJVM/test
sbt fmriWorkflowJS/test
```

Runnable examples live under `examples/`. They are non-published sbt projects
with smoke tests:

```sh
sbt examplesTest
sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.describeStandardAtlases"
```

See [docs/ecosystem-port.md](docs/ecosystem-port.md) for the rewrite plan for
`fmridataset`, `fmrireg`, and later ecosystem modules.
See [docs/module-relations.md](docs/module-relations.md) for a concise map of
how the modules relate and where new code should live.
See [docs/plans/scenario-parity-harness.md](docs/plans/scenario-parity-harness.md)
for the Scala-native plan to add realistic workflow scenarios, parity fixtures,
and regenerable receipts without making Python/R part of the sbt test runtime.
