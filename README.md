# scalafim

This repository consolidates the Scala fMRI and neuroimaging libraries into one
cross-compiled sbt build.

## Modules

- `graph`: dependency-free ordered vertex bases, canonical simple graph values, alignment, components, paths, cycles, DAG layers, and reusable topology laws.
- `graph-linalg`: basis-carrying topology/weighted adjacency, incidence, degree/strength, and Laplacian operators over the shared sparse linear-map contracts.
- `linalg`: small primitive array-backed vectors, matrices, and linear solves for portable fitting kernels.
- `pipeline`: generic typed pipeline graphs, artifact references, deterministic staging, local execution, and receipts.
- `graphics`: renderer-neutral grammar-of-graphics core, row-aware typed scales, immutable grid-like scene trees, and plot/layer specifications.
- `graphics-svg`: deterministic SVG renderer for `graphics` scene trees.
- `graphics-canvas`: Scala.js Canvas 2D renderer with deterministic command recording.
- `graphics-java2d`: JVM Java2D raster renderer with deterministic commands and image-level tests.
- `graphics-javafx`: JVM JavaFX Canvas renderer with deterministic commands behind a toolkit-free drawing contract.
- `latent`: typed latent fMRI response contracts, explicit basis/loadings responses, coefficient projection, and decoder-ready selections.
- `ar`: AR/ARMA whitening plans, run/censor-aware segment construction, and pure design/data prewhitening.
- `hrf`: hemodynamic response functions, bases, sampling, convolution, and regressors.
- `design`: fMRI event models, formulas, baselines, contrasts, and design matrices.
- `image`: neuroimaging volumes, masks, spaces, affine/dense-field spatial morphisms, indexing, statistics, clustering, and image IO.
- `image-view`: renderer-neutral world-space slice viewing, typed colorizers and layers, orthogonal scene compilation, and interaction receipts.
- `image-view-canvas`: thin Scala.js Canvas host for image-view scenes and device-event translation.
- `image-view-java2d`: thin JVM Java2D host with direct `Graphics2D` and `BufferedImage` rendering.
- `image-view-javafx`: thin JVM JavaFX Canvas host behind the existing toolkit-free drawing contract.
- `threshold`: spatial inference over statistic maps — LR-MFT set scoring, maxT/stepdown correction, octree regions, and thresholding primitives.
- `motion`: fMRI rigid-motion traces, baseline rigid estimation, motion QC metrics, and one-pass motion application over 4D runs.
- `surface`: surface-mesh data structures, topology, vertex fields, ROIs, surface sets, geodesics, parcel operations, and JVM surface IO.
- `spatial`: spatial-functor infrastructure — typed domains, sampled geometries, morphism-ready ids, hybrid offsets, and later graph/operator compilation.
- `atlas`: typed standard-atlas metadata, registry, transform plans, parcel lookup, reduction, overlap, and adjacency operations.
- `archive`: typed Latent NeuroArchive-style manifests, transform descriptors, validation, quant roundtrips, and JVM storage boundaries.
- `bids`: BIDS filename/entity parsing, typed manifests, query semantics, BIDS URIs, event tables, and fMRIPrep confound selections.
- `dataset`: fMRI dataset shapes, typed ids, selections, backend contracts, and series.
- `model`: fMRI model composition and typed fitting plans/configuration.
- `fit`: portable ordinary least squares kernels over timepoints-by-voxels response blocks.
- `mvpa`: portable MVPA engine primitives, fold plans, ROI feature sets, and RDM/crossnobis kernels.
- `mvpa-fit`: shared composition of fit-owned trial readouts with MVPA pattern operators, run metadata, and leave-one-run-out execution.
- `multivar`: typed duality-diagram core — nominal primal/dual spaces, certified row/column forms, semantic GPCA, explicit partial row alignment, direct-sum multiset objectives, sparse-aware operators, decompositions, and pure execution plans.
- `multivar-ir`: versioned language-neutral multivar semantics, portable JSON codecs, numeric payload references, and cross-binding conformance fixtures.
- `inference`: typed perturbation inference over fitted multivariate structures — targets, resampling designs, null actions, Monte Carlo ladders, latent units, stability, validity, and provenance.
- `connectivity`: shared typed connectivity algebra, edge spaces, vectorization orders, static/dynamic containers, and inspectable estimator plans.
- `mvpa-dataset`: typed adapters from dataset series and sample metadata into MVPA pattern sources.
- `mvpa-spatial`: adapters from atlas, volume searchlight, and surface parcel objects into MVPA feature-set plans.
- `group`: second-level (group) analysis — group GLM, fixed/random-effects meta-analysis, group contrasts, and FDR over subjects-by-samples effect maps.
- `fmri-workflow`: typed, payload-free study plans and catalogs that compose BIDS ingest, first-level fitting, durable results, group analysis, and scheduler-neutral orchestration.

Each module is built for both the JVM and Scala.js with `sbt-crossproject`.

See [docs/image-viewer.md](docs/image-viewer.md) for the world-coordinate
contract, slice and layer APIs, interaction reducer, caching receipts, and
platform-host boundaries.

## Common Commands

```sh
sbt compileAll
sbt testAll
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
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.describeStandardAtlases"
```

See [docs/ecosystem-port.md](docs/ecosystem-port.md) for the rewrite plan for
`fmridataset`, `fmrireg`, and later ecosystem modules.
See [docs/module-relations.md](docs/module-relations.md) for a concise map of
how the modules relate and where new code should live.
See [docs/plans/scenario-parity-harness.md](docs/plans/scenario-parity-harness.md)
for the Scala-native plan to add realistic workflow scenarios, parity fixtures,
and regenerable receipts without making Python/R part of the sbt test runtime.
