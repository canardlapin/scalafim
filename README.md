# scalafim

This repository consolidates the Scala fMRI and neuroimaging libraries into one
cross-compiled sbt build.

## Modules

- `locus-data`: ScalaFIM domain adapters, supported parcellations, searchlights, and one-pass commutative aggregation over standalone locus4s spaces and data.
- `linalg`: small primitive array-backed vectors, matrices, and linear solves for portable fitting kernels.
- `pipeline`: generic typed pipeline graphs, artifact references, deterministic staging, local execution, and receipts.
- `response`: dependency-light response identity, axis-safe selections, owned time-by-sample `Double` blocks, source planning, provenance, and physical-read receipts.
- `response-laws`: reusable JVM/Scala.js law checks for response ordering, shape, decode consistency, partitions, raw-bit persistence, receipts, and provenance.
- `latent`: archive-independent fMRI response mathematics, inspectable applicative decode plans, explicit basis/loadings responses, coefficient projection, and locus-backed active/full-grid selections.
- `ar`: AR/ARMA whitening plans, run/censor-aware segment construction, and pure design/data prewhitening.
- `hrf`: causal hemodynamic kernels with enforced causality and declared support, pulse-shaped neural drive (impulse, unit-height and unit-mass boxes), convergent box quadrature, response bases with typed dual coefficients and reported basis transforms, sampling, convolution, and regressors.
- `hrf-laws`: reusable JVM/Scala.js law checks for kernel causality and support, event additivity, homogeneity, permutation invariance, translation equivariance, pulse and quadrature convergence, basis reconstruction and gauge invariance, and evaluation-plan equivalence.
- `design`: fMRI event models, formulas, baselines, contrasts, and design matrices.
- `image`: neuroimaging volumes, locus-backed masks/selections and volume domains, metric searchlight construction, affine/dense-field spatial morphisms, statistics, clustering, and image IO.
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
- `archive`: format-neutral revisions and publication state, separately versioned normalized manifests, exact canonical encoding, transactional write orchestration, and typed resource-backed payload execution and receipts.
- `archive-lna`: typed LNA 2 schema, pure manifest normalization, validation, shared-basis artifacts, payload codecs, JVM HDF5 stores, and the eager physical driver; no scientific reconstruction.
- `interop-archived-response`: typed representation-to-archive lowering for LNA and Zarr, LNA pipeline reconstruction, archive-aware dataset adapters, first-class narrow representation envelopes, canonical dense-BOLD response binding, explicit registries, and resource-safe runtime assembly.
- `dataset`: pure fMRI descriptions and queries, explicit synchronous readers, checked effectful response attachment, acquisition locus domains, typed study/run indexing, segmented reads, evidence propagation, and series adapters.
- `model`: fMRI model composition and typed fitting plans/configuration.
- `fit`: portable fit kernels plus explicit synchronous-reader and effectful opened-dataset execution boundaries.
- `mvpa`: portable MVPA engine primitives, fold plans, ROI feature sets, and RDM/crossnobis kernels.
- `mvpa-fit`: shared composition of fit-owned trial readouts with MVPA pattern operators, run metadata, and leave-one-run-out execution.
- Multivariate perturbation inference now lives in standalone [`multivar-inference`](https://github.com/canardlapin/multivar/tree/main/modules/inference); ScalaFIM keeps only downstream domain adapters.
- `connectivity`: shared typed connectivity algebra, locus-backed node/edge domains and masks, explicit vectorization orders, static/dynamic containers, and inspectable estimator plans.
- `mvpa-dataset`: typed synchronous-reader and effectful opened-dataset adapters into MVPA pattern sources.
- `mvpa-spatial`: adapters from locus regions, selections, parcellations, and searchlights plus image/surface/atlas objects into MVPA feature-set plans.
- `group`: second-level (group) analysis — group GLM, fixed/random-effects meta-analysis, group contrasts, and FDR over subjects-by-samples effect maps.
- `fmri-workflow`: typed, payload-free study plans and catalogs that compose BIDS ingest, first-level fitting, durable results, group analysis, and scheduler-neutral orchestration.
- Generic Zarr mechanics and the optional Blosc/Zstandard provider now live in the standalone `zarr4s` repository. ScalaFIM consumes its core through a pinned source build and retains only neuroimaging-specific adapters.
- `archive-zarr`: NeuroArchive Zarr 0.1 refinement with canonical BOLD archive metadata, immutable publication, measured sharded layout policy, and resource-safe typed async payload execution with exact object/range evidence.
- `dataset-zarr`: JVM NeuroArchive-to-`FmriDataset` composition, bounded reads, regular-timing refinement, streaming NIfTI import, and BIDS/NIfTI export over NeuroArchive Zarr revisions.

Each module is built for both the JVM and Scala.js with `sbt-crossproject`.

The immutable typed dataframe work formerly incubated here now lives in the
standalone [`frame4s`](https://github.com/canardlapin/frame4s) repository.

Reusable BIDS names, manifests, queries, metadata tables, confound selection,
and resource-safe loaders formerly incubated here now live in the standalone
[`bids4s`](https://github.com/canardlapin/bids4s) repository. ScalaFIM pins an
immutable source revision and consumes it only at BIDS-facing integration
boundaries.

General multivariate analysis and its language-neutral IR now live in the
standalone [`multivar`](https://github.com/canardlapin/multivar) repository.
ScalaFIM pins an immutable source revision and owns only downstream
neuroimaging integrations.

Nonlinear registration and the experimental HalfFlow engine now live in
standalone [`reframe4s`](https://github.com/canardlapin/reframe4s). ScalaFIM
does not retain a registration module or depend on reframe4s.

Renderer-neutral graphics, plotting, and the SVG/Canvas/Java2D/JavaFX backends
live in standalone [`Intaglio`](https://github.com/canardlapin/intaglio).
ScalaFIM pins an immutable source revision; design and viewer modules consume
only the smallest required Intaglio core or backend project.

Generic graph topology and algorithms formerly incubated here now live in
standalone [`graph4s`](https://github.com/canardlapin/graph4s). ScalaFIM pins an
immutable source revision. The optional `graph4s-gale` module now owns indexed
numerical graph operators, spectra, embeddings, and similarities. Pipeline,
surface, atlas, and connectivity retain only ScalaFIM domain metadata and
adapters.

Generic finite domains, points, regions, selections, maps, relations, indexed
fields, and their laws formerly incubated here now live in standalone
[`locus4s`](https://github.com/canardlapin/locus4s). ScalaFIM pins an immutable
source revision; `locus-data` retains only ScalaFIM-specific adapters and
higher-level parcellation, searchlight, and aggregation policy.

An ordinary build loads both libraries from their pinned GitHub revisions. To
test coordinated changes in sibling checkouts, select those checkouts
explicitly:

```sh
sbt \
  -Dscalafim.graph4s.build=../graph4s \
  -Dscalafim.locus4s.build=../locus4s \
  compileAll
```

The override applies only to that sbt process. Removing the properties restores
the immutable GitHub source dependencies.

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
sbt locusDataJVM/test
sbt locusDataJS/test
sbt linalgJVM/test
sbt linalgJS/test
sbt pipelineJVM/test
sbt pipelineJS/test
sbt responseJVM/test
sbt responseJS/test
sbt responseLawsJVM/test
sbt responseLawsJS/test
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
sbt archiveLnaJVM/test
sbt archiveLnaJS/test
sbt archivedResponseInteropJVM/test
sbt archivedResponseInteropJS/test
sbt datasetJVM/test
sbt datasetJS/test
sbt modelJVM/test
sbt modelJS/test
sbt fitJVM/test
sbt fitJS/test
sbt archiveZarrJVM/test
sbt archiveZarrJS/test
sbt datasetZarrJVM/test
sbt datasetZarrJS/test
sbt mvpaJVM/test
sbt mvpaJS/test
sbt mvpaFitJVM/test
sbt mvpaFitJS/test
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
