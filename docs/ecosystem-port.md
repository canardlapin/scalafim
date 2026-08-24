# ScalaFIM Ecosystem Port

This is the migration plan for moving the fMRI R ecosystem into the modular
`scalafim` build. The goal is not transliteration. The Scala code should be
a typed, composable rewrite that keeps the statistical semantics while leaving
R's S3/list-oriented surface behind.

## Design Rules

1. Model concepts as algebraic data types first. Prefer `enum`, `opaque type`,
   small `trait` contracts, and immutable `case class` values over stringly
   typed option lists.
2. Keep module boundaries honest. A lower-level module must not depend on a
   higher-level workflow module just to reuse a convenience helper.
3. Make invalid states unrepresentable where practical. Use constructors,
   opaque identifiers, and required invariants instead of downstream defensive
   checks.
4. Separate pure descriptions from execution. A model, fit plan, backend, or
   contrast specification should be inspectable before it opens files, starts
   workers, or allocates large matrices.
5. Cross-compile every public core module to JVM and Scala.js. Platform-specific
   implementations live in `jvm/` or `js/`; shared contracts stay in `shared/`.
6. Preserve R parity with fixtures and property tests, not by copying R's API
   shape. When the R surface is ad hoc, choose the smaller typed Scala concept.

These rules align with current Scala 3 language direction: opaque types define
domain-specific public APIs without runtime wrappers, extension methods provide
syntax where it helps readability, and enums/givens are the default tools for
closed choices and typeclass-style abstractions.

## Current Modules

- `scalafim-linalg`: primitive array-backed vectors, matrices, cross-products,
  and linear solves for portable numerical kernels.
- `scalafim-fmri-ar`: checked run/censor-aware AR estimation, immutable AR/ARMA
  whitening plans, diagnostics, and pure design/data prewhitening.
- `scalafim-image`: spatial data structures, masks, resampling, clustering, and
  image IO contracts.
- `scalafim-surface`: surface-mesh data structures, topology views,
  vertex-indexed fields, ROIs, surface sets, geodesic/parcel operations, and
  JVM-only surface geometry IO.
- `scalafim-atlas`: typed standard-atlas descriptors, region metadata,
  transform-route plans, coordinate lookup, parcel reduction, overlap, and
  adjacency operations over `scalafim-image` payloads.
- `scalafim-archive`: typed Latent NeuroArchive-style manifests, transform
  descriptors, payload references, validation layers, portable transform
  execution, and JVM-only storage boundaries.
- standalone `io.github.canardlapin:bids4s`: BIDS filename/entity parsing, immutable datatype specs,
  typed manifests and queries, BIDS URI resolution, dataframe-like TSV tables,
  and fMRIPrep confound set/strategy definitions. JVM adapters own file walking
  and image-header inspection; the shared core stays platform-neutral.
- `scalafim-fmri-hrf`: HRFs, bases, sampling frames, convolution, and regressors.
- `scalafim-fmri-design`: event models, formula parsing, baselines, contrasts,
  and design metadata.
- `scalafim-dataset`: fMRI dataset shape, typed ids, backend contracts,
  selections, and timepoints-by-voxels series.
- `scalafim-fmri-model`: first `fmrireg` landing zone, combining dataset and
  design objects with typed fit plans and configuration.
- `scalafim-fmri-fit`: portable fitting kernels, including dense OLS, runwise
  OLS, coefficient standard errors, residual diagnostics, and t-contrast
  evaluation over timepoints-by-voxels response blocks.
- `scalafim-fmri-group`: second-level (group) analysis over subjects-by-samples
  effect maps — a pure `GroupModel` description plus a total interpreter for the
  group GLM (OLS/WLS), fixed- and random-effects meta-analysis, group
  t-contrasts, and BH/BY FDR. See `docs/plans/group-analysis.md`.

## R Package Mapping

### `fmridataset`

Target module: `scalafim-dataset`

Initial Scala concepts:

- `DatasetId`, `SubjectId`, `SessionId`, `RunId` as opaque identifiers.
- `DatasetShape` as a spatial `NeuroSpace` plus timepoint count.
- `DataSelection` as explicit temporal and voxel index selections.
- `DatasetBackend` as the small storage contract replacing S3 generics.
- `FmriDataset` as the typed container joining a backend with a sampling frame
  and event table.
- `FmriSeries` as canonical timepoints x voxels data plus selection metadata.

Later ports:

- NIfTI, BIDS-H5, HDF5, Zarr, latent, matrix, and study backends.
- Event table adapters from BIDS and study-level metadata.
- Streaming/chunked reads as a separate capability, not hidden in `read()`.
- Backend resource lifecycle through platform-specific implementations.

### `neuroarchive`

Target module: `scalafim-archive`, with dataset-facing adapters in
`scalafim-dataset`.

The Scala rendering keeps the central LNA idea: compressed payloads are useful
only when stored with typed transform descriptors, spatial metadata, dataset
references, and a validation contract that makes reconstruction inspectable.
It does not port R6 handles, S3 dispatch, mutable transform registries, or JSON
option bags as the public API.

Initial Scala concepts:

- `ArchivePath` and `RunLabel` as checked identifiers.
- `LnaManifest`, `TransformDescriptor`, `DatasetRef`, and `Payload` as the
  pure archive contract.
- `LnaValidator` as layered structure, descriptor, reference, shape, and
  checksum validation.
- `QuantParams` and `LnaPipeline.quantArchive` as the first executable
  transform pipeline.
- `LatentArchiveDatasetBackend` as the bridge into the existing
  `DatasetBackend` contract.
- JVM storage adapters behind `scalafim.archive.io`; shared archive code does
  not depend on HDF5.

Later ports:

- Real JVM HDF5 read/write implementation for the storage SPI.
- Basis/embed/shared-basis pipelines, with fixtures against small
  `neuroarchive` archives where practical.
- Spatial experimental transforms only after the archive contract, quant
  roundtrip, and dataset backend path are stable.

### `neurotransform`

Target home: core transform algebra in `scalafim-image`, surface-specific
projection/resampling in `scalafim-surface`, route-planning integration in
`scalafim-atlas`, and transform file IO behind JVM boundaries.

`neurotransform` is the lightweight geometric transform kernel: first-class
identity, affine, nonlinear warp, volume-to-surface, and surface-to-surface
mappings with pullback semantics. In ScalaFIM this should not become a direct
S4-shaped port or a new dependency-heavy umbrella module. The shared core sits
where `NeuroSpace`, `DMat`, `NeuroVol`, affine utilities, and interpolation
already live.

Initial Scala concepts:

- `SpatialDomainId` as the checked opaque source/target domain identifier.
- `SpatialMorphism` as the small shared contract for target-to-source
  pullback coordinate transforms.
- `IdentityMorphism`, `Affine3DMorphism`, and `MorphismPath` as the first
  executable algebra, including domain-checked composition and analytic
  inversion.
- `JacobianField` and `JacobianMode` for pullback/pushforward affine and path
  Jacobians.
- Coordinate-convention helpers for RAS/LPS/tkRAS/FSL-style boundaries should
  remain pure shared helpers near the affine utilities.

Implemented first:

- Shared identity and affine morphisms over `DMat`.
- Source-to-target path validation with target-to-source pullback evaluation.
- Affine fusion during two-step composition.
- Analytic affine inversion and chain-rule Jacobians.
- RAS/LPS and tkRAS coordinate-convention helpers.
- `GridSpec` for regular 3D grid coordinate generation and voxel/world affine
  conversion.
- Dense displacement and absolute-coordinate image-space morphisms with
  nearest/linear/cubic field sampling and numeric pullback Jacobians.
- Reusable dense-field interpolation plans that precompute nearest/linear/cubic
  sampling weights for fixed query coordinates.
- `ResamplingPlan` over `GridSpec`, `SpatialMorphism`, and
  `NeuroVol[Double]`/`NeuroVec[Double]`, with precomputed target world, source
  world, and source voxel coordinates plus nearest/linear/cubic execution and
  optional Jacobian or square-root-Jacobian modulation.
- Dense source-coordinate/displacement field materialization and Jacobian
  determinant volumes for executable morphisms.
- Opt-in fixed-grid dense inverse approximation for displacement and
  absolute-coordinate morphisms, with convergence metadata rather than implicit
  inverse claims.
- `MorphismExecutionPlan` for validated paths with identity compaction and
  adjacent-affine fusion.
- JVM transform-file format descriptors for ANTs H5, FSL FLIRT/FNIRT,
  AFNI affine, FreeSurfer LTA, and X5 assets.
- `VolToSurfMorphism` and `SurfToSurfMorphism` homes in `scalafim-surface` for
  reusable sampling plans and explicit target-to-source vertex maps.
- Affine `SpaceTransforms` lowering into pullback `SpatialMorphism` values, and
  `spatial` graph-path lowering for executable identity/affine coordinate maps.

Later ports:

- Dense warp composition and stronger inverse estimation policies for harder
  non-near-identity fields.
- Actual ANTs H5, FSL/FNIRT, AFNI, FreeSurfer LTA, and X5 readers/writers in
  JVM platform packages; shared code should continue to expose typed
  descriptors, not file handles.
- Barycentric/ribbon surface morphism variants, surface adjoints beyond the
  current sparse operator path, and surface-to-surface inverse quality models.
- Nonlinear atlas route lowering once TemplateFlow/ANTs descriptors can be
  turned into executable dense morphisms.

Non-goals:

- Hidden downloads or implicit TemplateFlow/ANTs/FSL process execution from
  shared code.
- Treating volume-to-surface projection as an invertible 3D coordinate
  transform.
- Copying mutable loader registries or list-shaped R configuration into the
  public Scala API.

### `volregger`

Target module: a new cross-compiled `modules/motion` module published as
`scalafim-fmri-motion`, with package root `scalafim.fmri.motion`.

`volregger` is retrospective fMRI motion correction: six-parameter rigid motion
estimation, per-frame transform application, slice/packet-aware refinement,
motion QC, and optional repair/reporting layers. It should not be folded into
`scalafim-image`: image owns generic volumes, spaces, affine morphisms, and
resampling primitives, while motion correction owns temporal fMRI assumptions,
optimizer policy, acquisition timing, frame diagnostics, and censoring hints.
It should also not live in `scalafim-fmri-fit`, because it is preprocessing over
4D runs rather than statistical model fitting. The motion module should depend
on `image` and `linalg`; JVM-only NIfTI/CLI/reporting adapters can depend on
`image.io`, `bids`, or later workflow modules without pulling those concerns
into the shared core.

See `docs/plans/motion.md` for the staged module plan.

Initial Scala concepts:

- `RigidPose` as a checked six-DOF value (`tx`, `ty`, `tz`, `rx`, `ry`, `rz`)
  with matrix conversion through `DMat` and `Affine3DMorphism`.
- `MotionTrace` as a frame-indexed non-empty vector of `RigidPose` values, with
  opaque `FrameIndex`, `Millimeters`, `Radians`, and `Seconds` where useful.
- `ReferenceStrategy` as a closed choice: middle frame, robust mean, or explicit
  frame index.
- `MotionEngine` as a closed choice: `RigidRobust` first, `RigidSpline` later.
- `Interpolation` and `PadMode` as typed execution choices, mapped to reusable
  image resampling kernels rather than string flags.
- `MotionControl` as a small nested product of typed settings
  (`PyramidControl`, `OptimizerControl`, `CaptureControl`, `TemplateControl`,
  `TemporalControl`, `ExecutionControl`) instead of one large option list.
- `MotionProfile` as named constructors that produce `MotionControl` deltas and
  conflict diagnostics.
- `MotionEstimate`, `FrameFitDiagnostics`, `MotionQc`, and
  `MotionCorrectionResult` as immutable result records with finite-value and
  shape invariants.
- `SliceTiming` and `PacketTiming` as validated acquisition timing values for
  the later spline application path.
- `MotionError` as the public error ADT for bad dimensions, invalid masks,
  singular transforms, unsupported interpolation, failed convergence, and
  acquisition-timing mismatches.

First port:

- Move the reusable pose algebra into Scala: pose-to-matrix, matrix-to-pose,
  transform inversion/composition, framewise displacement, transform
  displacement over a head-radius surrogate, and masked displacement summaries.
- Add deterministic JVM/Scala.js tests against small fixtures exported from
  `volregger` for `motion_matrices()`, `invert_motion()`, `framewise_disp()`,
  `dvars()`, and displacement summaries.
- Add an `applyMotion` path over `NeuroVec[Double]` for rigid per-frame
  transforms using existing image-space morphism/resampling concepts. Start
  with linear interpolation and explicit clamp/zero padding; extend
  `scalafim-image` only for interpolation kernels that are genuinely generic.
- Model controls and profiles before the estimator is ported, so invalid
  optimizer configurations are rejected at construction and can be inspected in
  tests.
- Port the cheap QC layer (`fd`, `dvars`, robust DVARS, cost drop,
  motion-spike and censor suggestions) before the heavy optimizer.

Later ports:

- `RigidRobustEstimator`: middle/reference initialization, robust template
  refresh, information-content sampling, whitening/nuisance residual options,
  coarse-to-fine LM/Gauss-Newton updates, temporal warm starts, restart seeds,
  and frame-level diagnostics.
- High-order final interpolation (`cubic`/B-spline, Lanczos, quintic, heptic)
  as shared image kernels only if they are useful outside motion correction;
  otherwise keep the policy in `motion` and the primitive sampler in `image`.
- `RigidSpline`: smooth pose traces plus slice/packet-time application as a
  separate engine layered over the rigid estimate.
- Spin-history repair as an explicit post-estimation stage, not part of the
  rigid solver.
- JVM adapters for NIfTI input/output, BIDS/fMRIPrep metadata discovery,
  command-line execution, reports, plots, and benchmark harnesses.

Non-goals:

- A direct translation of the R S3 API, CLI contract, or list-shaped control
  object.
- Reusing Rcpp or JVM-native dependencies in the shared core.
- Making `motion` a general nonlinear registration package before the rigid
  fMRI path has portable parity tests.
- Hiding repeated resampling or repair stages inside a one-call workflow. The
  core should expose an inspectable estimate/apply/QC plan, with a convenience
  pipeline layered on top.

### `bidser`

Target library: standalone [`bids4s`](https://github.com/canardlapin/bids4s)

`bidser` is the BIDS-facing discovery, query, metadata, events, and fMRIPrep
confounds layer. The Scala rendering keeps the useful semantics while replacing
the R S3/mutable-registry surface with typed values and pure query
descriptions.

Initial Scala concepts:

- `BidsName`, `BidsEntities`, `EntityKey`, and `BidsDatatypeSpec` as the
  filename/entity kernel.
- `BidsRegistry` as an immutable parser registry with built-in raw and
  fMRIPrep datatype specs.
- `BidsManifest`, `BidsFile`, `BidsQuery`, `MatchMode`, and `BidsScope` as the
  query layer replacing `search_files()` / `query_files()`.
- `DatasetDescription` and `BidsUri` for DatasetLinks-aware URI resolution.
- `BidsTable`, `BidsColumn`, `BidsTableFile`, and `BidsEvents` for generic
  BIDS TSV tables, including tab-vs-whitespace delimiter sniffing and
  path/entity metadata attachment.
- `ConfoundSets`, `ConfoundStrategy`, `ConfoundSelector`, `NaAction`, and
  `ConfoundClean` as the fMRIPrep confound-selection foundation.

Implemented first:

- Shared JVM/Scala.js parser, render, manifest-query, URI, event-table, and
  confound-selection tests.
- JVM project loader for participants, derivative pipeline discovery, event and
  confound table reading, and sidecar inheritance over an indexed manifest.
- First-class `participants.tsv` access through `BidsProject.participantsTable`
  and JVM helpers for reading arbitrary BIDS TSV files.
- Project summaries for subject, session, task, and run entity values.
- Raw functional, fMRIPrep preprocessed, and anatomical scan selectors over the
  typed manifest.
- Scan-anchored metadata records plus repetition-time inference from inherited
  `RepetitionTime` or `VolumeTiming` metadata.
- fMRIPrep confound alias/wildcard resolution, missing-value policy, and
  zero-variance/rank diagnostics.
- PCA-backed `ConfoundStrategy` execution with retained component metadata.

Later ports:

- JVM NIfTI header adapter for `n_volumes`; no image IO dependency in the
  shared BIDS core.
- Broader scan-compliance summaries over the indexed manifest.
- Full schema validation, reports, plotting, downloads, and archive/packing are
  deferred IO/reporting layers, not first-cut core behavior.

### `fmrireg`

Target modules: `scalafim-fmri-model` for pure fit descriptions and
`scalafim-fmri-fit` for numerical execution kernels.

Initial Scala concepts:

- `FmriModel`: event model + baseline model + dataset, row-checked on
  construction.
- `FitConfig`: typed robust, AR, volume-weighting, nuisance-projection, and
  missing-data policy.
- `FitEngine`: a closed set of planned engines, separate from configuration.
- `FitPlan`: an inspectable pure description of what will be fitted.
- `WhiteningPlan`: the `fmriAR`-inspired pure AR/ARMA prewhitening description,
  separate from both model construction and fit execution.

Implemented first:

- Ordinary least squares and runwise least squares.
- Coefficient/result containers with standard errors, residual diagnostics, and
  t-contrast evaluation by design-column name.
- AR/ARMA whitening plans, Yule-Walker AR estimation, ACF diagnostics, and dense
  plan-based GLS execution for fixed AR(1)/AR(p) and estimated AR models.
- Strict whole-response rejection, explicit whole-voxel exclusion, and
  voxel-specific finite-row omission. Voxel-specific omission groups identical
  row masks for execution but returns pattern-partitioned results so rank,
  covariance, ordinary residual degrees of freedom, and AR reset geometry are
  never falsely shared across unlike voxels.

Later ports:

- Robust fitting, volume weighting, and nuisance projection as explicit
  transformations or engines.
- Voxelwise AR, iterated AR re-estimation, and ARMA fitting should extend the
  `WhiteningPlan` path rather than adding special-purpose GLS branches.
- Low-rank, latent sketch, and reduced-rank GLS only after the dense path has
  parity tests.
- Export/reporting/BIDS results in a separate IO/reporting layer, not in the
  fitting core.
- Distributed execution, including Spark-style adapters, should remain outside
  the shared computational core. Add it later as a JVM-only orchestration module
  once the dense engine has parity tests.

### `neurosurf`

Target module: `scalafim-surface`

Initial Scala concepts:

- `TriangleMesh` and `SurfaceGeometry` as immutable mesh and metadata values.
- Opaque vertex/face identifiers or equivalent typed indices.
- `MeshTopology` as a derived graph view over triangles, not a stored opaque
  graph object.
- `SurfaceField`, `SurfaceMatrix`, `SurfaceRoi`, `LabeledSurface`,
  `SurfaceSet`, and bilateral containers for vertex-indexed data.
- Geodesic, neighborhood, connected-component, and parcel operations as pure
  algorithms over the shared data model.

Later ports:

- Cross-platform GIFTI readers and JVM FreeSurfer readers under
  `scalafim.surface.io`.
- Surface-to-volume sampling bridges that reuse `scalafim-image` spaces and
  affine transforms.
- Renderer-neutral export data only after the computational core is stable.

Non-goals:

- RGL/htmlwidget plotting surfaces, color-map rendering classes, screenshots,
  and interactive viewers. These belong in a later visualization/export layer if
  needed.

### `fmrigds`

Target module: `scalafim-fmri-group`

fmrigds is a lazy, format-agnostic group-analysis engine (verb pipeline, mutable
reducer/adapter registries, `model.matrix` NSE). The Scala rendering keeps its
statistical content and its "a sample is a voxel / parcel / vertex / component"
insight, but replaces the R surface with a pure inspectable description plus a
total interpreter.

Initial Scala concepts:

- `GroupData`: the canonical subjects × samples × contrasts cube, one
  `GroupResponse` (effects + optional variances) per first-level contrast.
- `GroupSpace`: the sealed sample axis (`SampleAxis`, `VoxelAxis`, `ParcelAxis`).
- `GroupDesign`: an explicit `[subjects × terms]` design with combinators
  (`intercept`, `twoSample`, `covariates`) instead of `model.matrix`.
- `GroupWeighting`: the estimator as a sealed choice (`Unweighted`,
  `InverseVariance`, `RandomEffects`) — fmrigds's registry becomes a closed ADT.
- `GroupModel` / `GroupEngine`: inspectable description and total interpreter.
- `GroupContrast` / `Fdr`: name-keyed group t-contrasts and BH/BY correction.

Later ports (see `docs/plans/group-analysis.md`): restricted LMM, permutation
and spatial (TFCE/cluster) correction, parcel/Simes spatial FDR, evidence
combiners, disk adapters/catalog ingestion, a lazy verb-pipeline façade, and
cross-space variance propagation.

### `neurothresh`

Target module: `scalafim-fmri-threshold`

`neurothresh` is the spatial-inference layer: LR-MFT set statistics,
hierarchical/octree search, resampling-based FWER, TFCE, cluster-FDR, RFT
baselines, statistic canonicalization, and null-draw contracts. The Scala
rendering should be a new cross-compiled `modules/threshold` module, not part
of `image` or `group`: `image` owns spatial data structures, while `threshold`
owns statistical decisions over statistic maps. `group`, `fit`, `atlas`, and
`surface` can feed maps or regions into it later.

Initial Scala concepts:

- `Alpha`, `QValue`, `Kappa`, and `PermutationCount` as checked settings.
- `Tail`, `EvidenceScore`, and `StatKind` as closed method choices.
- `MaskedField`, `PriorWeights`, `Region`, and `RegionTree` as compact
  mask-space inputs for region scoring.
- `NullDraw` as a deterministic-by-index resampling contract.
- `WestfallYoung`, `MaxT`, and cluster-level FDR as pure multiple-testing
  kernels.
- `ThresholdResult` variants for LR-MFT, TFCE, cluster-FDR/FWER, and RFT.

First port: set scoring, Westfall-Young/maxT, octree split utilities, and R
parity fixtures. Later ports: full `HierScan`, TFCE FWER, cluster-FDR/RFT,
canonicalization, LISA, scale-space localized energy, and adapters from group,
surface, and atlas workflows. See `docs/plans/neurothresh.md`.

### `neuroatlas`

Target module: `scalafim-atlas`

Initial Scala concepts:

- `AtlasRef`, `AtlasArtifact`, and `AtlasHistoryStep` as structured provenance.
- `Region`, `RegionId`, `Hemisphere`, `NetworkId`, and `RegionIndex` as the
  region table replacement for S3 list fields and tibble metadata.
- `VolumeAtlas` as a typed wrapper over `ClusteredNeuroVol`.
- `AtlasRegistry` and `AtlasSpec` as immutable discovery values.
- `Schaefer2018` and `GlasserHcpMmp1` descriptors with typed parameter enums.
- `SpaceTransforms` as a graph-scored transform planner rather than a direct
  lookup table.

Implemented first:

- Shared JVM/Scala.js atlas metadata and registry.
- Coordinate exact/radius lookup.
- Parcel reductions over `NeuroVol` and `NeuroVec`.
- Overlap metrics and parcel adjacency edges.
- JVM asset-store and labelmap loading foundation.
- JVM Schaefer2018 and Glasser HCP-MMP1.0 loader entry points with deterministic
  parser/provenance tests.
- Toy fixtures covering non-download behavior.

Later ports:

- Brainnetome and ASEG JVM loaders.
- True NIfTI fixture coverage for loader `loadFromPaths()` paths.
- Optional live-download smoke checks kept out of the default suite.
- Optional TemplateFlow asset resolution and nonlinear warp execution.
- Surface atlas payloads once the volume API is stable.
- Visualization/reporting adapters outside the atlas core.

## Migration Phases

1. Foundation: keep `compileAll` and `testAll` green while adding typed module
   contracts. This is now started with `dataset`, `model`, `linalg`, and `fit`.
2. Dataset backends: port one simple in-memory/matrix backend, then NIfTI, then
   BIDS-H5/study backends. Each backend must satisfy the same `DatasetBackend`
   tests.
3. Model construction: replace R's `create_fmri_model()` with builders that
   convert dataset events into `EventModel` and default `BaselineModel` values.
4. Dense fitting: OLS and runwise OLS now live in `scalafim-fmri-fit` with
   deterministic fixtures and deliberately avoid Breeze so they can
   cross-compile.
5. Inference: coefficient standard errors, residual diagnostics, t contrasts,
   and F contrasts are wired through typed contrast descriptions; spatial
   thresholding belongs in the planned `scalafim-fmri-threshold` module.
6. Advanced engines: AR/GLS is started via shared whitening plans; robust
   fitting, soft projection, and low-rank paths remain behind typed engine
   interfaces.
7. IO/reporting: add BIDS export, reports, plotting data, and summaries as
   separate modules once the core statistics are stable.

## Acceptance Standard

A ported feature is accepted only when it has:

- a typed Scala API that can be explained without referring to S3 dispatch;
- JVM and Scala.js compilation unless it is explicitly platform-specific;
- direct tests of invariants and failure modes;
- parity fixtures for numerical behavior that must match the R implementation;
- no hidden dependency from core logic into plotting, reporting, or file IO.
