# MVPA Engine Plan

This plan maps the useful computational structure in `~/code/rMVPA` into a
ScalaFIM module without porting the R package surface.

## What rMVPA contributes

The durable rMVPA shape is:

```text
dataset -> design/labels -> model spec -> regional/searchlight engine -> result
```

The parts worth preserving first are:

- a model-independent iterator over feature sets, used by both searchlights and
  labeled regions;
- a small per-ROI analysis contract, now expressed in rMVPA as `fit_roi.*`;
- blocked cross-validation as data, not hidden runner state;
- dense sample-by-feature matrices at the model boundary;
- scalar metric vectors plus optional richer result payloads;
- RDM and crossnobis kernels with explicit squared/normalized semantics.

The parts not worth carrying over directly are S3 dispatch, list-shaped model
specs, future/shard orchestration as a core concern, and R package model
registries. Those belong in adapters or higher-level runners later.

### Built-in classifier review

`corclass` is the cleanest lightweight classifier in rMVPA: it computes class
templates and predicts by row-wise correlation to those templates. The useful
core is only class means, row centering/scaling, a score matrix, and softmax
probabilities.

`swift` is not a general model-registry classifier. It is a fast multiclass
image-searchlight path for internally cross-validated analyses. Its core idea
is still useful: fold-local scaling, class means, linear template scores, and a
per-voxel evidence vector that can be summed over any ROI/searchlight. The
ScalaFIM core keeps the classifier version as `SwiftCentroidClassifier`; the
searchlight-wide evidence scanner can be added later as an optimizer over the
same classifier semantics.

`dual_lda` has two layers in rMVPA. The portable model is ridge LDA:

```text
Sigma = centered_residuals' centered_residuals + gamma I
score_k(x) = x' Sigma^-1 mu_k - 0.5 mu_k' Sigma^-1 mu_k + log prior_k
```

The rMVPA fast path rewrites the same discriminant in dual form and updates
neighboring searchlights by adding/removing boundary columns. That optimization
is searchlight-geometry-specific and should not be the base API. ScalaFIM keeps
the portable model as `RidgeLdaClassifier`; a dual/rank-update scanner can be a
later execution strategy.

## ScalaFIM boundary

The `mvpa` shared module should stay cross-platform and primitive-array based.
It should depend on `linalg` first. Spatial modules remain responsible for
constructing feature sets:

- `image.Searchlight` can provide spherical voxel feature sets.
- `atlas.VolumeAtlas` and surface parcel helpers can provide regional feature
  sets.
- `dataset` can adapt stored fMRI series into sample-by-feature matrices.

The MVPA core then owns:

- `Response`: categorical or continuous sample targets.
- `FoldPlan`: explicit train/test folds such as leave-one-run/block-out.
- `FeatureSet`: an ROI/searchlight/parcel as global feature indices.
- `FeatureSetPlan`: a named regional or searchlight feature-set stream.
- `PatternMatrix`: the canonical samples-by-features numerical view.
- `PatternSource`: a backend-neutral source that materializes one ROI/searchlight
  `PatternMatrix` at a time.
- `RoiAnalysis`: the per-ROI analysis contract.
- `Classifier`: a dependency-free, pluggable classifier contract with fitted
  models that return class probabilities.
- `MvpaTask`: the single-feature-set kernel that distributed runners can map
  over a `FeatureSetPlan`.
- `MvpaStream`: a non-collecting local runner that streams, visits, or folds
  `RoiOutcome`s over a `PatternSource` and `FeatureSetPlan`.
- `MvpaEngine`: deterministic local feature-set collection with typed per-ROI
  errors.
- `SearchlightClassifierScanner`: an optimizer over the classifier analysis
  contract. It returns the same `RoiOutcome`/`MvpaResult` surface as the
  reference engine, with direct SWIFT centroid and ridge LDA fast paths and
  fallback through `CrossValidatedClassifierAnalysis` for classifiers without a
  specialized scanner.
- `Rdm`: squared Euclidean, Euclidean, correlation, cross-validated second
  moments, and crossnobis distances.
- `PartitionMeansBuilder`: converts fold-wise categorical patterns into the
  primitive-array `PartitionMeans` layout used by crossnobis distances.
- `RdmAnalysis`/`RsaAnalysis`: first-class ROI analyses over sample rows or
  categorical class means, with labeled model RDM alignment and pluggable RDM
  scorers.
- `FeatureModelAnalysis`: a bidirectional feature encoding/decoding analysis
  derived from the useful core of rMVPA `feature_rsa_model`. It predicts either
  neural patterns from a fixed feature design or feature vectors from neural
  patterns, using a dependency-free standardized ridge estimator first.
- `scalafim-fmri-mvpa-dataset`: a thin adapter module that converts selected
  `FmriSeries`/`FmriDataset` reads plus row-aligned sample metadata into
  `PatternMatrix`/`PatternSource` values. Its `PatternTable` also accepts
  generic row-by-feature products such as trialwise beta maps or coefficient
  tables, without depending on the `fit` module. It preserves global voxel
  feature indices, records a typed `FeatureSpaceRef`, distinguishes acquisition
  timepoints from derived estimate rows via `SampleOrigin`, and derives
  categorical `Response` and leave-one-run/block fold plans from typed sample
  records.
- `scalafim-fmri-mvpa-spatial`: a thin adapter module that converts volume
  labels, `VolumeAtlas` payloads, image ROI/searchlight windows, mask
  searchlights, and surface parcels into `FeatureSetPlan`s without making the
  portable MVPA core depend on spatial libraries.
- `MvpaParityFixtures`/`MvpaParitySuite`: shared-test fixtures that keep RDM,
  crossnobis, RSA, and classifier estimands explicit across JVM and Scala.js.

## First implementation slice

The first scaffold intentionally avoids classifiers. It establishes the engine
and kernels that future analyses will sit on:

1. Validate labels, folds, feature sets, and matrix shape.
2. Run a `RoiAnalysis` over a sequence of feature sets.
3. Report per-ROI failures without failing the whole run.
4. Add RDM and crossnobis primitives with tests anchored to hand-computable
   values.
5. Add lightweight classifier baselines:
   - correlation centroid, matching the useful part of rMVPA `corclass`;
   - SWIFT-style linear centroid scoring over fold-local scaled features;
   - ridge LDA, matching the core `dual_lda` discriminant without searchlight
     rank-update machinery.

## Classifier correctness contract

The first classifier layer should remain small but strict:

- classifier fits and predictions reject non-finite matrices through
  `Either[MvpaError, ...]`;
- cross-validation validates that the fold plan and data have the same sample
  count when called directly;
- fold aggregation aligns probability columns by class label, so pluggable
  classifiers are not required to preserve the response's first-seen class
  order internally;
- built-in classifiers return finite, normalized class probabilities, including
  zero-variance-feature cases;
- ridge LDA is anchored by an analytic one-dimensional probability oracle, and
  correlation centroid is covered by a row-offset invariance test.

## Regional/searchlight and RSA infrastructure

The engine now treats "regional" and "searchlight" as feature-set plan kinds,
not as separate runner APIs. This keeps the computational core small:

- atlas and surface code can build regional `FeatureSetPlan`s from parcel/label
  indices;
- image and surface searchlight code can build searchlight `FeatureSetPlan`s
  from precomputed neighborhoods;
- `MvpaEngine.run` accepts either a raw sequence of feature sets or a named
  `FeatureSetPlan`;
- `RoiAnalysisResult` carries scalar metrics plus an optional typed payload for
  heavier outputs such as predictions, observed RDMs, or RSA scores.

The distributed execution boundary is deliberately one layer lower than
`MvpaEngine`: a worker needs a `PatternSource`, one `FeatureSet`, the validated
response/fold plan, and an immutable `RoiAnalysis`. `MvpaStream` is the
non-collecting local runner over that same kernel, and `MvpaEngine` is just the
reference collector:

```text
FeatureSetPlan -> partition feature sets -> MvpaTask.evaluate -> RoiOutcome
```

Future Spark or cluster runners should live outside `mvpa` and replace the
collection strategy, not the analysis contracts.

Streaming validates response and fold-plan shape before visiting any feature
set. Per-ROI feature-selection and analysis failures are emitted as
`RoiOutcome.Failure` values and do not stop the stream unless the visitor
returns `MvpaStreamControl.Stop`.

RSA is similarly split into small parts:

- `RdmRows` chooses whether the observed RDM is over samples or categorical
  class means;
- `RdmMethod` keeps squared Euclidean, normalized squared Euclidean, Euclidean,
  and correlation semantics explicit;
- `RdmModel` stores item labels with the model RDM, and `RsaAnalysis` aligns it
  to the observed class/sample order before scoring;
- `RdmScorer` is a pluggable contract, with dependency-free Pearson, Spearman,
  and partial Pearson scorers. Partial controls are labeled `RdmModel`s and are
  aligned at scoring time rather than pre-aligned by callers.
- `CrossnobisAnalysis` exposes crossvalidated class distances as the same
  `RoiAnalysis` shape used by regional and searchlight RSA; normalization by
  feature count is explicit.

Parity and benchmark fixtures are intentionally kept in shared tests first.
They pin the exact estimand before any external R/Python fixture generation or
timing comparison. The lightweight `MvpaBenchmarkHarness` uses explicit
checksums and an injected clock so deterministic JVM/JS tests can cover the
benchmark workload shape without depending on wall-clock timings.

Feature-based prediction is not modeled as "RSA" in ScalaFIM. The R
`feature_rsa_model` combines a fixed trial feature matrix with cross-validated
prediction of held-out neural patterns, then reports pattern- and RDM-level
metrics. ScalaFIM keeps that core and makes it bidirectional:

- `FeaturesToPatterns` is an encoding model: feature rows predict ROI/searchlight
  pattern rows.
- `PatternsToFeatures` is a decoding model: ROI/searchlight pattern rows predict
  feature rows.
- `FeatureRidgeEstimator` standardizes source and target columns on each
  training fold, solves a multivariate ridge system, and unstandardizes
  predictions. PLS/PCR/glmnet-style estimators can be added behind the same
  analysis contract later if they justify their dependencies.

Spatial feature-set adapters live in `mvpa-spatial`, not `mvpa`. That module is
the intended bridge from atlas/image/surface objects into the distributed-ready
MVPA execution contract. The adapter produces plain `FeatureSetPlan` values, so
regional and searchlight analyses remain map-friendly: a local runner,
streaming runner, or future Spark runner can distribute `MvpaTask.evaluate`
over the same feature-set stream.

Searchlight classifier optimization is explicitly layered over the same result
surface. `SearchlightClassifierScanner` avoids the generic per-ROI analysis
dispatch for SWIFT centroid and ridge LDA scans by validating response/folds
once, reusing the full-matrix feature lookup, and computing fold-local
scaling/centroids or ridge discriminants directly over each feature set. The
scanner allocates one probability buffer per feature set plus fold-local work
arrays and has reference-equivalence tests against `MvpaEngine` stored
prediction probabilities.

## Next slices

1. Add thin fit-specific convenience wrappers over `PatternTable` once the
   `fit`/`mvpa-dataset` build edge is available, so dense and LSS fit results
   can expose trialwise MVPA samples without hand-written matrix plumbing.
2. Add Kendall and robust-rank RSA variants if they are needed for R parity or
   specific datasets.
3. Add R/Python fixture-generation scripts that emit the table described in
   `tools/r-parity/mvpa-fixtures.md`.
4. Add rank-update reuse for ridge-LDA neighborhood sweeps if profiling shows
   the direct scanner is not enough. Keep it as an optimizer over the classifier
   contract rather than a separate model API.
