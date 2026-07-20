# scalafim-fmri-mvpa

Portable MVPA engine primitives for ScalaFIM.

Package root:

```scala
import scalafim.fmri.mvpa.*
```

This module is the ScalaFIM landing zone for the computational core of
`rMVPA`: typed sample responses, fold plans, feature-set/ROI execution,
feature-set plans, dependency-free classifiers, metric vectors,
representational-distance kernels, and first-class ROI RDM/RSA analyses. It
deliberately does not port the R S3 model registry or runner surface. Spatial
modules provide feature index sets, dataset/backends provide sample-by-feature
matrices, and `mvpa` owns the small analysis contract that runs over those
matrices.

`PatternOperator` is the linear representation of a sample-by-feature table.
Its orientation is features to sample scores, it requires both forward and
transpose products, and it supports checked row/feature restriction and row
stacking without first producing a dense `PatternMatrix`. `PatternSource[P]`
and `RoiAnalysis[P]` make the ordinary `MvpaTask`, `MvpaStream`, and
`MvpaEngine` boundaries representation-polymorphic: dense analyses use the
`DensePatternSource`/`DenseRoiAnalysis` aliases, while operator-native analyses
use `OperatorPatternSource`/`OperatorRoiAnalysis`. The representation types must
agree at compile time. `RoiAnalysis.materializing` is the explicit dense parity
adapter; it is an execution choice, not a second MVPA hierarchy.

Operator-native ridge classification is the first analysis that consumes this
boundary directly. It centers patterns and class targets within each training
fold, solves the augmented ridge system with Gale LSQR, and applies an
unpenalized intercept. `ClassMembership` represents either hard one-hot targets
or validated simplex-valued targets. Ridge predictions expose class scores—not
probabilities—and the payload records per-class convergence, normal residuals,
regularization, solver limits, operator provenance, and operator-application
counts.

```scala
val ridgeConfig =
  OperatorRidgeConfig(penalty = 0.5, tolerance = 1e-10).toOption.get

val ridge =
  CrossValidatedOperatorRidgeAnalysis(
    ridgeConfig,
    storePredictions = true
  )

val result =
  MvpaEngine.runSource(operatorSource, plan, response, ridge, Some(folds))
```

Soft targets use the same analysis through a typed response:

```scala
val membership =
  ClassMembership.simplex(classes, sampleByClassMemberships).toOption.get

val response = Response.Probabilistic(membership)
```

Regional and searchlight analyses use the same engine:

```scala
val plan =
  FeatureSetPlan.fromLabelVector("regions", Vector(0, 1, 1, 2, 2)).toOption.get

val analysis =
  CrossValidatedClassifierAnalysis(SwiftCentroidClassifier())

val result =
  MvpaEngine.run(patterns, plan, labels, analysis, folds = Some(folds))
```

Streaming runners can visit outcomes without collecting every ROI/searchlight:

```scala
MvpaStream.foreach(source, plan, labels, analysis, Some(folds)) { outcome =>
  MvpaStreamControl.Continue
}
```

Distributed runners should target the lower-level single-ROI boundary instead
of depending on the local collector:

```scala
val source = PatternSource.fromMatrix(patterns)
val outcome =
  MvpaTask.evaluate(source, plan.featureSets.head, labels, analysis, Some(folds))
```

A future Spark adapter should provide a JVM `PatternSource[P]` backed by its data
layout and map `MvpaTask.evaluate` over partitions of `FeatureSetPlan`; a local
non-collecting runner can use `MvpaStream`.

For high-volume searchlight classification, `SearchlightClassifierScanner`
keeps the same `RoiOutcome`/`MvpaResult` surface but adds direct SWIFT centroid
and ridge LDA fast paths:

```scala
val scanner =
  SearchlightClassifierScanner(
    SwiftCentroidClassifier(FeatureScaling.unsafeDiagonalShrinkage(0.2)),
    storePredictions = true
  )

val result =
  scanner.run(patterns, searchlightPlan, labels, folds)
```

The fast path validates response/folds once, reuses the global feature lookup,
and computes fold-local scaling, centroids, priors, and probabilities directly
over the selected columns. It allocates one output probability buffer per
feature set plus fold-local work arrays. Classifiers without a specialized path
fall back to `CrossValidatedClassifierAnalysis`.

Cross-domain decoding uses a typed `CrossDomainDataset` over source/target
pattern sources. Standard feature-set plans are lifted to same-source/target
`PairedFeatureSet`s, and lower-level runners can pass explicit paired feature
sets when the source and target domains use different feature ids. The naive
rMVPA-style baseline is expressed as a classifier analysis rather than a
special runner: fit source-domain prototypes with `CorrelationCentroidClassifier`,
predict target-domain patterns, and report accuracy through the usual ROI result
surface.

```scala
val design =
  CrossDecodingDesign.unsafe(
    sourceLabels = Vector("face", "face", "house", "house"),
    targetLabels = Vector("face", "house")
  )

val xdec =
  CrossDecoding.naive(storePredictions = true)

val result =
  CrossDomainMvpaEngine.run(sourcePatterns, targetPatterns, plan, design, xdec)
```

The lower-level `CrossDomainMvpaTask.evaluate` boundary evaluates one
`PairedFeatureSet` against a `CrossDomainDataset`; distributed runners can map
that single-ROI task across regional/searchlight feature sets.

For high-volume naive cross-decoding searchlights, `NaiveCrossDecodingScanner`
keeps the same result surface while computing source prototypes and target
correlation scores directly over selected feature columns:

```scala
val fast =
  NaiveCrossDecodingScanner(storePredictions = false)

val result =
  fast.run(sourcePatterns, targetPatterns, searchlightPlan, design)
```

RSA is a `RoiAnalysis` too. Observed RDM rows can be sample rows or categorical
class means, and model RDMs carry item labels so scoring aligns by label:

```scala
val model =
  RdmModel.unsafe("geometry", Vector("face", "house", "tool"), modelRdm)

val rsa =
  RsaAnalysis(RdmMethod.SquaredEuclidean(), Vector(model), rows = RdmRows.ClassMeans)
```

RDM scoring is pluggable. Pearson and Spearman are dependency-free, and partial
Pearson accepts labeled control RDMs so nuisance models are aligned by item
label before residualization:

```scala
val partial =
  RdmScorer.PartialPearson.unsafe(Vector(controlModel))

val rsa =
  RsaAnalysis(
    RdmMethod.SquaredEuclidean(),
    Vector(targetModel),
    scorer = partial
  )
```

Samplewise RSA is the ScalaFIM equivalent of rMVPA's `vector_rsa_model`. It
keeps the reference RDM item labels unique, then maps repeated sample rows onto
those items and block labels. For each sample, it correlates the neural
distance row against the reference distance row after excluding same-block
samples, then reports the mean defined score for the ROI/searchlight:

```scala
val model =
  RdmModel.unsafe("identity", Vector("face", "house"), referenceRdm)

val design =
  SamplewiseRsaDesign.unsafe(
    model,
    sampleItems = Vector("face", "house", "face", "house"),
    blocks = Vector("run1", "run1", "run2", "run2")
  )

val samplewise =
  SamplewiseRsaAnalysis(
    design,
    method = RdmMethod.Euclidean,
    scorer = RowSimilarity.Pearson,
    storeScores = true
  )
```

The ROI metric is `SamplewiseRsa`; optional `RoiPayload.SamplewiseRsa` stores
one typed score per sample for trial-level modeling downstream.

Crossvalidated distances use the same ROI engine. `CrossnobisAnalysis` builds
fold-wise class means internally and keeps feature normalization explicit:

```scala
val crossnobis =
  CrossnobisAnalysis(normalizeByFeatures = true, storeRdm = true)

val distances =
  MvpaEngine.run(patterns, plan, labels, crossnobis, folds = Some(folds))
```

Feature encoding and decoding use the same ROI contract. This is the core idea
behind rMVPA's `feature_rsa_model`, but expressed as a bidirectional
cross-validated prediction model rather than an RSA-named wrapper:

```scala
val design =
  FeatureModelDesign.unsafe(items, featureMatrix, Vector("semantic", "visual"))

val encode =
  FeatureModelAnalysis(
    design,
    FeaturePredictionDirection.FeaturesToPatterns,
    estimator = FeatureRidgeEstimator(lambda = 1e-2)
  )

val decode =
  FeatureModelAnalysis(design, FeaturePredictionDirection.PatternsToFeatures)
```

The first supported estimator is standardized multivariate ridge regression. It
has no heavy dependencies, works in both directions, and reports pattern, RDM,
correlation, MSE, and R-squared metrics.

Parity fixtures and lightweight benchmark checks live in the shared test tree:

```text
MvpaParityFixtures
MvpaParitySuite
```

Those fixtures keep squared, unsquared, and feature-normalized estimands
separate. See `tools/r-parity/mvpa-fixtures.md` before adding an R or Python
reference comparison.

Run it directly with:

```sh
sbt mvpaJVM/test
sbt mvpaJS/test
```
