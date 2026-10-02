# scalafim-fmri-mvpa

Portable MVPA engine primitives for ScalaFIM.

Package root:

```scala
import scalafim.fmri.mvpa.*
```

This module provides identified observation/response axes, measurement legs,
relational estimands, structured pattern fits and portable numerical kernels.
Predictive validation runs through the [native Alder adapters](../mvpa-dataset/README.md).

Dense categorical kernels fit `DMat` with `Vector[ClassLabel]` and return their
concrete model. `model.predict(test)` returns validated `CategoricalProbabilities`;
class columns follow training first-occurrence order, and labels are derived
from probabilities. Swift uses training-only sample-SD scaling; correlation
requires at least two features. Operator ridge and soft LDA retain single-fit
kernels and operator application/convergence receipts.

Predictive orchestration has moved to `AlderSwiftCentroid`,
`AlderCorrelationCentroid`, `AlderRidgeLda`, `AlderFeatureModel`,
`AlderCrossDecoding`, `AlderOperatorRidge` and `AlderSoftLda`.
Bind `ValidationDesign` to the actual sample axis and admit observations with
`AlderPredictiveAdmission.nativeTables` or `.nativeMeasurement` before fitting.
Regional/searchlight selection uses a typed `MeasurementFrame` rather than
an analysis enum or universal payload. See the executable
[atlas workflow](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala),
tested on JVM and Scala.js.

`PatternMatrix`, `PatternOperator`, `PatternSource`, `Response`, `FoldPlan`,
`RoiAnalysis`, `RoiPayload`, `MvpaTask`, `MvpaStream`, and `MvpaEngine` remain
only for inventoried relational consumers (M2.09) and structured/one-shot
consumers (M3.13). These definitions are temporary migration boundaries;
new predictive workflows use the identified APIs above.

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

Operator-backed data also has an exact crossvalidated RSA path. It accumulates
the condition Gram fold by fold through adjoint operator products and never
requests the sample-by-feature table. `OperatorCrossnobisAnalysis` emits the
ordinary labeled RDM payload; `OperatorCrossnobisRsaAnalysis` feeds that same
RDM to the existing Pearson, Spearman, or partial-Pearson model scorers:

```scala
val rsa =
  OperatorCrossnobisRsaAnalysis(
    models = Vector(targetModel),
    scorer = RdmScorer.PartialPearson.unsafe(Vector(controlModel)),
    storeObservedRdm = true
  )

val result =
  MvpaEngine.runSource(operatorSource, plan, labels, rsa, Some(folds))
```

The computation retains signed crossnobis distances, including negative null
estimates. Its metrics expose a zero trial-pattern materialization count and a
fold-independent upper bound on working storage owned by the reduction.

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

Feature encoding and decoding use `AlderFeatureModel` with an explicit
`FeatureModelDesign`, `FeaturePredictionDirection` and positive `RidgePenalty`.
Its result exposes `FeatureModelMetricSet`, the penalty and an optional
`FeatureModelPrediction` directly. Standardization fits on training rows only;
repeated exact validation averages predictions per identified item.

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
