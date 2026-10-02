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

Identified relational workflows return `RelationSet`, `RelationRdm`, or
method-owned comparison results directly. `ObservationMeanPlan` binds condition
and partition columns to sample axes. `ObservationMeanRelations.withSource`
composes sparse condition averages with an acquired observation operator;
missing condition cells retain `NotEstimable`. Scoped forward/adjoint replay
expires when the callback closes. `fromOwnedDense` explicitly copies a bounded
dense source and provides owned replay; storage representation alone never
supplies replay permission.

`RelationRdm.identity` computes signed squared Euclidean cross-products over
an explicit pairing, optionally normalized by neural feature count. Negative
values are retained. Grouping does not establish independent endpoint errors:
shared or unknown acquisition/preparation support remains descriptive.
Residual precision has a separate admission and provenance contract.

For ordinary within-partition squared Euclidean, Euclidean or correlation
geometry, use `OrdinaryRelationGeometry.compute` with a typed partition and
`RelationConsumerBudget`. It materializes only the bounded effect-by-neural
mean table and returns a labeled ordinary RDM. Its `score` aligns `RdmModel`
labels and supports `RdmScorer.Pearson`, `.Spearman` and labeled partial controls.
These ordinary distances are distinct from signed crossvalidated products.

`RelationConsumers.rsaDirect` evaluates inside a live acquired scope and returns
detached scores. `.cache` and `.rsa` reuse owned geometry under exact query
identities; preparation/noise changes require refitting. First-order contrasts
and rectangular forms preserve nominal endpoint axes. Typed samplewise RSA
uses `SamplewiseGeometry` plus sample-bound item and block columns, supports
repeated items, excludes same-block comparisons and reports undefined rows
explicitly. Signed geometry cannot be admitted as ordinary sample dissimilarity.

The old relational analyses, partition builder and universal relational payload
cases were removed in M2.09. `PatternMatrix`, `PatternOperator`, `PatternSource`,
`Response`, `FoldPlan` and generic engine/result definitions remain only for
named canonical/one-shot consumers until M3.13. New workflows use identified
observations and method-owned results.

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
