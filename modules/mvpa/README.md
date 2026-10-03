# scalafim-fmri-mvpa

Identified MVPA evidence, method-owned artifacts and portable numerical kernels for ScalaFIM.

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

Scientific workflows use nominal `AxisRef` domains and identified observations,
with explicit validation, measurement and acquisition scopes. The universal
engine, response/fold hierarchy, feature-set registry and payload collector have
been removed. Canonical consumers use the native global artifacts.

`PatternMatrix` and `PatternOperator` remain numerical storage adapters. Their
integer row/column indices describe storage, and supply no scientific domain,
independence or replay permission. Select columns with ordered `FeatureIndex`
values. Explicit operator materialization requires `PatternCopyBudget`; admission
checks the returned cell count and primitive capacity before reading the source.
The copy evaluates one feature column at a time. Its ceiling excludes source
storage, provider workspace and process RSS.

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

## Global decompositions

`GlobalDecompositions.pca` consumes an already live `PreparedObservationProduct`
with explicit preprocessing and Euclidean sample/neural geometry. Paired `plsc`
and `cca` require both marginal row geometries, their cross-row relationship,
scaling, access permission and a materialization ceiling. Their artifacts retain
actual nominal endpoints, inspectable geometry/provenance receipts, declared
evidence identity and the actual owned cross-form content identity. CCA also
retains its declared regularization.

Neutral `svd` accepts a relation, effect form or neural form. It preserves signs
and offsets, and reports singular values and owned factors without adding PSD,
covariance or signed-eigenvalue guarantees. All outputs are descriptive global
artifacts. Returned fits own their numeric products; scoped input operators can
expire without invalidating fitted cross forms.

The materialization ceiling covers the declared input/form snapshots and
retained factors, not solver-private workspace or process RSS. These are dense
adapters; operator representation alone is not evidence of whole-brain compute
readiness. Kernels come from pinned Multivar/Gale.

Current migration evidence: [M3.01 native global/canonical verification](../../docs/verification/umvpa-global-canonical-20261001.md).

### Pattern interpretation

`PatternInterpretation.calibrated(heads)` returns the structured forward
loadings and the distinct calibrated filters `W = Psi^-1 A G^-1`; its named
scores are unshrunk component estimates. A singular or relatively deficient
Gram matrix returns a typed factorization refusal with the requested tolerance.
No jitter or pseudoinverse is added.

`empiricalCalibrated`, `empiricalRaw`, `empiricalPosterior`, and
`empiricalTargets` estimate `Cov(x, score) Cov(score)^-1` on a declared
independent diagnostic population. They require actual sample reindexing legs
and reject training, selection, or tuning overlap before scoring. Diagnostics
retain the score coordinates, population/source/content identity and covariance
denominator. They may be dense or differ from the forward loadings; they are
not significance maps. Singular empirical score covariance is an explicit
refusal. Dense ceilings cover adapter arrays; process and upstream numerical
workspace are outside those ceilings.
