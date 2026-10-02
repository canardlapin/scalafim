# scalafim-fmri-mvpa-dataset

Typed dataset adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.dataset.*
```

Predictive workflows use identified `Observations`, `MultiResponse` and
`Column` metadata. `AlderPredictiveAdmission.nativeTables` admits the native
values with their source identities, axes, read policy and materialization
budget; `.nativeMeasurement` additionally binds a typed spatial/basis leg.
A `ValidationDesign` or `LeaveOneGroupOutDesign` binds assessment to the same
ordered sample axis. Native IDs remain unique, including repeated-draw
occurrences. Equal-sized foreign axes are refused.

The method-owned adapters are `AlderSwiftCentroid`, `AlderCorrelationCentroid`,
`AlderRidgeLda`, `AlderRidgeRegression`, `AlderFeatureModel`,
`AlderCrossDecoding`, `AlderOperatorRidge`, `AlderSoftLda`, and
`AlderPatternSelection`. Their results carry numerical outputs and receipts
directly. Dense categorical and feature-model heads accept repeated exact
validation; each categorical row records every contributing assessment unit
and its training keys, with equal weight per contribution.

See the executable
[atlas classification workflow](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala)
for native spatial admission and a bound validation plan. The same source and
suite run on JVM and Scala.js. R parity expectations live in
`NativePredictiveParitySuite`; numerical laws and refusal cases live in
`NativePredictiveLawsSuite` and the owning method suites.

The dataset views below remain migration boundaries for M3.13 and relational
callers scheduled for M2.09. They preserve selected rows, feature identity and
sample metadata; new predictive workflows use native admission.

For lower-level construction, pass a single typed request:

```scala
val request =
  DatasetPatternRequest(
    selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
    metadata = SampleMetadataRequest.labeled(
      labels = Vector("face", "scene", "face", "scene"),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
  )

val unlabeledOrLabeled =
  MvpaDatasetView.fromDataset(dataset, request)
```

Derived rows such as condition coefficients or LSS trial betas use the same
view surface:

```scala
val betaView =
  LabeledMvpaDatasetView
    .fromPatternRows(
      rows = betaRows,
      rowNames = trialNames,
      featureIndices = selectedVoxels,
      labels = trialLabels,
      blocks = Some(runLabels),
      featureSpaceId = FeatureSpaceId.unsafe("trial-betas")
    )
    .toOption
    .get
```

`voxelIndices` remain global feature ids. A `FeatureSetPlan` built from an
atlas or searchlight can therefore be used with either raw timepoint patterns or
derived beta rows as long as both refer to the same voxel id space.

This module has no JVM-only dependencies and is built for both JVM and
Scala.js. JVM-specific readers should stay in `dataset` or higher-level
workflow modules; they can produce `FmriSeries` values and then use this bridge.

Identified predictive entry points live in `dataset.predictive`:
`AlderCorrelationCentroid`, `AlderRidgeLda`, `AlderCrossDecoding`,
`AlderOperatorRidge`, `AlderFeatureModel`, and `AlderSoftLda`. Each completes
an actual Alder fit on explicitly selected training rows and returns its named
result with fit and validation receipts. Operator ridge and SoftLda retain
operator products; they never request a dense trial-by-feature table. SoftLda
uses the single-fit kernel in `mvpa-fit`, which is the additional module edge.

Feature-model designs require ordered item keys matching the sample axis and
explicit neural-column names. Both directions retain training-only sample-SD
standardization, coefficients, repeated exact assessment means, metrics and the
optional prediction payload. Cross-domain heads require separate sample domains
and an exact feature correspondence; native read receipts are checked against
that correspondence. A materialized feature axis and source-only preparation
remain caller declarations. Probability normalization supplies no calibration
claim. Legacy engine/example deletion is the separate M1.12 cutover packet;
the caller map is in `docs/verification/umvpa-m1-10-caller-map-20261001.tsv`.

Run it directly with:

```sh
sbt mvpaDatasetJVM/test
sbt mvpaDatasetJS/test
```
