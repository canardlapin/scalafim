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

`DatasetObservationEvidence` constructs nominal sample and neural domains,
identified observations, selected feature mappings and sample metadata. It
supports synchronous `DatasetSeriesReader` input, a synchronous dataset bridge,
and explicit derived pattern rows. `OpenedDatasetMvpaExecutor.observations`
performs selected reads through the opened dataset's effectful boundary.

```scala
val request =
  DatasetPatternRequest(
    selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
    metadata = SampleMetadataRequest.labeled(
      labels = Vector("face", "scene", "face", "scene"),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
  )
val evidence = DatasetObservationEvidence.fromDataset(dataset, request)
```

Derived coefficients use `DatasetObservationEvidence.fromPatternRows`, with
explicit row names, ordered feature indices and optional labels, runs, blocks
and item names. Estimate origins remain distinct from timepoint origins.
`categoricalLabels` returns a column bound to the actual sample domain or a
typed missing-label refusal. Validation binds run or block columns through the
native design; the dataset adapter does not construct a second fold engine.
Regional and searchlight selection uses typed spatial measurement frames.

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
claim. Predictive engine/example deletion was completed in M1.12;
the caller map is in `docs/verification/umvpa-m1-10-caller-map-20261001.tsv`.

Run it directly with:

```sh
sbt mvpaDatasetJVM/test
sbt mvpaDatasetJS/test
```
