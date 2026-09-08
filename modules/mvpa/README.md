# scalafim-fmri-mvpa

ScalaFIM MVPA expresses predictive modelling and relational geometry through
one identified-evidence architecture. Import the public vocabulary with:

```scala
import scalafim.fmri.mvpa.*
```

The central request has four scientific parts:

```text
source + evidence design + measurement frame + estimand
```

`Mvpa.run` validates those identities, binds method-specific capabilities,
plans execution, visits every measurement, and returns one typed
`AnalysisResult[A, Rejection, Failure, Rendition]` with an
`ExecutionReceipt`.

## Data shapes and outputs

The predictive kernel starts from a sample-by-neural `EvidenceTable`, a target
`Column` on the exact sample axis, and a validation or cross-fit design. Its
first complete workflow returns exact source-ordered out-of-fold predictions:

```text
samples x neural coordinates
  + Column[samples, class]
  + ValidationDesign
  -> OutOfFoldClassification[samples]
```

The relational kernel starts from partitioned effect-by-neural relations and
an ordered pairing design. Closing the neural boundary yields an identified
RDM; comparing it with a pair-axis-bound model yields an RSA estimate:

```text
partitions x (effects x neural coordinates)
  + PairingDesign
  -> MeasuredRelationalRdm
  -> MeasuredRankRsa
```

Both kernels localize evidence with the same `MeasurementFrame`. A hard
region, global identity map, weighted region, fixed projection, and spatial
searchlight are all typed linear `Measurement`s. Spatial centers and labels
remain rendition metadata used only when rendering results.

## Construct evidence once

Neuroimaging inputs enter the same two source types directly:

- `DatasetEvidence.fromSeries`, `fromReader`, `fromDataset`, and `fromOpened`
  preserve selected timepoint and voxel order in authoritative sample and
  neural axes, then return `Observations`.
- `RunwiseObservations` composes fit-owned `TrialReadout` operators with
  runwise response blocks and stacks the resulting trial-by-neural evidence on
  a declared sample axis.
- `RunwiseRelations.estimateOnly` composes the same readout boundary into
  partitioned effect-by-neural relations for RDM, RSA, crossnobis, and related
  estimands.

These constructors do not create another sample table, feature-set hierarchy,
or analysis result shell. Targets and metadata are ordinary axis-bound
`Column` values; spatial localization is introduced later by the frame.

## Predictive workflow

Given an identified evidence table, categorical target, exact-once validation
design, and measurement frame:

```scala
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

val result =
  for
    source <- CategoricalObservationSource(patterns, target).left.map(_.message)
    configuration <- ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).left.map(_.message)
    analysis = source.classify(configuration)
    result <- Mvpa
      .run(source)(validation, frame, analysis, strategy)
      .left
      .map(_.message)
  yield result
```

Every successful measurement contains an `OutOfFoldClassification` with
typed predictions, ordered class scores, fold-linked preparation and
fit receipts, and derived metrics such as accuracy. Standardization and
learning occur inside Alder's training roles. ScalaFIM reconstructs every
output against authoritative `SampleId`s before it can enter the result.

## Relational workflow

`PartitionedRelations` binds each `PartitionId` to a relation estimate, exact
effect and neural axes, estimability receipt, preparation identity, and a
`RelationCapabilities[N, K]` value tied to that exact neural space. Residual
methods require `CertifiedResidualMoments[N, K]`; crossnobis additionally
requires certified noise precision on the same axis. Identity-precision
distance and noise-whitened crossnobis are distinct typed queries compiled by
one relational fit path:

```scala
import scalafim.fmri.mvpa.RelationalAnalysis.given

val result =
  Mvpa.run(relations)(
    pairing,
    frame,
    RelationalAnalysis.crossnobisRdm(
      relations,
      RdmNormalization.Raw
    ),
    strategy
  )
```

The relation compiler composes each measurement before forming local
sufficient statistics. `RelationalFit.query` is the sole public fit boundary;
the frame estimands above delegate to it. A compatible fit can answer
distance, RDM, and RSA views without reopening source operators. Signed
crossvalidated distances, including negative null estimates, are retained.

A model is bound to the exact canonical effect-pair axis:

```scala
val rsa =
  for
    pairDomain <- WithinPairDomain(relations.effects).left.map(_.message)
    modelName <- SecondOrderModelName("theory").left.map(_.message)
    model <- SecondOrderModel
      .signal(pairDomain, modelName, modelDistances)
      .left
      .map(_.message)
    result <- Mvpa
      .run(relations)(
        pairing,
        frame,
        RelationalAnalysis.rankRsa(
          relations,
          model,
          RdmNormalization.Raw
        ),
        strategy
      )
      .left
      .map(_.message)
  yield result
```

## Inspect before execution

The same inspection API describes either family without erasing its result or
error types:

```scala
val inspection: Either[
  ScientificSpecificationError,
  ScientificPlanInspection
] =
  Mvpa
    .specify(source)(design, frame, estimand)
    .map(specification => Mvpa.inspect(specification))
```

The inspection exposes exact source, design, frame, measurement, estimand,
normalization, and output-boundary identities. Backend, solver,
materialization, and scheduling choices belong to the execution plan and
receipt instead.

## Failure and execution policy

Each frame entry produces exactly one `MeasurementOutcome`: `Success`,
`Rejected`, or `Failed`. A local numerical or materialization failure does not
erase successful neighboring regions. Collected and streaming traversal use
the same measurement values and receipts.

`MvpaRunError.message` names the failing specification, binding, planning, or
execution stage. Measurement-local failures retain the measurement identity,
typed method error, and execution receipt; source axes expose their scientific
role and coordinate provenance, while relational sources expose the exact
capability identity. A downstream consumer can therefore render this context
without string dispatch or erased payloads. The portable consumer court's
`DownstreamFailureRenderer` is a complete example: it takes a typed failure
renderer plus the exact source, partition, and `MeasurementValue`, obtains the
capability from that source partition, and derives stage, axis, role,
capability, provenance, measurement, and execution-target context.

Dense materialization is never implicit. It must be admitted by an
`ExecutionStrategy` with a positive `MaterializationBudget`, and performed
work is recorded per measurement and in aggregate. Operator-native and
sufficient-statistic paths remain available when an estimand supports them.

## Verification

The focused local and CI gate checks oracle freshness, source formatting,
strict JVM and Scala.js tests, JMH compilation, optimized Scala.js linking, and
the executable downstream workflow project:

```sh
bash tools/ci/mvpa-gate.sh
```

For a narrower iteration, `sbt mvpaJVM/test` and `sbt mvpaJS/test` run the
portable test court directly.

The architectural laws and their executable evidence are indexed in
[`docs/architecture/mvpa-architecture-acceptance.md`](../../docs/architecture/mvpa-architecture-acceptance.md).
The atlas and workflow example projects compile complete frame construction,
predictive classification, and relational RDM consumers from public imports.
The portable `downstream.mvpa.PublicWorkflowSuite` runs the same two public
families on both the JVM and Scala.js and checks typed bind, capability, and
provenance failures from outside the implementation package.
