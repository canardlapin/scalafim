package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity
import scalafim.fmri.fit.ResponseBlock
import scalafim.fmri.fit.TrialEstimability
import scalafim.fmri.fit.TrialReadout
import scalafim.fmri.fit.TrialReadoutReceipt

/** One response-independent trial readout bound to exact sample and neural axes. The row mapping is the order of
  * `samples`; coefficient names remain in the receipt and need not masquerade as globally unique sample keys.
  */
final class RunwiseObservationInput[
    N <: SemanticSpace,
    NeuralKey
] private (
    val partition: PartitionId,
    val response: ResponseBlock,
    val readout: TrialReadout,
    val samples: AxisRef[SampleId],
    val neural: AxisRef.Aux[NeuralKey, N],
    val responseRevision: ValueIdentity,
    val estimateValueId: ValueId
)

object RunwiseObservationInput:
  def apply[N <: SemanticSpace, K](
      partition: PartitionId,
      response: ResponseBlock,
      readout: TrialReadout,
      samples: AxisRef[SampleId],
      neural: AxisRef.Aux[K, N],
      responseRevision: ValueIdentity,
      estimateValueId: ValueId
  ): Either[FmriEvidenceError, RunwiseObservationInput[N, K]] =
    if response.timepoints != readout.timepoints then
      Left(
        FmriEvidenceError.TimepointMismatch(
          partition,
          readout.timepoints,
          response.timepoints
        )
      )
    else if response.voxels != neural.size then
      Left(
        FmriEvidenceError.NeuralCountMismatch(
          partition,
          neural.size,
          response.voxels
        )
      )
    else if samples.size != readout.trials then
      Left(
        FmriEvidenceError.SampleCountMismatch(
          partition,
          readout.trials,
          samples.size
        )
      )
    else if samples.identity.purpose != AxisPurpose.Samples then
      Left(
        FmriEvidenceError.Observations(
          ObservationsError.InvalidSamplePurpose(samples.identity.purpose)
        )
      )
    else if neural.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(
        FmriEvidenceError.Observations(
          ObservationsError.InvalidNeuralPurpose(neural.identity.purpose)
        )
      )
    else
      Right(
        new RunwiseObservationInput(
          partition,
          response,
          readout,
          samples,
          neural,
          responseRevision,
          estimateValueId
        )
      )

final case class RunwiseObservationRunReceipt(
    partition: PartitionId,
    samples: AxisIdentity,
    neural: AxisIdentity,
    coefficientNames: Vector[String],
    estimability: Vector[Boolean],
    responseRevision: ValueIdentity,
    readout: TrialReadoutReceipt,
    estimateValue: ValueIdentity
)

final case class RunwiseObservationReceipt(
    partitions: AxisIdentity,
    samples: AxisIdentity,
    neural: AxisIdentity,
    runs: Vector[RunwiseObservationRunReceipt],
    source: ScientificSourceIdentity
)

/** Predictive evidence produced directly from runwise trial readouts. */
final class RunwiseObservationEvidence[
    S <: SemanticSpace,
    N <: SemanticSpace,
    NeuralKey
] private[mvpa] (
    val observations: Observations[S, N, NeuralKey],
    val estimability: Column[S, Boolean],
    private val samplePartitions: Map[SampleId, PartitionId],
    val receipt: RunwiseObservationReceipt
):
  def partitionOf(sample: SampleId): Option[PartitionId] =
    samplePartitions.get(sample)

object RunwiseObservations:
  def apply[
      P <: SemanticSpace,
      S <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      partitions: PartitionAxis[P],
      samples: AxisRef.Aux[SampleId, S],
      neural: AxisRef.Aux[K, N],
      runs: Seq[RunwiseObservationInput[N, K]],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural")
  ): Either[FmriEvidenceError, RunwiseObservationEvidence[S, N, K]] =
    val input = runs.toVector
    if input.isEmpty then Left(FmriEvidenceError.EmptyRuns)
    else if input.length != partitions.axis.size then
      Left(
        FmriEvidenceError.PartitionCountMismatch(
          partitions.axis.size,
          input.length
        )
      )
    else
      val byPartition = scala.collection.mutable.HashMap.empty[
        PartitionId,
        RunwiseObservationInput[N, K]
      ]
      var position = 0
      while position < input.length do
        val run = input(position)
        if partitions.axis.positionOf(run.partition).isEmpty then
          return Left(FmriEvidenceError.UnknownPartition(run.partition))
        if byPartition.contains(run.partition) then return Left(FmriEvidenceError.DuplicatePartition(run.partition))
        if run.neural.identity != neural.identity then
          return Left(
            FmriEvidenceError.NeuralAxisMismatch(
              run.partition,
              neural.identity.fingerprint,
              run.neural.identity.fingerprint
            )
          )
        if !(run.neural.evidence eq neural.evidence) then
          return Left(FmriEvidenceError.NeuralWitnessMismatch(run.partition))
        byPartition += run.partition -> run
        position += 1

      val orderedBuilder = Vector.newBuilder[RunwiseObservationInput[N, K]]
      position = 0
      while position < partitions.axis.size do
        val partition = partitions.axis.keys(position)
        byPartition.get(partition) match
          case None      => return Left(FmriEvidenceError.UnknownPartition(partition))
          case Some(run) => orderedBuilder += run
        position += 1
      val ordered = orderedBuilder.result()
      val sampleOwners = scala.collection.mutable.HashMap.empty[SampleId, PartitionId]
      val parts = Vector.newBuilder[EvidenceTable[?, ?, SampleId, K]]
      val estimability = Vector.newBuilder[Boolean]
      val receipts = Vector.newBuilder[RunwiseObservationRunReceipt]
      position = 0
      while position < ordered.length do
        val run = ordered(position)
        var samplePosition = 0
        while samplePosition < run.samples.size do
          val sample = run.samples.keys(samplePosition)
          sampleOwners.get(sample) match
            case Some(_) => return Left(FmriEvidenceError.DuplicateSample(sample))
            case None    => sampleOwners += sample -> run.partition
          samplePosition += 1

        val composed = run.readout.operator.compose(run.response.value) match
          case Left(error) =>
            return Left(
              FmriEvidenceError.CompositionFailure(
                run.partition,
                error.getMessage
              )
            )
          case Right(value) => value
        val evidence = EvidenceTable
          .operator(
            run.samples,
            neural,
            composed,
            run.estimateValueId
          )
          .left
          .map(FmriEvidenceError.Evidence.apply) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        parts += evidence

        val flags = run.readout.axis.estimability.map:
          case TrialEstimability.Estimable     => true
          case TrialEstimability.ZeroRegressor => false
        estimability ++= flags
        receipts += RunwiseObservationRunReceipt(
          run.partition,
          run.samples.identity,
          neural.identity,
          run.readout.trialNames,
          flags,
          run.responseRevision,
          run.readout.receipt,
          evidence.table.valueIdentity
        )
        position += 1

      for
        evidence <- EvidenceTable
          .stackRows(samples, neural, parts.result())
          .left
          .map(FmriEvidenceError.Evidence.apply)
        observations <- Observations(
          evidence,
          sampleAxisName,
          neuralAxisName
        ).left.map(FmriEvidenceError.Observations.apply)
        flags <- Column(samples, estimability.result()).left.map(FmriEvidenceError.Column.apply)
      yield new RunwiseObservationEvidence(
        observations,
        flags,
        sampleOwners.toMap,
        RunwiseObservationReceipt(
          partitions.axis.identity,
          samples.identity,
          neural.identity,
          receipts.result(),
          observations.identity
        )
      )
