package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity
import scalafim.fmri.fit.ResponseBlock
import scalafim.fmri.fit.TrialEstimability
import scalafim.fmri.fit.TrialReadout

/** One keyed, response-independent readout and the exact response revision to which it will be applied. Construction
  * performs no readout application.
  */
final class RunwiseRelationInput private (
    val partition: PartitionId,
    val response: ResponseBlock,
    val readout: TrialReadout,
    val trainingSamples: AxisRef[SampleId],
    val responseRevision: ValueIdentity,
    val estimateValueId: ValueId
)

object RunwiseRelationInput:
  def apply(
      partition: PartitionId,
      response: ResponseBlock,
      readout: TrialReadout,
      trainingSamples: AxisRef[SampleId],
      responseRevision: ValueIdentity,
      estimateValueId: ValueId
  ): Either[FmriEvidenceError, RunwiseRelationInput] =
    if response.timepoints != readout.timepoints then
      Left(
        FmriEvidenceError.TimepointMismatch(
          partition,
          readout.timepoints,
          response.timepoints
        )
      )
    else if trainingSamples.size != readout.timepoints then
      Left(
        FmriEvidenceError.SampleCountMismatch(
          partition,
          readout.timepoints,
          trainingSamples.size
        )
      )
    else if trainingSamples.identity.purpose != AxisPurpose.Samples then
      Left(
        FmriEvidenceError.Observations(
          ObservationsError.InvalidSamplePurpose(trainingSamples.identity.purpose)
        )
      )
    else
      Right(
        new RunwiseRelationInput(
          partition,
          response,
          readout,
          trainingSamples,
          responseRevision,
          estimateValueId
        )
      )

object RunwiseRelations:
  def estimateOnly[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      runs: Seq[RunwiseRelationInput],
      design: DesignIdentity,
      preparation: NormalizationIdentity = NormalizationIdentity.none,
      effectAxisName: ScientificAxisName = ScientificAxisName.unsafe("effects"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural")
  )(using
      codec: AxisKeyCodec[EK]
  ): Either[
    FmriEvidenceError,
    PartitionedRelations[
      P,
      E,
      N,
      EK,
      NK,
      EstimateOnlyCapabilities[N, NK]
    ]
  ] =
    val input = runs.toVector
    if input.isEmpty then Left(FmriEvidenceError.EmptyRuns)
    else
      val seen = scala.collection.mutable.HashSet.empty[PartitionId]
      val entries = Vector.newBuilder[
        PartitionRelation[E, N, EK, NK, EstimateOnlyCapabilities[N, NK]]
      ]
      var position = 0
      while position < input.length do
        val run = input(position)
        if seen.contains(run.partition) then return Left(FmriEvidenceError.DuplicatePartition(run.partition))
        if partitions.axis.positionOf(run.partition).isEmpty then
          return Left(FmriEvidenceError.UnknownPartition(run.partition))
        buildRelation(run, effects, neural, design, preparation) match
          case Left(error)     => return Left(error)
          case Right(relation) =>
            entries += PartitionRelation(run.partition, relation)
            seen += run.partition
        position += 1
      PartitionedRelations(
        partitions,
        effects,
        neural,
        entries.result(),
        effectAxisName,
        neuralAxisName
      ).left.map(FmriEvidenceError.Relation.apply)

  private def buildRelation[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      run: RunwiseRelationInput,
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      design: DesignIdentity,
      preparation: NormalizationIdentity
  )(using
      codec: AxisKeyCodec[EK]
  ): Either[
    FmriEvidenceError,
    Relation[E, N, EK, NK, EstimateOnlyCapabilities[N, NK]]
  ] =
    if run.response.timepoints != run.readout.timepoints then
      Left(
        FmriEvidenceError.TimepointMismatch(
          run.partition,
          run.readout.timepoints,
          run.response.timepoints
        )
      )
    else if run.response.voxels != neural.size then
      Left(
        FmriEvidenceError.NeuralCountMismatch(
          run.partition,
          neural.size,
          run.response.voxels
        )
      )
    else if run.readout.trials != effects.size then
      Left(
        FmriEvidenceError.ReadoutEffectCountMismatch(
          run.partition,
          effects.size,
          run.readout.trials
        )
      )
    else if run.trainingSamples.size != run.readout.timepoints then
      Left(
        FmriEvidenceError.SampleCountMismatch(
          run.partition,
          run.readout.timepoints,
          run.trainingSamples.size
        )
      )
    else
      var effect = 0
      while effect < effects.size do
        val expected = codec.encode(effects.keys(effect)).value
        val actual = run.readout.axis.ids(effect).value
        if expected != actual then
          return Left(
            FmriEvidenceError.EffectKeyMismatch(
              run.partition,
              effect,
              expected,
              actual
            )
          )
        effect += 1

      for
        composed <- run.readout.operator
          .compose(run.response.value)
          .left
          .map(error =>
            FmriEvidenceError.CompositionFailure(
              run.partition,
              error.getMessage
            )
          )
        estimate <- EvidenceTable
          .operator(
            effects,
            neural,
            composed,
            run.estimateValueId
          )
          .left
          .map(FmriEvidenceError.Evidence.apply)
        estimability <- Estimability(
          effects,
          run.readout.axis.estimability.map:
            case TrialEstimability.Estimable     => true
            case TrialEstimability.ZeroRegressor => false
        ).left.map(FmriEvidenceError.Relation.apply)
        receipt <- RelationFitReceipt(
          run.responseRevision,
          design,
          estimability,
          preparation,
          run.trainingSamples
        ).left.map(FmriEvidenceError.Relation.apply)
        capabilities <- EstimateOnlyCapabilities(neural).left
          .map(FmriEvidenceError.Relation.apply)
        relation <- Relation(estimate, receipt, capabilities).left
          .map(FmriEvidenceError.Relation.apply)
      yield relation
