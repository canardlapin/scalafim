package scalafim.fmri.mvpa

import gale.linalg.Matrix
import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity
import scalafim.fmri.fit.RunIndex
import scalafim.fmri.fit.TemporalPreparationReceipt
import scalafim.fmri.fit.TemporalPreparationScope
import scalafim.fmri.fit.TemporalWhiteningReceipt

/** fMRI response and prepared temporal geometry lowered directly into the canonical relation source. Stable and
  * training-fold-specific geometries produce the same [[CrossValidatedRelations]] type; the latter retain one exact
  * relation set for each held-out partition.
  */
object RunwiseCanonicalRelations:
  def scalar[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      dataset: CanonicalEffectDataset[N, NK],
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      responseRevisions: Map[PartitionId, ValueIdentity]
  ): Either[
    FmriEvidenceError,
    CrossValidatedRelations[
      P,
      E,
      N,
      EK,
      NK,
      ScalarEffectFitCapabilities[N, NK]
    ]
  ] =
    for
      _ <- validateDatasetAxes(dataset.runs.map(_.partition), 1, partitions, effects)
      scheduled <- scalarSchedule(dataset, partitions, effects, responseRevisions)
      source <- CrossValidatedRelations
        .scheduled(partitions, effects, dataset.neural, scheduled)
        .left
        .map(FmriEvidenceError.Schedule.apply)
    yield source

  def manova[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      dataset: ManovaDataset[N, NK],
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      responseRevisions: Map[PartitionId, ValueIdentity]
  ): Either[
    FmriEvidenceError,
    CrossValidatedRelations[P, E, N, EK, NK, ResidualFitCapabilities[N, NK]]
  ] =
    for
      _ <- validateDatasetAxes(
        dataset.runs.map(_.partition),
        dataset.contrastRank,
        partitions,
        effects
      )
      scheduled <- manovaSchedule(dataset, partitions, effects, responseRevisions)
      source <- CrossValidatedRelations
        .scheduled(partitions, effects, dataset.neural, scheduled)
        .left
        .map(FmriEvidenceError.Schedule.apply)
    yield source

  private def scalarSchedule[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      dataset: CanonicalEffectDataset[N, NK],
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      responseRevisions: Map[PartitionId, ValueIdentity]
  ): Either[
    FmriEvidenceError,
    Vector[(PartitionId, PartitionedRelations[P, E, N, EK, NK, ScalarEffectFitCapabilities[N, NK]])]
  ] =
    val neural = dataset.neural
    val scheduled = Vector.newBuilder[
      (PartitionId, PartitionedRelations[P, E, N, EK, NK, ScalarEffectFitCapabilities[N, NK]])
    ]
    val positions = Array.tabulate(neural.size)(identity)
    var heldOutPosition = 0
    while heldOutPosition < dataset.runs.length do
      val heldOut = partitions.axis.keys(heldOutPosition)
      val trainingScope = dataset.trainingScope(heldOutPosition)
      val entries = Vector.newBuilder[
        PartitionRelation[E, N, EK, NK, ScalarEffectFitCapabilities[N, NK]]
      ]
      var runPosition = 0
      while runPosition < dataset.runs.length do
        val run = dataset.runs(runPosition)
        val partition = partitions.axis.keys(runPosition)
        val revision = responseRevisions.get(partition) match
          case None        => return Left(FmriEvidenceError.MissingResponseRevision(partition))
          case Some(value) => value
        val geometry = run.geometry
          .resolve(run.partition, RunIndex.unsafe(runPosition), trainingScope) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        val prepared = geometry.prepareResponse(run.response) match
          case Left(error)  => return Left(FmriEvidenceError.TemporalPreparationFailure(partition, error))
          case Right(value) => value
        val moments = CanonicalMoments.accumulate(prepared, geometry, positions) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        scalarRelation(
          heldOut,
          partition,
          effects,
          neural,
          revision,
          moments
        ) match
          case Left(error)  => return Left(error)
          case Right(value) => entries += PartitionRelation(partition, value)
        runPosition += 1
      PartitionedRelations(partitions, effects, neural, entries.result()) match
        case Left(error)  => return Left(FmriEvidenceError.Relation(error))
        case Right(value) => scheduled += heldOut -> value
      heldOutPosition += 1
    Right(scheduled.result())

  private def manovaSchedule[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      dataset: ManovaDataset[N, NK],
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      responseRevisions: Map[PartitionId, ValueIdentity]
  ): Either[
    FmriEvidenceError,
    Vector[(PartitionId, PartitionedRelations[P, E, N, EK, NK, ResidualFitCapabilities[N, NK]])]
  ] =
    val neural = dataset.neural
    val scheduled = Vector.newBuilder[
      (PartitionId, PartitionedRelations[P, E, N, EK, NK, ResidualFitCapabilities[N, NK]])
    ]
    val positions = Array.tabulate(neural.size)(identity)
    var heldOutPosition = 0
    while heldOutPosition < dataset.runs.length do
      val heldOut = partitions.axis.keys(heldOutPosition)
      val trainingScope = dataset.trainingScope(heldOutPosition)
      val entries = Vector.newBuilder[
        PartitionRelation[E, N, EK, NK, ResidualFitCapabilities[N, NK]]
      ]
      var runPosition = 0
      while runPosition < dataset.runs.length do
        val run = dataset.runs(runPosition)
        val partition = partitions.axis.keys(runPosition)
        val revision = responseRevisions.get(partition) match
          case None        => return Left(FmriEvidenceError.MissingResponseRevision(partition))
          case Some(value) => value
        val geometry = run.geometry
          .resolve(run.partition, RunIndex.unsafe(runPosition), trainingScope) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        val prepared = geometry.prepareResponse(run.response) match
          case Left(error)  => return Left(FmriEvidenceError.TemporalPreparationFailure(partition, error))
          case Right(value) => value
        val moments = ManovaMoments.accumulate(prepared, geometry, positions) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        manovaRelation(
          heldOut,
          partition,
          effects,
          neural,
          revision,
          moments
        ) match
          case Left(error)  => return Left(error)
          case Right(value) => entries += PartitionRelation(partition, value)
        runPosition += 1
      PartitionedRelations(partitions, effects, neural, entries.result()) match
        case Left(error)  => return Left(FmriEvidenceError.Relation(error))
        case Right(value) => scheduled += heldOut -> value
      heldOutPosition += 1
    Right(scheduled.result())

  private def scalarRelation[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      heldOut: PartitionId,
      partition: PartitionId,
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      responseRevision: ValueIdentity,
      moments: CanonicalRunMoments
  ): Either[
    FmriEvidenceError,
    Relation[E, N, EK, NK, ScalarEffectFitCapabilities[N, NK]]
  ] =
    val estimateBuilder = Matrix.newBuilder(1, neural.size)
    val scale = Math.sqrt(moments.contrastVariance)
    var feature = 0
    while feature < neural.size do
      estimateBuilder(0, feature) = moments.contrastEstimate(feature) / scale
      feature += 1
    val suffix = s"${heldOut.value}-${partition.value}"
    for
      estimate <- EvidenceTable
        .dense(
          effects,
          neural,
          estimateBuilder.result(),
          ValueId.unsafe(s"canonical-effect-$suffix")
        )
        .left
        .map(FmriEvidenceError.Evidence.apply)
      residual <- EvidenceTable
        .dense(
          neural,
          neural,
          moments.residual,
          ValueId.unsafe(s"canonical-residual-$suffix")
        )
        .left
        .map(FmriEvidenceError.Evidence.apply)
      certifiedResidual <- CertifiedResidualMoments(residual).left.map(FmriEvidenceError.Relation.apply)
      training <- trainingAxis(partition, heldOut, moments.temporalReceipt.selectedTimepoints.toVector)
      preparation <- normalization(moments.temporalReceipt)
      design <- relationDesign(moments.temporalReceipt)
      estimability <- Estimability(effects, Vector(true)).left.map(FmriEvidenceError.Relation.apply)
      receipt <- RelationFitReceipt(
        responseRevision,
        design,
        estimability,
        preparation,
        training
      ).left.map(FmriEvidenceError.Relation.apply)
      capabilities <- ScalarEffectFitCapabilities(
        certifiedResidual,
        scalafim.fmri.mvpa.ResidualDegreesOfFreedom.unsafe(
          moments.temporalReceipt.residualDegreesOfFreedom.value.toDouble
        ),
        EffectNormalizationVariance.unsafe(moments.contrastVariance)
      ).left.map(FmriEvidenceError.Relation.apply)
      relation <- Relation(estimate, receipt, capabilities).left.map(FmriEvidenceError.Relation.apply)
    yield relation

  private def manovaRelation[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      heldOut: PartitionId,
      partition: PartitionId,
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      responseRevision: ValueIdentity,
      moments: ManovaRunMoments
  ): Either[
    FmriEvidenceError,
    Relation[E, N, EK, NK, ResidualFitCapabilities[N, NK]]
  ] =
    val suffix = s"${heldOut.value}-${partition.value}"
    for
      estimate <- EvidenceTable
        .dense(
          effects,
          neural,
          moments.normalizedEffects.t,
          ValueId.unsafe(s"manova-effect-$suffix")
        )
        .left
        .map(FmriEvidenceError.Evidence.apply)
      residual <- EvidenceTable
        .dense(
          neural,
          neural,
          moments.residual,
          ValueId.unsafe(s"manova-residual-$suffix")
        )
        .left
        .map(FmriEvidenceError.Evidence.apply)
      certifiedResidual <- CertifiedResidualMoments(residual).left.map(FmriEvidenceError.Relation.apply)
      training <- trainingAxis(partition, heldOut, moments.temporalReceipt.selectedTimepoints.toVector)
      preparation <- normalization(moments.temporalReceipt)
      design <- relationDesign(moments.temporalReceipt)
      estimability <- Estimability(effects, Vector.fill(effects.size)(true)).left.map(FmriEvidenceError.Relation.apply)
      receipt <- RelationFitReceipt(
        responseRevision,
        design,
        estimability,
        preparation,
        training
      ).left.map(FmriEvidenceError.Relation.apply)
      capabilities <- ResidualFitCapabilities(
        certifiedResidual,
        scalafim.fmri.mvpa.ResidualDegreesOfFreedom.unsafe(
          moments.temporalReceipt.residualDegreesOfFreedom.value.toDouble
        )
      ).left.map(FmriEvidenceError.Relation.apply)
      relation <- Relation(estimate, receipt, capabilities).left.map(FmriEvidenceError.Relation.apply)
    yield relation

  private def trainingAxis(
      partition: PartitionId,
      heldOut: PartitionId,
      timepoints: Vector[Int]
  ): Either[FmriEvidenceError, AxisRef[SampleId]] =
    AxisRef
      .create(
        AxisId.unsafe(s"canonical-training-${heldOut.value}-${partition.value}"),
        AxisPurpose.Samples,
        timepoints.map(timepoint => SampleId.unsafe(s"${partition.value}-time-$timepoint")),
        CoordinateBasis.unsafe(
          "prepared-timepoints",
          "held-out" -> heldOut.value,
          "partition" -> partition.value
        ),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe(
          "runwise-canonical-relations",
          "v1",
          heldOut.value,
          partition.value
        )
      )
      .left
      .map(FmriEvidenceError.Axis.apply)

  private def relationDesign(
      receipt: TemporalPreparationReceipt
  ): Either[FmriEvidenceError, DesignIdentity] =
    DesignIdentity(
      DesignKind.unsafe("fmri-canonical-relation-fit"),
      Vector(
        "contrast" -> receipt.contrastName,
        "contrast-rank" -> receipt.contrastRank.toString,
        "design-rank" -> receipt.designRank.toString,
        "scope" -> scopeLabel(receipt.scope),
        "timepoints" -> receipt.selectedTimepoints.toVector.mkString(",")
      )
    ).left.map(FmriEvidenceError.Identity.apply)

  private def normalization(
      receipt: TemporalPreparationReceipt
  ): Either[FmriEvidenceError, NormalizationIdentity] =
    NormalizationStepIdentity(
      NormalizationStepId.unsafe("temporal-preparation"),
      Vector(
        "nuisance-rank" -> receipt.nuisanceRank.value.toString,
        "provenance" -> receipt.provenance.toString,
        "scope" -> scopeLabel(receipt.scope),
        "timepoints" -> receipt.selectedTimepoints.toVector.mkString(","),
        "whitening" -> whiteningLabel(receipt.whitening)
      )
    ).left
      .map(FmriEvidenceError.Identity.apply)
      .map(step => NormalizationIdentity(Vector(step)))

  private def scopeLabel(scope: TemporalPreparationScope): String =
    scope match
      case TemporalPreparationScope.Fixed                  => "fixed"
      case TemporalPreparationScope.PerRun(run)            => s"per-run:${run.value}"
      case TemporalPreparationScope.TrainingFold(training) =>
        s"training-fold:${training.runs.map(_.value).mkString(",")}"

  private def whiteningLabel(receipt: TemporalWhiteningReceipt): String =
    receipt match
      case TemporalWhiteningReceipt.Iid                                               => "iid"
      case TemporalWhiteningReceipt.Shared(method, pooling, arOrder, segments, exact) =>
        s"shared:$method:$pooling:$arOrder:$segments:$exact"

  private def validateDatasetAxes[
      P <: SemanticSpace,
      E <: SemanticSpace,
      EK
  ](
      runPartitions: Vector[PartitionId],
      effectCount: Int,
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E]
  ): Either[FmriEvidenceError, Unit] =
    if partitions.axis.size != runPartitions.length then
      Left(
        FmriEvidenceError.PartitionCountMismatch(
          runPartitions.length,
          partitions.axis.size
        )
      )
    else
      var position = 0
      while position < runPartitions.length do
        val expected = runPartitions(position)
        val actual = partitions.axis.keys(position)
        if actual != expected then
          return Left(
            FmriEvidenceError.PartitionKeyMismatch(
              position,
              expected,
              actual
            )
          )
        position += 1
      if effects.size != effectCount then Left(FmriEvidenceError.EffectCountMismatch(effectCount, effects.size))
      else Right(())
