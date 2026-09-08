package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import scalafim.fmri.fit.{
  PreparedManovaGeometry,
  ResponseBlock,
  RunIndex,
  TemporalPreparationReceipt,
  TemporalPreparationScope,
  TrainingRunScope
}
import gale.linalg.DMat

private enum ManovaScheduleKind:
  case Stable
  case TrainingFold

final class ManovaGeometrySchedule private (
    private val kind: ManovaScheduleKind,
    val geometries: Vector[PreparedManovaGeometry]
):
  private[mvpa] def isStable: Boolean = kind == ManovaScheduleKind.Stable

  private[mvpa] def resolve(
      partition: PartitionId,
      runIndex: RunIndex,
      training: TrainingRunScope
  ): Either[FmriEvidenceError, PreparedManovaGeometry] =
    kind match
      case ManovaScheduleKind.Stable =>
        val geometry = geometries(0)
        geometry.receipt.scope match
          case TemporalPreparationScope.Fixed                                    => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) if expected == runIndex => Right(geometry)
          case TemporalPreparationScope.PerRun(expected)                         =>
            Left(
              FmriEvidenceError.InvalidGeometrySchedule(
                s"partition ${partition.value} uses per-run MANOVA geometry ${expected.value}, expected ${runIndex.value}"
              )
            )
          case TemporalPreparationScope.TrainingFold(_) =>
            Left(FmriEvidenceError.InvalidGeometrySchedule("stable MANOVA geometry cannot carry training-fold scope"))
      case ManovaScheduleKind.TrainingFold =>
        geometries.find:
          _.receipt.scope match
            case TemporalPreparationScope.TrainingFold(scope) => scope == training
            case _                                            => false
        match
          case Some(geometry) => Right(geometry)
          case None           => Left(FmriEvidenceError.MissingFoldGeometry(partition, training))

object ManovaGeometrySchedule:
  def stable(geometry: PreparedManovaGeometry): Either[FmriEvidenceError, ManovaGeometrySchedule] =
    geometry.receipt.scope match
      case TemporalPreparationScope.Fixed | TemporalPreparationScope.PerRun(_) =>
        Right(new ManovaGeometrySchedule(ManovaScheduleKind.Stable, Vector(geometry)))
      case TemporalPreparationScope.TrainingFold(_) =>
        Left(
          FmriEvidenceError.InvalidGeometrySchedule(
            "training-fold MANOVA geometry must be supplied through ManovaGeometrySchedule.trainingFolds"
          )
        )

  def trainingFolds(
      geometries: Seq[PreparedManovaGeometry]
  ): Either[FmriEvidenceError, ManovaGeometrySchedule] =
    val values = geometries.toVector
    if values.isEmpty then
      Left(FmriEvidenceError.InvalidGeometrySchedule("MANOVA training-fold schedule must be non-empty"))
    else
      val scopes = values.map:
        _.receipt.scope match
          case TemporalPreparationScope.TrainingFold(scope) => Right(scope)
          case _                                            =>
            Left(FmriEvidenceError.InvalidGeometrySchedule("every MANOVA geometry must carry training-fold scope"))
      collectManova(scopes).flatMap: typedScopes =>
        if typedScopes.distinct.length != typedScopes.length then
          Left(FmriEvidenceError.InvalidGeometrySchedule("MANOVA training-fold scopes must be unique"))
        else validateHypothesis(values).map(_ => new ManovaGeometrySchedule(ManovaScheduleKind.TrainingFold, values))

  private def validateHypothesis(values: Vector[PreparedManovaGeometry]): Either[FmriEvidenceError, Unit] =
    val first = values(0).receipt
    values.find(geometry =>
      geometry.receipt.contrastRank != first.contrastRank || geometry.receipt.contrastName != first.contrastName
    ) match
      case Some(_) =>
        Left(FmriEvidenceError.InvalidGeometrySchedule("scheduled MANOVA geometries must describe one hypothesis"))
      case None => Right(())

final class ManovaRunInput[
    N <: SemanticSpace,
    NeuralKey
] private (
    val partition: PartitionId,
    val response: ResponseBlock,
    val neural: AxisRef.Aux[NeuralKey, N],
    val geometry: ManovaGeometrySchedule
):
  require(response.voxels == neural.size, "MANOVA run neural axis must match response columns")

object ManovaRunInput:
  def apply[N <: SemanticSpace, K](
      partition: PartitionId,
      response: ResponseBlock,
      geometry: ManovaGeometrySchedule,
      neural: AxisRef.Aux[K, N]
  ): Either[FmriEvidenceError, ManovaRunInput[N, K]] =
    if neural.size != response.voxels then
      Left(FmriEvidenceError.NeuralCountMismatch(partition, neural.size, response.voxels))
    else if neural.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(
        FmriEvidenceError.Observations(
          ObservationsError.InvalidNeuralPurpose(neural.identity.purpose)
        )
      )
    else
      geometry.geometries.find(_.preparedDesign.timepoints != response.timepoints) match
        case Some(prepared) =>
          Left(FmriEvidenceError.TimepointMismatch(partition, prepared.preparedDesign.timepoints, response.timepoints))
        case None => Right(new ManovaRunInput(partition, response, neural, geometry))

final class ManovaDataset[
    N <: SemanticSpace,
    NeuralKey
] private (
    val runs: Vector[ManovaRunInput[N, NeuralKey]],
    val neural: AxisRef.Aux[NeuralKey, N],
    private val trainingScopes: Vector[TrainingRunScope],
    val contrastRank: Int
):
  private[mvpa] def trainingScope(heldOut: Int): TrainingRunScope = trainingScopes(heldOut)

object ManovaDataset:
  def apply[N <: SemanticSpace, K](
      runs: Seq[ManovaRunInput[N, K]]
  ): Either[FmriEvidenceError, ManovaDataset[N, K]] =
    val values = runs.toVector
    if values.length < 2 then Left(FmriEvidenceError.InsufficientPartitions(values.length))
    else
      val duplicate = values
        .groupBy(_.partition)
        .collect:
          case (id, occurrences) if occurrences.length > 1 => id
      if duplicate.nonEmpty then Left(FmriEvidenceError.DuplicatePartition(duplicate.toVector.sortBy(_.value).apply(0)))
      else
        val expectedNeural = values(0).neural
        values.find(_.neural.identity != expectedNeural.identity) match
          case Some(run) =>
            Left(
              FmriEvidenceError.NeuralAxisMismatch(
                run.partition,
                expectedNeural.identity.fingerprint,
                run.neural.identity.fingerprint
              )
            )
          case None =>
            values.find(run => !(run.neural.evidence eq expectedNeural.evidence)) match
              case Some(run) =>
                Left(FmriEvidenceError.NeuralWitnessMismatch(run.partition))
              case None =>
                val rank = values(0).geometry.geometries(0).contrastRank
                val name = values(0).geometry.geometries(0).receipt.contrastName
                if values.exists(
                    _.geometry.geometries.exists(geometry =>
                      geometry.contrastRank != rank || geometry.receipt.contrastName != name
                    )
                  )
                then
                  Left(
                    FmriEvidenceError.InvalidGeometrySchedule(
                      "all MANOVA runs must use the same named contrast subspace"
                    )
                  )
                else
                  val scopes = values.indices.toVector.map: heldOut =>
                    TrainingRunScope
                      .fromInts(values.indices.filter(_ != heldOut).toVector)
                      .left
                      .map(FmriEvidenceError.TemporalPreparationFailure(values(heldOut).partition, _))
                  collectManova(scopes).flatMap: trainingScopes =>
                    val dataset = new ManovaDataset(values, expectedNeural, trainingScopes, rank)
                    validateSchedules(dataset).map(_ => dataset)

  private def validateSchedules[N <: SemanticSpace, K](
      dataset: ManovaDataset[N, K]
  ): Either[FmriEvidenceError, Unit] =
    val checks = Vector.newBuilder[Either[FmriEvidenceError, Unit]]
    var heldOut = 0
    while heldOut < dataset.runs.length do
      val training = dataset.trainingScope(heldOut)
      var run = 0
      while run < dataset.runs.length do
        val input = dataset.runs(run)
        checks += input.geometry.resolve(input.partition, RunIndex.unsafe(run), training).map(_ => ())
        run += 1
      heldOut += 1
    collectManova(checks.result()).map(_ => ())

final case class ManovaRunMoments private[mvpa] (
    total: DMat,
    responseDesign: DMat,
    normalizedEffects: DMat,
    effect: DMat,
    residual: DMat,
    temporalReceipt: TemporalPreparationReceipt
):
  require(effect.rows == effect.cols, "MANOVA effect moment must be square")
  require(residual.rows == effect.rows && residual.cols == effect.cols, "MANOVA residual must match effect")
  require(normalizedEffects.rows == effect.rows, "MANOVA effect scores must match feature space")
  require(normalizedEffects.cols == temporalReceipt.contrastRank, "MANOVA effect scores must match contrast rank")

private[mvpa] object ManovaMoments:
  def accumulate(
      preparedResponse: ResponseBlock,
      geometry: PreparedManovaGeometry,
      positions: Array[Int]
  ): Either[FmriEvidenceError, ManovaRunMoments] =
    if positions.isEmpty then
      Left(FmriEvidenceError.InvalidGeometrySchedule("MANOVA feature selection must be non-empty"))
    else if positions.exists(position => position < 0 || position >= preparedResponse.voxels) then
      Left(FmriEvidenceError.InvalidGeometrySchedule("MANOVA feature position is out of bounds"))
    else
      val features = positions.length
      val predictors = geometry.preparedDesign.predictors
      val rank = geometry.contrastRank
      val h = new Array[Double](features * features)
      val g = new Array[Double](features * predictors)
      val response = preparedResponse.value
      val design = geometry.preparedDesign.value
      var row = 0
      while row < response.rows do
        var left = 0
        while left < features do
          val zLeft = response(row, positions(left))
          var right = 0
          while right < features do
            h(left * features + right) += zLeft * response(row, positions(right))
            right += 1
          var predictor = 0
          while predictor < predictors do
            g(left * predictors + predictor) += zLeft * design(row, predictor)
            predictor += 1
          left += 1
        row += 1

      val effects = new Array[Double](features * rank)
      var feature = 0
      while feature < features do
        var component = 0
        while component < rank do
          var predictor = 0
          while predictor < predictors do
            effects(feature * rank + component) +=
              g(feature * predictors + predictor) * geometry.effectBasis(predictor, component)
            predictor += 1
          component += 1
        feature += 1

      val effect = new Array[Double](features * features)
      val residual = new Array[Double](features * features)
      var left = 0
      while left < features do
        var right = 0
        while right < features do
          var component = 0
          while component < rank do
            effect(left * features + right) +=
              effects(left * rank + component) * effects(right * rank + component)
            component += 1
          var correction = 0.0
          var p = 0
          while p < predictors do
            var q = 0
            while q < predictors do
              correction += g(left * predictors + p) * geometry.inverseXtX(p, q) * g(right * predictors + q)
              q += 1
            p += 1
          residual(left * features + right) = h(left * features + right) - correction
          right += 1
        left += 1

      Right(
        ManovaRunMoments(
          matrix(features, features, h),
          matrix(features, predictors, g),
          matrix(features, rank, effects),
          symmetrized(features, effect),
          symmetrized(features, residual),
          geometry.receipt
        )
      )

private def collectManova[A](
    values: Vector[Either[FmriEvidenceError, A]]
): Either[FmriEvidenceError, Vector[A]] =
  val out = Vector.newBuilder[A]
  var index = 0
  while index < values.length do
    values(index) match
      case Left(error)  => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
