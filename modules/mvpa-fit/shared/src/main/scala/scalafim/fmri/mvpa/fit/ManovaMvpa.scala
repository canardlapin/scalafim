package scalafim.fmri.mvpa.fit

import multivar.family.canonical.{CanonicalEffectProblem, CanonicalRootSpectrum, CanonicalSpectrumFit, ManovaStatistics, ResidualRegularization}

import scalafim.dataset.RunId
import scalafim.fmri.fit.{
  PreparedManovaGeometry,
  ResponseBlock,
  RunIndex,
  TemporalPreparationReceipt,
  TemporalPreparationScope,
  TrainingRunScope
}
import gale.linalg.{DMat, Matrix}

private enum ManovaScheduleKind:
  case Stable
  case TrainingFold

final class ManovaGeometrySchedule private (
    private val kind: ManovaScheduleKind,
    val geometries: Vector[PreparedManovaGeometry]
):
  private[fit] def isStable: Boolean = kind == ManovaScheduleKind.Stable

  private[fit] def resolve(
      runId: RunId,
      runIndex: RunIndex,
      training: TrainingRunScope
  ): Either[OneShotMvpaError, PreparedManovaGeometry] =
    kind match
      case ManovaScheduleKind.Stable =>
        val geometry = geometries.head
        geometry.receipt.scope match
          case TemporalPreparationScope.Fixed => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) if expected == runIndex => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) =>
            Left(
              OneShotMvpaError.InvalidGeometrySchedule(
                s"run ${runId.value} uses per-run MANOVA geometry ${expected.value}, expected ${runIndex.value}"
              )
            )
          case TemporalPreparationScope.TrainingFold(_) =>
            Left(OneShotMvpaError.InvalidGeometrySchedule("stable MANOVA geometry cannot carry training-fold scope"))
      case ManovaScheduleKind.TrainingFold =>
        geometries.find:
          _.receipt.scope match
            case TemporalPreparationScope.TrainingFold(scope) => scope == training
            case _                                             => false
        match
          case Some(geometry) => Right(geometry)
          case None           => Left(OneShotMvpaError.MissingFoldGeometry(runId, training))

object ManovaGeometrySchedule:
  def stable(geometry: PreparedManovaGeometry): Either[OneShotMvpaError, ManovaGeometrySchedule] =
    geometry.receipt.scope match
      case TemporalPreparationScope.Fixed | TemporalPreparationScope.PerRun(_) =>
        Right(new ManovaGeometrySchedule(ManovaScheduleKind.Stable, Vector(geometry)))
      case TemporalPreparationScope.TrainingFold(_) =>
        Left(
          OneShotMvpaError.InvalidGeometrySchedule(
            "training-fold MANOVA geometry must be supplied through ManovaGeometrySchedule.trainingFolds"
          )
        )

  def trainingFolds(
      geometries: Seq[PreparedManovaGeometry]
  ): Either[OneShotMvpaError, ManovaGeometrySchedule] =
    val values = geometries.toVector
    if values.isEmpty then Left(OneShotMvpaError.InvalidGeometrySchedule("MANOVA training-fold schedule must be non-empty"))
    else
      val scopes = values.map:
        _.receipt.scope match
          case TemporalPreparationScope.TrainingFold(scope) => Right(scope)
          case _ => Left(OneShotMvpaError.InvalidGeometrySchedule("every MANOVA geometry must carry training-fold scope"))
      collectManova(scopes).flatMap: typedScopes =>
        if typedScopes.distinct.length != typedScopes.length then
          Left(OneShotMvpaError.InvalidGeometrySchedule("MANOVA training-fold scopes must be unique"))
        else validateHypothesis(values).map(_ => new ManovaGeometrySchedule(ManovaScheduleKind.TrainingFold, values))

  private def validateHypothesis(values: Vector[PreparedManovaGeometry]): Either[OneShotMvpaError, Unit] =
    val first = values.head.receipt
    values.find(geometry =>
      geometry.receipt.contrastRank != first.contrastRank || geometry.receipt.contrastName != first.contrastName
    ) match
      case Some(_) => Left(OneShotMvpaError.InvalidGeometrySchedule("scheduled MANOVA geometries must describe one hypothesis"))
      case None    => Right(())

final case class ManovaRunMoments private[fit] (
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

enum ManovaMomentExecution:
  case RunwiseSufficientStatistics

final case class ManovaFoldReceipt(
    trainingRuns: Vector[RunId],
    heldOutRun: RunId,
    temporalPreparation: Vector[(RunId, TemporalPreparationReceipt)],
    execution: ManovaMomentExecution
)

private[fit] object ManovaMoments:
  def accumulate(
      preparedResponse: ResponseBlock,
      geometry: PreparedManovaGeometry,
      positions: Array[Int]
  ): Either[OneShotMvpaError, ManovaRunMoments] =
    if positions.isEmpty then Left(OneShotMvpaError.InvalidGeometrySchedule("MANOVA feature selection must be non-empty"))
    else if positions.exists(position => position < 0 || position >= preparedResponse.voxels) then
      Left(OneShotMvpaError.InvalidGeometrySchedule("MANOVA feature position is out of bounds"))
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
    values: Vector[Either[OneShotMvpaError, A]]
): Either[OneShotMvpaError, Vector[A]] =
  val out = Vector.newBuilder[A]
  var index = 0
  while index < values.length do
    values(index) match
      case Left(error)  => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
