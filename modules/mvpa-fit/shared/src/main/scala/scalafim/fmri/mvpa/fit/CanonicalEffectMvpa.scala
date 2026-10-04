package scalafim.fmri.mvpa.fit


import scalafim.dataset.RunId
import scalafim.fmri.fit.{
  PreparedContrastGeometry,
  ResponseBlock,
  RunIndex,
  TemporalPreparationReceipt,
  TemporalPreparationScope,
  TrainingRunScope
}
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.Matrix
import gale.linalg.Vec

private enum GeometryScheduleKind:
  case Stable
  case TrainingFold

/** Inspectable schedule for response-independent temporal geometry.
  *
  * Stable geometry may be fixed or learned independently within one run.
  * Response-learned shared geometry must instead name every training fold that
  * produced it; the engine resolves that exact scope before touching responses.
  */
final class CanonicalGeometrySchedule private (
    private val kind: GeometryScheduleKind,
    val geometries: Vector[PreparedContrastGeometry]
):
  private[fit] def isStable: Boolean =
    kind == GeometryScheduleKind.Stable

  private[fit] def resolve(
      runId: RunId,
      runIndex: RunIndex,
      training: TrainingRunScope
  ): Either[OneShotMvpaError, PreparedContrastGeometry] =
    kind match
      case GeometryScheduleKind.Stable =>
        val geometry = geometries.head
        geometry.receipt.scope match
          case TemporalPreparationScope.Fixed => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) if expected == runIndex => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) =>
            Left(
              OneShotMvpaError.InvalidGeometrySchedule(
                s"run ${runId.value} uses per-run geometry ${expected.value}, expected ${runIndex.value}"
              )
            )
          case TemporalPreparationScope.TrainingFold(_) =>
            Left(OneShotMvpaError.InvalidGeometrySchedule("stable geometry cannot carry training-fold scope"))
      case GeometryScheduleKind.TrainingFold =>
        geometries.find:
          _.receipt.scope match
            case TemporalPreparationScope.TrainingFold(scope) => scope == training
            case _ => false
        match
          case Some(geometry) => Right(geometry)
          case None           => Left(OneShotMvpaError.MissingFoldGeometry(runId, training))

object CanonicalGeometrySchedule:
  def stable(geometry: PreparedContrastGeometry): Either[OneShotMvpaError, CanonicalGeometrySchedule] =
    geometry.receipt.scope match
      case TemporalPreparationScope.Fixed | TemporalPreparationScope.PerRun(_) =>
        Right(new CanonicalGeometrySchedule(GeometryScheduleKind.Stable, Vector(geometry)))
      case TemporalPreparationScope.TrainingFold(_) =>
        Left(
          OneShotMvpaError.InvalidGeometrySchedule(
            "training-fold geometry must be supplied through CanonicalGeometrySchedule.trainingFolds"
          )
        )

  def trainingFolds(
      geometries: Seq[PreparedContrastGeometry]
  ): Either[OneShotMvpaError, CanonicalGeometrySchedule] =
    val values = geometries.toVector
    if values.isEmpty then Left(OneShotMvpaError.InvalidGeometrySchedule("training-fold schedule must be non-empty"))
    else
      val scopes = values.map:
        _.receipt.scope match
          case TemporalPreparationScope.TrainingFold(scope) => Right(scope)
          case _ => Left(OneShotMvpaError.InvalidGeometrySchedule("every scheduled geometry must carry training-fold scope"))
      collect(scopes).flatMap: typedScopes =>
        if typedScopes.distinct.length != typedScopes.length then
          Left(OneShotMvpaError.InvalidGeometrySchedule("training-fold scopes must be unique"))
        else Right(new CanonicalGeometrySchedule(GeometryScheduleKind.TrainingFold, values))

final case class CanonicalRunMoments private[fit] (
    total: DMat,
    responseDesign: DMat,
    contrastEstimate: DVec,
    contrastVariance: Double,
    effect: DMat,
    residual: DMat,
    temporalReceipt: TemporalPreparationReceipt
):
  require(total.rows == total.cols, "canonical total moment must be square")
  require(effect.rows == total.rows && effect.cols == total.cols, "canonical effect moment must match the feature space")
  require(residual.rows == total.rows && residual.cols == total.cols, "canonical residual moment must match the feature space")
  require(responseDesign.rows == total.rows, "response-design moment must have one row per feature")
  require(contrastEstimate.length == total.rows, "canonical contrast estimate must match the feature space")
  require(contrastVariance.isFinite && contrastVariance > 0.0, "canonical contrast variance must be positive and finite")

enum CanonicalMomentExecution:
  case RunwiseSufficientStatistics

final case class CanonicalFoldReceipt(
    trainingRuns: Vector[RunId],
    heldOutRun: RunId,
    temporalPreparation: Vector[(RunId, TemporalPreparationReceipt)],
    execution: CanonicalMomentExecution
)

private[fit] object CanonicalMoments:
  def accumulate(
      preparedResponse: ResponseBlock,
      geometry: PreparedContrastGeometry,
      positions: Array[Int]
  ): Either[OneShotMvpaError, CanonicalRunMoments] =
    if positions.isEmpty then Left(OneShotMvpaError.InvalidGeometrySchedule("canonical feature selection must be non-empty"))
    else if positions.exists(position => position < 0 || position >= preparedResponse.voxels) then
      Left(OneShotMvpaError.InvalidGeometrySchedule("canonical feature position is out of bounds"))
    else
      val features = positions.length
      val predictors = geometry.preparedDesign.predictors
      val h = Array.ofDim[Double](features * features)
      val g = Array.ofDim[Double](features * predictors)
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

      val effectVector = Array.ofDim[Double](features)
      var feature = 0
      while feature < features do
        var predictor = 0
        while predictor < predictors do
          effectVector(feature) += g(feature * predictors + predictor) * geometry.effectBasis(predictor, 0)
          predictor += 1
        feature += 1

      val effect = Array.ofDim[Double](features * features)
      val residual = Array.ofDim[Double](features * features)
      var left = 0
      while left < features do
        var right = 0
        while right < features do
          effect(left * features + right) = effectVector(left) * effectVector(right)
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

      val totalMatrix = matrix(features, features, h)
      val responseDesign = matrix(features, predictors, g)
      val contrastEstimate = Vec.newBuilder(features)
      val contrastScale = Math.sqrt(geometry.contrastVariance)
      feature = 0
      while feature < features do
        contrastEstimate(feature) = effectVector(feature) * contrastScale
        feature += 1
      val effectMatrix = matrix(features, features, effect)
      val residualMatrix = symmetrized(features, residual)
      Right(
        CanonicalRunMoments(
          totalMatrix,
          responseDesign,
          contrastEstimate.result(),
          geometry.contrastVariance,
          effectMatrix,
          residualMatrix,
          geometry.receipt
        )
      )

private[fit] def sumMatrices(values: Vector[DMat]): DMat =
  require(values.nonEmpty, "matrix sum must be non-empty")
  val rows = values.head.rows
  val cols = values.head.cols
  val out = Matrix.newBuilder(rows, cols)
  var row = 0
  while row < rows do
    var col = 0
    while col < cols do
      var value = 0.0
      var index = 0
      while index < values.length do
        value += values(index)(row, col)
        index += 1
      out(row, col) = value
      col += 1
    row += 1
  out.result()

private[fit] def quadratic(vector: DVec, matrix: DMat): Double =
  var value = 0.0
  var row = 0
  while row < vector.length do
    var col = 0
    while col < vector.length do
      value += vector(row) * matrix(row, col) * vector(col)
      col += 1
    row += 1
  value

private[fit] def matrixFrobenius(matrix: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      squared += matrix(row, col) * matrix(row, col)
      col += 1
    row += 1
  Math.sqrt(squared)

private[fit] def symmetrized(size: Int, values: Array[Double]): DMat =
  val out = Matrix.newBuilder(size, size)
  var row = 0
  while row < size do
    var col = 0
    while col < size do
      out(row, col) = 0.5 * (values(row * size + col) + values(col * size + row))
      col += 1
    row += 1
  out.result()

private[fit] def matrix(rows: Int, cols: Int, values: Array[Double]): DMat =
  val out = Matrix.newBuilder(rows, cols)
  var index = 0
  while index < values.length do
    out.writeLinear(index, values(index))
    index += 1
  out.result()

private def collect[A](values: Vector[Either[OneShotMvpaError, A]]): Either[OneShotMvpaError, Vector[A]] =
  val out = Vector.newBuilder[A]
  var index = 0
  while index < values.length do
    values(index) match
      case Left(error)  => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
