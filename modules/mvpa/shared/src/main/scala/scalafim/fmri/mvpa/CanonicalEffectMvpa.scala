package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
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
  * Stable geometry may be fixed or learned independently within one run. Data-learned shared geometry must instead name
  * every training fold that produced it; the engine resolves that exact scope before touching responses.
  */
final class CanonicalGeometrySchedule private (
    private val kind: GeometryScheduleKind,
    val geometries: Vector[PreparedContrastGeometry]
):
  private[mvpa] def isStable: Boolean =
    kind == GeometryScheduleKind.Stable

  private[mvpa] def resolve(
      partition: PartitionId,
      runIndex: RunIndex,
      training: TrainingRunScope
  ): Either[FmriEvidenceError, PreparedContrastGeometry] =
    kind match
      case GeometryScheduleKind.Stable =>
        val geometry = geometries(0)
        geometry.receipt.scope match
          case TemporalPreparationScope.Fixed                                    => Right(geometry)
          case TemporalPreparationScope.PerRun(expected) if expected == runIndex => Right(geometry)
          case TemporalPreparationScope.PerRun(expected)                         =>
            Left(
              FmriEvidenceError.InvalidGeometrySchedule(
                s"partition ${partition.value} uses per-run geometry ${expected.value}, expected ${runIndex.value}"
              )
            )
          case TemporalPreparationScope.TrainingFold(_) =>
            Left(FmriEvidenceError.InvalidGeometrySchedule("stable geometry cannot carry training-fold scope"))
      case GeometryScheduleKind.TrainingFold =>
        geometries.find:
          _.receipt.scope match
            case TemporalPreparationScope.TrainingFold(scope) => scope == training
            case _                                            => false
        match
          case Some(geometry) => Right(geometry)
          case None           => Left(FmriEvidenceError.MissingFoldGeometry(partition, training))

object CanonicalGeometrySchedule:
  def stable(geometry: PreparedContrastGeometry): Either[FmriEvidenceError, CanonicalGeometrySchedule] =
    geometry.receipt.scope match
      case TemporalPreparationScope.Fixed | TemporalPreparationScope.PerRun(_) =>
        Right(new CanonicalGeometrySchedule(GeometryScheduleKind.Stable, Vector(geometry)))
      case TemporalPreparationScope.TrainingFold(_) =>
        Left(
          FmriEvidenceError.InvalidGeometrySchedule(
            "training-fold geometry must be supplied through CanonicalGeometrySchedule.trainingFolds"
          )
        )

  def trainingFolds(
      geometries: Seq[PreparedContrastGeometry]
  ): Either[FmriEvidenceError, CanonicalGeometrySchedule] =
    val values = geometries.toVector
    if values.isEmpty then Left(FmriEvidenceError.InvalidGeometrySchedule("training-fold schedule must be non-empty"))
    else
      val scopes = values.map:
        _.receipt.scope match
          case TemporalPreparationScope.TrainingFold(scope) => Right(scope)
          case _                                            =>
            Left(FmriEvidenceError.InvalidGeometrySchedule("every scheduled geometry must carry training-fold scope"))
      collect(scopes).flatMap: typedScopes =>
        if typedScopes.distinct.length != typedScopes.length then
          Left(FmriEvidenceError.InvalidGeometrySchedule("training-fold scopes must be unique"))
        else Right(new CanonicalGeometrySchedule(GeometryScheduleKind.TrainingFold, values))

final class CanonicalRunInput[
    N <: SemanticSpace,
    NeuralKey
] private (
    val partition: PartitionId,
    val response: ResponseBlock,
    val neural: AxisRef.Aux[NeuralKey, N],
    val geometry: CanonicalGeometrySchedule
):
  require(response.voxels == neural.size, "canonical run neural axis must match response columns")

object CanonicalRunInput:
  def apply[N <: SemanticSpace, K](
      partition: PartitionId,
      response: ResponseBlock,
      geometry: CanonicalGeometrySchedule,
      neural: AxisRef.Aux[K, N]
  ): Either[FmriEvidenceError, CanonicalRunInput[N, K]] =
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
        case None => Right(new CanonicalRunInput(partition, response, neural, geometry))

final class CanonicalEffectDataset[
    N <: SemanticSpace,
    NeuralKey
] private (
    val runs: Vector[CanonicalRunInput[N, NeuralKey]],
    val neural: AxisRef.Aux[NeuralKey, N],
    private val trainingScopes: Vector[TrainingRunScope]
):
  require(runs.length >= 2, "canonical effect dataset requires at least two runs")

  private[mvpa] def trainingScope(heldOut: Int): TrainingRunScope =
    trainingScopes(heldOut)

object CanonicalEffectDataset:
  def apply[N <: SemanticSpace, K](
      runs: Seq[CanonicalRunInput[N, K]]
  ): Either[FmriEvidenceError, CanonicalEffectDataset[N, K]] =
    val values = runs.toVector
    if values.length < 2 then Left(FmriEvidenceError.InsufficientPartitions(values.length))
    else
      val duplicates = values
        .groupBy(_.partition)
        .collect:
          case (id, occurrences) if occurrences.length > 1 => id
        .toVector
      if duplicates.nonEmpty then Left(FmriEvidenceError.DuplicatePartition(duplicates.sortBy(_.value).apply(0)))
      else
        val expected = values(0).neural
        values.find(_.neural.identity != expected.identity) match
          case Some(run) =>
            Left(
              FmriEvidenceError.NeuralAxisMismatch(
                run.partition,
                expected.identity.fingerprint,
                run.neural.identity.fingerprint
              )
            )
          case None =>
            values.find(run => !(run.neural.evidence eq expected.evidence)) match
              case Some(run) =>
                Left(FmriEvidenceError.NeuralWitnessMismatch(run.partition))
              case None =>
                val scopes = values.indices.toVector.map: heldOut =>
                  TrainingRunScope
                    .fromInts(values.indices.filter(_ != heldOut).toVector)
                    .left
                    .map(FmriEvidenceError.TemporalPreparationFailure(values(heldOut).partition, _))
                collect(scopes).flatMap: trainingScopes =>
                  val candidate = new CanonicalEffectDataset(values, expected, trainingScopes)
                  validateSchedules(candidate).map(_ => candidate)

  private def validateSchedules[N <: SemanticSpace, K](
      dataset: CanonicalEffectDataset[N, K]
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
    collect(checks.result()).map(_ => ())

final case class CanonicalRunMoments private[mvpa] (
    total: DMat,
    responseDesign: DMat,
    contrastEstimate: DVec,
    contrastVariance: Double,
    effect: DMat,
    residual: DMat,
    temporalReceipt: TemporalPreparationReceipt
):
  require(total.rows == total.cols, "canonical total moment must be square")
  require(
    effect.rows == total.rows && effect.cols == total.cols,
    "canonical effect moment must match the feature space"
  )
  require(
    residual.rows == total.rows && residual.cols == total.cols,
    "canonical residual moment must match the feature space"
  )
  require(responseDesign.rows == total.rows, "response-design moment must have one row per feature")
  require(contrastEstimate.length == total.rows, "canonical contrast estimate must match the feature space")
  require(
    contrastVariance.isFinite && contrastVariance > 0.0,
    "canonical contrast variance must be positive and finite"
  )

private[mvpa] object CanonicalMoments:
  def accumulate(
      preparedResponse: ResponseBlock,
      geometry: PreparedContrastGeometry,
      positions: Array[Int]
  ): Either[FmriEvidenceError, CanonicalRunMoments] =
    if positions.isEmpty then
      Left(FmriEvidenceError.InvalidGeometrySchedule("canonical feature selection must be non-empty"))
    else if positions.exists(position => position < 0 || position >= preparedResponse.voxels) then
      Left(FmriEvidenceError.InvalidGeometrySchedule("canonical feature position is out of bounds"))
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

private[mvpa] def symmetrized(size: Int, values: Array[Double]): DMat =
  val out = Matrix.newBuilder(size, size)
  var row = 0
  while row < size do
    var col = 0
    while col < size do
      out(row, col) = 0.5 * (values(row * size + col) + values(col * size + row))
      col += 1
    row += 1
  out.result()

private[mvpa] def matrix(rows: Int, cols: Int, values: Array[Double]): DMat =
  val out = Matrix.newBuilder(rows, cols)
  var index = 0
  while index < values.length do
    out.writeLinear(index, values(index))
    index += 1
  out.result()

private def collect[A](values: Vector[Either[FmriEvidenceError, A]]): Either[FmriEvidenceError, Vector[A]] =
  val out = Vector.newBuilder[A]
  var index = 0
  while index < values.length do
    values(index) match
      case Left(error)  => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
