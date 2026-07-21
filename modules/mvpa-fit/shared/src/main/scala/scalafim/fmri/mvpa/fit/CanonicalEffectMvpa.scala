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
import scalafim.fmri.mvpa.{
  FeatureIndex,
  FeatureSet,
  FeatureSetPlan,
  MetricVector,
  MvpaError,
  MvpaResult,
  MvpaStreamControl,
  RoiOutcome
}
import scalafim.multivar.{
  CanonicalEffectFit,
  CanonicalEffectProblem,
  CanonicalEffectSolution,
  ResidualRegularization,
  SemanticSpace,
  SpaceRef,
  SpaceRole
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

final class CanonicalRunInput private (
    val runId: RunId,
    val response: ResponseBlock,
    val featureIndices: Vector[FeatureIndex],
    val geometry: CanonicalGeometrySchedule
):
  require(response.voxels == featureIndices.length, "canonical run feature axis must match response columns")

object CanonicalRunInput:
  def make(
      runId: RunId,
      response: ResponseBlock,
      geometry: CanonicalGeometrySchedule,
      featureIndices: Vector[FeatureIndex]
  ): Either[OneShotMvpaError, CanonicalRunInput] =
    if featureIndices.length != response.voxels then
      Left(OneShotMvpaError.FeatureAxisLengthMismatch(runId, featureIndices.length, response.voxels))
    else if featureIndices.distinct.length != featureIndices.length then
      Left(OneShotMvpaError.InvalidGeometrySchedule(s"run ${runId.value} feature indices must be unique"))
    else
      geometry.geometries.find(_.preparedDesign.timepoints != response.timepoints) match
        case Some(prepared) =>
          Left(OneShotMvpaError.TimepointMismatch(runId, prepared.preparedDesign.timepoints, response.timepoints))
        case None => Right(new CanonicalRunInput(runId, response, featureIndices, geometry))

  def make(
      runId: RunId,
      response: ResponseBlock,
      geometry: CanonicalGeometrySchedule
  ): Either[OneShotMvpaError, CanonicalRunInput] =
    make(runId, response, geometry, Vector.tabulate(response.voxels)(FeatureIndex.apply))

final class CanonicalEffectDataset private (
    val runs: Vector[CanonicalRunInput],
    private val trainingScopes: Vector[TrainingRunScope]
):
  require(runs.length >= 2, "canonical effect dataset requires at least two runs")

  val featureIndices: Vector[FeatureIndex] = runs.head.featureIndices

  private[fit] def trainingScope(heldOut: Int): TrainingRunScope =
    trainingScopes(heldOut)

object CanonicalEffectDataset:
  def make(runs: Seq[CanonicalRunInput]): Either[OneShotMvpaError, CanonicalEffectDataset] =
    val values = runs.toVector
    if values.length < 2 then Left(OneShotMvpaError.InsufficientRunsForCrossValidation(values.length))
    else
      val duplicates = values
        .groupBy(_.runId)
        .collect:
          case (id, occurrences) if occurrences.length > 1 => id
        .toVector
      if duplicates.nonEmpty then Left(OneShotMvpaError.DuplicateRuns(duplicates.sortBy(_.value)))
      else
        val expected = values.head.featureIndices
        values.find(_.featureIndices != expected) match
          case Some(run) => Left(OneShotMvpaError.FeatureAxisMismatch(run.runId, expected, run.featureIndices))
          case None =>
            val scopes = values.indices.toVector.map: heldOut =>
              TrainingRunScope
                .fromInts(values.indices.filter(_ != heldOut).toVector)
                .left
                .map(OneShotMvpaError.TemporalPreparationFailure(values(heldOut).runId, _))
            collect(scopes).flatMap: trainingScopes =>
              val candidate = new CanonicalEffectDataset(values, trainingScopes)
              validateSchedules(candidate).map(_ => candidate)

  private def validateSchedules(dataset: CanonicalEffectDataset): Either[OneShotMvpaError, Unit] =
    val checks = Vector.newBuilder[Either[OneShotMvpaError, Unit]]
    var heldOut = 0
    while heldOut < dataset.runs.length do
      val training = dataset.trainingScope(heldOut)
      var run = 0
      while run < dataset.runs.length do
        val input = dataset.runs(run)
        checks += input.geometry.resolve(input.runId, RunIndex.unsafe(run), training).map(_ => ())
        run += 1
      heldOut += 1
    collect(checks.result()).map(_ => ())

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

final case class CanonicalFoldResult(
    receipt: CanonicalFoldReceipt,
    trainingFit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoot: Double
):
  require(heldOutRoot.isFinite && heldOutRoot >= 0.0, "held-out root must be finite and non-negative")

final case class CanonicalFeatureSetPayload(
    featureSet: FeatureSet,
    folds: Vector[CanonicalFoldResult],
    meanHeldOutRoot: Double,
    rootToCorrelation: Double
):
  require(folds.nonEmpty, "canonical feature-set payload requires folds")
  require(meanHeldOutRoot.isFinite && meanHeldOutRoot >= 0.0, "mean held-out root must be finite and non-negative")
  require(rootToCorrelation.isFinite && rootToCorrelation >= 0.0 && rootToCorrelation <= 1.0, "root correlation must be in [0, 1]")

enum CanonicalFeatureSetOutcome:
  case Success(payload: CanonicalFeatureSetPayload)
  case Failure(selectedFeatureSet: FeatureSet, error: OneShotMvpaError)

  def featureSet: FeatureSet =
    this match
      case Success(payload)       => payload.featureSet
      case Failure(selectedFeatureSet, _) => selectedFeatureSet

final case class CanonicalEffectMvpaResult(
    summary: MvpaResult,
    outcomes: Vector[CanonicalFeatureSetOutcome]
):
  require(summary.outcomes.length == outcomes.length, "typed canonical outcomes must align with the MVPA summary")

  def successes: Vector[CanonicalFeatureSetPayload] =
    outcomes.collect:
      case CanonicalFeatureSetOutcome.Success(payload) => payload

object CanonicalEffectMvpa:
  val AnalysisName: String = "canonical_contrast_effect"

  def run(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): CanonicalEffectMvpaResult =
    val typed = outcomes(dataset, featureSetPlan, regularization).toVector
    val ordinary = typed.map(toRoiOutcome)
    CanonicalEffectMvpaResult(MvpaResult(AnalysisName, Some(featureSetPlan), ordinary), typed)

  def outcomes(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): Iterator[CanonicalFeatureSetOutcome] =
    featureSetPlan.featureSets.iterator.map: featureSet =>
      evaluate(dataset, featureSet, regularization) match
        case Right(payload) => CanonicalFeatureSetOutcome.Success(payload)
        case Left(error)    => CanonicalFeatureSetOutcome.Failure(featureSet, error)

  def foreach(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  )(visit: CanonicalFeatureSetOutcome => MvpaStreamControl): Unit =
    val iterator = outcomes(dataset, featureSetPlan, regularization)
    var running = true
    while running && iterator.hasNext do
      visit(iterator.next()) match
        case MvpaStreamControl.Continue =>
        case MvpaStreamControl.Stop     => running = false

  private def evaluate(
      dataset: CanonicalEffectDataset,
      featureSet: FeatureSet,
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, CanonicalFeatureSetPayload] =
    for
      positions <- featurePositions(dataset.featureIndices, featureSet)
      folds <- evaluateFolds(dataset, featureSet, positions, regularization)
      mean = folds.map(_.heldOutRoot).sum / folds.length.toDouble
      _ <-
        if mean.isFinite && mean >= 0.0 then Right(())
        else Left(OneShotMvpaError.InvalidHeldOutRoot(folds.head.receipt.heldOutRun, mean))
      correlation = Math.sqrt(mean / (1.0 + mean))
    yield CanonicalFeatureSetPayload(featureSet, folds, mean, correlation)

  private def evaluateFolds(
      dataset: CanonicalEffectDataset,
      featureSet: FeatureSet,
      positions: Array[Int],
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, Vector[CanonicalFoldResult]] =
    val stable = Array.fill[Option[CanonicalRunMoments]](dataset.runs.length)(None)
    val results = Vector.newBuilder[CanonicalFoldResult]
    var heldOut = 0
    while heldOut < dataset.runs.length do
      val trainingScope = dataset.trainingScope(heldOut)
      val moments = Vector.newBuilder[CanonicalRunMoments]
      var run = 0
      while run < dataset.runs.length do
        momentsFor(dataset.runs(run), RunIndex.unsafe(run), trainingScope, positions, stable(run)) match
          case Left(error) => return Left(error)
          case Right(value) =>
            moments += value
            if dataset.runs(run).geometry.isStable then stable(run) = Some(value)
        run += 1
      val runMoments = moments.result()
      val trainingIndices = dataset.runs.indices.filter(_ != heldOut).toVector
      val effect = sumMatrices(trainingIndices.map(index => runMoments(index).effect))
      val residual = sumMatrices(trainingIndices.map(index => runMoments(index).residual))
      val featureSpace = SpaceRef
        .of(s"canonical.roi-${featureSet.id.value}", SpaceRole.Observed, featureSet.size)
        .left
        .map(OneShotMvpaError.CanonicalEffectFailure.apply)
      val fitted = featureSpace.flatMap: space =>
        CanonicalEffectProblem
          .fromDense(space.evidence, effect, residual, regularization)
          .left
          .map(OneShotMvpaError.CanonicalEffectFailure.apply)
          .flatMap(_.fit.left.map(OneShotMvpaError.CanonicalEffectFailure.apply))
      fitted match
        case Left(error) => return Left(error)
        case Right(fit) =>
          heldOutScore(dataset.runs(heldOut).runId, fit, runMoments(heldOut)) match
            case Left(error) => return Left(error)
            case Right(root) =>
              val preparation = dataset.runs.indices.map: index =>
                dataset.runs(index).runId -> runMoments(index).temporalReceipt
              results += CanonicalFoldResult(
                CanonicalFoldReceipt(
                  trainingIndices.map(index => dataset.runs(index).runId),
                  dataset.runs(heldOut).runId,
                  preparation.toVector,
                  CanonicalMomentExecution.RunwiseSufficientStatistics
                ),
                fit,
                root
              )
      heldOut += 1
    Right(results.result())

  private def momentsFor(
      run: CanonicalRunInput,
      runIndex: RunIndex,
      training: TrainingRunScope,
      positions: Array[Int],
      cached: Option[CanonicalRunMoments]
  ): Either[OneShotMvpaError, CanonicalRunMoments] =
    cached match
      case Some(value) => Right(value)
      case None =>
        for
          geometry <- run.geometry.resolve(run.runId, runIndex, training)
          prepared <- geometry.prepareResponse(run.response).left.map(OneShotMvpaError.TemporalPreparationFailure(run.runId, _))
          moments <- CanonicalMoments.accumulate(prepared, geometry, positions)
        yield moments

  private def heldOutScore(
      runId: RunId,
      fit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
      heldOut: CanonicalRunMoments
  ): Either[OneShotMvpaError, Double] =
    fit.solution match
      case CanonicalEffectSolution.LeadingSubspace(_, _, multiplicity) =>
        Left(OneShotMvpaError.NonIdentifiableHeldOutDirection(runId, multiplicity))
      case CanonicalEffectSolution.Simple(direction, _) =>
        val numerator = quadratic(direction, heldOut.effect)
        val denominator = quadratic(direction, heldOut.residual)
        val denominatorThreshold = fit.programFit.identifiability.context.tolerance.threshold(matrixFrobenius(heldOut.residual))
        if !denominator.isFinite || denominator <= denominatorThreshold then
          Left(OneShotMvpaError.NonPositiveHeldOutDenominator(runId, denominator))
        else
          val root = numerator / denominator
          if root.isFinite && root >= 0.0 then Right(root)
          else if root.isFinite && root >= -1e-10 then Right(0.0)
          else Left(OneShotMvpaError.InvalidHeldOutRoot(runId, root))

  private def featurePositions(
      available: Vector[FeatureIndex],
      featureSet: FeatureSet
  ): Either[OneShotMvpaError, Array[Int]] =
    val index = available.zipWithIndex.map((feature, position) => feature.value -> position).toMap
    val positions = new Array[Int](featureSet.size)
    var feature = 0
    while feature < featureSet.size do
      val requested = featureSet.featureIndices(feature)
      index.get(requested.value) match
        case Some(position) => positions(feature) = position
        case None => return Left(OneShotMvpaError.MvpaFailure(MvpaError.MissingFeature(featureSet.id, requested)))
      feature += 1
    Right(positions)

  private def toRoiOutcome(outcome: CanonicalFeatureSetOutcome): RoiOutcome =
    outcome match
      case CanonicalFeatureSetOutcome.Success(payload) =>
        RoiOutcome.Success(
          payload.featureSet.id,
          payload.featureSet.featureIndices,
          MetricVector(
            "MeanCanonicalRoot" -> payload.meanHeldOutRoot,
            "CanonicalCorrelation" -> payload.rootToCorrelation
          ),
          None
        )
      case CanonicalFeatureSetOutcome.Failure(featureSet, error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.AnalysisFailed(featureSet.id, error.message))

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
    out.updateRowMajor(index, values(index))
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
