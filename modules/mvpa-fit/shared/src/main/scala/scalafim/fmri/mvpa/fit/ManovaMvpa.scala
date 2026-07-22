package scalafim.fmri.mvpa.fit

import scalafim.multivar.core.{SemanticSpace, SpaceRef, SpaceRole}
import scalafim.multivar.family.canonical.{CanonicalEffectProblem, CanonicalRootSpectrum, CanonicalSpectrumFit, ManovaStatistics, ResidualRegularization}

import scalafim.dataset.RunId
import scalafim.fmri.fit.{
  PreparedManovaGeometry,
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

final class ManovaRunInput private (
    val runId: RunId,
    val response: ResponseBlock,
    val featureIndices: Vector[FeatureIndex],
    val geometry: ManovaGeometrySchedule
):
  require(response.voxels == featureIndices.length, "MANOVA run feature axis must match response columns")

object ManovaRunInput:
  def make(
      runId: RunId,
      response: ResponseBlock,
      geometry: ManovaGeometrySchedule,
      featureIndices: Vector[FeatureIndex]
  ): Either[OneShotMvpaError, ManovaRunInput] =
    if featureIndices.length != response.voxels then
      Left(OneShotMvpaError.FeatureAxisLengthMismatch(runId, featureIndices.length, response.voxels))
    else if featureIndices.distinct.length != featureIndices.length then
      Left(OneShotMvpaError.InvalidGeometrySchedule(s"run ${runId.value} MANOVA feature indices must be unique"))
    else
      geometry.geometries.find(_.preparedDesign.timepoints != response.timepoints) match
        case Some(prepared) =>
          Left(OneShotMvpaError.TimepointMismatch(runId, prepared.preparedDesign.timepoints, response.timepoints))
        case None => Right(new ManovaRunInput(runId, response, featureIndices, geometry))

  def make(
      runId: RunId,
      response: ResponseBlock,
      geometry: ManovaGeometrySchedule
  ): Either[OneShotMvpaError, ManovaRunInput] =
    make(runId, response, geometry, Vector.tabulate(response.voxels)(FeatureIndex.apply))

final class ManovaDataset private (
    val runs: Vector[ManovaRunInput],
    private val trainingScopes: Vector[TrainingRunScope],
    val contrastRank: Int
):
  val featureIndices: Vector[FeatureIndex] = runs.head.featureIndices
  private[fit] def trainingScope(heldOut: Int): TrainingRunScope = trainingScopes(heldOut)

object ManovaDataset:
  def make(runs: Seq[ManovaRunInput]): Either[OneShotMvpaError, ManovaDataset] =
    val values = runs.toVector
    if values.length < 2 then Left(OneShotMvpaError.InsufficientRunsForCrossValidation(values.length))
    else
      val duplicate = values.groupBy(_.runId).collect:
        case (id, occurrences) if occurrences.length > 1 => id
      if duplicate.nonEmpty then Left(OneShotMvpaError.DuplicateRuns(duplicate.toVector.sortBy(_.value)))
      else
        val expectedFeatures = values.head.featureIndices
        values.find(_.featureIndices != expectedFeatures) match
          case Some(run) => Left(OneShotMvpaError.FeatureAxisMismatch(run.runId, expectedFeatures, run.featureIndices))
          case None =>
            val rank = values.head.geometry.geometries.head.contrastRank
            val name = values.head.geometry.geometries.head.receipt.contrastName
            if values.exists(_.geometry.geometries.exists(geometry =>
                geometry.contrastRank != rank || geometry.receipt.contrastName != name
              ))
            then
              Left(OneShotMvpaError.InvalidGeometrySchedule("all MANOVA runs must use the same named contrast subspace"))
            else
              val scopes = values.indices.toVector.map: heldOut =>
                TrainingRunScope
                  .fromInts(values.indices.filter(_ != heldOut).toVector)
                  .left
                  .map(OneShotMvpaError.TemporalPreparationFailure(values(heldOut).runId, _))
              collectManova(scopes).flatMap: trainingScopes =>
                val dataset = new ManovaDataset(values, trainingScopes, rank)
                validateSchedules(dataset).map(_ => dataset)

  private def validateSchedules(dataset: ManovaDataset): Either[OneShotMvpaError, Unit] =
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
    collectManova(checks.result()).map(_ => ())

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

final case class ManovaFoldResult(
    receipt: ManovaFoldReceipt,
    trainingFit: CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoots: CanonicalRootSpectrum,
    heldOutStatistics: ManovaStatistics
)

final case class ManovaFeatureSetPayload(
    featureSet: FeatureSet,
    folds: Vector[ManovaFoldResult],
    meanStatistics: ManovaStatistics
):
  require(folds.nonEmpty, "MANOVA feature-set payload requires folds")

enum ManovaFeatureSetOutcome:
  case Success(payload: ManovaFeatureSetPayload)
  case Failure(selectedFeatureSet: FeatureSet, error: OneShotMvpaError)

  def featureSet: FeatureSet =
    this match
      case Success(payload)                => payload.featureSet
      case Failure(selectedFeatureSet, _) => selectedFeatureSet

final case class ManovaMvpaResult(
    summary: MvpaResult,
    outcomes: Vector[ManovaFeatureSetOutcome]
):
  require(summary.outcomes.length == outcomes.length, "typed MANOVA outcomes must align with summary")

object ManovaMvpa:
  val AnalysisName: String = "canonical_manova"

  def run(
      dataset: ManovaDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): ManovaMvpaResult =
    val typed = outcomes(dataset, featureSetPlan, regularization).toVector
    ManovaMvpaResult(
      MvpaResult(AnalysisName, Some(featureSetPlan), typed.map(toRoiOutcome)),
      typed
    )

  def outcomes(
      dataset: ManovaDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): Iterator[ManovaFeatureSetOutcome] =
    featureSetPlan.featureSets.iterator.map: featureSet =>
      evaluate(dataset, featureSet, regularization) match
        case Right(payload) => ManovaFeatureSetOutcome.Success(payload)
        case Left(error)    => ManovaFeatureSetOutcome.Failure(featureSet, error)

  def foreach(
      dataset: ManovaDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  )(visit: ManovaFeatureSetOutcome => MvpaStreamControl): Unit =
    val iterator = outcomes(dataset, featureSetPlan, regularization)
    var running = true
    while running && iterator.hasNext do
      visit(iterator.next()) match
        case MvpaStreamControl.Continue =>
        case MvpaStreamControl.Stop     => running = false

  private def evaluate(
      dataset: ManovaDataset,
      featureSet: FeatureSet,
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, ManovaFeatureSetPayload] =
    for
      positions <- featurePositions(dataset.featureIndices, featureSet)
      folds <- evaluateFolds(dataset, featureSet, positions, regularization)
    yield ManovaFeatureSetPayload(featureSet, folds, meanStatistics(folds.map(_.heldOutStatistics)))

  private def evaluateFolds(
      dataset: ManovaDataset,
      featureSet: FeatureSet,
      positions: Array[Int],
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, Vector[ManovaFoldResult]] =
    val stable = Array.fill[Option[ManovaRunMoments]](dataset.runs.length)(None)
    val results = Vector.newBuilder[ManovaFoldResult]
    var heldOut = 0
    while heldOut < dataset.runs.length do
      val trainingScope = dataset.trainingScope(heldOut)
      val moments = Vector.newBuilder[ManovaRunMoments]
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
      val trainingFit = fitSpectrum(
        s"manova.roi-${featureSet.id.value}",
        effect,
        residual,
        dataset.contrastRank,
        regularization
      )
      trainingFit match
        case Left(error) => return Left(error)
        case Right(fit) =>
          heldOutSpectrum(fit, runMoments(heldOut), dataset.contrastRank, regularization) match
            case Left(error) => return Left(error)
            case Right(heldOutFit) =>
              val preparation = dataset.runs.indices.map: index =>
                dataset.runs(index).runId -> runMoments(index).temporalReceipt
              results += ManovaFoldResult(
                ManovaFoldReceipt(
                  trainingIndices.map(index => dataset.runs(index).runId),
                  dataset.runs(heldOut).runId,
                  preparation.toVector,
                  ManovaMomentExecution.RunwiseSufficientStatistics
                ),
                fit,
                heldOutFit.roots,
                heldOutFit.statistics
              )
      heldOut += 1
    Right(results.result())

  private def fitSpectrum(
      id: String,
      effect: DMat,
      residual: DMat,
      rank: Int,
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace]] =
    SpaceRef
      .of(id, SpaceRole.Observed, effect.rows)
      .left
      .map(OneShotMvpaError.CanonicalEffectFailure.apply)
      .flatMap: space =>
        CanonicalEffectProblem
          .fromDense(space.evidence, effect, residual, regularization)
          .left
          .map(OneShotMvpaError.CanonicalEffectFailure.apply)
          .flatMap(_.fitSpectrum(rank).left.map(OneShotMvpaError.CanonicalEffectFailure.apply))

  private def heldOutSpectrum(
      trainingFit: CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace],
      heldOut: ManovaRunMoments,
      rank: Int,
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace]] =
    for
      frame <- trainingFit.denseFrame.left.map(OneShotMvpaError.CanonicalEffectFailure.apply)
      effect = compress(frame, heldOut.effect)
      residual = compress(frame, heldOut.residual)
      fit <- fitSpectrum("manova.held-out", effect, residual, rank, regularization)
    yield fit

  private def momentsFor(
      run: ManovaRunInput,
      runIndex: RunIndex,
      training: TrainingRunScope,
      positions: Array[Int],
      cached: Option[ManovaRunMoments]
  ): Either[OneShotMvpaError, ManovaRunMoments] =
    cached match
      case Some(value) => Right(value)
      case None =>
        for
          geometry <- run.geometry.resolve(run.runId, runIndex, training)
          prepared <- geometry.prepareResponse(run.response).left.map(OneShotMvpaError.TemporalPreparationFailure(run.runId, _))
          moments <- ManovaMoments.accumulate(prepared, geometry, positions)
        yield moments

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

  private def meanStatistics(values: Vector[ManovaStatistics]): ManovaStatistics =
    val count = values.length.toDouble
    ManovaStatistics(
      values.map(_.royLargestRoot).sum / count,
      values.map(_.wilksLambda).sum / count,
      values.map(_.pillaiTrace).sum / count,
      values.map(_.hotellingLawleyTrace).sum / count
    )

  private def toRoiOutcome(outcome: ManovaFeatureSetOutcome): RoiOutcome =
    outcome match
      case ManovaFeatureSetOutcome.Success(payload) =>
        RoiOutcome.Success(
          payload.featureSet.id,
          payload.featureSet.featureIndices,
          MetricVector(
            "RoyLargestRoot" -> payload.meanStatistics.royLargestRoot,
            "WilksLambda" -> payload.meanStatistics.wilksLambda,
            "PillaiTrace" -> payload.meanStatistics.pillaiTrace,
            "HotellingLawleyTrace" -> payload.meanStatistics.hotellingLawleyTrace
          ),
          None
        )
      case ManovaFeatureSetOutcome.Failure(featureSet, error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.AnalysisFailed(featureSet.id, error.message))

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

private def compress(frame: DMat, value: DMat): DMat =
  require(frame.rows == value.rows && value.rows == value.cols, "MANOVA compression dimensions must agree")
  val out = Matrix.newBuilder(frame.cols, frame.cols)
  var left = 0
  while left < frame.cols do
    var right = 0
    while right < frame.cols do
      var total = 0.0
      var row = 0
      while row < value.rows do
        var col = 0
        while col < value.cols do
          total += frame(row, left) * value(row, col) * frame(col, right)
          col += 1
        row += 1
      out(left, right) = total
      right += 1
    left += 1
  out.result()

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
