package scalafim.fmri.mvpa.fit

import gale.linalg.DVec
import scalafim.dataset.RunId
import scalafim.fmri.fit.{RunIndex, TrainingRunScope}
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

/** A finite signed Rayleigh quotient. Unlike [[scalafim.multivar.CanonicalRoot]],
  * negative values are scientifically meaningful: they indicate that the
  * held-out contrast projection opposes the aggregate training projection.
  */
opaque type SignedCrossRunRayleigh = Double

object SignedCrossRunRayleigh:
  def apply(value: Double): Either[OneShotMvpaError, SignedCrossRunRayleigh] =
    if value.isFinite then Right(value)
    else Left(OneShotMvpaError.InvalidSignedCrossRunValue(value))

  private[fit] def unsafe(value: Double): SignedCrossRunRayleigh =
    require(value.isFinite, "signed cross-run Rayleigh statistic must be finite")
    value

  extension (statistic: SignedCrossRunRayleigh)
    inline def value: Double = statistic

enum SignedCrossRunEstimator:
  /** Learn the direction and residual regularization from training runs, then
    * evaluate the rank-one cross-run numerator without re-optimizing on the
    * held-out response.
    */
  case FrozenTrainingDirection

enum SignedCrossRunOrientation:
  /** The direction's presentation sign cancels from the quadratic cross-run
    * product; the statistic sign comes only from train/held-out agreement.
    */
  case AgreementSign

enum SignedCrossRunExchangeability:
  /** Under the null, independently flip each run's contrast score while
    * leaving residual moments unchanged.
    */
  case RunWiseContrastSignFlip

final case class SignedCrossRunReceipt(
    canonical: CanonicalFoldReceipt,
    estimator: SignedCrossRunEstimator,
    orientation: SignedCrossRunOrientation,
    exchangeability: SignedCrossRunExchangeability
)

final case class SignedCrossRunFoldResult(
    receipt: SignedCrossRunReceipt,
    trainingFit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
    numerator: Double,
    denominator: Double,
    statistic: SignedCrossRunRayleigh
):
  require(numerator.isFinite, "signed cross-run numerator must be finite")
  require(denominator.isFinite && denominator > 0.0, "signed cross-run denominator must be positive and finite")

final case class SignedCrossRunFeatureSetPayload(
    featureSet: FeatureSet,
    folds: Vector[SignedCrossRunFoldResult],
    meanStatistic: SignedCrossRunRayleigh
):
  require(folds.nonEmpty, "signed cross-run payload requires at least one fold")

enum SignedCrossRunFeatureSetOutcome:
  case Success(payload: SignedCrossRunFeatureSetPayload)
  case Failure(selectedFeatureSet: FeatureSet, error: OneShotMvpaError)

  def featureSet: FeatureSet =
    this match
      case Success(payload)                  => payload.featureSet
      case Failure(selectedFeatureSet, _)    => selectedFeatureSet

final case class SignedCrossRunRayleighMvpaResult(
    summary: MvpaResult,
    outcomes: Vector[SignedCrossRunFeatureSetOutcome]
):
  require(summary.outcomes.length == outcomes.length, "typed signed cross-run outcomes must align with the MVPA summary")

  def successes: Vector[SignedCrossRunFeatureSetPayload] =
    outcomes.collect:
      case SignedCrossRunFeatureSetOutcome.Success(payload) => payload

private final case class SignedCrossRunScore(
    numerator: Double,
    denominator: Double,
    statistic: SignedCrossRunRayleigh
)

object SignedCrossRunRayleighMvpa:
  val AnalysisName: String = "signed_cross_run_rayleigh"

  def run(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): SignedCrossRunRayleighMvpaResult =
    val typed = outcomes(dataset, featureSetPlan, regularization).toVector
    SignedCrossRunRayleighMvpaResult(
      MvpaResult(AnalysisName, Some(featureSetPlan), typed.map(toRoiOutcome)),
      typed
    )

  def outcomes(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): Iterator[SignedCrossRunFeatureSetOutcome] =
    featureSetPlan.featureSets.iterator.map: featureSet =>
      evaluate(dataset, featureSet, regularization) match
        case Right(payload) => SignedCrossRunFeatureSetOutcome.Success(payload)
        case Left(error)    => SignedCrossRunFeatureSetOutcome.Failure(featureSet, error)

  def foreach(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  )(visit: SignedCrossRunFeatureSetOutcome => MvpaStreamControl): Unit =
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
  ): Either[OneShotMvpaError, SignedCrossRunFeatureSetPayload] =
    for
      positions <- featurePositions(dataset.featureIndices, featureSet)
      folds <- evaluateFolds(dataset, featureSet, positions, regularization)
      mean = folds.map(_.statistic.value).sum / folds.length.toDouble
      statistic <- signedStatistic(folds.head.receipt.canonical.heldOutRun, mean)
    yield SignedCrossRunFeatureSetPayload(featureSet, folds, statistic)

  private def evaluateFolds(
      dataset: CanonicalEffectDataset,
      featureSet: FeatureSet,
      positions: Array[Int],
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, Vector[SignedCrossRunFoldResult]] =
    val stable = Array.fill[Option[CanonicalRunMoments]](dataset.runs.length)(None)
    val results = Vector.newBuilder[SignedCrossRunFoldResult]
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
      fitTraining(featureSet, trainingIndices, runMoments, regularization) match
        case Left(error) => return Left(error)
        case Right(fit) =>
          scoreFold(dataset.runs(heldOut).runId, trainingIndices, heldOut, runMoments, fit) match
            case Left(error) => return Left(error)
            case Right(scored) =>
              val preparation = dataset.runs.indices.map: index =>
                dataset.runs(index).runId -> runMoments(index).temporalReceipt
              val canonicalReceipt = CanonicalFoldReceipt(
                trainingIndices.map(index => dataset.runs(index).runId),
                dataset.runs(heldOut).runId,
                preparation.toVector,
                CanonicalMomentExecution.RunwiseSufficientStatistics
              )
              results += SignedCrossRunFoldResult(
                SignedCrossRunReceipt(
                  canonicalReceipt,
                  SignedCrossRunEstimator.FrozenTrainingDirection,
                  SignedCrossRunOrientation.AgreementSign,
                  SignedCrossRunExchangeability.RunWiseContrastSignFlip
                ),
                fit,
                scored.numerator,
                scored.denominator,
                scored.statistic
              )
      heldOut += 1
    Right(results.result())

  private def fitTraining(
      featureSet: FeatureSet,
      trainingIndices: Vector[Int],
      moments: Vector[CanonicalRunMoments],
      regularization: ResidualRegularization
  ): Either[OneShotMvpaError, CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace]] =
    val effect = sumMatrices(trainingIndices.map(index => moments(index).effect))
    val residual = sumMatrices(trainingIndices.map(index => moments(index).residual))
    for
      space <- SpaceRef
        .of(s"signed-rayleigh.roi-${featureSet.id.value}", SpaceRole.Observed, featureSet.size)
        .left
        .map(OneShotMvpaError.CanonicalEffectFailure.apply)
      problem <- CanonicalEffectProblem
        .fromDense(space.evidence, effect, residual, regularization)
        .left
        .map(OneShotMvpaError.CanonicalEffectFailure.apply)
      fit <- problem.fit.left.map(OneShotMvpaError.CanonicalEffectFailure.apply)
    yield fit

  private def scoreFold(
      heldOutRun: RunId,
      trainingIndices: Vector[Int],
      heldOut: Int,
      moments: Vector[CanonicalRunMoments],
      fit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace]
  ): Either[OneShotMvpaError, SignedCrossRunScore] =
    fit.solution match
      case CanonicalEffectSolution.LeadingSubspace(_, _, multiplicity) =>
        Left(OneShotMvpaError.NonIdentifiableHeldOutDirection(heldOutRun, multiplicity))
      case CanonicalEffectSolution.Simple(direction, _) =>
        val held = moments(heldOut)
        var trainingProjection = 0.0
        var index = 0
        while index < trainingIndices.length do
          trainingProjection += dot(direction, moments(trainingIndices(index)).contrastEstimate)
          index += 1
        val heldOutProjection = dot(direction, held.contrastEstimate)
        val numerator = trainingProjection * heldOutProjection / held.contrastVariance
        val residual = sumMatrices(trainingIndices.map(run => moments(run).residual))
        val denominator = quadratic(direction, residual) + fit.regularization.ridgeAmount * squaredNorm(direction)
        val scale = matrixFrobenius(residual) + fit.regularization.ridgeAmount * Math.sqrt(direction.length.toDouble)
        val threshold = fit.programFit.identifiability.context.tolerance.threshold(scale)
        if !numerator.isFinite then Left(OneShotMvpaError.NonFiniteCrossRunNumerator(heldOutRun, numerator))
        else if !denominator.isFinite || denominator <= threshold then
          Left(OneShotMvpaError.NonPositiveCrossRunDenominator(heldOutRun, denominator))
        else
          signedStatistic(heldOutRun, numerator / denominator).map: statistic =>
            SignedCrossRunScore(numerator, denominator, statistic)

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

  private def signedStatistic(
      runId: RunId,
      value: Double
  ): Either[OneShotMvpaError, SignedCrossRunRayleigh] =
    if value.isFinite then Right(SignedCrossRunRayleigh.unsafe(value))
    else Left(OneShotMvpaError.InvalidSignedCrossRunStatistic(runId, value))

  private def dot(left: DVec, right: DVec): Double =
    var value = 0.0
    var index = 0
    while index < left.length do
      value += left(index) * right(index)
      index += 1
    value

  private def squaredNorm(vector: DVec): Double =
    dot(vector, vector)

  private def toRoiOutcome(outcome: SignedCrossRunFeatureSetOutcome): RoiOutcome =
    outcome match
      case SignedCrossRunFeatureSetOutcome.Success(payload) =>
        RoiOutcome.Success(
          payload.featureSet.id,
          payload.featureSet.featureIndices,
          MetricVector("SignedCrossRunRayleigh" -> payload.meanStatistic.value),
          None
        )
      case SignedCrossRunFeatureSetOutcome.Failure(featureSet, error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.AnalysisFailed(featureSet.id, error.message))
