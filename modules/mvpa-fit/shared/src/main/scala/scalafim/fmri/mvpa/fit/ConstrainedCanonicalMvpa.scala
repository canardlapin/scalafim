package scalafim.fmri.mvpa.fit

import scalafim.multivar.core.{SemanticSpace, SpaceRef, SpaceRole}
import scalafim.multivar.family.canonical.{CanonicalFrameConstraint, ConstrainedCanonicalFit, ConstrainedCanonicalProblem, ConstrainedCanonicalSolverSpec, ResidualRegularization}

import scalafim.dataset.RunId
import scalafim.fmri.fit.RunIndex
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

enum ConstrainedCanonicalSelection:
  case FixedBeforeFolds

/** Inspectable model specification for the v1 constrained estimand. There is
  * deliberately no response-fitted tuning field: regularization and numerical
  * policy are fixed before any outer fold is evaluated.
  */
final case class NonnegativeCanonicalModelSpec(
    regularization: ResidualRegularization,
    solver: ConstrainedCanonicalSolverSpec = ConstrainedCanonicalSolverSpec.default,
    selection: ConstrainedCanonicalSelection = ConstrainedCanonicalSelection.FixedBeforeFolds
):
  val constraint: CanonicalFrameConstraint = CanonicalFrameConstraint.Nonnegative

final case class NonnegativeCanonicalFoldResult(
    receipt: CanonicalFoldReceipt,
    trainingFit: ConstrainedCanonicalFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoot: Double
):
  require(heldOutRoot.isFinite && heldOutRoot >= 0.0)

final case class NonnegativeCanonicalFeatureSetPayload(
    featureSet: FeatureSet,
    folds: Vector[NonnegativeCanonicalFoldResult],
    meanHeldOutRoot: Double,
    rootToCorrelation: Double
):
  require(folds.nonEmpty)
  require(meanHeldOutRoot.isFinite && meanHeldOutRoot >= 0.0)
  require(rootToCorrelation.isFinite && rootToCorrelation >= 0.0 && rootToCorrelation <= 1.0)

enum NonnegativeCanonicalFeatureSetOutcome:
  case Success(payload: NonnegativeCanonicalFeatureSetPayload)
  case Failure(selectedFeatureSet: FeatureSet, error: OneShotMvpaError)

  def featureSet: FeatureSet =
    this match
      case Success(payload)                 => payload.featureSet
      case Failure(selectedFeatureSet, _) => selectedFeatureSet

final case class NonnegativeCanonicalMvpaResult(
    model: NonnegativeCanonicalModelSpec,
    summary: MvpaResult,
    outcomes: Vector[NonnegativeCanonicalFeatureSetOutcome]
):
  require(summary.outcomes.length == outcomes.length)

  def successes: Vector[NonnegativeCanonicalFeatureSetPayload] =
    outcomes.collect:
      case NonnegativeCanonicalFeatureSetOutcome.Success(payload) => payload

/** One-shot nonnegative canonical MVPA.
  *
  * Each outer fold derives effect and residual moments only from its training
  * runs, fits the typed constrained program, and then accesses the held-out run
  * for the first time. Solver settings are fixed before fold construction; v1
  * performs no response-selected tuning.
  */
object NonnegativeCanonicalMvpa:
  val AnalysisName: String = "nonnegative_canonical_contrast_effect"

  def run(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      model: NonnegativeCanonicalModelSpec
  ): NonnegativeCanonicalMvpaResult =
    val typed = outcomes(dataset, featureSetPlan, model).toVector
    NonnegativeCanonicalMvpaResult(
      model,
      MvpaResult(AnalysisName, Some(featureSetPlan), typed.map(toRoiOutcome)),
      typed
    )

  def run(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  ): NonnegativeCanonicalMvpaResult =
    run(dataset, featureSetPlan, NonnegativeCanonicalModelSpec(regularization))

  def outcomes(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      model: NonnegativeCanonicalModelSpec
  ): Iterator[NonnegativeCanonicalFeatureSetOutcome] =
    featureSetPlan.featureSets.iterator.map: featureSet =>
      evaluate(dataset, featureSet, model) match
        case Right(payload) => NonnegativeCanonicalFeatureSetOutcome.Success(payload)
        case Left(error)    => NonnegativeCanonicalFeatureSetOutcome.Failure(featureSet, error)

  def foreach(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      model: NonnegativeCanonicalModelSpec
  )(visit: NonnegativeCanonicalFeatureSetOutcome => MvpaStreamControl): Unit =
    val iterator = outcomes(dataset, featureSetPlan, model)
    var running = true
    while running && iterator.hasNext do
      visit(iterator.next()) match
        case MvpaStreamControl.Continue =>
        case MvpaStreamControl.Stop     => running = false

  def foreach(
      dataset: CanonicalEffectDataset,
      featureSetPlan: FeatureSetPlan,
      regularization: ResidualRegularization
  )(visit: NonnegativeCanonicalFeatureSetOutcome => MvpaStreamControl): Unit =
    foreach(dataset, featureSetPlan, NonnegativeCanonicalModelSpec(regularization))(visit)

  private def evaluate(
      dataset: CanonicalEffectDataset,
      featureSet: FeatureSet,
      model: NonnegativeCanonicalModelSpec
  ): Either[OneShotMvpaError, NonnegativeCanonicalFeatureSetPayload] =
    for
      positions <- featurePositions(dataset.featureIndices, featureSet)
      folds <- evaluateFolds(dataset, featureSet, positions, model)
      mean = folds.map(_.heldOutRoot).sum / folds.length.toDouble
      _ <-
        if mean.isFinite && mean >= 0.0 then Right(())
        else Left(OneShotMvpaError.InvalidHeldOutRoot(folds.head.receipt.heldOutRun, mean))
      correlation = Math.sqrt(mean / (1.0 + mean))
    yield NonnegativeCanonicalFeatureSetPayload(featureSet, folds, mean, correlation)

  private def evaluateFolds(
      dataset: CanonicalEffectDataset,
      featureSet: FeatureSet,
      positions: Array[Int],
      model: NonnegativeCanonicalModelSpec
  ): Either[OneShotMvpaError, Vector[NonnegativeCanonicalFoldResult]] =
    val stable = Array.fill[Option[CanonicalRunMoments]](dataset.runs.length)(None)
    val results = Vector.newBuilder[NonnegativeCanonicalFoldResult]
    var heldOut = 0
    while heldOut < dataset.runs.length do
      val trainingScope = dataset.trainingScope(heldOut)
      val trainingIndices = dataset.runs.indices.filter(_ != heldOut).toVector
      val trainingMoments = Vector.newBuilder[(Int, CanonicalRunMoments)]
      var trainingPosition = 0
      while trainingPosition < trainingIndices.length do
        val runIndex = trainingIndices(trainingPosition)
        momentsFor(dataset.runs(runIndex), RunIndex.unsafe(runIndex), trainingScope, positions, stable(runIndex)) match
          case Left(error) => return Left(error)
          case Right(value) =>
            trainingMoments += runIndex -> value
            if dataset.runs(runIndex).geometry.isStable then stable(runIndex) = Some(value)
        trainingPosition += 1
      val indexedTraining = trainingMoments.result()
      val effect = sumMatrices(indexedTraining.map(_._2.effect))
      val residual = sumMatrices(indexedTraining.map(_._2.residual))
      fit(featureSet, effect, residual, model) match
        case Left(error) => return Left(error)
        case Right(fitted) =>
          momentsFor(dataset.runs(heldOut), RunIndex.unsafe(heldOut), trainingScope, positions, stable(heldOut)) match
            case Left(error) => return Left(error)
            case Right(heldOutMoments) =>
              if dataset.runs(heldOut).geometry.isStable then stable(heldOut) = Some(heldOutMoments)
              heldOutScore(dataset.runs(heldOut).runId, fitted, heldOutMoments) match
                case Left(error) => return Left(error)
                case Right(root) =>
                  val preparation =
                    indexedTraining.map: (index, moments) =>
                      dataset.runs(index).runId -> moments.temporalReceipt
                    :+ (dataset.runs(heldOut).runId -> heldOutMoments.temporalReceipt)
                  results += NonnegativeCanonicalFoldResult(
                    CanonicalFoldReceipt(
                      trainingIndices.map(index => dataset.runs(index).runId),
                      dataset.runs(heldOut).runId,
                      preparation,
                      CanonicalMomentExecution.RunwiseSufficientStatistics
                    ),
                    fitted,
                    root
                  )
      heldOut += 1
    Right(results.result())

  private def fit(
      featureSet: FeatureSet,
      effect: gale.linalg.DMat,
      residual: gale.linalg.DMat,
      model: NonnegativeCanonicalModelSpec
  ): Either[OneShotMvpaError, ConstrainedCanonicalFit[? <: SemanticSpace, ? <: SemanticSpace]] =
    SpaceRef
      .of(s"nonnegative-canonical.roi-${featureSet.id.value}", SpaceRole.Observed, featureSet.size)
      .left
      .map(OneShotMvpaError.CanonicalEffectFailure.apply)
      .flatMap: space =>
        ConstrainedCanonicalProblem
          .fromDense(space.evidence, effect, residual, model.regularization, model.constraint, model.solver)
          .left
          .map(OneShotMvpaError.CanonicalEffectFailure.apply)
          .flatMap(_.fit.left.map(OneShotMvpaError.CanonicalEffectFailure.apply))

  private def momentsFor(
      run: CanonicalRunInput,
      runIndex: RunIndex,
      training: scalafim.fmri.fit.TrainingRunScope,
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
      fit: ConstrainedCanonicalFit[? <: SemanticSpace, ? <: SemanticSpace],
      heldOut: CanonicalRunMoments
  ): Either[OneShotMvpaError, Double] =
    val numerator = quadratic(fit.direction, heldOut.effect)
    val denominator = quadratic(fit.direction, heldOut.residual)
    val threshold = fit.programFit.identifiability.context.tolerance.threshold(matrixFrobenius(heldOut.residual))
    if !denominator.isFinite || denominator <= threshold then
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

  private def toRoiOutcome(outcome: NonnegativeCanonicalFeatureSetOutcome): RoiOutcome =
    outcome match
      case NonnegativeCanonicalFeatureSetOutcome.Success(payload) =>
        RoiOutcome.Success(
          payload.featureSet.id,
          payload.featureSet.featureIndices,
          MetricVector(
            "MeanNonnegativeCanonicalRoot" -> payload.meanHeldOutRoot,
            "NonnegativeCanonicalCorrelation" -> payload.rootToCorrelation
          ),
          None
        )
      case NonnegativeCanonicalFeatureSetOutcome.Failure(featureSet, error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.AnalysisFailed(featureSet.id, error.message))
