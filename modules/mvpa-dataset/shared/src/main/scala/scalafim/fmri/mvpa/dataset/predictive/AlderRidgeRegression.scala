package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{CompleteResampler, CoordinateError, CoordinateWriter, FeatureSchema, FeatureView, FixedCoverage, IdentifiedRows, Resample4sResampler, crossFitted}
import alder.kernel.*
import alder.models.linear.{RidgeConfig, RidgeModel, RidgeRegression, RidgeSolution}
import alder.ridge.gale.{GaleRidgeBackend, GaleRidgeStrategy}
import cats.Id
import cats.data.EitherT
import gale.backend.PureBackend
import gale.linalg.{DMat, Matrix}
import multivar.core.SemanticSpace
import resample4s.core.{Coverage, DigestAlgorithm, Labels, PlanReceipt, UnitKey}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, CrossFitDesign, EvidenceError, ValidationDesign}

/** Distinct, finite, non-negative penalties searched with one shared value for
  * every response column. Grid order is the tie-break order.
  */
final class RidgePenaltyGrid private (val penalties: Vector[Double])
object RidgePenaltyGrid:
  def apply(penalties: Vector[Double]): Either[AlderRidgeRegressionError, RidgePenaltyGrid] =
    if penalties.isEmpty then Left(AlderRidgeRegressionError.InvalidPenaltyGrid("grid is empty"))
    else if penalties.exists(value => !value.isFinite || value < 0.0) then Left(AlderRidgeRegressionError.InvalidPenaltyGrid("penalties must be finite and non-negative"))
    else if penalties.distinct.length != penalties.length then Left(AlderRidgeRegressionError.InvalidPenaltyGrid("penalties must be distinct"))
    else Right(new RidgePenaltyGrid(penalties))

/** Admission budget on planned ridge solve work, checked with exact
  * arithmetic before the first solve. A solve on `n` rows and `p` prepared
  * features is counted as `(2n + p) p` cells: the declared shapes of the
  * provider's problem design and its augmented QR system. It is a planned
  * shape/work budget, not a peak or total allocation bound: Gale builder
  * snapshots, QR copies, vectors, per-row reads and encoder preparation are
  * not counted.
  */
final class RidgeSolveBudget private (val maximumDesignCells: Long)
object RidgeSolveBudget:
  def apply(maximumDesignCells: Long): Either[AlderRidgeRegressionError, RidgeSolveBudget] =
    if maximumDesignCells <= 0L then Left(AlderRidgeRegressionError.InvalidBudget) else Right(new RidgeSolveBudget(maximumDesignCells))

/** Execution route. Only the bounded materialized provider route exists; a
  * matrix-native request is refused rather than silently materialized.
  */
enum RidgeExecution:
  case BoundedMaterialized
  case MatrixNative

/** Tuning uses a fixed target metric. Uniform coordinates preserve the pooled
  * cell mean; declared blocks use a mass-balanced target mean per assessment
  * appearance. The same positive target weights apply to residual and penalty
  * terms in the separable objective, so fixed-lambda provider fits are unchanged.
  * Residual-only weights or per-output penalties are different estimands.
  */
enum RidgeSelectionLoss:
  case PooledMeanSquaredError
  case TargetWeightedMeanSquaredError

/** Policy for a response column that is constant over any training set a
  * ridge is fit on: inner analysis sets during selection, and the outer (or
  * refit) training set. With an intercept, ridge then predicts that constant;
  * `Record` accepts and reports it on every fit receipt, `Refuse` fails before
  * the first solve.
  */
enum ConstantTargetPolicy:
  case Refuse
  case Record

/** A metric value, or the typed reason it does not exist. No R-squared is
  * reported for a constant reference, no metric for an empty assessment, and
  * no value when the inputs or the arithmetic are not finite.
  */
enum RegressionMetric:
  case Defined(value: Double)
  case UndefinedConstantReference
  case UndefinedNoAssessment
  case UndefinedNonFinite

final case class RegressionOutputAssessment(target: String, assessed: Int, meanSquaredError: RegressionMetric, rSquared: RegressionMetric)
final case class RegressionPooledAssessment(assessed: Int, outputs: Int, meanSquaredError: RegressionMetric, rSquared: RegressionMetric, targetGeometry: Option[RidgeTargetGeometry] = None):
  def loss: RidgeSelectionLoss = targetGeometry.map(_.origin) match
    case Some(RidgeTargetMetricOrigin.FixedDeclared(_)) => RidgeSelectionLoss.TargetWeightedMeanSquaredError
    case _ => RidgeSelectionLoss.PooledMeanSquaredError

/** Out-of-fold regression metrics. R-squared is `1 - SSE / SST`, with SST taken
  * about the mean of the assessed observations themselves, so it can be
  * negative. It is undefined when every assessed observation is identical.
  * The pooled R-squared is `1 - sum SSE / sum SST` over outputs and is
  * undefined if any output's reference is constant. Inputs must be aligned:
  * every observed/predicted pair, and every pooled column, has one length.
  */
object RegressionAssessment:
  def output(target: String, observed: Vector[Double], predicted: Vector[Double]): RegressionOutputAssessment =
    require(observed.length == predicted.length, "observed and predicted lengths differ")
    val n = observed.length
    if n == 0 then RegressionOutputAssessment(target, 0, RegressionMetric.UndefinedNoAssessment, RegressionMetric.UndefinedNoAssessment)
    else if !allFinite(observed) || !allFinite(predicted) then RegressionOutputAssessment(target, n, RegressionMetric.UndefinedNonFinite, RegressionMetric.UndefinedNonFinite)
    else
      val sse = squaredError(observed, predicted)
      val r2 =
        if observed.forall(_ == observed.head) then RegressionMetric.UndefinedConstantReference
        else finite(1.0 - sse / totalSquares(observed))
      RegressionOutputAssessment(target, n, finite(sse / n.toDouble), r2)

  def pooled(columns: Vector[(Vector[Double], Vector[Double])]): RegressionPooledAssessment =
    require(columns.forall((observed, predicted) => observed.length == predicted.length), "observed and predicted lengths differ")
    require(columns.map(_._1.length).distinct.length <= 1, "pooled columns must have one row count")
    val rows = columns.headOption.fold(0)(_._1.length)
    if rows == 0 then RegressionPooledAssessment(0, columns.length, RegressionMetric.UndefinedNoAssessment, RegressionMetric.UndefinedNoAssessment)
    else if columns.exists((observed, predicted) => !allFinite(observed) || !allFinite(predicted)) then
      RegressionPooledAssessment(rows, columns.length, RegressionMetric.UndefinedNonFinite, RegressionMetric.UndefinedNonFinite)
    else
      val sse = columns.map((observed, predicted) => squaredError(observed, predicted)).sum
      val r2 =
        if columns.exists((observed, _) => observed.forall(_ == observed.head)) then RegressionMetric.UndefinedConstantReference
        else finite(1.0 - sse / columns.map((observed, _) => totalSquares(observed)).sum)
      RegressionPooledAssessment(rows, columns.length, finite(sse / (rows.toDouble * columns.length.toDouble)), r2)

  /** Fixed target-utility assessment. Each target's SST is centered separately;
    * as in the uniform API, any constant target makes pooled R-squared undefined.
    */
  def pooled(columns: Vector[(Vector[Double], Vector[Double])], geometry: RidgeTargetGeometry): RegressionPooledAssessment =
    require(columns.length == geometry.targets.length, "target geometry arity differs")
    require(columns.forall((observed, predicted) => observed.length == predicted.length), "observed and predicted lengths differ")
    require(columns.map(_._1.length).distinct.length <= 1, "pooled columns must have one row count")
    val rows = columns.head._1.length
    if rows == 0 then RegressionPooledAssessment(0, columns.length, RegressionMetric.UndefinedNoAssessment, RegressionMetric.UndefinedNoAssessment, Some(geometry))
    else if columns.exists((observed, predicted) => !allFinite(observed) || !allFinite(predicted)) then
      RegressionPooledAssessment(rows, columns.length, RegressionMetric.UndefinedNonFinite, RegressionMetric.UndefinedNonFinite, Some(geometry))
    else
      val sse = weightedSquares(columns, geometry, centeredReference = false)
      val r2 =
        if columns.exists((observed, _) => observed.forall(_ == observed.head)) then RegressionMetric.UndefinedConstantReference
        else finite(1.0 - sse / weightedSquares(columns, geometry, centeredReference = true))
      RegressionPooledAssessment(rows, columns.length, finite(sse / (rows.toDouble * geometry.normalizedMass)), r2, Some(geometry))

  private def weightedSquares(columns: Vector[(Vector[Double], Vector[Double])], geometry: RidgeTargetGeometry, centeredReference: Boolean): Double =
    var sum = 0.0
    var column = 0
    while column < columns.length do
      val (observed, predicted) = columns(column)
      val reference = if centeredReference then observed.sum / observed.length.toDouble else 0.0
      val scale = math.sqrt(geometry.normalizedWeights(column))
      var row = 0
      while row < observed.length do
        val residual = observed(row) - (if centeredReference then reference else predicted(row))
        val scaled = scale * residual
        sum += scaled * scaled
        row += 1
      column += 1
    sum

  private def finite(value: Double): RegressionMetric =
    if value.isFinite then RegressionMetric.Defined(value) else RegressionMetric.UndefinedNonFinite

  private def allFinite(values: Vector[Double]): Boolean =
    values.forall(_.isFinite)

  private def squaredError(observed: Vector[Double], predicted: Vector[Double]): Double =
    var sum = 0.0
    var row = 0
    while row < observed.length do
      val residual = observed(row) - predicted(row)
      sum += residual * residual
      row += 1
    sum

  private def totalSquares(observed: Vector[Double]): Double =
    val mean = observed.sum / observed.length.toDouble
    observed.iterator.map(value => (value - mean) * (value - mean)).sum

/** Target-aware preparation, cross-fitted so no prepared row sees its own
  * target. Preparation is refitted inside every training scope that is
  * scored: each inner analysis set during selection, then the outer (or
  * refit) training set for the final model. Rows outside a scope never reach
  * its encoder.
  */
final class RidgeCrossFitPreparation[S <: SemanticSpace, K, M](
    val design: CrossFitDesign[S, K],
    val encoder: FoldEncoder[Id, Array[Double], Array[Double], M, Array[Double]]
)

/** Exact-once preparation coverage over one training population, recorded
  * separately from validation coverage. `design` is the parent cross-fit
  * design; `assignment` is the executed restriction of it to this population,
  * relabelled densely in parent fold order.
  */
final case class RidgeCrossFitReceipt(
    design: PlanReceipt,
    population: DataFingerprint,
    retainedUnits: Vector[UnitKey],
    assignment: Vector[(String, Int)]
)

/** One inner unit restricted to a training scope. Predictions are indexed
  * `[penalty][assessment row][output]`, made by a workflow whose preparation
  * and ridge fits saw only `analysisKeys`.
  */
final case class RidgeInnerUnitReceipt(
    unit: UnitKey,
    analysisKeys: Vector[String],
    assessmentKeys: Vector[String],
    preparation: Option[RidgeCrossFitReceipt],
    constantTrainingTargets: Vector[Boolean],
    normalizedSquaredErrorByPenalty: Vector[Double],
    predictedByPenalty: Vector[Vector[Vector[Double]]],
    auditByPenalty: Vector[Audit]
)

/** Training-only penalty selection over raw rows of `trainingKeys`. */
final case class RidgeSelectionReceipt(
    innerDesign: PlanReceipt,
    targetGeometry: RidgeTargetGeometry,
    loss: RidgeSelectionLoss,
    penalties: Vector[Double],
    pooledLossByPenalty: Vector[Double],
    selectedPenalty: Double,
    assessmentAppearances: Long,
    normalizedSquaredErrorByPenalty: Vector[Double],
    lossDenominator: Double,
    trainingKeys: Vector[String],
    units: Vector[RidgeInnerUnitReceipt],
    skippedUnits: Vector[UnitKey]
)

/** One provider scalar solve per response column, with its audit verbatim. */
final case class RidgeOutputFit(target: String, solution: RidgeSolution, audit: Audit, constantTrainingTarget: Boolean)

final case class RidgeSolveReceipt(execution: RidgeExecution, plannedSolves: Long, plannedDesignCells: Long, maximumDesignCells: Long)

final case class RidgePrediction(targetGeometry: RidgeTargetGeometry, values: Vector[Double]):
  require(values.length == targetGeometry.targets.length, "prediction target arity differs")
  def targets: Vector[String] = targetGeometry.targets
  def responseAxis: AxisDescriptor = targetGeometry.responseAxis

/** Separable multiresponse ridge: one Alder scalar model per response column,
  * all at one fixed penalty. Weights stay in prepared-feature coordinates.
  */
final class SeparableRidgeModel private[predictive] (
    val targetGeometry: RidgeTargetGeometry,
    val features: Vector[String],
    val penalty: Double,
    val outputs: Vector[RidgeOutputFit],
    models: Vector[RidgeModel[Array[Double]]],
    stage: StagePath
) extends Pipe[Array[Double], AlderRidgeRegressionError, Array[Double]]:
  def targets: Vector[String] = targetGeometry.targets
  def responseAxis: AxisDescriptor = targetGeometry.responseAxis
  def predict(input: Array[Double]): Either[AlderRidgeRegressionError, RidgePrediction] =
    run(input).left.map(_.cause).map(values => RidgePrediction(targetGeometry, values.toVector))

  def run(input: Array[Double]): Either[Failure[AlderRidgeRegressionError], Array[Double]] =
    val values = new Array[Double](models.length)
    var output = 0
    var failure: Option[Failure[AlderRidgeRegressionError]] = None
    while output < models.length && failure.isEmpty do
      models(output).run(input) match
        case Left(error) => failure = Some(stage.failure[AlderRidgeRegressionError](AlderRidgeRegressionError.Provider("ridge-predict", error.cause.toString)))
        case Right(value) => values(output) = value
      output += 1
    failure.toLeft(values)

  def weights(target: String): Option[Vector[(String, Double)]] =
    outputs.find(_.target == target).map(fit => features.zipWithIndex.map((name, index) => name -> fit.solution.coefficient(index)))

  def intercept(target: String): Option[Double] =
    outputs.find(_.target == target).map(_.solution.intercept)

final case class RidgeOofRow(stableKey: String, outerUnit: UnitKey, targetGeometry: RidgeTargetGeometry, observed: Vector[Double], predicted: Vector[Double]):
  require(observed.length == targetGeometry.targets.length && predicted.length == targetGeometry.targets.length, "OOF target arity differs")
  def targets: Vector[String] = targetGeometry.targets
  def responseAxis: AxisDescriptor = targetGeometry.responseAxis

final case class RidgeOuterFold(
    unit: UnitKey,
    trainingKeys: Vector[String],
    assessmentKeys: Vector[String],
    selection: RidgeSelectionReceipt,
    model: SeparableRidgeModel,
    crossFit: Option[RidgeCrossFitReceipt],
    audit: Audit
)

final case class AlderRidgeRegressionResult(
    sampleAxis: AxisDescriptor,
    targetGeometry: RidgeTargetGeometry,
    features: Vector[String],
    predictions: DMat,
    rows: Vector[RidgeOofRow],
    outerDesign: PlanReceipt,
    innerDesign: PlanReceipt,
    crossFitDesign: Option[PlanReceipt],
    folds: Vector[RidgeOuterFold],
    outputs: Vector[RegressionOutputAssessment],
    pooled: RegressionPooledAssessment,
    solve: RidgeSolveReceipt,
    materialization: MaterializationReceipt,
    nativeRead: Option[NativeReadReceipt]
):
  def responseAxis: AxisDescriptor = targetGeometry.responseAxis
  def targets: Vector[String] = targetGeometry.targets

/** Refitting on every row is a separate, explicitly declared use. It carries
  * no out-of-fold metric. This is a domain refit receipt: the provider fit is
  * an ordinary training fit, not Alder's application-level `Use.Refit`
  * promotion, which is not admitted here.
  */
enum RidgeRefitAuthorization:
  case Declared(reason: String)

final case class RidgeRefitReceipt(
    reason: String,
    training: DataFingerprint,
    trainingKeys: Vector[String],
    selection: RidgeSelectionReceipt,
    crossFit: Option[RidgeCrossFitReceipt],
    solve: RidgeSolveReceipt,
    materialization: MaterializationReceipt,
    nativeRead: Option[NativeReadReceipt]
)

final class AlderRidgeRefit private[predictive] (
    val sampleAxis: AxisDescriptor,
    val model: SeparableRidgeModel,
    val receipt: RidgeRefitReceipt,
    val audit: Audit,
    serve: Array[Double] => Either[AlderRidgeRegressionError, Array[Double]]
):
  def targetGeometry: RidgeTargetGeometry = model.targetGeometry
  def responseAxis: AxisDescriptor = targetGeometry.responseAxis
  def predict(input: Array[Double]): Either[AlderRidgeRegressionError, RidgePrediction] =
    serve(input).map(values => RidgePrediction(targetGeometry, values.toVector))

enum AlderRidgeRegressionError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case InvalidPenaltyGrid(detail: String)
  case InvalidBudget
  case SolveOverBudget(plannedCells: Long, maximumCells: Long)
  case SolvePlanOverflow
  case NonFiniteSelectionLoss(scope: String, penalty: Double)
  case InvalidFeatureNames(detail: String)
  case FeatureCountMismatch(expected: Int, actual: Int)
  case ResponseCountMismatch(expected: Int, actual: Int)
  case ResponseAxisMismatch(expected: String, actual: String)
  case DesignAxisMismatch(role: String, expected: String, actual: String)
  case SharedOuterInnerDesign
  case TargetShape(stableKey: String, expected: Int, actual: Int)
  case NoInnerAssessment(scope: String)
  case ConstantTrainingTarget(target: String, scope: String)
  case CrossFitTooFewFolds(scope: String, folds: Int)
  case CrossFitCoverage(scope: String, detail: String)
  case ProviderSelection(detail: String)
  case Provider(stage: String, detail: String)
  case Workflow(detail: String)
  case DuplicateAssessment(stableKey: String)
  case MissingAssessment(stableKey: String)
  case InvalidRefitAuthorization
  case InvalidTargetGeometry(detail: String)
  case TargetGeometryAxisMismatch(expected: String, actual: String)

  def message: String = this match
    case Admission(error) => s"admission: $error"
    case Evidence(error) => s"evidence: ${error.message}"
    case InvalidPenaltyGrid(detail) => s"invalid penalty grid: $detail"
    case InvalidBudget => "solve budget must be positive"
    case SolveOverBudget(planned, maximum) => s"planned $planned design cells exceed the budget of $maximum"
    case SolvePlanOverflow => "planned design cells overflow a 64-bit count"
    case NonFiniteSelectionLoss(scope, penalty) => s"inner selection loss for penalty $penalty in $scope is not finite"
    case InvalidFeatureNames(detail) => s"invalid prepared feature names: $detail"
    case FeatureCountMismatch(expected, actual) => s"expected $expected prepared features, got $actual"
    case ResponseCountMismatch(expected, actual) => s"expected $expected response columns, got $actual"
    case ResponseAxisMismatch(expected, actual) => s"response axis $actual does not match $expected"
    case DesignAxisMismatch(role, expected, actual) => s"$role design axis $actual does not match $expected"
    case SharedOuterInnerDesign => "outer and inner validation must be distinct designs"
    case TargetShape(key, expected, actual) => s"row $key has $actual targets, expected $expected"
    case NoInnerAssessment(scope) => s"no inner unit has both analysis and assessment rows in $scope"
    case ConstantTrainingTarget(target, scope) => s"target $target is constant over the training rows of $scope"
    case CrossFitTooFewFolds(scope, folds) => s"cross-fit preparation in $scope retains $folds fold(s); at least 2 are required"
    case CrossFitCoverage(scope, detail) => s"cross-fit coverage in $scope: $detail"
    case ProviderSelection(detail) => s"provider row selection: $detail"
    case Provider(stage, detail) => s"provider $stage: $detail"
    case Workflow(detail) => s"workflow: $detail"
    case DuplicateAssessment(key) => s"row $key was assessed twice"
    case MissingAssessment(key) => s"row $key was never assessed"
    case InvalidRefitAuthorization => "refit requires a non-blank declared reason"
    case InvalidTargetGeometry(detail) => s"invalid fixed target geometry: $detail"
    case TargetGeometryAxisMismatch(expected, actual) => s"target metric axis $actual does not match $expected"

object AlderRidgeRegression:
  private type Row[M] = Example[Array[Double], Array[Double], M]
  private type ScalarRow[M] = Example[Array[Double], Double, M]
  private type PreparedWorkflow[M] = (FoldEncoder[Id, Array[Double], Array[Double], M, Array[Double]], CompleteResampler[Row[M]])

  private[predictive] final case class InnerUnit(key: UnitKey, analysis: Vector[Long], assessment: Vector[Long])

  private[predictive] final case class Common(targetGeometry: RidgeTargetGeometry, view: ArrayFeatureView, keyOf: Map[Long, String]):
    def targets: Vector[String] = targetGeometry.targets

  private final case class FittedWorkflow(serve: Array[Double] => Either[AlderRidgeRegressionError, Array[Double]], model: SeparableRidgeModel, audit: Audit)

  private val backend = new GaleRidgeBackend[Id](PureBackend, GaleRidgeStrategy.AugmentedQR, NumericMode.Deterministic)

  /** Nested cross-validated separable ridge. The outer design produces the
    * out-of-fold predictions. Within each outer fold, one shared penalty is
    * selected on raw outer-training rows by the inner design restricted to
    * them; every inner unit refits its own preparation (if any) on its raw
    * analysis rows only. The final fixed-penalty model is then fit through
    * preparation cross-fitted on the outer training rows. Every numerical
    * solve is the provider's dense Gale ridge.
    */
  def crossValidate[S <: SemanticSpace, K, C <: Coverage.Exact, R, M](
      rows: AlderMaterializedRows[M],
      outer: ValidationDesign[S, K, Coverage.ExactOnce],
      inner: ValidationDesign[S, K, C],
      responseAxis: AxisRef[R],
      features: Vector[String],
      grid: RidgePenaltyGrid,
      budget: RidgeSolveBudget,
      preparation: Option[RidgeCrossFitPreparation[S, K, M]] = None,
      constantTargets: ConstantTargetPolicy = ConstantTargetPolicy.Refuse,
      execution: RidgeExecution = RidgeExecution.BoundedMaterialized,
      targetGeometry: Option[RidgeTargetGeometry] = None
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, AlderRidgeRegressionResult] =
    for
      common <- validate(rows, outer.samples.descriptor, responseAxis, features, preparation.isDefined, execution, targetGeometry)
      _ <- if outer eq inner then Left(AlderRidgeRegressionError.SharedOuterInnerDesign) else Right(())
      _ <- sameAxis("inner", outer.samples.descriptor, inner.samples.descriptor)
      outerUnits <- units(rows, outer.keys, key => outer.at(key).map(unit => (unit.analysis.ordinals.toVector, unit.assessment.ordinals.toVector)))
      innerUnits <- units(rows, inner.keys, key => inner.at(key).map(unit => (unit.analysis.ordinals.toVector, unit.assessment.ordinals.toVector)))
      foldOf <- preparation.fold[Either[AlderRidgeRegressionError, Map[Long, Int]]](Right(Map.empty))(prep => crossFitAssignment(rows, prep))
      solve <- plan(outerUnits.map(_.analysis), innerUnits, grid, common.targets.length, features.length, budget)
      _ <- precheck(rows, outerUnits.map(unit => (s"outer unit ${unit.key.repeat}/${unit.key.fold}", unit.analysis)), innerUnits, preparation.isDefined, foldOf, constantTargets, common)
      folds <- traverse(outerUnits)(unit => outerFold(rows, unit, innerUnits, inner.receipt, outer.receipt, grid, common, preparation, foldOf, constantTargets))
      result <- assemble(rows, outer.receipt, inner.receipt, preparation.map(_.design.receipt), features, common, folds, solve)
    yield result

  /** Refits on every row under an explicit authorization: the shared penalty
    * is selected with the inner design over raw rows, then the final model is
    * fit through preparation cross-fitted on all rows.
    */
  def refit[S <: SemanticSpace, K, C <: Coverage.Exact, R, M](
      rows: AlderMaterializedRows[M],
      inner: ValidationDesign[S, K, C],
      responseAxis: AxisRef[R],
      features: Vector[String],
      grid: RidgePenaltyGrid,
      budget: RidgeSolveBudget,
      authorization: RidgeRefitAuthorization,
      preparation: Option[RidgeCrossFitPreparation[S, K, M]] = None,
      constantTargets: ConstantTargetPolicy = ConstantTargetPolicy.Refuse,
      execution: RidgeExecution = RidgeExecution.BoundedMaterialized,
      targetGeometry: Option[RidgeTargetGeometry] = None
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, AlderRidgeRefit] =
    val reason = authorization match
      case RidgeRefitAuthorization.Declared(value) => value
    val all = rows.mapping.nativeIds
    for
      _ <- if reason.trim.isEmpty then Left(AlderRidgeRegressionError.InvalidRefitAuthorization) else Right(())
      common <- validate(rows, inner.samples.descriptor, responseAxis, features, preparation.isDefined, execution, targetGeometry)
      innerUnits <- units(rows, inner.keys, key => inner.at(key).map(unit => (unit.analysis.ordinals.toVector, unit.assessment.ordinals.toVector)))
      foldOf <- preparation.fold[Either[AlderRidgeRegressionError, Map[Long, Int]]](Right(Map.empty))(prep => crossFitAssignment(rows, prep))
      solve <- plan(Vector(all), innerUnits, grid, common.targets.length, features.length, budget)
      _ <- precheck(rows, Vector(("refit", all)), innerUnits, preparation.isDefined, foldOf, constantTargets, common)
      selection <- select(rows, all, innerUnits, inner.receipt, grid, common, preparation, foldOf, constantTargets, "refit")
      train <- rows.root.training(all).left.map(error => AlderRidgeRegressionError.ProviderSelection(error.toString))
      prepared <- prepare(preparation, all, train, foldOf, common, "refit")
      learner = new SeparableRidgeLearner[M](selection.selectedPenalty, common, constantTargets == ConstantTargetPolicy.Refuse, "refit")
      fitted <- fitWorkflow(train, learner, prepared.map((encoder, resampler, _) => (encoder, resampler)), context("scalafim.alder-ridge.refit.v1", rows, inner.receipt, None, common.targetGeometry))
    yield new AlderRidgeRefit(rows.mapping.axis, fitted.model,
      RidgeRefitReceipt(reason, train.fingerprint, all.map(common.keyOf), selection, prepared.map(_._3), solve, rows.receipt, rows.nativeReadReceipt),
      fitted.audit, fitted.serve)

  private def validate[R, M](rows: AlderMaterializedRows[M], samples: AxisDescriptor, responseAxis: AxisRef[R], features: Vector[String], prepared: Boolean, execution: RidgeExecution, targetGeometry: Option[RidgeTargetGeometry]): Either[AlderRidgeRegressionError, Common] =
    for
      _ <- execution match
        case RidgeExecution.MatrixNative => AlderPredictiveAdmission.rejectMatrixNativeRidge.left.map(AlderRidgeRegressionError.Admission.apply)
        case RidgeExecution.BoundedMaterialized => Right(())
      _ <- NativeAxisMapping.verify(samples, rows.mapping, rows.mapping.declaredSource).left.map(AlderRidgeRegressionError.Admission.apply)
      _ <- if rows.root.ids == rows.mapping.nativeIds then Right(()) else Left(AlderRidgeRegressionError.Admission(AlderPredictiveAdmissionError.CrossFitPopulationMismatch))
      _ <- if responseAxis.size == rows.receipt.targets then Right(()) else Left(AlderRidgeRegressionError.ResponseCountMismatch(rows.receipt.targets, responseAxis.size))
      _ <- rows.nativeReadReceipt match
        case Some(read) if read.targetsIdentity.columns != responseAxis.descriptor =>
          Left(AlderRidgeRegressionError.ResponseAxisMismatch(read.targetsIdentity.columns.stableKey, responseAxis.descriptor.stableKey))
        case _ => Right(())
      _ <- if prepared || features.length == rows.receipt.inputs then Right(()) else Left(AlderRidgeRegressionError.FeatureCountMismatch(rows.receipt.inputs, features.length))
      geometry <- targetGeometry.fold(RidgeTargetGeometry.uniform(responseAxis))(Right(_))
      _ <- if geometry.responseAxis == responseAxis.descriptor then Right(()) else Left(AlderRidgeRegressionError.TargetGeometryAxisMismatch(responseAxis.descriptor.stableKey, geometry.responseAxis.stableKey))
      schema <- FeatureSchema.named[ArrayFeatureView](IArray.from(features)).left.map(error => AlderRidgeRegressionError.InvalidFeatureNames(error.toString))
    yield Common(geometry, new ArrayFeatureView(schema), rows.mapping.entriesByOrdinal.map(entry => entry.nativeId -> entry.stableKey).toMap)

  private def sameAxis(role: String, expected: AxisDescriptor, actual: AxisDescriptor): Either[AlderRidgeRegressionError, Unit] =
    if expected == actual then Right(()) else Left(AlderRidgeRegressionError.DesignAxisMismatch(role, expected.stableKey, actual.stableKey))

  private def units[M](rows: AlderMaterializedRows[M], keys: IndexedSeq[UnitKey], legs: UnitKey => Either[EvidenceError, (Vector[Int], Vector[Int])]): Either[AlderRidgeRegressionError, Vector[InnerUnit]] =
    traverse(keys.toVector)(key =>
      legs(key).left.map(AlderRidgeRegressionError.Evidence.apply).map((analysis, assessment) =>
        InnerUnit(key, analysis.map(rows.mapping.nativeIds), assessment.map(rows.mapping.nativeIds))))

  /** Validates the full cross-fit design as an exact-once partition of the
    * root (independently of any validation design) and returns each native
    * row's preparation fold.
    */
  private def crossFitAssignment[S <: SemanticSpace, K, M](rows: AlderMaterializedRows[M], prep: RidgeCrossFitPreparation[S, K, M])(using DigestAlgorithm): Either[AlderRidgeRegressionError, Map[Long, Int]] =
    for
      _ <- AlderPredictiveAdmission.crossFit(rows, prep.design).left.map(AlderRidgeRegressionError.Admission.apply)
      assessed <- traverse(prep.design.keys.toVector.zipWithIndex)((key, fold) =>
        prep.design.at(key).left.map(AlderRidgeRegressionError.Evidence.apply).map(unit => unit.assessment.ordinals.toVector.map(ordinal => rows.mapping.nativeIds(ordinal) -> fold)))
    yield assessed.flatten.toMap

  private def plan(trainingSets: Vector[Vector[Long]], innerUnits: Vector[InnerUnit], grid: RidgePenaltyGrid, outputs: Int, features: Int, budget: RidgeSolveBudget): Either[AlderRidgeRegressionError, RidgeSolveReceipt] =
    val counts = trainingSets.map: training =>
      val present = training.toSet
      val analysis = innerUnits.collect { case unit if unit.analysis.exists(present.contains) && unit.assessment.exists(present.contains) => unit.analysis.count(present.contains) }
      (training.length, analysis)
    planCounts(counts, grid.penalties.length, outputs, features, budget)

  /** Exact solve-plan arithmetic over `(training rows, usable inner analysis
    * row counts)` per training scope. Overflow is refused, never wrapped.
    */
  private[predictive] def planCounts(scopes: Vector[(Int, Vector[Int])], penalties: Int, outputs: Int, features: Int, budget: RidgeSolveBudget): Either[AlderRidgeRegressionError, RidgeSolveReceipt] =
    try
      var cells = 0L
      var solves = 0L
      scopes.foreach: (training, analysis) =>
        analysis.foreach: rowsInUnit =>
          val unitSolves = Math.multiplyExact(penalties.toLong, outputs.toLong)
          cells = Math.addExact(cells, Math.multiplyExact(unitSolves, solveCells(rowsInUnit, features)))
          solves = Math.addExact(solves, unitSolves)
        cells = Math.addExact(cells, Math.multiplyExact(outputs.toLong, solveCells(training, features)))
        solves = Math.addExact(solves, outputs.toLong)
      if cells > budget.maximumDesignCells then Left(AlderRidgeRegressionError.SolveOverBudget(cells, budget.maximumDesignCells))
      else Right(RidgeSolveReceipt(RidgeExecution.BoundedMaterialized, solves, cells, budget.maximumDesignCells))
    catch case _: ArithmeticException => Left(AlderRidgeRegressionError.SolvePlanOverflow)

  private def solveCells(rows: Int, features: Int): Long =
    Math.multiplyExact(Math.addExact(Math.multiplyExact(2L, rows.toLong), features.toLong), features.toLong)

  /** Refusals that need no solve, checked before the first solve: too few
    * cross-fit folds in any scope that will be prepared, and, under `Refuse`,
    * a constant target in any scope a ridge will be fit on (each outer or
    * refit training set and each usable inner analysis set within it).
    */
  private def precheck[M](
      rows: AlderMaterializedRows[M],
      scopes: Vector[(String, Vector[Long])],
      innerUnits: Vector[InnerUnit],
      prepared: Boolean,
      foldOf: Map[Long, Int],
      constantTargets: ConstantTargetPolicy,
      common: Common
  ): Either[AlderRidgeRegressionError, Unit] =
    val targets = rows.root.training(rows.mapping.nativeIds).toOption.fold(Map.empty[Long, Array[Double]])(data =>
      data.data.foldRows(Map.empty[Long, Array[Double]])((seen, id, example) => seen.updated(id.value, example.target)))
    def tooFewFolds(label: String, ids: Vector[Long]): Option[AlderRidgeRegressionError] =
      val retained = ids.map(foldOf).distinct.length
      if retained < 2 then Some(AlderRidgeRegressionError.CrossFitTooFewFolds(label, retained)) else None
    def constantOutput(scope: String, training: Vector[Long]): Option[AlderRidgeRegressionError] =
      val values = training.flatMap(targets.get)
      common.targets.indices.find(output => values.nonEmpty && values.forall(target => target.length > output && target(output) == values.head(output)))
        .map(output => AlderRidgeRegressionError.ConstantTrainingTarget(common.targets(output), scope))
    def scopeFailure(scope: String, training: Vector[Long]): Option[AlderRidgeRegressionError] =
      val present = training.toSet
      val innerScopes = innerUnits.collect { case unit if unit.analysis.exists(present.contains) && unit.assessment.exists(present.contains) =>
        (s"$scope inner unit ${unit.key.repeat}/${unit.key.fold}", unit.analysis.filter(present.contains)) }
      val folds = if prepared then ((scope, training) +: innerScopes).view.flatMap((label, ids) => tooFewFolds(label, ids)).headOption else None
      folds.orElse(
        if constantTargets == ConstantTargetPolicy.Refuse then ((scope, training) +: innerScopes).view.flatMap((label, ids) => constantOutput(label, ids)).headOption
        else None)
    scopes.view.flatMap((scope, training) => scopeFailure(scope, training)).headOption.toLeft(())

  private def outerFold[S <: SemanticSpace, K, M](
      rows: AlderMaterializedRows[M],
      unit: InnerUnit,
      innerUnits: Vector[InnerUnit],
      innerReceipt: PlanReceipt,
      outerReceipt: PlanReceipt,
      grid: RidgePenaltyGrid,
      common: Common,
      preparation: Option[RidgeCrossFitPreparation[S, K, M]],
      foldOf: Map[Long, Int],
      constantTargets: ConstantTargetPolicy
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, (RidgeOuterFold, Vector[RidgeOofRow])] =
    val scope = s"outer unit ${unit.key.repeat}/${unit.key.fold}"
    for
      selection <- select(rows, unit.analysis, innerUnits, innerReceipt, grid, common, preparation, foldOf, constantTargets, scope)
      split <- rows.fixedHoldout(unit.analysis, unit.assessment, FixedCoverage.DeclaredSubset).left.map(AlderRidgeRegressionError.Admission.apply)
      prepared <- prepare(preparation, unit.analysis, split.train, foldOf, common, scope)
      learner = new SeparableRidgeLearner[M](selection.selectedPenalty, common, constantTargets == ConstantTargetPolicy.Refuse, scope)
      fitted <- fitWorkflow(split.train, learner, prepared.map((encoder, resampler, _) => (encoder, resampler)), context("scalafim.alder-ridge.outer.v1", rows, outerReceipt, Some(unit.key), common.targetGeometry))
      assessed <- predict(split.test.data, fitted, unit.key, common)
    yield (RidgeOuterFold(unit.key, unit.analysis.map(common.keyOf), unit.assessment.map(common.keyOf), selection, fitted.model, prepared.map(_._3), fitted.audit), assessed)

  /** Selects one shared penalty from raw `trainIds` rows only. Each usable
    * inner unit (restricted to `trainIds`) fits its own workflow on its raw
    * analysis rows, with preparation cross-fitted inside those rows, and is
    * scored on its raw assessment rows.
    */
  private def select[S <: SemanticSpace, K, M](
      rows: AlderMaterializedRows[M],
      trainIds: Vector[Long],
      innerUnits: Vector[InnerUnit],
      innerReceipt: PlanReceipt,
      grid: RidgePenaltyGrid,
      common: Common,
      preparation: Option[RidgeCrossFitPreparation[S, K, M]],
      foldOf: Map[Long, Int],
      constantTargets: ConstantTargetPolicy,
      scope: String
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, RidgeSelectionReceipt] =
    val present = trainIds.toSet
    val restricted = innerUnits.map(unit => InnerUnit(unit.key, unit.analysis.filter(present.contains), unit.assessment.filter(present.contains)))
    val (usable, skipped) = restricted.partition(unit => unit.analysis.nonEmpty && unit.assessment.nonEmpty)
    if usable.isEmpty then Left(AlderRidgeRegressionError.NoInnerAssessment(scope))
    else
      for
        scored <- traverse(usable)(unit => scoreUnit(rows, unit, innerReceipt, grid, common, preparation, foldOf, constantTargets, s"$scope inner unit ${unit.key.repeat}/${unit.key.fold}"))
        appearances <-
          try Right(usable.foldLeft(0L)((total, unit) => Math.addExact(total, unit.assessment.length.toLong)))
          catch case _: ArithmeticException => Left(AlderRidgeRegressionError.SolvePlanOverflow)
        squared = grid.penalties.indices.toVector.map(penalty => scored.map(_.normalizedSquaredErrorByPenalty(penalty)).sum)
        denominator = appearances.toDouble * common.targetGeometry.normalizedMass
        pooled = squared.map(_ / denominator)
        _ <- grid.penalties.zip(pooled).collectFirst { case (penalty, loss) if !loss.isFinite => AlderRidgeRegressionError.NonFiniteSelectionLoss(scope, penalty) }.toLeft(())
      yield
        val loss = common.targetGeometry.origin match
          case RidgeTargetMetricOrigin.UniformCoordinates => RidgeSelectionLoss.PooledMeanSquaredError
          case RidgeTargetMetricOrigin.FixedDeclared(_) => RidgeSelectionLoss.TargetWeightedMeanSquaredError
        RidgeSelectionReceipt(innerReceipt, common.targetGeometry, loss, grid.penalties, pooled,
          grid.penalties(pooled.indexOf(pooled.min)), appearances, squared, denominator, trainIds.map(common.keyOf), scored, skipped.map(_.key))

  private def scoreUnit[S <: SemanticSpace, K, M](
      rows: AlderMaterializedRows[M],
      unit: InnerUnit,
      innerReceipt: PlanReceipt,
      grid: RidgePenaltyGrid,
      common: Common,
      preparation: Option[RidgeCrossFitPreparation[S, K, M]],
      foldOf: Map[Long, Int],
      constantTargets: ConstantTargetPolicy,
      scope: String
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, RidgeInnerUnitReceipt] =
    for
      split <- rows.fixedValidation(unit.analysis, unit.assessment, FixedCoverage.DeclaredSubset).left.map(AlderRidgeRegressionError.Admission.apply)
      prepared <- prepare(preparation, unit.analysis, split.train, foldOf, common, scope)
      byPenalty <- traverse(grid.penalties)(penalty =>
        val learner = new SeparableRidgeLearner[M](penalty, common, constantTargets == ConstantTargetPolicy.Refuse, scope)
        for
          fitted <- fitWorkflow(split.train, learner, prepared.map((encoder, resampler, _) => (encoder, resampler)), context("scalafim.alder-ridge.inner.v1", rows, innerReceipt, Some(unit.key), common.targetGeometry))
          assessed <- predict(split.validation.data, fitted, unit.key, common)
        yield (assessed, fitted)
      )
    yield
      val squared = byPenalty.map((assessed, _) => assessed.map(row => common.targetGeometry.squaredError(row.observed, row.predicted)).sum)
      RidgeInnerUnitReceipt(unit.key, unit.analysis.map(common.keyOf), unit.assessment.map(common.keyOf),
        prepared.map(_._3), byPenalty.headOption.fold(Vector.empty[Boolean])(_._2.model.outputs.map(_.constantTrainingTarget)),
        squared, byPenalty.map((assessed, _) => assessed.map(_.predicted)), byPenalty.map(_._2.audit))

  private def prepare[S <: SemanticSpace, K, M](
      preparation: Option[RidgeCrossFitPreparation[S, K, M]],
      trainIds: Vector[Long],
      train: NonEmptyData[Use.Train, Row[M]],
      foldOf: Map[Long, Int],
      common: Common,
      scope: String
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, Option[(FoldEncoder[Id, Array[Double], Array[Double], M, Array[Double]], CompleteResampler[Row[M]], RidgeCrossFitReceipt)]] =
    preparation match
      case None => Right(None)
      case Some(prep) => foldResampler(prep, trainIds, train, foldOf, common, scope).map(Some(_))

  /** Restricts the full cross-fit partition to one training population and
    * binds a provider resampler to exactly that population and row order.
    */
  private def foldResampler[S <: SemanticSpace, K, M](
      prep: RidgeCrossFitPreparation[S, K, M],
      trainIds: Vector[Long],
      train: NonEmptyData[Use.Train, Row[M]],
      foldOf: Map[Long, Int],
      common: Common,
      scope: String
  )(using DigestAlgorithm): Either[AlderRidgeRegressionError, (FoldEncoder[Id, Array[Double], Array[Double], M, Array[Double]], CompleteResampler[Row[M]], RidgeCrossFitReceipt)] =
    val folds = trainIds.map(foldOf)
    val retained = folds.distinct.sorted
    if retained.length < 2 then Left(AlderRidgeRegressionError.CrossFitTooFewFolds(scope, retained.length))
    else
      val dense = retained.zipWithIndex.toMap
      val assignment = folds.map(dense)
      for
        labels <- Labels.retained(IArray.unsafeFromArray(assignment.toArray)).left.map(error => AlderRidgeRegressionError.CrossFitCoverage(scope, error.message))
        fixed <- FixedPartitions.once(labels).left.map(error => AlderRidgeRegressionError.CrossFitCoverage(scope, error.message))
        resampler <- Resample4sResampler.fromDesignForPopulation[Row[M]](fixed, train.fingerprint, trainIds).left.map(error => AlderRidgeRegressionError.CrossFitCoverage(scope, error.toString))
      yield (prep.encoder, resampler, RidgeCrossFitReceipt(prep.design.receipt, train.fingerprint, retained.map(prep.design.keys), trainIds.map(common.keyOf).zip(assignment)))

  private def context(schema: String, rows: AlderMaterializedRows[?], receipt: PlanReceipt, unit: Option[UnitKey], targetGeometry: RidgeTargetGeometry): FitContext =
    FitContext.root(
      Seed(receipt.seed.value),
      PlanFingerprint(AxisDigest.sha256Hex: writer =>
        writer.string(schema)
        writer.string(rows.root.fingerprint.digest)
        writer.string(rows.mapping.axis.stableKey)
        writer.string(targetGeometry.identity)
        unit.foreach: key =>
          writer.intLE(key.repeat)
          writer.intLE(key.fold)
        val assignment = receipt.assignment.value.toIArray
        writer.intLE(assignment.length)
        assignment.foreach(byte => writer.intLE(byte & 0xff))
      ),
      SchemaFingerprint("scalafim.alder-ridge.v2"),
      NumericMode.Deterministic
    )

  private def fitWorkflow[M](
      train: NonEmptyData[Use.Train, Row[M]],
      learner: SeparableRidgeLearner[M],
      preparation: Option[PreparedWorkflow[M]],
      fitContext: FitContext
  ): Either[AlderRidgeRegressionError, FittedWorkflow] =
    preparation match
      case None =>
        learner.fit(train)(using fitContext).value match
          case Left(failure) => Left(failure.cause)
          case Right(trained) =>
            Right(FittedWorkflow(input => trained.artifact.run(input).left.map(_.cause), trained.artifact, trained.audit))
      case Some((encoder, resampler)) =>
        val crossFittedMap = FeatureMap.crossFitted(encoder, resampler)
        val workflow = crossFittedMap.learnWith(learner)
        workflow.fit(train)(using fitContext).value match
          case Left(failure) => Left(recover(failure.cause))
          case Right(trained) =>
            Right(FittedWorkflow(input => trained.artifact.run(input).left.map(failure => recover(failure.cause)), trained.artifact.second, trained.audit))

  private def recover(cause: Any): AlderRidgeRegressionError = cause match
    case error: AlderRidgeRegressionError => error
    case other => AlderRidgeRegressionError.Workflow(String.valueOf(other))

  private def predict[M](assessed: Data[?, Row[M]], fitted: FittedWorkflow, unit: UnitKey, common: Common): Either[AlderRidgeRegressionError, Vector[RidgeOofRow]] =
    assessed.foldRows[Either[AlderRidgeRegressionError, Vector[RidgeOofRow]]](Right(Vector.empty)) {
      case (Left(error), _, _) => Left(error)
      case (Right(done), id, example) =>
        val key = common.keyOf(id.value)
        if example.target.length != common.targets.length then Left(AlderRidgeRegressionError.TargetShape(key, common.targets.length, example.target.length))
        else fitted.serve(example.input).map(values => done :+ RidgeOofRow(key, unit, common.targetGeometry, example.target.toVector, values.toVector))
    }

  private def assemble[M](
      rows: AlderMaterializedRows[M],
      outerReceipt: PlanReceipt,
      innerReceipt: PlanReceipt,
      crossFitReceipt: Option[PlanReceipt],
      features: Vector[String],
      common: Common,
      folds: Vector[(RidgeOuterFold, Vector[RidgeOofRow])],
      solve: RidgeSolveReceipt
  ): Either[AlderRidgeRegressionError, AlderRidgeRegressionResult] =
    val n = rows.mapping.nativeIds.length
    val q = common.targets.length
    val placed = Array.fill[Option[RidgeOofRow]](n)(None)
    val ordinalByKey = rows.mapping.entriesByOrdinal.map(_.stableKey).zipWithIndex.toMap
    val assessed = folds.flatMap(_._2)
    var index = 0
    var duplicate: Option[AlderRidgeRegressionError] = None
    while index < assessed.length && duplicate.isEmpty do
      val row = assessed(index)
      val ordinal = ordinalByKey(row.stableKey)
      if placed(ordinal).nonEmpty then duplicate = Some(AlderRidgeRegressionError.DuplicateAssessment(row.stableKey))
      else placed(ordinal) = Some(row)
      index += 1
    duplicate.orElse(placed.indices.collectFirst { case ordinal if placed(ordinal).isEmpty => AlderRidgeRegressionError.MissingAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey) }) match
      case Some(error) => Left(error)
      case None =>
        val ordered = placed.toVector.flatten
        val out = Matrix.newBuilder(n, q)
        var row = 0
        while row < n do
          var column = 0
          while column < q do
            out(row, column) = ordered(row).predicted(column)
            column += 1
          row += 1
        val columns = Vector.tabulate(q)(column => (ordered.map(_.observed(column)), ordered.map(_.predicted(column))))
        val outputs = common.targets.zip(columns).map { case (target, (observed, predicted)) => RegressionAssessment.output(target, observed, predicted) }
        Right(AlderRidgeRegressionResult(rows.mapping.axis, common.targetGeometry, features, out.result(), ordered, outerReceipt, innerReceipt,
          crossFitReceipt, folds.map(_._1), outputs, RegressionAssessment.pooled(columns, common.targetGeometry), solve, rows.receipt, rows.nativeReadReceipt))

  /** Fits one fixed-penalty provider ridge per response column on exactly the
    * rows it is given.
    */
  private def fitSeparable[U <: Use.Fit, M](
      data: NonEmptyData[U, Row[M]],
      penalty: Double,
      common: Common,
      refuseConstant: Boolean,
      scope: String,
      fitContext: FitContext
  ): Either[AlderRidgeRegressionError, SeparableRidgeModel] =
    val q = common.targets.length
    val rows = data.data.foldRows(Vector.empty[(Long, Row[M])])((done, id, example) => done :+ (id.value -> example))
    val ids = rows.map(_._1)
    lazy val constant = Vector.tabulate(q)(output => rows.forall((_, example) => example.target(output) == rows.head._2.target(output)))
    rows.find((_, example) => example.target.length != q) match
      case Some((id, example)) => Left(AlderRidgeRegressionError.TargetShape(common.keyOf(id), q, example.target.length))
      case None if refuseConstant && constant.contains(true) =>
        Left(AlderRidgeRegressionError.ConstantTrainingTarget(common.targets(constant.indexOf(true)), scope))
      case None =>
        for
          trained <- traverse((0 until q).toVector)(output =>
            IdentifiedRows.fromRows(rows.map((id, example) => id -> Example(example.input, example.target(output), example.meta)), outputSource(data.fingerprint, output, common.targets(output)))
              .flatMap(root => root.training(ids))
              .left.map(error => AlderRidgeRegressionError.ProviderSelection(error.toString))
              .flatMap(train => fitScalar(train, penalty, common, fitContext)))
        yield new SeparableRidgeModel(common.targetGeometry, common.view.names.toVector, penalty,
          trained.zipWithIndex.map((fit, output) => RidgeOutputFit(common.targets(output), fit.artifact.solution, fit.audit, constant(output))),
          trained.map(_.artifact), fitContext.stagePath)

  private def fitScalar[M](train: NonEmptyData[Use.Train, ScalarRow[M]], penalty: Double, common: Common, fitContext: FitContext): Either[AlderRidgeRegressionError, Trained[RidgeModel[Array[Double]]]] =
    RidgeConfig.create(penalty).left.map(error => AlderRidgeRegressionError.InvalidPenaltyGrid(error.toString)).flatMap: config =>
      val ridge = RidgeRegression.sync[Array[Double], M](config, backend)(using common.view)
      ridge.fit(train)(using fitContext).value.left.map(failure => AlderRidgeRegressionError.Provider("ridge-fit", failure.cause.toString))

  private def outputSource(training: DataFingerprint, output: Int, target: String): DataFingerprint =
    new DataFingerprint(FingerprintPolicy.Summary("scalafim.separable-ridge-output.v1"), AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.separable-ridge-output.v1")
      writer.string(training.policy.toString)
      writer.string(training.digest)
      writer.intLE(output)
      writer.string(target)
    )

  private def traverse[A, B](values: Vector[A])(step: A => Either[AlderRidgeRegressionError, B]): Either[AlderRidgeRegressionError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var index = 0
    var failure: Option[AlderRidgeRegressionError] = None
    while index < values.length && failure.isEmpty do
      step(values(index)) match
        case Left(error) => failure = Some(error)
        case Right(value) => out += value
      index += 1
    failure.toLeft(out.result())

  private[predictive] final class SeparableRidgeLearner[M](
      penalty: Double,
      common: Common,
      refuseConstant: Boolean,
      scope: String
  ) extends Learner[Id, Array[Double], Array[Double], M, Array[Double]]:
    type FitError = AlderRidgeRegressionError
    type RunError = AlderRidgeRegressionError
    type Model = SeparableRidgeModel
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      fitSeparable(data, penalty, common, refuseConstant, scope, context) match
        case Left(error) => EitherT.leftT(context.stagePath.failure[AlderRidgeRegressionError](error))
        case Right(model) =>
          EitherT.rightT(context.complete(model, data, ComponentDescriptor(
            ComponentId("scalafim.separable-ridge"), ComponentVersion("2"),
            AuditValue.record(
              "execution" -> AuditValue.text("bounded-materialized"),
              "penalty" -> AuditValue.text(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(penalty))),
              "targets" -> AuditValue.sequence(model.targets.map(AuditValue.text)*),
              "targetMetric" -> AuditValue.text(model.targetGeometry.identity)
            ),
            BackendFingerprint("scalafim", "1", AuditValue.record()))))

  /** Plain dense coordinates under a fixed, named prepared-feature schema. */
  private[predictive] final class ArrayFeatureView(schema: FeatureSchema[ArrayFeatureView]) extends FeatureView[Array[Double]]:
    def featureSchema: FeatureSchema[?] = schema
    def read(value: Array[Double]): Either[CoordinateError, IArray[Double]] =
      if value.length != size then Left(CoordinateError.ArityMismatch(size, value.length))
      else Right(IArray.unsafeFromArray(value.clone()))
    def writeTo(value: Array[Double], destination: CoordinateWriter): Either[CoordinateError, Unit] =
      if value.length != size then Left(CoordinateError.ArityMismatch(size, value.length))
      else if destination.size != size then Left(CoordinateError.DestinationArityMismatch(size, destination.size))
      else
        val labels = names
        var index = 0
        var failure: Option[CoordinateError] = None
        while index < value.length && failure.isEmpty do
          destination.write(index, labels(index), value(index)) match
            case Left(error) => failure = Some(error)
            case Right(()) => ()
          index += 1
        failure.toLeft(())
