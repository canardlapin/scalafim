package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinAlgError, LinearOperator, Matrix, MutableDVec}
import gale.solvers.{SolverConfig, ToleranceMode, lsqr}
import multivar.core.{OperatorRepresentation, SemanticSpace}
import scalafim.fmri.mvpa.predictive.ClassId

opaque type RidgePenalty = Double

object RidgePenalty:
  def apply(value: Double): Either[OperatorRidgeError, RidgePenalty] =
    if !value.isFinite || value <= 0.0 then Left(OperatorRidgeError.InvalidPenalty(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): RidgePenalty =
    value

  extension (penalty: RidgePenalty) inline def value: Double = penalty

opaque type OperatorRidgeTolerance = Double

object OperatorRidgeTolerance:
  def apply(value: Double): Either[OperatorRidgeError, OperatorRidgeTolerance] =
    if !value.isFinite || value <= 0.0 then Left(OperatorRidgeError.InvalidTolerance(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): OperatorRidgeTolerance =
    value

  extension (tolerance: OperatorRidgeTolerance) inline def value: Double = tolerance

opaque type OperatorRidgeIterationLimit = Int

object OperatorRidgeIterationLimit:
  def apply(value: Int): Either[OperatorRidgeError, OperatorRidgeIterationLimit] =
    if value <= 0 then Left(OperatorRidgeError.InvalidIterationLimit(value))
    else Right(value)

  private[mvpa] def unsafe(value: Int): OperatorRidgeIterationLimit =
    value

  extension (limit: OperatorRidgeIterationLimit) inline def value: Int = limit

enum OperatorRidgeError:
  case InvalidPenalty(value: Double)
  case InvalidTolerance(value: Double)
  case InvalidIterationLimit(value: Int)
  case InsufficientTrainingSamples(fitId: String, actual: Int)
  case NumericalFailure(fitId: String, cause: LinAlgError)
  case NonFiniteResult(fitId: String, stage: String)
  case InvalidSolverIdentity(detail: String)
  case InvalidSolverChoice(detail: String)
  case TargetShapeMismatch(
      expectedRows: Int,
      expectedClasses: Int,
      actualRows: Int,
      actualClasses: Int
  )
  case MissingTrainingClass(fitId: String, classId: ClassId)
  case FeatureAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case FeatureWitnessMismatch
  case Evidence(error: EvidenceTableError)
  case DidNotConverge(
      fitId: String,
      classId: ClassId,
      iterations: Int,
      normalResidual: Double
  )

  def message: String =
    this match
      case InvalidPenalty(value) =>
        s"ridge penalty must be positive and finite, got $value"
      case InvalidTolerance(value) =>
        s"LSQR tolerance must be positive and finite, got $value"
      case InvalidIterationLimit(value) =>
        s"LSQR iteration limit must be positive, got $value"
      case InsufficientTrainingSamples(fitId, actual) =>
        s"ridge fit '$fitId' requires at least two training samples, got $actual"
      case NumericalFailure(fitId, cause) =>
        s"ridge fit '$fitId' failed numerically: ${cause.getMessage}"
      case NonFiniteResult(fitId, stage) =>
        s"ridge fit '$fitId' produced non-finite values in $stage"
      case InvalidSolverIdentity(detail) =>
        s"invalid operator-ridge solver identity: $detail"
      case InvalidSolverChoice(detail) =>
        s"invalid operator-ridge solver choice: $detail"
      case TargetShapeMismatch(
            expectedRows,
            expectedClasses,
            actualRows,
            actualClasses
          ) =>
        s"operator-ridge target expected ${expectedRows}x$expectedClasses values, obtained ${actualRows}x$actualClasses"
      case MissingTrainingClass(fitId, classId) =>
        s"ridge fit '$fitId' has no training membership mass for class ${classId.value}"
      case FeatureAxisMismatch(expected, actual) =>
        s"ridge prediction feature axis ${actual.value} does not match fitted axis ${expected.value}"
      case FeatureWitnessMismatch =>
        "ridge prediction and fitted model use different nominal feature witnesses"
      case Evidence(error) =>
        error.message
      case DidNotConverge(fitId, classId, iterations, normalResidual) =>
        s"ridge fit '$fitId' for class ${classId.value} did not converge after $iterations iteration(s); " +
          s"normal residual=$normalResidual"

/** LSQR execution settings for the identified-evidence ridge kernel. The settings are encoded in [[SolverIdentity]],
  * not in the scientific estimand identity.
  */
final class OperatorRidgeSolverSettings private (
    val tolerance: OperatorRidgeTolerance,
    val maxIterations: OperatorRidgeIterationLimit,
    val identity: SolverIdentity
)

object OperatorRidgeSolverSettings:
  private val Solver = SolverId.unsafe("operator-ridge-lsqr")

  val Default: OperatorRidgeSolverSettings =
    unsafe(1e-10, 2000)

  def apply(
      tolerance: Double = 1e-10,
      maxIterations: Int = 2000
  ): Either[OperatorRidgeError, OperatorRidgeSolverSettings] =
    for
      solverTolerance <- OperatorRidgeTolerance(tolerance)
      iterationLimit <- OperatorRidgeIterationLimit(maxIterations)
      identity <- SolverIdentity(
        Solver,
        Vector(
          "max-iterations" -> iterationLimit.value.toString,
          "tolerance" -> java.lang.Double.toHexString(solverTolerance.value)
        )
      ).left.map(error => OperatorRidgeError.InvalidSolverIdentity(error.message))
    yield new OperatorRidgeSolverSettings(
      solverTolerance,
      iterationLimit,
      identity
    )

  private[mvpa] def unsafe(
      tolerance: Double = 1e-10,
      maxIterations: Int = 2000
  ): OperatorRidgeSolverSettings =
    val solverTolerance = OperatorRidgeTolerance.unsafe(tolerance)
    val iterationLimit = OperatorRidgeIterationLimit.unsafe(maxIterations)
    new OperatorRidgeSolverSettings(
      solverTolerance,
      iterationLimit,
      SolverIdentity.trusted(
        Solver,
        Vector(
          "max-iterations" -> iterationLimit.value.toString,
          "tolerance" -> java.lang.Double.toHexString(solverTolerance.value)
        )
      )
    )

  def choice(settings: OperatorRidgeSolverSettings): SolverChoice =
    SolverChoice.Selected(settings.identity)

  def from(choice: SolverChoice): Either[OperatorRidgeError, OperatorRidgeSolverSettings] =
    choice match
      case SolverChoice.NotApplicable =>
        Left(OperatorRidgeError.InvalidSolverChoice("no solver was selected"))
      case SolverChoice.Selected(identity) if identity.id != Solver =>
        Left(
          OperatorRidgeError.InvalidSolverChoice(
            s"expected '${Solver.value}', obtained '${identity.id.value}'"
          )
        )
      case SolverChoice.Selected(identity) =>
        val fields = identity.fields.map(field => field.name -> field.value).toMap
        (fields.get("tolerance"), fields.get("max-iterations")) match
          case (Some(tolerance), Some(iterations)) =>
            try
              apply(java.lang.Double.valueOf(tolerance), iterations.toInt).flatMap: decoded =>
                if decoded.identity == identity then Right(decoded)
                else Left(OperatorRidgeError.InvalidSolverChoice("solver fields are not canonical"))
            catch
              case _: NumberFormatException =>
                Left(OperatorRidgeError.InvalidSolverChoice("solver fields are not numeric"))
          case _ =>
            Left(
              OperatorRidgeError.InvalidSolverChoice(
                "solver identity requires tolerance and max-iterations fields"
              )
            )

final case class OperatorRidgeKernelClassReceipt private[mvpa] (
    classId: ClassId,
    iterations: Int,
    normalResidual: Double
):
  require(iterations >= 0, "ridge iterations must be non-negative")
  require(
    normalResidual.isFinite && normalResidual >= 0.0,
    "ridge residual must be finite and non-negative"
  )

final case class OperatorRidgeKernelFitReceipt private[mvpa] (
    fitId: String,
    trainingAxis: AxisIdentity,
    featureAxis: AxisIdentity,
    classAxis: AxisIdentity,
    sourceRepresentation: OperatorRepresentation,
    penalty: RidgePenalty,
    solver: OperatorRidgeSolverSettings,
    classFits: Vector[OperatorRidgeKernelClassReceipt],
    forwardApplications: Int,
    transposeApplications: Int
):
  require(fitId.trim.nonEmpty, "ridge fit id must be non-empty")
  require(classFits.length >= 2, "ridge receipt requires at least two class fits")
  require(forwardApplications >= 0, "forward applications must be non-negative")
  require(transposeApplications >= 0, "transpose applications must be non-negative")

  def operatorApplications: Long =
    forwardApplications.toLong + transposeApplications.toLong

/** A fitted operator-ridge model bound to exact feature and class axes. */
final class OperatorRidgeKernelModel[
    Features <: SemanticSpace,
    FeatureKey
] private[mvpa] (
    val features: AxisRef.Aux[FeatureKey, Features],
    val classes: AxisRef[ClassId],
    val coefficients: DMat,
    val intercepts: IArray[Double],
    val receipt: OperatorRidgeKernelFitReceipt
):
  def predict[
      Rows <: SemanticSpace,
      RowKey
  ](
      test: EvidenceTable[Rows, Features, RowKey, FeatureKey]
  ): Either[OperatorRidgeError, DMat] =
    if test.columns.identity != features.identity then
      Left(
        OperatorRidgeError.FeatureAxisMismatch(
          features.identity.fingerprint,
          test.columns.identity.fingerprint
        )
      )
    else if !(test.columns.evidence eq features.evidence) then Left(OperatorRidgeError.FeatureWitnessMismatch)
    else
      test
        .rightMultiply(coefficients)
        .left
        .map(error => OperatorRidgeError.Evidence(error))
        .flatMap(addIntercepts)

  private def addIntercepts(raw: DMat): Either[OperatorRidgeError, DMat] =
    val scores = Matrix.newBuilder(raw.rows, raw.cols)
    var row = 0
    while row < raw.rows do
      var klass = 0
      while klass < raw.cols do
        val value = raw(row, klass) + intercepts(klass)
        if !value.isFinite then
          return Left(
            OperatorRidgeError.NonFiniteResult(
              receipt.fitId,
              "held-out class scores"
            )
          )
        scores(row, klass) = value
        klass += 1
      row += 1
    Right(scores.result())

/** Matrix-free numerical kernel used by the typed predictive compiler. It consumes identified evidence directly and
  * never substitutes local ordinals for scientific sample or feature identity.
  */
object OperatorRidgeKernel:
  private val MembershipMassTolerance = 1e-12

  def fit[
      Rows <: SemanticSpace,
      Features <: SemanticSpace,
      RowKey,
      FeatureKey
  ](
      train: EvidenceTable[Rows, Features, RowKey, FeatureKey],
      classes: AxisRef[ClassId],
      targetValues: DMat,
      penalty: RidgePenalty,
      solver: OperatorRidgeSolverSettings,
      fitId: String
  ): Either[
    OperatorRidgeError,
    OperatorRidgeKernelModel[Features, FeatureKey]
  ] =
    if targetValues.rows != train.rowCount || targetValues.cols != classes.size then
      Left(
        OperatorRidgeError.TargetShapeMismatch(
          train.rowCount,
          classes.size,
          targetValues.rows,
          targetValues.cols
        )
      )
    else if train.rowCount < 2 then Left(OperatorRidgeError.InsufficientTrainingSamples(fitId, train.rowCount))
    else
      missingClass(targetValues, classes) match
        case Some(classId) =>
          Left(OperatorRidgeError.MissingTrainingClass(fitId, classId))
        case None =>
          try
            val counter = new KernelApplicationCounter
            val checked = checkedOperator(train.operator, counter, fitId)
            val featureMeans = columnMeans(checked)
            val targetMeans = columnMeans(targetValues)
            val centered = centeredOperator(checked, featureMeans)
            val augmented = augmentedRidgeOperator(
              centered,
              math.sqrt(penalty.value)
            )
            val coefficients = Matrix.newBuilder(train.columnCount, classes.size)
            val intercepts = new Array[Double](classes.size)
            val classReceipts = Vector.newBuilder[OperatorRidgeKernelClassReceipt]
            val solverConfig = SolverConfig(
              tolerance = solver.tolerance.value,
              maxIterations = solver.maxIterations.value
            )

            var klass = 0
            while klass < classes.size do
              val rhs = DVec.newBuilder(train.rowCount + train.columnCount)
              var row = 0
              while row < train.rowCount do
                rhs(row) = targetValues(row, klass) - targetMeans(klass)
                row += 1

              val solved = lsqr(
                augmented,
                rhs.result(),
                solverConfig,
                ToleranceMode.RelativeToRhs
              )
              if !solved.converged then
                return Left(
                  OperatorRidgeError.DidNotConverge(
                    fitId,
                    classes.keys(klass),
                    solved.iterations,
                    solved.residual
                  )
                )
              if !solved.residual.isFinite then
                return Left(
                  OperatorRidgeError.NonFiniteResult(fitId, "LSQR residual")
                )

              var feature = 0
              var meanContribution = 0.0
              while feature < train.columnCount do
                val coefficient = solved.x(feature)
                if !coefficient.isFinite then
                  return Left(
                    OperatorRidgeError.NonFiniteResult(
                      fitId,
                      "ridge coefficients"
                    )
                  )
                coefficients(feature, klass) = coefficient
                meanContribution += featureMeans(feature) * coefficient
                feature += 1
              val intercept = targetMeans(klass) - meanContribution
              if !intercept.isFinite then
                return Left(
                  OperatorRidgeError.NonFiniteResult(fitId, "ridge intercepts")
                )
              intercepts(klass) = intercept
              classReceipts += OperatorRidgeKernelClassReceipt(
                classes.keys(klass),
                solved.iterations,
                solved.residual
              )
              klass += 1

            val receipt = OperatorRidgeKernelFitReceipt(
              fitId,
              train.rows.identity,
              train.columns.identity,
              classes.identity,
              train.representation,
              penalty,
              solver,
              classReceipts.result(),
              counter.forwardApplications,
              counter.transposeApplications
            )
            Right(
              new OperatorRidgeKernelModel(
                train.columns,
                classes,
                coefficients.result(),
                IArray.unsafeFromArray(intercepts),
                receipt
              )
            )
          catch
            case cause: LinAlgError =>
              Left(OperatorRidgeError.NumericalFailure(fitId, cause))

  private def checkedOperator(
      source: DoubleLinearOperator,
      counter: KernelApplicationCounter,
      fitId: String
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(source.rows, source.cols)(
      (input, output) =>
        counter.forwardApplications += 1
        source.applyTo(input, output)
        requireFinite(output, s"ridge fit '$fitId' forward operator output")
      ,
      (input, output) =>
        counter.transposeApplications += 1
        source.transposeApplyTo(input, output)
        requireFinite(output, s"ridge fit '$fitId' transpose operator output")
    )

  private def centeredOperator(
      source: DoubleLinearOperator,
      featureMeans: Array[Double]
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(source.rows, source.cols)(
      (input, output) =>
        source.applyTo(input, output)
        var meanContribution = 0.0
        var feature = 0
        while feature < source.cols do
          meanContribution += featureMeans(feature) * input(feature)
          feature += 1
        var row = 0
        while row < source.rows do
          output(row) = output(row) - meanContribution
          row += 1
      ,
      (input, output) =>
        source.transposeApplyTo(input, output)
        var inputSum = 0.0
        var row = 0
        while row < source.rows do
          inputSum += input(row)
          row += 1
        var feature = 0
        while feature < source.cols do
          output(feature) = output(feature) - featureMeans(feature) * inputSum
          feature += 1
    )

  private def augmentedRidgeOperator(
      source: DoubleLinearOperator,
      penaltyRoot: Double
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(
      source.rows + source.cols,
      source.cols
    )(
      (input, output) =>
        val dataOutput = MutableDVec.zeros(source.rows)
        source.applyTo(input, dataOutput)
        var row = 0
        while row < source.rows do
          output(row) = dataOutput(row)
          row += 1
        var feature = 0
        while feature < source.cols do
          output(source.rows + feature) = penaltyRoot * input(feature)
          feature += 1
      ,
      (input, output) =>
        val dataInput = DVec.newBuilder(source.rows)
        var row = 0
        while row < source.rows do
          dataInput(row) = input(row)
          row += 1
        source.transposeApplyTo(dataInput.result(), output)
        var feature = 0
        while feature < source.cols do
          output(feature) = output(feature) + penaltyRoot * input(source.rows + feature)
          feature += 1
    )

  private def columnMeans(operator: DoubleLinearOperator): Array[Double] =
    val ones = DVec.fill(operator.rows)(1.0 / operator.rows.toDouble)
    val sums = MutableDVec.zeros(operator.cols)
    operator.transposeApplyTo(ones, sums)
    val means = new Array[Double](operator.cols)
    var feature = 0
    while feature < operator.cols do
      means(feature) = sums(feature)
      feature += 1
    means

  private def columnMeans(matrix: DMat): Array[Double] =
    val means = new Array[Double](matrix.cols)
    var column = 0
    while column < matrix.cols do
      var row = 0
      while row < matrix.rows do
        means(column) += matrix(row, column)
        row += 1
      means(column) = means(column) / matrix.rows.toDouble
      column += 1
    means

  private def missingClass(
      targetValues: DMat,
      classes: AxisRef[ClassId]
  ): Option[ClassId] =
    var klass = 0
    while klass < targetValues.cols do
      var mass = 0.0
      var row = 0
      while row < targetValues.rows do
        mass += targetValues(row, klass)
        row += 1
      if !mass.isFinite || mass <= MembershipMassTolerance then return Some(classes.keys(klass))
      klass += 1
    None

  private def requireFinite(vector: MutableDVec, stage: String): Unit =
    var index = 0
    while index < vector.length do
      if !vector(index).isFinite then throw LinAlgError.InvalidArgument(s"$stage contains non-finite values")
      index += 1

  private final class KernelApplicationCounter:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0
