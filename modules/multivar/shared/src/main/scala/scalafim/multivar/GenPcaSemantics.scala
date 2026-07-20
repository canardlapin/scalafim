package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** Numerically normalized row axes `U`, satisfying `U' A U = I`. */
final case class StandardRowScores private[multivar] (
    values: DoubleMatrix,
    space: Option[MvSpace]
)

/** Principal row scores `X R V = U Sigma`. */
final case class PrincipalRowScores private[multivar] (
    values: DoubleMatrix,
    space: Option[MvSpace]
)

/** Column axes `V`, satisfying `V' R V = I`. */
final case class ColumnAxes private[multivar] (
    values: DoubleMatrix,
    space: MvSpace
)

/** Column metric loadings `R V`, the covectors applied by the table. */
final case class ColumnMetricLoadings private[multivar] (
    values: DoubleMatrix,
    space: MvSpace
)

/** Row metric loadings `A U`, dual to the normalized row axes. */
final case class RowMetricLoadings private[multivar] (
    values: DoubleMatrix,
    space: Option[MvSpace]
)

/** Row-dual principal scores `A U Sigma`. */
final case class RowDualPrincipalScores private[multivar] (
    values: DoubleMatrix,
    space: Option[MvSpace]
)

final case class SpectralCluster private[multivar] (
    firstComponent: Int,
    componentCount: Int,
    leadingEigenvalue: Double,
    trailingEigenvalue: Double
):
  require(firstComponent >= 0, "spectral cluster start must be non-negative")
  require(componentCount > 0, "spectral cluster must contain at least one component")

  def lastComponentExclusive: Int =
    firstComponent + componentCount

  def individuallyIdentifiable: Boolean =
    componentCount == 1

final case class SpectralClusteringTolerance private (
    absolute: Double,
    relative: Double
):
  private[multivar] def threshold(left: Double, right: Double): Double =
    absolute + relative * Math.max(Math.abs(left), Math.abs(right))

object SpectralClusteringTolerance:
  val default: SpectralClusteringTolerance =
    new SpectralClusteringTolerance(1e-12, 1e-8)

  def from(absolute: Double, relative: Double): Either[MultivarError, SpectralClusteringTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(MultivarError.InvalidTolerance("spectral clustering absolute tolerance", absolute))
    else if !relative.isFinite || relative < 0.0 then
      Left(MultivarError.InvalidTolerance("spectral clustering relative tolerance", relative))
    else Right(new SpectralClusteringTolerance(absolute, relative))

final case class GenPcaSpectrum private[multivar] (
    singularValues: DoubleVector,
    generalizedEigenvalues: DoubleVector
):
  require(singularValues.length == generalizedEigenvalues.length, "GenPCA spectrum lengths must agree")

  def clusters(
      tolerance: SpectralClusteringTolerance = SpectralClusteringTolerance.default
  ): Vector[SpectralCluster] =
    if generalizedEigenvalues.length == 0 then Vector.empty
    else
      val out = Vector.newBuilder[SpectralCluster]
      var start = 0
      var component = 1
      while component < generalizedEigenvalues.length do
        val previous = generalizedEigenvalues(component - 1)
        val current = generalizedEigenvalues(component)
        if Math.abs(previous - current) > tolerance.threshold(previous, current) then
          out += SpectralCluster(
            firstComponent = start,
            componentCount = component - start,
            leadingEigenvalue = generalizedEigenvalues(start),
            trailingEigenvalue = previous
          )
          start = component
        component += 1
      out += SpectralCluster(
        firstComponent = start,
        componentCount = generalizedEigenvalues.length - start,
        leadingEigenvalue = generalizedEigenvalues(start),
        trailingEigenvalue = generalizedEigenvalues(generalizedEigenvalues.length - 1)
      )
      out.result()

/** Semantically named view of the generalized singular system returned by GenPCA.
  *
  * The legacy `ou`, `ov`, `u`, `v`, and `metricScores` accessors remain available on
  * [[GenPcaFit]], but this value makes their primal and dual roles explicit.
  */
final case class GenPcaSemanticResult private[multivar] (
    standardRowScores: StandardRowScores,
    principalRowScores: PrincipalRowScores,
    columnAxes: ColumnAxes,
    columnMetricLoadings: ColumnMetricLoadings,
    rowMetricLoadings: RowMetricLoadings,
    rowDualPrincipalScores: RowDualPrincipalScores,
    spectrum: GenPcaSpectrum
):
  val componentCount: Int =
    spectrum.singularValues.length

object GenPcaSemanticResult:
  def from(fit: GenPcaFit): GenPcaSemanticResult =
    val eigenvalues = new Array[Double](fit.componentCount)
    var component = 0
    while component < fit.componentCount do
      val singularValue = fit.d(component)
      eigenvalues(component) = singularValue * singularValue
      component += 1

    val rowSpace = fit.rowMetric.space
    val columnSpace = fit.projection.map.domain
    GenPcaSemanticResult(
      standardRowScores = StandardRowScores(fit.ou, rowSpace),
      principalRowScores = PrincipalRowScores(fit.projection.scores, rowSpace),
      columnAxes = ColumnAxes(fit.ov, columnSpace),
      columnMetricLoadings = ColumnMetricLoadings(fit.v, columnSpace),
      rowMetricLoadings = RowMetricLoadings(fit.u, rowSpace),
      rowDualPrincipalScores = RowDualPrincipalScores(fit.metricScores, rowSpace),
      spectrum = GenPcaSpectrum(fit.d, DoubleVector.unsafe(eigenvalues))
    )

extension (fit: GenPcaFit)
  def semanticResult: GenPcaSemanticResult =
    GenPcaSemanticResult.from(fit)

enum NumericalResidualNorm:
  case Frobenius

final case class NumericalLawTolerance private (
    absolute: Double,
    relative: Double
):
  private[multivar] def threshold(referenceScale: Double): Double =
    absolute + relative * referenceScale

object NumericalLawTolerance:
  val strict: NumericalLawTolerance =
    new NumericalLawTolerance(1e-10, 1e-8)

  def from(absolute: Double, relative: Double): Either[MultivarError, NumericalLawTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(MultivarError.InvalidTolerance("law absolute tolerance", absolute))
    else if !relative.isFinite || relative < 0.0 then
      Left(MultivarError.InvalidTolerance("law relative tolerance", relative))
    else Right(new NumericalLawTolerance(absolute, relative))

/** An explicit numerical statement of a law residual.
  *
  * A law is satisfied when `residual <= absolute + relative * referenceScale`.
  */
final case class NumericalLawResidual private[multivar] (
    law: String,
    norm: NumericalResidualNorm,
    residual: Double,
    referenceScale: Double,
    tolerance: NumericalLawTolerance
):
  def threshold: Double =
    tolerance.threshold(referenceScale)

  def satisfied: Boolean =
    residual <= threshold

final case class GenPcaLawDiagnostics private[multivar] (
    columnToRowTransport: NumericalLawResidual,
    rowToColumnTransport: NumericalLawResidual,
    rowOrthonormality: NumericalLawResidual,
    columnOrthonormality: NumericalLawResidual
):
  def residuals: Vector[NumericalLawResidual] =
    Vector(columnToRowTransport, rowToColumnTransport, rowOrthonormality, columnOrthonormality)

  def satisfied: Boolean =
    residuals.forall(_.satisfied)

object GenPcaLaws:
  /** Evaluate the dual transport and metric-orthonormality laws on the fitted table.
    *
    * The provided training table is transformed by the fit preprocessor before the
    * transport equations are evaluated.
    */
  def evaluate(
      trainingTable: MatrixView,
      fit: GenPcaFit,
      tolerance: NumericalLawTolerance = NumericalLawTolerance.strict
  ): Either[MultivarError, GenPcaLawDiagnostics] =
    for
      transformed <- fit.preprocessor.transform(trainingTable)
      columnToRow <- transformed.rightMultiply(fit.v)
      rowToColumn <- transformed.transposeMultiply(MatrixView.dense(fit.u))
    yield
      val expectedPrincipalScores = MetricOperator.scaleColumnsDense(fit.ou, fit.d)
      val expectedColumnTransport = MetricOperator.scaleColumnsDense(fit.ov, fit.d)
      val rowGram = DoubleMatrix.transposeMultiply(fit.ou, fit.u)
      val columnGram = DoubleMatrix.transposeMultiply(fit.ov, fit.v)
      val identity = DoubleMatrix.eye(fit.componentCount)
      GenPcaLawDiagnostics(
        columnToRowTransport = residual(
          "X R V = U Sigma",
          columnToRow,
          expectedPrincipalScores,
          tolerance
        ),
        rowToColumnTransport = residual(
          "X* A U = V Sigma",
          rowToColumn,
          expectedColumnTransport,
          tolerance
        ),
        rowOrthonormality = residual(
          "U* A U = I",
          rowGram,
          identity,
          tolerance
        ),
        columnOrthonormality = residual(
          "V* R V = I",
          columnGram,
          identity,
          tolerance
        )
      )

  def weightedSquaredError(
      actual: DoubleMatrix,
      approximation: DoubleMatrix,
      rowMetric: MvMetric,
      columnMetric: MvMetric
  ): Either[MultivarError, Double] =
    if actual.rows != approximation.rows || actual.cols != approximation.cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"weighted reconstruction compares ${actual.rows}x${actual.cols} with ${approximation.rows}x${approximation.cols}"
        )
      )
    else if rowMetric.dim != actual.rows then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, actual.rows, rowMetric.dim))
    else if columnMetric.dim != actual.cols then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, actual.cols, columnMetric.dim))
    else
      val difference = subtract(actual, approximation)
      rowMetric.matvec(difference).flatMap { weighted =>
        val gram = DoubleMatrix.transposeMultiply(difference, weighted)
        columnMetric.contract(gram)
      }

  private def residual(
      law: String,
      actual: DoubleMatrix,
      expected: DoubleMatrix,
      tolerance: NumericalLawTolerance
  ): NumericalLawResidual =
    require(actual.rows == expected.rows && actual.cols == expected.cols, "law residual shapes must agree")
    var squaredResidual = 0.0
    var squaredReference = 0.0
    var i = 0
    while i < actual.dataArray.length do
      val difference = actual.dataArray(i) - expected.dataArray(i)
      squaredResidual += difference * difference
      squaredReference += expected.dataArray(i) * expected.dataArray(i)
      i += 1
    NumericalLawResidual(
      law = law,
      norm = NumericalResidualNorm.Frobenius,
      residual = Math.sqrt(squaredResidual),
      referenceScale = Math.sqrt(squaredReference),
      tolerance = tolerance
    )

  private def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) -= right.dataArray(i)
      i += 1
    DoubleMatrix.unsafe(left.rows, left.cols, out)
