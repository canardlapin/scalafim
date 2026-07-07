package scalafim.fmri.fit

import scalafim.fmri.model.{FitEngine, FitSummary}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

final case class FitDiagnostics(
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    residualVariance: DoubleVector
):
  require(residualVariance.length > 0, "diagnostics must contain at least one residual variance")

final case class ArRunDiagnostic(
    runIndex: Int,
    rho: Double,
    method: String,
    rows: Int,
    coefficients: Vector[Double] = Vector.empty
):
  require(runIndex >= 0, "run index must be non-negative")
  require(rho.isFinite, "AR rho must be finite")
  require(method.nonEmpty, "AR diagnostic method must be non-empty")
  require(rows > 0, "AR diagnostic rows must be positive")
  require(coefficients.forall(_.isFinite), "AR diagnostic coefficients must be finite")
  require(coefficients.isEmpty || coefficients.head == rho, "AR diagnostic rho must match the first coefficient")

  def phi: Vector[Double] =
    if coefficients.isEmpty then Vector(rho) else coefficients

final case class ArDiagnostics(
    order: Int,
    runs: Vector[ArRunDiagnostic]
):
  require(order >= 1, "AR diagnostics require positive order")
  require(runs.nonEmpty, "AR diagnostics must contain at least one run")
  require(runs.forall(_.phi.length == order), "AR diagnostic coefficient length must match order")

sealed trait FmriFitResult:
  def columnNames: Vector[String]
  def voxelIndices: Vector[Int]
  def timepoints: Vector[Int]
  def engine: FitEngine
  def summary: FitSummary
  def voxels: Int = voxelIndices.length

final case class DenseFmriFitResult(
    coefficients: CoefficientBlock,
    standardErrors: StandardErrorBlock,
    normalizedCovariance: DoubleMatrix,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary,
    olsDiagnostics: Option[OlsDiagnostics] = None,
    autocorrelation: Option[ArDiagnostics] = None
) extends FmriFitResult:
  require(columnNames.length == coefficients.predictors, "column names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "voxel indices must match coefficient columns")
  require(timepoints.nonEmpty, "fit result must contain at least one timepoint")
  require(residualVariance.length == coefficients.voxels, "residual variances must match coefficient columns")
  require(standardErrors.predictors == coefficients.predictors, "standard errors must match coefficient rows")
  require(standardErrors.voxels == coefficients.voxels, "standard errors must match coefficient columns")
  require(normalizedCovariance.rows == coefficients.predictors, "normalized covariance rows must match predictors")
  require(normalizedCovariance.cols == coefficients.predictors, "normalized covariance cols must match predictors")
  require(olsDiagnostics.forall(_.predictors == coefficients.predictors), "OLS diagnostics must match coefficient rows")

  def predictors: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  def diagnostics: FitDiagnostics = FitDiagnostics(residualDegreesOfFreedom, residualVariance)
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def inferenceReady: Either[FitError, InferenceReadyDenseFit] =
    Right(InferenceReadyDenseFit(this, residualDegreesOfFreedom))

  def coefficient(columnName: String, voxelIndex: Int): Option[Double] =
    val row = columnNames.indexOf(columnName)
    val col = voxelIndices.indexOf(voxelIndex)
    if row < 0 || col < 0 then None else Some(coefficients(row, col))

final case class LssFmriFitResult(
    coefficients: CoefficientBlock,
    trialNames: Vector[String],
    lssDiagnostics: LssDiagnostics,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary
) extends FmriFitResult:
  require(trialNames.length == coefficients.predictors, "trial names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "voxel indices must match coefficient columns")
  require(timepoints.nonEmpty, "LSS result must contain at least one timepoint")
  require(engine == FitEngine.LeastSquaresSeparate, "LSS result engine must be LeastSquaresSeparate")

  def columnNames: Vector[String] = trialNames
  def trials: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def coefficient(trialName: String, voxelIndex: Int): Option[Double] =
    val row = trialNames.indexOf(trialName)
    val col = voxelIndices.indexOf(voxelIndex)
    if row < 0 || col < 0 then None else Some(coefficients(row, col))

final case class RunwiseFmriRunResult(
    runIndex: Int,
    rowIndices: Vector[Int],
    timepoints: Vector[Int],
    coefficients: CoefficientBlock,
    standardErrors: StandardErrorBlock,
    normalizedCovariance: DoubleMatrix,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    olsDiagnostics: OlsDiagnostics
):
  require(rowIndices.nonEmpty, "run result must contain at least one selected row")
  require(rowIndices.length == timepoints.length, "run rows and timepoints must align")
  require(residualVariance.length == coefficients.voxels, "run residual variances must match coefficient columns")
  require(standardErrors.predictors == coefficients.predictors, "run standard errors must match coefficient rows")
  require(standardErrors.voxels == coefficients.voxels, "run standard errors must match coefficient columns")
  require(normalizedCovariance.rows == coefficients.predictors, "run covariance rows must match predictors")
  require(normalizedCovariance.cols == coefficients.predictors, "run covariance cols must match predictors")
  require(olsDiagnostics.predictors == coefficients.predictors, "run OLS diagnostics must match coefficient rows")

  def coefficient(columnName: String, voxelIndex: Int, columnNames: Vector[String], voxelIndices: Vector[Int]): Option[Double] =
    val row = columnNames.indexOf(columnName)
    val col = voxelIndices.indexOf(voxelIndex)
    if row < 0 || col < 0 then None else Some(coefficients(row, col))

  def diagnostics: FitDiagnostics = FitDiagnostics(residualDegreesOfFreedom, residualVariance)
  def typedRunIndex: RunIndex = RunIndex.unsafe(runIndex)
  def selectedRows: Vector[SelectedRowIndex] = rowIndices.map(SelectedRowIndex.unsafe)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

final case class RunwiseFmriFitResult(
    runs: Vector[RunwiseFmriRunResult],
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary
) extends FmriFitResult:
  require(runs.nonEmpty, "runwise result must contain at least one run")
  require(timepoints.nonEmpty, "runwise result must contain at least one timepoint")
  require(runs.forall(_.coefficients.predictors == columnNames.length), "run coefficients must match column names")
  require(runs.forall(_.coefficients.voxels == voxelIndices.length), "run coefficients must match voxel indices")

  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def run(runIndex: Int): Option[RunwiseFmriRunResult] =
    runs.find(_.runIndex == runIndex)
