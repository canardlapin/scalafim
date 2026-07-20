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
    coefficients: Vector[Double] = Vector.empty,
    voxelwiseCoefficients: Vector[Vector[Double]] = Vector.empty
):
  require(runIndex >= 0, "run index must be non-negative")
  require(rho.isFinite, "AR rho must be finite")
  require(method.nonEmpty, "AR diagnostic method must be non-empty")
  require(rows > 0, "AR diagnostic rows must be positive")
  require(coefficients.forall(_.isFinite), "AR diagnostic coefficients must be finite")
  require(coefficients.isEmpty || coefficients.head == rho, "AR diagnostic rho must match the first coefficient")
  require(voxelwiseCoefficients.forall(_.forall(_.isFinite)), "voxelwise AR coefficients must be finite")

  def phi: Vector[Double] =
    if coefficients.isEmpty then Vector(rho) else coefficients

final case class ArDiagnostics(
    order: Int,
    runs: Vector[ArRunDiagnostic],
    iterations: Int = 1,
    sharedNormalizedCovariance: Boolean = true
):
  require(order >= 1, "AR diagnostics require positive order")
  require(runs.nonEmpty, "AR diagnostics must contain at least one run")
  require(runs.forall(_.phi.length == order), "AR diagnostic coefficient length must match order")
  require(runs.forall(_.voxelwiseCoefficients.forall(_.length == order)), "voxelwise AR coefficient length must match order")
  require(iterations >= 0, "AR diagnostics iterations must be non-negative")
  require(sharedNormalizedCovariance || runs.forall(_.voxelwiseCoefficients.nonEmpty), "voxelwise AR diagnostics must carry per-voxel coefficients")

object ArDiagnostics:
  def merge(blocks: IndexedSeq[ArDiagnostics]): Either[FitError, ArDiagnostics] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one AR diagnostics block is required"))
    else
      val first = blocks.head
      if blocks.forall(_.sharedNormalizedCovariance) then
        if blocks.forall(_ == first) then Right(first)
        else Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical autocorrelation diagnostics"))
      else if blocks.exists(_.sharedNormalizedCovariance) then
        Left(FitError.IncompatibleFitBlocks("cannot merge shared and voxelwise autocorrelation diagnostics"))
      else validateVoxelwiseCompatible(blocks, first).map { _ =>
        val mergedRuns =
          first.runs.indices.toVector.map { runIndex =>
            val perVoxel =
              blocks.iterator.flatMap(block => block.runs(runIndex).voxelwiseCoefficients).toVector
            val summary = averageCoefficients(perVoxel, first.order)
            first.runs(runIndex).copy(
              rho = summary.head,
              coefficients = summary,
              voxelwiseCoefficients = perVoxel
            )
          }
        first.copy(runs = mergedRuns, sharedNormalizedCovariance = false)
      }

  private def validateVoxelwiseCompatible(
      blocks: IndexedSeq[ArDiagnostics],
      first: ArDiagnostics
  ): Either[FitError, Unit] =
    var blockIndex = 0
    while blockIndex < blocks.length do
      val block = blocks(blockIndex)
      if block.order != first.order then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share order"))
      if block.iterations != first.iterations then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share iteration count"))
      if block.runs.length != first.runs.length then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share run layout"))

      var runIndex = 0
      while runIndex < first.runs.length do
        val left = first.runs(runIndex)
        val right = block.runs(runIndex)
        if left.runIndex != right.runIndex || left.method != right.method || left.rows != right.rows then
          return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share run diagnostics"))
        if right.voxelwiseCoefficients.exists(_.length != first.order) then
          return Left(FitError.IncompatibleFitBlocks("voxelwise AR coefficient length must match order"))
        runIndex += 1
      blockIndex += 1
    Right(())

  private def averageCoefficients(coefficients: Vector[Vector[Double]], order: Int): Vector[Double] =
    val out = new Array[Double](order)
    var row = 0
    while row < coefficients.length do
      var col = 0
      while col < order do
        out(col) += coefficients(row)(col)
        col += 1
      row += 1
    var col = 0
    while col < order do
      out(col) /= coefficients.length.toDouble
      col += 1
    out.toVector

sealed trait FmriFitResult:
  def columnNames: Vector[String]
  def voxelIndices: Vector[Int]
  def timepoints: Vector[Int]
  def engine: FitEngine
  def summary: FitSummary
  def voxels: Int = voxelIndices.length

final case class DenseFmriFitResult(
    coefficients: CoefficientBlock,
    inference: CoefficientInference,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary,
    olsDiagnostics: Option[OlsDiagnostics] = None,
    autocorrelation: Option[ArDiagnostics] = None,
    robustDiagnostics: Option[RobustDiagnostics] = None
) extends FmriFitResult:
  require(columnNames.length == coefficients.predictors, "column names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "voxel indices must match coefficient columns")
  require(timepoints.nonEmpty, "fit result must contain at least one timepoint")
  require(residualVariance.length == coefficients.voxels, "residual variances must match coefficient columns")
  require(inference.predictors == coefficients.predictors, "coefficient inference must match coefficient rows")
  require(inference.voxels == coefficients.voxels, "coefficient inference must match coefficient columns")
  require(olsDiagnostics.forall(_.predictors == coefficients.predictors), "OLS diagnostics must match coefficient rows")

  def predictors: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  def standardErrors: StandardErrorBlock = inference.standardErrors
  def normalizedCovariance: DoubleMatrix = inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = inference.covariance
  def inferenceScope: CoefficientInferenceScope = inference.scope
  def diagnostics: FitDiagnostics = FitDiagnostics(residualDegreesOfFreedom, residualVariance)
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def inferenceReady: Either[FitError, InferenceReadyDenseFit] =
    autocorrelation match
      case Some(diagnostics) if !diagnostics.sharedNormalizedCovariance && !inference.covariance.isVoxelwise =>
        Left(FitError.UnsupportedAutocorrelation("voxelwise AR contrast inference requires per-voxel covariance result bundles"))
      case _ =>
        robustDiagnostics match
          case Some(diagnostics) if !diagnostics.sharedNormalizedCovariance && !inference.covariance.isVoxelwise =>
            Left(FitError.UnsupportedRobust("robust contrast inference requires per-voxel covariance result bundles"))
          case _ =>
            Right(InferenceReadyDenseFit(this, inference.residualDegreesOfFreedom))

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
