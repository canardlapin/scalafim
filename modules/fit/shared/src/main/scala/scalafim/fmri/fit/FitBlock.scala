package scalafim.fmri.fit

import scalafim.fmri.model.{FitConfig, FitEngine}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

final case class LssBlockDesign(
    prepared: LssPreparedDesign
):
  def timepoints: Int = prepared.timepoints
  def trials: Int = prepared.trials

final case class FitBlockInput(
    design: DesignMatrix,
    response: ResponseBlock,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    partitions: Vector[RunPartition] = Vector.empty,
    lssDesign: Option[LssBlockDesign] = None
):
  require(design.timepoints == response.timepoints, "block design and response rows must match")
  require(timepoints.length == design.timepoints, "block timepoint indices must match design rows")
  require(voxelIndices.length == response.voxels, "block voxel indices must match response columns")
  require(voxelIndices.nonEmpty, "fit block must contain at least one voxel")
  require(timepoints.nonEmpty, "fit block must contain at least one timepoint")
  partitions.foreach { partition =>
    require(partition.rowIndices.forall(row => row >= 0 && row < design.timepoints), "run partition row out of block bounds")
  }
  lssDesign.foreach { lss =>
    require(lss.timepoints == design.timepoints, "LSS design rows must match block design rows")
  }

  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

sealed trait FitBlockResult:
  def voxelIndices: Vector[Int]
  def timepoints: Vector[Int]
  def engine: FitEngine
  def voxels: Int = voxelIndices.length
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

final case class DenseFitBlockResult(
    coefficients: CoefficientBlock,
    standardErrors: StandardErrorBlock,
    normalizedCovariance: DoubleMatrix,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    autocorrelation: Option[ArDiagnostics] = None
) extends FitBlockResult:
  require(voxelIndices.length == coefficients.voxels, "block voxel indices must match coefficient columns")
  require(residualVariance.length == coefficients.voxels, "block residual variances must match coefficient columns")
  require(standardErrors.predictors == coefficients.predictors, "block standard errors must match coefficient rows")
  require(standardErrors.voxels == coefficients.voxels, "block standard errors must match coefficient columns")
  require(normalizedCovariance.rows == coefficients.predictors, "block covariance rows must match predictors")
  require(normalizedCovariance.cols == coefficients.predictors, "block covariance cols must match predictors")

object DenseFitBlockResult:
  def fromOls(input: FitBlockInput, fit: OlsFit, engine: FitEngine): DenseFitBlockResult =
    DenseFitBlockResult(
      coefficients = fit.coefficients,
      standardErrors = fit.standardErrors,
      normalizedCovariance = fit.normalizedCovariance,
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      engine = engine
    )

  def fromGls(input: FitBlockInput, fit: GlsFit): DenseFitBlockResult =
    DenseFitBlockResult(
      coefficients = fit.coefficients,
      standardErrors = fit.standardErrors,
      normalizedCovariance = fit.normalizedCovariance,
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      engine = FitEngine.GeneralizedLeastSquares,
      autocorrelation = Some(fit.diagnostics)
    )

  def merge(blocks: IndexedSeq[DenseFitBlockResult]): Either[FitError, DenseFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one dense block is required"))
    else
      val first = blocks.head
      validateCompatible(blocks, first).map { _ =>
        DenseFitBlockResult(
          coefficients = CoefficientBlock(bindMatrixColumns(blocks, _.coefficients.value)),
          standardErrors = StandardErrorBlock(bindMatrixColumns(blocks, _.standardErrors.value)),
          normalizedCovariance = DoubleMatrix.unsafe(first.normalizedCovariance.rows, first.normalizedCovariance.cols, first.normalizedCovariance.copyData),
          residualVariance = DoubleVector.unsafe(bindVectors(blocks, _.residualVariance)),
          residualDegreesOfFreedom = first.residualDegreesOfFreedom,
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints,
          engine = first.engine,
          autocorrelation = first.autocorrelation
        )
      }

  private def validateCompatible(
      blocks: IndexedSeq[DenseFitBlockResult],
      first: DenseFitBlockResult
  ): Either[FitError, Unit] =
    var i = 0
    while i < blocks.length do
      val block = blocks(i)
      if block.engine != first.engine then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must use the same engine"))
      if block.timepoints != first.timepoints then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must use the same selected timepoints"))
      if block.residualDegreesOfFreedom != first.residualDegreesOfFreedom then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have the same residual degrees of freedom"))
      if block.coefficients.predictors != first.coefficients.predictors then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have the same predictor count"))
      if block.autocorrelation != first.autocorrelation then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical autocorrelation diagnostics"))
      if !sameMatrix(block.normalizedCovariance, first.normalizedCovariance) then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must share the same normalized covariance"))
      i += 1
    Right(())

  private def sameMatrix(left: DoubleMatrix, right: DoubleMatrix): Boolean =
    left.rows == right.rows &&
      left.cols == right.cols &&
      left.copyData.sameElements(right.copyData)

  private def bindMatrixColumns(
      blocks: IndexedSeq[DenseFitBlockResult],
      matrix: DenseFitBlockResult => DoubleMatrix
  ): DoubleMatrix =
    val rows = matrix(blocks.head).rows
    val cols = blocks.iterator.map(block => matrix(block).cols).sum
    val out = new Array[Double](rows * cols)

    var colOffset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = matrix(blocks(blockIndex))
      require(current.rows == rows, "dense block matrix row mismatch")
      var row = 0
      while row < rows do
        System.arraycopy(current.dataArray, row * current.cols, out, row * cols + colOffset, current.cols)
        row += 1
      colOffset += current.cols
      blockIndex += 1

    DoubleMatrix.unsafe(rows, cols, out)

  private def bindVectors(
      blocks: IndexedSeq[DenseFitBlockResult],
      vector: DenseFitBlockResult => DoubleVector
  ): Array[Double] =
    val length = blocks.iterator.map(block => vector(block).length).sum
    val out = new Array[Double](length)
    var offset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = vector(blocks(blockIndex)).copyData
      System.arraycopy(current, 0, out, offset, current.length)
      offset += current.length
      blockIndex += 1
    out

final case class RunwiseFitBlockResult(
    runs: Vector[RunwiseFmriRunResult],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int]
) extends FitBlockResult:
  require(runs.nonEmpty, "runwise block result must contain at least one run")
  require(runs.forall(_.coefficients.voxels == voxelIndices.length), "runwise block runs must match block voxel count")
  def engine: FitEngine = FitEngine.RunwiseLeastSquares

object RunwiseFitBlockResult:
  def fromRunwise(input: FitBlockInput, fit: RunwiseOlsFit): RunwiseFitBlockResult =
    RunwiseFitBlockResult(
      runs = fit.runs.map { run =>
        RunwiseFmriRunResult(
          runIndex = run.partition.runIndex,
          rowIndices = run.partition.rowIndices,
          timepoints = run.partition.timepoints,
          coefficients = run.fit.coefficients,
          standardErrors = run.fit.standardErrors,
          normalizedCovariance = run.fit.normalizedCovariance,
          residualVariance = run.fit.residualVariance,
          residualDegreesOfFreedom = run.fit.residualDegreesOfFreedom
        )
      },
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints
    )

final case class LssFitBlockResult(
    coefficients: CoefficientBlock,
    trialNames: Vector[String],
    diagnostics: LssDiagnostics,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int]
) extends FitBlockResult:
  require(trialNames.length == coefficients.predictors, "LSS block trial names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "LSS block voxel indices must match coefficient columns")
  def engine: FitEngine = FitEngine.LeastSquaresSeparate

object LssFitBlockResult:
  def fromLss(input: FitBlockInput, fit: LssFit): LssFitBlockResult =
    LssFitBlockResult(
      coefficients = fit.coefficients,
      trialNames = fit.trialNames,
      diagnostics = fit.diagnostics,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints
    )

  def merge(blocks: IndexedSeq[LssFitBlockResult]): Either[FitError, LssFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one LSS block is required"))
    else
      val first = blocks.head
      validateCompatible(blocks, first).map { _ =>
        LssFitBlockResult(
          coefficients = CoefficientBlock(bindCoefficientColumns(blocks)),
          trialNames = first.trialNames,
          diagnostics = first.diagnostics,
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints
        )
      }

  private def validateCompatible(
      blocks: IndexedSeq[LssFitBlockResult],
      first: LssFitBlockResult
  ): Either[FitError, Unit] =
    var i = 0
    while i < blocks.length do
      val block = blocks(i)
      if block.timepoints != first.timepoints then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have the same selected timepoints"))
      if block.trialNames != first.trialNames then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have the same trial names"))
      if block.diagnostics != first.diagnostics then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have identical diagnostics"))
      i += 1
    Right(())

  private def bindCoefficientColumns(blocks: IndexedSeq[LssFitBlockResult]): DoubleMatrix =
    val rows = blocks.head.coefficients.predictors
    val cols = blocks.iterator.map(_.coefficients.voxels).sum
    val out = new Array[Double](rows * cols)

    var colOffset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = blocks(blockIndex).coefficients.value
      require(current.rows == rows, "LSS block coefficient row mismatch")
      var row = 0
      while row < rows do
        System.arraycopy(current.dataArray, row * current.cols, out, row * cols + colOffset, current.cols)
        row += 1
      colOffset += current.cols
      blockIndex += 1

    DoubleMatrix.unsafe(rows, cols, out)

object FitKernel:
  def fitDense(
      input: FitBlockInput,
      engine: FitEngine,
      config: FitConfig = FitConfig()
  ): Either[FitError, DenseFitBlockResult] =
    engine match
      case FitEngine.OrdinaryLeastSquares =>
        Ols
          .fit(input.design, input.response)
          .map(DenseFitBlockResult.fromOls(input, _, engine))

      case FitEngine.GeneralizedLeastSquares =>
        Gls
          .fit(input.design, input.response, input.partitions, config.autocorrelation)
          .map(DenseFitBlockResult.fromGls(input, _))

      case other =>
        Left(FitError.UnsupportedEngine(s"$other does not produce a dense fit block result"))

  def fitRunwise(input: FitBlockInput): Either[FitError, RunwiseFitBlockResult] =
    RunwiseOls
      .fit(input.design, input.response, input.partitions)
      .map(RunwiseFitBlockResult.fromRunwise(input, _))

  def fitLss(input: FitBlockInput): Either[FitError, LssFitBlockResult] =
    input.lssDesign match
      case None =>
        Left(FitError.UnsupportedLssDesign("block input is missing an LSS trial/fixed design"))
      case Some(lss) =>
        LeastSquaresSeparate
          .fit(
            prepared = lss.prepared,
            response = input.response
          )
          .map(LssFitBlockResult.fromLss(input, _))

  def fit(
      input: FitBlockInput,
      engine: FitEngine,
      config: FitConfig = FitConfig()
  ): Either[FitError, FitBlockResult] =
    engine match
      case FitEngine.OrdinaryLeastSquares | FitEngine.GeneralizedLeastSquares =>
        fitDense(input, engine, config)
      case FitEngine.RunwiseLeastSquares =>
        fitRunwise(input)
      case FitEngine.LeastSquaresSeparate =>
        fitLss(input)
      case other =>
        Left(FitError.UnsupportedEngine(other.toString))
