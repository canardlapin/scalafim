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
    inference: CoefficientInference,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    olsDiagnostics: Option[OlsDiagnostics] = None,
    autocorrelation: Option[ArDiagnostics] = None,
    robustDiagnostics: Option[RobustDiagnostics] = None
) extends FitBlockResult:
  require(voxelIndices.length == coefficients.voxels, "block voxel indices must match coefficient columns")
  require(residualVariance.length == coefficients.voxels, "block residual variances must match coefficient columns")
  require(inference.predictors == coefficients.predictors, "block inference must match coefficient rows")
  require(inference.voxels == coefficients.voxels, "block inference must match coefficient columns")

  def standardErrors: StandardErrorBlock = inference.standardErrors
  def normalizedCovariance: DoubleMatrix = inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = inference.covariance
  def inferenceScope: CoefficientInferenceScope = inference.scope

object DenseFitBlockResult:
  def fromOls(input: FitBlockInput, fit: OlsFit, engine: FitEngine): DenseFitBlockResult =
    DenseFitBlockResult(
      coefficients = fit.coefficients,
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        fit.standardErrors,
        fit.coefficientCovariance,
        fit.residualVariance,
        fit.residualDegreesOfFreedom
      ),
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      engine = engine,
      olsDiagnostics = Some(fit.diagnostics)
    )

  def fromGls(
      input: FitBlockInput,
      fit: GlsFit,
      engine: FitEngine = FitEngine.GeneralizedLeastSquares
  ): DenseFitBlockResult =
    DenseFitBlockResult(
      coefficients = fit.coefficients,
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        fit.standardErrors,
        fit.coefficientCovariance,
        fit.residualVariance,
        fit.residualDegreesOfFreedom
      ),
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      engine = engine,
      olsDiagnostics = Some(fit.finalOlsDiagnostics),
      autocorrelation = Some(fit.diagnostics)
    )

  def fromRobust(input: FitBlockInput, fit: RobustFit): DenseFitBlockResult =
    DenseFitBlockResult(
      coefficients = fit.coefficients,
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        fit.standardErrors,
        fit.coefficientCovariance,
        fit.residualVariance,
        fit.residualDegreesOfFreedom
      ),
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      engine = FitEngine.RobustLeastSquares,
      olsDiagnostics = Some(fit.finalOlsDiagnostics),
      autocorrelation = fit.autocorrelation,
      robustDiagnostics = Some(fit.diagnostics)
    )

  def merge(blocks: IndexedSeq[DenseFitBlockResult]): Either[FitError, DenseFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one dense block is required"))
    else
      val first = blocks.head
      for
        autocorrelation <- mergeAutocorrelation(blocks)
        robust <- mergeRobustDiagnostics(blocks)
        inference <- CoefficientInference.mergeByVoxel(blocks.map(_.inference))
        _ <- validateCompatible(blocks, first)
      yield
        DenseFitBlockResult(
          coefficients = CoefficientBlock(bindMatrixColumns(blocks, _.coefficients.value)),
          inference = inference,
          residualVariance = DoubleVector.unsafe(bindVectors(blocks, _.residualVariance)),
          residualDegreesOfFreedom = first.residualDegreesOfFreedom,
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints,
          engine = first.engine,
          olsDiagnostics = first.olsDiagnostics,
          autocorrelation = autocorrelation,
          robustDiagnostics = robust
        )

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
      if block.olsDiagnostics != first.olsDiagnostics then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical OLS diagnostics"))
      i += 1
    Right(())

  private def mergeAutocorrelation(blocks: IndexedSeq[DenseFitBlockResult]): Either[FitError, Option[ArDiagnostics]] =
    val diagnostics = blocks.map(_.autocorrelation)
    if diagnostics.forall(_.isEmpty) then Right(None)
    else if diagnostics.exists(_.isEmpty) then Left(FitError.IncompatibleFitBlocks("autocorrelation diagnostics must be present for every dense block"))
    else ArDiagnostics.merge(diagnostics.flatten).map(Some(_))

  private def mergeRobustDiagnostics(blocks: IndexedSeq[DenseFitBlockResult]): Either[FitError, Option[RobustDiagnostics]] =
    val diagnostics = blocks.map(_.robustDiagnostics)
    if diagnostics.forall(_.isEmpty) then Right(None)
    else if diagnostics.exists(_.isEmpty) then Left(FitError.IncompatibleFitBlocks("robust diagnostics must be present for every dense block"))
    else RobustDiagnostics.merge(diagnostics.flatten).map(Some(_))

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
          residualDegreesOfFreedom = run.fit.residualDegreesOfFreedom,
          olsDiagnostics = run.fit.diagnostics
        )
      },
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints
    )

  def merge(blocks: IndexedSeq[RunwiseFitBlockResult]): Either[FitError, RunwiseFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one runwise block is required"))
    else
      val first = blocks.head
      validateCompatible(blocks, first).map { _ =>
        RunwiseFitBlockResult(
          runs = mergeRuns(blocks),
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints
        )
      }

  private def validateCompatible(
      blocks: IndexedSeq[RunwiseFitBlockResult],
      first: RunwiseFitBlockResult
  ): Either[FitError, Unit] =
    var blockIndex = 0
    while blockIndex < blocks.length do
      val block = blocks(blockIndex)
      if block.timepoints != first.timepoints then
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have the same selected timepoints"))
      if block.runs.length != first.runs.length then
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have the same run count"))

      var runIndex = 0
      while runIndex < block.runs.length do
        val run = block.runs(runIndex)
        val firstRun = first.runs(runIndex)
        if run.runIndex != firstRun.runIndex then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must preserve run order"))
        if run.rowIndices != firstRun.rowIndices then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run row indices"))
        if run.timepoints != firstRun.timepoints then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run timepoints"))
        if run.residualDegreesOfFreedom != firstRun.residualDegreesOfFreedom then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run residual degrees of freedom"))
        if run.coefficients.predictors != firstRun.coefficients.predictors then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run predictor counts"))
        if run.olsDiagnostics != firstRun.olsDiagnostics then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run OLS diagnostics"))
        if !sameMatrix(run.normalizedCovariance, firstRun.normalizedCovariance) then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must share run normalized covariance"))
        runIndex += 1

      blockIndex += 1
    Right(())

  private def mergeRuns(blocks: IndexedSeq[RunwiseFitBlockResult]): Vector[RunwiseFmriRunResult] =
    val out = Vector.newBuilder[RunwiseFmriRunResult]
    val first = blocks.head
    var runIndex = 0
    while runIndex < first.runs.length do
      val firstRun = first.runs(runIndex)
      val currentRuns = blocks.map(_.runs(runIndex))
      out += RunwiseFmriRunResult(
        runIndex = firstRun.runIndex,
        rowIndices = firstRun.rowIndices,
        timepoints = firstRun.timepoints,
        coefficients = CoefficientBlock(bindMatrixColumns(currentRuns, _.coefficients.value)),
        standardErrors = StandardErrorBlock(bindMatrixColumns(currentRuns, _.standardErrors.value)),
        normalizedCovariance = DoubleMatrix.unsafe(firstRun.normalizedCovariance.rows, firstRun.normalizedCovariance.cols, firstRun.normalizedCovariance.copyData),
        residualVariance = DoubleVector.unsafe(bindVectors(currentRuns, _.residualVariance)),
        residualDegreesOfFreedom = firstRun.residualDegreesOfFreedom,
        olsDiagnostics = firstRun.olsDiagnostics
      )
      runIndex += 1
    out.result()

  private def bindMatrixColumns(
      runs: IndexedSeq[RunwiseFmriRunResult],
      matrix: RunwiseFmriRunResult => DoubleMatrix
  ): DoubleMatrix =
    val rows = matrix(runs.head).rows
    val cols = runs.iterator.map(run => matrix(run).cols).sum
    val out = new Array[Double](rows * cols)
    var colOffset = 0
    var runIndex = 0
    while runIndex < runs.length do
      val current = matrix(runs(runIndex))
      require(current.rows == rows, "runwise block matrix row mismatch")
      var row = 0
      while row < rows do
        System.arraycopy(current.dataArray, row * current.cols, out, row * cols + colOffset, current.cols)
        row += 1
      colOffset += current.cols
      runIndex += 1
    DoubleMatrix.unsafe(rows, cols, out)

  private def bindVectors(
      runs: IndexedSeq[RunwiseFmriRunResult],
      vector: RunwiseFmriRunResult => DoubleVector
  ): Array[Double] =
    val length = runs.iterator.map(run => vector(run).length).sum
    val out = new Array[Double](length)
    var offset = 0
    var runIndex = 0
    while runIndex < runs.length do
      val current = vector(runs(runIndex)).copyData
      System.arraycopy(current, 0, out, offset, current.length)
      offset += current.length
      runIndex += 1
    out

  private def sameMatrix(left: DoubleMatrix, right: DoubleMatrix): Boolean =
    left.rows == right.rows &&
      left.cols == right.cols &&
      left.copyData.sameElements(right.copyData)

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
          .fit(input.design, input.response, input.partitions, config.autocorrelation, input.voxelIndices)
          .map(DenseFitBlockResult.fromGls(input, _))

      case FitEngine.RobustLeastSquares =>
        Robust
          .fit(input.design, input.response, input.partitions, config.robust, robustAutocorrelation(config))
          .map(DenseFitBlockResult.fromRobust(input, _))

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
      case FitEngine.OrdinaryLeastSquares | FitEngine.GeneralizedLeastSquares | FitEngine.RobustLeastSquares =>
        fitDense(input, engine, config)
      case FitEngine.RunwiseLeastSquares =>
        fitRunwise(input)
      case FitEngine.LeastSquaresSeparate =>
        fitLss(input)
      case other =>
        Left(FitError.UnsupportedEngine(other.toString))

  private def robustAutocorrelation(config: FitConfig): Option[scalafim.fmri.model.ArOptions] =
    if config.robust.reestimateAutocorrelation then Some(config.autocorrelation) else None
