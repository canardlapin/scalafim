package scalafim.fmri.fit

import scalafim.fmri.model.{FitConfig, FitEngine}
import scalafim.fmri.design.{CoefficientAxis, RunCoefficientProjection, RunIndex as DesignRunIndex}
import gale.linalg.{DMat, DVec, Matrix, Vec}

final case class LssBlockDesign(
    prepared: LssPreparedDesign,
    trialColumns: Vector[Int] = Vector.empty
):
  def timepoints: Int = prepared.timepoints
  def trials: Int = prepared.trials
  require(trialColumns.isEmpty || trialColumns.length == trials, "LSS trial column metadata must match trial count")
  require(trialColumns.distinct.length == trialColumns.length, "LSS trial column metadata must be unique")
  require(trialColumns.forall(_ >= 0), "LSS trial column metadata must be non-negative")

final case class FitBlockInput(
    design: DesignMatrix,
    response: ResponseBlock,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    partitions: Vector[RunPartition] = Vector.empty,
    lssDesign: Option[LssBlockDesign] = None,
    coefficientAxis: Option[CoefficientAxis] = None,
    runwiseProjections: Vector[RunCoefficientProjection] = Vector.empty,
    preparationProvenance: Option[ResponsePreparationProvenance] = None,
    voxelStatuses: Option[Vector[VoxelFitStatus]] = None,
    fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(design.timepoints == response.timepoints, "block design and response rows must match")
  require(timepoints.length == design.timepoints, "block timepoint indices must match design rows")
  require(voxelIndices.length == response.voxels, "block voxel indices must match response columns")
  require(voxelIndices.nonEmpty, "fit block must contain at least one voxel")
  require(timepoints.nonEmpty, "fit block must contain at least one timepoint")
  require(voxelStatuses.forall(_.length == response.voxels), "voxel statuses must match response columns")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "fit block input")
  coefficientAxis.foreach { axis =>
    require(axis.predictors == design.predictors, "coefficient axis must match design predictors")
  }
  partitions.foreach { partition =>
    require(partition.rowIndices.forall(row => row >= 0 && row < design.timepoints), "run partition row out of block bounds")
  }
  runwiseProjections.foreach { projection =>
    require(
      partitions.exists(_.runIndex == projection.run.oneBased - 1),
      s"runwise projection ${projection.run.oneBased} has no matching partition"
    )
  }
  lssDesign.foreach { lss =>
    require(lss.timepoints == design.timepoints, "LSS design rows must match block design rows")
  }

  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)
  def resolvedVoxelStatuses: Vector[VoxelFitStatus] =
    voxelStatuses.getOrElse(VoxelFitStatus.classify(response))

sealed trait FitBlockResult:
  def voxelIndices: Vector[Int]
  def timepoints: Vector[Int]
  def engine: FitEngine
  def coefficientAxis: Option[CoefficientAxis] = None
  def preparationProvenance: Option[ResponsePreparationProvenance] = None
  def fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
  def voxels: Int = voxelIndices.length
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

final case class DenseFitBlockResult(
    coefficients: CoefficientBlock,
    inference: CoefficientInference,
    residualVariance: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    olsDiagnostics: Option[OlsDiagnostics] = None,
    autocorrelation: Option[ArDiagnostics] = None,
    robustDiagnostics: Option[RobustDiagnostics] = None,
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    voxelStatuses: Option[Vector[VoxelFitStatus]] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FitBlockResult:
  require(voxelIndices.length == coefficients.voxels, "block voxel indices must match coefficient columns")
  require(residualVariance.length == coefficients.voxels, "block residual variances must match coefficient columns")
  require(inference.predictors == coefficients.predictors, "block inference must match coefficient rows")
  require(inference.voxels == coefficients.voxels, "block inference must match coefficient columns")
  require(voxelStatuses.forall(_.length == coefficients.voxels), "block voxel statuses must match coefficient columns")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "dense fit block")

  def standardErrors: StandardErrorBlock = inference.standardErrors
  def normalizedCovariance: DMat = inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = inference.covariance
  def inferenceScope: CoefficientInferenceScope = inference.scope
  def rankReport: Option[RankDiagnostics] = olsDiagnostics.map(_.rankReport)
  def resolvedVoxelStatuses: Vector[VoxelFitStatus] =
    voxelStatuses.getOrElse(Vector.fill(coefficients.voxels)(VoxelFitStatus.Estimable))

  def structuralRankReport: Either[FitError, StructuralRankReport] =
    for
      diagnostics <- olsDiagnostics.toRight(FitError.MissingStructuralIdentity("rank diagnostics"))
      axis <- coefficientAxis.toRight(FitError.MissingStructuralIdentity("rank diagnostics structural coefficient axis"))
      report <- diagnostics.rankReport.bind(axis)
    yield report

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
      olsDiagnostics = Some(fit.diagnostics),
      coefficientAxis = input.coefficientAxis,
      preparationProvenance = input.preparationProvenance,
      voxelStatuses = Some(VoxelFitStatus.refine(input.resolvedVoxelStatuses, fit.residualVariance)),
      fitExclusions = input.fitExclusions
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
      autocorrelation = Some(fit.diagnostics),
      coefficientAxis = input.coefficientAxis,
      preparationProvenance = input.preparationProvenance,
      voxelStatuses = Some(VoxelFitStatus.refine(input.resolvedVoxelStatuses, fit.residualVariance)),
      fitExclusions = input.fitExclusions
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
      robustDiagnostics = Some(fit.diagnostics),
      coefficientAxis = input.coefficientAxis,
      preparationProvenance = input.preparationProvenance,
      voxelStatuses = Some(VoxelFitStatus.refine(input.resolvedVoxelStatuses, fit.residualVariance)),
      fitExclusions = input.fitExclusions
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
        exclusions <- VoxelInferenceExclusions.combine(blocks.iterator.flatMap(_.fitExclusions))
      yield
        DenseFitBlockResult(
          coefficients = CoefficientBlock(bindMatrixColumns(blocks, _.coefficients.value)),
          inference = inference,
          residualVariance = bindVectors(blocks, _.residualVariance),
          residualDegreesOfFreedom = first.residualDegreesOfFreedom,
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints,
          engine = first.engine,
          olsDiagnostics = first.olsDiagnostics,
          autocorrelation = autocorrelation,
          robustDiagnostics = robust,
          coefficientAxis = first.coefficientAxis,
          preparationProvenance = first.preparationProvenance,
          voxelStatuses = Some(blocks.iterator.flatMap(_.resolvedVoxelStatuses).toVector),
          fitExclusions = exclusions
        )

  private def validateCompatible(
      blocks: IndexedSeq[DenseFitBlockResult],
      first: DenseFitBlockResult
  ): Either[FitError, Unit] =
    validateAxis(blocks, first, "dense") match
      case Left(error) => return Left(error)
      case Right(_) => ()
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
      if block.inference.scope != first.inference.scope then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical inference scopes"))
      if block.preparationProvenance != first.preparationProvenance then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical response-preparation provenance"))
      if !compatibleOlsDiagnostics(block.olsDiagnostics, first.olsDiagnostics) then
        return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical OLS diagnostics"))
      i += 1
    Right(())

  private def compatibleOlsDiagnostics(
      left: Option[OlsDiagnostics],
      right: Option[OlsDiagnostics]
  ): Boolean =
    (left, right) match
      case (None, None)       => true
      case (Some(a), Some(b)) => a.structurallyCompatible(b)
      case _                  => false

  private def validateAxis(
      blocks: IndexedSeq[DenseFitBlockResult],
      first: DenseFitBlockResult,
      label: String
  ): Either[FitError, Unit] =
    first.coefficientAxis match
      case None if blocks.exists(_.coefficientAxis.nonEmpty) =>
        Left(FitError.IncompatibleFitBlocks(s"all $label blocks must either carry a coefficient axis or omit it"))
      case Some(axis) if blocks.exists(block => block.coefficientAxis.forall(other => !axis.structurallyCompatible(other))) =>
        Left(FitError.IncompatibleFitBlocks(s"all $label blocks must carry the same structural coefficient axis"))
      case _ => Right(())

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
      matrix: DenseFitBlockResult => DMat
  ): DMat =
    val rows = matrix(blocks.head).rows
    val cols = blocks.iterator.map(block => matrix(block).cols).sum
    val out = Matrix.newBuilder(rows, cols)

    var colOffset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = matrix(blocks(blockIndex))
      require(current.rows == rows, "dense block matrix row mismatch")
      var row = 0
      while row < rows do
        var col = 0
        while col < current.cols do
          out(row, colOffset + col) = current(row, col)
          col += 1
        row += 1
      colOffset += current.cols
      blockIndex += 1

    out.result()

  private def bindVectors(
      blocks: IndexedSeq[DenseFitBlockResult],
      vector: DenseFitBlockResult => DVec
  ): DVec =
    val length = blocks.iterator.map(block => vector(block).length).sum
    val out = Vec.newBuilder(length)
    var offset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = vector(blocks(blockIndex))
      var index = 0
      while index < current.length do
        out(offset + index) = current(index)
        index += 1
      offset += current.length
      blockIndex += 1
    out.result()

final case class RunwiseFitBlockResult(
    runs: Vector[RunwiseFmriRunResult],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FitBlockResult:
  require(runs.nonEmpty, "runwise block result must contain at least one run")
  require(runs.forall(_.coefficients.voxels == voxelIndices.length), "runwise block runs must match block voxel count")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "runwise fit block")
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
          olsDiagnostics = run.fit.diagnostics,
          coefficientAxis = run.projection.map(_.axis).orElse {
            input.coefficientAxis.flatMap { axis =>
              axis.forRunwiseCoefficient(DesignRunIndex.unsafeOneBased(run.partition.runIndex + 1)).toOption
            }
          },
          sourceColumnIndices = run.projection.map(_.sourceColumnIndices).getOrElse(Vector.empty),
          projection = run.projection,
          voxelStatuses = Some(
            VoxelFitStatus.refine(
              VoxelFitStatus.classify(input.response.value, run.partition.rowIndices),
              run.fit.residualVariance
            )
          )
        )
      },
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      coefficientAxis = input.coefficientAxis,
      preparationProvenance = input.preparationProvenance,
      fitExclusions = input.fitExclusions
    )

  def merge(blocks: IndexedSeq[RunwiseFitBlockResult]): Either[FitError, RunwiseFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one runwise block is required"))
    else
      val first = blocks.head
      for
        _ <- validateCompatible(blocks, first)
        exclusions <- VoxelInferenceExclusions.combine(blocks.iterator.flatMap(_.fitExclusions))
      yield
        RunwiseFitBlockResult(
          runs = mergeRuns(blocks),
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints,
          coefficientAxis = first.coefficientAxis,
          preparationProvenance = first.preparationProvenance,
          fitExclusions = exclusions
        )

  private def validateCompatible(
      blocks: IndexedSeq[RunwiseFitBlockResult],
      first: RunwiseFitBlockResult
  ): Either[FitError, Unit] =
    first.coefficientAxis match
      case None if blocks.exists(_.coefficientAxis.nonEmpty) =>
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must either carry a coefficient axis or omit it"))
      case Some(axis) if blocks.exists(block => block.coefficientAxis.forall(other => !axis.structurallyCompatible(other))) =>
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must carry the same structural coefficient axis"))
      case _ => ()
    var blockIndex = 0
    while blockIndex < blocks.length do
      val block = blocks(blockIndex)
      if block.timepoints != first.timepoints then
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have the same selected timepoints"))
      if block.preparationProvenance != first.preparationProvenance then
        return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical response-preparation provenance"))
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
        if run.sourceColumns != firstRun.sourceColumns then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical local source-column mappings"))
        (firstRun.coefficientAxis, run.coefficientAxis) match
          case (None, None) => ()
          case (Some(expected), Some(actual)) if expected.structurallyCompatible(actual) => ()
          case _ => return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical local coefficient axes"))
        if run.projection.map(_.sourceColumnIndices) != firstRun.projection.map(_.sourceColumnIndices) then
          return Left(FitError.IncompatibleFitBlocks("all runwise blocks must have identical run projection mappings"))
        if !run.olsDiagnostics.structurallyCompatible(firstRun.olsDiagnostics) then
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
        normalizedCovariance = Matrix.tabulate(firstRun.normalizedCovariance.rows, firstRun.normalizedCovariance.cols)(firstRun.normalizedCovariance.apply),
        residualVariance = bindVectors(currentRuns, _.residualVariance),
        residualDegreesOfFreedom = firstRun.residualDegreesOfFreedom,
        olsDiagnostics = firstRun.olsDiagnostics,
        coefficientAxis = firstRun.coefficientAxis,
        sourceColumnIndices = firstRun.sourceColumns,
        projection = firstRun.projection,
        voxelStatuses = Some(currentRuns.iterator.flatMap(_.resolvedVoxelStatuses).toVector)
      )
      runIndex += 1
    out.result()

  private def bindMatrixColumns(
      runs: IndexedSeq[RunwiseFmriRunResult],
      matrix: RunwiseFmriRunResult => DMat
  ): DMat =
    val rows = matrix(runs.head).rows
    val cols = runs.iterator.map(run => matrix(run).cols).sum
    val out = Matrix.newBuilder(rows, cols)
    var colOffset = 0
    var runIndex = 0
    while runIndex < runs.length do
      val current = matrix(runs(runIndex))
      require(current.rows == rows, "runwise block matrix row mismatch")
      var row = 0
      while row < rows do
        var col = 0
        while col < current.cols do
          out(row, colOffset + col) = current(row, col)
          col += 1
        row += 1
      colOffset += current.cols
      runIndex += 1
    out.result()

  private def bindVectors(
      runs: IndexedSeq[RunwiseFmriRunResult],
      vector: RunwiseFmriRunResult => DVec
  ): DVec =
    val length = runs.iterator.map(run => vector(run).length).sum
    val out = Vec.newBuilder(length)
    var offset = 0
    var runIndex = 0
    while runIndex < runs.length do
      val current = vector(runs(runIndex))
      var index = 0
      while index < current.length do
        out(offset + index) = current(index)
        index += 1
      offset += current.length
      runIndex += 1
    out.result()

  private def sameMatrix(left: DMat, right: DMat): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          if left(row, col) != right(row, col) then return false
          col += 1
        row += 1
      true

final case class LssFitBlockResult(
    coefficients: CoefficientBlock,
    trialNames: Vector[String],
    diagnostics: LssDiagnostics,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FitBlockResult:
  require(trialNames.length == coefficients.predictors, "LSS block trial names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "LSS block voxel indices must match coefficient columns")
  require(coefficientAxis.forall(_.predictors == coefficients.predictors), "LSS coefficient axis must match coefficient rows")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "LSS fit block")
  def engine: FitEngine = FitEngine.LeastSquaresSeparate

object LssFitBlockResult:
  def fromLss(input: FitBlockInput, fit: LssFit): LssFitBlockResult =
    LssFitBlockResult(
      coefficients = fit.coefficients,
      trialNames = fit.trialNames,
      diagnostics = fit.diagnostics,
      voxelIndices = input.voxelIndices,
      timepoints = input.timepoints,
      coefficientAxis =
        for
          axis <- input.coefficientAxis
          lss <- input.lssDesign
          if lss.trialColumns.nonEmpty
          selected <- axis.select(lss.trialColumns).toOption
        yield selected,
      preparationProvenance = input.preparationProvenance,
      fitExclusions = input.fitExclusions
    )

  def merge(blocks: IndexedSeq[LssFitBlockResult]): Either[FitError, LssFitBlockResult] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one LSS block is required"))
    else
      val first = blocks.head
      for
        _ <- validateCompatible(blocks, first)
        exclusions <- VoxelInferenceExclusions.combine(blocks.iterator.flatMap(_.fitExclusions))
      yield
        LssFitBlockResult(
          coefficients = CoefficientBlock(bindCoefficientColumns(blocks)),
          trialNames = first.trialNames,
          diagnostics = first.diagnostics,
          voxelIndices = blocks.iterator.flatMap(_.voxelIndices).toVector,
          timepoints = first.timepoints,
          coefficientAxis = first.coefficientAxis,
          preparationProvenance = first.preparationProvenance,
          fitExclusions = exclusions
        )

  private def validateCompatible(
      blocks: IndexedSeq[LssFitBlockResult],
      first: LssFitBlockResult
  ): Either[FitError, Unit] =
    first.coefficientAxis match
      case None if blocks.exists(_.coefficientAxis.nonEmpty) =>
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must either carry a coefficient axis or omit it"))
      case Some(axis) if blocks.exists(block => block.coefficientAxis.forall(other => !axis.structurallyCompatible(other))) =>
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must carry the same structural coefficient axis"))
      case _ => ()
    var i = 0
    while i < blocks.length do
      val block = blocks(i)
      if block.timepoints != first.timepoints then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have the same selected timepoints"))
      if block.preparationProvenance != first.preparationProvenance then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have identical response-preparation provenance"))
      if block.trialNames != first.trialNames then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have the same trial names"))
      if block.diagnostics != first.diagnostics then
        return Left(FitError.IncompatibleFitBlocks("all LSS blocks must have identical diagnostics"))
      i += 1
    Right(())

  private def bindCoefficientColumns(blocks: IndexedSeq[LssFitBlockResult]): DMat =
    val rows = blocks.head.coefficients.predictors
    val cols = blocks.iterator.map(_.coefficients.voxels).sum
    val out = Matrix.newBuilder(rows, cols)

    var colOffset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = blocks(blockIndex).coefficients.value
      require(current.rows == rows, "LSS block coefficient row mismatch")
      var row = 0
      while row < rows do
        var col = 0
        while col < current.cols do
          out(row, colOffset + col) = current(row, col)
          col += 1
        row += 1
      colOffset += current.cols
      blockIndex += 1

    out.result()

/** A chunk whose selected response columns were all excluded before numerical
  * fitting. It participates only in chunk reduction so healthy chunks can
  * complete; no numerical result is fabricated for these voxels.
  */
private[fit] final case class ExcludedFitBlockResult(
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    override val fitExclusions: Vector[VoxelInferenceExclusion]
) extends FitBlockResult:
  require(fitExclusions.nonEmpty, "excluded fit block must contain exclusions")
  require(voxelIndices == fitExclusions.map(_.voxelIndex), "excluded fit block voxel indices must match exclusions")
  require(timepoints.nonEmpty, "excluded fit block must retain selected timepoints")

object FitKernel:
  /** Lift numerical predictor positions to the structural axis at a public fit boundary. */
  private[fit] def bindRankFailure(error: FitError, axis: CoefficientAxis): FitError =
    error match
      case FitError.RankDeficientDesign(report) =>
        report.bind(axis) match
          case Right(structural) => FitError.StructuralRankDeficientDesign(structural)
          case Left(binding)     => binding
      case FitError.RunwiseFitFailed(runIndex, cause) =>
        FitError.RunwiseFitFailed(runIndex, bindRankFailure(cause, axis))
      case FitError.ChunkFailed(chunkOrdinal, cause) =>
        FitError.ChunkFailed(chunkOrdinal, bindRankFailure(cause, axis))
      case other => other

  private def bindInputRankFailure(error: FitError, input: FitBlockInput): FitError =
    input.coefficientAxis match
      case Some(axis) => bindRankFailure(error, axis)
      case None       => error

  def fitDense(
      input: FitBlockInput,
      engine: FitEngine,
      config: FitConfig = FitConfig()
  ): Either[FitError, DenseFitBlockResult] =
    engine match
      case FitEngine.OrdinaryLeastSquares =>
        Ols
          .fit(input.design, input.response)
          .left
          .map(error => bindInputRankFailure(error, input))
          .map(DenseFitBlockResult.fromOls(input, _, engine))

      case FitEngine.GeneralizedLeastSquares =>
        Gls
          .fit(input.design, input.response, input.partitions, config.autocorrelation, input.voxelIndices)
          .left
          .map(error => bindInputRankFailure(error, input))
          .map(DenseFitBlockResult.fromGls(input, _))

      case FitEngine.RobustLeastSquares =>
        Robust
          .fit(input.design, input.response, input.partitions, config.robust, robustAutocorrelation(config))
          .left
          .map(error => bindInputRankFailure(error, input))
          .map(DenseFitBlockResult.fromRobust(input, _))

      case other =>
        Left(FitError.UnsupportedEngine(s"$other does not produce a dense fit block result"))

  def fitRunwise(input: FitBlockInput): Either[FitError, RunwiseFitBlockResult] =
    RunwiseOls
      .fit(input.design, input.response, input.partitions, input.runwiseProjections)
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
