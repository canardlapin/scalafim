package scalafim.fmri.fit

import scalafim.fmri.ar.{ArEstimation, ArFitOptions, ArOrder as ArFitOrder, NoisePooling, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{ArCoefficientSpec, ArOptions, AutocorrelationConfig, RobustOptions, RobustPsi, ScaleScope}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

enum RobustWeightTopology:
  case RowWise

final case class RobustScaleEstimates(
    scope: ScaleScope,
    labels: Vector[String],
    values: DoubleMatrix
):
  require(labels.nonEmpty, "robust scale labels must be non-empty")
  require(values.rows == labels.length, "robust scale rows must match labels")
  require(values.cols > 0, "robust scale estimates must contain at least one column")
  require(values.copyData.forall(value => value > 0.0 && value.isFinite), "robust scale estimates must be positive and finite")

final case class RobustDiagnostics(
    psi: RobustPsi,
    scaleScope: ScaleScope,
    iterations: Int,
    converged: Boolean,
    tolerance: Double,
    maxCoefficientDelta: Double,
    scaleEstimates: RobustScaleEstimates,
    weightTopology: RobustWeightTopology,
    weights: DoubleMatrix,
    sharedNormalizedCovariance: Boolean
):
  require(iterations >= 0, "robust iterations must be non-negative")
  require(tolerance > 0.0 && tolerance.isFinite, "robust tolerance must be positive and finite")
  require(maxCoefficientDelta >= 0.0 && maxCoefficientDelta.isFinite, "robust coefficient delta must be non-negative and finite")
  require(weights.rows > 0 && weights.cols > 0, "robust weights must be non-empty")
  require(weights.copyData.forall(value => value >= 0.0 && value.isFinite), "robust weights must be non-negative and finite")
  require(scaleEstimates.scope == scaleScope, "robust scale estimate scope must match diagnostics")
  weightTopology match
    case RobustWeightTopology.RowWise =>
      require(weights.cols == 1, "row-wise robust diagnostics must carry one weight column")

object RobustDiagnostics:
  def merge(blocks: IndexedSeq[RobustDiagnostics]): Either[FitError, RobustDiagnostics] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one robust diagnostics block is required"))
    else
      val first = blocks.head
      validateCompatible(blocks, first).flatMap { _ =>
        for
          scales <- mergeScales(blocks.map(_.scaleEstimates))
        yield RobustDiagnostics(
          psi = first.psi,
          scaleScope = first.scaleScope,
          iterations = blocks.map(_.iterations).max,
          converged = blocks.forall(_.converged),
          tolerance = first.tolerance,
          maxCoefficientDelta = blocks.map(_.maxCoefficientDelta).max,
          scaleEstimates = scales,
          weightTopology = first.weightTopology,
          weights = mergeWeights(blocks),
          sharedNormalizedCovariance = blocks.forall(_.sharedNormalizedCovariance)
        )
      }

  private def validateCompatible(
      blocks: IndexedSeq[RobustDiagnostics],
      first: RobustDiagnostics
  ): Either[FitError, Unit] =
    var i = 0
    while i < blocks.length do
      val block = blocks(i)
      if block.psi != first.psi then
        return Left(FitError.IncompatibleFitBlocks("robust chunks must share psi"))
      if block.scaleScope != first.scaleScope then
        return Left(FitError.IncompatibleFitBlocks("robust chunks must share scale scope"))
      if block.weightTopology != first.weightTopology then
        return Left(FitError.IncompatibleFitBlocks("robust chunks must share weight topology"))
      if block.tolerance != first.tolerance then
        return Left(FitError.IncompatibleFitBlocks("robust chunks must share convergence tolerance"))
      if block.weights.rows != first.weights.rows then
        return Left(FitError.IncompatibleFitBlocks("robust chunks must share selected timepoints"))
      if block.weightTopology == RobustWeightTopology.RowWise && !sameMatrix(block.weights, first.weights) then
        return Left(FitError.IncompatibleFitBlocks("row-wise robust chunks must share the prepared weight vector"))
      i += 1
    Right(())

  private def mergeScales(scales: IndexedSeq[RobustScaleEstimates]): Either[FitError, RobustScaleEstimates] =
    val first = scales.head
    var i = 0
    while i < scales.length do
      val current = scales(i)
      if current.scope != first.scope then
        return Left(FitError.IncompatibleFitBlocks("robust scale estimates must share scope"))
      if current.labels != first.labels then
        return Left(FitError.IncompatibleFitBlocks("robust scale estimates must share labels"))
      i += 1
    Right(RobustScaleEstimates(first.scope, first.labels, bindMatrixColumns(scales, _.values)))

  private def mergeWeights(blocks: IndexedSeq[RobustDiagnostics]): DoubleMatrix =
    blocks.head.weightTopology match
      case RobustWeightTopology.RowWise =>
        val first = blocks.head.weights
        DoubleMatrix.unsafe(first.rows, first.cols, first.copyData)

  private def sameMatrix(left: DoubleMatrix, right: DoubleMatrix): Boolean =
    left.rows == right.rows &&
      left.cols == right.cols &&
      left.copyData.sameElements(right.copyData)

  private def bindMatrixColumns[A](blocks: IndexedSeq[A], matrix: A => DoubleMatrix): DoubleMatrix =
    val rows = matrix(blocks.head).rows
    val cols = blocks.iterator.map(block => matrix(block).cols).sum
    val out = new Array[Double](rows * cols)

    var colOffset = 0
    var blockIndex = 0
    while blockIndex < blocks.length do
      val current = matrix(blocks(blockIndex))
      require(current.rows == rows, "robust matrix rows must match")
      var row = 0
      while row < rows do
        System.arraycopy(current.dataArray, row * current.cols, out, row * cols + colOffset, current.cols)
        row += 1
      colOffset += current.cols
      blockIndex += 1

    DoubleMatrix.unsafe(rows, cols, out)

final case class RobustFit(
    coefficients: CoefficientBlock,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    normalizedCovariance: DoubleMatrix,
    standardErrors: StandardErrorBlock,
    diagnostics: RobustDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics,
    finalOlsDiagnostics: OlsDiagnostics,
    coefficientCovariance: CoefficientCovariance,
    autocorrelation: Option[ArDiagnostics] = None,
    private[fit] val whiteningPlan: Option[WhiteningPlan] = None
):
  require(coefficientCovariance.predictors == coefficients.predictors, "robust coefficient covariance must match coefficient rows")
  require(coefficientCovariance.validateVoxelCount(coefficients.voxels).isRight, "robust coefficient covariance must be shared or match voxel count")
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels

final case class RobustPrepared(
    design: DesignMatrix,
    partitions: Vector[RunPartition],
    selectedVoxelIndices: Vector[Int],
    diagnostics: RobustDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics,
    autocorrelation: Option[ArDiagnostics],
    private[fit] val whiteningPlan: Option[WhiteningPlan]
):
  require(selectedVoxelIndices.nonEmpty, "robust prepared context must contain selected voxels")
  require(diagnostics.scaleEstimates.values.cols == selectedVoxelIndices.length, "robust scale components must match selected voxels")

object Robust:
  private val DefaultTolerance = 1e-8
  private val Epsilon = 1e-12
  private val MadScale = 1.4826

  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      autocorrelation: Option[ArOptions] = None
  ): Either[FitError, RobustFit] =
    for
      initial <- Ols.fit(design, response)
      fit <- fitWithoutAutocorrelation(design, response, partitions, options, initial)
      finalFit <-
        if options.reestimateAutocorrelation then
          fitWithAutocorrelation(design, response, partitions, options, autocorrelation, fit)
        else Right(fit)
    yield finalFit

  def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      selectedVoxelIndices: Vector[Int],
      options: RobustOptions,
      autocorrelation: Option[ArOptions] = None
  ): Either[FitError, RobustPrepared] =
    for
      fit <- Robust.fit(design, response, partitions, options, autocorrelation)
    yield RobustPrepared(
      design = design,
      partitions = effectivePartitions(response.timepoints, partitions),
      selectedVoxelIndices = selectedVoxelIndices,
      diagnostics = fit.diagnostics,
      initialOlsDiagnostics = fit.initialOlsDiagnostics,
      autocorrelation = fit.autocorrelation,
      whiteningPlan = fit.whiteningPlan
    )

  def fitPrepared(
      prepared: RobustPrepared,
      response: ResponseBlock,
      voxelIndices: Vector[Int]
  ): Either[FitError, RobustFit] =
    for
      diagnostics <- subsetDiagnostics(prepared.diagnostics, prepared.selectedVoxelIndices, voxelIndices)
      input <- preparedInput(prepared, response)
      (design, fitResponse) = input
      initial <- Ols.fit(design, fitResponse)
      fit <- weightedLeastSquares(design, fitResponse, diagnostics.weights)
    yield RobustFit(
      coefficients = fit.coefficients,
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      normalizedCovariance = fit.normalizedCovariance,
      standardErrors = fit.standardErrors,
      diagnostics = diagnostics,
      initialOlsDiagnostics = initial.diagnostics,
      finalOlsDiagnostics = fit.diagnostics,
      autocorrelation = prepared.autocorrelation,
      whiteningPlan = prepared.whiteningPlan,
      coefficientCovariance = fit.coefficientCovariance
    )

  private def fitWithoutAutocorrelation(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      initial: OlsFit
  ): Either[FitError, RobustFit] =
    options.psi match
      case RobustPsi.Disabled =>
        disabledFit(design, response, partitions, options, initial)
      case RobustPsi.Huber(_) | RobustPsi.Bisquare(_) =>
        iterativelyReweightedFit(design, response, partitions, options, initial)

  private def fitWithAutocorrelation(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      autocorrelation: Option[ArOptions],
      firstRobust: RobustFit
  ): Either[FitError, RobustFit] =
    for
      arOptions <- autocorrelation.toRight(FitError.UnsupportedRobust("robust AR re-estimation requires an AR autocorrelation policy"))
      config <- Gls.autocorrelationConfig(arOptions)
      _ <- validateRobustAutocorrelation(config)
      _ <- Gls.validatePartitions(partitions)
      segments <- Gls.timeSegments(partitions, arOptions.censoredTimepoints)
      plan <- iterateRobustWhitening(
        design = design,
        response = response,
        partitions = partitions,
        options = options.copy(reestimateAutocorrelation = false),
        initialCoefficients = firstRobust.coefficients.value,
        segments = segments,
        config = config
      )
      input <- whitenedInput(plan, design, response)
      (whitenedDesign, whitenedResponse) = input
      initial <- Ols.fit(whitenedDesign, whitenedResponse)
      fit <- fitWithoutAutocorrelation(whitenedDesign, whitenedResponse, partitions, options.copy(reestimateAutocorrelation = false), initial)
      arDiagnostics = Gls.diagnostics(
        GlsWhitening.Shared(plan),
        partitions,
        method = "robust-estimated",
        iterations = Gls.diagnosticIterations(config)
      )
    yield fit.copy(
      autocorrelation = Some(arDiagnostics),
      whiteningPlan = Some(plan),
      initialOlsDiagnostics = firstRobust.initialOlsDiagnostics
    )

  private def validateRobustAutocorrelation(config: AutocorrelationConfig): Either[FitError, Unit] =
    if config.voxelwise then
      Left(FitError.UnsupportedRobust("robust AR re-estimation currently requires shared or run-pooled AR"))
    else
      config.coefficients match
        case ArCoefficientSpec.Estimate =>
          Right(())
        case _ =>
          Left(FitError.UnsupportedRobust("robust AR re-estimation requires estimated AR coefficients"))

  private def iterateRobustWhitening(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      initialCoefficients: DoubleMatrix,
      segments: Vector[TimeSegment],
      config: AutocorrelationConfig
  ): Either[FitError, WhiteningPlan] =
    val pooling = if config.global then NoisePooling.Global else NoisePooling.Run
    val arOptions =
      ArFitOptions(
        order = ArFitOrder.Fixed(config.order.value),
        pooling = pooling,
        exactFirstAr1 = config.exactFirst
      )

    def estimate(coefficients: DoubleMatrix): Either[FitError, WhiteningPlan] =
      val residuals = residualMatrix(design.value, response.value, coefficients)
      ArEstimation
        .fitNoise(MatrixAdapters.toGaleMatrix(residuals), segments, arOptions)
        .left
        .map(Gls.arToFitError)

    def refitCoefficients(plan: WhiteningPlan): Either[FitError, DoubleMatrix] =
      for
        input <- whitenedInput(plan, design, response)
        (whitenedDesign, whitenedResponse) = input
        initial <- Ols.fit(whitenedDesign, whitenedResponse)
        fit <- fitWithoutAutocorrelation(whitenedDesign, whitenedResponse, partitions, options, initial)
      yield fit.coefficients.value

    def loop(iteration: Int, coefficients: DoubleMatrix): Either[FitError, WhiteningPlan] =
      estimate(coefficients).flatMap { plan =>
        if iteration >= config.iterations then Right(plan)
        else
          for
            refit <- refitCoefficients(plan)
            finalPlan <- loop(iteration + 1, refit)
          yield finalPlan
      }

    loop(iteration = 1, initialCoefficients)

  private def preparedInput(
      prepared: RobustPrepared,
      response: ResponseBlock
  ): Either[FitError, (DesignMatrix, ResponseBlock)] =
    prepared.whiteningPlan match
      case None =>
        Right(prepared.design -> response)
      case Some(plan) =>
        whitenedInput(plan, prepared.design, response)

  private def whitenedInput(
      plan: WhiteningPlan,
      design: DesignMatrix,
      response: ResponseBlock
  ): Either[FitError, (DesignMatrix, ResponseBlock)] =
    WhiteningTransform(
      plan,
      MatrixAdapters.toGaleMatrix(design.value),
      MatrixAdapters.toGaleMatrix(response.value)
    )
      .left
      .map(Gls.arToFitError)
      .map(whitened =>
        DesignMatrix.unsafe(MatrixAdapters.fromGaleMatrix(whitened.design)) ->
          ResponseBlock.unsafe(MatrixAdapters.fromGaleMatrix(whitened.response))
      )

  private def disabledFit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      initial: OlsFit
  ): Either[FitError, RobustFit] =
    val weights = constantMatrix(response.timepoints, 1, 1.0)
    val residuals = residualMatrix(design.value, response.value, initial.coefficients.value)
    val scales = scaleEstimates(residuals, effectivePartitions(response.timepoints, partitions), options.scaleScope)
    Right(
      RobustFit(
        coefficients = initial.coefficients,
        residualVariance = initial.residualVariance,
        residualDegreesOfFreedom = initial.residualDegreesOfFreedom,
        normalizedCovariance = initial.normalizedCovariance,
        standardErrors = initial.standardErrors,
        diagnostics = RobustDiagnostics(
          psi = options.psi,
          scaleScope = options.scaleScope,
          iterations = 0,
          converged = true,
          tolerance = DefaultTolerance,
          maxCoefficientDelta = 0.0,
          scaleEstimates = scales,
          weightTopology = RobustWeightTopology.RowWise,
          weights = weights,
          sharedNormalizedCovariance = true
        ),
        initialOlsDiagnostics = initial.diagnostics,
        finalOlsDiagnostics = initial.diagnostics,
        coefficientCovariance = initial.coefficientCovariance
      )
    )

  private def iterativelyReweightedFit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: RobustOptions,
      initial: OlsFit
  ): Either[FitError, RobustFit] =
    val fitPartitions = effectivePartitions(response.timepoints, partitions)
    var coefficients = initial.coefficients.value
    var current = weightedLeastSquares(design, response, constantMatrix(response.timepoints, 1, 1.0))
    var lastScales = scaleEstimates(residualMatrix(design.value, response.value, coefficients), fitPartitions, options.scaleScope)
    var lastWeights = constantMatrix(response.timepoints, 1, 1.0)
    var iterations = 0
    var maxDelta = Double.PositiveInfinity
    var converged = false

    while iterations < options.maxIterations && !converged do
      val residuals = residualMatrix(design.value, response.value, coefficients)
      lastScales = scaleEstimates(residuals, fitPartitions, options.scaleScope)
      lastWeights = robustWeights(residuals, lastScales, fitPartitions, options.psi)
      current = weightedLeastSquares(design, response, lastWeights)
      current match
        case Left(error) =>
          return Left(error)
        case Right(fit) =>
          maxDelta = maxCoefficientDelta(coefficients, fit.coefficients.value)
          coefficients = fit.coefficients.value
          iterations += 1
          converged = maxDelta <= DefaultTolerance

    current.map { fit =>
      RobustFit(
        coefficients = fit.coefficients,
        residualVariance = fit.residualVariance,
        residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
        normalizedCovariance = fit.normalizedCovariance,
        standardErrors = fit.standardErrors,
        diagnostics = RobustDiagnostics(
          psi = options.psi,
          scaleScope = options.scaleScope,
          iterations = iterations,
          converged = converged,
          tolerance = DefaultTolerance,
          maxCoefficientDelta = if maxDelta.isFinite then maxDelta else 0.0,
          scaleEstimates = lastScales,
          weightTopology = RobustWeightTopology.RowWise,
          weights = lastWeights,
          sharedNormalizedCovariance = true
        ),
        initialOlsDiagnostics = initial.diagnostics,
        finalOlsDiagnostics = fit.diagnostics,
        coefficientCovariance = fit.coefficientCovariance
      )
    }

  private def effectivePartitions(timepoints: Int, partitions: Vector[RunPartition]): Vector[RunPartition] =
    if partitions.nonEmpty then partitions
    else Vector(RunPartition(0, rowIndices = (0 until timepoints).toVector, timepoints = (0 until timepoints).toVector))

  private def weightedLeastSquares(
      design: DesignMatrix,
      response: ResponseBlock,
      weights: DoubleMatrix
  ): Either[FitError, OlsFit] =
    require(weights.rows == response.timepoints, "robust weight rows must match response rows")
    require(weights.cols == 1 || weights.cols == response.voxels, "robust weight cols must be one or match response voxels")

    val coefficientData = new Array[Double](design.predictors * response.voxels)
    val standardErrorData = new Array[Double](design.predictors * response.voxels)
    val residualVarianceData = new Array[Double](response.voxels)
    val covarianceMatrices = Vector.newBuilder[DoubleMatrix]
    var covariance: DoubleMatrix | Null = null
    var diagnostics: OlsDiagnostics | Null = null

    var voxel = 0
    while voxel < response.voxels do
      val weightedDesignData = new Array[Double](design.timepoints * design.predictors)
      val weightedResponseData = new Array[Double](design.timepoints)

      var row = 0
      while row < design.timepoints do
        val scale = math.sqrt(math.max(0.0, weightAt(weights, row, voxel)))
        var predictor = 0
        while predictor < design.predictors do
          weightedDesignData(row * design.predictors + predictor) = design.value(row, predictor) * scale
          predictor += 1
        weightedResponseData(row) = response.value(row, voxel) * scale
        row += 1

      val weightedDesign = DesignMatrix.unsafe(DoubleMatrix.unsafe(design.timepoints, design.predictors, weightedDesignData))
      val weightedResponse = ResponseBlock.unsafe(DoubleMatrix.unsafe(design.timepoints, 1, weightedResponseData))
      Ols.fit(weightedDesign, weightedResponse) match
        case Left(error) =>
          return Left(error)
        case Right(fit) =>
          var predictor = 0
          while predictor < design.predictors do
            coefficientData(predictor * response.voxels + voxel) = fit.coefficients(predictor, 0)
            standardErrorData(predictor * response.voxels + voxel) = fit.standardErrors(predictor, 0)
            predictor += 1
          residualVarianceData(voxel) = fit.residualVariance(0)
          covarianceMatrices += fit.normalizedCovariance
          if covariance == null then covariance = fit.normalizedCovariance
          if diagnostics == null then diagnostics = fit.diagnostics
      voxel += 1

    Right(
      OlsFit(
        coefficients = CoefficientBlock(DoubleMatrix.unsafe(design.predictors, response.voxels, coefficientData)),
        residualVariance = DoubleVector.unsafe(residualVarianceData),
        residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(design.timepoints - design.predictors),
        normalizedCovariance = covariance.asInstanceOf[DoubleMatrix],
        standardErrors = StandardErrorBlock(DoubleMatrix.unsafe(design.predictors, response.voxels, standardErrorData)),
        diagnostics = diagnostics.asInstanceOf[OlsDiagnostics],
        coefficientCovariance =
          if weights.cols == 1 then CoefficientCovariance.unsafeShared(covariance.asInstanceOf[DoubleMatrix])
          else CoefficientCovariance.unsafeVoxelwise(covarianceMatrices.result())
      )
    )

  private def robustWeights(
      residuals: DoubleMatrix,
      scales: RobustScaleEstimates,
      partitions: Vector[RunPartition],
      psi: RobustPsi
  ): DoubleMatrix =
    val out = new Array[Double](residuals.rows)
    var row = 0
    while row < residuals.rows do
      out(row) = weight(rowMedianStandardizedResidual(row, residuals, scales, partitions), psi)
      row += 1
    DoubleMatrix.unsafe(residuals.rows, 1, out)

  private def weight(standardizedResidual: Double, psi: RobustPsi): Double =
    val u = math.abs(standardizedResidual)
    psi match
      case RobustPsi.Disabled =>
        1.0
      case RobustPsi.Huber(k) =>
        if u <= k then 1.0 else k / math.max(u, Epsilon)
      case RobustPsi.Bisquare(c) =>
        if u >= c then 0.0
        else
          val scaled = u / c
          val inner = 1.0 - scaled * scaled
          inner * inner

  private def scaleEstimates(
      residuals: DoubleMatrix,
      partitions: Vector[RunPartition],
      scope: ScaleScope
  ): RobustScaleEstimates =
    scope match
      case ScaleScope.Global =>
        val scale = robustPositive(MadScale * median(rowAbsMedians(residuals, 0 until residuals.rows)))
        RobustScaleEstimates(scope, Vector("global"), repeatedScaleMatrix(1, residuals.cols, scale))

      case ScaleScope.Voxel =>
        val out = new Array[Double](residuals.cols)
        var voxel = 0
        while voxel < residuals.cols do
          out(voxel) = robustPositive(MadScale * median(absResidualColumn(residuals, voxel, 0 until residuals.rows)))
          voxel += 1
        RobustScaleEstimates(scope, Vector("all"), DoubleMatrix.unsafe(1, residuals.cols, out))

      case ScaleScope.Run =>
        val out = new Array[Double](partitions.length * residuals.cols)
        var run = 0
        while run < partitions.length do
          val scale = robustPositive(MadScale * median(rowAbsMedians(residuals, partitions(run).rowIndices)))
          var voxel = 0
          while voxel < residuals.cols do
            out(run * residuals.cols + voxel) = scale
            voxel += 1
          run += 1
        RobustScaleEstimates(scope, partitions.map(partition => s"run_${partition.runIndex}"), DoubleMatrix.unsafe(partitions.length, residuals.cols, out))

  private def scaleFor(
      row: Int,
      voxel: Int,
      scales: RobustScaleEstimates,
      partitions: Vector[RunPartition]
  ): Double =
    scales.scope match
      case ScaleScope.Global =>
        scales.values(0, 0)
      case ScaleScope.Voxel =>
        scales.values(0, voxel)
      case ScaleScope.Run =>
        val run = partitions.indexWhere(_.rowIndices.contains(row))
        scales.values(math.max(run, 0), voxel)

  private def rowMedianStandardizedResidual(
      row: Int,
      residuals: DoubleMatrix,
      scales: RobustScaleEstimates,
      partitions: Vector[RunPartition]
  ): Double =
    val values = new Array[Double](residuals.cols)
    var voxel = 0
    while voxel < residuals.cols do
      val scale = scaleFor(row, voxel, scales, partitions)
      values(voxel) = math.abs(residuals(row, voxel)) / math.max(scale, Epsilon)
      voxel += 1
    median(values.toIndexedSeq)

  private def robustPositive(value: Double): Double =
    if value > Epsilon && value.isFinite then value else Epsilon

  private def median(values: IndexedSeq[Double]): Double =
    if values.isEmpty then 0.0
    else
      val sorted = values.toVector.sorted
      val mid = sorted.length / 2
      if sorted.length % 2 == 1 then sorted(mid)
      else (sorted(mid - 1) + sorted(mid)) / 2.0

  private def residualMatrix(
      design: DoubleMatrix,
      response: DoubleMatrix,
      coefficients: DoubleMatrix
  ): DoubleMatrix =
    val out = new Array[Double](response.rows * response.cols)
    var row = 0
    while row < response.rows do
      var voxel = 0
      while voxel < response.cols do
        var fitted = 0.0
        var predictor = 0
        while predictor < design.cols do
          fitted += design(row, predictor) * coefficients(predictor, voxel)
          predictor += 1
        out(row * response.cols + voxel) = response(row, voxel) - fitted
        voxel += 1
      row += 1
    DoubleMatrix.unsafe(response.rows, response.cols, out)

  private def absResidualColumn(
      residuals: DoubleMatrix,
      voxel: Int,
      rows: Iterable[Int]
  ): Vector[Double] =
    rows.iterator.map(row => math.abs(residuals(row, voxel))).toVector

  private def rowAbsMedians(residuals: DoubleMatrix, rows: Iterable[Int]): Vector[Double] =
    rows.iterator.map { row =>
      val values = new Array[Double](residuals.cols)
      var voxel = 0
      while voxel < residuals.cols do
        values(voxel) = math.abs(residuals(row, voxel))
        voxel += 1
      median(values.toIndexedSeq)
    }.toVector

  private def constantMatrix(rows: Int, cols: Int, value: Double): DoubleMatrix =
    val out = Array.fill(rows * cols)(value)
    DoubleMatrix.unsafe(rows, cols, out)

  private def repeatedScaleMatrix(rows: Int, cols: Int, value: Double): DoubleMatrix =
    constantMatrix(rows, cols, value)

  private def weightAt(weights: DoubleMatrix, row: Int, voxel: Int): Double =
    if weights.cols == 1 then weights(row, 0) else weights(row, voxel)

  private def subsetDiagnostics(
      diagnostics: RobustDiagnostics,
      selectedVoxelIndices: Vector[Int],
      voxelIndices: Vector[Int]
  ): Either[FitError, RobustDiagnostics] =
    val positions = new Array[Int](voxelIndices.length)
    var i = 0
    while i < voxelIndices.length do
      val position = selectedVoxelIndices.indexOf(voxelIndices(i))
      if position < 0 then
        return Left(FitError.IncompatibleFitBlocks(s"robust chunk voxel ${voxelIndices(i)} was not in the prepared selection"))
      positions(i) = position
      i += 1

    val scaleValues = diagnostics.scaleEstimates.values
    val out = new Array[Double](scaleValues.rows * positions.length)
    var row = 0
    while row < scaleValues.rows do
      var col = 0
      while col < positions.length do
        out(row * positions.length + col) = scaleValues(row, positions(col))
        col += 1
      row += 1

    Right(
      diagnostics.copy(
        scaleEstimates = RobustScaleEstimates(
          diagnostics.scaleEstimates.scope,
          diagnostics.scaleEstimates.labels,
          DoubleMatrix.unsafe(scaleValues.rows, positions.length, out)
        )
      )
    )

  private def maxCoefficientDelta(left: DoubleMatrix, right: DoubleMatrix): Double =
    require(left.rows == right.rows && left.cols == right.cols, "coefficient matrices must align")
    var max = 0.0
    var i = 0
    val leftData = left.copyData
    val rightData = right.copyData
    while i < leftData.length do
      max = math.max(max, math.abs(leftData(i) - rightData(i)))
      i += 1
    max
