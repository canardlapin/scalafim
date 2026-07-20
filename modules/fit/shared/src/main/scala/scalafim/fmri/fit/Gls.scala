package scalafim.fmri.fit

import scalafim.fmri.ar.{
  ArError,
  ArEstimation,
  ArFitOptions,
  ArmaCoefficients,
  ArOrder,
  NoisePooling,
  TimeSegment,
  TimeSegments,
  WhiteningPlan,
  WhiteningTransform
}
import scalafim.fmri.model.{ArCoefficientSpec, ArOptions, AutocorrelationConfig}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

final case class GlsFit(
    coefficients: CoefficientBlock,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    normalizedCovariance: DoubleMatrix,
    standardErrors: StandardErrorBlock,
    diagnostics: ArDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics,
    finalOlsDiagnostics: OlsDiagnostics,
    coefficientCovariance: CoefficientCovariance
):
  require(coefficientCovariance.predictors == coefficients.predictors, "GLS coefficient covariance must match coefficient rows")
  require(coefficientCovariance.validateVoxelCount(coefficients.voxels).isRight, "GLS coefficient covariance must be shared or match voxel count")
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels
  def olsDiagnostics: OlsDiagnostics = finalOlsDiagnostics

private[fit] enum GlsWhitening:
  case Shared(plan: WhiteningPlan)
  case Voxelwise(plans: Vector[WhiteningPlan])

final case class GlsPrepared private[fit] (
    design: DesignMatrix,
    partitions: Vector[RunPartition],
    whitening: GlsWhitening,
    diagnostics: ArDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics,
    selectedVoxelIndices: Vector[Int]
):
  require(selectedVoxelIndices.nonEmpty, "GLS prepared context must contain selected voxels")

  def fit(response: ResponseBlock): Either[FitError, GlsFit] =
    Gls.fitPrepared(this, response, selectedVoxelIndices)

  def fit(response: ResponseBlock, voxelIndices: Vector[Int]): Either[FitError, GlsFit] =
    Gls.fitPrepared(this, response, voxelIndices)

object Gls:

  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions
  ): Either[FitError, GlsFit] =
    fit(design, response, partitions, options, (0 until response.voxels).toVector)

  private[fit] def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions,
      voxelIndices: Vector[Int]
  ): Either[FitError, GlsFit] =
    prepare(design, response, partitions, options, voxelIndices).flatMap(_.fit(response, voxelIndices))

  private[fit] def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions
  ): Either[FitError, GlsPrepared] =
    prepare(design, response, partitions, options, (0 until response.voxels).toVector)

  private[fit] def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions,
      selectedVoxelIndices: Vector[Int]
  ): Either[FitError, GlsPrepared] =
    for
      _ <- validateVoxelIndices(response, selectedVoxelIndices)
      config <- autocorrelationConfig(options)
      _ <- validatePartitions(partitions)
      segments <- timeSegments(partitions, options.censoredTimepoints)
      initial <- Ols.fit(design, response)
      whiteningAndMethod <- whiteningPlan(design.value, response.value, initial.coefficients.value, segments, config)
      (whitening, method) = whiteningAndMethod
    yield GlsPrepared(
      design = design,
      partitions = partitions,
      whitening = whitening,
      diagnostics = diagnostics(whitening, partitions, method, diagnosticIterations(config)),
      initialOlsDiagnostics = initial.diagnostics,
      selectedVoxelIndices = selectedVoxelIndices
    )

  private[fit] def fitPrepared(
      prepared: GlsPrepared,
      response: ResponseBlock,
      voxelIndices: Vector[Int]
  ): Either[FitError, GlsFit] =
    validateVoxelIndices(response, voxelIndices).flatMap { _ =>
      prepared.whitening match
        case GlsWhitening.Shared(plan) =>
          for
            whitened <- WhiteningTransform(plan, prepared.design.value, response.value).left.map(arToFitError)
            fit <- Ols.fit(DesignMatrix.unsafe(whitened.design), ResponseBlock.unsafe(whitened.response))
          yield GlsFit(
            coefficients = fit.coefficients,
            residualVariance = fit.residualVariance,
            residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
            normalizedCovariance = fit.normalizedCovariance,
            standardErrors = fit.standardErrors,
            diagnostics = prepared.diagnostics,
            initialOlsDiagnostics = prepared.initialOlsDiagnostics,
            finalOlsDiagnostics = fit.diagnostics,
            coefficientCovariance = fit.coefficientCovariance
          )

        case GlsWhitening.Voxelwise(plans) =>
          for
            selectedPlans <- subsetPlans(plans, prepared.diagnostics, prepared.selectedVoxelIndices, voxelIndices)
            diagnostics <- subsetDiagnostics(prepared.diagnostics, selectedPlans.positions)
            fit <- fitVoxelwise(prepared.design.value, response.value, selectedPlans.plans)
          yield GlsFit(
            coefficients = fit.coefficients,
            residualVariance = fit.residualVariance,
            residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
            normalizedCovariance = fit.normalizedCovariance,
            standardErrors = fit.standardErrors,
            diagnostics = diagnostics,
            initialOlsDiagnostics = prepared.initialOlsDiagnostics,
            finalOlsDiagnostics = fit.diagnostics,
            coefficientCovariance = fit.coefficientCovariance
          )
    }

  private[fit] def autocorrelationConfig(options: ArOptions): Either[FitError, AutocorrelationConfig] =
    AutocorrelationConfig
      .fromLegacy(options)
      .left
      .map(error => FitError.UnsupportedAutocorrelation(error.message))

  private def validateVoxelIndices(response: ResponseBlock, voxelIndices: Vector[Int]): Either[FitError, Unit] =
    if voxelIndices.length != response.voxels then
      Left(FitError.InvalidFitAxis("voxel indices", s"expected ${response.voxels}, got ${voxelIndices.length}"))
    else SelectedVoxelIndices.fromInts(voxelIndices).map(_ => ())

  private[fit] def validatePartitions(partitions: Vector[RunPartition]): Either[FitError, Unit] =
    if partitions.isEmpty then Left(FitError.UnsupportedAutocorrelation("GLS requires at least one run partition"))
    else if partitions.exists(p => !isContiguous(p.timepoints)) then
      Left(FitError.UnsupportedAutocorrelation("AR GLS requires contiguous selected timepoints within each run"))
    else if partitions.exists(p => !isContiguous(p.rowIndices)) then
      Left(FitError.UnsupportedAutocorrelation("AR GLS requires contiguous selected rows within each run"))
    else Right(())

  private def isContiguous(timepoints: Vector[Int]): Boolean =
    var i = 1
    while i < timepoints.length do
      if timepoints(i) != timepoints(i - 1) + 1 then return false
      i += 1
    true

  private[fit] def timeSegments(
      partitions: Vector[RunPartition],
      censoredTimepoints: Vector[Int]
  ): Either[FitError, Vector[TimeSegment]] =
    val base = partitions.map { partition =>
      TimeSegment(
        start = partition.rowIndices.head,
        endExclusive = partition.rowIndices.last + 1,
        runIndex = partition.runIndex
      )
    }
    val missingCensors =
      censoredTimepoints.distinct.filterNot { timepoint =>
        partitions.exists(_.timepoints.contains(timepoint))
      }
    if missingCensors.nonEmpty then
      Left(FitError.UnsupportedAutocorrelation(s"censored timepoints are not selected: ${missingCensors.mkString(", ")}"))
    else
      val censoredRows =
        censoredTimepoints.flatMap { timepoint =>
          partitions.iterator
            .flatMap(partition => partition.timepoints.zip(partition.rowIndices))
            .find { case (candidate, _) => candidate == timepoint }
            .map(_._2)
        }.toSet
      val segments = TimeSegments.withCensorResets(base, censoredRows)
      TimeSegments.validateCoverage(segments, base.last.endExclusive).left.map(arToFitError).map(_ => segments)

  private def whiteningPlan(
      design: DoubleMatrix,
      response: DoubleMatrix,
      coefficients: DoubleMatrix,
      segments: Vector[TimeSegment],
      config: AutocorrelationConfig
  ): Either[FitError, (GlsWhitening, String)] =
    config.coefficients match
      case ArCoefficientSpec.Rho(rho) =>
        Right(GlsWhitening.Shared(WhiteningPlan.global(ArmaCoefficients(Vector(rho)), segments, exactFirstAr1 = config.exactFirst)) -> "fixed")

      case ArCoefficientSpec.Phi(phi) =>
        Right(GlsWhitening.Shared(WhiteningPlan.global(ArmaCoefficients(phi), segments, exactFirstAr1 = config.exactFirst)) -> "fixed")

      case ArCoefficientSpec.Estimate =>
        if config.voxelwise then
          iterateEstimatedVoxelwiseWhitening(design, response, coefficients, segments, config)
            .map(GlsWhitening.Voxelwise.apply)
            .map(_ -> "voxelwise-estimated")
        else iterateEstimatedWhitening(design, response, coefficients, segments, config).map(GlsWhitening.Shared.apply).map(_ -> "estimated")

  private def iterateEstimatedWhitening(
      design: DoubleMatrix,
      response: DoubleMatrix,
      initialCoefficients: DoubleMatrix,
      segments: Vector[TimeSegment],
      config: AutocorrelationConfig
  ): Either[FitError, WhiteningPlan] =
    val pooling = if config.global then NoisePooling.Global else NoisePooling.Run
    val arOptions =
      ArFitOptions(
        order = ArOrder.Fixed(config.order.value),
        pooling = pooling,
        exactFirstAr1 = config.exactFirst
      )

    def estimate(coefficients: DoubleMatrix): Either[FitError, WhiteningPlan] =
      val residuals = residualMatrix(design, response, coefficients)
      ArEstimation
        .fitNoise(residuals, segments, arOptions)
        .left
        .map(arToFitError)

    def loop(iteration: Int, coefficients: DoubleMatrix): Either[FitError, WhiteningPlan] =
      estimate(coefficients).flatMap { plan =>
        if iteration >= config.iterations then Right(plan)
        else
          for
            fit <- fitWithPlan(plan, design, response)
            finalPlan <- loop(iteration + 1, fit.coefficients.value)
          yield finalPlan
      }

    loop(iteration = 1, initialCoefficients)

  private def iterateEstimatedVoxelwiseWhitening(
      design: DoubleMatrix,
      response: DoubleMatrix,
      initialCoefficients: DoubleMatrix,
      segments: Vector[TimeSegment],
      config: AutocorrelationConfig
  ): Either[FitError, Vector[WhiteningPlan]] =
    val plans = Vector.newBuilder[WhiteningPlan]
    var voxel = 0
    while voxel < response.cols do
      iterateEstimatedWhitening(
        design = design,
        response = matrixColumn(response, voxel),
        initialCoefficients = matrixColumn(initialCoefficients, voxel),
        segments = segments,
        config = config
      ) match
        case Left(error) =>
          return Left(error)
        case Right(plan) =>
          plans += plan
      voxel += 1
    Right(plans.result())

  private[fit] def fitWithPlan(
      plan: WhiteningPlan,
      design: DoubleMatrix,
      response: DoubleMatrix
  ): Either[FitError, OlsFit] =
    for
      whitened <- WhiteningTransform(plan, design, response).left.map(arToFitError)
      fit <- Ols.fit(DesignMatrix.unsafe(whitened.design), ResponseBlock.unsafe(whitened.response))
    yield fit

  private final case class SelectedVoxelPlans(plans: Vector[WhiteningPlan], positions: Vector[Int])

  private def subsetPlans(
      plans: Vector[WhiteningPlan],
      diagnostics: ArDiagnostics,
      selectedVoxelIndices: Vector[Int],
      voxelIndices: Vector[Int]
  ): Either[FitError, SelectedVoxelPlans] =
    val selectedCount = diagnostics.runs.head.voxelwiseCoefficients.length
    if plans.length != selectedCount || plans.length != selectedVoxelIndices.length then
      Left(FitError.UnsupportedAutocorrelation("voxelwise AR plans do not match diagnostics"))
    else
      val positions = Vector.newBuilder[Int]
      val selected = Vector.newBuilder[WhiteningPlan]
      var i = 0
      while i < voxelIndices.length do
        val position = selectedVoxelIndices.indexOf(voxelIndices(i))
        if position < 0 then
          return Left(FitError.InvalidFitAxis("voxel index", s"value ${voxelIndices(i)} was not in the prepared GLS selection"))
        positions += position
        selected += plans(position)
        i += 1
      Right(SelectedVoxelPlans(selected.result(), positions.result()))

  private def subsetDiagnostics(
      diagnostics: ArDiagnostics,
      positions: Vector[Int]
  ): Either[FitError, ArDiagnostics] =
    if diagnostics.sharedNormalizedCovariance then Right(diagnostics)
    else
      val runs =
        diagnostics.runs.map { run =>
          val perVoxel = positions.map(position => run.voxelwiseCoefficients(position))
          val summary = averageCoefficients(perVoxel, diagnostics.order)
          run.copy(
            rho = summary.head,
            coefficients = summary,
            voxelwiseCoefficients = perVoxel
          )
        }
      Right(diagnostics.copy(runs = runs))

  private def fitVoxelwise(
      design: DoubleMatrix,
      response: DoubleMatrix,
      plans: Vector[WhiteningPlan]
  ): Either[FitError, OlsFit] =
    if plans.length != response.cols then
      Left(FitError.UnsupportedAutocorrelation(s"voxelwise AR requires ${response.cols} whitening plans, got ${plans.length}"))
    else
      val coefficientData = new Array[Double](design.cols * response.cols)
      val standardErrorData = new Array[Double](design.cols * response.cols)
      val residualVarianceData = new Array[Double](response.cols)
      val covarianceMatrices = Vector.newBuilder[DoubleMatrix]
      var covariance: DoubleMatrix | Null = null
      var diagnostics: OlsDiagnostics | Null = null
      var residualDf: ResidualDegreesOfFreedom | Null = null

      var voxel = 0
      while voxel < response.cols do
        fitWithPlan(plans(voxel), design, matrixColumn(response, voxel)) match
          case Left(error) =>
            return Left(error)
          case Right(fit) =>
            var predictor = 0
            while predictor < design.cols do
              coefficientData(predictor * response.cols + voxel) = fit.coefficients(predictor, 0)
              standardErrorData(predictor * response.cols + voxel) = fit.standardErrors(predictor, 0)
              predictor += 1
            residualVarianceData(voxel) = fit.residualVariance(0)
            covarianceMatrices += fit.normalizedCovariance
            if covariance == null then covariance = fit.normalizedCovariance
            if diagnostics == null then diagnostics = fit.diagnostics
            if residualDf == null then residualDf = fit.residualDegreesOfFreedom
        voxel += 1

      Right(
        OlsFit(
          coefficients = CoefficientBlock(DoubleMatrix.unsafe(design.cols, response.cols, coefficientData)),
          residualVariance = DoubleVector.unsafe(residualVarianceData),
          residualDegreesOfFreedom = residualDf.asInstanceOf[ResidualDegreesOfFreedom],
          normalizedCovariance = covariance.asInstanceOf[DoubleMatrix],
          standardErrors = StandardErrorBlock(DoubleMatrix.unsafe(design.cols, response.cols, standardErrorData)),
          diagnostics = diagnostics.asInstanceOf[OlsDiagnostics],
          coefficientCovariance = CoefficientCovariance.unsafeVoxelwise(covarianceMatrices.result())
        )
      )

  private[fit] def residualMatrix(
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

  private[fit] def diagnostics(
      whitening: GlsWhitening,
      partitions: Vector[RunPartition],
      method: String,
      iterations: Int
  ): ArDiagnostics =
    whitening match
      case GlsWhitening.Shared(plan) =>
        ArDiagnostics(
          order = plan.arOrder,
          runs = partitions.map { partition =>
            val coefficients =
              plan.pooling match
                case NoisePooling.Global => plan.coefficients.head
                case NoisePooling.Run    => plan.coefficients(partition.runIndex)
            val phi = coefficients.phi
            ArRunDiagnostic(
              runIndex = partition.runIndex,
              rho = phi.headOption.getOrElse(0.0),
              method = method,
              rows = partition.rowIndices.length,
              coefficients = phi
            )
          },
          iterations = iterations
        )

      case GlsWhitening.Voxelwise(plans) =>
        val order = plans.map(_.arOrder).max
        ArDiagnostics(
          order = order,
          runs = partitions.map { partition =>
            val perVoxel =
              plans.map { plan =>
                val segment = plan.segments.find(_.runIndex == partition.runIndex).getOrElse(plan.segments.head)
                plan.coefficientsFor(segment).phi
              }
            val summary = averageCoefficients(perVoxel, order)
            ArRunDiagnostic(
              runIndex = partition.runIndex,
              rho = summary.headOption.getOrElse(0.0),
              method = method,
              rows = partition.rowIndices.length,
              coefficients = summary,
              voxelwiseCoefficients = perVoxel
            )
          },
          iterations = iterations,
          sharedNormalizedCovariance = false
        )

  private[fit] def diagnosticIterations(config: AutocorrelationConfig): Int =
    config.coefficients match
      case ArCoefficientSpec.Estimate => config.iterations
      case _                          => 0

  private def matrixColumn(matrix: DoubleMatrix, col: Int): DoubleMatrix =
    val out = new Array[Double](matrix.rows)
    var row = 0
    while row < matrix.rows do
      out(row) = matrix(row, col)
      row += 1
    DoubleMatrix.unsafe(matrix.rows, 1, out)

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

  private[fit] def arToFitError(error: ArError): FitError =
    FitError.UnsupportedAutocorrelation(error.message)
