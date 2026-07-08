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
import scalafim.fmri.model.{ArOptions, ArStructure}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

final case class GlsFit(
    coefficients: CoefficientBlock,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    normalizedCovariance: DoubleMatrix,
    standardErrors: StandardErrorBlock,
    diagnostics: ArDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics,
    finalOlsDiagnostics: OlsDiagnostics
):
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels
  def olsDiagnostics: OlsDiagnostics = finalOlsDiagnostics

final case class GlsPrepared private[fit] (
    design: DesignMatrix,
    partitions: Vector[RunPartition],
    plan: WhiteningPlan,
    diagnostics: ArDiagnostics,
    initialOlsDiagnostics: OlsDiagnostics
):
  def fit(response: ResponseBlock): Either[FitError, GlsFit] =
    Gls.fitPrepared(this, response)

object Gls:

  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions
  ): Either[FitError, GlsFit] =
    prepare(design, response, partitions, options).flatMap(_.fit(response))

  private[fit] def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      options: ArOptions
  ): Either[FitError, GlsPrepared] =
    for
      _ <- validateOptions(options)
      _ <- validatePartitions(partitions)
      segments <- timeSegments(partitions, options.censoredTimepoints)
      initial <- Ols.fit(design, response)
      planAndMethod <- whiteningPlan(design.value, response.value, initial.coefficients.value, segments, options)
      (plan, method) = planAndMethod
    yield GlsPrepared(
      design = design,
      partitions = partitions,
      plan = plan,
      diagnostics = diagnostics(plan, partitions, method),
      initialOlsDiagnostics = initial.diagnostics
    )

  private[fit] def fitPrepared(
      prepared: GlsPrepared,
      response: ResponseBlock
  ): Either[FitError, GlsFit] =
    for
      whitened <- WhiteningTransform(prepared.plan, prepared.design.value, response.value).left.map(arToFitError)
      fit <- Ols.fit(DesignMatrix.unsafe(whitened.design), ResponseBlock.unsafe(whitened.response))
    yield GlsFit(
      coefficients = fit.coefficients,
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      normalizedCovariance = fit.normalizedCovariance,
      standardErrors = fit.standardErrors,
      diagnostics = prepared.diagnostics,
      initialOlsDiagnostics = prepared.initialOlsDiagnostics,
      finalOlsDiagnostics = fit.diagnostics
    )

  private def validateOptions(options: ArOptions): Either[FitError, Unit] =
    options.structure match
      case ArStructure.Ar(_) =>
        if options.voxelwise then Left(FitError.UnsupportedAutocorrelation("voxelwise AR is not implemented"))
        else if options.iterations > 1 then Left(FitError.UnsupportedAutocorrelation("iterated AR re-estimation is not implemented"))
        else Right(())
      case ArStructure.Iid =>
        Left(FitError.UnsupportedAutocorrelation("GeneralizedLeastSquares requires ArStructure.Ar(p)"))

  private def validatePartitions(partitions: Vector[RunPartition]): Either[FitError, Unit] =
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

  private def timeSegments(
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
      options: ArOptions
  ): Either[FitError, (WhiteningPlan, String)] =
    options.structure match
      case ArStructure.Iid =>
        Left(FitError.UnsupportedAutocorrelation("GeneralizedLeastSquares requires ArStructure.Ar(p)"))

      case ArStructure.Ar(order) =>
        fixedPhi(options) match
          case Some(phi) =>
            Right(WhiteningPlan.global(ArmaCoefficients(phi), segments, exactFirstAr1 = options.exactFirst) -> "fixed")

          case None if options.iterations >= 1 =>
            val residuals = residualMatrix(design, response, coefficients)
            val pooling = if options.global then NoisePooling.Global else NoisePooling.Run
            ArEstimation
              .fitNoise(
                residuals,
                segments,
                ArFitOptions(
                  order = ArOrder.Fixed(order),
                  pooling = pooling,
                  exactFirstAr1 = options.exactFirst
                )
              )
              .left
              .map(arToFitError)
              .map(_ -> "estimated")

          case None =>
            Left(FitError.UnsupportedAutocorrelation(s"AR($order) GLS needs fixed coefficients or at least one estimation iteration"))

  private def fixedPhi(options: ArOptions): Option[Vector[Double]] =
    options.phi.orElse(options.rho.map(rho => Vector(rho)))

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

  private def diagnostics(
      plan: WhiteningPlan,
      partitions: Vector[RunPartition],
      method: String
  ): ArDiagnostics =
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
      }
    )

  private def arToFitError(error: ArError): FitError =
    FitError.UnsupportedAutocorrelation(error.message)
