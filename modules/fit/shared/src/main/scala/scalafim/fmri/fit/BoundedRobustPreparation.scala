package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DatasetSeriesReader, FmriSeries}
import scalafim.fmri.ar.{ArEstimation, ArFitOptions, ArNoiseSummary, ArOrder, ArOrderValue, NoisePooling, WhiteningPlan}
import scalafim.fmri.model.{FitEngine, FitPlan, RobustOptions, RobustPsi, ScaleScope}

/** Compact temporal state. Population response and coefficient matrices are
  * reconstructed one spatial block at a time. Scales describe the residuals
  * BEFORE the last IRLS update, matching the dense diagnostic convention.
  */
private[fit] final case class RobustReductionState(
    weights: DMat,
    previousWeights: Option[DMat],
    sharedScales: Option[RobustScaleEstimates],
    iterations: Int,
    converged: Boolean,
    maxDelta: Double
)

private[fit] final class BoundedRobustPrepared(
    source: BoundedResponseReplay,
    state: RobustReductionState,
    whitening: Option[WhiteningPlan],
    autocorrelation: Option[ArDiagnostics]
) extends PreparedSpatialReduction:
  val engine: FitEngine = FitEngine.RobustLeastSquares

  def fit(series: FmriSeries): Either[FitError, FitBlockResult] =
    val result = for
      input <- source.input(series)
      white <- BoundedRobustPreparation.whiten(input.design, input.response, whitening)
      (design, response) = white
      initial <- Ols.fit(design, response)
      fit <- Robust.weightedLeastSquares(design, response, state.weights)
      scales <- BoundedRobustPreparation.blockScales(design, response, source.partitions,
        source.plan.config.robust.scaleScope, state.previousWeights, state.sharedScales)
      diagnostics = RobustDiagnostics(source.plan.config.robust.psi, source.plan.config.robust.scaleScope,
        state.iterations, state.converged, Robust.DefaultTolerance, state.maxDelta, scales,
        RobustWeightTopology.RowWise, state.weights, sharedNormalizedCovariance = true)
      robust = RobustFit(fit.coefficients, fit.residualVariance, fit.residualDegreesOfFreedom,
        fit.normalizedCovariance, fit.standardErrors, diagnostics, initial.diagnostics,
        fit.diagnostics, fit.coefficientCovariance, autocorrelation, whitening)
    yield DenseFitBlockResult.fromRobust(input, robust)
    result.left.map(error => source.plan.coefficientAxis.fold(error)(FitKernel.bindRankFailure(error, _)))

private[fit] object BoundedRobustPreparation:
  def prepare(reader: DatasetSeriesReader, plan: FitPlan, chunks: FitChunkPlan): Either[FitError, PreparedFitContext] =
    val result = for
      source <- BoundedResponseReplay.prepare(reader, plan, chunks)
      prepared <- complete(source)
    yield new SpatialReductionFitContext(plan, prepared)
    result.left.map(error => plan.coefficientAxis.fold(error)(FitKernel.bindRankFailure(error, _)))

  private def complete(source: BoundedResponseReplay): Either[FitError, BoundedRobustPrepared] =
    val options = source.plan.config.robust
    var whitening = Option.empty[WhiteningPlan]
    var diagnostics = Option.empty[ArDiagnostics]
    var state = reduce(source, options, whitening) match
      case Left(error) => return Left(error)
      case Right(value) => value
    if options.reestimateAutocorrelation then
      val ar = source.plan.config.autocorrelation
      val setup = for
        config <- Gls.autocorrelationConfig(ar)
        _ <- Robust.validateRobustAutocorrelation(config)
        _ <- Gls.validatePartitions(source.partitions)
        layout <- Gls.noiseEstimationLayout(source.partitions, ar.censoredTimepoints)
      yield (config, layout)
      val (config, layout) = setup match
        case Left(error) => return Left(error)
        case Right(value) => value
      val noiseOptions = ArFitOptions(ArOrder.Fixed(config.order.value),
        if config.global then NoisePooling.Global else NoisePooling.Run, exactFirstAr1 = config.exactFirst)
      var iteration = 0
      while iteration < math.max(1, config.iterations) do
        var summary = Option.empty[ArNoiseSummary]
        source.foreachBlock { input =>
          for
            white <- whiten(input.design, input.response, whitening)
            fit <- Robust.weightedLeastSquares(white._1, white._2, state.weights)
            // The AR estimator consumes original-coordinate residuals.
            residuals = Robust.residualMatrix(input.design.value, input.response.value, fit.coefficients.value)
            next <- ArEstimation.summarizeNoise(residuals, layout, ArOrderValue.unsafe(config.order.value)).left.map(Gls.arToFitError)
            combined <- summary.fold[Either[FitError, ArNoiseSummary]](Right(next))(_.merge(next).left.map(Gls.arToFitError))
          yield summary = Some(combined)
        } match
          case Left(error) => return Left(error)
          case Right(_) => ()
        ArEstimation.fitNoise(summary.get, noiseOptions).left.map(Gls.arToFitError) match
          case Left(error) => return Left(error)
          case Right(value) => whitening = Some(value)
        state = reduce(source, options, whitening) match
          case Left(error) => return Left(error)
          case Right(value) => value
        iteration += 1
      diagnostics = Some(Gls.diagnostics(GlsWhitening.Shared(whitening.get), source.partitions,
        "robust-estimated", Gls.diagnosticIterations(config)))
    Right(new BoundedRobustPrepared(source, state, whitening, diagnostics))

  private def reduce(source: BoundedResponseReplay, options: RobustOptions, whitening: Option[WhiteningPlan]): Either[FitError, RobustReductionState] =
    val rows = source.design.timepoints
    var current = Option.empty[DMat]
    var previous = Option.empty[DMat]
    var scales = Option.empty[RobustScaleEstimates]
    var iterations = 0
    var converged = options.psi == RobustPsi.Disabled
    var maxDelta = 0.0
    if options.psi == RobustPsi.Disabled then
      sharedScales(source, current, whitening, options.scaleScope).map { values =>
        RobustReductionState(Matrix.tabulate(rows, 1)((_, _) => 1.0), None, values, 0, true, 0.0)
      }
    else
      while iterations < options.maxIterations && !converged do
        scales = sharedScales(source, current, whitening, options.scaleScope) match
          case Left(error) => return Left(error)
          case Right(value) => value
        val standardized = ExactRowMedians(rows, source.retainedVoxelIndices.length, consume =>
          source.foreachBlock { input =>
            for
              white <- whiten(input.design, input.response, whitening)
              residuals <- residualsFor(white._1, white._2, current)
              localScales = scales.fold(Robust.scaleEstimates(residuals, source.partitions, options.scaleScope))(repeatScales(_, residuals.cols))
            yield consume(Matrix.tabulate(rows, residuals.cols) { (row, voxel) =>
              math.abs(residuals(row, voxel)) / math.max(Robust.scaleFor(row, voxel, localScales, source.partitions), Robust.Epsilon)
            })
          }) match
          case Left(error) => return Left(error)
          case Right(value) => value
        val next = Matrix.tabulate(rows, 1)((row, _) => Robust.weight(standardized(row), options.psi))
        maxDelta = 0.0
        source.foreachBlock { input =>
          for
            white <- whiten(input.design, input.response, whitening)
            before <- fitFor(white._1, white._2, current)
            after <- Robust.weightedLeastSquares(white._1, white._2, next)
          yield maxDelta = math.max(maxDelta, Robust.maxCoefficientDelta(before.coefficients.value, after.coefficients.value))
        } match
          case Left(error) => return Left(error)
          case Right(_) => ()
        previous = current
        current = Some(next)
        iterations += 1
        converged = maxDelta <= Robust.DefaultTolerance
      Right(RobustReductionState(current.get, previous, scales, iterations, converged,
        if maxDelta.isFinite then maxDelta else 0.0))

  private def sharedScales(source: BoundedResponseReplay, weights: Option[DMat], whitening: Option[WhiteningPlan], scope: ScaleScope): Either[FitError, Option[RobustScaleEstimates]] =
    if scope == ScaleScope.Voxel then Right(None)
    else
      ExactRowMedians(source.design.timepoints, source.retainedVoxelIndices.length, consume =>
        source.foreachBlock { input =>
          for
            white <- whiten(input.design, input.response, whitening)
            residuals <- residualsFor(white._1, white._2, weights)
          yield consume(Matrix.tabulate(residuals.rows, residuals.cols)((row, col) => math.abs(residuals(row, col))))
        }).map { medians =>
          val partitions = Robust.effectivePartitions(source.design.timepoints, source.partitions)
          val (labels, values) = scope match
            case ScaleScope.Global =>
              (Vector("global"), Vector(Robust.robustPositive(Robust.MadScale * Robust.median(Vector.tabulate(medians.length)(medians.apply)))))
            case ScaleScope.Run =>
              (partitions.map(p => s"run_${p.runIndex}"), partitions.map(p =>
                Robust.robustPositive(Robust.MadScale * Robust.median(p.rowIndices.map(medians.apply)))))
            case ScaleScope.Voxel => throw new IllegalStateException("voxel scales are block-local")
          Some(RobustScaleEstimates(scope, labels, Matrix.tabulate(values.length, 1)((row, _) => values(row))))
        }

  private def fitFor(design: DesignMatrix, response: ResponseBlock, weights: Option[DMat]): Either[FitError, OlsFit] =
    weights.fold(Ols.fit(design, response))(Robust.weightedLeastSquares(design, response, _))

  private def residualsFor(design: DesignMatrix, response: ResponseBlock, weights: Option[DMat]): Either[FitError, DMat] =
    fitFor(design, response, weights).map(fit => Robust.residualMatrix(design.value, response.value, fit.coefficients.value))

  private[fit] def whiten(design: DesignMatrix, response: ResponseBlock, plan: Option[WhiteningPlan]): Either[FitError, (DesignMatrix, ResponseBlock)] =
    plan.fold[Either[FitError, (DesignMatrix, ResponseBlock)]](Right(design -> response))(Robust.whitenedInput(_, design, response))

  private def repeatScales(scales: RobustScaleEstimates, voxels: Int): RobustScaleEstimates =
    scales.copy(values = Matrix.tabulate(scales.values.rows, voxels)((row, _) => scales.values(row, 0)))

  private[fit] def blockScales(design: DesignMatrix, response: ResponseBlock, partitions: Vector[RunPartition], scope: ScaleScope,
      previous: Option[DMat], shared: Option[RobustScaleEstimates]): Either[FitError, RobustScaleEstimates] =
    shared match
      case Some(value) => Right(repeatScales(value, response.voxels))
      case None => residualsFor(design, response, previous).map(residuals => Robust.scaleEstimates(residuals, partitions, scope))
