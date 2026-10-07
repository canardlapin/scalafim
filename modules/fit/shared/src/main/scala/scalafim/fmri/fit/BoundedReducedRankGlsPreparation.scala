package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Matrix, QR, Vec}
import scalafim.dataset.{DatasetSeriesReader, FmriSeries}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{FitEngine, FitPlan, ReducedRankGlsConfig, ReducedRankInferencePolicy}
import ReducedRankGlsProjection.*

/** Bounded response reads with population-wide learned state. The exact p×V
  * task scores go through the SAME full SVD as dense fitting, preserving its
  * tied and null singular-vector conventions. Retained V×r bases (one per
  * bootstrap replicate) are declared state, not constant-memory preparation.
  */
private[fit] object BoundedReducedRankGlsPreparation:
  private[fit] final case class Geometry(
      whitenedDesign: DMat,
      targetDesign: DMat,
      nuisanceDesign: DMat,
      residualizedTargetDesign: DMat,
      targetQr: QR,
      whitening: WhiteningPlan,
      gls: GlsPrepared,
      ols: OlsPrepared,
      partition: ReducedRankDesignPartition
  ):
    def response(input: FitBlockInput): Either[FitError, (DMat, DMat)] =
      for
        white <- WhiteningTransform(whitening, input.design.value, input.response.value).left.map(Gls.arToFitError)
        residualized <- residualizeAgainstNuisance(targetDesign, white.response, nuisanceDesign)
      yield white.response -> residualized.response

  def prepare(reader: DatasetSeriesReader, plan: FitPlan, chunks: FitChunkPlan): Either[FitError, PreparedFitContext] =
    val result = for
      config <- FitInterpreters.reducedRankGlsConfig(plan)
      partition <- FitInterpreters.reducedRankDesignPartition(plan)
      source <- BoundedResponseReplay.prepare(reader, plan, chunks)
      request <- ReducedRankGlsPrepared.rankRequest(config, partition.targetPredictors, source.retainedVoxelIndices.length)
      prepared <-
        if config.autocorrelation.voxelwise then
          if ReducedRankGlsPrepared.isFullRankRequest(config, partition.targetPredictors, source.retainedVoxelIndices.length) &&
              config.inference == ReducedRankInferencePolicy.Conditional
          then Right(new VoxelwisePrepared(source, config, partition))
          else Left(FitError.UnsupportedEngine(
            "compressed or bootstrap ReducedRankGls requires shared GLS whitening; voxelwise-AR reduced-rank geometry is tracked separately"))
        else prepareShared(source, config, partition, request)
    yield new SpatialReductionFitContext(plan, prepared)
    result.left.map(error => plan.coefficientAxis.fold(error)(FitKernel.bindRankFailure(error, _)))

  private def prepareShared(source: BoundedResponseReplay, config: ReducedRankGlsConfig,
      partition: ReducedRankDesignPartition, request: ReducedRankRequest): Either[FitError, PreparedSpatialReduction] =
    for
      gls <-
        if FitPreparation.fixedAr(source.plan.config.autocorrelation) then
          Gls.prepare(source.design, ResponseBlock.unsafe(Matrix.zeros(source.design.timepoints, 1)), source.partitions,
            config.autocorrelation.toLegacy, Vector(0)).map(_.copy(selectedVoxelIndices = source.retainedVoxelIndices))
        else PooledGlsPreparation.shared(source)
      whitening <- gls.whitening match
        case GlsWhitening.Shared(value) => Right(value)
        case _ => Left(FitError.UnsupportedEngine("reduced-rank shared preparation requires shared whitening"))
      white <- WhiteningTransform(whitening, source.design.value, Matrix.zeros(source.design.timepoints, 1)).left.map(Gls.arToFitError)
      target = selectColumns(white.design, partition.targetColumns)
      nuisance = selectColumns(white.design, partition.nuisanceColumns)
      residualized <- residualizeAgainstNuisance(target, white.response, nuisance)
      qr <- fullRankQr(residualized.targetDesign)
      ols <- Ols.prepare(DesignMatrix.unsafe(white.design))
      geometry = Geometry(white.design, target, nuisance, residualized.targetDesign, qr, whitening, gls, ols, partition)
      scores <- taskScores(source, geometry, None)
      task <- fitTaskScores(qr, scores, request)
      inference <- config.inference match
        case ReducedRankInferencePolicy.Conditional =>
          conditionalSigma(source, geometry, task).map(sigma => LearnedInference.Conditional(sigma))
        case ReducedRankInferencePolicy.Bootstrap(bootstrap) =>
          val rng = ParkMillerRng(bootstrap.seed.value)
          val fits = Vector.newBuilder[ReducedTaskFit]
          var failure = Option.empty[FitError]
          var replicate = 0
          while replicate < bootstrap.replicates.value && failure.isEmpty do
            val indices = sampleIndices(source.design.timepoints, bootstrap.blockSize.value, rng)
            val next = for
              sampledScores <- taskScores(source, geometry, Some(task -> indices))
              sampledFit <- fitTaskScores(qr, sampledScores, request)
            yield sampledFit
            next match
              case Left(error) => failure = Some(error)
              case Right(value) => fits += value
            replicate += 1
          failure match
            case Some(error) => Left(error)
            case None => Right(LearnedInference.Bootstrap(fits.result(), bootstrap.replicates.value,
              bootstrap.blockSize.value, bootstrap.seed.value))
    yield new SharedPrepared(source, geometry, task, inference)

  /** One global matrix assembled in retained selection order, never T×V. */
  private[fit] def taskScores(source: BoundedResponseReplay, geometry: Geometry,
      resample: Option[(ReducedTaskFit, Array[Int])]): Either[FitError, DMat] =
    val out = Matrix.newBuilder(geometry.partition.targetPredictors, source.retainedVoxelIndices.length)
    val positions = source.retainedVoxelIndices.zipWithIndex.toMap
    source.foreachBlock { input =>
      for
        response <- geometry.response(input)
        selected = input.voxelIndices.map(positions)
        residualized = response._2
        sampled = resample.fold(residualized) { (original, indices) =>
          val fitted = geometry.residualizedTargetDesign * localCoefficients(original, selected)
          resampledResponse(fitted, subtract(residualized, fitted), indices)
        }
        scores <- leadingQtRows(geometry.targetQr, sampled, geometry.partition.targetPredictors)
      yield
        var row = 0
        while row < scores.rows do
          var col = 0
          while col < scores.cols do
            out(row, selected(col)) = scores(row, col)
            col += 1
          row += 1
    }.map(_ => out.result())

  private def localBasis(task: ReducedTaskFit, positions: Vector[Int]): DMat =
    Matrix.tabulate(positions.length, task.basis.cols)((row, col) => task.basis(positions(row), col))

  private def localCoefficients(task: ReducedTaskFit, positions: Vector[Int]): DMat =
    task.latentCoefficients * localBasis(task, positions).t

  private def conditionalSigma(source: BoundedResponseReplay, geometry: Geometry, task: ReducedTaskFit): Either[FitError, DMat] =
    val latent = Matrix.newBuilder(source.design.timepoints, task.basis.cols)
    val positions = source.retainedVoxelIndices.zipWithIndex.toMap
    source.foreachBlock { input =>
      geometry.response(input).map { (_, response) =>
        val contribution = response * localBasis(task, input.voxelIndices.map(positions))
        var row = 0
        while row < contribution.rows do
          var col = 0
          while col < contribution.cols do
            latent(row, col) = latent(row, col) + contribution(row, col)
            col += 1
          row += 1
      }
    }.map { _ =>
      val residual = subtract(latent.result(), geometry.residualizedTargetDesign * task.latentCoefficients)
      scaleMatrix(residual.t * residual, 1.0 / (source.design.timepoints - geometry.ols.diagnostics.rank).toDouble)
    }

  private enum LearnedInference:
    case Conditional(sigma: DMat)
    case Bootstrap(fits: Vector[ReducedTaskFit], replicates: Int, blockSize: Int, seed: Int)

  private final class SharedPrepared(source: BoundedResponseReplay, geometry: Geometry,
      task: ReducedTaskFit, learned: LearnedInference) extends PreparedSpatialReduction:
    val engine: FitEngine = FitEngine.ReducedRankGls
    private val positions = source.retainedVoxelIndices.zipWithIndex.toMap
    private val scope = CoefficientInferenceScope.unsafeOnly(geometry.partition.targetColumns, "reduced-rank GLS target/event coefficient")
    private val df = ResidualDegreesOfFreedom.unsafe(source.design.timepoints - geometry.ols.diagnostics.rank)

    def fit(series: FmriSeries): Either[FitError, FitBlockResult] =
      val result = for
        input <- source.input(series)
        response <- geometry.response(input)
        selected = input.voxelIndices.map(positions)
        targetCoefficients = localCoefficients(task, selected)
        nuisanceCoefficients <- fitNuisance(geometry.nuisanceDesign,
          subtract(response._1, geometry.targetDesign * targetCoefficients))
        coefficients = CoefficientBlock(Matrix.tabulate(geometry.partition.predictors, input.voxelIndices.length) { (row, col) =>
          val target = geometry.partition.targetColumns.indexOf(row)
          if target >= 0 then targetCoefficients(target, col)
          else nuisanceCoefficients(geometry.partition.nuisanceColumns.indexOf(row), col)
        })
        inference <- learned match
          case LearnedInference.Conditional(sigma) =>
            for
              variance <- conditionalVariance(task, selected, sigma)
              covariance <- CoefficientCovariance.shared(embedTargetCovariance(task.normalizedCovariance,
                geometry.partition.predictors, geometry.partition.targetColumns))
              value <- CoefficientInference.fromCovariance(scope, covariance, variance, df, CoefficientInferenceMethod.ReducedRankConditional)
            yield value
          case LearnedInference.Bootstrap(fits, replicates, blockSize, seed) =>
            for
              covariance <- bootstrapCovariance(fits, selected, geometry.partition)
              value <- CoefficientInference.fromCovariance(scope, covariance, DVec.fromSeq(Vector.fill(selected.length)(1.0)), df,
                CoefficientInferenceMethod.ReducedRankBootstrap(replicates, blockSize, seed))
            yield value
        residualVariance = Ols.residualVariance(geometry.whitenedDesign, response._1, coefficients.value, df)
      yield DenseFitBlockResult(coefficients, inference, residualVariance, df, input.voxelIndices, input.timepoints,
        engine, olsDiagnostics = Some(geometry.ols.diagnostics), autocorrelation = Some(geometry.gls.diagnostics),
        coefficientAxis = input.coefficientAxis, preparationProvenance = input.preparationProvenance,
        voxelStatuses = Some(VoxelFitStatus.refine(input.resolvedVoxelStatuses, residualVariance)), fitExclusions = input.fitExclusions)
      result.left.map(error => source.plan.coefficientAxis.fold(error)(FitKernel.bindRankFailure(error, _)))

  private final class VoxelwisePrepared(source: BoundedResponseReplay, config: ReducedRankGlsConfig,
      partition: ReducedRankDesignPartition) extends PreparedSpatialReduction:
    val engine: FitEngine = FitEngine.ReducedRankGls
    def fit(series: FmriSeries): Either[FitError, FitBlockResult] =
      val result = for
        input <- source.input(series)
        gls <- Gls.prepare(input.design, input.response, input.partitions, config.autocorrelation.toLegacy, input.voxelIndices)
        fit <- gls.fit(input.response, input.voxelIndices)
        block = DenseFitBlockResult.fromGls(input, fit, engine)
        inference <- block.inference.restrict(CoefficientInferenceScope.unsafeOnly(partition.targetColumns,
          "reduced-rank GLS target/event coefficient"), CoefficientInferenceMethod.ReducedRankFullRankVoxelwiseFallback)
      yield block.copy(inference = inference)
      result.left.map(error => source.plan.coefficientAxis.fold(error)(FitKernel.bindRankFailure(error, _)))

  private def conditionalVariance(task: ReducedTaskFit, positions: Vector[Int], sigma: DMat): Either[FitError, DVec] =
    val out = Vec.newBuilder(positions.length)
    var voxel = 0
    while voxel < positions.length do
      var value = 0.0
      var left = 0
      while left < task.basis.cols do
        var right = 0
        while right < task.basis.cols do
          value += task.basis(positions(voxel), left) * sigma(left, right) * task.basis(positions(voxel), right)
          right += 1
        left += 1
      if !value.isFinite || value < -1e-12 then
        return Left(FitError.InvalidFitAxis("reduced-rank GLS conditional variance", s"voxel ${positions(voxel)} has value $value"))
      out(voxel) = math.max(value, 2.220446049250313e-16)
      voxel += 1
    Right(out.result())

  private def bootstrapCovariance(fits: Vector[ReducedTaskFit], positions: Vector[Int],
      partition: ReducedRankDesignPartition): Either[FitError, CoefficientCovariance] =
    val predictors = partition.targetPredictors
    val voxels = positions.length
    val mean = new Array[Double](predictors * voxels)
    val m2 = new Array[Double](voxels * predictors * predictors)
    val delta = new Array[Double](predictors)
    var replicate = 0
    while replicate < fits.length do
      val coefficients = localCoefficients(fits(replicate), positions)
      var voxel = 0
      while voxel < voxels do
        var predictor = 0
        while predictor < predictors do
          val index = predictor * voxels + voxel
          delta(predictor) = coefficients(predictor, voxel) - mean(index)
          mean(index) += delta(predictor) / (replicate + 1).toDouble
          predictor += 1
        var left = 0
        while left < predictors do
          var right = 0
          while right < predictors do
            m2((voxel * predictors + left) * predictors + right) += delta(left) * (coefficients(right, voxel) - mean(right * voxels + voxel))
            right += 1
          left += 1
        voxel += 1
      replicate += 1
    CoefficientCovariance.voxelwise(Vector.tabulate(voxels) { voxel =>
      val task = Matrix.tabulate(predictors, predictors)((row, col) =>
        m2((voxel * predictors + row) * predictors + col) / (fits.length - 1).toDouble)
      embedTargetCovariance(task, partition.predictors, partition.targetColumns)
    })
