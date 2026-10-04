package scalafim.fmri.fit

import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.ar.{ArEstimation, ArFitOptions, ArNoiseSummary, ArOrder, ArOrderValue, NoiseEstimationLayout, NoisePooling, WhiteningPlan}
import scalafim.fmri.design.CoefficientAxis
import scalafim.fmri.model.{ArOptions, AutocorrelationConfig, FitPlan, FitStrategy}

/** Replays spatial blocks for each noise-estimation pass. Only raw per-run lag
  * statistics cross the reduction boundary; no population response or residual
  * matrix is retained. Sources must replay the same immutable response snapshot.
  */
private[fit] object PooledGlsPreparation:
  private final case class NoiseUnit(
      design: DesignMatrix,
      partitions: Vector[RunPartition],
      config: AutocorrelationConfig,
      layout: NoiseEstimationLayout,
      initial: OlsPrepared,
      axis: Option[CoefficientAxis],
      run: Option[RunwiseGlsDesign]
  ):
    def bind(error: FitError): FitError =
      val structural = axis.fold(error)(FitKernel.bindRankFailure(error, _))
      run.fold(structural)(value => FitError.RunwiseFitFailed(value.partition.runIndex, structural))

    def response(input: ResponseBlock): ResponseBlock =
      run.fold(input)(value => ResponseBlock.unsafe(RunwiseGls.selectRows(input.value, value.partition.rowIndices)))

    def noiseOptions: ArFitOptions =
      ArFitOptions(ArOrder.Fixed(config.order.value),
        if config.global then NoisePooling.Global else NoisePooling.Run,
        exactFirstAr1 = config.exactFirst)

    def prepared(whitening: WhiteningPlan, voxels: Vector[Int]): GlsPrepared =
      val shared = GlsWhitening.Shared(whitening)
      GlsPrepared(design, partitions, shared,
        Gls.diagnostics(shared, partitions, "estimated", Gls.diagnosticIterations(config)),
        initial.diagnostics, voxels)

  def prepare(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunks: FitChunkPlan
  ): Either[FitError, PreparedFitContext] =
    val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
    def setup = for
      shapeOnly <- FitPreparation.designSeries(plan, chunks.timepoints)
      input <- FitPlanExecutor.fitBlockInput(plan, shapeOnly, partitions)
      units <- plan.strategy match
        case FitStrategy.RunwiseGeneralizedLeastSquares(_, _) =>
          RunwiseGls.prepareDesigns(input.design, partitions, plan.config.autocorrelation, input.runwiseProjections)
            .flatMap(runUnits)
        case _ =>
          makeUnit(input.design, partitions, plan.config.autocorrelation, input.coefficientAxis, None)
            .map(Vector(_))
    yield units
    reduce(reader, plan, chunks, partitions, setup)

  private def makeUnit(
      design: DesignMatrix,
      partitions: Vector[RunPartition],
      options: ArOptions,
      axis: Option[CoefficientAxis],
      run: Option[RunwiseGlsDesign]
  ): Either[FitError, NoiseUnit] =
    val prepared = for
      config <- Gls.autocorrelationConfig(options)
      _ <- Gls.validatePartitions(partitions)
      layout <- Gls.noiseEstimationLayout(partitions, options.censoredTimepoints)
      initial <- Ols.prepare(design)
      _ <- ResidualDegreesOfFreedom(design.timepoints - initial.diagnostics.rank)
    yield NoiseUnit(design, partitions, config, layout, initial, axis, run)
    prepared.left.map { error =>
      val structural = axis.fold(error)(FitKernel.bindRankFailure(error, _))
      run.fold(structural)(value => FitError.RunwiseFitFailed(value.partition.runIndex, structural))
    }

  private def runUnits(designs: Vector[RunwiseGlsDesign]): Either[FitError, Vector[NoiseUnit]] =
    val out = Vector.newBuilder[NoiseUnit]
    var index = 0
    while index < designs.length do
      val run = designs(index)
      makeUnit(run.design, Vector(run.localPartition), run.options, run.projection.map(_.axis), Some(run)) match
        case Left(error) => return Left(error)
        case Right(unit) => out += unit
      index += 1
    Right(out.result())

  private def reduce(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunks: FitChunkPlan,
      partitions: Vector[RunPartition],
      setup: => Either[FitError, Vector[NoiseUnit]]
  ): Either[FitError, PreparedFitContext] =
    // Membership is metadata only. Check that a legacy reader does not change
    // the finite-column population between preparation passes.
    val membership = Array.fill(chunks.length)(Vector.empty[Int])
    val excluded = Vector.newBuilder[VoxelInferenceExclusion]
    val iterations = math.max(1, plan.config.autocorrelation.iterations)
    var units = Vector.empty[NoiseUnit]
    var previous = Vector.empty[WhiteningPlan]
    var pass = 0
    while pass < iterations do
      var summaries = Array.fill[Option[ArNoiseSummary]](units.length)(None)
      var chunkIndex = 0
      while chunkIndex < chunks.length do
        val chunk = chunks.indexed(chunkIndex)
        val input = for
          series <- ChunkedFitExecutor.readChunk(reader, chunk)
          prepared <- FitPlanExecutor.fitBlockInput(plan, series, partitions)
        yield prepared
        val currentVoxels = input match
          case Left(FitError.AllVoxelsExcluded(values)) =>
            if pass == 0 then excluded ++= values
            Vector.empty
          case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
          case Right(value) => value.voxelIndices
        if pass == 0 then membership(chunkIndex) = currentVoxels
        else if membership(chunkIndex) != currentVoxels then
          return Left(FitError.InvalidFitAxis("pooled AR replay", s"finite voxel membership changed in chunk ${chunk.ordinal.value}"))
        input match
          case Left(_) => () // an entirely excluded spatial block
          case Right(value) =>
            // Preserve all-excluded precedence over design rank/df failures.
            if units.isEmpty then
              setup match
                case Left(error) => return Left(error)
                case Right(value) => units = value
              summaries = Array.fill[Option[ArNoiseSummary]](units.length)(None)
            var unitIndex = 0
            while unitIndex < units.length do
              val unit = units(unitIndex)
              val response = unit.response(value.response)
              val fitted = if previous.isEmpty then unit.initial.fit(response)
                else Gls.fitWithPlan(previous(unitIndex), unit.design.value, response.value)
              val summarized = for
                fit <- fitted
                // Noise estimation uses ORIGINAL residuals, not whitened ones.
                residuals = Gls.residualMatrix(unit.design.value, response.value, fit.coefficients.value)
                summary <- ArEstimation.summarizeNoise(residuals, unit.layout, ArOrderValue.unsafe(unit.config.order.value))
                  .left.map(Gls.arToFitError)
                combined <- summaries(unitIndex) match
                  case None => Right(summary)
                  case Some(current) => current.merge(summary).left.map(Gls.arToFitError)
              yield combined
              summarized match
                case Left(error) => return Left(unit.bind(error))
                case Right(summary) => summaries(unitIndex) = Some(summary)
              unitIndex += 1
        chunkIndex += 1
      if summaries.isEmpty || summaries.head.isEmpty then return Left(FitError.AllVoxelsExcluded(excluded.result()))
      val completed = Vector.newBuilder[WhiteningPlan]
      var unitIndex = 0
      while unitIndex < units.length do
        val unit = units(unitIndex)
        ArEstimation.fitNoise(summaries(unitIndex).get, unit.noiseOptions).left.map(Gls.arToFitError) match
          case Left(error) => return Left(unit.bind(error))
          case Right(whitening) => completed += whitening
        unitIndex += 1
      previous = completed.result()
      pass += 1
    val voxels = membership.iterator.flatten.toVector
    plan.strategy match
      case FitStrategy.RunwiseGeneralizedLeastSquares(_, _) =>
        val runs = units.zip(previous).map { (unit, whitening) =>
          val source = unit.run.get
          RunwiseGlsPreparedRun(source.partition, unit.prepared(whitening, voxels), source.projection)
        }
        Right(FitInterpreters.runwiseGlsContext(plan, new RunwiseGlsPrepared(chunks.timepoints.length, runs)))
      case _ =>
        Right(FitInterpreters.glsContext(plan, units.head.prepared(previous.head, voxels)))
