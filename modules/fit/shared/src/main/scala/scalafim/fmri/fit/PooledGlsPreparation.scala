package scalafim.fmri.fit

import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.ar.{ArEstimation, ArFitOptions, ArNoiseSummary, ArOrder, ArOrderValue, NoiseEstimationLayout, NoisePooling, WhiteningPlan, WhiteningMethod, InitialConditionPolicy}
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
    complete(reader, plan, chunks, saveIdentity = false).map(_._1)

  /** Shared whitening for a learned spatial reduction. Response preparation is
    * already frozen; use its effective design and post-filter run geometry.
    */
  private[fit] def shared(source: BoundedResponseReplay): Either[FitError, GlsPrepared] =
    val prepared = makeUnit(source.design, source.partitions, source.plan.config.autocorrelation,
      source.plan.coefficientAxis, None)
    prepared.flatMap(shared(source, _))

  private def shared(source: BoundedResponseReplay, unit: NoiseUnit): Either[FitError, GlsPrepared] =
    var previous = Option.empty[WhiteningPlan]
    var pass = 0
    while pass < math.max(1, unit.config.iterations) do
      var summary = Option.empty[ArNoiseSummary]
      source.foreachBlock { input =>
        val fitted = previous.fold(unit.initial.fit(input.response))(
          Gls.fitWithPlan(_, unit.design.value, input.response.value))
        for
          fit <- fitted
          residuals = Gls.residualMatrix(unit.design.value, input.response.value, fit.coefficients.value)
          next <- ArEstimation.summarizeNoise(residuals, unit.layout, ArOrderValue.unsafe(unit.config.order.value)).left.map(Gls.arToFitError)
          combined <- summary.fold[Either[FitError, ArNoiseSummary]](Right(next))(_.merge(next).left.map(Gls.arToFitError))
        yield summary = Some(combined)
      } match
        case Left(error) => return Left(unit.bind(error))
        case Right(_) => ()
      ArEstimation.fitNoise(summary.get, unit.noiseOptions).left.map(Gls.arToFitError) match
        case Left(error) => return Left(unit.bind(error))
        case Right(value) => previous = Some(value)
      pass += 1
    Right(unit.prepared(previous.get, source.retainedVoxelIndices))

  private[fit] def complete(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunks: FitChunkPlan,
      saveIdentity: Boolean = true
  ): Either[FitError, (PreparedFitContext, CompletedGlsNoise)] =
    val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
    reduce(reader, plan, chunks, partitions, setup(plan, chunks), saveIdentity)

  private def setup(plan: FitPlan, chunks: FitChunkPlan): Either[FitError, Vector[NoiseUnit]] =
    val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
    for
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

  private def identity(plan: FitPlan, units: Vector[NoiseUnit]): String =
    val header = Vector("pooled-gls-noise-v1", plan.coefficientScope.toString,
      plan.config.missingData.toString,
      plan.model.designSchema.fold("")(_.rows.canonical),
      FitWorkDescriptor.frame(plan.structuralColumns.map(column =>
        FitWorkDescriptor.frame(Vector(column.canonical, column.renderedLabel)))))
    val designs = units.map { unit =>
      FitWorkDescriptor.frame(Vector(
        unit.run.fold("shared")(_.partition.runIndex.toString),
        PreparedGlsArtifact.matrixIdentity(unit.design.value),
        unit.config.order.value.toString, unit.config.iterations.toString,
        unit.config.global.toString, unit.config.voxelwise.toString, unit.config.exactFirst.toString,
        unit.layout.excludedRows.mkString(","),
        unit.partitions.map(p => s"${p.runIndex}:${p.rowIndices.mkString(",")}:${p.timepoints.mkString(",")}").mkString(";"),
        unit.run.flatMap(_.projection).fold("")(_.sourceColumnIndices.mkString(",")),
        PreparedGlsArtifact.policyIdentity(unit.initial.diagnostics.policy)
      ))
    }
    FitWorkDescriptor.frame(header ++ designs)

  private[fit] def restore(
      plan: FitPlan,
      chunks: FitChunkPlan,
      saved: CompletedGlsNoise
  ): Either[FitError, PreparedFitContext] =
    setup(plan, chunks).flatMap { units =>
      if saved.designIdentity != identity(plan, units) then
        Left(PreparedGlsArtifact.invalid("design or preparation configuration differs"))
      else validateWhitening(units, saved.units).map { _ =>
        context(plan, chunks, units, saved.units.map(_.whitening), saved.retainedVoxelIndices)
      }
    }

  private def validateWhitening(units: Vector[NoiseUnit], saved: Vector[PreparedGlsUnit]): Either[FitError, Unit] =
    if units.length != saved.length then
      return Left(PreparedGlsArtifact.invalid("preparation unit count differs"))
    var index = 0
    while index < units.length do
      val unit = units(index)
      val snapshot = saved(index)
      val w = snapshot.whitening
      val pooling = if unit.config.global then NoisePooling.Global else NoisePooling.Run
      val initial = InitialConditionPolicy.fromExactFirstAr1(unit.config.exactFirst)
      val order = unit.config.order.value
      if unit.layout.retainedRows == 0 || (0 until unit.layout.runCount).exists { run =>
          val segments = unit.layout.segmentsForRun(run)
          segments.map(_.length).sum > 1 && !segments.exists(_.length > order)
        } then return Left(PreparedGlsArtifact.invalid(s"AR order is not estimable for unit $index"))
      val expectedOrders =
        if pooling == NoisePooling.Global then
          Vector(if (0 until unit.layout.runCount).exists(r => unit.layout.segmentsForRun(r).map(_.length).sum > 1) then order else 0)
        else Vector.tabulate(unit.layout.runCount)(r => if unit.layout.segmentsForRun(r).map(_.length).sum > 1 then order else 0)
      if snapshot.sourceRun != unit.run.map(_.partition.runIndex) ||
          w.segments != unit.layout.whiteningSegments || w.pooling != pooling ||
          w.initialCondition != initial || w.method != WhiteningMethod.Estimated || w.maOrder != 0 ||
          w.coefficients.map(_.arOrder) != expectedOrders then
        return Left(PreparedGlsArtifact.invalid(s"whitening contract differs for unit $index"))
      index += 1
    Right(())

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
      setup: => Either[FitError, Vector[NoiseUnit]],
      saveIdentity: Boolean
  ): Either[FitError, (PreparedFitContext, CompletedGlsNoise)] =
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
          return Left(FitError.PreparationReplayMismatch(s"pooled AR finite voxel membership changed in chunk ${chunk.ordinal.value}"))
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
    val saved = CompletedGlsNoise(if saveIdentity then identity(plan, units) else "", voxels,
      units.zip(previous).map((unit, whitening) => PreparedGlsUnit(unit.run.map(_.partition.runIndex), whitening)))
    Right((context(plan, chunks, units, previous, voxels), saved))

  private def context(
      plan: FitPlan,
      chunks: FitChunkPlan,
      units: Vector[NoiseUnit],
      whitening: Vector[WhiteningPlan],
      voxels: Vector[Int]
  ): PreparedFitContext =
    val fitted = plan.strategy match
      case FitStrategy.RunwiseGeneralizedLeastSquares(_, _) =>
        val runs = units.zip(whitening).map { (unit, value) =>
          val source = unit.run.get
          RunwiseGlsPreparedRun(source.partition, unit.prepared(value, voxels), source.projection)
        }
        FitInterpreters.runwiseGlsContext(plan, new RunwiseGlsPrepared(chunks.timepoints.length, runs))
      case _ => FitInterpreters.glsContext(plan, units.head.prepared(whitening.head, voxels))
    // The final pass must fit exactly the population that estimated the shared whitening.
    new RetainedMembershipFitContext(fitted, voxels.toSet, plan.config.missingData)
