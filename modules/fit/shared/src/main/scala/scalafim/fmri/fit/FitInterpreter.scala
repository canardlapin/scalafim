package scalafim.fmri.fit

import scalafim.dataset.FmriSeries
import scalafim.fmri.model.{
  ArOptions,
  DesignColumnSource,
  FitConfig,
  FitEngine,
  FitPlan,
  FitStrategy,
  LatentSketchConfig,
  ReducedRankGlsConfig
}

trait FitInterpreter:
  type Prepared
  type Block <: FitBlockResult
  type Result <: FmriFitResult

  def engine: FitEngine

  def fit(plan: FitPlan, series: FmriSeries): Either[FitError, Result]

  def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, Prepared]

  def fitChunk(
      plan: FitPlan,
      series: FmriSeries,
      prepared: Prepared
  ): Either[FitError, Block]

  def merge(
      plan: FitPlan,
      chunks: IndexedSeq[Block]
  ): Either[FitError, Result]

  private[fit] final def prepareContext(
      plan: FitPlan,
      series: FmriSeries
  ): Either[FitError, PreparedFitContext] =
    prepare(plan, series).map { prepared =>
      new PreparedFitContext(
        engine = engine,
        runChunk = chunkSeries => fitChunk(plan, chunkSeries, prepared),
        combine = chunks => mergeAny(plan, chunks)
      )
    }

  private[fit] final def mergeAny(
      plan: FitPlan,
      chunks: IndexedSeq[FitBlockResult]
  ): Either[FitError, FmriFitResult] =
    collect(chunks).flatMap(typed => merge(plan, typed))

  protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[Block]]

private[fit] final case class PreparedFitContext private[fit] (
    engine: FitEngine,
    private val runChunk: FmriSeries => Either[FitError, FitBlockResult],
    private val combine: IndexedSeq[FitBlockResult] => Either[FitError, FmriFitResult]
):
  def fitChunk(series: FmriSeries): Either[FitError, FitBlockResult] =
    runChunk(series)

  def merge(chunks: IndexedSeq[FitBlockResult]): Either[FitError, FmriFitResult] =
    combine(chunks)

private[fit] final case class OlsExecutionPrepared(
    solver: OlsPrepared,
    responsePreparation: ResolvedResponsePreparation,
    partitions: Vector[RunPartition]
)

object FitInterpreters:
  private def bindRankFailure(input: FitBlockInput, error: FitError): FitError =
    input.coefficientAxis match
      case Some(axis) => FitKernel.bindRankFailure(error, axis)
      case None       => error

  def forPlan(plan: FitPlan): Either[FitError, FitInterpreter] =
    forEngine(plan.engine)

  def forEngine(engine: FitEngine): Either[FitError, FitInterpreter] =
    engine match
      case FitEngine.OrdinaryLeastSquares    => Right(OrdinaryLeastSquares)
      case FitEngine.GeneralizedLeastSquares => Right(GeneralizedLeastSquares)
      case FitEngine.RobustLeastSquares      => Right(RobustLeastSquares)
      case FitEngine.RunwiseLeastSquares     => Right(RunwiseLeastSquares)
      case FitEngine.FixedEffects            => Right(SeparateRunsThenFixedEffects)
      case FitEngine.LeastSquaresSeparate    => Right(LeastSquaresSeparate)
      case FitEngine.LatentSketch            => Right(LatentSketch)
      case FitEngine.ReducedRankGls          => Right(ReducedRankGls)

  private def robustAutocorrelation(config: FitConfig): Option[ArOptions] =
    if config.robust.reestimateAutocorrelation then Some(config.autocorrelation) else None

  private def latentSketchConfig(plan: FitPlan): Either[FitError, LatentSketchConfig] =
    plan.strategy match
      case FitStrategy.LatentSketch(sketch, _) => Right(sketch)
      case _ => Left(FitError.UnsupportedEngine(s"${plan.engine} plan does not carry a latent sketch config"))

  private def reducedRankGlsConfig(plan: FitPlan): Either[FitError, ReducedRankGlsConfig] =
    plan.strategy match
      case FitStrategy.ReducedRankGls(lowRank, _) => Right(lowRank)
      case _ => Left(FitError.UnsupportedEngine(s"${plan.engine} plan does not carry a reduced-rank GLS config"))

  private def reducedRankDesignPartition(plan: FitPlan): Either[FitError, ReducedRankDesignPartition] =
    val targets = Vector.newBuilder[Int]
    val nuisance = Vector.newBuilder[Int]
    plan.model.designBlock.columns.zipWithIndex.foreach { case (column, index) =>
      column.source match
        case DesignColumnSource.Event =>
          targets += index
        case DesignColumnSource.Baseline =>
          nuisance += index
    }
    ReducedRankDesignPartition.fromColumns(plan.model.nPredictors, targets.result(), nuisance.result())

  private object OrdinaryLeastSquares extends FitInterpreter:
    type Prepared = OlsExecutionPrepared
    type Block = DenseFitBlockResult
    type Result = DenseFmriFitResult

    val engine: FitEngine = FitEngine.OrdinaryLeastSquares

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, DenseFmriFitResult] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        dense <- FitKernel.fitDense(input, engine, plan.config)
      yield FitPlanExecutor.denseResult(plan, dense)

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, OlsExecutionPrepared] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        raw <- FitPlanExecutor.rawFitBlockInput(plan, series, partitions = partitions)
        responsePreparation <- ResponsePreparationPlan.fromPlan(plan).resolve(raw)
        prepared <- responsePreparation.prepare(raw)
        solver <- Ols.prepare(prepared.input.design).left.map(error => bindRankFailure(prepared.input, error))
      yield OlsExecutionPrepared(solver, responsePreparation, partitions)

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: OlsExecutionPrepared
    ): Either[FitError, DenseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(
          plan,
          series,
          prepared.partitions,
          prepared.responsePreparation
        )
        fit <- prepared.solver.fit(input.response)
      yield DenseFitBlockResult.fromOls(input, fit, engine)

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[DenseFitBlockResult]
    ): Either[FitError, DenseFmriFitResult] =
      DenseFitBlockResult.merge(chunks).map(FitPlanExecutor.denseResult(plan, _))

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[DenseFitBlockResult]] =
      FitBlockCollectors.dense(chunks)

  private object GeneralizedLeastSquares extends FitInterpreter:
    type Prepared = GlsPrepared
    type Block = DenseFitBlockResult
    type Result = DenseFmriFitResult

    val engine: FitEngine = FitEngine.GeneralizedLeastSquares

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, DenseFmriFitResult] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        dense <- FitKernel.fitDense(input, engine, plan.config)
      yield FitPlanExecutor.denseResult(plan, dense)

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, GlsPrepared] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        prepared <- Gls
          .prepare(input.design, input.response, partitions, plan.config.autocorrelation, series.voxelIndices)
          .left
          .map(error => bindRankFailure(input, error))
      yield prepared

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: GlsPrepared
    ): Either[FitError, DenseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = prepared.partitions)
        fit <- prepared.fit(input.response, input.voxelIndices)
      yield DenseFitBlockResult.fromGls(input, fit)

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[DenseFitBlockResult]
    ): Either[FitError, DenseFmriFitResult] =
      DenseFitBlockResult.merge(chunks).map(FitPlanExecutor.denseResult(plan, _))

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[DenseFitBlockResult]] =
      FitBlockCollectors.dense(chunks)

  private object RunwiseLeastSquares extends FitInterpreter:
    type Prepared = Vector[RunPartition]
    type Block = RunwiseFitBlockResult
    type Result = RunwiseFmriFitResult

    val engine: FitEngine = FitEngine.RunwiseLeastSquares

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, RunwiseFmriFitResult] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        runwise <- FitKernel.fitRunwise(input)
      yield RunwiseFmriFitResult(
        runs = runwise.runs,
        columnNames = plan.model.columnNames,
        voxelIndices = runwise.voxelIndices,
        timepoints = runwise.timepoints,
        engine = engine,
        summary = plan.summary,
        coefficientAxis = runwise.coefficientAxis,
        preparationProvenance = runwise.preparationProvenance
      )

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, Vector[RunPartition]] =
      Right(RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints))

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: Vector[RunPartition]
    ): Either[FitError, RunwiseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = prepared)
        runwise <- FitKernel.fitRunwise(input)
      yield runwise

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[RunwiseFitBlockResult]
    ): Either[FitError, RunwiseFmriFitResult] =
      RunwiseFitBlockResult.merge(chunks).map { merged =>
        RunwiseFmriFitResult(
          runs = merged.runs,
          columnNames = plan.model.columnNames,
          voxelIndices = merged.voxelIndices,
          timepoints = merged.timepoints,
          engine = engine,
          summary = plan.summary,
          coefficientAxis = merged.coefficientAxis,
          preparationProvenance = merged.preparationProvenance
        )
      }

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[RunwiseFitBlockResult]] =
      FitBlockCollectors.runwise(chunks)

  /** Fit each run with the existing runwise OLS kernel, then combine the
    * resulting coefficient covariances as a fixed-effects estimand.  Keeping
    * the runwise block path here means chunking and multi-response execution
    * reuse the same numerical and boundary checks as RunwiseLeastSquares.
    */
  private object SeparateRunsThenFixedEffects extends FitInterpreter:
    type Prepared = Vector[RunPartition]
    type Block = RunwiseFitBlockResult
    type Result = FixedEffectsFmriFitResult

    val engine: FitEngine = FitEngine.FixedEffects

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, FixedEffectsFmriFitResult] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        runwise <- FitKernel.fitRunwise(input)
        runwiseResult = RunwiseFmriFitResult(
          runs = runwise.runs,
          columnNames = plan.model.columnNames,
          voxelIndices = runwise.voxelIndices,
          timepoints = runwise.timepoints,
          engine = FitEngine.RunwiseLeastSquares,
          summary = plan.summary,
          coefficientAxis = runwise.coefficientAxis,
          preparationProvenance = runwise.preparationProvenance
        )
        fixed <- FixedEffects.combine(runwiseResult)
      yield fixed

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, Vector[RunPartition]] =
      Right(RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints))

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: Vector[RunPartition]
    ): Either[FitError, RunwiseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = prepared)
        runwise <- FitKernel.fitRunwise(input)
      yield runwise

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[RunwiseFitBlockResult]
    ): Either[FitError, FixedEffectsFmriFitResult] =
      for
        merged <- RunwiseFitBlockResult.merge(chunks)
        runwise = RunwiseFmriFitResult(
          runs = merged.runs,
          columnNames = plan.model.columnNames,
          voxelIndices = merged.voxelIndices,
          timepoints = merged.timepoints,
          engine = FitEngine.RunwiseLeastSquares,
          summary = plan.summary,
          coefficientAxis = merged.coefficientAxis,
          preparationProvenance = merged.preparationProvenance
        )
        fixed <- FixedEffects.combine(runwise)
      yield fixed

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[RunwiseFitBlockResult]] =
      FitBlockCollectors.runwise(chunks)

  private object RobustLeastSquares extends FitInterpreter:
    type Prepared = RobustPrepared
    type Block = DenseFitBlockResult
    type Result = DenseFmriFitResult

    val engine: FitEngine = FitEngine.RobustLeastSquares

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, DenseFmriFitResult] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        dense <- FitKernel.fitDense(input, engine, plan.config)
      yield FitPlanExecutor.denseResult(plan, dense)

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, RobustPrepared] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        prepared <- Robust
          .prepare(input.design, input.response, partitions, series.voxelIndices, plan.config.robust, robustAutocorrelation(plan.config))
          .left
          .map(error => bindRankFailure(input, error))
      yield prepared

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: RobustPrepared
    ): Either[FitError, DenseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = prepared.partitions)
        fit <- Robust.fitPrepared(prepared, input.response, input.voxelIndices)
      yield DenseFitBlockResult.fromRobust(input, fit)

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[DenseFitBlockResult]
    ): Either[FitError, DenseFmriFitResult] =
      DenseFitBlockResult.merge(chunks).map(FitPlanExecutor.denseResult(plan, _))

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[DenseFitBlockResult]] =
      FitBlockCollectors.dense(chunks)

  private object LeastSquaresSeparate extends FitInterpreter:
    type Prepared = LssBlockDesign
    type Block = LssFitBlockResult
    type Result = LssFmriFitResult

    val engine: FitEngine = FitEngine.LeastSquaresSeparate

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, LssFmriFitResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, lssDesign = Some(FitPlanExecutor.lssExecutionDesign(plan, series.timepoints)))
        lss <- FitKernel.fitLss(input)
      yield LssFmriFitResult(
        coefficients = lss.coefficients,
        trialNames = lss.trialNames,
        lssDiagnostics = lss.diagnostics,
        voxelIndices = lss.voxelIndices,
        timepoints = lss.timepoints,
        engine = engine,
        summary = plan.summary,
        coefficientAxis = lss.coefficientAxis,
        preparationProvenance = input.preparationProvenance
      )

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, LssBlockDesign] =
      FitPlanExecutor.lssExecutionDesign(plan, series.timepoints)

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: LssBlockDesign
    ): Either[FitError, LssFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, lssDesign = Some(Right(prepared)))
        lss <- FitKernel.fitLss(input)
      yield lss

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[LssFitBlockResult]
    ): Either[FitError, LssFmriFitResult] =
      LssFitBlockResult.merge(chunks).map { merged =>
        LssFmriFitResult(
          coefficients = merged.coefficients,
          trialNames = merged.trialNames,
          lssDiagnostics = merged.diagnostics,
          voxelIndices = merged.voxelIndices,
          timepoints = merged.timepoints,
          engine = engine,
          summary = plan.summary,
          coefficientAxis = merged.coefficientAxis,
          preparationProvenance = merged.preparationProvenance
        )
      }

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[LssFitBlockResult]] =
      FitBlockCollectors.lss(chunks)

  private object LatentSketch extends FitInterpreter:
    type Prepared = LatentSketchPrepared
    type Block = DenseFitBlockResult
    type Result = DenseFmriFitResult

    val engine: FitEngine = FitEngine.LatentSketch

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, DenseFmriFitResult] =
      for
        prepared <- prepare(plan, series)
        block <- fitChunk(plan, series, prepared)
      yield FitPlanExecutor.denseResult(plan, block)

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, LatentSketchPrepared] =
      for
        sketch <- latentSketchConfig(plan)
        input <- FitPlanExecutor.fitBlockInput(plan, series)
        prepared <- LatentSketchPrepared.prepare(input.design, input.response, series.voxelIndices, sketch)
      yield prepared

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: LatentSketchPrepared
    ): Either[FitError, DenseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series)
        fit <- prepared.fitBlock(input)
      yield fit

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[DenseFitBlockResult]
    ): Either[FitError, DenseFmriFitResult] =
      DenseFitBlockResult.merge(chunks).map(FitPlanExecutor.denseResult(plan, _))

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[DenseFitBlockResult]] =
      FitBlockCollectors.dense(chunks)

  private object ReducedRankGls extends FitInterpreter:
    type Prepared = ReducedRankGlsPrepared
    type Block = DenseFitBlockResult
    type Result = DenseFmriFitResult

    val engine: FitEngine = FitEngine.ReducedRankGls

    def fit(plan: FitPlan, series: FmriSeries): Either[FitError, DenseFmriFitResult] =
      for
        prepared <- prepare(plan, series)
        block <- fitChunk(plan, series, prepared)
      yield FitPlanExecutor.denseResult(plan, block)

    def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, ReducedRankGlsPrepared] =
      val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
      for
        config <- reducedRankGlsConfig(plan)
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
        designPartition <- reducedRankDesignPartition(plan)
        prepared <- ReducedRankGlsPrepared.prepare(input.design, input.response, partitions, config, series.voxelIndices, designPartition)
      yield prepared

    def fitChunk(
        plan: FitPlan,
        series: FmriSeries,
        prepared: ReducedRankGlsPrepared
    ): Either[FitError, DenseFitBlockResult] =
      for
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = prepared.partitions)
        fit <- prepared.fitBlock(input)
      yield fit

    def merge(
        plan: FitPlan,
        chunks: IndexedSeq[DenseFitBlockResult]
    ): Either[FitError, DenseFmriFitResult] =
      DenseFitBlockResult.merge(chunks).map(FitPlanExecutor.denseResult(plan, _))

    protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[DenseFitBlockResult]] =
      FitBlockCollectors.dense(chunks)

private[fit] object FitBlockCollectors:
  def dense(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[DenseFitBlockResult]] =
    val out = Vector.newBuilder[DenseFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case dense: DenseFitBlockResult => out += dense
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected dense fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())

  def lss(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[LssFitBlockResult]] =
    val out = Vector.newBuilder[LssFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case lss: LssFitBlockResult => out += lss
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected LSS fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())

  def runwise(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[RunwiseFitBlockResult]] =
    val out = Vector.newBuilder[RunwiseFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case runwise: RunwiseFitBlockResult => out += runwise
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected runwise fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())
