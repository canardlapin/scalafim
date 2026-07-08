package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, FmriSeries, IndexSelection, ResolvedDataSelection}
import scalafim.fmri.model.{FitEngine, FitPlan}

import scala.concurrent.{ExecutionContext, Future}

opaque type ChunkOrdinal = Int

object ChunkOrdinal:
  def apply(value: Int): Either[FitError, ChunkOrdinal] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("chunk ordinal", s"value $value must be non-negative"))

  def unsafe(value: Int): ChunkOrdinal =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (ordinal: ChunkOrdinal)
    inline def value: Int = ordinal

opaque type ChunkSize = Int

object ChunkSize:
  def apply(value: Int): Either[FitError, ChunkSize] =
    if value > 0 then Right(value)
    else Left(FitError.InvalidFitAxis("chunk size", s"value $value must be positive"))

  def unsafe(value: Int): ChunkSize =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (size: ChunkSize)
    inline def value: Int = size

enum FitChunkingStrategy:
  case WholeSelection
  case ByVoxelCount(size: ChunkSize)

object FitChunkingStrategy:
  def byVoxelCount(size: Int): Either[FitError, FitChunkingStrategy] =
    ChunkSize(size).map(ByVoxelCount.apply)

  def unsafeByVoxelCount(size: Int): FitChunkingStrategy =
    byVoxelCount(size).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FitParallelism private (maxConcurrency: Int):
  require(maxConcurrency > 0, "maxConcurrency must be positive")

object FitParallelism:
  val sequential: FitParallelism =
    FitParallelism(1)

  val unbounded: FitParallelism =
    FitParallelism(Int.MaxValue)

  def bounded(maxConcurrency: Int): Either[FitError, FitParallelism] =
    if maxConcurrency > 0 then Right(FitParallelism(maxConcurrency))
    else Left(FitError.InvalidFitAxis("fit parallelism", s"maxConcurrency must be positive, got $maxConcurrency"))

  def unsafe(maxConcurrency: Int): FitParallelism =
    bounded(maxConcurrency).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FitChunkSpec private (
    ordinal: ChunkOrdinal,
    timepoints: Vector[Int],
    voxelIndices: Vector[Int]
):
  require(timepoints.nonEmpty, "fit chunk must contain at least one timepoint")
  require(voxelIndices.nonEmpty, "fit chunk must contain at least one voxel")

  def selection: DataSelection =
    DataSelection(
      time = IndexSelection.Indices(timepoints),
      voxels = IndexSelection.Indices(voxelIndices)
    )

object FitChunkSpec:
  def make(
      ordinal: ChunkOrdinal,
      timepoints: Vector[Int],
      voxelIndices: Vector[Int]
  ): Either[FitError, FitChunkSpec] =
    if timepoints.isEmpty then Left(FitError.InvalidFitAxis("fit chunk timepoints", "must be non-empty"))
    else if voxelIndices.isEmpty then Left(FitError.InvalidFitAxis("fit chunk voxels", "must be non-empty"))
    else Right(new FitChunkSpec(ordinal, timepoints, voxelIndices))

  def unsafe(
      ordinal: ChunkOrdinal,
      timepoints: Vector[Int],
      voxelIndices: Vector[Int]
  ): FitChunkSpec =
    make(ordinal, timepoints, voxelIndices).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FitChunkPlan private (chunks: IndexedSeq[FitChunkSpec]) extends Iterable[FitChunkSpec]:
  require(chunks.nonEmpty, "fit chunk plan must contain at least one chunk")

  override def iterator: Iterator[FitChunkSpec] =
    chunks.iterator

  override def knownSize: Int =
    chunks.knownSize

  def length: Int =
    chunks.length

  def indexed: IndexedSeq[FitChunkSpec] =
    chunks

  def timepoints: Vector[Int] =
    chunks.head.timepoints

  def voxelIndices: Vector[Int] =
    chunks.iterator.flatMap(_.voxelIndices).toVector

  def selection: DataSelection =
    DataSelection(
      time = IndexSelection.Indices(timepoints),
      voxels = IndexSelection.Indices(voxelIndices)
    )

object FitChunkPlan:
  def fromSelection(
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FitChunkPlan] =
    val resolved =
      selection
        .resolveEither(plan.model.dataset.shape, plan.model.dataset.backend.voxelDomain)
        .left
        .map(mapDatasetError)
    resolved.flatMap(fromResolvedSelection(_, chunking))

  def fromResolvedSelection(
      selection: ResolvedDataSelection,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FitChunkPlan] =
    val timepoints = selection.timepoints
    val voxels = selection.voxels
    val grouped =
      chunking match
        case FitChunkingStrategy.WholeSelection =>
          Vector(voxels)
        case FitChunkingStrategy.ByVoxelCount(size) =>
          voxels.grouped(size.value).toVector

    val chunks =
      grouped.zipWithIndex.map { case (chunkVoxels, ordinal) =>
        FitChunkSpec.unsafe(
          ordinal = ChunkOrdinal.unsafe(ordinal),
          timepoints = timepoints,
          voxelIndices = chunkVoxels
        )
      }
    make(chunks)

  def make(chunks: IndexedSeq[FitChunkSpec]): Either[FitError, FitChunkPlan] =
    if chunks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one fit chunk is required"))
    else
      val timepoints = chunks.head.timepoints
      var i = 0
      while i < chunks.length do
        if chunks(i).ordinal.value != i then
          return Left(FitError.IncompatibleFitBlocks(s"chunk ordinal ${chunks(i).ordinal.value} does not match position $i"))
        if chunks(i).timepoints != timepoints then
          return Left(FitError.IncompatibleFitBlocks("all fit chunks must share the same selected timepoints"))
        i += 1
      Right(FitChunkPlan(chunks))

  private[fit] def mapDatasetError(error: DatasetError): FitError =
    FitError.InvalidFitAxis("data selection", error.message)

private[fit] enum PreparedFitContext:
  case OrdinaryLeastSquares(prepared: OlsPrepared)
  case GeneralizedLeastSquares(prepared: GlsPrepared)
  case Runwise(partitions: Vector[RunPartition])
  case Lss(design: LssBlockDesign)

object PreparedFitContext:
  def prepare(
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, PreparedFitContext] =
    plan.model.dataset.seriesEither(chunkPlan.selection)
      .left
      .map(FitChunkPlan.mapDatasetError)
      .flatMap(series => prepareSeries(plan, series))

  private def prepareSeries(plan: FitPlan, series: FmriSeries): Either[FitError, PreparedFitContext] =
    plan.engine match
      case FitEngine.OrdinaryLeastSquares =>
        for
          input <- FitPlanExecutor.fitBlockInput(plan, series)
          prepared <- Ols.prepare(input.design)
        yield PreparedFitContext.OrdinaryLeastSquares(prepared)

      case FitEngine.GeneralizedLeastSquares =>
        val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
        for
          input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
          prepared <- Gls.prepare(input.design, input.response, partitions, plan.config.autocorrelation)
        yield PreparedFitContext.GeneralizedLeastSquares(prepared)

      case FitEngine.RunwiseLeastSquares =>
        Right(PreparedFitContext.Runwise(RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)))

      case FitEngine.LeastSquaresSeparate =>
        FitPlanExecutor
          .lssExecutionDesign(plan, series.timepoints)
          .map(PreparedFitContext.Lss.apply)

      case other =>
        Left(FitError.UnsupportedEngine(other.toString))

private[fit] final case class CompletedFitChunk(
    ordinal: ChunkOrdinal,
    result: FitBlockResult
)

object ChunkedFitExecutor:
  def fit(
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FmriFitResult] =
    for
      chunkPlan <- FitChunkPlan.fromSelection(plan, selection, chunking)
      context <- PreparedFitContext.prepare(plan, chunkPlan)
      chunks <- fitChunks(plan, chunkPlan, context)
      result <- mergeChunks(plan, chunks)
    yield result

  def fitChunks(
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, Vector[FitBlockResult]] =
    PreparedFitContext.prepare(plan, chunkPlan).flatMap(fitChunks(plan, chunkPlan, _))

  private[fit] def fitChunks(
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      context: PreparedFitContext
  ): Either[FitError, Vector[FitBlockResult]] =
    val out = Vector.newBuilder[FitBlockResult]
    val iterator = chunkPlan.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      fitChunk(plan, chunk, context) match
        case Left(error) =>
          return Left(error)
        case Right(result) =>
          out += result
    Right(out.result())

  def fitChunk(
      plan: FitPlan,
      chunk: FitChunkSpec
  ): Either[FitError, FitBlockResult] =
    val normalized =
      FitChunkSpec.unsafe(
        ordinal = ChunkOrdinal.unsafe(0),
        timepoints = chunk.timepoints,
        voxelIndices = chunk.voxelIndices
      )
    FitChunkPlan
      .make(Vector(normalized))
      .flatMap(PreparedFitContext.prepare(plan, _))
      .flatMap(fitChunk(plan, chunk, _))

  private[fit] def fitChunk(
      plan: FitPlan,
      chunk: FitChunkSpec,
      context: PreparedFitContext
  ): Either[FitError, FitBlockResult] =
    plan.model.dataset.seriesEither(chunk.selection)
      .left
      .map(FitChunkPlan.mapDatasetError)
      .flatMap(series => fitSeriesChunk(plan, series, context))

  private def fitSeriesChunk(
      plan: FitPlan,
      series: FmriSeries,
      context: PreparedFitContext
  ): Either[FitError, FitBlockResult] =
    context match
      case PreparedFitContext.OrdinaryLeastSquares(prepared) =>
        for
          response <- MatrixAdapters.responseBlock(series)
          input = FitBlockInput(
            design = prepared.design,
            response = response,
            voxelIndices = series.voxelIndices,
            timepoints = series.timepoints
          )
          fit <- prepared.fit(response)
        yield DenseFitBlockResult.fromOls(input, fit, plan.engine)

      case PreparedFitContext.GeneralizedLeastSquares(prepared) =>
        for
          response <- MatrixAdapters.responseBlock(series)
          input = FitBlockInput(
            design = prepared.design,
            response = response,
            voxelIndices = series.voxelIndices,
            timepoints = series.timepoints,
            partitions = prepared.partitions
          )
          fit <- prepared.fit(response)
        yield DenseFitBlockResult.fromGls(input, fit)

      case PreparedFitContext.Runwise(partitions) =>
        for
          input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
          runwise <- FitKernel.fitRunwise(input)
        yield runwise

      case PreparedFitContext.Lss(lssDesign) =>
        for
          input <- FitPlanExecutor.fitBlockInput(plan, series, lssDesign = Some(Right(lssDesign)))
          lss <- FitKernel.fitLss(input)
        yield lss

  private[fit] def mergeChunks(
      plan: FitPlan,
      chunks: IndexedSeq[FitBlockResult]
  ): Either[FitError, FmriFitResult] =
    if chunks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one fit chunk result is required"))
    else
      chunks.head match
        case _: DenseFitBlockResult =>
          collectDense(chunks).flatMap { dense =>
            DenseFitBlockResult.merge(dense).map(FitPlanExecutor.denseResult(plan, _))
          }
        case _: LssFitBlockResult =>
          collectLss(chunks).flatMap { lss =>
            LssFitBlockResult.merge(lss).map { merged =>
              LssFmriFitResult(
                coefficients = merged.coefficients,
                trialNames = merged.trialNames,
                lssDiagnostics = merged.diagnostics,
                voxelIndices = merged.voxelIndices,
                timepoints = merged.timepoints,
                engine = plan.engine,
                summary = plan.summary
              )
            }
          }
        case _: RunwiseFitBlockResult =>
          collectRunwise(chunks).flatMap { runwise =>
            RunwiseFitBlockResult.merge(runwise).map { merged =>
              RunwiseFmriFitResult(
                runs = merged.runs,
                columnNames = plan.model.columnNames,
                voxelIndices = merged.voxelIndices,
                timepoints = merged.timepoints,
                engine = plan.engine,
                summary = plan.summary
              )
            }
          }

  private def collectDense(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[DenseFitBlockResult]] =
    val out = Vector.newBuilder[DenseFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case dense: DenseFitBlockResult => out += dense
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected dense fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())

  private def collectLss(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[LssFitBlockResult]] =
    val out = Vector.newBuilder[LssFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case lss: LssFitBlockResult => out += lss
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected LSS fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())

  private def collectRunwise(chunks: IndexedSeq[FitBlockResult]): Either[FitError, Vector[RunwiseFitBlockResult]] =
    val out = Vector.newBuilder[RunwiseFitBlockResult]
    var i = 0
    while i < chunks.length do
      chunks(i) match
        case runwise: RunwiseFitBlockResult => out += runwise
        case other => return Left(FitError.IncompatibleFitBlocks(s"expected runwise fit chunk but got ${other.engine}"))
      i += 1
    Right(out.result())

object FutureChunkedFitExecutor:
  def fit(
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection,
      parallelism: FitParallelism = FitParallelism.unbounded
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    FitChunkPlan.fromSelection(plan, selection, chunking) match
      case Left(error) =>
        Future.successful(Left(error))
      case Right(chunkPlan) =>
        PreparedFitContext.prepare(plan, chunkPlan) match
          case Left(error) =>
            Future.successful(Left(error))
          case Right(context) =>
            fitChunks(plan, chunkPlan, context, parallelism).map {
              case Left(error) =>
                Left(error)
              case Right(chunks) =>
                ChunkedFitExecutor.mergeChunks(plan, chunks.map(_.result))
            }

  private[fit] def fitChunks(
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      parallelism: FitParallelism = FitParallelism.unbounded
  )(using ExecutionContext): Future[Either[FitError, Vector[CompletedFitChunk]]] =
    PreparedFitContext.prepare(plan, chunkPlan) match
      case Left(error) =>
        Future.successful(Left(error))
      case Right(context) =>
        fitChunks(plan, chunkPlan, context, parallelism)

  private[fit] def fitChunks(
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      context: PreparedFitContext,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, Vector[CompletedFitChunk]]] =
    runBounded(chunkPlan.indexed, parallelism) { chunk =>
      Future {
        ChunkedFitExecutor.fitChunk(plan, chunk, context).map { result =>
          CompletedFitChunk(chunk.ordinal, result)
        }
      }
    }.map { results =>
      var error: FitError | Null = null
      val sorted = Vector.newBuilder[CompletedFitChunk]
      var i = 0
      while i < results.length && error == null do
        results(i) match
          case Left(err) =>
            error = err
          case Right(result) =>
            sorted += result
        i += 1
      error match
        case null => Right(sorted.result().sortBy(_.ordinal.value))
        case err  => Left(err)
    }

  private def runBounded[A, B](
      values: IndexedSeq[A],
      parallelism: FitParallelism
  )(
      f: A => Future[B]
  )(using ExecutionContext): Future[Vector[B]] =
    if values.isEmpty then Future.successful(Vector.empty)
    else
      val chunkSize = math.min(parallelism.maxConcurrency, values.length)
      values.grouped(chunkSize).foldLeft(Future.successful(Vector.empty[B])) { (acc, chunk) =>
        acc.flatMap { collected =>
          Future.sequence(chunk.map(f)).map(results => collected ++ results)
        }
      }
