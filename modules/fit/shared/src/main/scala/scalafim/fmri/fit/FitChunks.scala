package scalafim.fmri.fit

import scalafim.dataset.{
  DataSelection,
  DatasetError,
  DatasetSeriesReader,
  IndexSelection,
  ResolvedDataSelection,
  SynchronousFmriDataset
}
import scalafim.fmri.model.{FitEngine, FitPlan, MissingDataPolicy}

import scala.concurrent.{ExecutionContext, Future}

private[fit] def legacyDatasetReader(
    plan: FitPlan
): Either[FitError, SynchronousFmriDataset] =
  SynchronousFmriDataset
    .readerFor(plan.model.dataset)
    .left
    .map(error => FitError.InvalidFitAxis("dataset reader", error.message))

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
        .resolveEither(
          plan.model.dataset.shape,
          plan.model.dataset.voxelDomain
        )
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

private[fit] object PreparedFitContexts:
  /** Synchronous compatibility overload. */
  def prepare(
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, PreparedFitContext] =
    legacyDatasetReader(plan).flatMap(prepare(_, plan, chunkPlan))

  def prepare(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, PreparedFitContext] =
    for
      _ <-
        if reader.dataset.id == plan.model.dataset.id &&
            reader.dataset.shape == plan.model.dataset.shape
        then Right(())
        else
          Left(FitError.InvalidFitAxis(
            "dataset reader",
            s"reader dataset '${reader.dataset.id.value}' does not match model dataset '${plan.model.dataset.id.value}'"
          ))
      interpreter <- FitInterpreters.forPlan(plan)
      series <- reader
        .seriesEither(chunkPlan.selection)
        .left
        .map(FitChunkPlan.mapDatasetError)
      context <- interpreter.prepareContext(plan, series)
    yield context

private[fit] type CompletedFitChunk = CompletedChunk[FitBlockResult]

private[fit] final case class FitChunkWork private (chunk: FitChunkSpec) extends ChunkWork:
  def ordinal: ChunkOrdinal =
    chunk.ordinal

  def selection: DataSelection =
    chunk.selection

object FitChunkWork:
  def fromChunk(chunk: FitChunkSpec): FitChunkWork =
    FitChunkWork(chunk)

private[fit] final case class FitChunkProgram private (
    reader: DatasetSeriesReader,
    plan: FitPlan,
    chunkPlan: FitChunkPlan,
    context: PreparedFitContext,
    private[fit] val workProgram: ChunkProgram[FitChunkWork]
) extends Iterable[FitChunkWork]:
  override def iterator: Iterator[FitChunkWork] =
    workProgram.iterator

  override def knownSize: Int =
    workProgram.knownSize

  def indexed: IndexedSeq[FitChunkWork] =
    workProgram.indexed

  def chunkCount: Int =
    workProgram.length

  def selection: DataSelection =
    chunkPlan.selection

  def engine: FitEngine =
    context.engine

object FitChunkProgram:
  /** Synchronous compatibility overload. */
  def fromSelection(
      plan: FitPlan
  ): Either[FitError, FitChunkProgram] =
    fromSelection(
      plan,
      DataSelection.All,
      FitChunkingStrategy.WholeSelection
    )

  /** Synchronous compatibility overload. */
  def fromSelection(
      plan: FitPlan,
      selection: DataSelection
  ): Either[FitError, FitChunkProgram] =
    fromSelection(plan, selection, FitChunkingStrategy.WholeSelection)

  /** Synchronous compatibility overload. */
  def fromSelection(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  ): Either[FitError, FitChunkProgram] =
    legacyDatasetReader(plan)
      .flatMap(fromSelection(_, plan, selection, chunking))

  def fromSelection(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FitChunkProgram] =
    FitChunkPlan
      .fromSelection(plan, selection, chunking)
      .flatMap(fromChunkPlan(reader, plan, _))

  def fromChunkPlan(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, FitChunkProgram] =
    for
      work <- ChunkProgram.make(chunkPlan.indexed.map(FitChunkWork.fromChunk))
      context <- PreparedFitContexts.prepare(reader, plan, chunkPlan)
    yield FitChunkProgram(reader, plan, chunkPlan, context, work)

  /** Synchronous compatibility overload. */
  def fromChunkPlan(
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, FitChunkProgram] =
    legacyDatasetReader(plan).flatMap(fromChunkPlan(_, plan, chunkPlan))

  private[fit] def fromPrepared(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      context: PreparedFitContext
  ): FitChunkProgram =
    FitChunkProgram(
      reader = reader,
      plan = plan,
      chunkPlan = chunkPlan,
      context = context,
      workProgram = ChunkProgram.unsafe(chunkPlan.indexed.map(FitChunkWork.fromChunk))
    )

private[fit] final case class FitChunkReducer(plan: FitPlan) extends ChunkReducer[FitBlockResult, FmriFitResult]:
  def reduce(chunks: IndexedSeq[CompletedChunk[FitBlockResult]]): Either[FitError, FmriFitResult] =
    if chunks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one completed fit chunk is required"))
    else ChunkedFitExecutor.mergeChunks(plan, chunks.sortBy(_.ordinal.value).map(_.result))

private[fit] trait FitChunkInterpreter[F[_]]:
  def execute(program: FitChunkProgram): F[Either[FitError, Vector[CompletedFitChunk]]]

private[fit] object SequentialFitChunkInterpreter extends FitChunkInterpreter[[A] =>> A]:
  def execute(program: FitChunkProgram): Either[FitError, Vector[CompletedFitChunk]] =
    SequentialChunkProgramInterpreter.execute(program.workProgram) { work =>
      ChunkedFitExecutor.fitChunk(
        program.reader,
        work.chunk,
        program.context
      )
    }

private[fit] final case class FutureFitChunkInterpreter(
    parallelism: FitParallelism = FitParallelism.unbounded
)(
    using executionContext: ExecutionContext
) extends FitChunkInterpreter[Future]:
  def execute(program: FitChunkProgram): Future[Either[FitError, Vector[CompletedFitChunk]]] =
    FutureChunkProgramInterpreter.execute(program.workProgram, parallelism) { work =>
      Future {
        ChunkedFitExecutor.fitChunk(
          program.reader,
          work.chunk,
          program.context
        )
      }
    }

object ChunkedFitExecutor:
  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan
  ): Either[FitError, FmriFitResult] =
    fit(plan, DataSelection.All, FitChunkingStrategy.WholeSelection)

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection
  ): Either[FitError, FmriFitResult] =
    fit(plan, selection, FitChunkingStrategy.WholeSelection)

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  ): Either[FitError, FmriFitResult] =
    legacyDatasetReader(plan).flatMap(fit(_, plan, selection, chunking))

  def fit(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FmriFitResult] =
    plan.config.missingData match
      case MissingDataPolicy.OmitRowsPerVoxel =>
        MaskedResponseExecutor.fitChunked(reader, plan, selection, chunking)
      case _ =>
        for
          program <- FitChunkProgram.fromSelection(reader, plan, selection, chunking)
          chunks <- SequentialFitChunkInterpreter.execute(program)
          result <- mergeCompletedChunks(program.plan, chunks)
        yield result

  def fitChunks(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, Vector[FitBlockResult]] =
    FitChunkProgram
      .fromChunkPlan(reader, plan, chunkPlan)
      .flatMap(fitChunks)

  /** Synchronous compatibility overload. */
  def fitChunks(
      plan: FitPlan,
      chunkPlan: FitChunkPlan
  ): Either[FitError, Vector[FitBlockResult]] =
    legacyDatasetReader(plan).flatMap(fitChunks(_, plan, chunkPlan))

  private[fit] def fitChunks(program: FitChunkProgram): Either[FitError, Vector[FitBlockResult]] =
    SequentialFitChunkInterpreter
      .execute(program)
      .map(_.map(_.result))

  private[fit] def fitChunks(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      context: PreparedFitContext
  ): Either[FitError, Vector[FitBlockResult]] =
    fitChunks(FitChunkProgram.fromPrepared(reader, plan, chunkPlan, context))

  def fitChunk(
      reader: DatasetSeriesReader,
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
      .flatMap(PreparedFitContexts.prepare(reader, plan, _))
      .flatMap(fitChunk(reader, chunk, _))

  /** Synchronous compatibility overload. */
  def fitChunk(
      plan: FitPlan,
      chunk: FitChunkSpec
  ): Either[FitError, FitBlockResult] =
    legacyDatasetReader(plan).flatMap(fitChunk(_, plan, chunk))

  private[fit] def fitChunk(
      reader: DatasetSeriesReader,
      chunk: FitChunkSpec,
      context: PreparedFitContext
  ): Either[FitError, FitBlockResult] =
    reader.seriesEither(chunk.selection)
      .left
      .map(FitChunkPlan.mapDatasetError)
      .flatMap(context.fitChunk)

  private[fit] def mergeChunks(
      plan: FitPlan,
      chunks: IndexedSeq[FitBlockResult]
  ): Either[FitError, FmriFitResult] =
    if chunks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one fit chunk result is required"))
    else
      FitInterpreters
        .forPlan(plan)
        .flatMap(_.mergeAny(plan, chunks))

  private[fit] def mergeCompletedChunks(
      plan: FitPlan,
      chunks: IndexedSeq[CompletedFitChunk]
  ): Either[FitError, FmriFitResult] =
    FitChunkReducer(plan).reduce(chunks)

object FutureChunkedFitExecutor:
  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fit(
      plan,
      DataSelection.All,
      FitChunkingStrategy.WholeSelection,
      FitParallelism.unbounded
    )

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fit(
      plan,
      selection,
      FitChunkingStrategy.WholeSelection,
      FitParallelism.unbounded
    )

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      chunking: FitChunkingStrategy,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fit(plan, DataSelection.All, chunking, parallelism)

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fit(plan, selection, chunking, FitParallelism.unbounded)

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    legacyDatasetReader(plan) match
      case Left(error) =>
        Future.successful(Left(error))
      case Right(reader) =>
        fit(reader, plan, selection, chunking, parallelism)

  def fit(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection,
      parallelism: FitParallelism = FitParallelism.unbounded
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    plan.config.missingData match
      case MissingDataPolicy.OmitRowsPerVoxel =>
        MaskedResponseExecutor.fitChunkedFuture(reader, plan, selection, chunking, parallelism)
      case _ =>
        FitChunkProgram.fromSelection(reader, plan, selection, chunking) match
          case Left(error) =>
            Future.successful(Left(error))
          case Right(program) =>
            FutureFitChunkInterpreter(parallelism).execute(program).map {
              case Left(error) =>
                Left(error)
              case Right(chunks) =>
                ChunkedFitExecutor.mergeCompletedChunks(program.plan, chunks)
            }

  private[fit] def fitChunks(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      parallelism: FitParallelism = FitParallelism.unbounded
  )(using ExecutionContext): Future[Either[FitError, Vector[CompletedFitChunk]]] =
    FitChunkProgram.fromChunkPlan(reader, plan, chunkPlan) match
      case Left(error)    => Future.successful(Left(error))
      case Right(program) => FutureFitChunkInterpreter(parallelism).execute(program)

  private[fit] def fitChunks(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunkPlan: FitChunkPlan,
      context: PreparedFitContext,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, Vector[CompletedFitChunk]]] =
    FutureFitChunkInterpreter(parallelism)
      .execute(FitChunkProgram.fromPrepared(reader, plan, chunkPlan, context))
