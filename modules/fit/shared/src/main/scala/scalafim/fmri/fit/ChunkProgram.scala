package scalafim.fmri.fit

import scala.concurrent.{ExecutionContext, Future}

trait ChunkWork:
  def ordinal: ChunkOrdinal

final case class CompletedChunk[+A](
    ordinal: ChunkOrdinal,
    result: A
)

final case class ChunkProgram[Work <: ChunkWork] private (work: IndexedSeq[Work]) extends Iterable[Work]:
  require(work.nonEmpty, "chunk program must contain at least one work item")

  override def iterator: Iterator[Work] =
    work.iterator

  override def knownSize: Int =
    work.knownSize

  def length: Int =
    work.length

  def indexed: IndexedSeq[Work] =
    work

object ChunkProgram:
  def make[Work <: ChunkWork](work: IndexedSeq[Work]): Either[FitError, ChunkProgram[Work]] =
    if work.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one chunk work item is required"))
    else
      var i = 0
      while i < work.length do
        val ordinal = work(i).ordinal.value
        if ordinal != i then
          return Left(FitError.IncompatibleFitBlocks(s"chunk work ordinal $ordinal does not match position $i"))
        i += 1
      Right(ChunkProgram(work))

  def unsafe[Work <: ChunkWork](work: IndexedSeq[Work]): ChunkProgram[Work] =
    make(work).fold(error => throw new IllegalArgumentException(error.message), identity)

trait ChunkReducer[-Piece, +Out]:
  def reduce(chunks: IndexedSeq[CompletedChunk[Piece]]): Either[FitError, Out]

object SequentialChunkProgramInterpreter:
  def execute[Work <: ChunkWork, Piece](
      program: ChunkProgram[Work]
  )(
      run: Work => Either[FitError, Piece]
  ): Either[FitError, Vector[CompletedChunk[Piece]]] =
    val out = Vector.newBuilder[CompletedChunk[Piece]]
    val iterator = program.iterator
    while iterator.hasNext do
      val work = iterator.next()
      run(work) match
        case Left(error) =>
          return Left(FitError.ChunkFailed(work.ordinal.value, error))
        case Right(result) =>
          out += CompletedChunk(work.ordinal, result)
    Right(out.result())

object FutureChunkProgramInterpreter:
  def execute[Work <: ChunkWork, Piece](
      program: ChunkProgram[Work],
      parallelism: FitParallelism
  )(
      run: Work => Future[Either[FitError, Piece]]
  )(using ExecutionContext): Future[Either[FitError, Vector[CompletedChunk[Piece]]]] =
    runBounded(program.indexed, parallelism) { work =>
      run(work).map {
        case Left(error) =>
          Left(FitError.ChunkFailed(work.ordinal.value, error))
        case Right(result) =>
          Right(CompletedChunk(work.ordinal, result))
      }
    }.map(collect)

  private def collect[Piece](
      results: Vector[Either[FitError, CompletedChunk[Piece]]]
  ): Either[FitError, Vector[CompletedChunk[Piece]]] =
    var error: FitError | Null = null
    val sorted = Vector.newBuilder[CompletedChunk[Piece]]
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
