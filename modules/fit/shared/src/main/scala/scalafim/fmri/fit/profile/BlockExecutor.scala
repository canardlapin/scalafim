package scalafim.fmri.fit.profile

import scala.util.control.NonFatal

/** A contiguous range of voxel indices processed as one unit. */
final case class VoxelBlock(index: Int, start: Int, count: Int):
  def end: Int = start + count

enum ExecutionError:
  case InvalidBudget(detail: String)
  case WorkerFailed(block: VoxelBlock, detail: String)
  case SinkFailed(block: VoxelBlock, detail: String)
  case Cancelled(completedBlocks: Int)
  case WorkersStillRunning(original: ExecutionError, deliveredBlockIds: Vector[Int], termination: ExecutionTermination)

  def message: String =
    this match
      case InvalidBudget(detail) => s"invalid execution budget: $detail"
      case WorkerFailed(block, detail) => s"worker failed on block ${block.index} (voxels ${block.start} until ${block.end}): $detail"
      case SinkFailed(block, detail) => s"sink refused block ${block.index}: $detail"
      case Cancelled(completed) => s"execution cancelled after $completed blocks"
      case WorkersStillRunning(original, _, _) => s"${original.message}; owned workers are still running"

/** An owned worker pool that may outlive a nonfinal execution result. Readers
  * used by that pool must stay available until this handle confirms termination.
  * Waiting tolerates interruption and restores the caller's interrupt flag.
  */
trait ExecutionTermination:
  def isTerminated: Boolean
  def awaitStopped(): Unit

/** Per-worker state owned by the backend; one instance per worker, never shared. */
trait BlockWorker[P]:
  def process(block: VoxelBlock): P

/** Where completed payloads go. Blocks are delivered in index order regardless
  * of completion order; the sink returns a small receipt and the payload is
  * released. A sink may refuse.
  */
trait BlockSink[P, R]:
  def accept(block: VoxelBlock, payload: P): Either[String, R]

final case class ExecutionBudget(blockSize: Int = 256, workers: Int = 1):
  def validate: Either[ExecutionError, ExecutionBudget] =
    if blockSize < 1 || blockSize > ExecutionBudget.MaxBlockSize then Left(ExecutionError.InvalidBudget(s"blockSize must be 1..${ExecutionBudget.MaxBlockSize}, got $blockSize"))
    else if workers < 1 || workers > ExecutionBudget.MaxWorkers then Left(ExecutionError.InvalidBudget(s"workers must be 1..${ExecutionBudget.MaxWorkers}, got $workers"))
    else Right(this)

object ExecutionBudget:
  val MaxBlockSize: Int = 256
  val MaxWorkers: Int = 8

/** Small receipts only: the executor never retains payloads. */
final case class ExecutionSummary[R](receipts: Vector[R], blocks: Int, voxels: Int, workersUsed: Int)

object BlockExecutor:

  def blocks(voxels: Int, blockSize: Int): Vector[VoxelBlock] =
    Vector.tabulate((voxels + blockSize - 1) / blockSize) { i =>
      val start = i * blockSize
      VoxelBlock(i, start, math.min(blockSize, voxels - start))
    }

  /** Sequential execution: the reference semantics on every platform. */
  def runSequential[P, R](
      voxels: Int,
      budget: ExecutionBudget,
      newWorker: () => BlockWorker[P],
      sink: BlockSink[P, R],
      cancelled: () => Boolean = () => false
  ): Either[ExecutionError, ExecutionSummary[R]] =
    budget.validate.flatMap { b =>
      val plan = blocks(voxels, b.blockSize)
      val receipts = Vector.newBuilder[R]
      var failure: Option[ExecutionError] = None
      var i = 0
      val worker =
        if plan.isEmpty then None
        else
          try Some(newWorker())
          catch
            case NonFatal(t) =>
              failure = Some(ExecutionError.WorkerFailed(plan.head, t.toString))
              None
      while i < plan.length && failure.isEmpty do
        if cancelled() then failure = Some(ExecutionError.Cancelled(i))
        else
          val block = plan(i)
          val payload =
            try Right(worker.get.process(block))
            catch case NonFatal(t) => Left(ExecutionError.WorkerFailed(block, t.toString))
          payload match
            case Left(err) => failure = Some(err)
            case Right(p) =>
              val accepted =
                try sink.accept(block, p)
                catch case NonFatal(t) => Left(t.toString)
              accepted match
                case Left(detail) => failure = Some(ExecutionError.SinkFailed(block, detail))
                case Right(r) => receipts += r
        i += 1
      failure match
        case Some(err) => Left(err)
        case None => Right(ExecutionSummary(receipts.result(), plan.length, voxels, 1))
    }
