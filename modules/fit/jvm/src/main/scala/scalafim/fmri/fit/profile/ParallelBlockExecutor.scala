package scalafim.fmri.fit.profile

import java.util.concurrent.{Callable, ExecutionException, Executors, Future, TimeUnit}
import scala.util.control.NonFatal

/** JVM parallel execution in windows of at most `workers` blocks. Each worker
  * owns one task per window; the next window starts only after ordered delivery
  * has released every payload in the current one.
  */
object ParallelBlockExecutor:
  private val DefaultCleanupTimeoutMillis = 60000L

  private final class PoolTermination(pool: java.util.concurrent.ExecutorService) extends ExecutionTermination:
    def isTerminated: Boolean = pool.isTerminated

    def awaitStopped(): Unit =
      var interrupted = false
      var stopped = false
      while !stopped do
        try stopped = pool.awaitTermination(1, TimeUnit.DAYS)
        catch case _: InterruptedException => interrupted = true
      if interrupted then Thread.currentThread().interrupt()

  def run[P, R](
      voxels: Int,
      budget: ExecutionBudget,
      newWorker: () => BlockWorker[P],
      sink: BlockSink[P, R],
      cancelled: () => Boolean = () => false,
      cleanupTimeoutMillis: Long = DefaultCleanupTimeoutMillis
  ): Either[ExecutionError, ExecutionSummary[R]] =
    budget.validate.flatMap { b =>
      if cleanupTimeoutMillis < 1 || cleanupTimeoutMillis > DefaultCleanupTimeoutMillis then
        Left(ExecutionError.InvalidBudget(s"cleanup timeout must be 1..$DefaultCleanupTimeoutMillis ms, got $cleanupTimeoutMillis"))
      else if b.workers == 1 then BlockExecutor.runSequential(voxels, b, newWorker, sink, cancelled)
      else
        val plan = BlockExecutor.blocks(voxels, b.blockSize)
        if plan.isEmpty then Right(ExecutionSummary(Vector.empty[R], 0, voxels, 0))
        else
          val workerCount = math.min(b.workers, plan.length)
          val workers = new Array[BlockWorker[P]](workerCount)
          val receipts = Vector.newBuilder[R]
          val deliveredBlockIds = Vector.newBuilder[Int]
          var delivered = 0
          var failure: Option[ExecutionError] = None
          var interrupted = false
          var unfinished: Option[ExecutionTermination] = None
          val pool = Executors.newFixedThreadPool(workerCount)
          try
            var next = 0
            while next < plan.length && failure.isEmpty do
              val windowSize = math.min(workerCount, plan.length - next)
              val futures = new Array[Future[Either[ExecutionError, P]]](windowSize)
              var submitted = 0
              while submitted < windowSize && failure.isEmpty do
                val slot = submitted
                val block = plan(next + slot)
                try
                  futures(slot) = pool.submit(new Callable[Either[ExecutionError, P]]:
                    def call(): Either[ExecutionError, P] =
                      try
                        if workers(slot) == null then workers(slot) = newWorker()
                        if cancelled() then Left(ExecutionError.Cancelled(0))
                        else Right(workers(slot).process(block))
                      catch case NonFatal(t) => Left(ExecutionError.WorkerFailed(block, t.toString))
                  )
                  submitted += 1
                catch case NonFatal(t) => failure = Some(ExecutionError.WorkerFailed(block, t.toString))

              var offset = 0
              while offset < submitted && failure.isEmpty do
                val block = plan(next + offset)
                val completed =
                  try futures(offset).get()
                  catch
                    case _: InterruptedException =>
                      interrupted = true
                      Left(ExecutionError.Cancelled(delivered))
                    case t: ExecutionException =>
                      Left(ExecutionError.WorkerFailed(block, Option(t.getCause).getOrElse(t).toString))
                    case NonFatal(t) => Left(ExecutionError.WorkerFailed(block, t.toString))
                futures(offset) = null
                completed match
                  case Left(ExecutionError.Cancelled(_)) => failure = Some(ExecutionError.Cancelled(delivered))
                  case Left(err) => failure = Some(err)
                  case Right(payload) =>
                    val stopped =
                      try Right(cancelled())
                      catch case NonFatal(t) => Left(ExecutionError.WorkerFailed(block, t.toString))
                    stopped match
                      case Left(err) => failure = Some(err)
                      case Right(true) => failure = Some(ExecutionError.Cancelled(delivered))
                      case Right(false) =>
                        val accepted =
                          try sink.accept(block, payload)
                          catch case NonFatal(t) => Left(t.toString)
                        accepted match
                          case Left(detail) => failure = Some(ExecutionError.SinkFailed(block, detail))
                          case Right(receipt) =>
                            receipts += receipt
                            deliveredBlockIds += block.index
                            delivered += 1
                offset += 1
              if failure.nonEmpty then
                futures.foreach: f =>
                  if f != null then
                    f.cancel(true)
                    ()
              next += windowSize
          finally
            if failure.nonEmpty then pool.shutdownNow() else pool.shutdown()
            val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(cleanupTimeoutMillis)
            var stopped = pool.isTerminated
            while !stopped && deadline - System.nanoTime() > 0L do
              try stopped = pool.awaitTermination(deadline - System.nanoTime(), TimeUnit.NANOSECONDS)
              catch
                case _: InterruptedException =>
                  interrupted = true
                  if failure.isEmpty then failure = Some(ExecutionError.Cancelled(delivered))
                  pool.shutdownNow()
                  ()
            if !stopped && !pool.isTerminated then
              if failure.isEmpty then
                failure = Some(ExecutionError.WorkerFailed(plan.last, "worker pool did not terminate within cleanup deadline"))
              pool.shutdownNow()
              unfinished = Some(new PoolTermination(pool))
            if interrupted then Thread.currentThread().interrupt()

          unfinished match
            case Some(termination) =>
              val original = failure.getOrElse(ExecutionError.WorkerFailed(plan.last,
                "worker pool did not terminate within cleanup deadline"))
              Left(ExecutionError.WorkersStillRunning(original, deliveredBlockIds.result(), termination))
            case None => failure match
              case Some(err) => Left(err)
              case None => Right(ExecutionSummary(receipts.result(), plan.length, voxels, workerCount))
    }
