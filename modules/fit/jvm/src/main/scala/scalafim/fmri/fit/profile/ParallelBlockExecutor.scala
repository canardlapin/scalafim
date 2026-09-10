package scalafim.fmri.fit.profile

import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

/** JVM parallel execution over a bounded pool: workers pull block indices from
  * a counter, each with its own `BlockWorker`, and a reorder buffer delivers
  * payloads to the sink strictly in block order, so at most `workers`
  * payloads are ever held and receipts are identical to the sequential run.
  */
object ParallelBlockExecutor:

  def run[P, R](
      voxels: Int,
      budget: ExecutionBudget,
      newWorker: () => BlockWorker[P],
      sink: BlockSink[P, R],
      cancelled: () => Boolean = () => false
  ): Either[ExecutionError, ExecutionSummary[R]] =
    budget.validate.flatMap { b =>
      if b.workers == 1 then BlockExecutor.runSequential(voxels, b, newWorker, sink, cancelled)
      else
        val plan = BlockExecutor.blocks(voxels, b.blockSize)
        val next = new AtomicInteger(0)
        val stop = new AtomicBoolean(false)
        val pending = new java.util.TreeMap[Int, P]()
        val lock = new Object
        var delivered = 0
        val receipts = new Array[Any](plan.length)
        var failure: Option[ExecutionError] = None
        val pool = Executors.newFixedThreadPool(b.workers)
        def deliverReady(): Unit =
          // called under lock
          var continue = true
          while continue do
            val head = pending.firstEntry()
            if head == null || head.getKey != delivered then continue = false
            else
              val block = plan(head.getKey)
              pending.pollFirstEntry()
              sink.accept(block, head.getValue) match
                case Left(detail) =>
                  if failure.isEmpty then failure = Some(ExecutionError.SinkFailed(block, detail))
                  stop.set(true)
                  continue = false
                case Right(r) =>
                  receipts(block.index) = r
                  delivered += 1
        val tasks = (0 until b.workers).map { _ =>
          pool.submit(new Runnable {
            def run(): Unit =
              val worker = newWorker()
              var running = true
              while running && !stop.get() do
                val i = next.getAndIncrement()
                if i >= plan.length then running = false
                else if cancelled() then
                  lock.synchronized {
                    if failure.isEmpty then failure = Some(ExecutionError.Cancelled(delivered))
                  }
                  stop.set(true)
                else
                  val block = plan(i)
                  try
                    val payload = worker.process(block)
                    lock.synchronized {
                      pending.put(i, payload)
                      deliverReady()
                    }
                  catch
                    case scala.util.control.NonFatal(t) =>
                      lock.synchronized {
                        if failure.isEmpty then failure = Some(ExecutionError.WorkerFailed(block, t.toString))
                      }
                      stop.set(true)
          })
        }
        tasks.foreach(_.get())
        pool.shutdown()
        pool.awaitTermination(1, TimeUnit.MINUTES)
        failure match
          case Some(err) => Left(err)
          case None =>
            Right(ExecutionSummary(receipts.toVector.map(_.asInstanceOf[R]), plan.length, voxels, b.workers))
    }
