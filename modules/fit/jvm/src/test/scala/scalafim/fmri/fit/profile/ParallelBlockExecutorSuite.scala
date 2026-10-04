package scalafim.fmri.fit.profile

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class ParallelBlockExecutorSuite extends munit.FunSuite:

  private final class SlowWorker extends BlockWorker[Array[Double]]:
    def process(block: VoxelBlock): Array[Double] =
      // reverse the timing so later blocks tend to finish first
      Thread.sleep(if block.index % 3 == 0 then 6 else 1)
      Array.tabulate(block.count)(i => (block.start + i).toDouble)

  private final class OrderSink extends BlockSink[Array[Double], (Int, Double)]:
    val order = Vector.newBuilder[Int]
    def accept(block: VoxelBlock, payload: Array[Double]): Either[String, (Int, Double)] =
      order += block.index
      Right((block.index, payload.sum))

  test("parallel receipts equal the sequential run and arrive in block order"):
    val voxels = 3000
    val sequential = BlockExecutor.runSequential(voxels, ExecutionBudget(128, 1), () => new SlowWorker, new OrderSink).fold(e => fail(e.message), identity)
    for workers <- Seq(2, 4, 8) do
      val sink = new OrderSink
      val parallel = ParallelBlockExecutor.run(voxels, ExecutionBudget(128, workers), () => new SlowWorker, sink).fold(e => fail(e.message), identity)
      assertEquals(parallel.receipts, sequential.receipts, s"workers=$workers")
      assertEquals(sink.order.result(), (0 until sequential.blocks).toVector, s"delivery order with workers=$workers")
      assertEquals(parallel.workersUsed, workers)

  test("a refusing sink stops the parallel run with a typed error"):
    val refusing = new BlockSink[Array[Double], Int]:
      def accept(block: VoxelBlock, payload: Array[Double]): Either[String, Int] = if block.index == 2 then Left("no") else Right(block.index)
    ParallelBlockExecutor.run(2000, ExecutionBudget(100, 4), () => new SlowWorker, refusing) match
      case Left(ExecutionError.SinkFailed(block, _)) => assertEquals(block.index, 2)
      case other => fail(s"expected SinkFailed, got $other")

  test("a held first block prevents submission beyond the initial worker window"):
    val started = new AtomicInteger(0)
    val firstStarted = new CountDownLatch(1)
    val laterFinished = new CountDownLatch(2)
    val releaseFirst = new CountDownLatch(1)
    val overflowStarted = new CountDownLatch(1)
    val result = new AtomicReference[Either[ExecutionError, ExecutionSummary[Int]]]()
    val worker = new BlockWorker[Int]:
      def process(block: VoxelBlock): Int =
        started.incrementAndGet()
        if block.index == 0 then
          firstStarted.countDown()
          if !releaseFirst.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("first block was not released")
        else if block.index < 3 then laterFinished.countDown()
        else overflowStarted.countDown()
        block.index
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    val runner = new Thread(() => result.set(ParallelBlockExecutor.run(12, ExecutionBudget(1, 3), () => worker, sink)))
    runner.start()
    try
      assert(firstStarted.await(5, TimeUnit.SECONDS))
      assert(laterFinished.await(5, TimeUnit.SECONDS))
      assert(!overflowStarted.await(250, TimeUnit.MILLISECONDS))
      assertEquals(started.get(), 3)
    finally
      releaseFirst.countDown()
      runner.join(5000)
    assert(!runner.isAlive)
    assertEquals(result.get().map(_.receipts), Right((0 until 12).toVector))

  test("factory and thrown sink failures are typed and owned pool threads exit"):
    val seenThreads = new ConcurrentLinkedQueue[Thread]()
    val factory = () =>
      seenThreads.add(Thread.currentThread())
      throw new IllegalStateException("factory boom")
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    ParallelBlockExecutor.run[Int, Int](10, ExecutionBudget(1, 3), factory, sink) match
      case Left(ExecutionError.WorkerFailed(_, detail)) => assert(detail.contains("factory boom"))
      case other => fail(s"expected WorkerFailed, got $other")
    assert(seenThreads.asScala.forall(t => !t.isAlive))

    val throwing = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = throw new IllegalStateException("sink boom")
    ParallelBlockExecutor.run(10, ExecutionBudget(1, 3), () => new BlockWorker[Int]:
      def process(block: VoxelBlock): Int = block.index
    , throwing) match
      case Left(ExecutionError.SinkFailed(block, detail)) =>
        assertEquals(block.index, 0)
        assert(detail.contains("sink boom"))
      case other => fail(s"expected SinkFailed, got $other")

  test("cancellation returns a typed error and terminates owned pool threads"):
    val stop = new AtomicBoolean(false)
    val seenThreads = new ConcurrentLinkedQueue[Thread]()
    val worker = new BlockWorker[Int]:
      def process(block: VoxelBlock): Int =
        seenThreads.add(Thread.currentThread())
        if block.index == 0 then stop.set(true)
        block.index
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    ParallelBlockExecutor.run(10, ExecutionBudget(1, 3), () => worker, sink, () => stop.get()) match
      case Left(ExecutionError.Cancelled(completed)) => assertEquals(completed, 0)
      case other => fail(s"expected Cancelled, got $other")
    assert(seenThreads.asScala.forall(t => !t.isAlive))

  test("worker failure interrupts another in-flight task and releases owned threads"):
    val bothStarted = new CountDownLatch(2)
    val otherInterrupted = new CountDownLatch(1)
    val seenThreads = new ConcurrentLinkedQueue[Thread]()
    val worker = new BlockWorker[Int]:
      def process(block: VoxelBlock): Int =
        seenThreads.add(Thread.currentThread())
        bothStarted.countDown()
        if !bothStarted.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("workers did not start")
        if block.index == 0 then throw new IllegalStateException("worker boom")
        try
          if !new CountDownLatch(1).await(5, TimeUnit.SECONDS) then throw new IllegalStateException("task was not interrupted")
        catch
          case _: InterruptedException => otherInterrupted.countDown()
        block.index
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    ParallelBlockExecutor.run(2, ExecutionBudget(1, 2), () => worker, sink) match
      case Left(ExecutionError.WorkerFailed(block, detail)) =>
        assertEquals(block.index, 0)
        assert(detail.contains("worker boom"))
      case other => fail(s"expected WorkerFailed, got $other")
    assertEquals(otherInterrupted.getCount, 0L)
    assert(seenThreads.asScala.forall(t => !t.isAlive))

  test("interrupting the caller cancels in-flight tasks and restores its interrupt flag"):
    val bothStarted = new CountDownLatch(2)
    val workerInterrupted = new CountDownLatch(2)
    val seenThreads = new ConcurrentLinkedQueue[Thread]()
    val result = new AtomicReference[Either[ExecutionError, ExecutionSummary[Int]]]()
    val interruptRestored = new AtomicBoolean(false)
    val worker = new BlockWorker[Int]:
      def process(block: VoxelBlock): Int =
        seenThreads.add(Thread.currentThread())
        bothStarted.countDown()
        try
          if !new CountDownLatch(1).await(5, TimeUnit.SECONDS) then throw new IllegalStateException("task was not interrupted")
        catch
          case _: InterruptedException => workerInterrupted.countDown()
        block.index
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    val runner = new Thread(() =>
      result.set(ParallelBlockExecutor.run(2, ExecutionBudget(1, 2), () => worker, sink))
      interruptRestored.set(Thread.currentThread().isInterrupted)
    )
    runner.start()
    try
      assert(bothStarted.await(5, TimeUnit.SECONDS))
      runner.interrupt()
      runner.join(5000)
    finally
      if runner.isAlive then runner.interrupt()
    assert(!runner.isAlive)
    assertEquals(result.get(), Left(ExecutionError.Cancelled(0)))
    assert(interruptRestored.get())
    assertEquals(workerInterrupted.getCount, 0L)
    assert(seenThreads.asScala.forall(t => !t.isAlive))

  test("sink interruption waits for owned workers or returns a nonfinal termination handle"):
    for expires <- Vector(false, true) do
      val bothStarted = new CountDownLatch(3)
      val workerInterrupted = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val returned = new CountDownLatch(1)
      val active = new AtomicInteger(0)
      val deliveries = new AtomicInteger(0)
      val result = new AtomicReference[Either[ExecutionError, ExecutionSummary[Int]]]()
      val restored = new AtomicBoolean(false)
      val worker = new BlockWorker[Int]:
        def process(block: VoxelBlock): Int =
          bothStarted.countDown()
          if !bothStarted.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("second worker did not start")
          if block.index == 2 then
            active.incrementAndGet()
            try
              var done = false
              while !done do
                try done = release.await(10, TimeUnit.SECONDS)
                catch case _: InterruptedException => workerInterrupted.countDown()
              if !done then throw new IllegalStateException("held worker was not released")
            finally
              active.decrementAndGet()
              ()
          block.index
      val sink = new BlockSink[Int, Int]:
        def accept(block: VoxelBlock, payload: Int): Either[String, Int] =
          deliveries.incrementAndGet()
          if block.index == 0 then Right(block.index)
          else
            Thread.currentThread().interrupt()
            Left("reject")
      val runner = new Thread(() =>
        result.set(ParallelBlockExecutor.run(3, ExecutionBudget(1, 3), () => worker, sink,
          cleanupTimeoutMillis = if expires then 40L else 60000L))
        restored.set(Thread.currentThread().isInterrupted)
        returned.countDown()
      )
      runner.start()
      try
        assert(workerInterrupted.await(5, TimeUnit.SECONDS))
        assertEquals(active.get(), 1)
        if expires then
          assert(returned.await(5, TimeUnit.SECONDS))
          assertEquals(active.get(), 1)
          result.get() match
            case Left(ExecutionError.WorkersStillRunning(ExecutionError.SinkFailed(block, detail), ids, handle)) =>
              assertEquals(block.index, 1)
              assertEquals(detail, "reject")
              assertEquals(ids, Vector(0))
              assert(!handle.isTerminated)
              release.countDown()
              handle.awaitStopped()
              assert(handle.isTerminated)
            case other => fail(s"expected nonfinal sink failure, got $other")
        else
          assert(!returned.await(50, TimeUnit.MILLISECONDS))
          release.countDown()
          assert(returned.await(5, TimeUnit.SECONDS))
          result.get() match
            case Left(ExecutionError.SinkFailed(block, detail)) =>
              assertEquals(block.index, 1)
              assertEquals(detail, "reject")
            case other => fail(s"expected final sink failure, got $other")
        assertEquals(deliveries.get(), 2)
        assert(restored.get())
      finally
        release.countDown()
        runner.join(5000)
      assert(!runner.isAlive)

  test("cleanup timeout seam rejects unbounded and nonpositive values before work"):
    val started = new AtomicBoolean(false)
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = Right(payload)
    for timeout <- Vector(0L, -1L, 60001L) do
      val result = ParallelBlockExecutor.run(2, ExecutionBudget(1, 2), () =>
        started.set(true)
        new BlockWorker[Int]:
          def process(block: VoxelBlock): Int = block.index
      , sink, cleanupTimeoutMillis = timeout)
      assert(result.left.toOption.exists(_.isInstanceOf[ExecutionError.InvalidBudget]))
    assert(!started.get())

  test("caller interruption keeps the owned pool nonfinal while workers ignore cancellation"):
    val started = new CountDownLatch(2)
    val interrupted = new CountDownLatch(2)
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val active = new AtomicInteger(0)
    val restored = new AtomicBoolean(false)
    val result = new AtomicReference[Either[ExecutionError, ExecutionSummary[Int]]]()
    val worker = new BlockWorker[Int]:
      def process(block: VoxelBlock): Int =
        active.incrementAndGet()
        started.countDown()
        try
          var done = false
          while !done do
            try done = release.await(10, TimeUnit.SECONDS)
            catch case _: InterruptedException => interrupted.countDown()
          if !done then throw new IllegalStateException("worker was not released")
          block.index
        finally
          active.decrementAndGet()
          ()
    val sink = new BlockSink[Int, Int]:
      def accept(block: VoxelBlock, payload: Int): Either[String, Int] = fail("no delivery after caller interrupt")
    val runner = new Thread(() =>
      result.set(ParallelBlockExecutor.run(2, ExecutionBudget(1, 2), () => worker, sink,
        cleanupTimeoutMillis = 40L))
      restored.set(Thread.currentThread().isInterrupted)
      returned.countDown()
    )
    runner.start()
    try
      assert(started.await(5, TimeUnit.SECONDS))
      runner.interrupt()
      assert(interrupted.await(5, TimeUnit.SECONDS))
      assert(returned.await(5, TimeUnit.SECONDS))
      assertEquals(active.get(), 2)
      result.get() match
        case Left(ExecutionError.WorkersStillRunning(ExecutionError.Cancelled(completed), ids, handle)) =>
          assertEquals(completed, 0)
          assertEquals(ids, Vector.empty[Int])
          assert(!handle.isTerminated)
          release.countDown()
          handle.awaitStopped()
          assert(handle.isTerminated)
        case other => fail(s"expected nonfinal caller cancellation, got $other")
      assert(restored.get())
    finally
      release.countDown()
      runner.join(5000)
    assert(!runner.isAlive)
