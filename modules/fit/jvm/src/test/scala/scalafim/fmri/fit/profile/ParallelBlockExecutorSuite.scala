package scalafim.fmri.fit.profile

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
