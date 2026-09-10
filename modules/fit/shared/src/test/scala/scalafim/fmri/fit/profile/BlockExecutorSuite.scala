package scalafim.fmri.fit.profile

class BlockExecutorSuite extends munit.FunSuite:

  private final class SumWorker extends BlockWorker[Array[Double]]:
    def process(block: VoxelBlock): Array[Double] = Array.tabulate(block.count)(i => (block.start + i).toDouble)

  private final class CollectingSink(fail: Int = -1) extends BlockSink[Array[Double], (Int, Double)]:
    val seen = Vector.newBuilder[Int]
    def accept(block: VoxelBlock, payload: Array[Double]): Either[String, (Int, Double)] =
      seen += block.index
      if block.index == fail then Left("refused") else Right((block.index, payload.sum))

  test("blocks partition the voxel range with the last block shorter"):
    val plan = BlockExecutor.blocks(1000, 256)
    assertEquals(plan.length, 4)
    assertEquals(plan.last, VoxelBlock(3, 768, 232))
    assertEquals(plan.map(_.count).sum, 1000)

  test("sequential execution delivers receipts in block order and retains no payloads"):
    val sink = new CollectingSink
    val summary = BlockExecutor.runSequential(1000, ExecutionBudget(256, 1), () => new SumWorker, sink).fold(e => fail(e.message), identity)
    assertEquals(summary.blocks, 4)
    assertEquals(summary.receipts.map(_._1), Vector(0, 1, 2, 3))
    assertEqualsDouble(summary.receipts.map(_._2).sum, (0 until 1000).map(_.toDouble).sum, 1e-9)
    assertEquals(sink.seen.result(), Vector(0, 1, 2, 3))

  test("budget, sink failure and cancellation are typed outcomes"):
    assert(ExecutionBudget(0, 1).validate.isLeft)
    assert(ExecutionBudget(257, 1).validate.isLeft)
    assert(ExecutionBudget(256, 9).validate.isLeft)
    BlockExecutor.runSequential(600, ExecutionBudget(256, 1), () => new SumWorker, new CollectingSink(fail = 1)) match
      case Left(ExecutionError.SinkFailed(block, _)) => assertEquals(block.index, 1)
      case other => fail(s"expected SinkFailed, got $other")
    var calls = 0
    BlockExecutor.runSequential(600, ExecutionBudget(256, 1), () => new SumWorker, new CollectingSink, () => { calls += 1; calls > 2 }) match
      case Left(ExecutionError.Cancelled(completed)) => assertEquals(completed, 2)
      case other => fail(s"expected Cancelled, got $other")
    val boom = new BlockWorker[Array[Double]]:
      def process(block: VoxelBlock): Array[Double] = throw new IllegalStateException("boom")
    assert(BlockExecutor.runSequential(10, ExecutionBudget(4, 1), () => boom, new CollectingSink).left.exists(_.isInstanceOf[ExecutionError.WorkerFailed]))
