package scalafim.locus

import com.sun.management.ThreadMXBean
import java.lang.management.ManagementFactory

class LocusAllocationBenchmarkSuite extends munit.FunSuite:
  private sealed trait S

  private final case class Measurement(
      elapsedNanos: Long,
      allocatedBytes: Long,
      checksum: Int
  )

  private def allocationBean(): ThreadMXBean =
    ManagementFactory.getThreadMXBean match
      case bean: ThreadMXBean =>
        if !bean.isThreadAllocatedMemoryEnabled then
          bean.setThreadAllocatedMemoryEnabled(true)
        bean
      case _ =>
        fail("JVM thread-allocation accounting is required for the locus benchmark receipt")

  private def measure(bean: ThreadMXBean, iterations: Int)(body: => Int): Measurement =
    var warmup = 0
    while warmup < 20 do
      body
      warmup += 1

    val allocatedBefore = bean.getCurrentThreadAllocatedBytes
    val started = System.nanoTime()
    var checksum = 0
    var iteration = 0
    while iteration < iterations do
      checksum ^= body
      iteration += 1
    val elapsed = System.nanoTime() - started
    val allocated = bean.getCurrentThreadAllocatedBytes - allocatedBefore
    Measurement(elapsed, allocated, checksum)

  test("records primitive-region Boolean allocation and timing"):
    val space = FiniteSpace.make[S](SpaceKey.unsafe("benchmark:allocation"), 100000).toOption.get
    val left = Region
      .fromOrdinals(space, Array.tabulate(12000)(i => i * 7))
      .toOption
      .get
    val right = Region
      .fromOrdinals(space, Array.tabulate(10000)(i => i * 9))
      .toOption
      .get
    val bean = allocationBean()

    val union = measure(bean, 100):
      left.union(right).toOption.get.cardinality
    val intersection = measure(bean, 100):
      left.intersect(right).toOption.get.cardinality

    assert(union.allocatedBytes >= 0L)
    assert(intersection.allocatedBytes >= 0L)
    assert(union.checksum >= 0)
    assert(intersection.checksum >= 0)

    println:
      s"locus allocation benchmark: operation=union, iterations=100, " +
        s"elapsed_ns=${union.elapsedNanos}, allocated_bytes=${union.allocatedBytes}"
    println:
      s"locus allocation benchmark: operation=intersection, iterations=100, " +
        s"elapsed_ns=${intersection.elapsedNanos}, allocated_bytes=${intersection.allocatedBytes}"
