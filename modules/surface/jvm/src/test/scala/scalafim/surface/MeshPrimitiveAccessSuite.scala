package scalafim.surface

import java.lang.management.ManagementFactory

class MeshPrimitiveAccessSuite extends munit.FunSuite:
  @volatile private var sink = 0.0

  test("packed coordinate and face reads allocate no per-element objects"):
    val available = ManagementFactory.getThreadMXBean match
      case value: com.sun.management.ThreadMXBean => Some(value)
      case _ => None
    assume(available.nonEmpty, "thread allocation accounting is unavailable")
    val bean = available.get
    assume(bean.isThreadAllocatedMemorySupported, "thread allocation accounting is unsupported")
    if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
    val mesh = TriangleMesh.fromArrays(
      Array(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0), Array(0, 1, 2))
    def read(count: Int): Double =
      var sum = 0.0
      var i = 0
      while i < count do
        sum += mesh.coordinates(i % 9) + mesh.faceIndices(i % 3)
        i += 1
      sum
    var warmup = 0
    while warmup < 20 do
      sink = read(90000)
      warmup += 1
    val thread = Thread.currentThread().getId
    val before = bean.getThreadAllocatedBytes(thread)
    val total = read(900000)
    val allocated = bean.getThreadAllocatedBytes(thread) - before
    sink = total
    assertEqualsDouble(total, 1100000.0, 0.0)
    assert(allocated < 65536L, s"1.8 million primitive reads allocated $allocated bytes")
