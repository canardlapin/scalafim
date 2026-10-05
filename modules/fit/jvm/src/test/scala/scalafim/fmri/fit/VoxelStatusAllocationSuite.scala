package scalafim.fmri.fit

import java.lang.management.ManagementFactory
import com.sun.management.ThreadMXBean

class VoxelStatusAllocationSuite extends munit.FunSuite:
  test("single-voxel lookup allocation is independent of the full status-vector size"):
    val bean = ManagementFactory.getThreadMXBean match
      case value: ThreadMXBean if value.isThreadAllocatedMemorySupported => value
      case _ => fail("this allocation regression requires JVM thread-allocation counters")
    bean.setThreadAllocatedMemoryEnabled(true)
    val result = VoxelStatusLookupFixture.dense(4096)
    val id = result.voxelIndices.head
    var i = 0
    while i < 1000 do
      result.voxelStatus(id)
      i += 1
    val before = bean.getCurrentThreadAllocatedBytes
    var estimable = 0
    i = 0
    while i < 256 do
      if result.voxelStatus(id).contains(VoxelFitStatus.Estimable) then estimable += 1
      i += 1
    val bytes = bean.getCurrentThreadAllocatedBytes - before
    assertEquals(estimable, 256)
    // Generous fixed budget, far below 256 materializations of 4096 statuses.
    assert(bytes < 262144L, s"point lookups allocated $bytes bytes")
    println(s"voxel-status: 256 lookups / 4096 voxels allocated $bytes bytes")
