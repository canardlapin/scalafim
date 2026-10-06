package scalafim.fmri.fit

import java.lang.management.ManagementFactory
import com.sun.management.ThreadMXBean
import scalafim.dataset.DatasetShape
import scalafim.image.SampleSpaces

class ResultArtifactsAllocationSuite extends munit.FunSuite:
  private def allocationCounter: ThreadMXBean =
    val counter = ManagementFactory.getThreadMXBean match
      case value: ThreadMXBean if value.isThreadAllocatedMemorySupported => Some(value)
      case _ => None
    assume(counter.isDefined, "this allocation regression requires JVM thread-allocation counters")
    val bean = counter.get
    bean.setThreadAllocatedMemoryEnabled(true)
    bean

  private def exportArtifact(result: FmriFitResult): StatMap =
    val provenance = AnalysisProvenance.fromResult(result, source = "allocation-export")
    StatMap.unsafe(
      name = "residual_variance",
      kind = StatisticKind.ResidualVariance,
      values = gale.linalg.DVec.fromSeq(Vector.fill(result.voxelIndices.length)(1.0)),
      shape = DatasetShape.unsafe(SampleSpaces(Vector(result.voxelIndices.max + 1, 1, 1)), result.timepoints.length),
      selectedVoxels = SelectedVoxelIndices.unsafe(result.voxelIndices),
      provenance = provenance
    )

  private def measuredExport(bean: ThreadMXBean, result: FmriFitResult): Long =
    var warmup = 0
    while warmup < 3 do
      exportArtifact(result)
      warmup += 1
    val before = bean.getCurrentThreadAllocatedBytes
    val artifact = exportArtifact(result)
    val bytes = bean.getCurrentThreadAllocatedBytes - before
    assertEquals(artifact.voxelIndices, result.voxelIndices)
    assertEquals(artifact.provenance.voxelStatuses.get.map(_.voxelIndex), result.voxelIndices)
    assert(artifact.provenance.voxelStatuses.get.forall(_.status == VoxelFitStatus.Estimable))
    bytes

  private def checkLinearExport(label: String, source: Int => FmriFitResult): Unit =
    val bean = allocationCounter
    val small = measuredExport(bean, source(1024))
    val large = measuredExport(bean, source(4096))
    println(s"EXPORT_ALLOCATION kind=$label voxels=1024 bytes=$small")
    println(s"EXPORT_ALLOCATION kind=$label voxels=4096 bytes=$large")
    // The complete artifact includes provenance records, axis validation and
    // payload construction. This generous linear allowance is still far below
    // allocating a 4096-element default status vector at every run/voxel pair.
    assert(large < 4096L * 4096L, s"$label export of 4096 voxels allocated $large bytes")
    assert(large < small * 6L, s"$label export grew from $small to $large bytes when voxels grew fourfold")

  test("whole runwise artifact export allocates linearly in voxels"):
    checkLinearExport("runwise", ResultArtifactsExportFixture.runwise)

  test("whole patterned artifact export allocates linearly in voxels"):
    checkLinearExport("patterned", voxels =>
      val source = ResultArtifactsExportFixture.runwise(voxels)
      ResultArtifactsExportFixture.patterned(Vector(source), source.voxelIndices.reverse)
    )
