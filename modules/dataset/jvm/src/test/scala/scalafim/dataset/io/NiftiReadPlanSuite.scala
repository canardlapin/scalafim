package scalafim.dataset.io

import munit.FunSuite
import scalafim.dataset.DatasetError

class NiftiReadPlanSuite extends FunSuite:
  test("full and dense selections collapse to one bounded window") {
    val full = plan((0 until 50000).toVector, bytesPerValue = 8)
    val denseMask = Vector.tabulate(50000)(index => index + index / 100)
    val dense = plan(denseMask, bytesPerValue = 8)

    assertEquals(full.windows.length, 1)
    assertEquals(full.windows.head.startVoxel, 0)
    assertEquals(full.windows.head.endVoxelExclusive, 50000)
    assertEquals(full.stats(200).plannedReads, 200L)
    assertEquals(dense.windows.length, 1)
    assertEquals(dense.stats(200).plannedReads, 200L)
    assert(dense.stats(200).plannedReads < 200L * denseMask.length.toLong / 1000L)
  }

  test("sparse selections preserve output order without an unbounded enclosing read") {
    val readPlan = plan(Vector(1000000, 0, 500000), bytesPerValue = 4)

    assertEquals(readPlan.windows.length, 3)
    assertEquals(readPlan.maxBufferBytes, 4)
    assertEquals(readPlan.windows.map(_.startVoxel), Vector(0, 500000, 1000000))
    assertEquals(readPlan.windows.map(_.outputColumns.toVector), Vector(Vector(1), Vector(2), Vector(0)))
  }

  test("maximum window size splits otherwise contiguous reads") {
    val readPlan = plan(
      voxels = (0 until 10).toVector,
      bytesPerValue = 8,
      maxGapBytes = 0,
      maxWindowBytes = 32
    )

    assertEquals(readPlan.windows.map(_.byteCount), Vector(32, 32, 16))
    assertEquals(readPlan.maxBufferBytes, 32)
    assertEquals(readPlan.stats(2).plannedReads, 6L)
    assertEquals(readPlan.stats(2).plannedBytes, 160L)
  }

  test("canonical voxel ordinals map explicitly to x-fastest NIfTI windows") {
    val readPlan =
      NiftiReadPlan
        .fromCanonicalVoxels(
          voxels = Vector(5, 1, 4),
          spatialDims = Vector(2, 3, 2),
          bytesPerValue = 8,
          maxGapBytes = 0
        )
        .fold(error => fail(error.message), identity)

    assertEquals(readPlan.windows.map(_.startVoxel), Vector(4, 6, 10))
    assertEquals(
      readPlan.windows.map(_.outputColumns.toVector),
      Vector(Vector(2), Vector(1), Vector(0))
    )
  }

  test("planner rejects invalid and duplicate voxel inputs") {
    assertEquals(
      NiftiReadPlan.make(Vector.empty, bytesPerValue = 8),
      Left(DatasetError.EmptySelection(scalafim.dataset.DatasetAxis.Voxel))
    )
    assertEquals(
      NiftiReadPlan.make(Vector(-1), bytesPerValue = 8),
      Left(DatasetError.NegativeIndex(scalafim.dataset.DatasetAxis.Voxel, -1))
    )
    assertEquals(
      NiftiReadPlan.make(Vector(2, 2), bytesPerValue = 8),
      Left(DatasetError.DuplicateSelection(scalafim.dataset.DatasetAxis.Voxel, 2))
    )
    assert(NiftiReadPlan.make(Vector(0), bytesPerValue = 0).isLeft)
    assert(NiftiReadPlan.make(Vector(0), bytesPerValue = 8, maxWindowBytes = 4).isLeft)
    assert(NiftiReadPlan.make(Vector(Int.MaxValue), bytesPerValue = 8).isLeft)
  }

  private def plan(
      voxels: Vector[Int],
      bytesPerValue: Int,
      maxGapBytes: Int = NiftiReadPlan.DefaultMaxGapBytes,
      maxWindowBytes: Int = NiftiReadPlan.DefaultMaxWindowBytes
  ): NiftiReadPlan =
    NiftiReadPlan
      .make(voxels, bytesPerValue, maxGapBytes, maxWindowBytes)
      .fold(error => fail(error.message), identity)
