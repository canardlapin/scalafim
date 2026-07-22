package scalafim.dataset.zarr

import scalafim.dataset.*
import scalafim.zarr.*

class ZarrSelectionLoweringSuite extends munit.FunSuite:
  test("BIDS mapping derives stable acquisition identity from the complete entity key"):
    val acquisition = BidsAcquisition.fromRelativePath(
      "sub-01/ses-pre/func/sub-01_ses-pre_task-rest_run-02_bold.nii.gz"
    ).fold(fail(_), identity)
    assertEquals(acquisition.acquisitionId.value, "sub-01_ses-pre_task-rest_run-02_bold")
    assert(BidsAcquisition.fromRelativePath("sub-01/anat/sub-01_T1w.nii.gz").isLeft)

  test("canonical point lowering preserves non-contiguous time and voxel order"):
    val shape = Shape(3L, 2L, 2L, 3L).fold(error => fail(error.message), identity)
    val selection = ResolvedDataSelection.make(
      Vector(TimepointIndex.unsafe(2), TimepointIndex.unsafe(0)),
      Vector(VoxelIndex.unsafe(11), VoxelIndex.unsafe(0), VoxelIndex.unsafe(5))
    ).fold(error => fail(error.message), identity)
    val points = ZarrSelectionLowering.canonicalPoints(shape, selection)
      .fold(error => fail(error.message), identity)
    assertEquals(Vector.tabulate(points.count)(points.coordinate).map(_.toVector), Vector(
      Vector(2L, 1L, 1L, 2L),
      Vector(2L, 0L, 0L, 0L),
      Vector(2L, 0L, 1L, 2L),
      Vector(0L, 1L, 1L, 2L),
      Vector(0L, 0L, 0L, 0L),
      Vector(0L, 0L, 1L, 2L)
    ))

  test("canonical point lowering rejects spatial-size overflow at its trust boundary"):
    val shape = Shape(3L, Long.MaxValue, 2L, 2L).fold(error => fail(error.message), identity)
    val selection = ResolvedDataSelection.make(
      Vector(TimepointIndex.unsafe(0)),
      Vector(VoxelIndex.unsafe(0))
    ).fold(error => fail(error.message), identity)
    ZarrSelectionLowering.canonicalPoints(shape, selection) match
      case Left(DatasetError.ShapeMismatch(message)) => assert(message.contains("overflows Long"))
      case other => fail(s"expected spatial overflow rejection, found $other")
