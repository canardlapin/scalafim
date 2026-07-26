package scalafim.dataset

import narr.NArray
import scalafim.image.{
  DMat,
  NeuroSpace,
  VoxelSelection as ImageVoxelSelection
}
import scalafim.locus.{FiniteSpace, Selection as LocusSelection}

class DatasetAcquisitionDomainSuite extends munit.FunSuite:

  private def shape(
      affine: DMat = DMat.eye(4),
      timepoints: Int = 3
  ): DatasetShape =
    DatasetShape.unsafe(
      NeuroSpace(Vector(4, 1, 1), trans = Some(affine)),
      timepoints
    )

  private def domain(
      id: String,
      requestedShape: DatasetShape,
      voxels: VoxelDomain
  ): DatasetAcquisitionDomain =
    DatasetAcquisitionDomain
      .semantic(DatasetId(id), requestedShape, voxels)
      .fold(error => fail(error.message), identity)

  test("semantic acquisition identity includes dataset and exact grid geometry"):
    val original = shape()
    val shifted =
      shape(
        DMat.fromRows(
          Vector(
            Vector(1.0, 0.0, 0.0, 10.0),
            Vector(0.0, 1.0, 0.0, 0.0),
            Vector(0.0, 0.0, 1.0, 0.0),
            Vector(0.0, 0.0, 0.0, 1.0)
          )
        )
      )
    val first = domain("run-a", original, VoxelDomain.fullUnsafe(original))
    val otherDataset = domain("run-b", original, VoxelDomain.fullUnsafe(original))
    val otherGrid = domain("run-a", shifted, VoxelDomain.fullUnsafe(shifted))

    assert(!first.fullVoxelSpace.sameIdentityAs(otherDataset.fullVoxelSpace))
    assert(!first.fullVoxelSpace.sameIdentityAs(otherGrid.fullVoxelSpace))

  test("active-to-full injection and checked reverse preserve global voxel identity"):
    val requestedShape = shape()
    val active =
      VoxelDomain.activeUnsafe(
        requestedShape.spatialSize,
        Vector(VoxelIndex.unsafe(0), VoxelIndex.unsafe(2), VoxelIndex.unsafe(3))
      )
    val acquisition = domain("masked", requestedShape, active)

    assertEquals(acquisition.activeToFull.mapping.targetOrdinals.toVector, Vector(0, 2, 3))
    val activeTwo = acquisition.activeVoxelSpace.point(1).get
    assertEquals(acquisition.fullPointFor(activeTwo).ordinal, 2)
    assertEquals(
      acquisition.activePointFor(acquisition.fullVoxelSpace.point(2).get).map(_.ordinal),
      Right(1)
    )
    assertEquals(
      acquisition.activePointFor(acquisition.fullVoxelSpace.point(1).get).left.toOption,
      Some(DatasetError.VoxelOutsideMask(1))
    )

  test("All and AllSpatial retain availability policy while explicit order survives"):
    val requestedShape = shape()
    val active =
      VoxelDomain.activeUnsafe(
        requestedShape.spatialSize,
        Vector(VoxelIndex.unsafe(0), VoxelIndex.unsafe(2), VoxelIndex.unsafe(3))
      )
    val acquisition = domain("masked", requestedShape, active)

    val readable =
      acquisition
        .resolveVoxels(VoxelSelection.All)
        .fold(error => fail(error.message), identity)
    val full =
      acquisition
        .resolveVoxels(VoxelSelection.AllSpatial)
        .fold(error => fail(error.message), identity)
    val ordered =
      acquisition
        .resolveVoxels(VoxelSelection.indices(3, 0))
        .fold(error => fail(error.message), identity)

    assertEquals(readable.ordinals.toVector, Vector(0, 2, 3))
    assertEquals(full.ordinals.toVector, Vector(0, 1, 2, 3))
    assertEquals(ordered.ordinals.toVector, Vector(3, 0))

  test("resolved selections reject another acquisition even when sizes match"):
    val requestedShape = shape()
    val full = VoxelDomain.fullUnsafe(requestedShape)
    val first = domain("run-a", requestedShape, full)
    val second = domain("run-b", requestedShape, full)
    val timepoints =
      scalafim.locus.Selection
        .fromOrdinals(first.timeSpace, Vector(2, 0))
        .fold(error => fail(error.message), identity)
    val voxels =
      scalafim.locus.Selection
        .fromOrdinals(first.fullVoxelSpace, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assert(first.fromSelections(timepoints, voxels).isRight)

    val foreignTimeSpace =
      FiniteSpace
        .make[second.T](first.timeSpace.key, second.timeSpace.size)
        .fold(error => fail(error.message), identity)
    val foreignVoxelSpace =
      FiniteSpace
        .make[second.X](first.fullVoxelSpace.key, second.fullVoxelSpace.size)
        .fold(error => fail(error.message), identity)
    val foreignTimepoints =
      LocusSelection
        .fromOrdinals(foreignTimeSpace, Vector(2, 0))
        .fold(error => fail(error.message), identity)
    val foreignVoxels =
      LocusSelection
        .fromOrdinals(foreignVoxelSpace, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assert(second.fromSelections(foreignTimepoints, foreignVoxels).isLeft)

  test("image selection adapter preserves requested order and checks exact volume grid"):
    val requestedShape = shape()
    val selected =
      ImageVoxelSelection
        .make(requestedShape.volumeSpace, NArray[Int](3, 0))
        .fold(error => fail(error.message), identity)
    val adapted =
      VoxelSelection
        .fromImage(selected, requestedShape)
        .fold(error => fail(error.message), identity)
    val resolved =
      domain("run-a", requestedShape, VoxelDomain.fullUnsafe(requestedShape))
        .resolveVoxels(adapted)
        .fold(error => fail(error.message), identity)

    assertEquals(resolved.ordinals.toVector, Vector(3, 0))

    val shifted =
      shape(
        DMat.fromRows(
          Vector(
            Vector(1.0, 0.0, 0.0, 10.0),
            Vector(0.0, 1.0, 0.0, 0.0),
            Vector(0.0, 0.0, 1.0, 0.0),
            Vector(0.0, 0.0, 0.0, 1.0)
          )
        )
      )
    assert(VoxelSelection.fromImage(selected, shifted).isLeft)

  test("compatibility constructor rejects duplicate resolved order"):
    assert(
      ResolvedDataSelection
        .make(
          Vector(TimepointIndex.unsafe(0), TimepointIndex.unsafe(0)),
          Vector(VoxelIndex.unsafe(0))
        )
        .left
        .toOption
        .contains(DatasetError.DuplicateSelection(DatasetAxis.Timepoint, 0))
    )
