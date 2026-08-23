package scalafim.dataset

import locus4s.DomainRegistry
import scalafim.locus.mapping
import scalafim.image.{
  DMat,
  NeuroSpace,
  VoxelSelection as ImageVoxelSelection
}

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

  private def domainIn(
      registry: DomainRegistry,
      id: String,
      requestedShape: DatasetShape,
      voxels: VoxelDomain
  ): DatasetAcquisitionDomain =
    DatasetAcquisitionDomain
      .semanticIn(registry, DatasetId(id), requestedShape, voxels)
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

    assert(!first.fullVoxelSpace.sameRuntimeOwnerAs(otherDataset.fullVoxelSpace))
    assert(!first.fullVoxelSpace.sameRuntimeOwnerAs(otherGrid.fullVoxelSpace))

  test("acquisition metadata is separate from exact voxel-domain identity"):
    val requestedShape =
      DatasetShape.unsafe(NeuroSpace(Vector(2, 3, 2)), 3)
    val full = VoxelDomain.fullUnsafe(requestedShape)
    val first =
      domainIn(
        DomainRegistry.empty,
        "run-a",
        requestedShape,
        full
      )
    val second =
      domainIn(
        first.registry,
        "run-b",
        requestedShape,
        full
      )

    assert(!first.timeSpace.sameRuntimeOwnerAs(second.timeSpace))
    assert(
      first.fullVoxelSpace.sameRuntimeOwnerAs(second.fullVoxelSpace)
    )
    assertEquals(
      first.volumeDomain.gridDomainRecord,
      second.volumeDomain.gridDomainRecord
    )

  test("asymmetric acquisition converts legacy mask and selection ordinals"):
    val requestedShape =
      DatasetShape.unsafe(NeuroSpace(Vector(2, 3, 2)), 3)
    val active =
      VoxelDomain.activeUnsafe(
        requestedShape.spatialSize,
        Vector(
          VoxelIndex.unsafe(1),
          VoxelIndex.unsafe(2),
          VoxelIndex.unsafe(6),
          VoxelIndex.unsafe(11)
        )
      )
    val acquisition = domain("asymmetric", requestedShape, active)
    val selected =
      acquisition
        .resolveVoxels(VoxelSelection.indices(11, 1, 6))
        .fold(error => fail(error.message), identity)
    val resolved =
      acquisition
        .resolve(
          TimepointSelection.indices(0),
          VoxelSelection.indices(11, 1, 6)
        )
        .fold(error => fail(error.message), identity)

    assertEquals(
      acquisition.activeToFull.mapping.targetOrdinals.toVector,
      Vector(6, 2, 1, 11)
    )
    assertEquals(selected.ordinals.toVector, Vector(11, 6, 1))
    assertEquals(resolved.voxels, Vector(11, 1, 6))

  test("active-to-full injection and checked reverse preserve global voxel identity"):
    val requestedShape = shape()
    val active =
      VoxelDomain.activeUnsafe(
        requestedShape.spatialSize,
        Vector(VoxelIndex.unsafe(0), VoxelIndex.unsafe(2), VoxelIndex.unsafe(3))
      )
    val acquisition = domain("masked", requestedShape, active)

    assertEquals(acquisition.activeToFull.mapping.targetOrdinals.toVector, Vector(0, 2, 3))
    val activeTwo = acquisition.activeVoxelSpace.indexOption(1).get
    assertEquals(acquisition.fullPointFor(activeTwo).value, 2)
    assertEquals(
      acquisition.activePointFor(acquisition.fullVoxelSpace.indexOption(2).get).map(_.value),
      Right(1)
    )
    assertEquals(
      acquisition.activePointFor(acquisition.fullVoxelSpace.indexOption(1).get).left.toOption,
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

  test("matching semantic acquisitions share owners only in an explicit scope"):
    val requestedShape = shape()
    val full = VoxelDomain.fullUnsafe(requestedShape)
    val first = domain("run-a", requestedShape, full)
    val restored = domainIn(first.registry, "run-a", requestedShape, full)
    val independent = domain("run-a", requestedShape, full)
    val timepoints =
      scalafim.locus.Selection
        .fromOrdinals(first.timeSpace, Vector(2, 0))
        .fold(error => fail(error.message), identity)
    val voxels =
      scalafim.locus.Selection
        .fromOrdinals(first.fullVoxelSpace, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assert(first.fromSelections(timepoints, voxels).isRight)
    assert(first.timeSpace.samePersistentIdentityAs(restored.timeSpace))
    assert(first.fullVoxelSpace.samePersistentIdentityAs(restored.fullVoxelSpace))
    assert(first.timeSpace.sameRuntimeOwnerAs(restored.timeSpace))
    assert(first.fullVoxelSpace.sameRuntimeOwnerAs(restored.fullVoxelSpace))
    assert(first.timeSpace.samePersistentIdentityAs(independent.timeSpace))
    assert(!first.timeSpace.sameRuntimeOwnerAs(independent.timeSpace))
    assert(!first.fullVoxelSpace.sameRuntimeOwnerAs(independent.fullVoxelSpace))
    assert(first.timeSpace.align(restored.timeSpace).isRight)
    assert(first.fullVoxelSpace.align(restored.fullVoxelSpace).isRight)
    assertEquals(first.registry.size, 2)

  test("acquisitions differing only in run length get distinct time domains"):
    // The time key must carry the timepoint count; without it two run lengths
    // over one grid claimed the same domain id at two different sizes.
    val full4 = VoxelDomain.fullUnsafe(shape())
    val short = domain("run-a", shape(timepoints = 2), VoxelDomain.fullUnsafe(shape(timepoints = 2)))
    val long = domain("run-a", shape(timepoints = 4), full4)
    assert(!short.timeSpace.sameRuntimeOwnerAs(long.timeSpace))
    assertEquals(short.timeSpace.size, 2)
    assertEquals(long.timeSpace.size, 4)

  test("image selection adapter preserves requested order and checks exact volume grid"):
    val requestedShape = shape()
    val selected =
      ImageVoxelSelection
        .make(requestedShape.volumeSpace, Array[Int](3, 0))
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
