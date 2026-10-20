package scalafim.dataset

import image4s.SomeSampleSpace

import gale.linalg.DMat
import image4s.locus.GridDomain
import neuroim4s.image.{SampleSpaces}

class DatasetAcquisitionDomainSuite extends munit.FunSuite:

  private def shape(
      affine: DMat = DMat.eye(4),
      timepoints: Int = 3
  ): DatasetShape =
    DatasetShape.unsafe(
      SampleSpaces(Vector(4, 1, 1), affine = Some(GaleTestData.affineD3(affine))),
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

  private val shifted =
    GaleTestData.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 10.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  /** A grid in a declared template world: separately built, equal geometry
    * yields the same persistent grid, so a reload can be simulated faithfully.
    */
  private def placed(affine: DMat = DMat.eye(4), timepoints: Int = 3): DatasetShape =
    val world =
      neuroim4s.spaces.WorldSpace
        .template("MNI152NLin2009cAsym")
        .fold(error => fail(error.message), identity)
    val space =
      SampleSpaces
        .inWorld(SampleSpaces(Vector(4, 1, 1), affine = Some(GaleTestData.affineD3(affine))), world)
        .fold(error => fail(error.message), identity)
    DatasetShape.unsafe(space, timepoints)

  private def placedDomain(id: String, requested: DatasetShape): DatasetAcquisitionDomain =
    domain(id, requested, VoxelDomain.fullUnsafe(requested))

  test("semantic acquisition identity includes dataset and exact grid geometry"):
    val first = placedDomain("run-a", placed())
    val reload = placedDomain("run-a", placed())
    val otherDataset = placedDomain("run-b", placed())
    val otherGrid = placedDomain("run-a", placed(shifted))

    assert(first.fullVoxelSpace.align(reload.fullVoxelSpace).isRight, "a rebuilt grid reloads")
    assert(first.timeSpace.align(reload.timeSpace).isRight)
    assert(first.fullVoxelSpace.align(otherDataset.fullVoxelSpace).isLeft)
    assert(first.fullVoxelSpace.align(otherGrid.fullVoxelSpace).isLeft, "same frame, other affine")

  test("no dataset name collides with the structural identity basis"):
    val requested = placed()
    val named = placedDomain("structural", requested)
    val structural =
      DatasetAcquisitionDomain
        .structuralCompatibility(requested, VoxelDomain.fullUnsafe(requested))
        .fold(error => fail(error.message), identity)
    assert(named.fullVoxelSpace.align(structural.fullVoxelSpace).isLeft)

  test("active-to-full injection and checked reverse preserve global voxel identity"):
    val requestedShape = shape()
    val active =
      VoxelDomain.activeUnsafe(
        requestedShape.spatialSize,
        Vector(VoxelIndex.unsafe(0), VoxelIndex.unsafe(2), VoxelIndex.unsafe(3))
      )
    val acquisition = domain("masked", requestedShape, active)

    assertEquals(acquisition.activeToFull.toTotalMap.targetOrdinals.toVector, Vector(0, 2, 3))
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

  test("matching semantic acquisitions align across independent constructions"):
    // Each construction restores independent owners. Equal persistent keys make
    // a reload usable with the first view through a checked alignment; nothing
    // is shared process-wide.
    val requestedShape = shape()
    val full = VoxelDomain.fullUnsafe(requestedShape)
    val first = domain("run-a", requestedShape, full)
    val second = domain("run-a", requestedShape, full)
    val timepoints =
      locus4s.Selection
        .fromOrdinals(first.timeSpace, Vector(2, 0))
        .fold(error => fail(error.message), identity)
    val voxels =
      locus4s.Selection
        .fromOrdinals(first.fullVoxelSpace, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assert(first.fromSelections(timepoints, voxels).isRight)
    assert(first.timeSpace.samePersistentIdentityAs(second.timeSpace))
    assert(first.fullVoxelSpace.samePersistentIdentityAs(second.fullVoxelSpace))
    assert(!first.fullVoxelSpace.sameRuntimeOwnerAs(second.fullVoxelSpace))
    // The distinct static owners mean `second.fromSelections(timepoints, ...)`
    // does not compile. The dynamic boundary realigns explicitly instead.
    val timeAlignment =
      first.timeSpace.align(second.timeSpace).fold(error => fail(error.message), identity)
    val voxelAlignment =
      first.fullVoxelSpace.align(second.fullVoxelSpace).fold(error => fail(error.message), identity)
    val moved =
      second
        .fromSelections(timeAlignment.transport(timepoints), voxelAlignment.transport(voxels))
        .fold(error => fail(error.message), identity)
    assertEquals(moved.voxels, Vector(3, 1))
    assertEquals(moved.timepoints, Vector(2, 0))

  test("separately built grids in an unresolved world make no shared identity claim"):
    // Identity follows the persistent grid key, never object hash codes. A grid
    // whose world is unresolved gets a fresh frame per construction, so equal
    // dimensions and affine do not assert that two loads are one voxel space.
    val first = domain("run-a", shape(), VoxelDomain.fullUnsafe(shape()))
    val second = domain("run-a", shape(), VoxelDomain.fullUnsafe(shape()))
    assert(first.fullVoxelSpace.isPersistable)
    assert(first.fullVoxelSpace.align(second.fullVoxelSpace).isLeft)

  test("acquisitions differing only in run length get distinct time domains"):
    // The time key must carry the timepoint count; without it two run lengths
    // over one grid claimed the same domain id at two different sizes.
    val short = placedDomain("run-a", placed(timepoints = 2))
    val long = placedDomain("run-a", placed(timepoints = 4))
    assert(short.timeSpace.align(long.timeSpace).isLeft)
    assert(short.fullVoxelSpace.align(long.fullVoxelSpace).isRight, "the grid is shared")
    assertEquals(short.timeSpace.size, 2)
    assertEquals(long.timeSpace.size, 4)

  test("image selection adapter preserves requested order and checks exact volume grid"):
    val requestedShape = shape()
    val packed =
      GridDomain
        .register(
          requestedShape.grid,
          "dataset image selection adapter",
          locus4s.DomainRegistry.empty
        )
        .fold(error => fail(error.message), identity)
    type Voxel = packed.S
    val imageDomain = packed.value
    val selected =
      locus4s.Selection
        .fromOrdinals(imageDomain.space, Vector(3, 0))
        .fold(error => fail(error.message), identity)
    val adapted =
      VoxelSelection
        .fromImage(imageDomain, selected, requestedShape)
        .fold(error => fail(error.message), identity)
    val resolved =
      domain("run-a", requestedShape, VoxelDomain.fullUnsafe(requestedShape))
        .resolveVoxels(adapted)
        .fold(error => fail(error.message), identity)

    assertEquals(resolved.ordinals.toVector, Vector(3, 0))

    val shifted =
      shape(
        GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 0.0, 0.0, 10.0),
            Vector(0.0, 1.0, 0.0, 0.0),
            Vector(0.0, 0.0, 1.0, 0.0),
            Vector(0.0, 0.0, 0.0, 1.0)
          )
        )
      )
    assert(VoxelSelection.fromImage(imageDomain, selected, shifted).isLeft)

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
