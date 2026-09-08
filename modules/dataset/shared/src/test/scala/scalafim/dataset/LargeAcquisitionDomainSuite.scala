package scalafim.dataset

import scalafim.image.NeuroSpace

class LargeAcquisitionDomainSuite extends munit.FunSuite:
  // Full MNI2009c 2 mm grid used by the ds000001 real-mask regression.
  private val shape = DatasetShape.unsafe(NeuroSpace(Vector(97, 115, 97),
    spacing = Some(Vector(2.0, 2.0, 2.0)), origin = Some(Vector(-96.5, -132.5, -78.5))), 1)

  test("full-sized MNI acquisition roundtrips all absolute voxel identities"):
    val domain = DatasetAcquisitionDomain.structuralCompatibility(shape, VoxelDomain.fullUnsafe(shape))
      .fold(e => fail(e.message), identity)
    assertEquals(domain.activeVoxelSpace.size, 1082035)
    var index = 0
    while index < shape.spatialSize do
      val active = domain.activeVoxelSpace.indexOption(index).get
      val full = domain.fullPointFor(active)
      assertEquals(full.value, index)
      assertEquals(domain.activePointFor(full).map(_.value), Right(index))
      index += 1

  test("large sparse acquisition preserves reordered identities and excludes unavailable voxels"):
    val indices = (0 until shape.spatialSize by 5).reverse.toVector
    val mask = VoxelDomain.activeUnsafe(shape.spatialSize, indices.map(VoxelIndex.unsafe))
    val domain = DatasetAcquisitionDomain.semantic(DatasetId("mni-sparse-reverse"), shape, mask)
      .fold(e => fail(e.message), identity)
    assertEquals(domain.activeVoxelSpace.size, indices.size)
    indices.zipWithIndex.foreach { (absolute, position) =>
      val active = domain.activeVoxelSpace.indexOption(position).get
      val full = domain.fullPointFor(active)
      assertEquals(full.value, absolute)
      assertEquals(domain.activePointFor(full).map(_.value), Right(position))
    }
    assertEquals(domain.activePointFor(domain.fullVoxelSpace.indexOption(1).get).left.toOption,
      Some(DatasetError.VoxelOutsideMask(1)))
