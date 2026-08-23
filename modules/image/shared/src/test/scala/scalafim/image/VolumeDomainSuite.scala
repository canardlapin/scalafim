package scalafim.image

import locus4s.DomainRegistry
import scalafim.locus.*

class VolumeDomainSuite extends munit.FunSuite:
  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(2, 2, 1)))

  test("fresh registry scopes keep equal exact grids runtime-distinct"):
    val first =
      VolumeDomain.canonical(volumeSpace).value
    val second =
      VolumeDomain.canonical(volumeSpace).value

    assert(!first.finiteSpace.sameRuntimeOwnerAs(second.finiteSpace))
    assert(first.finiteSpace.samePersistentIdentityAs(second.finiteSpace))

  test("explicit registry ownership canonicalizes restores without leaking across resources"):
    val first =
      VolumeDomain
        .canonicalIn(DomainRegistry.empty, volumeSpace)
        .toOption
        .get
    val restored =
      VolumeDomain
        .canonicalIn(first.registry, volumeSpace)
        .toOption
        .get
    val independentlyOpened = VolumeDomain.canonical(volumeSpace)

    assert(first.value.finiteSpace.sameRuntimeOwnerAs(restored.value.finiteSpace))
    assert(!first.value.finiteSpace.sameRuntimeOwnerAs(independentlyOpened.value.finiteSpace))
    assert(first.value.finiteSpace.samePersistentIdentityAs(independentlyOpened.value.finiteSpace))

  test("different exact grids have different canonical records in one registry"):
    val first =
      VolumeDomain
        .canonicalIn(DomainRegistry.empty, volumeSpace)
        .toOption
        .get
    val differentSize = VolumeSpace(NeuroSpace(Vector(3, 2, 1)))
    val second =
      VolumeDomain
        .canonicalIn(first.registry, differentSize)
        .toOption
        .get

    assertNotEquals(
      first.value.gridDomainRecord.domain.key,
      second.value.gridDomainRecord.domain.key
    )

  test("legacy voxel regions and selections are backed by locus semantics"):
    val domain = VolumeDomain.canonical(volumeSpace).value
    val voxelRegion = VoxelRegion.make(volumeSpace, Array(3, 1, 1)).toOption.get
    val voxelSelection = VoxelSelection.make(volumeSpace, Array(3, 1)).toOption.get
    val region = domain.region(voxelRegion).toOption.get
    val selection = domain.selection(voxelSelection).toOption.get

    assertEquals(
      region.ordinalsInDomainOrder.toSet,
      Set(2, 3)
    )
    assertEquals(selection.ordinals.toVector, Vector(3, 2))
    assertEquals(domain.voxelRegion(region).toOption.get, voxelRegion)
    assertEquals(domain.voxelSelection(selection).toOption.get, voxelSelection)

  test("volume values expose a pure indexed field without copying geometry policy"):
    val domain =
      VolumeDomain.canonical(volumeSpace).value
    val volume =
      NeuroVol.fromLinear(Array(10, 11, 12, 13), volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val even = domain.supportWhere(field)(_ % 2 == 0).toOption.get

    assertEquals(
      domain.finiteSpace.indices.map(field.apply).toVector,
      Vector(10, 12, 11, 13)
    )
    assertEquals(even.ordinalsInDomainOrder.toVector, Vector(0, 1))

  test("volume adapters reject an exact-grid mismatch"):
    val translated =
      VolumeSpace(
        NeuroSpace(
          Vector(2, 2, 1),
          trans = Some(
            DMat.fromRows(
              Vector(
                Vector(1.0, 0.0, 0.0, 2.0),
                Vector(0.0, 1.0, 0.0, 0.0),
                Vector(0.0, 0.0, 1.0, 0.0),
                Vector(0.0, 0.0, 0.0, 1.0)
              )
            )
          )
        )
      )
    val domain =
      VolumeDomain.canonical(volumeSpace).value
    val wrong = VoxelRegion.make(translated, Array(0)).toOption.get

    assert(domain.region(wrong).isLeft)

  test("runtime-loaded volume domains retain a fresh path-dependent point type"):
    val loaded =
      SomeVolumeDomain.canonical(volumeSpace)
    val domain = loaded.value
    val region = Region.fromOrdinals(domain.finiteSpace, Vector(0, 3)).toOption.get

    assertEquals(
      intValues(domain.voxelRegion(region).toOption.get.linearIndices),
      Vector(0, 3)
    )

  private def intValues(values: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))
