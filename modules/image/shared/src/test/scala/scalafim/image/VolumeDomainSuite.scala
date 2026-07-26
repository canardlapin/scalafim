package scalafim.image

import narr.NArray
import scalafim.locus.*
import scalafim.locus.laws.LocusDifferential

class VolumeDomainSuite extends munit.FunSuite:
  private sealed trait S

  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(2, 2, 1)))

  test("semantic keys distinguish equal-geometry volume domains at runtime"):
    val first =
      VolumeDomain.semantic[S](SpaceKey.unsafe("subject:01:native"), volumeSpace)
    val second =
      VolumeDomain.semantic[S](SpaceKey.unsafe("subject:02:native"), volumeSpace)
    val voxelRegion = VoxelRegion.make(volumeSpace, NArray(0, 1)).toOption.get
    val firstRegion = first.region(voxelRegion).toOption.get
    val secondRegion = second.region(voxelRegion).toOption.get

    assert(firstRegion.union(secondRegion).isLeft)

  test("legacy voxel regions and selections are backed by locus semantics"):
    val domain = VolumeDomain.structuralCompatibility(volumeSpace)
    val voxelRegion = VoxelRegion.make(volumeSpace, NArray(3, 1, 1)).toOption.get
    val voxelSelection = VoxelSelection.make(volumeSpace, NArray(3, 1)).toOption.get
    val region = domain.region(voxelRegion).toOption.get
    val selection = domain.selection(voxelSelection).toOption.get

    assertEquals(
      LocusDifferential.region(region).members,
      Set(1, 3)
    )
    assertEquals(selection.ordinals.toVector, Vector(3, 1))
    assertEquals(domain.voxelRegion(region).toOption.get, voxelRegion)
    assertEquals(domain.voxelSelection(selection).toOption.get, voxelSelection)

  test("volume values expose a pure indexed field without copying geometry policy"):
    val domain =
      VolumeDomain.semantic[S](SpaceKey.unsafe("atlas:mni:test"), volumeSpace)
    val volume =
      NeuroVol.fromLinear(NArray(10, 11, 12, 13), volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val even = domain.supportWhere(field)(_ % 2 == 0).toOption.get

    assertEquals(domain.finiteSpace.points.map(field.apply).toVector, Vector(10, 11, 12, 13))
    assertEquals(even.ordinalsInDomainOrder.toVector, Vector(0, 2))

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
      VolumeDomain.semantic[S](SpaceKey.unsafe("subject:01:native"), volumeSpace)
    val wrong = VoxelRegion.make(translated, NArray(0)).toOption.get

    assert(domain.region(wrong).isLeft)

  test("runtime-loaded volume domains retain a fresh path-dependent point type"):
    val loaded =
      SomeVolumeDomain.semantic(SpaceKey.unsafe("runtime:volume"), volumeSpace)
    val domain = loaded.value
    val region = Region.fromOrdinals(domain.finiteSpace, Vector(0, 3)).toOption.get

    assertEquals(
      intValues(domain.voxelRegion(region).toOption.get.linearIndices),
      Vector(0, 3)
    )

  private def intValues(values: NArray[Int]): Vector[Int] =
    Vector.tabulate(values.length)(values.apply)
