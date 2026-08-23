package scalafim.image

import VolumeDomain.*
import image4s.locus.GridDomainError
import image4s.locus.GridDomainLayout
import locus4s.DomainRegistry
import ravel.CanonicalArray
import ravel.CanonicalArray.*
import ravel.DType.given
import ravel.NDArray

class VolumeDomainSuite extends munit.FunSuite:
  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(2, 3, 5)))
  private val seriesSpace =
    volumeSpace.toNeuroSpace.addDim(7, Some(Axis.Time))

  private val volumeData =
    NDArray.tabulate[Double](2, 3, 5): (x, y, z) =>
      100.0 * x + 10.0 * y + z

  private val seriesData =
    NDArray.tabulate[Double](2, 3, 5, 7): (x, y, z, time) =>
      1000.0 * x + 100.0 * y + 10.0 * z + time

  private val volume =
    NeuroVol.fromRavel(volumeData, volumeSpace.toNeuroSpace, "oracle-volume")
  private val series =
    NeuroVec.fromRavel(seriesData, seriesSpace, "oracle-series")
  private val registered =
    right(
      VolumeDomain.register(
        volumeSpace,
        "2x3x5 oracle voxels",
        DomainRegistry.empty
      )
    )
  private type Voxel = registered.S
  private val domain: VolumeDomain[Voxel] = registered.value

  test("2x3x5x7 uses one canonical Ravel and GridDomain order"):
    val spatialField = right(domain.spatialField(volume.sampled))
    val seriesField = right(domain.seriesField(series.sampled))
    val canonicalVolume = right(CanonicalArray.from(volume.values))
    val canonicalSeries = right(CanonicalArray.from(series.values))

    assertEquals(
      domain.layout,
      GridDomainLayout.RowMajorLastAxisFastestV1
    )
    assert(spatialField.sourceData.eq(volume.values))
    assert(seriesField.sourceData.eq(series.values))

    var x = 0
    while x < 2 do
      var y = 0
      while y < 3 do
        var z = 0
        while z < 5 do
          val voxelOrdinal = (x * 3 + y) * 5 + z
          val lattice = right(domain.indexOfOrdinal(voxelOrdinal))
          val index = right(domain.space.index(voxelOrdinal))
          val expectedVolume = 100.0 * x + 10.0 * y + z

          assertEquals(lattice.values, Vector(x, y, z))
          assertEquals(right(domain.ordinalOf(lattice)), voxelOrdinal)
          assertEqualsDouble(
            canonicalVolume.readLinear(voxelOrdinal),
            expectedVolume,
            0.0,
            clue = ""
          )
          assertEqualsDouble(
            spatialField(index),
            expectedVolume,
            0.0,
            clue = ""
          )

          var time = 0
          while time < 7 do
            val sampleOrdinal = voxelOrdinal * 7 + time
            val expectedSeries =
              1000.0 * x + 100.0 * y + 10.0 * z + time
            assertEqualsDouble(
              canonicalSeries.readLinear(sampleOrdinal),
              expectedSeries,
              0.0,
              clue = ""
            )
            assertEqualsDouble(
              right(seriesField(index).valueAt(Vector(time))),
              expectedSeries,
              0.0,
              clue = ""
            )
            time += 1
          z += 1
        y += 1
      x += 1

  test("same persistent grid key does not admit a foreign live owner"):
    val foreignSpace =
      VolumeSpace(NeuroSpace(Vector(2, 3, 5)))
    val foreignVolume =
      NeuroVol.fromRavel(
        volumeData,
        foreignSpace.toNeuroSpace,
        "foreign-owner"
      )

    assert(
      domain.grid.samePersistentKeyAs(foreignSpace.sampleSpace.grid)
    )
    assert(
      domain.validateGrid(foreignSpace.sampleSpace.grid) match
        case Left(_: GridDomainError.GridRuntimeOwnerMismatch) => true
        case _                                                  => false
    )
    assert(
      domain.fieldOf(foreignVolume) match
        case Left(_: GridDomainError.ImageGridRuntimeOwnerMismatch) =>
          true
        case _ => false
    )

  test("versioned domain evidence restores through the same registry"):
    val restored =
      right(
        VolumeDomain.restore(
          domain.record,
          volumeSpace,
          registered.registry
        )
      )

    assertEquals(restored.value.record, domain.record)
    assert(
      restored.value.space.sameRuntimeOwnerAs(domain.space)
    )

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
