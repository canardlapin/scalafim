package scalafim.atlas

import ravel.Shape
import scalafim.atlas.fixtures.AtlasParityFixtures
import scalafim.atlas.fixtures.AtlasParityFixtures.OverlapExpected
import scalafim.atlas.syntax.*

class AtlasParityCorpusSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-12): Unit =
    assert(math.abs(actual - expected) <= tol, clues(s"actual=$actual expected=$expected"))

  test("parity fixture preserves non-contiguous atlas ids and metadata") {
    val atlas = AtlasParityFixtures.atlas()
    assertEquals(atlas.regions.ids, Vector(RegionId(10), RegionId(50), RegionId(90)))

    AtlasParityFixtures.regionExpectations.foreach { expected =>
      val region = atlas.region(RegionId(expected.id)).get
      assertEquals(region.label, expected.label)
      assertEquals(region.fullLabel, expected.labelFull)
      assertEquals(region.hemisphere, expected.hemisphere)
      assertEquals(region.network.map(_.value), expected.network)
      assertEquals(
        atlas.realization.region(RegionId(expected.id)).get.cardinality,
        expected.voxelCount
      )
    }
  }

  test("query and parcel reduction match neuroatlas parity fixture") {
    val atlas = AtlasParityFixtures.atlas()

    assertEquals(atlas.query(Point3D(0.0, 0.0, 0.0)).head.id, Some(RegionId(10)))
    assertEquals(atlas.query(Point3D(4.0, 4.0, 4.0)).head.id, Some(RegionId(50)))
    assertEquals(atlas.query(Point3D(2.0, 2.0, 0.0)).head.id, Some(RegionId(90)))
    assertEquals(atlas.query(Point3D(2.0, 2.0, 4.0)).head.region, None)

    val reduced = atlas.reduce(AtlasParityFixtures.dataVolume(atlas))
    AtlasParityFixtures.parcelMeans.foreach { case (id, expected) =>
      assertEquals(reduced.value(RegionId(id)), Some(expected))
    }
  }

  test("parcel-series reduction keeps non-contiguous ids in parcel order") {
    val atlas = AtlasParityFixtures.atlas()
    val series =
      AtlasReduce.reduceSeries(
        atlas,
        AtlasParityFixtures.dataSeries(atlas),
        Some(AtlasParityFixtures.fullMask(atlas))
      )
    val matrix = series.data
    val tLen = series.nTime

    assertEquals(matrix.shape, Shape(3, 3))
    atlas.regions.ids.zipWithIndex.foreach { case (id, parcel) =>
      val actual = Vector.tabulate(tLen)(time => matrix(parcel, time))
      assertEquals(actual, AtlasParityFixtures.vecSeries(id.value))
    }
  }

  test("parcel-series reduction writes NaN when the mask misses the atlas") {
    val atlas = AtlasParityFixtures.atlas()
    val series =
      AtlasReduce.reduceSeries(
        atlas,
        AtlasParityFixtures.dataSeries(atlas),
        Some(AtlasParityFixtures.emptyMask(atlas))
      )

    var parcel = 0
    while parcel < atlas.regions.size do
      var time = 0
      while time < series.nTime do
        assert(
          series.data(parcel, time).isNaN,
          clues(
            s"parcel=$parcel time=$time value=${series.data(parcel, time)}"
          )
        )
        time += 1
      parcel += 1
  }

  test("overlap corpus matches Dice and Jaccard golden values") {
    val overlap =
      AtlasParityFixtures
        .atlas()
        .overlap(
          AtlasParityFixtures.comparisonAtlas(),
          AtlasAlignment.Exact
        )

    assertEquals(
      overlap.map(o => (o.region1.id.value, o.region2.id.value)),
      AtlasParityFixtures.overlapExpectations.map(e => (e.region1, e.region2))
    )

    overlap.zip(AtlasParityFixtures.overlapExpectations).foreach { case (actual, expected: OverlapExpected) =>
      assertClose(actual.dice, expected.dice)
      assertClose(actual.jaccard, expected.jaccard)
      assertEquals(actual.nOverlap, expected.nOverlap)
      assertEquals(actual.nRegion1, expected.nRegion1)
      assertEquals(actual.nRegion2, expected.nRegion2)
    }
  }

  test("fixture rejects duplicate metadata ids and unknown payload labels") {
    interceptMessage[IllegalArgumentException]("requirement failed: atlas region ids must be unique: 10") {
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(10), "A"),
          AtlasRegionMetadata(RegionId(10), "A-duplicate")
        )
      )
    }

    val badLabels = AtlasParityFixtures.labelData()
    badLabels(0) = 999
    interceptMessage[IllegalArgumentException]("atlas payload is missing region id 999") {
      VolumeAtlas.fromLabelVolume(
        AtlasParityFixtures.ref,
        AtlasParityFixtures.regions,
        AtlasTestImages.labelVolume(
          AtlasParityFixtures.atlas().space,
          badLabels,
          "bad-labels"
        )
      )
    }
  }
