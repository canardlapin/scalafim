package scalafim.examples.atlas

import scalafim.atlas.*

class AtlasExamplesSmokeSuite extends munit.FunSuite:
  test("standard descriptor example lists volume and surface atlases") {
    val rows = StandardAtlasDescriptions.rows
    assert(rows.exists(row => row.id == Schaefer2018.default.id && row.representation == AtlasRepresentation.Volume))
    assert(rows.exists(row => row.id == GlasserHcpMmp1Surface.default.id && row.representation == AtlasRepresentation.Surface))
  }

  test("toy atlas query returns typed region metadata") {
    val row = QueryAtlas.exactToyQuery(Point3D(0.0, 0.0, 0.0))
    assertEquals(row.regionId, Some(RegionId(1)))
    assertEquals(row.label, Some("Visual"))
  }

  test("toy parcel reduction preserves region ids and means") {
    val rows = ReduceByParcel.toyParcelMeans()
    assertEquals(rows.map(row => row.regionId.value), Vector(1, 2, 3))
    assertEquals(rows.map(_.value), Vector(10.0, 20.0, 30.0))
  }

  test("atlas-to-MVPA example preserves atlas labels and feature counts") {
    val plan = AtlasToMvpaRegionPlans.toyRegionPlan()
    assertEquals(plan.name, "toy-atlas-regions")
    assertEquals(plan.size, 3)
    assertEquals(
      AtlasToMvpaRegionPlans.toyRegionRows().map(row => (row.regionId, row.label, row.nFeatures)),
      Vector(
        (1, Some("Visual"), 4),
        (2, Some("Somatomotor"), 4),
        (3, Some("Default"), 4)
      )
    )
  }

  test("loader summary works for already constructed atlases") {
    val summary = LoadAtlasFromPaths.summarize(AtlasExampleData.atlas())
    assertEquals(summary.name, "example:ToyAtlas")
    assertEquals(summary.nRegions, 3)
    assertEquals(summary.spatialDims, Vector(4, 4, 2))
  }
