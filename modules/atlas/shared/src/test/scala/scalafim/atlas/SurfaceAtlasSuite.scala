package scalafim.atlas

import scalafim.surface.{
  Hemisphere as SurfaceHemisphere,
  LabelInfo,
  LabeledSurface,
  SurfaceGeometry,
  SurfaceKind,
  TriangleMesh,
  VertexId
}

class SurfaceAtlasSuite extends munit.FunSuite:

  private def regions: RegionIndex =
    RegionIndex(
      Vector(
        AtlasRegionMetadata(RegionId(1), "L_A", hemisphere = Some(Hemisphere.Left)),
        AtlasRegionMetadata(RegionId(2), "L_B", hemisphere = Some(Hemisphere.Left)),
        AtlasRegionMetadata(RegionId(3), "R_A", hemisphere = Some(Hemisphere.Right)),
        AtlasRegionMetadata(RegionId(4), "R_B", hemisphere = Some(Hemisphere.Right))
      )
    )

  private def ref: SurfaceAtlasRef =
    AtlasRef.surface(
      family = "toy",
      model = "ToySurface",
      templateSpace = SpaceId.FsAverage6,
      coordSpace = SpaceId.FsAverage6,
      density = Some("41k"),
      confidence = Confidence.Exact
    )

  private def rightGeometry: SurfaceGeometry =
    SurfaceGeometry(
      mesh = sheetMesh,
      hemisphere = SurfaceHemisphere.Right,
      kind = SurfaceKind.Pial
    )

  private def sheetMesh: TriangleMesh =
    TriangleMesh.fromRows(
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0)
      ),
      Vector(
        (0, 1, 2),
        (1, 3, 2)
      )
    )

  private def leftGeometry: SurfaceGeometry =
    SurfaceGeometry(
      mesh = sheetMesh,
      hemisphere = SurfaceHemisphere.Left,
      kind = SurfaceKind.Pial
    )

  private def leftLabels: LabeledSurface =
    LabeledSurface.fromIndexed(
      leftGeometry,
      Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
      Vector(1, 1, 2, 2),
      Vector(LabelInfo(1, "L_A"), LabelInfo(2, "L_B"))
    )

  private def atlas(): SurfaceAtlas =
    val right =
      LabeledSurface.fromIndexed(
        rightGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(3, 3, 4, 4),
        Vector(LabelInfo(3, "R_A"), LabelInfo(4, "R_B"))
      )
    SurfaceAtlas.fromLabeledSurfaces(ref, regions, leftLabels, right)

  test("SurfaceAtlas wraps bilateral labeled surfaces with typed region lookup"):
    val a = atlas()

    assertEquals(a.representation, AtlasRepresentation.Surface)
    assertEquals(a.vertexCount(SurfaceHemisphere.Left), 4)
    assertEquals(a.vertexCount(SurfaceHemisphere.Right), 4)
    assertEquals(a.labelIdAt(SurfaceHemisphere.Left, VertexId(0)), Some(RegionId(1)))
    assertEquals(a.regionAt(SurfaceHemisphere.Right, VertexId(3)).map(_.label), Some("R_B"))
    assertEquals(a.region("L_A", Some(Hemisphere.Left)).map(_.id), Vector(RegionId(1)))
    assertEquals(a.provenance.labels.encoding, LabelEncoding.SurfaceIntegerLabels)

  test("SurfaceAtlas validates geometry hemispheres"):
    val rightLabels =
      LabeledSurface.fromIndexed(
        leftGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(3, 3, 4, 4),
        Vector(LabelInfo(3, "R_A"), LabelInfo(4, "R_B"))
      )

    interceptMessage[IllegalArgumentException]("requirement failed: right surface atlas payload must use right hemisphere geometry"):
      SurfaceAtlas.fromLabeledSurfaces(ref, regions, leftLabels, rightLabels)

  test("SurfaceAtlas requires non-zero payload labels and region ids to match"):
    val right =
      LabeledSurface.fromIndexed(
        rightGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(3, 3, 99, 99),
        Vector(LabelInfo(3, "R_A"), LabelInfo(99, "unknown"))
      )

    val err =
      intercept[IllegalArgumentException]:
        SurfaceAtlas.fromLabeledSurfaces(ref, regions, leftLabels, right)

    assert(err.getMessage.contains("surface payload labels must match region ids"), clue = err.getMessage)

  test("SurfaceAtlas rejects unused label-table ids"):
    val right =
      LabeledSurface.fromIndexed(
        rightGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(3, 3, 4, 4),
        Vector(LabelInfo(3, "R_A"), LabelInfo(4, "R_B"), LabelInfo(99, "unused"))
      )

    val err =
      intercept[IllegalArgumentException]:
        SurfaceAtlas.fromLabeledSurfaces(ref, regions, leftLabels, right)

    assert(err.getMessage.contains("surface label tables contain ids not present in regions: 99"), clue = err.getMessage)

  test("SurfaceAtlas requires lateral hemisphere access and exposes label metadata"):
    val a = atlas()

    val err =
      intercept[IllegalArgumentException]:
        a.surface(SurfaceHemisphere.Both)

    assert(err.getMessage.contains("surface atlas requires left or right hemisphere, got both"), clue = err.getMessage)
    assertEquals(a.labelInfo(SurfaceHemisphere.Left, RegionId(1)).map(_.name), Some("L_A"))
    assertEquals(a.labelInfo(SurfaceHemisphere.Right, RegionId(1)), None)

  test("SurfaceAtlas exposes parcel units, distances, and boundary contacts per hemisphere"):
    val a = atlas()
    val parcels = a.parcelUnits(SurfaceHemisphere.Left)
    val contacts = a.boundaryContacts(SurfaceHemisphere.Left)
    val distances = a.distanceMatrix(SurfaceHemisphere.Left)

    assertEquals(parcels.map(_._1.id), Vector(RegionId(1), RegionId(2)))
    assertEquals(parcels.map(_._2.size), Vector(2, 2))
    assertEquals(contacts.count(0, 1), 3)
    assertEquals(distances.size, 2)
