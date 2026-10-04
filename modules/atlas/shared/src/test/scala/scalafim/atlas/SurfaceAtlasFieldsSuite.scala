package scalafim.atlas

import locus4s.data.Field
import scalafim.surface.{CorticalHemisphere, Hemisphere as SurfaceHemisphere, LabelInfo, LabeledSurface, SurfaceGeometry, SurfaceKind, TriangleMesh}

class SurfaceAtlasFieldsSuite extends munit.FunSuite:
  test("bilateral scalar and series reductions retain the realization owner"):
    val atlas = fixture()
    val realization: atlas.realization.type = atlas.realization
    val first = Field.view(realization.parcelAssignment.from)(point => Vector(1.0, 3.0, -1.0, 10.0, 30.0, -1.0)(point.ordinal))
    val second = Field.view(realization.parcelAssignment.from)(point => Vector(2.0, 4.0, -1.0, 20.0, 40.0, -1.0)(point.ordinal))

    val scalar = SurfaceAtlasFields.reduce(atlas, first).fold(error => fail(error.message), identity)
    assertEquals(scalar.toVector, Vector(2.0, 20.0))
    assert(scalar.space.sameRuntimeOwnerAs(realization.parcelDomain))

    val series = SurfaceAtlasFields.reduceSeries(atlas, Vector(first, second)).fold(error => fail(error.message), identity)
    assertEquals(series.map(_.toVector), Vector(Vector(2.0, 20.0), Vector(3.0, 30.0)))

  test("hemisphere selection is exact combined-vertex support and rejects a foreign owner"):
    val atlas = fixture()
    val realization: atlas.realization.type = atlas.realization
    val left = SurfaceAtlasFields.vertices(atlas, CorticalHemisphere.Left)
    val right = SurfaceAtlasFields.vertices(atlas, CorticalHemisphere.Right)
    assertEquals(left.ordinalsInDomainOrder.toVector, Vector(0, 1, 2))
    assertEquals(right.ordinalsInDomainOrder.toVector, Vector(3, 4, 5))

    val foreign = fixture()
    val invalid = Field.view(foreign.realization.parcelAssignment.from)(_ => 1.0)
    SurfaceAtlasFields.reduce(atlas, invalid.asInstanceOf[Field[atlas.realization.X, Double]]) match
      case Left(AtlasReductionError.WrongInputOwner(_)) => ()
      case other => fail(s"expected exact-owner rejection, got $other")

  private def fixture(): SurfaceAtlas =
    val mesh = TriangleMesh.fromRows(
      Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)),
      Vector((0, 1, 2))
    )
    val leftGeometry = SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Midthickness)
    val rightGeometry = SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Midthickness)
    val left = LabeledSurface(leftGeometry, Array(0, 1), Array(1, 1), Vector(LabelInfo(1, "left")))
    val right = LabeledSurface(rightGeometry, Array(0, 1), Array(2, 2), Vector(LabelInfo(2, "right")))
    val ref = AtlasRef.surface("synthetic", "fields", SpaceId.FsAverage6, SpaceId.FsAverage6, confidence = Confidence.Exact)
    val regions = RegionIndex(Vector(
      AtlasRegionMetadata.fromStrings(RegionId(1), "left", hemisphere = Some(Hemisphere.Left)),
      AtlasRegionMetadata.fromStrings(RegionId(2), "right", hemisphere = Some(Hemisphere.Right))
    ))
    SurfaceAtlas.fromLabeledSurfaces(ref, regions, left, right)
