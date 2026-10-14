package scalafim.surface.view

import scalafim.surface.VertexId

class SurfaceFlatMapSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(String.valueOf(error)), identity)
  private val surface = SurfaceId.unsafe("flat")
  private val map = ok(SurfaceFlatMap.make(surface,
    Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(2, 0), SurfaceFlatMapPoint(2, 1), SurfaceFlatMapPoint(0, 1)),
    Vector((0, 1, 2), (0, 2, 3))))
  private val contain = ok(SurfaceFlatMapViewport.make(100, 100, 10, SurfaceFlatMapFit.Contain))

  test("contain preserves aspect, flips Y, and unprojects consistently"):
    val lower = ok(map.project(VertexId(0), contain))
    val upper = ok(map.project(VertexId(2), contain))
    assertEqualsDouble(lower.x, 10, 1e-9)
    assertEqualsDouble(lower.y, 70, 1e-9)
    assertEqualsDouble(upper.y, 30, 1e-9)
    assertEquals(map.unproject(lower, contain), SurfaceFlatMapPoint(0, 0))

  test("picking is deterministic and polygons include their closed boundary"):
    val point = ok(map.project(VertexId(0), contain))
    assertEquals(ok(map.closestVertex(point, contain, 2)).map(_.value), Some(VertexId(0)))
    assert(map.triangleAt(ok(map.project(VertexId(2), contain)), contain).contains(SurfaceFlatMapPick.Triangle(0, 0, 0, 1)))
    val roi = ok(map.verticesInPolygon(Vector(SurfaceFlatMapPoint(10, 70), SurfaceFlatMapPoint(90, 70), SurfaceFlatMapPoint(10, 30)), contain))
    assert(roi.contains(VertexId(0)))
    assert(roi.contains(VertexId(1)))
    assert(roi.contains(VertexId(3)))

  test("topology and ROI identities stay in the scientific vertex domain"):
    assert(SurfaceFlatMap.make(surface, Vector(SurfaceFlatMapPoint(0, 0)), Vector((0, 1, 0))).isLeft)
    assert(SurfaceFlatMapRoi.make("roi", map, Vector(VertexId(99)), Vector.empty).isLeft)
