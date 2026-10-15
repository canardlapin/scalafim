package scalafim.surface.view

import scalafim.surface.VertexId

class SurfaceFlatMapQualificationFixtureSuite extends munit.FunSuite:
  private val prepared = SurfaceFlatMapQualificationFixture.decode.fold(fail(_), identity)

  test("C11 frozen square, contain viewport, and reference-defined style are explicit"):
    assertEquals(prepared.source.width, 600)
    assertEquals(prepared.source.height, 450)
    assertEquals(prepared.map.triangles.map((a, b, c) => (a.index, b.index, c.index)), Vector((0, 1, 2), (1, 3, 2)))
    assertEquals(prepared.viewport.fit, SurfaceFlatMapFit.Contain)
    assertEquals(prepared.style, SurfaceFlatMapQualificationFixture.Style("#050505", "#d9d9d9", "#888888", "#58c4ff", "#ffd166", "#ffd166", 4))
    assertEquals(prepared.map.project(VertexId(0), prepared.viewport), Right(SurfaceFlatMapPoint(95, 430)))

  test("C11 actions retain selected identity and advance the two-frame source"):
    val selected = prepared.selected.fold(fail(_), identity)
    assertEquals(selected.selection.map(_.vertex), Some(VertexId(0)))
    val timed = prepared.atTimeOne.fold(fail(_), identity)
    assertEquals(timed.timepoint, 1)
    val marker = prepared.linkedMarker.fold(fail(_), identity).head match
      case value: SurfaceMarkerDecoration => value
      case other => fail(s"unexpected C11 decoration: $other")
    assertEquals(marker.id.value, "c11-linked-marker")
    assertEquals(marker.anchor, SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(prepared.surface, VertexId(0))))

  test("C11 map-space ROI inputs preserve the frozen closed-boundary policy"):
    val polygon = SurfaceFlatMapQualificationFixture.Polygon.map(prepared.map.projectMapPoint(_, prepared.viewport))
    assertEquals(prepared.map.verticesInPolygon(polygon, prepared.viewport), Right(Vector(VertexId(0))))
    assertEquals(SurfaceFlatMapQualificationFixture.Lasso.last, SurfaceFlatMapQualificationFixture.Lasso.head)
