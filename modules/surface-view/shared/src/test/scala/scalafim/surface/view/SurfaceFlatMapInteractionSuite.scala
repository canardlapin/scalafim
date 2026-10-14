package scalafim.surface.view

import scalafim.surface.VertexId

class SurfaceFlatMapInteractionSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(String.valueOf(error)), identity)
  private val surface = SurfaceFaceFixture.Surface
  private val map = ok(SurfaceFlatMap.make(surface,
    Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(1, 0), SurfaceFlatMapPoint(1, 1), SurfaceFlatMapPoint(0, 1)),
    Vector((0, 1, 2), (0, 2, 3))))

  test("state selection preserves the map scientific domain"):
    val model = SurfaceFaceFixture.model
    val foreign = SurfaceFaceFixture.state(model)
    assertEquals(ok(SurfaceFlatMapInteraction.selectedVertex(map, model, foreign)), None)
    val selected = foreign.copy(selection = Some(SurfaceSelection(surface, VertexId(1))))
    assertEquals(ok(SurfaceFlatMapInteraction.selectedVertex(map, model, selected)), Some(VertexId(1)))
    val invalid = foreign.copy(selection = Some(SurfaceSelection(surface, VertexId(4))))
    assert(SurfaceFlatMapInteraction.selectedVertex(map, model, invalid).isLeft)
