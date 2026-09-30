package scalafim.surface.view

import image4s.geometry.{Affine, D3}
import scalafim.image.WorldPoint
import scalafim.surface.*

class SurfaceWorldIndexSuite extends munit.FunSuite:
  private val id = SurfaceId.unsafe("left")
  private val unlimited = SurfaceLinkRadius.unsafe(Double.MaxValue)
  private def geometry(points: Seq[Seq[Double]], affine: Affine[D3] = Affine.identity[D3]) =
    SurfaceGeometry(TriangleMesh.fromRows(points, Seq((0, 1, 2))), Hemisphere.Left, SurfaceKind.Pial, affine)

  test("prepared picks agree with independent world landmarks under full affine transforms"):
    val points = Vector.tabulate(211)(i => Vector(math.sin(i * 0.7) * 8, math.cos(i * 0.3) * 7, (i % 17).toDouble))
    val transforms = Vector(
      Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
      Vector(0.0, -1, 0, 10, 1, 0, 0, 20, 0, 0, 1, 30, 0, 0, 0, 1),
      Vector(2.0, 0.7, 0, 10, 0, 0.5, 0.4, -20, 0.2, 0, 3, 5, 0, 0, 0, 1)
    )
    for matrix <- transforms do
      val g = geometry(points, Affine.fromRowMajor[D3](matrix).toOption.get)
      val index = SurfaceWorldLink.prepare(id, g).toOption.get
      val landmarks = points.map(p => Vector.tabulate(3)(r => matrix(r * 4) * p(0) + matrix(r * 4 + 1) * p(1) + matrix(r * 4 + 2) * p(2) + matrix(r * 4 + 3)))
      for i <- 0 until 101 do
        val q = WorldPoint(math.sin(i * 1.1) * 25, math.cos(i * 1.7) * 30, (i % 31).toDouble)
        val expected = landmarks.indices.minBy(v =>
          val p = landmarks(v)
          math.hypot(math.hypot(p(0) - q.x, p(1) - q.y), p(2) - q.z))
        assertEquals(index.nearestVertex(q, unlimited).toOption.get.vertex.index, expected)
        assertEquals(index.nearestVertex(q, unlimited), SurfaceWorldLink.nearestVertex(id, g, q, unlimited))
      val p = landmarks(77)
      assertEquals(index.nearestVertex(WorldPoint(p(0), p(1), p(2)), SurfaceLinkRadius.unsafe(1e-12)).toOption.get.vertex.index, 77)

  test("ties choose original lowest id and radius boundaries are inclusive"):
    val index = SurfaceWorldLink.prepare(id, geometry(Seq(Seq(-1.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 3.0, 0.0)))).toOption.get
    val query = WorldPoint(0.0, 0.0, 0.0)
    assertEquals(index.nearestVertex(query, SurfaceLinkRadius.unsafe(1.0)), Right(SurfaceSelection(id, VertexId(0))))
    assert(index.nearestVertex(query, SurfaceLinkRadius.unsafe(0.999)).isLeft)

  test("snapshot lifetime is explicit across coordinate mutation and equal-topology replacements"):
    val g = geometry(Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0)))
    val before = SurfaceWorldLink.prepare(id, g).toOption.get
    g.mesh.coordinates(0) = 100.0
    val after = SurfaceWorldLink.prepare(id, g).toOption.get
    val query = WorldPoint(0.0, 0.0, 0.0)
    assertEquals(before.nearestVertex(query, unlimited).toOption.get.vertex.index, 0)
    assertEquals(after.nearestVertex(query, unlimited).toOption.get.vertex.index, 1)
    val replacement = geometry(Seq(Seq(50.0, 0.0, 0.0), Seq(30.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0)))
    assert(g.mesh.hasSameTopology(replacement.mesh))
    assertEquals(SurfaceWorldLink.prepare(id, replacement).toOption.get.nearestVertex(query, unlimited).toOption.get.vertex.index, 2)

  test("repeated coincident points retain deterministic ties"):
    val index = SurfaceWorldLink.prepare(id, geometry(Vector.fill(513)(Vector(2.0, 3.0, 4.0)))).toOption.get
    assertEquals(index.nearestVertex(WorldPoint(2.0, 3.0, 4.0), unlimited).toOption.get.vertex.index, 0)

  test("malformed transforms are refused at the affine boundary and mutated nonfinite coordinates cannot be indexed"):
    assert(Affine.fromRowMajor[D3](Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0.1, 0, 0, 1)).isLeft)
    val g = geometry(Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0)))
    g.mesh.coordinates(0) = Double.NaN
    assert(SurfaceWorldLink.prepare(id, g).isLeft)
