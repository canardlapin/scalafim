package scalafim.surface.view

import scalafim.image.{DMat, WorldPoint}
import scalafim.surface.*

class SurfaceWorldLinkAffineSuite extends munit.FunSuite:
  private val surface = SurfaceId.unsafe("affine")

  test("world links apply rotation, non-uniform scale, shear, and translation before selection"):
    val transform = DMat.fromRows(Vector(
      Vector(0.0, -2.0, 0.5, 10.0),
      Vector(3.0, 0.25, 0.0, -4.0),
      Vector(0.0, 0.0, 0.5, 2.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val geometry = triangleGeometry(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      ),
      transform
    )
    val expected = Vector(
      WorldPoint(10.0, -1.0, 2.0),
      WorldPoint(8.0, -3.75, 2.0),
      WorldPoint(10.5, -4.0, 2.5)
    )
    expected.indices.foreach: index =>
      val world = SurfaceWorldLink.worldPoint(geometry, VertexId.unsafe(index)).toOption.get
      assertWorldPoint(world, expected(index), 1e-12)
      assertEquals(
        SurfaceWorldLink.nearestVertex(surface, geometry, expected(index), SurfaceLinkRadius.unsafe(0.0)),
        Right(SurfaceSelection(surface, VertexId.unsafe(index)))
      )

  test("world links perform homogeneous division when w is not one"):
    val transform = DMat.fromRows(Vector(
      Vector(2.0, 0.0, 0.0, 4.0),
      Vector(0.0, 2.0, 0.0, 6.0),
      Vector(0.0, 0.0, 2.0, 8.0),
      Vector(0.0, 0.0, 0.0, 2.0)
    ))
    val geometry = triangleGeometry(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      ),
      transform
    )
    assertWorldPoint(
      SurfaceWorldLink.worldPoint(geometry, VertexId.unsafe(0)).toOption.get,
      WorldPoint(3.0, 3.0, 4.0),
      0.0
    )
    assertEquals(
      SurfaceWorldLink.nearestVertex(
        surface,
        geometry,
        WorldPoint(2.0, 4.0, 4.0),
        SurfaceLinkRadius.unsafe(0.0)
      ),
      Right(SurfaceSelection(surface, VertexId.unsafe(1)))
    )

  test("w=0 fails closed for direct and nearest world links"):
    val transform = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0, 0.0),
      Vector(0.0, 0.0, 1.0, 0.0),
      Vector(1.0, 0.0, 0.0, 0.0)
    ))
    val geometry = triangleGeometry(
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0)
      ),
      transform
    )
    val expected = SurfaceViewError.IncompatibleMorph("surface-to-world transform produced w=0")
    assertEquals(SurfaceWorldLink.worldPoint(geometry, VertexId.unsafe(0)), Left(expected))
    assertEquals(
      SurfaceWorldLink.nearestVertex(
        surface,
        geometry,
        WorldPoint(0.0, 0.0, 0.0),
        SurfaceLinkRadius.unsafe(100.0)
      ),
      Left(expected)
    )

  test("nearest-vertex ties prefer the lower ordinal and radius boundaries are inclusive"):
    val geometry = triangleGeometry(
      Vector(
        Vector(-1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 2.0, 0.0)
      ),
      DMat.eye(4)
    )
    val query = WorldPoint(0.0, 0.0, 0.0)
    assertEquals(
      SurfaceWorldLink.nearestVertex(surface, geometry, query, SurfaceLinkRadius.unsafe(1.0)),
      Right(SurfaceSelection(surface, VertexId.unsafe(0)))
    )
    SurfaceWorldLink.nearestVertex(
      surface,
      geometry,
      query,
      SurfaceLinkRadius.unsafe(0.999999)
    ) match
      case Left(SurfaceViewError.LinkDistanceExceeded(distance, maximum)) =>
        assertEqualsDouble(distance, 1.0, 0.0)
        assertEqualsDouble(maximum, 0.999999, 0.0)
      case other => fail(s"expected radius rejection, found $other")

  private def triangleGeometry(vertices: Vector[Vector[Double]], transform: DMat): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(vertices, Vector((0, 1, 2))),
      Hemisphere.Left,
      SurfaceKind.Pial,
      transform
    )

  private def assertWorldPoint(actual: WorldPoint, expected: WorldPoint, tolerance: Double): Unit =
    assertEqualsDouble(actual.x, expected.x, tolerance)
    assertEqualsDouble(actual.y, expected.y, tolerance)
    assertEqualsDouble(actual.z, expected.z, tolerance)
