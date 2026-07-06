package scalafim.surface

import scalafim.image.{DMat, SpatialPoint}
import scalafim.surface.fixtures.SurfaceTestFixtures

class SurfaceCoreSuite extends munit.FunSuite:

  test("Point3D is a surface alias for image SpatialPoint"):
    val point: SpatialPoint = Point3D(1.0, 2.0, 3.0)
    val surfacePoint: Point3D = SpatialPoint(1.0, 2.0, 3.0)

    assertEquals(point, surfacePoint)
    assertEquals(Point3D.fromVector(Vector(1.0, 2.0, 3.0)), point)

  test("TriangleMesh builds a valid tetrahedron"):
    val mesh = SurfaceTestFixtures.tetraMesh

    assertEquals(mesh.vertexCount, 4)
    assertEquals(mesh.faceCount, 4)
    assertEquals(mesh.vertex(VertexId(2)), Point3D(0.0, 1.0, 0.0))
    assertEquals(mesh.face(FaceId(0)), Triangle(VertexId(0), VertexId(1), VertexId(2)))

  test("TriangleMesh rejects invalid shapes and indices"):
    interceptMessage[IllegalArgumentException]("requirement failed: vertex rows must have exactly 3 coordinates"):
      TriangleMesh.fromRows(Vector(Vector(0.0, 0.0)), SurfaceTestFixtures.tetraFaces)

    interceptMessage[IllegalArgumentException]("requirement failed: face indices must be non-negative"):
      TriangleMesh.fromRows(SurfaceTestFixtures.tetraVertices, Vector((0, -1, 2)))

    interceptMessage[IllegalArgumentException]("requirement failed: face indices out of range"):
      TriangleMesh.fromRows(SurfaceTestFixtures.tetraVertices, Vector((0, 1, 4)))

    interceptMessage[IllegalArgumentException]("requirement failed: triangle faces must reference three distinct vertices"):
      TriangleMesh.fromRows(SurfaceTestFixtures.tetraVertices, Vector((0, 1, 1)))

    interceptMessage[IllegalArgumentException]("requirement failed: vertex coordinates must be finite"):
      TriangleMesh.fromRows(Vector(Vector(Double.NaN, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2)))

  test("SurfaceGeometry stores mesh metadata and a 4x4 surface-to-world transform"):
    val mesh = SurfaceTestFixtures.tetraMesh
    val transform =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 10.0),
          Vector(0.0, 1.0, 0.0, 20.0),
          Vector(0.0, 0.0, 1.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val geom = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial, transform)

    assertEquals(geom.vertexCount, 4)
    assertEquals(geom.faceCount, 4)
    assertEquals(geom.hemisphere, Hemisphere.Left)
    assertEquals(geom.label, "pial")
    assertEquals(geom.surfaceToWorld, transform)

  test("SurfaceGeometry rejects non-4x4 transforms"):
    val mesh = SurfaceTestFixtures.tetraMesh
    val bad = DMat.eye(3)

    interceptMessage[IllegalArgumentException]("requirement failed: surfaceToWorld must be 4x4"):
      SurfaceGeometry(mesh, surfaceToWorld = bad)

  test("surface tag parsers normalize common labels"):
    assertEquals(Hemisphere.fromString("lh"), Hemisphere.Left)
    assertEquals(Hemisphere.fromString("right"), Hemisphere.Right)
    assertEquals(SurfaceKind.fromString("smooth_wm"), SurfaceKind.SmoothWm)
    assertEquals(SurfaceKind.fromString("customLabel"), SurfaceKind.Custom("customLabel"))
