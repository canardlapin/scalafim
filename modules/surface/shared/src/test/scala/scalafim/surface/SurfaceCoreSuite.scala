package scalafim.surface

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.image.SpatialPoint
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

  test("mesh topology identity ignores coordinates but preserves exact face ordering"):
    val mesh = SurfaceTestFixtures.tetraMesh
    val moved =
      TriangleMesh.fromRows(
        SurfaceTestFixtures.tetraVertices.map(_.map(_ + 10.0)),
        SurfaceTestFixtures.tetraFaces
      )
    val reordered =
      TriangleMesh.fromRows(
        SurfaceTestFixtures.tetraVertices,
        SurfaceTestFixtures.tetraFaces.reverse
      )
    val rewound =
      TriangleMesh.fromRows(
        SurfaceTestFixtures.tetraVertices,
        SurfaceTestFixtures.tetraFaces.updated(0, (0, 2, 1))
      )

    assertEquals(mesh.topologyIdentity, moved.topologyIdentity)
    assert(mesh.hasSameTopology(moved))
    assertNotEquals(mesh.topologyIdentity, reordered.topologyIdentity)
    assert(!mesh.hasSameTopology(reordered))
    assertNotEquals(mesh.topologyIdentity, rewound.topologyIdentity)
    assert(!mesh.hasSameTopology(rewound))
    assertEquals(mesh.topologyIdentity.stableKey.length, 16)

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
      Affine.fromRowMajor[D3](
        Vector(
          1.0, 0.0, 0.0, 10.0,
          0.0, 1.0, 0.0, 20.0,
          0.0, 0.0, 1.0, 30.0,
          0.0, 0.0, 0.0, 1.0
        )
      ).toOption.get
    val geom = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial, transform)

    assertEquals(geom.vertexCount, 4)
    assertEquals(geom.faceCount, 4)
    assertEquals(geom.hemisphere, Hemisphere.Left)
    assertEquals(geom.label, "pial")
    assertEquals(geom.surfaceToWorld, transform)
    assertEquals(geom.domainEither, scala.util.Right(SurfaceDomain(CorticalHemisphere.Left, 4)))
    val meshDomain = geom.meshDomainEither.toOption.get
    assertEquals(meshDomain.hemisphere, CorticalHemisphere.Left)
    assertEquals(meshDomain.vertexCount, 4)
    assertEquals(meshDomain.faceCount, 4)
    assertEquals(meshDomain.topology, mesh.topologyIdentity)

  test("SurfaceGeometry accepts only provider-validated D3 affines"):
    val malformed = Affine.fromRowMajor[D3](Vector.fill(9)(0.0))

    assert(malformed.isLeft)
    assertEquals(
      SurfaceGeometry(SurfaceTestFixtures.tetraMesh).surfaceToWorld,
      Affine.identity[D3]
    )

  test("surface domains reject IO-only hemisphere tags"):
    val mesh = SurfaceTestFixtures.tetraMesh
    val unknown = SurfaceGeometry(mesh, Hemisphere.Unknown, SurfaceKind.Pial)
    val both = SurfaceGeometry(mesh, Hemisphere.Both, SurfaceKind.Pial)

    assertEquals(unknown.domainEither.left.map(_.message), scala.util.Left("surface hemisphere unknown is an IO tag, not a usable cortical hemisphere"))
    assertEquals(both.domainEither.left.map(_.message), scala.util.Left("surface hemisphere both is an IO tag, not a usable cortical hemisphere"))
    assertEquals(CorticalHemisphere.fromString("rh"), CorticalHemisphere.Right)
    assert(CorticalHemisphere.fromStringEither("both").isLeft)

  test("surface tag parsers normalize common labels"):
    assertEquals(Hemisphere.fromString("lh"), Hemisphere.Left)
    assertEquals(Hemisphere.fromString("right"), Hemisphere.Right)
    assertEquals(SurfaceKind.fromString("smooth_wm"), SurfaceKind.SmoothWm)
    assertEquals(SurfaceKind.fromString("customLabel"), SurfaceKind.Custom("customLabel"))
