package scalafim.surface

import mesh4s.TopologyIssue
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
    assertEquals(mesh.topology.vertices.size, 4)
    assertEquals(mesh.topology.faces.size, 4)
    assertEquals(mesh.orientationReceipt, TriangleMeshOrientationReceipt.Strict)
    assert(mesh.realization.topology eq mesh.topology)
    assertEquals(mesh.nativeFrame.persistentId, None)
    val realized =
      mesh.realization.position(mesh.topology.vertices.indexAtValidatedOrdinal(2))
    assertEquals(realized.coordinate(0), Some(0.0))
    assertEquals(realized.coordinate(1), Some(1.0))
    assertEquals(realized.coordinate(2), Some(0.0))

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
        SurfaceTestFixtures.tetraFaces.map: (first, second, third) =>
          (first, third, second)
      )

    assertEquals(mesh.topologyIdentity, moved.topologyIdentity)
    assertEquals(mesh.connectivityFingerprint, moved.connectivityFingerprint)
    assertEquals(mesh.connectivityFingerprint.value.length, 64)
    assert(mesh.hasSameTopology(moved))
    assertNotEquals(mesh.topologyIdentity, reordered.topologyIdentity)
    assertNotEquals(mesh.connectivityFingerprint, reordered.connectivityFingerprint)
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

    val repeatedVertex = intercept[IllegalArgumentException]:
      TriangleMesh.fromRows(SurfaceTestFixtures.tetraVertices, Vector((0, 1, 1)))
    assert(repeatedVertex.getMessage.contains("face 0 repeats vertex 1"))

    interceptMessage[IllegalArgumentException]("requirement failed: vertex coordinates must be finite"):
      TriangleMesh.fromRows(Vector(Vector(Double.NaN, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2)))

  test("strict topology rejection retains the mesh4s audit witness"):
    val inconsistent =
      Vector(
        (0, 1, 2),
        (0, 1, 3),
        (0, 2, 3),
        (1, 2, 3)
      )

    TriangleMesh.fromRowsEither(SurfaceTestFixtures.tetraVertices, inconsistent) match
      case Left(TriangleMeshError.TopologyRejected(audit)) =>
        assert(
          audit.issues.exists:
            case TopologyIssue.OrientationConflict(_, _, _, _) => true
            case _                                             => false
        )
      case other =>
        fail(s"expected an orientation audit, found $other")

  test("orientation repair is explicit and retains flipped face ids"):
    val inconsistent =
      Vector(
        (0, 1, 2),
        (0, 1, 3),
        (0, 2, 3),
        (1, 2, 3)
      )
    val mesh =
      TriangleMesh
        .fromRowsEither(
          SurfaceTestFixtures.tetraVertices,
          inconsistent,
          TriangleMeshOrientationPolicy.Orient
        )
        .toOption
        .get

    mesh.orientationReceipt match
      case TriangleMeshOrientationReceipt.Oriented(flippedFaces) =>
        assert(flippedFaces.nonEmpty)
        assert(flippedFaces.forall(_.index < mesh.faceCount))
      case TriangleMeshOrientationReceipt.Strict =>
        fail("explicit orientation must retain an oriented-build receipt")
    assert(mesh.topology.isClosed)
    assertEquals(mesh.topology.eulerCharacteristic, 2)

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
    assertEquals(geom.domainEither, scala.util.Right(SurfaceDomain(CorticalHemisphere.Left, 4)))
    val meshDomain = geom.meshDomainEither.toOption.get
    assertEquals(meshDomain.hemisphere, CorticalHemisphere.Left)
    assertEquals(meshDomain.vertexCount, 4)
    assertEquals(meshDomain.faceCount, 4)
    assertEquals(meshDomain.topology, mesh.topologyIdentity)

  test("SurfaceGeometry rejects non-4x4 transforms"):
    val mesh = SurfaceTestFixtures.tetraMesh
    val bad = DMat.eye(3)

    interceptMessage[IllegalArgumentException]("requirement failed: surfaceToWorld must be 4x4"):
      SurfaceGeometry(mesh, surfaceToWorld = bad)

    assert(SurfaceGeometry.readEither(mesh, surfaceToWorld = bad).isLeft)

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
