package scalafim.surface

import mesh4s.TopologyAudit
import mesh4s.TopologyIssue
import scalafim.surface.fixtures.SurfaceTestFixtures

class MeshTopologySuite extends munit.FunSuite:

  test("MeshTopology derives unique edges and neighbor rows"):
    val topology = SurfaceTestFixtures.tetraTopology

    assertEquals(topology.edgeCount, 6)
    assertEquals(topology.edges.head, Edge.between(VertexId(0), VertexId(1)))
    assertEquals(topology.neighborsOf(VertexId(0)), Vector(VertexId(1), VertexId(2), VertexId(3)))
    assertEquals(topology.vertexDegree(VertexId(0)), 3)
    assertEquals(topology.vertexDegree(VertexId(3)), 3)
    assert(topology.isClosed)
    assert(topology.isConnected)
    assertEquals(topology.boundaryLoopCount, 0)
    assertEquals(topology.componentCount, 1)

  test("MeshTopology computes Euclidean edge lengths"):
    val topology = SurfaceTestFixtures.tetraTopology
    val byEdge = topology.edges.zip(topology.edgeLengths).toMap

    assertEqualsDouble(byEdge(Edge.between(VertexId(0), VertexId(1))), 1.0, 1e-12)
    assertEqualsDouble(byEdge(Edge.between(VertexId(1), VertexId(2))), math.sqrt(2.0), 1e-12)

  test("MeshTopology computes face and mesh metrics"):
    val topology = SurfaceTestFixtures.tetraTopology

    assertEqualsDouble(topology.faceArea(FaceId(0)), 0.5, 1e-12)
    assertEqualsDouble(topology.surfaceArea, 1.5 + math.sqrt(3.0) / 2.0, 1e-12)
    assertEquals(topology.eulerCharacteristic, 2)

  test("MeshTopology computes unit vertex normals when possible"):
    val topology = SurfaceTestFixtures.tetraTopology
    val normals = topology.vertexNormals

    assertEquals(normals.length, 4)
    normals.foreach { normal =>
      assertEqualsDouble(normal.norm, 1.0, 1e-12)
    }

  test("MeshTopology handles lawful disconnected open components"):
    val mesh =
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(10.0, 0.0, 0.0),
          Vector(11.0, 0.0, 0.0),
          Vector(10.0, 1.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (3, 4, 5)
        )
      )

    val topology = MeshTopology.from(mesh)
    assertEquals(topology.edgeCount, 6)
    assertEquals(topology.eulerCharacteristic, 2)
    assertEquals(topology.neighborsOf(VertexId(2)), Vector(VertexId(0), VertexId(1)))
    assertEquals(topology.componentCount, 2)
    assertEquals(topology.boundaryLoopCount, 2)
    assert(!topology.isConnected)
    assert(!topology.isClosed)

  test("strict construction rejects duplicate faces with a typed witness"):
    val audit =
      rejectedAudit(
        SurfaceTestFixtures.tetraVertices,
        Vector((0, 1, 2), (0, 1, 2))
      )

    assert(
      audit.issues.exists:
        case TopologyIssue.DuplicateFace(0, 1) => true
        case _                                 => false
    )

  test("strict construction rejects nonmanifold edges with a typed witness"):
    val audit =
      rejectedAudit(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, -1.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        ),
        Vector((0, 1, 2), (1, 0, 3), (0, 1, 4))
      )

    assert(
      audit.issues.exists:
        case TopologyIssue.NonManifoldEdge(0, 1, faces) => faces.length == 3
        case _                                          => false
    )

  test("strict construction rejects a vertex with disjoint fans"):
    val audit =
      rejectedAudit(
        Vector.tabulate(9): vertex =>
          Vector(vertex.toDouble, (vertex % 3).toDouble, 0.0),
        Vector((0, 1, 5), (0, 2, 6), (0, 3, 7), (0, 4, 8))
      )

    assert(
      audit.issues.exists:
        case TopologyIssue.NonManifoldVertex(0, fanCount, _) => fanCount == 4
        case _                                                => false
    )

  test("strict construction rejects unused vertices"):
    val audit =
      rejectedAudit(
        SurfaceTestFixtures.tetraVertices,
        Vector((0, 1, 2))
      )

    assert(
      audit.issues.exists:
        case TopologyIssue.UnusedVertex(3) => true
        case _                             => false
    )

  test("MeshTopology carries a surface domain when built from geometry"):
    val topology = MeshTopology.from(SurfaceTestFixtures.tetraGeometry)

    assertEquals(topology.domainEither, scala.util.Right(SurfaceDomain(CorticalHemisphere.Left, 4)))
    assertEquals(MeshTopology.from(SurfaceTestFixtures.tetraMesh).domainEither.left.map(_.message), scala.util.Left("mesh topology does not carry a usable surface domain"))

  private def rejectedAudit(
    vertices: Seq[Seq[Double]],
    faces: Seq[(Int, Int, Int)]
  ): TopologyAudit =
    TriangleMesh.fromRowsEither(vertices, faces) match
      case Left(TriangleMeshError.TopologyRejected(audit)) => audit
      case other => fail(s"expected a topology audit, found $other")
