package scalafim.surface

import scalafim.surface.fixtures.SurfaceTestFixtures

class MeshTopologySuite extends munit.FunSuite:

  test("MeshTopology derives unique edges and neighbor rows"):
    val topology = SurfaceTestFixtures.tetraTopology

    assertEquals(topology.edgeCount, 6)
    assertEquals(topology.edges.head, Edge.between(VertexId(0), VertexId(1)))
    assertEquals(topology.neighbors(VertexId(0).index), Vector(VertexId(1), VertexId(2), VertexId(3)))
    assertEquals(topology.vertexDegree(VertexId(0)), 3)
    assertEquals(topology.vertexDegree(VertexId(3)), 3)

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

  test("MeshTopology handles open fragmented meshes"):
    val mesh =
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(1.0, 1.0, 0.0),
          Vector(2.0, 1.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (2, 3, 4)
        )
      )

    val topology = MeshTopology.from(mesh)
    assertEquals(topology.edgeCount, 6)
    assertEquals(topology.eulerCharacteristic, 1)
    assertEquals(topology.neighbors(VertexId(2).index), Vector(VertexId(0), VertexId(1), VertexId(3), VertexId(4)))
