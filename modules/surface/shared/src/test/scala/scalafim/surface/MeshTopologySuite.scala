package scalafim.surface

import scalafim.surface.fixtures.SurfaceTestFixtures

class MeshTopologySuite extends munit.FunSuite:

  test("provider area retains thin triangles and unsigned winding"):
    val points = Array(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1e-10, 0.0)
    Vector(Array(0, 1, 2), Array(0, 2, 1)).foreach: faces =>
      val topology = MeshTopology.from(TriangleMesh.fromArrays(points, faces))
      assertEqualsDouble(topology.faceArea(FaceId(0)), 5e-11, 1e-25)

  test("provider delegation preserves intermediate refusal and norm overflow"):
    val differences = MeshTopology.from(TriangleMesh.fromArrays(
      Array(-1e308, 0, 0, 1e308, 0, 0, 0, 1, 0), Array(0, 1, 2)
    ))
    intercept[IllegalArgumentException](differences.edgeLengths)
    intercept[IllegalArgumentException](differences.faceArea(FaceId(0)))
    val cross = MeshTopology.from(TriangleMesh.fromArrays(
      Array(0.0, 0, 0, 1e200, 0, 0, 0, 1e200, 0), Array(0, 1, 2)
    ))
    intercept[IllegalArgumentException](cross.faceArea(FaceId(0)))
    val norm = MeshTopology.from(TriangleMesh.fromArrays(
      Array(0.0, 0, 0, 1e100, 0, 0, 0, 1e100, 0), Array(0, 1, 2)
    ))
    assert(norm.faceArea(FaceId(0)).isPosInfinity)

  test("public pair length still accepts a chord absent from topology"):
    val topology = MeshTopology.from(TriangleMesh.fromArrays(
      Array(0.0, 0, 0, 1.0, 0, 0, 0, 1.0, 0, 3.0, 4.0, 0), Array(0, 1, 2, 1, 3, 2)
    ))
    val chord = Edge.between(VertexId(0), VertexId(3))
    assert(!topology.edges.contains(chord))
    assertEqualsDouble(topology.edgeLength(chord), 5.0, 1e-12)

  test("MeshTopology derives unique edges and neighbor rows"):
    val topology = SurfaceTestFixtures.tetraTopology

    assertEquals(topology.edgeCount, 6)
    assertEquals(topology.edges.head, Edge.between(VertexId(0), VertexId(1)))
    assertEquals(topology.neighborsOf(VertexId(0)), Vector(VertexId(1), VertexId(2), VertexId(3)))
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
          Vector(2.0, 1.0, 0.0),
          Vector(2.0, 2.0, 0.0)
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

  test("MeshTopology carries a surface domain when built from geometry"):
    val topology = MeshTopology.from(SurfaceTestFixtures.tetraGeometry)

    assertEquals(topology.domainEither, scala.util.Right(SurfaceDomain(CorticalHemisphere.Left, 4)))
    assertEquals(MeshTopology.from(SurfaceTestFixtures.tetraMesh).domainEither.left.map(_.message), scala.util.Left("mesh topology does not carry a usable surface domain"))
