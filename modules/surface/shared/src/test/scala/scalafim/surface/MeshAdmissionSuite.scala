package scalafim.surface

import mesh4s.TopologyIssue

class MeshAdmissionSuite extends munit.FunSuite:
  private val coordinates = Array(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0)
  private val faces = Array(0, 1, 2, 0, 2, 3)

  private def issues(vertices: Array[Double], triangles: Array[Int]): Vector[TopologyIssue] =
    TriangleMesh.fromArraysEither(vertices, triangles) match
      case Left(MeshAdmissionError.Topology(audit)) => audit.issues
      case other => fail(s"expected topology refusal, found $other")

  test("strict admission refuses conflicting orientation, duplicate faces and unused vertices"):
    assert(issues(coordinates, Array(0, 1, 2, 0, 3, 2)).exists:
      case TopologyIssue.OrientationConflict(_, _, _, _) => true
      case _ => false
    )
    assert(issues(coordinates.take(9), Array(0, 1, 2, 2, 0, 1)).exists:
      case TopologyIssue.DuplicateFace(_, _) => true
      case _ => false
    )
    assert(issues(coordinates, Array(0, 1, 2)).contains(TopologyIssue.UnusedVertex(3)))

  test("strict admission refuses non-manifold edges and disconnected vertex fans"):
    val extra = coordinates ++ Array(0.0, 0.0, 1.0)
    assert(issues(extra, Array(0, 1, 2, 1, 0, 3, 0, 1, 4)).exists:
      case TopologyIssue.NonManifoldEdge(_, _, _) => true
      case _ => false
    )
    assert(issues(extra, Array(0, 1, 2, 0, 3, 4)).exists:
      case TopologyIssue.NonManifoldVertex(_, _, _) => true
      case _ => false
    )

  test("admitted buffers remain immutable through input mutation and exported copies"):
    val values = coordinates.clone()
    val triangles = faces.clone()
    val mesh = TriangleMesh.fromArrays(values, triangles)
    val hash = mesh.hashCode()
    values(0) = 99.0
    triangles(0) = 3
    mesh.coordinates.toArray(0) = -99.0
    mesh.faceIndices.toArray(0) = 3
    assertEqualsDouble(mesh.coordinates(0), 0.0, 0.0)
    assertEquals(mesh.faceIndices(0), 0)
    assertEquals(mesh.hashCode(), hash)
    assert(mesh.points.space.sameRuntimeOwnerAs(mesh.topology.vertices))

  test("coordinate replacement retains the topology and its vertex owner"):
    val mesh = TriangleMesh.fromArrays(coordinates, faces)
    val moved = mesh.withCoordinates(coordinates.map(_ * 2.0))
    assert(moved.topology eq mesh.topology)
    assert(moved.points.space.sameRuntimeOwnerAs(mesh.topology.vertices))
    assert(mesh.hasSameTopology(moved))
    assert(spatial4s.FrameAlignment.check(mesh.points.frame, moved.points.frame).isLeft)
    intercept[IllegalArgumentException](mesh.withCoordinates(Array(Double.NaN)))
    TriangleMesh.fromArraysEither(coordinates.updated(0, Double.NaN), faces) match
      case Left(MeshAdmissionError.Coordinates(_: mesh4s.geometry.PointFieldError.NonFiniteCoordinate)) => ()
      case other => fail(s"expected typed nonfinite-coordinate refusal, found $other")

  test("raw storage charts claim neither physical units nor alignment between meshes"):
    val mesh = TriangleMesh.fromArrays(coordinates, faces)
    val other = TriangleMesh.fromArrays(coordinates, faces)
    val chart = mesh.points.frame
    assertEquals(chart.unit.scaleToCanonical, None)
    assert(chart.unit.quantity != spatial4s.CoordinateQuantity.Length)
    assert(chart.unit.quantity != spatial4s.CoordinateQuantity.Dimensionless)
    assertEquals(chart.convention, spatial4s.CoordinateConvention.Unspecified)
    assertEquals(chart.persistentKey, None)
    assert(spatial4s.FrameAlignment.check(chart, other.points.frame).isLeft)
    val shared = other.sharingTopology(mesh).toOption.get
    assert(shared.topology eq mesh.topology)
    assert(shared.points.space.sameRuntimeOwnerAs(mesh.topology.vertices))
    assert(spatial4s.FrameAlignment.check(chart, shared.points.frame).isLeft)

  test("white, pial, inflated and sphere share the selected topology owner"):
    def geometry(kind: SurfaceKind, scale: Double): SurfaceGeometry =
      SurfaceGeometry(TriangleMesh.fromArrays(coordinates.map(_ * scale), faces), Hemisphere.Left, kind)
    val white = geometry(SurfaceKind.White, 1.0)
    val pial = geometry(SurfaceKind.Pial, 1.1)
    val set = SurfaceSet.of(SurfaceKind.White, white,
      SurfaceKind.Pial -> pial,
      SurfaceKind.Inflated -> geometry(SurfaceKind.Inflated, 2.0),
      SurfaceKind.Sphere -> geometry(SurfaceKind.Sphere, 3.0))
    set.surfaces.values.foreach: surface =>
      assert(surface.mesh.topology eq white.mesh.topology)
      assert(surface.mesh.points.space.sameRuntimeOwnerAs(white.mesh.topology.vertices))
    val pair = SurfaceGeometryPair(white, pial)
    assert(pair.white.mesh.topology eq pair.pial.mesh.topology)
    val reordered = TriangleMesh.fromArrays(coordinates, Array(0, 2, 3, 0, 1, 2))
    assert(reordered.sharingTopology(white.mesh).isLeft)

  test("mesh4s edge ordinals are the surface weight ordinals"):
    val mesh = TriangleMesh.fromArrays(coordinates, faces)
    val graph = MeshTopology.from(mesh)
    assertEquals(graph.edgeCount, mesh.topology.edges.size)
    graph.edges.zipWithIndex.foreach: (edge, ordinal) =>
      val endpoints = mesh.topology.endpointsOf(mesh.topology.edges.indexAtValidatedOrdinal(ordinal))
      assertEquals(edge, Edge.between(VertexId(endpoints.first.ordinal), VertexId(endpoints.second.ordinal)))
