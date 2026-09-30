package scalafim.surface

class TriangleMeshEqualitySuite extends munit.FunSuite:

  private def mesh(z: Double, faces: Vector[(Int, Int, Int)] = Vector((0, 1, 2))): TriangleMesh =
    TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, z), Vector(1.0, 0.0, z), Vector(0.0, 1.0, z)), faces)

  test("meshes built from separate arrays with the same content are equal and hash alike"):
    val a = mesh(0.5)
    val b = mesh(0.5)
    assert(a ne b)
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)

  test("coordinates, face order and signed zero distinguish meshes"):
    assertNotEquals(mesh(0.5), mesh(0.25))
    assertNotEquals(mesh(0.0, Vector((0, 1, 2))), mesh(0.0, Vector((0, 2, 1))))
    assertNotEquals(mesh(0.0), mesh(-0.0))

  test("surface geometries compare their meshes structurally"):
    val a = SurfaceGeometry(mesh(1.0), Hemisphere.Left, SurfaceKind.White)
    val b = SurfaceGeometry(mesh(1.0), Hemisphere.Left, SurfaceKind.White)
    assertEquals(a, b)
    assertNotEquals(a, SurfaceGeometry(mesh(1.0), Hemisphere.Right, SurfaceKind.White))

  test("a vertex mapping accepts a field on a separately loaded, identical geometry"):
    val source = SurfaceGeometry(mesh(1.0), Hemisphere.Left, SurfaceKind.White)
    val reloaded = SurfaceGeometry(mesh(1.0), Hemisphere.Left, SurfaceKind.White)
    val mapping = SurfaceVertexMapping.nearestIndex(source, source, Vector(VertexId(2), VertexId(1), VertexId(0)))
    val field = SurfaceField.full(reloaded, Vector(1.0, 2.0, 3.0), "reloaded")
    assertEqualsDouble(mapping(field).valueAt(VertexId(0)).get, 3.0, 0.0)
