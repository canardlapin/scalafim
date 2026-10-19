package scalafim.surface

class SurfaceIdentityGoldenSuite extends munit.FunSuite:
  // Independently calculated in docs/verification/topology-identity-20261019/reference.py.
  private val vertices = Vector(
    Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0),
    Vector(1.0, 1.0, 0.0), Vector(0.0, 1.0, 0.0)
  )
  private val faces = Vector((0, 1, 2), (0, 2, 3))

  test("legacy topology key retains exact face order and signed decimal representation"):
    val mesh = TriangleMesh.fromRows(vertices, faces)
    val reordered = TriangleMesh.fromRows(vertices, faces.reverse)
    assertEquals(mesh.topologyIdentity.stableKey, "fcf7e2e81f78e46f")
    assertEquals(mesh.topologyIdentity.toString, "-218456570315873169")
    assertEquals(reordered.topologyIdentity.stableKey, "6861754c1ed19bde")
    assertEquals(reordered.topologyIdentity.toString, "7521421822480456670")
    assert(!mesh.hasSameTopology(reordered))

  test("legacy surface domain excludes coordinates and surface kind"):
    val mesh = TriangleMesh.fromRows(vertices, faces)
    val moved = TriangleMesh.fromRows(vertices.map(_.map(_ * 2.0)), faces)
    val white = SurfaceMeshDomain.from(SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White)).toOption.get
    val inflated = SurfaceMeshDomain.from(SurfaceGeometry(moved, Hemisphere.Left, SurfaceKind.Inflated)).toOption.get
    assertEquals(white.display, "lh:4:2:fcf7e2e81f78e46f")
    assertEquals(inflated, white)
    assert(mesh.hasSameTopology(moved))
