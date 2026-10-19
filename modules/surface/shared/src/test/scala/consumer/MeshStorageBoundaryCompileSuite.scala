package consumer

class MeshStorageBoundaryCompileSuite extends munit.FunSuite:
  test("raw mesh point fields are not a public coordinate-frame declaration"):
    val errors = compileErrors(
      """
      val mesh = scalafim.surface.TriangleMesh.fromRows(
        Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)),
        Vector((0, 1, 2)))
      mesh.points
      """
    )
    assert(errors.contains("points") && errors.contains("access"), errors)
