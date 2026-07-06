package scalafim.surface

class SurfaceComponentsSuite extends munit.FunSuite:

  private val disconnectedGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(3.0, 0.0, 0.0),
          Vector(4.0, 0.0, 0.0),
          Vector(3.0, 1.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (3, 4, 5)
        )
      ),
      Hemisphere.Left,
      SurfaceKind.Custom("two-triangle")
    )

  test("connectedComponents labels active connected sets by decreasing size"):
    val field =
      SurfaceField.full(
        disconnectedGeometry,
        Vector(2.0, 2.2, 0.0, -2.0, -2.1, -2.2),
        "stat"
      )

    val result = SurfaceComponents.connectedComponents(field, SurfaceThreshold(-1.0, 1.0))

    assertEquals(result.index.valueAt(VertexId(3)), Some(1))
    assertEquals(result.index.valueAt(VertexId(4)), Some(1))
    assertEquals(result.index.valueAt(VertexId(5)), Some(1))
    assertEquals(result.size.valueAt(VertexId(3)), Some(3))
    assertEquals(result.index.valueAt(VertexId(0)), Some(2))
    assertEquals(result.index.valueAt(VertexId(1)), Some(2))
    assertEquals(result.size.valueAt(VertexId(0)), Some(2))
    assertEquals(result.index.valueAt(VertexId(2)), Some(0))
    assertEquals(result.size.valueAt(VertexId(2)), Some(0))

  test("connectedComponents returns zero fields when no vertices pass threshold"):
    val field = SurfaceField.full(disconnectedGeometry, Vector.fill(6)(0.0), "stat")
    val result = SurfaceComponents.connectedComponents(field, SurfaceThreshold(-1.0, 1.0))

    assertEquals(Vector.tabulate(6)(i => result.index.data(i)), Vector.fill(6)(0))
    assertEquals(Vector.tabulate(6)(i => result.size.data(i)), Vector.fill(6)(0))

  test("clusterThreshold keeps only clusters meeting minimum size"):
    val field =
      SurfaceField.full(
        disconnectedGeometry,
        Vector(2.0, 2.2, 0.0, -2.0, -2.1, -2.2),
        "stat"
      )

    val filtered = SurfaceComponents.clusterThreshold(field, SurfaceThreshold(-1.0, 1.0), minSize = 3)

    assertEquals(filtered.valueAt(VertexId(0)), Some(0.0))
    assertEquals(filtered.valueAt(VertexId(1)), Some(0.0))
    assertEquals(filtered.valueAt(VertexId(2)), Some(0.0))
    assertEquals(filtered.valueAt(VertexId(3)), Some(-2.0))
    assertEquals(filtered.valueAt(VertexId(4)), Some(-2.1))
    assertEquals(filtered.valueAt(VertexId(5)), Some(-2.2))

  test("SurfaceThreshold validates range order"):
    interceptMessage[IllegalArgumentException]("requirement failed: threshold low must be <= high"):
      SurfaceThreshold(1.0, -1.0)
