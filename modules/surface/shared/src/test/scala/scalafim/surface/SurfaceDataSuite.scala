package scalafim.surface

class SurfaceDataSuite extends munit.FunSuite:

  private val geometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        ),
        Vector(
          (0, 1, 2),
          (0, 1, 3),
          (0, 2, 3),
          (1, 2, 3)
        )
      ),
      Hemisphere.Left,
      SurfaceKind.Pial
    )

  test("SurfaceField stores indexed vertex values"):
    val field =
      SurfaceField.fromIndexed(
        geometry,
        Vector(VertexId(0), VertexId(2)),
        Vector(10.0, 20.0),
        "activation"
      )

    assertEquals(field.size, 2)
    assertEquals(field.vertexIds, Vector(VertexId(0), VertexId(2)))
    assertEquals(field.valueAt(VertexId(2)), Some(20.0))
    assertEquals(field.valueAt(VertexId(1)), None)

  test("SurfaceField validates data length, uniqueness, and bounds"):
    interceptMessage[IllegalArgumentException]("requirement failed: field data length must match vertex indices"):
      SurfaceField.fromIndexed(geometry, Vector(VertexId(0), VertexId(1)), Vector(1.0))

    interceptMessage[IllegalArgumentException]("requirement failed: surface vertex indices must be unique"):
      SurfaceField.fromIndexed(geometry, Vector(VertexId(0), VertexId(0)), Vector(1.0, 2.0))

    interceptMessage[IllegalArgumentException]("requirement failed: surface vertex index out of range"):
      SurfaceField.fromIndexed(geometry, Vector(VertexId(4)), Vector(1.0))

  test("SurfaceMatrix stores vertices by feature columns"):
    val matrix =
      SurfaceMatrix.fromRows(
        geometry,
        Vector(VertexId(1), VertexId(3)),
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 4.0)
        ),
        "timeseries"
      )

    assertEquals(matrix.rows, 2)
    assertEquals(matrix.columns, 2)
    assertEquals(matrix.rowVertex(1), VertexId(3))
    assertEquals(matrix(1, 0), 3.0)
    assertEquals(matrix(1, 1), 4.0)

  test("SurfaceRoi extracts a checked subset from a field"):
    val field = SurfaceField.full(geometry, Vector(1, 2, 3, 4), "labels")
    val roi = SurfaceRoi.fromField(field, Vector(VertexId(3), VertexId(1)), "roi")

    assertEquals(roi.size, 2)
    assertEquals(roi.vertexIds, Vector(VertexId(3), VertexId(1)))
    assertEquals(roi.data(0), 4)
    assertEquals(roi.data(1), 2)

    interceptMessage[IllegalArgumentException]("ROI vertex 2 is not present in field"):
      SurfaceRoi.fromField(SurfaceField.fromIndexed(geometry, Vector(VertexId(0)), Vector(1)), Vector(VertexId(2)))

  test("LabeledSurface stores label data and table metadata"):
    val labeled =
      LabeledSurface.fromIndexed(
        geometry,
        Vector(VertexId(0), VertexId(1), VertexId(2)),
        Vector(1, 1, 2),
        Vector(
          LabelInfo(1, "A", Some("#ff0000")),
          LabelInfo(2, "B", Some("#00ff00"))
        )
      )

    assertEquals(labeled.size, 3)
    assertEquals(labeled.labelAt(VertexId(2)), Some(2))
    assertEquals(labeled.info(1).map(_.name), Some("A"))

  test("SurfaceSet validates shared hemisphere and vertex count"):
    val inflated = SurfaceGeometry(geometry.mesh, Hemisphere.Left, SurfaceKind.Inflated)
    val set = SurfaceSet.of(SurfaceKind.Pial, geometry, SurfaceKind.Inflated -> inflated)

    assertEquals(set.hemisphere, Hemisphere.Left)
    assertEquals(set.vertexCount, 4)
    assertEquals(set.default, geometry)
    assertEquals(set.get(SurfaceKind.Inflated), Some(inflated))

    val right = SurfaceGeometry(geometry.mesh, Hemisphere.Right, SurfaceKind.Pial)
    interceptMessage[IllegalArgumentException]("requirement failed: all surface geometries must share a hemisphere"):
      SurfaceSet.of(SurfaceKind.Pial, geometry, SurfaceKind.Inflated -> right)

  test("HemispherePair provides typed left/right containers"):
    val pair = HemispherePair(left = "lh", right = "rh")
    assertEquals(pair.left, "lh")
    assertEquals(pair.right, "rh")
