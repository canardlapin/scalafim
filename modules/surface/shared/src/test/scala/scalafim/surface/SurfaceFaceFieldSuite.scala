package scalafim.surface

class SurfaceFaceFieldSuite extends munit.FunSuite:
  private def geometry(faces: Seq[(Int, Int, Int)] = Seq((0, 1, 2), (0, 2, 3)), shift: Double = 0.0): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(shift, 0.0, 0.0), Seq(shift + 1.0, 0.0, 0.0), Seq(shift + 1.0, 1.0, 0.0), Seq(shift, 1.0, 0.0)),
      faces), Hemisphere.Left, SurfaceKind.Inflated)

  test("face fields own frame-major values and validate their face domain"):
    val source = Array(10.0, 20.0, 30.0, 40.0)
    val field = SurfaceFaceField.make(geometry(), source, 2).toOption.get
    source(0) = 99.0
    assertEqualsDouble(field.valueAt(FaceId(0)).get, 10.0, 0.0)
    assertEqualsDouble(field.valueAt(FaceId(1), 1).get, 40.0, 0.0)
    assertEquals(field.valueAt(FaceId(2)), None)
    assertEquals(field.valueAt(FaceId(0), -1), None)
    assertEquals(field.valueAt(FaceId(0), 2), None)
    assert(SurfaceFaceField.make(geometry(), source).isLeft)
    assert(SurfaceFaceField.make(geometry(), Array.empty[Double], 0).isLeft)
    assert(SurfaceFaceField.make(geometry(), Array.empty[Double], Int.MaxValue).isLeft)

  test("coordinates may morph but face order and winding cannot change implicitly"):
    val field = SurfaceFaceField.make(geometry(), Array(1, 2)).toOption.get
    assert(field.isCompatibleWith(geometry(shift = 3.0)))
    assert(!field.isCompatibleWith(geometry(Seq((0, 2, 3), (0, 1, 2)))))
    assert(!field.isCompatibleWith(geometry(Seq((0, 2, 1), (0, 2, 3)))))
    assert(!field.isCompatibleWith(SurfaceGeometry(geometry().mesh, Hemisphere.Right, SurfaceKind.Inflated)))
