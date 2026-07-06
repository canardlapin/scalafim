package scalafim.image

import narr.NArray

class AffineSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows, clue = "")
    assertEquals(actual.cols, expected.cols, clue = "")
    var i = 0
    while i < actual.data.length do
      assertClose(actual.data(i), expected.data(i), tol)
      i += 1

  test("applyAffine transforms point matrices and preserves NDArray leading dims") {
    val affine =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 10.0),
          Vector(0.0, 3.0, 0.0, 20.0),
          Vector(0.0, 0.0, 4.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val points = Vector(Vector(1.0, 2.0, 3.0), Vector(4.0, 5.0, 6.0))
    val expected = Vector(Vector(12.0, 26.0, 42.0), Vector(18.0, 35.0, 54.0))
    assertEquals(Affine.applyAffines(affine, points), expected, clue = "")

    val arr = NDArray[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), Vector(1, 2, 3))
    val out = Affine.applyAffine(affine, arr)
    assertEquals(out.shape, Vector(1, 2, 3), clue = "")
    val values = Vector.tabulate(out.data.length)(i => out.data(i))
    assertEquals(values, Vector(12.0, 18.0, 26.0, 35.0, 42.0, 54.0), clue = "")
  }

  test("toMatVec and fromMatVec roundtrip") {
    val affine =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 9.0),
          Vector(0.0, 3.0, 0.0, 10.0),
          Vector(0.0, 0.0, 4.0, 11.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val mv = Affine.toMatVec(affine)
    assertEquals(mv.matrix, DMat.fromRows(Vector(Vector(2.0, 0.0, 0.0), Vector(0.0, 3.0, 0.0), Vector(0.0, 0.0, 4.0))), clue = "")
    assertEquals(mv.vector, Vector(9.0, 10.0, 11.0), clue = "")
    assertEquals(Affine.fromMatVec(mv.matrix, mv.vector), affine, clue = "")
  }

  test("appendDiag expands affine and validates starts") {
    val out = Affine.appendDiag(DMat.eye(4), Vector(9.0, 10.0), Vector(99.0, 100.0))

    assertEquals(out.rows, 6, clue = "")
    assertEquals(out.cols, 6, clue = "")
    assertEquals(out(3, 3), 9.0, clue = "")
    assertEquals(out(4, 4), 10.0, clue = "")
    assertEquals(out(3, 5), 99.0, clue = "")
    assertEquals(out(4, 5), 100.0, clue = "")
    assertEquals(out(5, 5), 1.0, clue = "")

    intercept[IllegalArgumentException] {
      Affine.appendDiag(DMat.eye(4), Vector(1.0, 2.0), Vector(1.0))
    }
  }

  test("dotReduce computes right-associated matrix product") {
    val a = DMat.fromRows(Vector(Vector(1.0, 3.0), Vector(2.0, 4.0)))
    val b = DMat.fromRows(Vector(Vector(0.0, 1.0), Vector(1.0, 0.0)))
    val c = DMat.fromRows(Vector(Vector(2.0, 0.0), Vector(0.0, 2.0)))

    val expected = Affine.multiply(a, Affine.multiply(b, c))
    assertEquals(Affine.dotReduce(a, b, c), expected, clue = "")
  }

  test("voxelSizes and obliquity are cardinal for diagonal affines") {
    val affine =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 0.0),
          Vector(0.0, 3.0, 0.0, 0.0),
          Vector(0.0, 0.0, 4.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    assertClose(Affine.voxelSizes(affine), Vector(2.0, 3.0, 4.0), 1e-10)
    assertClose(Affine.obliquity(affine), Vector(0.0, 0.0, 0.0), 1e-10)
  }

  test("rescaleAffine preserves center world coordinate") {
    val affine =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 10.0),
          Vector(0.0, 3.0, 0.0, 20.0),
          Vector(0.0, 0.0, 4.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val shape = Vector(5, 7, 9)
    val newShape = Vector(9, 11, 13)
    val zooms = Vector(1.0, 1.5, 2.0)
    val out = Affine.rescaleAffine(affine, shape, zooms, Some(newShape))

    assertClose(Affine.voxelSizes(out), zooms, 1e-10)
    val centerIn = shape.map(n => math.floor((n - 1).toDouble / 2.0))
    val centerOut = newShape.map(n => math.floor((n - 1).toDouble / 2.0))
    assertClose(Affine.applyAffine(affine, centerIn), Affine.applyAffine(out, centerOut), 1e-10)

    val expected =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 10.0),
          Vector(0.0, 1.5, 0.0, 21.5),
          Vector(0.0, 0.0, 2.0, 34.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    assertClose(out, expected, 1e-10)
  }
