package scalafim.fmri.design

import scalafim.fmri.design.basis.ParametricBasis

class PolySuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("Poly.fit matches stats::poly for x=0:4, degree=2 (R 4.5.1)") {
    val x = Vector(0.0, 1.0, 2.0, 3.0, 4.0)
    val poly = ParametricBasis.Poly.fit(x, degree = 2, argName = "x")

    val expected = Array(
      -0.632455532033676,
      -0.316227766016838,
      -3.28837987863123e-17,
      0.316227766016838,
      0.632455532033676,
      0.534522483824849,
      -0.267261241912424,
      -0.534522483824849,
      -0.267261241912424,
      0.534522483824849
    )

    assertEquals(poly.y.rows, 5)
    assertEquals(poly.y.cols, 2)
    // Mat is row-major; expected is column-major from R dput(), so transpose.
    val expectedRowMajor = Array.ofDim[Double](expected.length)
    var r = 0
    while r < 5 do
      var c = 0
      while c < 2 do
        expectedRowMajor(r * 2 + c) = expected(c * 5 + r)
        c += 1
      r += 1

    assertAllClose(poly.y.data, expectedRowMajor, tol = 1e-12)
    assertAllClose(poly.coefs.alpha.toArray, Array(2.0, 2.0), tol = 1e-12)
    assertAllClose(poly.coefs.norm2.toArray, Array(1.0, 5.0, 10.0, 14.0), tol = 1e-12)
  }
