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

  test("Poly.fit columns are orthonormal and predict reproduces the fitted basis") {
    val x = Vector(-3.0, -1.5, -0.25, 0.5, 2.0, 4.5)
    val poly = ParametricBasis.Poly.fit(x, degree = 3, argName = "x")
    var left = 0
    while left < poly.y.cols do
      var right = 0
      while right < poly.y.cols do
        var dot = 0.0
        var row = 0
        while row < poly.y.rows do
          dot += poly.y(row, left) * poly.y(row, right)
          row += 1
        assertEqualsDouble(dot, if left == right then 1.0 else 0.0, 1e-12)
        right += 1
      left += 1
    assertAllClose(ParametricBasis.Poly.predict(poly.coefs, x, degree = 3).data, poly.y.data, 1e-12)
  }

  test("Poly.fit is stable under representable offset and scale changes") {
    val x = Vector(-2.0, -1.0, 0.0, 1.0, 3.0, 5.0)
    val shifted = x.map(value => 1e8 + 1e4 * value)
    assertAllClose(
      ParametricBasis.Poly.fit(shifted, degree = 2, argName = "x").y.data,
      ParametricBasis.Poly.fit(x, degree = 2, argName = "x").y.data,
      1e-10
    )
  }

  test("Poly.fit rejects insufficient unique abscissae") {
    intercept[IllegalArgumentException](ParametricBasis.Poly.fit(Vector(1.0, 1.0, 2.0), degree = 2, argName = "x"))
  }
