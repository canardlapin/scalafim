package scalafim.fmri.design

import scalafim.fmri.hrf.linalg.Mat

class ResidualizeSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("Residualize: y=x^2 residualized on (1, x) matches known residuals") {
    val X = Mat.unsafe(
      5,
      2,
      Array(
        1.0, 0.0,
        1.0, 1.0,
        1.0, 2.0,
        1.0, 3.0,
        1.0, 4.0
      )
    )
    val y = Mat.unsafe(5, 1, Array(0.0, 1.0, 4.0, 9.0, 16.0))
    val r = Residualize(X, y)
    assertAllClose(r.data, Array(2.0, -1.0, -2.0, -1.0, 2.0), tol = 1e-12)
  }

  test("Residualize: rank-deficient design uses rank, not ncol") {
    val X = Mat.unsafe(3, 2, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val y = Mat.unsafe(3, 1, Array(1.0, 2.0, 3.0))
    val r = Residualize(X, y)
    assertAllClose(r.data, Array(-1.0, 0.0, 1.0), tol = 1e-12)
  }

  test("Residualize: empty design returns data unchanged") {
    val X = Mat.zeros(4, 0)
    val y = Mat.unsafe(4, 1, Array(1.0, -2.0, 3.0, -4.0))
    val r = Residualize(X, y)
    assertAllClose(r.data, y.data, tol = 0.0)
  }

  test("Residualize: column subset works") {
    val X = Mat.unsafe(
      5,
      2,
      Array(
        1.0, 0.0,
        1.0, 1.0,
        1.0, 2.0,
        1.0, 3.0,
        1.0, 4.0
      )
    )
    val y = Mat.unsafe(5, 1, Array(0.0, 1.0, 4.0, 9.0, 16.0))
    val r = Residualize(X, y, cols = Some(Seq(0))) // intercept only
    assertAllClose(r.data, Array(-6.0, -5.0, -2.0, 3.0, 10.0), tol = 1e-12)
  }
