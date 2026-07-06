package scalafim.fmri.design

import scalafim.fmri.design.basis.ParametricBasis

class NaturalSplineSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("NSpline.fit matches splines::ns for x=1:10, df=4 (R 4.5.1)") {
    val x = (1 to 10).map(_.toDouble).toVector
    val b = ParametricBasis.NSpline.fit(x, df = 4, argName = "x").y

    val expected = Array(
      0.0,
      0.0,
      0.0,
      0.0,
      0.0146319158664838,
      -0.105267498394169,
      0.315802495182507,
      -0.210534996788338,
      0.11705532693187,
      -0.16601658337054,
      0.49804975011162,
      -0.33203316674108,
      0.37037037037037,
      -0.139822421471322,
      0.437985782932485,
      -0.29199052195499,
      0.622770919067215,
      -0.00248207889064996,
      0.242700008962759,
      -0.161800005975173,
      0.622770919067215,
      0.255625298527632,
      0.124070606474716,
      -0.0808847481665001,
      0.37037037037037,
      0.524449008875723,
      0.148875195595053,
      -0.049867414347319,
      0.11705532693187,
      0.575413155660069,
      0.238095238095238,
      0.0694362793128225,
      0.0146319158664838,
      0.304069501600366,
      0.333333333333333,
      0.347965249199817,
      0.0,
      -0.142857142857143,
      0.428571428571429,
      0.714285714285714
    )

    assertEquals(b.rows, 10)
    assertEquals(b.cols, 4)
    assertAllClose(b.data, expected, tol = 1e-12)
  }

  test("NSpline.predict reproduces training design matrix") {
    val x = (1 to 10).map(_.toDouble).toVector
    val basis = ParametricBasis.NSpline.fit(x, df = 4, argName = "x")
    val pred = ParametricBasis.NSpline.predict(basis, x)
    assertAllClose(pred.data, basis.y.data, tol = 1e-12)
  }
