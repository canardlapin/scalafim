package scalafim.multivar

import gale.linalg.DMat

class CanonicalEffectOracleSuite extends munit.FunSuite:

  import CanonicalEffectReferenceFixtures as R

  test("analytic rank-one root and normalization are independently checkable"):
    val w0 = R.analyticDirection(0)
    val w1 = R.analyticDirection(1)
    val numerator =
      w0 * (R.analyticEffect(0, 0) * w0 + R.analyticEffect(0, 1) * w1) +
        w1 * (R.analyticEffect(1, 0) * w0 + R.analyticEffect(1, 1) * w1)
    val denominator =
      w0 * (R.analyticResidual(0, 0) * w0 + R.analyticResidual(0, 1) * w1) +
        w1 * (R.analyticResidual(1, 0) * w0 + R.analyticResidual(1, 1) * w1)

    assertEqualsDouble(denominator, 1.0, 1e-14)
    assertEqualsDouble(numerator / denominator, R.analyticRoot, 1e-14)
    assertEqualsDouble(Math.sqrt(R.analyticRoot / (1.0 + R.analyticRoot)), R.analyticCorrelation, 1e-14)

  test("fixed runwise moments agree with explicit dense projectors"):
    R.runs.foreach: run =>
      val actual = CanonicalEffectDenseOracle.evaluate(R.design, run.response, R.contrast)
      assertMatrixClose(actual.effect, run.effect, 1e-11)
      assertMatrixClose(actual.residual, run.residual, 1e-11)

  test("fixed fold aggregation uses transform of the mean held-out root"):
    val mean = R.folds.map(_.heldOutRoot).sum / R.folds.length.toDouble
    assertEqualsDouble(mean, R.meanHeldOutRoot, 1e-14)
    assertEqualsDouble(
      Math.sqrt(mean / (1.0 + mean)),
      R.rootToCorrelationOfMean,
      1e-14
    )

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
