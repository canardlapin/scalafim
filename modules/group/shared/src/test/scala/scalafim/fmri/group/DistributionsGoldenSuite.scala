package scalafim.fmri.group

/** Golden-value tests for the special functions, anchored to known references
  * (R's `erf`/`pnorm`/`pt` and exact incomplete-beta symmetries). Guards against
  * silent drift in the Numerical Recipes approximations.
  */
class DistributionsGoldenSuite extends munit.FunSuite:

  test("erfc golden values") {
    // Raw erfc carries the NR approximation's ~1e-7 error (unlike the clamped p-values).
    assertEqualsDouble(Distributions.erfc(0.0), 1.0, 1e-7)
    assertEqualsDouble(Distributions.erfc(0.5), 0.4795001222, 1e-7)
    assertEqualsDouble(Distributions.erfc(1.0), 0.1572992071, 1e-7)
    assertEqualsDouble(Distributions.erfc(2.0), 0.0046777350, 1e-7)
  }

  test("normal two-sided p golden values (2*pnorm(-|z|))") {
    assertEqualsDouble(Distributions.normalTwoSidedP(1.0), 0.3173105079, 1e-7)
    assertEqualsDouble(Distributions.normalTwoSidedP(2.0), 0.0455002639, 1e-7)
    assertEqualsDouble(Distributions.normalTwoSidedP(3.0), 0.0026997960, 1e-7)
  }

  test("normal survival function golden values (pnorm(-z) upper tail)") {
    assertEqualsDouble(Distributions.normalSf(1.6448536270), 0.05, 1e-7)
    assertEqualsDouble(Distributions.normalSf(2.3263478740), 0.01, 1e-7)
  }

  test("student-t two-sided p at tabulated critical values") {
    // qt(0.975, df) -> two-sided p = 0.05; qt(0.995, df) -> 0.01.
    assertEqualsDouble(Distributions.studentTTwoSidedP(2.2281388520, 10), 0.05, 1e-6)
    assertEqualsDouble(Distributions.studentTTwoSidedP(3.1692727330, 10), 0.01, 1e-6)
    assertEqualsDouble(Distributions.studentTTwoSidedP(2.0859634470, 20), 0.05, 1e-6)
    assertEqualsDouble(Distributions.studentTTwoSidedP(2.8453397300, 20), 0.01, 1e-6)
  }

  test("regularized incomplete beta symmetry and boundaries") {
    assertEqualsDouble(Distributions.regularizedIncompleteBeta(2.0, 2.0, 0.5), 0.5, 1e-9)
    assertEqualsDouble(Distributions.regularizedIncompleteBeta(0.5, 0.5, 0.5), 0.5, 1e-9)
    assertEqualsDouble(Distributions.regularizedIncompleteBeta(3.0, 5.0, 0.0), 0.0, 1e-12)
    assertEqualsDouble(Distributions.regularizedIncompleteBeta(3.0, 5.0, 1.0), 1.0, 1e-12)
  }
