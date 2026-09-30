package scalafim.group.research.bootstrap

/** Reference values from R (base lgamma, digamma, trigamma, psigamma(x, 2); a uniroot trigamma inverse). */
class SpecialFunctionsSuite extends munit.FunSuite:
  test("logGamma, digamma, trigamma and psi'' match R over a grid including 1.5, 2.5, 4 and 20"):
    val cases = BootstrapReferenceFixtures.SpecialCases
    assert(Vector(1.5, 2.5, 4.0, 20.0).forall(x => cases.exists(_.x == x)), "grid must include 1.5, 2.5, 4, 20")
    cases.foreach { c =>
      assertEqualsDouble(Special.logGamma(c.x), c.logGamma, 1e-13 * math.max(1.0, math.abs(c.logGamma)), s"lgamma(${c.x})")
      assertEqualsDouble(Special.digamma(c.x), c.digamma, 1e-13 * math.max(1.0, math.abs(c.digamma)), s"digamma(${c.x})")
      assertEqualsDouble(Special.trigamma(c.x), c.trigamma, 1e-13 * math.max(1.0, math.abs(c.trigamma)), s"trigamma(${c.x})")
      assertEqualsDouble(Special.tetragamma(c.x), c.tetragamma, 1e-12 * math.max(1.0, math.abs(c.tetragamma)), s"psi''(${c.x})")
    }

  test("trigammaInverse matches R's root, including the x > 1e7 and x < 1e-6 asymptotic branches"):
    val cases = BootstrapReferenceFixtures.TrigammaInverseCases
    assert(cases.exists(_.x > 1e7) && cases.exists(_.x < 1e-6), "both branches must be covered")
    cases.foreach { c =>
      val y = Special.trigammaInverse(c.x).fold(e => fail(e), identity)
      // The branches are asymptotic: 1/sqrt(x) has relative error ~ x^(-1/2)/2, 1/x has ~ x/2.
      val tol = if c.x > 1e7 then 1.0 / math.sqrt(c.x) else if c.x < 1e-6 then c.x else 1e-10
      assertEqualsDouble(y, c.y, tol * c.y, s"trigammaInverse(${c.x})")
    }
    assertEquals(Special.trigammaInverse(2e8), Right(1.0 / math.sqrt(2e8)))
    assertEquals(Special.trigammaInverse(4e-7), Right(1.0 / 4e-7))
    assert(Special.trigammaInverse(0.0).isLeft && Special.trigammaInverse(Double.NaN).isLeft)
