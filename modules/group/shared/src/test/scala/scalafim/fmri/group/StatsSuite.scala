package scalafim.fmri.group

class StatsSuite extends munit.FunSuite:

  test("normal two-sided p-values at anchor points") {
    assertEqualsDouble(Distributions.normalTwoSidedP(0.0), 1.0, 1e-12)
    // 1.959964 is the 0.05 two-sided normal critical value.
    assertEqualsDouble(Distributions.normalTwoSidedP(1.959963985), 0.05, 1e-6)
    // 2.575829 is the 0.01 two-sided normal critical value.
    assertEqualsDouble(Distributions.normalTwoSidedP(2.575829304), 0.01, 1e-6)
    assert(Distributions.normalTwoSidedP(Double.NaN).isNaN)
  }

  test("student-t two-sided p-values at anchor critical values") {
    assertEqualsDouble(Distributions.studentTTwoSidedP(0.0, 4), 1.0, 1e-12)
    // 2.776445 is the 0.05 two-sided critical t for df = 4.
    assertEqualsDouble(Distributions.studentTTwoSidedP(2.776445105, 4), 0.05, 1e-6)
    // 4.604095 is the 0.01 two-sided critical t for df = 4.
    assertEqualsDouble(Distributions.studentTTwoSidedP(4.604094871, 4), 0.01, 1e-6)
  }

  test("student-t approaches normal as df grows") {
    val z = 1.7
    assertEqualsDouble(
      Distributions.studentTTwoSidedP(z, 2_000_000),
      Distributions.normalTwoSidedP(z),
      1e-4
    )
  }

  test("Benjamini-Hochberg matches p.adjust") {
    // p.adjust(c(.01,.02,.03,.04,.05), "BH") == 0.05 for every entry.
    val flat = Fdr.benjaminiHochberg(Array(0.01, 0.02, 0.03, 0.04, 0.05))
    flat.foreach(q => assertEqualsDouble(q, 0.05, 1e-12))

    // p.adjust(c(.005,.01,.5), "BH") == c(0.015, 0.015, 0.5).
    val mixed = Fdr.benjaminiHochberg(Array(0.005, 0.01, 0.5))
    assertEqualsDouble(mixed(0), 0.015, 1e-12)
    assertEqualsDouble(mixed(1), 0.015, 1e-12)
    assertEqualsDouble(mixed(2), 0.5, 1e-12)
  }

  test("Benjamini-Hochberg preserves input order under shuffling") {
    val q = Fdr.benjaminiHochberg(Array(0.5, 0.005, 0.01))
    assertEqualsDouble(q(0), 0.5, 1e-12)
    assertEqualsDouble(q(1), 0.015, 1e-12)
    assertEqualsDouble(q(2), 0.015, 1e-12)
  }

  test("Benjamini-Yekutieli matches p.adjust") {
    // c(5) = 1 + 1/2 + 1/3 + 1/4 + 1/5 = 2.283333...; all BH-raw are 0.05.
    val q = Fdr.benjaminiYekutieli(Array(0.01, 0.02, 0.03, 0.04, 0.05))
    q.foreach(v => assertEqualsDouble(v, 0.05 * (1 + 1.0 / 2 + 1.0 / 3 + 1.0 / 4 + 1.0 / 5), 1e-12))
  }

  test("FDR clamps adjusted p-values at one and passes NaN through") {
    val q = Fdr.benjaminiHochberg(Array(0.9, Double.NaN, 0.95))
    assert(q(0) <= 1.0)
    assert(q(2) <= 1.0)
    assert(q(1).isNaN)
  }
