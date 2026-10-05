package scalafim.phrfcmp.score

/** Reference values from R 4.5.1: `qf`, `qchisq`, and the one-way ANOVA mean squares of `anova(lm(v ~ g))` (ICC =
  * (MSB - MSW) / (MSB + (k0 - 1) MSW); upper = (F qf(0.8, dfw, dfb) - 1) / (F qf(0.8, dfw, dfb) + k0 - 1), with k0 =
  * k when balanced). `psych::ICC(x, alpha = 0.4)` 2.6.3 was also run on the balanced matrix: it gives
  * 0.805646078657 (upper 0.902758918220), 7e-7 away from the exact ANOVA value because psych does not take the
  * plain mean-square route; the test binds to the ANOVA value, which a hand computation reproduces
  * (MSB = 7.691667, MSW = 0.4375).
  */
class DistIccSuite extends munit.FunSuite:
  private def rel(a: Double, b: Double, tol: Double, hint: String = ""): Unit =
    assert(math.abs(a - b) <= tol * math.abs(b), s"$hint got $a expected $b")

  test("F quantiles match R qf"):
    rel(Dist.fQuantile(0.8, 5, 20), 1.6217747878867836, 1e-11)
    rel(Dist.fQuantile(0.8, 19, 5), 2.16747137489285, 1e-11)
    rel(Dist.fQuantile(0.8, 3, 40), 1.6195377671457105, 1e-11)
    rel(Dist.fQuantile(0.8, 1, 2), 3.5555555555555554, 1e-11)
    rel(Dist.fQuantile(0.8, 100, 7), 1.8418714816511417, 1e-11)

  test("chi-square quantiles and the sigma UCL factor match R qchisq"):
    rel(Dist.chi2Quantile(0.5, 1), 0.45493642311957283, 1e-11)
    rel(Dist.chi2Quantile(0.2, 3), 1.0051740130523492, 1e-11)
    rel(Dist.sigmaUclFactor(14), 1.2160468477276118, 1e-11)
    rel(Dist.sigmaUclFactor(19), 1.1769727057611257, 1e-11)
    rel(Dist.sigmaUclFactor(24), 1.1527232541908461, 1e-11)
    rel(Dist.sigmaUclFactor(39), 1.1137493371571845, 1e-11)

  private val balanced: Vector[IndexedSeq[Double]] = Vector(
    Vector(0.5, 1.5, 1.0, 2.0), Vector(2.5, 3.0, 2.0, 3.5), Vector(-1.0, 0.0, 0.5, -0.5),
    Vector(1.0, 1.0, 2.0, 1.5), Vector(3.5, 2.5, 4.0, 3.0), Vector(0.0, -1.0, 1.0, 0.5)
  )

  test("balanced ICC and 80 % upper limit match anova(lm()) ICC"):
    val e = Icc.of(balanced).toOption.flatten.get
    rel(e.icc, 0.80564553447478016, 1e-12)
    rel(e.upper80, 0.90275862273421115, 1e-11)

  test("unbalanced ICC uses k0 and matches the anova(lm()) ICC reference"):
    val cl: Vector[IndexedSeq[Double]] = Vector(
      Vector(0.5, 1.5, 1.0), Vector(2.5, 3.0, 2.0, 3.5, 3.0), Vector(-1.0, 0.0), Vector(1.0, 1.0, 2.0, 1.5), Vector(3.5, 2.5, 4.0)
    )
    val e = Icc.of(cl).toOption.flatten.get
    rel(e.icc, 0.83821187364381944, 1e-12)
    rel(e.upper80, 0.92936752550127644, 1e-11)

  test("ICC is location invariant and truncated at 0"):
    val shifted = balanced.map(_.map(_ + 100.0))
    rel(Icc.of(shifted).toOption.flatten.get.icc, 0.80564553447478016, 1e-9)
    // identical cluster means, noisy within: MSB = 0, ICC truncates to 0
    val flat: Vector[IndexedSeq[Double]] = Vector(Vector(-1.0, 1.0), Vector(-1.0, 1.0), Vector(-1.0, 1.0))
    assertEquals(Icc.of(flat).toOption.flatten.get.icc, 0.0)

  test("degenerate ICC inputs yield None, non-finite is refused"):
    assertEquals(Icc.of(Vector(Vector(1.0, 2.0))), Right(None))
    assertEquals(Icc.of(Vector(Vector(1.0), Vector(2.0))), Right(None)) // no within df
    assertEquals(Icc.of(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0))), Right(None)) // MSB = MSW = 0
    assert(Icc.of(Vector(Vector(1.0, Double.NaN), Vector(1.0, 2.0))).isLeft)
    // perfectly clustered: MSW = 0
    val e = Icc.of(Vector(Vector(1.0, 1.0), Vector(2.0, 2.0))).toOption.flatten.get
    assertEquals((e.icc, e.upper80), (1.0, 1.0))
