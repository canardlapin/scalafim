package scalafim.fmri.hrf

/** Derivative-boost signed amplitude, checked against values computed by hand
  * and against algebraic properties of `sign(b_c) * sqrt(sum b_k^2)` rather
  * than against the implementation.
  */
class SignedAmplitudeSuite extends munit.FunSuite:

  private val tol = 1e-12

  private def estimate(hrf: Hrf, betas: Vector[Double], weighting: AmplitudeWeighting = AmplitudeWeighting.Unweighted): SignedAmplitudeEstimate =
    val basis = ResponseBasis.of(hrf)
    val beta = basis.coefficients(betas).fold(error => fail(error.message), identity)
    basis.signedAmplitude(beta, weighting).fold(error => fail(error.message), identity)

  private def error(hrf: Hrf, betas: Vector[Double], weighting: AmplitudeWeighting = AmplitudeWeighting.Unweighted): BasisError =
    val basis = ResponseBasis.of(hrf)
    val beta = basis.coefficients(betas).fold(error => fail(error.message), identity)
    basis.signedAmplitude(beta, weighting).fold(identity, value => fail(s"expected refusal, got $value"))

  test("SPMG2 hand-computed values follow sign(b1) * sqrt(b1^2 + b2^2)"):
    // 3-4-5 and 5-12-13 triangles.
    val cases = Vector(
      (Vector(3.0, 4.0), 5.0, AmplitudeSign.Positive),
      (Vector(-3.0, 4.0), -5.0, AmplitudeSign.Negative),
      (Vector(-3.0, -4.0), -5.0, AmplitudeSign.Negative),
      (Vector(5.0, -12.0), 13.0, AmplitudeSign.Positive),
      (Vector(0.6, 0.8), 1.0, AmplitudeSign.Positive)
    )
    cases.foreach { case (betas, expected, sign) =>
      val result = estimate(Hrfs.SPMG2, betas)
      assertEqualsDouble(result.value, expected, tol, clue = betas)
      assertEqualsDouble(result.magnitude, math.abs(expected), tol, clue = betas)
      assertEquals(result.sign, sign)
    }

  test("SPMG3 hand-computed values pool all three coefficients"):
    // 1^2 + 2^2 + 2^2 = 9; 2^2 + 3^2 + 6^2 = 49; 1^2 + 4^2 + 8^2 = 81.
    assertEqualsDouble(estimate(Hrfs.SPMG3, Vector(1.0, 2.0, 2.0)).value, 3.0, tol)
    assertEqualsDouble(estimate(Hrfs.SPMG3, Vector(-2.0, 3.0, -6.0)).value, -7.0, tol)
    assertEqualsDouble(estimate(Hrfs.SPMG3, Vector(1.0, -4.0, 8.0)).value, 9.0, tol)

  test("SPMG1 reduces to the canonical coefficient"):
    assertEqualsDouble(estimate(Hrfs.SPMG1, Vector(-2.5)).value, -2.5, tol)
    assertEqualsDouble(estimate(Hrfs.SPMG1, Vector(0.75)).value, 0.75, tol)

  test("a zero canonical coefficient has sign Zero and value 0 but keeps its magnitude"):
    Vector(0.0, -0.0).foreach { canonical =>
      val result = estimate(Hrfs.SPMG3, Vector(canonical, 3.0, 4.0))
      assertEquals(result.sign, AmplitudeSign.Zero)
      assertEqualsDouble(result.value, 0.0, 0.0)
      assert(!(1.0 / result.value).isNegInfinity, "value must not be negative zero")
      assertEqualsDouble(result.magnitude, 5.0, tol)
    }
    val all = estimate(Hrfs.SPMG2, Vector(0.0, 0.0))
    assertEquals(all.sign, AmplitudeSign.Zero)
    assertEqualsDouble(all.magnitude, 0.0, 0.0)

  test("the result is typed descriptive and records its provenance"):
    val result = estimate(Hrfs.SPMG3, Vector(1.0, 2.0, 2.0))
    assertEquals(result.inference, SummaryInference.Descriptive)
    assertEquals(result.summary, NonlinearResponseSummary.SignedAmplitude(AmplitudeWeighting.Unweighted))
    assertEquals(result.receipt.canonical.role, BasisRole.Canonical)
    assertEquals(result.receipt.canonical.index, 1)
    assertEquals(result.receipt.elements.map(_.role), Vector(BasisRole.Canonical, BasisRole.TemporalDerivative, BasisRole.DispersionDerivative))
    assertEquals(result.receipt.columnWeights, Vector(1.0, 1.0, 1.0))
    assertEquals(result.receipt.normDiscretization, None)

  test("the canonical column is found by role, not by position"):
    // Derivative first, canonical second: the sign must come from column 2.
    val elements = Vector(
      BasisElement(BasisElementId("deriv"), 1, BasisRole.TemporalDerivative, "temporal derivative"),
      BasisElement(BasisElementId("canon"), 2, BasisRole.Canonical, "canonical")
    )
    val reordered = Hrf.multiWithBasisElements("reordered", elements, span = Seconds(8.0))(_ => Array(1.0, 2.0))
      .fold(error => fail(error.message), identity)
    val result = estimate(reordered, Vector(4.0, -3.0))
    assertEqualsDouble(result.value, -5.0, tol)
    assertEquals(result.sign, AmplitudeSign.Negative)
    assertEquals(result.receipt.canonical.id.value, "canon")

  test("bases without exactly one canonical element, or with non-derivative columns, are refused"):
    error(Hrfs.fir(nBasis = 3, span = Seconds(12.0)), Vector(1.0, 2.0, 3.0)) match
      case BasisError.InvalidSummary(detail) => assert(detail.contains("informed basis"), detail)
      case other                              => fail(s"unexpected $other")

    val derivativesOnly = Vector(
      BasisElement(BasisElementId("td"), 1, BasisRole.TemporalDerivative, "td"),
      BasisElement(BasisElementId("dd"), 2, BasisRole.DispersionDerivative, "dd")
    )
    val noCanonical = Hrf.multiWithBasisElements("no-canonical", derivativesOnly, span = Seconds(8.0))(_ => Array(1.0, 2.0))
      .fold(e => fail(e.message), identity)
    assertEquals(error(noCanonical, Vector(1.0, 1.0)), BasisError.UnknownRole("canonical"))

    val twoCanonical = Vector(
      BasisElement(BasisElementId("a"), 1, BasisRole.Canonical, "a"),
      BasisElement(BasisElementId("b"), 2, BasisRole.Canonical, "b")
    )
    val doubled = Hrf.multiWithBasisElements("doubled", twoCanonical, span = Seconds(8.0))(_ => Array(1.0, 2.0))
      .fold(e => fail(e.message), identity)
    error(doubled, Vector(1.0, 1.0)) match
      case BasisError.InvalidSummary(detail) => assert(detail.contains("exactly one canonical"), detail)
      case other                              => fail(s"unexpected $other")

  test("property: |A|^2 = sum b^2, sign(A) = sign(b_c), and A is odd and homogeneous"):
    val random = new scala.util.Random(20261008L)
    (1 to 200).foreach { trial =>
      val hrf = if trial % 2 == 0 then Hrfs.SPMG2 else Hrfs.SPMG3
      val betas = Vector.fill(hrf.nbasis)(random.nextGaussian() * 3.0)
      val result = estimate(hrf, betas)
      var sumSquares = 0.0
      betas.foreach(b => sumSquares += b * b)
      assertEqualsDouble(result.value * result.value, sumSquares, 1e-10 * math.max(1.0, sumSquares), clue = betas)
      assertEquals(math.signum(result.value), math.signum(betas.head), clue = betas)
      val c = 0.1 + random.nextDouble() * 5.0
      assertEqualsDouble(estimate(hrf, betas.map(_ * c)).value, c * result.value, 1e-10 * math.max(1.0, c * result.magnitude), clue = betas)
      assertEqualsDouble(estimate(hrf, betas.map(-_)).value, -result.value, 1e-12 * math.max(1.0, result.magnitude), clue = betas)
      // Flipping a derivative coefficient never changes the amplitude.
      val flipped = betas.updated(1, -betas(1))
      assertEqualsDouble(estimate(hrf, flipped).value, result.value, 1e-12 * math.max(1.0, result.magnitude), clue = betas)
    }

  test("pooling is overflow safe for very large coefficients"):
    val result = estimate(Hrfs.SPMG2, Vector(3e200, 4e200))
    assertEqualsDouble(result.value / 1e200, 5.0, 1e-12)

  test("basis-norm weighting scales each coefficient by its basis function's L2 norm"):
    // Constant columns 1 and 2 on [0, 8]: norms are sqrt(8) and 2 sqrt(8).
    // A = sign(b1) sqrt((n1 b1)^2 + (n2 b2)^2) / n1 = sign(b1) sqrt(b1^2 + 4 b2^2).
    val elements = Vector(
      BasisElement(BasisElementId("canon"), 1, BasisRole.Canonical, "canonical"),
      BasisElement(BasisElementId("deriv"), 2, BasisRole.TemporalDerivative, "temporal derivative")
    )
    val constant = Hrf.multiWithBasisElements("constant", elements, span = Seconds(8.0), support = Support.Compact(Seconds(8.0)))(_ => Array(1.0, 2.0))
      .fold(e => fail(e.message), identity)
    val step = PositiveSeconds(0.5).fold(e => fail(e.message), identity)
    val weighting = AmplitudeWeighting.BasisL2Norm(step)
    val result = estimate(constant, Vector(-3.0, 2.0), weighting)
    assertEqualsDouble(result.value, -5.0, 1e-12)
    assertEqualsDouble(result.receipt.columnWeights(0), math.sqrt(8.0), 1e-12)
    assertEqualsDouble(result.receipt.columnWeights(1), 2.0 * math.sqrt(8.0), 1e-12)
    assertEquals(result.receipt.normDiscretization.map(_.samples), Some(17))
    assertEquals(result.summary, NonlinearResponseSummary.SignedAmplitude(weighting))
    // With zero derivative coefficients the weighted amplitude is b_c itself.
    assertEqualsDouble(estimate(Hrfs.SPMG3, Vector(1.7, 0.0, 0.0), weighting).value, 1.7, 1e-12)

  test("basis-norm weighting reports the quadrature limit instead of clamping"):
    val tiny = PositiveSeconds(1e-9).fold(e => fail(e.message), identity)
    error(Hrfs.SPMG2, Vector(1.0, 1.0), AmplitudeWeighting.BasisL2Norm(tiny)) match
      case BasisError.InvalidDiscretization(detail) => assert(detail.contains("limit"), detail)
      case other                                     => fail(s"unexpected $other")
