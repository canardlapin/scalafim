package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

/** Basis values live in `H`; fitted coefficients live in `H*`. Rescaling a
  * basis is a gauge change: it leaves the spanned space alone but moves the
  * units of every coefficient, contrast and penalty attached to it.
  */
class BasisGeometrySuite extends munit.FunSuite:

  private val times = (0 to 48).map(_ * 0.5)

  test("reconstruction agrees with direct contraction"):
    val hrf = Hrfs.bspline(nBasis = 5)
    val basis = ResponseBasis.of(hrf)
    val raw = Vector(0.3, -1.2, 2.0, 0.5, -0.7)
    val beta = basis.coefficients(raw).fold(e => fail(e.message), identity)
    val viaBasis = basis.reconstruct(beta)
    val viaExtension = hrf.withCoefficients(raw.toArray)
    times.foreach { t =>
      assertEqualsDouble(
        viaBasis(Lag(t)).data(0),
        viaExtension(Lag(t)).data(0),
        1e-12,
        s"reconstruction differs at t=$t"
      )
    }

  test("reconstruction is the sum of weighted basis columns"):
    val hrf = Hrfs.SPMG3
    val basis = ResponseBasis.of(hrf)
    val raw = Vector(1.0, 0.25, -0.5)
    val beta = basis.coefficients(raw).fold(e => fail(e.message), identity)
    val fitted = basis.reconstruct(beta)
    times.foreach { t =>
      val phi = hrf(Lag(t)).data
      val expected = phi.zip(raw).map(_ * _).sum
      assertEqualsDouble(fitted(Lag(t)).data(0), expected, 1e-12, s"at t=$t")
    }

  test("coefficients of the wrong width are rejected"):
    val basis = ResponseBasis.of(Hrfs.bspline(nBasis = 5))
    assertEquals(
      basis.coefficients(Vector(1.0, 2.0)),
      Left(BasisError.DimensionMismatch("bspline", 5, 2))
    )
    assert(basis.coefficients(Vector(1.0, 2.0, Double.NaN, 4.0, 5.0)).isLeft)

  test("a basis space is distinct per basis value"):
    // The compile-time guarantee: `b1.Space` and `b2.Space` are different types,
    // so coefficients cannot cross between them even at equal width. This test
    // pins the runtime half — that the two bases really are different kernels,
    // so the guarantee is worth having.
    val b1 = ResponseBasis.of(Hrfs.bspline(nBasis = 3, degree = 2))
    val b2 = ResponseBasis.of(Hrfs.SPMG3)
    assertEquals(b1.dimension, b2.dimension)
    val raw = Vector(1.0, 0.0, 0.0)
    val k1 = b1.reconstruct(b1.coefficients(raw).toOption.get)
    val k2 = b2.reconstruct(b2.coefficients(raw).toOption.get)
    val differs = times.exists(t => math.abs(k1(Lag(t)).data(0) - k2(Lag(t)).data(0)) > 1e-6)
    assert(differs, "two same-width bases produced the same kernel; the tagging test is vacuous")

  test("normalize reports the transform, and transported coefficients preserve the kernel"):
    val hrf = Hrfs.bspline(nBasis = 5)
    val basis = ResponseBasis.of(hrf)
    val raw = Vector(0.7, -1.1, 0.4, 2.0, -0.3)
    val beta = basis.coefficients(raw).fold(e => fail(e.message), identity)
    val original = basis.reconstruct(beta)

    val TransformedBasis(normalized, transform) = hrf.normalizeWithTransform()
    transform match
      case BasisTransform.Diagonal(scales) =>
        assertEquals(scales.length, 5)
        assert(scales.forall(_ > 0.0), "peak scales should be positive")
      case other => fail(s"expected a diagonal transform, got $other")

    val normBasis = ResponseBasis.of(normalized)
    val betaPrime = transform
      .transportCoefficients(beta)
      .fold(e => fail(e.message), identity)
    val transported = normBasis.reconstruct(BasisCoefficients.unsafe(betaPrime.values))

    // beta'(Phi') = beta(Phi): the gauge change is invisible in the response.
    times.foreach { t =>
      assertEqualsDouble(
        transported(Lag(t)).data(0),
        original(Lag(t)).data(0),
        1e-9,
        s"transported coefficients changed the reconstructed kernel at t=$t"
      )
    }

  test("stale coefficients after normalization do change the kernel"):
    // The reason the transform has to be reported: reusing untransported
    // coefficients silently yields a different response.
    val hrf = Hrfs.bspline(nBasis = 5)
    val raw = Vector(0.7, -1.1, 0.4, 2.0, -0.3)
    val original = hrf.withCoefficients(raw.toArray)
    val normalized = hrf.normalize().withCoefficients(raw.toArray)
    val differs = times.exists(t => math.abs(original(Lag(t)).data(0) - normalized(Lag(t)).data(0)) > 1e-6)
    assert(differs, "expected untransported coefficients to change the response")

  test("penalties transport under rescaling"):
    val hrf = Hrfs.bspline(nBasis = 5)
    val penalty = hrf.penaltyMatrix()
    val TransformedBasis(_, transform) = hrf.normalizeWithTransform()
    val transported = transform.transportPenalty(penalty).fold(e => fail(e.message), identity)
    assertEquals(transported.rows, 5)
    assertEquals(transported.cols, 5)
    val scales = transform match
      case BasisTransform.Diagonal(s) => s
      case other                      => fail(s"expected diagonal, got $other")
    // Q' = S Q S.
    var r = 0
    while r < 5 do
      var c = 0
      while c < 5 do
        assertEqualsDouble(transported(r, c), scales(r) * penalty(r, c) * scales(c), 1e-12, s"Q'($r,$c)")
        c += 1
      r += 1
    // And it is genuinely a different quadratic form, which is the point.
    val changed = (0 until 5).exists(i => math.abs(transported(i, i) - penalty(i, i)) > 1e-9)
    assert(changed, "rescaling left the penalty untouched; it should not have")

  test("diagonal transforms invert, and singular ones report it"):
    val t = BasisTransform.Diagonal(Vector(2.0, 4.0, 0.5))
    val inv = t.inverse.fold(e => fail(e.message), identity)
    assertEquals(inv, BasisTransform.Diagonal(Vector(0.5, 0.25, 2.0)))
    assert(BasisTransform.Diagonal(Vector(1.0, 0.0)).inverse.isLeft)
    assertEquals(BasisTransform.Identity(3).inverse, Right(BasisTransform.Identity(3)))

  test("Evaluate normalization does not depend on the query grid"):
    // Regression: normalization used to divide by the maximum observed on the
    // supplied grid, so omitting the peak rescaled everything. The same SPMG1
    // read 0.206 at t=2 on a full grid and 1.000 on one missing [3, 12].
    val hrf = Hrfs.SPMG1
    val full = (0 until 30).map(_ * 1.0)
    val punctured = full.filter(t => t < 3.0 || t > 12.0)
    val onFull = Evaluate.doubles(hrf, full, normalize = true)
    val onPunctured = Evaluate.doubles(hrf, punctured, normalize = true)
    punctured.zipWithIndex.foreach { case (t, i) =>
      val j = full.indexOf(t)
      assertEqualsDouble(
        onPunctured(i, 0),
        onFull(j, 0),
        1e-12,
        s"normalized value at t=$t changed with the query grid"
      )
    }

  test("normalized kernels peak at one"):
    // `normalize` takes its scales on its own `dt` grid, so the peak is exactly
    // one there; probe at the same resolution rather than a coarser one.
    val dt = 0.1
    Vector(Hrfs.SPMG1, Hrfs.Gamma, Hrfs.bspline(nBasis = 5)).foreach { hrf =>
      val normalized = hrf.normalize(Seconds(dt))
      val probe = (0 to math.ceil(hrf.span.value / dt).toInt).map(_ * dt)
      var j = 0
      while j < normalized.nbasis do
        val peak = probe.map(t => math.abs(normalized(Lag(t)).data(j))).max
        assertEqualsDouble(peak, 1.0, 1e-9, s"${hrf.name} column $j should peak at 1")
        j += 1
    }

  test("basis labels follow the module's column convention"):
    assertEquals(ResponseBasis.of(Hrfs.SPMG1).labels, Vector("SPMG1"))
    assertEquals(
      ResponseBasis.of(Hrfs.SPMG3).labels,
      Vector("SPMG3_b01", "SPMG3_b02", "SPMG3_b03")
    )
