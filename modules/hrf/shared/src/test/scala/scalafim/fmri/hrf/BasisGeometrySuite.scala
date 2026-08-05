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

  test("basis transforms transport covariance and adjusted linear hypotheses"):
    val covariance = Mat.fromRows(
      Seq(
        Seq(2.0, 0.2, -0.1),
        Seq(0.2, 1.5, 0.3),
        Seq(-0.1, 0.3, 0.8)
      )
    )
    val beta = Vector(0.7, -1.1, 0.4)
    val weights = Vector(0.2, -0.5, 1.3)
    val transforms = Vector[BasisTransform](
      BasisTransform.Diagonal(Vector(2.0, 0.5, 3.0)),
      BasisTransform.Permutation(Vector(2, 0, 1))
    )

    def dot(left: Vector[Double], right: Vector[Double]): Double =
      left.zip(right).map(_ * _).sum

    transforms.foreach { transform =>
      val betaPrime = transform
        .transportCoefficients(BasisCoefficients.unsafe[Unit](beta))
        .fold(error => fail(error.message), identity)
      val covariancePrime = transform
        .transportCovariance(covariance)
        .fold(error => fail(error.message), identity)
      val weightsPrime = transform
        .transportLinearWeights(weights)
        .fold(error => fail(error.message), identity)

      assertEqualsDouble(dot(weights, beta), dot(weightsPrime, betaPrime.values), 1e-12)
      val originalVariance = dot(weights, covariance.data.grouped(3).map(row => dot(weights, row.toVector)).toVector)
      val transformedVariance = dot(
        weightsPrime,
        covariancePrime.data.grouped(3).map(row => dot(weightsPrime, row.toVector)).toVector
      )
      assertEqualsDouble(transformedVariance, originalVariance, 1e-12)
    }

    assert(BasisTransform.Diagonal(Vector(1.0, 2.0)).transportCovariance(covariance).isLeft)
    assert(BasisTransform.Permutation(Vector(0, 0, 1)).transportLinearWeights(weights).isLeft)

  test("diagonal transforms invert, and singular ones report it"):
    val t = BasisTransform.Diagonal(Vector(2.0, 4.0, 0.5))
    val inv = t.inverse.fold(e => fail(e.message), identity)
    assertEquals(inv, BasisTransform.Diagonal(Vector(0.5, 0.25, 2.0)))
    assert(BasisTransform.Diagonal(Vector(1.0, 0.0)).inverse.isLeft)
    assertEquals(BasisTransform.Identity(3).inverse, Right(BasisTransform.Identity(3)))

  test("basis permutations transport coefficients and penalties contragrediently"):
    val source = Hrfs.SPMG3
    val sourceBasis = ResponseBasis.of(source)
    val order = Vector(2, 0, 1)
    val transform = BasisTransform.Permutation(order)
    val beta = Vector(0.7, -1.1, 0.4)
    val transported = transform
      .transportCoefficients(BasisCoefficients.unsafe[Unit](beta))
      .fold(error => fail(error.message), identity)
    assertEquals(transported.values, Vector(beta(2), beta(0), beta(1)))
    assertEquals(transform.inverse, Right(BasisTransform.Permutation(Vector(1, 2, 0))))
    assert(BasisTransform.Permutation(Vector(0, 0, 1)).inverse.isLeft)

    val permuted = Hrf.multi("SPMG3-permuted", nbasis = 3, span = source.span) { lag =>
      val values = source(lag).data
      order.map(i => values(i)).toArray
    }
    val permutedBasis = ResponseBasis.of(permuted)
    val originalKernel = sourceBasis.reconstruct(sourceBasis.coefficients(beta).fold(error => fail(error.message), identity))
    val permutedKernel = permutedBasis.reconstruct(BasisCoefficients.unsafe(transported.values))
    times.foreach { t =>
      assertEqualsDouble(
        originalKernel(Lag(t)).data(0),
        permutedKernel(Lag(t)).data(0),
        1e-12,
        s"permutation changed reconstructed response at t=$t"
      )
    }

    val penalty = source.penaltyMatrix()
    val transportedPenalty = transform.transportPenalty(penalty).fold(error => fail(error.message), identity)
    var r = 0
    while r < 3 do
      var c = 0
      while c < 3 do
        assertEqualsDouble(transportedPenalty(r, c), penalty(order(r), order(c)), 1e-12)
        c += 1
      r += 1

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

  test("basis elements expose stable semantic roles rather than labels"):
    val first = ResponseBasis.of(Hrfs.SPMG3).elements
    val second = ResponseBasis.of(Hrfs.SPMG3).elements
    assertEquals(first.map(_.role), Vector(BasisRole.Canonical, BasisRole.TemporalDerivative, BasisRole.DispersionDerivative))
    assertEquals(first.map(_.id.value), second.map(_.id.value))
    assertEquals(first.map(_.label), ResponseBasis.of(Hrfs.SPMG3).labels)

    val fir = ResponseBasis.of(Hrfs.fir(nBasis = 4)).elements
    assertEquals(fir.map(_.index), Vector(1, 2, 3, 4))
    fir.zipWithIndex.foreach { case (element, index) =>
      element.role match
        case BasisRole.FirBin(bin, from, until) =>
          assertEquals(bin, index + 1)
          assertEqualsDouble(from.value, index.toDouble * 6.0, 1e-12)
          assertEqualsDouble(until.value, (index + 1).toDouble * 6.0, 1e-12)
        case other => fail(s"expected FIR role, got $other")
    }

  test("custom bases retain caller-supplied identities and validate cardinality"):
    val elements = Vector(
      BasisElement(BasisElementId("early"), 1, BasisRole.Custom(1, "early"), "early response"),
      BasisElement(BasisElementId("late"), 2, BasisRole.Custom(2, "late"), "late response")
    )
    val custom = Hrf.multiWithBasisElements("identified", elements, span = Seconds(8.0))(_ => Array(1.0, 2.0))
      .fold(error => fail(error.message), identity)
    assertEquals(custom.basisElementsValidated.toOption.get.map(_.id.value), Vector("early", "late"))
    assertEquals(custom.basisElements.map(_.label), Vector("early response", "late response"))

    val duplicate = elements.updated(1, elements(1).copy(id = elements(0).id))
    assert(Hrf.multiWithBasisElements("duplicate", duplicate, span = Seconds(8.0))(_ => Array(1.0, 2.0)).left.exists(_.isInstanceOf[BasisIdentityError.DuplicateId]))
    assert(Hrf.withBasisElements(Hrf.multi("wrong-width", 2)(_ => Array(1.0, 2.0)), Vector(elements.head)).left.exists(_.isInstanceOf[BasisIdentityError.CardinalityMismatch]))
    assert(Hrf.multiWithBasisElements("empty", Vector.empty, span = Seconds(8.0))(_ => Array.empty[Double]).left.exists(_.isInstanceOf[BasisIdentityError.CardinalityMismatch]))

  test("basis roles distinguish coefficient selection from an omnibus basis"):
    val basis = ResponseBasis.of(Hrfs.SPMG3)
    val canonical = basis.element(BasisRole.Canonical).fold(error => fail(error.message), identity)
    assertEquals(canonical.index, 1)
    assert(basis.element(BasisRole.FirBin(1, 0.s, 1.s)).isLeft)
    assertEquals(basis.elements.size, 3)

  test("response functionals compile to explicit basis weights with units"):
    val basis = ResponseBasis.of(Hrfs.SPMG3)
    val atSix = basis
      .responseFunctional(ResponseFunctional.At(6.s))
      .fold(error => fail(error.message), identity)
    assertEquals(atSix.units, ResponseUnits.ResponseValue)
    assertEquals(atSix.weights.dimension, 3)
    assertEquals(atSix.receipt.samples, 1)
    basis.kernel(Lag.ofSeconds(6.s)).data.zip(atSix.values).foreach { case (expected, actual) =>
      assertEqualsDouble(expected, actual, 1e-12)
    }

    val mean = basis
      .responseFunctional(
        ResponseFunctional.WindowMean(4.s, 8.s),
        FunctionalDiscretization.Exact
      )
      .fold(error => fail(error.message), identity)
    val integral = basis
      .responseFunctional(
        ResponseFunctional.WindowIntegral(4.s, 8.s),
        FunctionalDiscretization.Exact
      )
      .fold(error => fail(error.message), identity)
    assertEquals(mean.units, ResponseUnits.ResponseValue)
    assertEquals(integral.units, ResponseUnits.ResponseIntegral)
    mean.values.zip(integral.values).foreach { case (average, area) =>
      assertEqualsDouble(average * 4.0, area, 1e-12)
    }

  test("non-primitive windows require an explicit discretization"):
    val basis = ResponseBasis.of(Hrfs.invLogit())
    val exact = basis.responseFunctional(ResponseFunctional.WindowMean(4.s, 8.s))
    assert(exact.isLeft)
    val step = PositiveSeconds.fromSeconds(0.05.s).fold(error => fail(error.message), identity)
    val sampled = basis
      .responseFunctional(ResponseFunctional.WindowMean(4.s, 8.s), FunctionalDiscretization.Trapezoid(step))
      .fold(error => fail(error.message), identity)
    assertEquals(sampled.units, ResponseUnits.ResponseValue)
    assert(sampled.receipt.samples > 1)

  test("invalid response functionals and absent roles are typed failures"):
    val basis = ResponseBasis.of(Hrfs.SPMG3)
    assert(basis.responseFunctional(ResponseFunctional.At((-1.0).s)).isLeft)
    assert(basis.responseFunctional(ResponseFunctional.WindowMean(8.s, 4.s), FunctionalDiscretization.Exact).isLeft)
    assert(basis.element(BasisRole.Spline(1)).isLeft)
