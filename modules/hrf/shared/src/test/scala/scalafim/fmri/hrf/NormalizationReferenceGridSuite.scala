package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

class NormalizationReferenceGridSuite extends munit.FunSuite:

  private def sampleLimit(result: Either[HrfNormalizationError, ?]): Unit = result match
    case Left(HrfNormalizationError.ReferenceSampleLimitExceeded(_, _, maximum)) =>
      assertEquals(maximum, NormalizationReferenceGrid.MaxSamples)
    case other => fail(s"expected reference sample refusal, got $other")

  private def workLimit(result: Either[HrfNormalizationError, ?]): Unit = result match
    case Left(HrfNormalizationError.ReferenceWorkLimitExceeded(_, _, maximum)) =>
      assertEquals(maximum, NormalizationReferenceGrid.MaxScalarEvaluations)
    case other => fail(s"expected reference work refusal, got $other")

  test("legacy sample budget admits the exact boundary and refuses the next sample"):
    val limit = NormalizationReferenceGrid.MaxSamples
    assertEquals(NormalizationReferenceGrid.stepped("boundary", Seconds(limit - 1.0), 1.0.s, 1.0), Right(limit))
    sampleLimit(NormalizationReferenceGrid.stepped("boundary", Seconds(limit.toDouble), 1.0.s, 1.0))

  test("legacy normalization refuses tiny steps before kernel evaluation"):
    var evaluations = 0
    val kernel = Hrf.scalar("sentinel"): _ =>
      evaluations += 1
      fail("normalization must refuse before evaluating")
    Vector(1e-300.s, Seconds(24.0 / NormalizationReferenceGrid.MaxSamples.toDouble)).foreach: step =>
      sampleLimit(kernel.normalizeWithTransformEither(step))
      assert(intercept[IllegalArgumentException](kernel.normalizeWithTransform(step)).getMessage.contains("reference samples"))
    assertEquals(evaluations, 0)

  test("legacy grid rejects a finite span whose ceil endpoint overflows"):
    var evaluations = 0
    val kernel = Hrf.scalar("overshoot", span = Seconds(Double.MaxValue)): _ =>
      evaluations += 1
      fail("non-finite endpoint must be refused before evaluation")
    kernel.normalizeWithTransformEither(Seconds(Double.MaxValue * 0.75)) match
      case Left(HrfNormalizationError.NonFiniteReferenceTime(_, value)) => assert(!value.isFinite)
      case other => fail(s"expected endpoint refusal, got $other")
    assertEquals(evaluations, 0)

  test("legacy admitted grid preserves the ceil endpoint and per-basis scale"):
    val times = scala.collection.mutable.ArrayBuffer.empty[Double]
    val kernel = Hrf.scalar("ceil", span = 1.0.s): lag =>
      times += lag.value
      2.0 * lag.value
    val normalized = kernel.normalizeWithTransformEither(0.4.s).toOption.get
    assertEquals(times.toVector.length, 4)
    times.toVector.zip(Vector(0.0, 0.4, 0.8, 1.2)).foreach: (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-15)
    normalized.transform match
      case BasisTransform.Diagonal(scales) => assertEqualsDouble(scales.head, 2.4, 1e-15)
      case other => fail(s"expected diagonal transform, got $other")

  test("fixed reference budget admits its boundary and refuses the next sample"):
    val limit = NormalizationReferenceGrid.MaxSamples
    val end = (limit - 1).toDouble / 50.0
    assertEquals(NormalizationReferenceGrid.fixed("boundary", Seconds(end), HrfNormalization.UnitPeak, 1.0).map(_._1), Right(limit))
    sampleLimit(NormalizationReferenceGrid.fixed("boundary", Seconds(limit.toDouble / 50.0), HrfNormalization.UnitPeak, 1.0))

  test("fixed normalization refuses oversized and overflowing spans before evaluation"):
    Vector(NormalizationReferenceGrid.MaxSamples.toDouble / 50.0, Double.MaxValue).foreach: span =>
      var evaluations = 0
      val kernel = Hrf.scalar("fixed-sentinel", span = Seconds(span)): _ =>
        evaluations += 1
        fail("oversized fixed grid must be refused before evaluation")
      Vector(HrfNormalization.UnitPeak, HrfNormalization.UnitIntegral, HrfNormalization.UnitPeakPerBasis).foreach: mode =>
        sampleLimit(kernel.normalize(mode))
      assertEquals(evaluations, 0)

  test("SPM remains a 1600-sample grid even for a very large finite span"):
    var evaluations = 0
    var last = 0.0
    val kernel = Hrf.scalar("spm", span = Seconds(Double.MaxValue)): lag =>
      evaluations += 1
      last = lag.value
      1.0
    assert(kernel.normalize(HrfNormalization.Spm).isRight)
    assertEquals(evaluations, 1600)
    assertEqualsDouble(last, 32.0, 1e-12)

  test("block normalization refuses composite quadrature work before evaluation"):
    var evaluations = 0
    val kernel = Hrf.scalar("block-sentinel", span = 1.0.s): _ =>
      evaluations += 1
      fail("composite work must be refused before evaluating")
    // Each grid has about 10,000 samples; their product exceeds 100 million.
    val error = intercept[IllegalArgumentException](kernel.block(1.0.s, precision = 0.0001.s, normalize = true))
    assert(error.getMessage.contains("scalar evaluations"))
    val blocked = kernel.block(1.0.s, precision = 0.0001.s)
    workLimit(blocked.normalizeWithTransformEither(0.0001.s))
    workLimit(blocked.normalize(HrfNormalization.Spm))
    assertEquals(evaluations, 0)

  test("work budget includes basis columns and admits the exact boundary"):
    val limit = NormalizationReferenceGrid.MaxScalarEvaluations.toDouble
    assertEquals(NormalizationReferenceGrid.stepped("work", 1.0.s, 1.0.s, limit / 2.0), Right(2))
    workLimit(NormalizationReferenceGrid.stepped("work", 1.0.s, 1.0.s, limit / 2.0 + 1.0))

  test("nested block work survives lagging and binding basis descriptors"):
    var evaluations = 0
    val kernel = Hrf.scalar("nested-sentinel", span = 1.0.s): _ =>
      evaluations += 1
      fail("nested work must be refused before evaluation")
    val inner = kernel.block(0.01.s, precision = 0.0001.s)
    val outer = inner.block(0.01.s, precision = 0.0001.s)
    workLimit(outer.normalize(HrfNormalization.Spm))
    workLimit(outer.lag(0.1.s).normalize(HrfNormalization.Spm))
    workLimit(HrfCombinators.bindBasis(Vector(outer.lag(0.1.s), kernel)).normalize(HrfNormalization.Spm))
    assertEquals(evaluations, 0)

  test("deep block descriptors are admitted in linear traversal work"):
    var kernel: Hrf = Hrf.scalar("deep-sentinel", span = 1.0.s)(_ => fail("deep work must be refused"))
    var depth = 0
    while depth < 256 do
      kernel = kernel.block(0.01.s, precision = 0.01.s)
      depth += 1
    workLimit(kernel.normalize(HrfNormalization.Spm))

  test("block work budget covers exact piecewise integration with coarse precision"):
    var evaluations = 0
    val descriptor = HrfDescriptor.custom("piecewise-sentinel", 1, 10_000.0.s).copy(
      integration = IntegrationPolicy.PiecewisePolynomial(Vector.tabulate(1001)(i => Seconds(i.toDouble * 10.0)), 0)
    )
    val kernel = Hrf.scalar("piecewise-sentinel", span = 10_000.0.s, descriptor = Some(descriptor)): _ =>
      evaluations += 1
      fail("exact composite work must be refused before evaluation")
    val error = intercept[IllegalArgumentException](kernel.block(1.0.s, precision = 1.0.s, normalize = true))
    assert(error.getMessage.contains("scalar evaluations"))
    assertEquals(evaluations, 0)

  test("known exact primitives avoid charging unused quadrature evaluations"):
    var evaluations = 0
    val descriptor = HrfDescriptor.custom("closed-sentinel", 1, 24.0.s).copy(
      integration = IntegrationPolicy.Boxcar(24.0.s, 1.0)
    )
    val kernel = Hrf.scalar("closed-sentinel", descriptor = Some(descriptor)): _ =>
      evaluations += 1
      fail("closed box primitive must not evaluate its kernel")
    val normalized = kernel.block(32.0.s, precision = 0.01.s, normalize = true)
    assertEqualsDouble(normalized(Lag(24.0)).data(0), 1.0, 1e-12)
    assertEquals(evaluations, 0)

  test("empty exact pieces still charge the returned vector width without allocation"):
    val columns = NormalizationReferenceGrid.MaxScalarEvaluations + 1
    Vector(Vector.empty[Seconds], Vector(0.0.s)).foreach: breaks =>
      val descriptor = HrfDescriptor.custom("empty-piece", columns, 1.0.s).copy(
        integration = IntegrationPolicy.PiecewisePolynomial(breaks, 0)
      )
      val work = NormalizationReferenceGrid.blockedSampleWork(descriptor, 1.0.s, 1.0.s,
        Double.PositiveInfinity, Integration.Exact)
      assertEqualsDouble(work, columns.toDouble, 0.0)
      workLimit(NormalizationReferenceGrid.stepped("empty-piece", 2.0.s, 1.0.s, work))
      workLimit(NormalizationReferenceGrid.fixed("empty-piece", 2.0.s, HrfNormalization.UnitPeak, work))

  test("huge empty-piece block normalization refuses before basis arrays"):
    val columns = NormalizationReferenceGrid.MaxScalarEvaluations + 1
    val descriptor = HrfDescriptor.custom("huge-empty-piece", columns, 1.0.s).copy(
      integration = IntegrationPolicy.PiecewisePolynomial(Vector.empty, 0)
    )
    val kernel = Hrf.multi("huge-empty-piece", columns, span = 1.0.s, descriptor = Some(descriptor)):
      _ => fail("metadata-only kernel must not be evaluated")
    val error = intercept[IllegalArgumentException](kernel.block(1.0.s, precision = 1.0.s, normalize = true))
    assert(error.getMessage.contains("scalar evaluations"))

  test("incomplete component metadata cannot hide output basis width"):
    val columns = 200_001
    val descriptor = HrfDescriptor.custom("wide-components", columns, 1.0.s).copy(
      components = Vector(HrfDescriptor.custom("one-column", 1, 1.0.s))
    )
    assertEqualsDouble(NormalizationReferenceGrid.evaluationWork(descriptor), columns.toDouble, 0.0)
    var evaluations = 0
    // Canonical fixed peak normalization allocates one factor before calling
    // the kernel; a missing floor therefore hits this sentinel without any
    // array of `columns` elements, keeping mutation verification bounded.
    val kernel = Hrf.multi("wide-components", columns, span = 1.0.s, descriptor = Some(descriptor)):
      _ =>
        evaluations += 1
        fail("output basis work must be refused before evaluation")
    workLimit(kernel.normalize(HrfNormalization.UnitPeak))
    assertEquals(evaluations, 0)

  test("HrfSpec keeps tiny-width normalization refusal typed"):
    val spec = HrfSpec(HrfKind.Spmg1, width = 1e-12.s, precision = 1e-12.s, normalize = true).toOption.get
    spec.toHrf match
      case Left(HrfSpecError.InvalidNormalization(error)) => sampleLimit(Left(error))
      case other => fail(s"expected typed spec refusal, got $other")

  test("HrfSpec keeps derived horizon overflow typed"):
    val spec = HrfSpec(HrfKind.Gamma, span = Seconds(Double.MaxValue), lag = Seconds(Double.MaxValue)).toOption.get
    spec.toHrf match
      case Left(HrfSpecError.InvalidNormalization(HrfNormalizationError.InvalidReferenceSpan(_, span))) =>
        assert(!span.value.isFinite)
      case other => fail(s"expected typed horizon refusal, got $other")
