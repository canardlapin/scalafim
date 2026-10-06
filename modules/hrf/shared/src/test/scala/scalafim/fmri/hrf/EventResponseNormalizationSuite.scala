package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

class EventResponseNormalizationSuite extends munit.FunSuite:
  private val ramp = Hrf.scalar("ramp", span = Seconds(2.0), support = Support.Compact(Seconds(2.0))) { lag =>
    lag.value
  }

  test("unit peak scaling is per event duration and not kernel normalization"):
    val unitPeak = EventResponseNormalization.unitPeak(Seconds(0.001)).toOption.getOrElse(fail("reference step"))
    val short = EventResponse.prepare(ramp, Pulse.box(Seconds(1.0)).toOption.getOrElse(fail("short pulse")), unitPeak, Integration.Trapezoid).toOption.getOrElse(fail("short response"))
    val long = EventResponse.prepare(ramp, Pulse.box(Seconds(2.0)).toOption.getOrElse(fail("long pulse")), unitPeak, Integration.Trapezoid).toOption.getOrElse(fail("long response"))
    val unscaled = EventResponse.prepare(ramp, Pulse.box(Seconds(2.0)).toOption.getOrElse(fail("long pulse"))).toOption.getOrElse(fail("unscaled response"))

    assertEqualsDouble(unscaled.at(Lag(2.0)).data(0), 2.0, 1e-12)
    val grid = (0 to 4000).map(index => Lag(index * 0.001)).toVector
    val shortPeak = short.evaluate(grid).data.map(math.abs).max
    val longPeak = long.evaluate(grid).data.map(math.abs).max
    assertEqualsDouble(longPeak, 1.0, 1e-12)
    assertEqualsDouble(shortPeak, 1.0, 1e-12)
    assert(long.peakScales(0) > short.peakScales(0), "duration-specific peak scales should differ")

  test("unit peak reference step is validated at the typed boundary"):
    assert(EventResponseNormalization.unitPeak(Seconds(0.0)).isLeft)

  test("unit peak refuses an impractically dense reference grid before allocation"):
    val tinyStep = EventResponseNormalization.unitPeak(Seconds(1e-9)).toOption.getOrElse(fail("reference step"))
    assert(EventResponse.prepare(ramp, Pulse.Impulse, tinyStep).isLeft)

  test("unit peak rejects non-finite response values"):
    val nonFinite = Hrf.scalar("non-finite", span = Seconds(1.0), support = Support.Compact(Seconds(1.0))) { _ => Double.NaN }
    val policy = EventResponseNormalization.unitPeak(Seconds(0.1)).toOption.getOrElse(fail("reference step"))
    assert(EventResponse.prepare(nonFinite, Pulse.Impulse, policy).isLeft)

  test("pulse-scale responses evaluate at the caller's declared precision"):
    val pulse = Pulse.box(Seconds(2.0)).toOption.getOrElse(fail("pulse"))
    val fine = PositiveSeconds(0.01).fold(error => fail(error.toString), identity)
    val response = EventResponse.prepare(Hrfs.SPMG1, pulse, precision = fine).fold(error => fail(error.message), identity)
    assertEquals(response.precision.value, 0.01)
    val expected = PulseResponse.at(pulse, Hrfs.SPMG1, Lag(5.0), Seconds(0.01), Integration.Exact).data(0)
    assertEqualsDouble(response.at(Lag(5.0)).data(0), expected, 0.0)

  private def policy(step: Double): EventResponseNormalization =
    EventResponseNormalization.unitPeak(Seconds(step)).fold(error => fail(error.message), identity)

  private def workRefusal(result: Either[EventResponseNormalizationError, EventResponse]): Unit = result match
    case Left(EventResponseNormalizationError.ReferenceWorkTooLarge(_, maximum)) =>
      assertEquals(maximum, EventResponse.MaximumReferenceEvaluations)
    case other => fail(s"expected composed reference work refusal, got $other")

  test("composed trapezoid work is refused before evaluating the kernel"):
    var calls = 0
    val kernel = Hrf.scalar("composed-sentinel", span = 1.0.s): _ =>
      calls += 1
      fail("excessive response work must be refused before evaluation")
    val pulse = Pulse.box(1.0.s).toOption.get
    for integration <- Seq(Integration.Trapezoid, Integration.Exact) do
      workRefusal(EventResponse.prepare(kernel, pulse, policy(0.0001), integration))
    assertEquals(calls, 0)

  test("reference endpoint overflow is a typed refusal before evaluation"):
    var calls = 0
    val kernel = Hrf.scalar("endpoint-sentinel", span = Seconds(Double.MaxValue)): _ =>
      calls += 1
      fail("non-finite reference lag must be refused before evaluation")
    EventResponse.prepare(kernel, Pulse.Impulse, policy(Double.MaxValue * 0.75)) match
      case Left(EventResponseNormalizationError.NonFiniteReferenceTime(value)) => assert(!value.isFinite)
      case other => fail(s"expected non-finite reference time, got $other")
    assertEquals(calls, 0)

  test("the reference sample and scalar-work limits admit their exact boundary"):
    var calls = 0
    val limit = EventResponse.MaximumReferenceSamples
    val kernel = Hrf.multi("boundary", 10, span = Seconds(limit - 1.0)): _ =>
      calls += 1
      throw new IllegalStateException("admitted first sample")
    // Stop at the first callback: admission at the full limits requires no
    // deliberately expensive normalization run.
    val error = intercept[IllegalStateException](EventResponse.prepare(kernel, Pulse.Impulse, policy(1.0)))
    assertEquals(error.getMessage, "admitted first sample")
    assertEquals(calls, 1)
    val wider = Hrf.multi("over-work", 11, span = kernel.span)(_ => fail("work must be refused"))
    workRefusal(EventResponse.prepare(wider, Pulse.Impulse, policy(1.0)))
    val longer = Hrf.scalar("over-samples", span = Seconds(limit.toDouble))(_ => fail("samples must be refused"))
    EventResponse.prepare(longer, Pulse.Impulse, policy(1.0)) match
      case Left(EventResponseNormalizationError.ReferenceGridTooLarge(samples, maximum)) =>
        assertEqualsDouble(samples, limit + 1.0, 0.0)
        assertEquals(maximum, limit)
      case other => fail(s"expected reference sample refusal, got $other")

  test("nested blocked, lagged and bound kernels contribute their response work"):
    var calls = 0
    val kernel = Hrf.scalar("nested-sentinel", span = 1.0.s): _ =>
      calls += 1
      fail("nested response work must be refused before evaluation")
    val blocked = kernel.block(0.01.s, precision = 0.0001.s).block(0.01.s, precision = 0.0001.s)
    val combined = HrfCombinators.bindBasis(Vector(blocked.lag(0.1.s), kernel))
    workRefusal(EventResponse.prepare(combined, Pulse.Impulse, policy(0.001)))
    workRefusal(EventResponse.prepare(combined, Pulse.boxMass(0.1.s).toOption.get, policy(0.001)))
    assertEquals(calls, 0)

  test("exact closed primitives admit work that trapezoids would exceed"):
    val pulse = Pulse.box(32.0.s).toOption.get
    val exact = EventResponse.prepare(Hrfs.boxcar(32.0.s), pulse, policy(0.01), Integration.Exact)
      .fold(error => fail(error.message), identity)
    assertEqualsDouble(exact.peakScales.head, 32.0, 1e-12)
    assertEqualsDouble(exact.at(Lag(32.0)).data(0), 1.0, 1e-12)
    workRefusal(EventResponse.prepare(Hrfs.boxcar(32.0.s), pulse, policy(0.01), Integration.Trapezoid))

  test("exact piecewise response work includes all segments before evaluation"):
    var calls = 0
    val descriptor = HrfDescriptor.custom("piecewise-sentinel", 1, 10_000.0.s).copy(
      integration = IntegrationPolicy.PiecewisePolynomial(Vector.tabulate(1001)(i => Seconds(i.toDouble * 10.0)), 0)
    )
    val kernel = Hrf.scalar("piecewise-sentinel", span = descriptor.span, descriptor = Some(descriptor)): _ =>
      calls += 1
      fail("piecewise response work must be refused before evaluation")
    workRefusal(EventResponse.prepare(kernel, Pulse.box(1.0.s).toOption.get, policy(1.0)))
    assertEquals(calls, 0)

  test("empty exact pieces retain the declared output-width work floor"):
    val basis = 100_001
    for breaks <- Seq(Vector.empty[Seconds], Vector(0.0.s)) do
      val descriptor = HrfDescriptor.custom("empty-pieces", basis, 1.0.s).copy(
        integration = IntegrationPolicy.PiecewisePolynomial(breaks, 0)
      )
      val kernel = Hrf.multi("empty-pieces", basis, span = descriptor.span, descriptor = Some(descriptor)):
        _ => fail("empty pieces must be refused before allocating basis arrays")
      workRefusal(EventResponse.prepare(kernel, Pulse.box(1.0.s).toOption.get, policy(0.01)))

  test("unit-peak uses its reference step and ignores unused caller precision"):
    val tiny = PositiveSeconds(java.lang.Double.MIN_VALUE).toOption.get
    val normal = EventResponse.prepare(ramp, Pulse.Impulse, policy(0.1)).toOption.get
    val unused = EventResponse.prepare(ramp, Pulse.Impulse, policy(0.1), precision = tiny).toOption.get
    assertEqualsDouble(unused.precision.value, 0.1, 0.0)
    assertEquals(unused.peakScales, normal.peakScales)

  test("preserve-pulse-scale preparation performs no normalization admission or evaluation"):
    var calls = 0
    val kernel = Hrf.scalar("preserve-sentinel", span = Seconds(Double.MaxValue)): _ =>
      calls += 1
      fail("preserved preparation must not evaluate the kernel")
    val tiny = PositiveSeconds(java.lang.Double.MIN_VALUE).toOption.get
    val response = EventResponse.prepare(kernel, Pulse.box(1.0.s).toOption.get,
      EventResponseNormalization.PreservePulseScale, Integration.Trapezoid, tiny).toOption.get
    assertEquals(response.peakScales, Vector(1.0))
    assertEqualsDouble(response.precision.value, tiny.value, 0.0)
    assertEquals(calls, 0)

  test("basis scale allocation is bounded for preserved and unit-peak responses"):
    var calls = 0
    val basis = EventResponse.MaximumBasisScales + 1
    val kernel = Hrf.multi("scale-sentinel", basis, span = 1.0.s): _ =>
      calls += 1
      fail("scale shape must be refused before evaluation")
    for normalization <- Seq(EventResponseNormalization.PreservePulseScale, policy(0.1)) do
      EventResponse.prepare(kernel, Pulse.Impulse, normalization) match
        case Left(EventResponseNormalizationError.ScaleLimitExceeded(actual, maximum)) =>
          assertEquals(actual, basis)
          assertEquals(maximum, EventResponse.MaximumBasisScales)
        case _ => fail("expected basis-scale allocation refusal")
    assertEquals(calls, 0)

  test("unit peak retains independent basis factors and pulse mass conventions"):
    val kernel = Hrf.multi("two-ramps", 2, span = 2.0.s, support = Support.Compact(2.0.s)):
      lag => Array(lag.value, -2.0 * lag.value)
    val height = EventResponse.prepare(kernel, Pulse.box(0.5.s).toOption.get, policy(0.1), Integration.Trapezoid).toOption.get
    val mass = EventResponse.prepare(kernel, Pulse.boxMass(0.5.s).toOption.get, policy(0.1), Integration.Trapezoid).toOption.get
    assertEqualsDouble(height.peakScales(1), 2.0 * height.peakScales(0), 1e-12)
    height.peakScales.zip(mass.peakScales).foreach:
      (h, m) => assertEqualsDouble(m, h / 0.5, 1e-12)
    height.at(Lag(1.5)).data.zip(mass.at(Lag(1.5)).data).foreach:
      (h, m) => assertEqualsDouble(h, m, 1e-12)
