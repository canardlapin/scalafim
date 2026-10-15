package scalafim.fmri.hrf

import HrfCombinators.*

class QuadratureSuite extends munit.FunSuite:
  private def refused(width: Double, step: Double): QuadratureError.WorkLimitExceeded =
    Quadrature.boxOffsetsEither(width, step) match
      case Left(error: QuadratureError.WorkLimitExceeded) => error
      case other => fail(s"expected a quadrature work-limit refusal, got $other")

  test("box quadrature refuses one interval above its work limit"):
    val requested = Quadrature.MaxBoxIntervals.toDouble + 1.0
    val error = refused(requested, 1.0)
    assertEqualsDouble(error.intervals, requested, 0.0)
    assertEquals(error.maximum, Quadrature.MaxBoxIntervals)

  test("box quadrature refuses counts that overflow Int or Double"):
    val intOverflow = refused(Int.MaxValue.toDouble, 1.0)
    assertEqualsDouble(intOverflow.intervals, Int.MaxValue.toDouble, 0.0)
    val doubleOverflow = refused(1.0, java.lang.Double.MIN_VALUE)
    assert(doubleOverflow.intervals.isPosInfinity)
    assertEquals(doubleOverflow.maximum, Quadrature.MaxBoxIntervals)

  test("box quadrature counts the partial endpoint against the budget"):
    val error = refused(Quadrature.MaxBoxIntervals.toDouble + 0.25, 1.0)
    assertEqualsDouble(error.intervals, Quadrature.MaxBoxIntervals.toDouble + 1.0, 0.0)
    // Division rounds to an integer, but the existing offset grid still needs
    // one more endpoint. A ceil(width / step) check alone misses this case.
    val width = 517947.26164718566
    val step = 0.5179472616471856
    assertEqualsDouble(width / step, Quadrature.MaxBoxIntervals.toDouble, 0.0)
    assertEqualsDouble(refused(width, step).intervals, Quadrature.MaxBoxIntervals.toDouble + 1.0, 0.0)

  test("box quadrature accepts its exact work limit"):
    val width = Quadrature.MaxBoxIntervals.toDouble
    val (offsets, weights) = Quadrature.boxOffsetsEither(width, 1.0).toOption.get
    assertEquals(offsets.length, Quadrature.MaxBoxIntervals + 1)
    assertEquals(weights.length, offsets.length)
    assertEqualsDouble(offsets.last, width, 0.0)
    assertEqualsDouble(weights.sum, width, 0.0)

  test("box quadrature rejects non-finite and invalid inputs"):
    for width <- Seq(-1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assert(Quadrature.boxOffsetsEither(width, 1.0).left.toOption.get.isInstanceOf[QuadratureError.InvalidWidth])
    for step <- Seq(0.0, -1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assert(Quadrature.boxOffsetsEither(1.0, step).left.toOption.get.isInstanceOf[QuadratureError.InvalidStep])

  test("box quadrature preserves the impulse and partial-interval weights"):
    val (impulse, mass) = Quadrature.boxOffsetsEither(0.0, java.lang.Double.MIN_VALUE).toOption.get
    assertEquals(impulse.length, 1)
    assertEqualsDouble(impulse(0), 0.0, 0.0)
    assertEqualsDouble(mass(0), 1.0, 0.0)
    val (offsets, weights) = Quadrature.boxOffsetsEither(1.0, 0.3).toOption.get
    val expectedOffsets = Array(0.0, 0.3, 0.6, 0.9, 1.0)
    val expectedWeights = Array(0.15, 0.3, 0.3, 0.2, 0.05)
    assertEquals(offsets.length, expectedOffsets.length)
    offsets.indices.foreach: i =>
      assertEqualsDouble(offsets(i), expectedOffsets(i), 1e-15)
      assertEqualsDouble(weights(i), expectedWeights(i), 1e-15)

  test("legacy block and pulse evaluation refuse excessive quadrature before kernel evaluation"):
    var evaluated = false
    val kernel = Hrf.of("probe", nbasis = 1, span = 2.0.s): _ =>
      evaluated = true
      scalafim.fmri.hrf.linalg.Vec.unsafe(Array(1.0))
    val step = Seconds(java.lang.Double.MIN_VALUE)
    val blockError = intercept[IllegalArgumentException](kernel.block(1.0.s, precision = step))
    assert(blockError.getMessage.contains("exceeding maximum"))
    val pulse = Pulse.box(1.0.s).toOption.get
    for integration <- Seq(Integration.Trapezoid, Integration.Exact) do
      val error = intercept[IllegalArgumentException](PulseResponse.at(pulse, kernel, Lag(1.0), step, integration))
      assert(error.getMessage.contains("exceeding maximum"))
    assert(!evaluated)

  test("closed-form pulse evaluation does not spend the quadrature budget"):
    val pulse = Pulse.box(1.0.s).toOption.get
    val tiny = PulseResponse.at(pulse, Hrfs.SPMG1, Lag(5.0), Seconds(java.lang.Double.MIN_VALUE))
    val ordinary = PulseResponse.at(pulse, Hrfs.SPMG1, Lag(5.0), 0.1.s)
    assertEqualsDouble(tiny.data(0), ordinary.data(0), 0.0)

  test("HRF specifications return a typed quadrature refusal"):
    val spec = HrfSpec(HrfKind.Spmg1, width = 1.0.s, precision = Seconds(java.lang.Double.MIN_VALUE)).toOption.get
    for result <- Seq(spec.toHrf, spec.toLegacyHrf) do
      result match
        case Left(HrfSpecError.InvalidQuadrature(_: QuadratureError.WorkLimitExceeded)) => ()
        case other => fail(s"expected typed quadrature refusal, got $other")
