package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.*

class ConvolutionDiscretizationSuite extends munit.FunSuite:
  private val box = Hrfs.boxcar(1.0.s)
  private val sampleLimit = ConvolutionDiscretization.MaxGridSamples
  private val cellLimit = ConvolutionDiscretization.MaxArrayCells

  test("sample budget includes zero and preserves floor semantics at the boundary"):
    assertEquals(ConvolutionDiscretization.sampleCount(sampleLimit - 1.0, 1.0, "test grid"), Right(sampleLimit))
    assertEquals(ConvolutionDiscretization.sampleCount(sampleLimit - 0.25, 1.0, "test grid"), Right(sampleLimit))
    assertEquals(
      ConvolutionDiscretization.sampleCount(sampleLimit.toDouble, 1.0, "test grid"),
      Left(ConvolutionError.SampleLimitExceeded("test grid", sampleLimit + 1.0, sampleLimit))
    )

  test("sample budget refuses overflow and finite requests beyond Int range before conversion"):
    for width <- Seq(Int.MaxValue.toDouble, Double.MaxValue) do
      val result = ConvolutionDiscretization.sampleCount(width, java.lang.Double.MIN_VALUE, "drive grid")
      result match
        case Left(ConvolutionError.SampleLimitExceeded("drive grid", requested, maximum)) =>
          assert(!requested.isFinite || requested > Int.MaxValue.toDouble)
          assertEquals(maximum, sampleLimit)
        case other => fail(s"expected a typed sample refusal, got $other")
    assertEquals(
      ConvolutionDiscretization.sampleCount(Int.MaxValue.toDouble, 1.0, "kernel grid"),
      Left(ConvolutionError.SampleLimitExceeded("kernel grid", Int.MaxValue.toDouble + 1.0, sampleLimit))
    )

  test("non-finite widths and non-positive or non-finite precision are typed refusals"):
    for precision <- Seq(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      assert(ConvolutionDiscretization.sampleCount(1.0, precision, "kernel grid").isLeft)
    for width <- Seq(-1.0, Double.NaN, Double.PositiveInfinity) do
      assert(ConvolutionDiscretization.sampleCount(width, 1.0, "drive grid").isLeft)
    assertEquals(ConvolutionDiscretization.sampleCount(0.0, 1.0, "kernel grid"), Right(1))

  test("cell budget refuses dimension products before integer multiplication"):
    assertEquals(ConvolutionDiscretization.cellCount(sampleLimit, 4.0, "sampled kernel"), Right(cellLimit))
    assertEquals(
      ConvolutionDiscretization.cellCount(sampleLimit, 5.0, "sampled kernel"),
      Left(ConvolutionError.CellLimitExceeded("sampled kernel", sampleLimit * 5.0, cellLimit))
    )
    assertEquals(
      ConvolutionDiscretization.cellCount(Int.MaxValue, 2.0, "rendered output"),
      Left(ConvolutionError.CellLimitExceeded("rendered output", Int.MaxValue.toDouble * 2.0, cellLimit))
    )

  test("kernel preparation refuses tiny precision without evaluating the HRF"):
    var calls = 0
    val kernel = Hrf.scalar("counted", span = 1.0.s): _ =>
      calls += 1
      1.0
    for precision <- Seq(1.0 / (sampleLimit + 0.5), java.lang.Double.MIN_VALUE) do
      Regressor.prepareConvolution(kernel, 1.0.s, Seconds(precision)) match
        case Left(ConvolutionError.SampleLimitExceeded("kernel grid", _, _)) => ()
        case other => fail(s"expected kernel grid refusal, got $other")
    assertEquals(calls, 0)
    assert(Regressor.prepareConvolution(kernel, 1.0.s, Seconds.unsafe(Double.NaN)).isLeft)

  test("kernel preparation accounts for every basis column before sampling"):
    var calls = 0
    val kernel = Hrf.multi("wide", 5, span = 1.0.s): _ =>
      calls += 1
      Array.fill(5)(1.0)
    Regressor.prepareConvolution(kernel, 1.0.s, Seconds(1.0 / (sampleLimit - 1.0))) match
      case Left(ConvolutionError.CellLimitExceeded("sampled kernel", _, _)) => ()
      case other => fail(s"expected kernel cells refusal, got $other")
    assertEquals(calls, 0)

  test("drive budget includes scan range, kernel horizon and long blocks"):
    val cases = Seq(
      (Regressor(Seq(0.0), box), Seq(0.0, sampleLimit.toDouble)),
      (Regressor(Seq(0.0), box, duration = Seq(sampleLimit.toDouble)), Seq(0.0)),
      (Regressor(Seq(0.0), box, duration = Seq(Double.MaxValue)), Seq(0.0))
    )
    for (reg, grid) <- cases do
      assert(Regressor.validateConvolution(reg, grid, 1.0).isLeft)
      for method <- Seq(Regressor.EvalMethod.Conv, Regressor.EvalMethod.FFT) do
        val error = intercept[IllegalArgumentException](reg.evaluate(grid, 1.0, method))
        assert(error.getMessage.contains("drive grid"))

  test("prepared evaluation returns a typed drive refusal and checks kernel identity"):
    val plan = Regressor.prepareConvolution(box, 1.0.s, 1.0.s).toOption.get
    val reg = Regressor(Seq(0.0), box)
    plan.evaluateEither(reg, Seq(0.0, sampleLimit.toDouble)) match
      case Left(ConvolutionError.SampleLimitExceeded("drive grid", _, _)) => ()
      case other => fail(s"expected typed drive refusal, got $other")
    assert(plan.evaluateEither(Regressor(Seq(0.0), Hrfs.boxcar(1.0.s)), Seq(0.0)).isLeft)
    assert(plan.evaluateEither(Regressor(Seq(0.0), box, span = Some(2.0)), Seq(0.0)).isLeft)
    assert(plan.evaluateEither(Regressor.perEvent(Seq(0.0), Seq(box)), Seq(0.0)).isLeft)
    assert(plan.evaluateEither(reg, Seq.empty).isLeft)
    assertEqualsDouble(plan.evaluateEither(reg, Seq(0.0)).toOption.get.data(0), 1.0, 1e-12)

  test("output budget also protects empty and loop evaluations"):
    val times = new scala.collection.immutable.IndexedSeq[Double]:
      def length: Int = cellLimit + 1
      def apply(index: Int): Double = fail("oversized output should be refused before inspecting the grid")
    for reg <- Seq(Regressor(Seq.empty, box), Regressor(Seq(0.0), box)) do
      assert(Regressor.validateConvolution(reg, times, 1.0).isLeft)
      for method <- Seq(Regressor.EvalMethod.Loop, Regressor.EvalMethod.Conv) do
        val error = intercept[IllegalArgumentException](reg.evaluate(times, 1.0, method))
        assert(error.getMessage.contains("rendered output"))

  test("finite grid endpoints whose difference overflows receive a typed span refusal"):
    val reg = Regressor(Seq(0.0), box)
    Regressor.validateConvolution(reg, Seq(-Double.MaxValue, Double.MaxValue), 1.0) match
      case Left(ConvolutionError.InvalidSpan(TimeError.NonFinite("drive grid", _))) => ()
      case other => fail(s"expected typed derived width refusal, got $other")

  test("zero-horizon and windowed-out schedules retain their existing behavior"):
    val zero = Regressor.fromEvents(
      Seq(StimulusEvent(0.0).toOption.get), HrfAssignment.Shared(box), 0.0.s, summate = true
    ).fold(error => fail(error.message), identity)
    assert(Regressor.validateConvolution(zero, Seq(0.0), 1.0).isRight)
    assertEqualsDouble(zero.evaluate(Seq(0.0), 1.0).data(0), 1.0, 1e-12)
    val empty = Regressor(Seq(10.0), box)
    assert(Regressor.validateConvolution(empty, Seq(0.0), java.lang.Double.MIN_VALUE).isRight)
    assertEqualsDouble(empty.evaluate(Seq(0.0), java.lang.Double.MIN_VALUE).data(0), 0.0, 0.0)
