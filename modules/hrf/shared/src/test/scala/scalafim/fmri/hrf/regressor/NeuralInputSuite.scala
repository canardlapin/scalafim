package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.*

class NeuralInputSuite extends munit.FunSuite:
  private val box = Hrfs.boxcar(1.0.s)
  private val empty = Regressor(Seq.empty, box)
  private val sampleLimit = NeuralInput.MaxGridSamples

  private def values(actual: Array[Double], expected: Seq[Double]): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), 1e-12)
      index += 1

  private def preview(
      reg: Regressor,
      from: Double,
      to: Double,
      resolution: Double
  ): (Array[Double], Array[Double]) =
    reg.neuralInputEither(from, Some(to), resolution).fold(error => fail(error.message), identity)

  test("impulses, overlapping blocks and inclusive end bins retain amplitude semantics"):
    for summate <- Seq(true, false) do
      val reg = Regressor(
        Seq(0.2, 0.4, 1.0), box,
        duration = Seq(0.0, 1.0, 0.5), amplitude = Seq(2.0, 3.0, -1.0),
        summate = summate
      )
      val (times, neural) = preview(reg, 0.0, 2.0, 0.5)
      values(times, Seq(0.0, 0.5, 1.0, 1.5, 2.0))
      values(neural, Seq(5.0, 3.0, 2.0, -1.0, 0.0))

  test("fractional last bin retains impulses beyond the requested endpoint"):
    val reg = Regressor(Seq(1.05, 1.2), box, amplitude = Seq(2.0, 8.0))
    val (times, neural) = preview(reg, 0.0, 1.0, 0.3)
    values(times, Seq(0.0, 0.3, 0.6, 0.9))
    values(neural, Seq(0.0, 0.0, 0.0, 2.0))

  test("blocks clip both window edges while retaining inclusive floor bins"):
    val reg = Regressor(Seq(0.0, 3.5, 5.0), box,
      duration = Seq(3.0, 10.0, 1.0), amplitude = Seq(2.0, 3.0, 100.0))
    val (times, neural) = preview(reg, 2.0, 4.0, 1.0)
    values(times, Seq(2.0, 3.0, 4.0))
    values(neural, Seq(2.0, 5.0, 3.0))

  test("huge blocks overlapping a tiny window skip all preceding bins"):
    val reg = Regressor(Seq(0.0), box, duration = Seq(Double.MaxValue), amplitude = Seq(2.0))
    val (_, neural) = preview(reg, 1e100, 1e100, java.lang.Double.MIN_VALUE)
    values(neural, Seq(2.0))

  test("outside impulses whose bin ratios overflow never saturate into the window"):
    val early = Regressor(Seq(0.0), box)
    val late = Regressor(Seq(Double.MaxValue), box)
    values(preview(early, 1e100, 1e100, java.lang.Double.MIN_VALUE)._2, Seq(0.0))
    values(preview(late, 0.0, 0.0, java.lang.Double.MIN_VALUE)._2, Seq(0.0))

  test("default window ends ten seconds after the latest event end"):
    val reg = Regressor(Seq(1.0, 5.0), box, duration = Seq(2.0, 1.0))
    val (times, _) = NeuralInput.either(reg, resolution = 1.0).fold(error => fail(error.message), identity)
    assertEquals(times.length, 17)
    assertEqualsDouble(times.last, 16.0, 0.0)
    values(empty.neuralInputEither(from = -2.0).toOption.get._1, Seq(-2.0))

  test("sample budget admits the exact cap and retains floor semantics"):
    val (times, neural) = preview(empty, 0.0, sampleLimit - 0.25, 1.0)
    assertEquals(times.length, sampleLimit)
    assertEquals(neural.length, sampleLimit)
    assertEqualsDouble(times.last, sampleLimit - 1.0, 0.0)
    assertEqualsDouble(neural.last, 0.0, 0.0)

  test("sample budget refuses one sample beyond the cap before allocation"):
    empty.neuralInputEither(to = Some(sampleLimit.toDouble), resolution = 1.0) match
      case Left(error) =>
        assertEquals(error, NeuralInputError.InvalidGrid(
          ConvolutionError.SampleLimitExceeded("neural input grid", sampleLimit + 1.0, sampleLimit)
        ))
      case Right((times, _)) => fail(s"expected a typed sample refusal, got ${times.length} samples")

  test("tiny steps and ratios beyond Int range receive typed sample refusals"):
    for (end, step) <- Seq(
      (1.0, java.lang.Double.MIN_VALUE), (Double.MaxValue, 1.0), (Int.MaxValue.toDouble, 1.0)
    ) do
      empty.neuralInputEither(to = Some(end), resolution = step) match
        case Left(NeuralInputError.InvalidGrid(ConvolutionError.SampleLimitExceeded("neural input grid", _, maximum))) =>
          assertEquals(maximum, sampleLimit)
        case other => fail(s"expected a typed sample refusal, got $other")

  test("invalid resolution is refused even for a zero-width grid"):
    for step <- Seq(0.0, -1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      empty.neuralInputEither(to = Some(0.0), resolution = step) match
        case Left(NeuralInputError.InvalidGrid(ConvolutionError.InvalidPrecision(_))) => ()
        case other => fail(s"expected invalid resolution, got $other")

  test("non-finite endpoints and reversed or overflowing ranges are typed refusals"):
    for endpoint <- Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      empty.neuralInputEither(from = endpoint) match
        case Left(NeuralInputError.InvalidFrom(_)) => ()
        case other => fail(s"expected invalid from, got $other")
      empty.neuralInputEither(to = Some(endpoint)) match
        case Left(NeuralInputError.InvalidTo(_)) => ()
        case other => fail(s"expected invalid to, got $other")
    for (from, end) <- Seq((2.0, 1.0), (-Double.MaxValue, Double.MaxValue)) do
      empty.neuralInputEither(from, Some(end), 1.0) match
        case Left(NeuralInputError.InvalidGrid(ConvolutionError.InvalidSpan(_))) => ()
        case other => fail(s"expected invalid derived span, got $other")

  test("overflowing event ends are refused before default or explicit preview allocation"):
    val reg = Regressor(Seq(Double.MaxValue), box, duration = Seq(Double.MaxValue))
    for end <- Seq(None, Some(0.0)) do
      reg.neuralInputEither(to = end) match
        case Left(NeuralInputError.InvalidEventEnd(0, value)) => assert(!value.isFinite && value > 0.0)
        case other => fail(s"expected invalid event end, got $other")

  test("legacy facades report the typed error message and preserve valid results"):
    val failure = empty.neuralInputEither(to = Some(sampleLimit.toDouble), resolution = 1.0).left.toOption.get
    val error = intercept[IllegalArgumentException](empty.neuralInput(to = Some(sampleLimit.toDouble), resolution = 1.0))
    assertEquals(error.getMessage, failure.message)
    val reg = Regressor(Seq(1.0), box, amplitude = Seq(3.0))
    val (times, neural) = reg.neuralInput(to = Some(2.0), resolution = 1.0)
    values(times, Seq(0.0, 1.0, 2.0))
    values(neural, Seq(0.0, 3.0, 0.0))
