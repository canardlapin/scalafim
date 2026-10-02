package scalafim.fmri.design.event

import scalafim.fmri.design.HrfColumnScaling
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class EventResponseConvolutionSuite extends munit.FunSuite:
  private val kernel = Hrfs.boxcar(1.0.s)
  private val unitPeak = EventResponseNormalization.unitPeak(0.1.s).fold(error => fail(error.message), identity)
  private val fineFrame = SamplingFrame(blockLens = Seq(50), tr = Seq(0.1), startTime = Seq(0.0), precision = 0.01)

  private def term(onsets: Vector[Double], durations: Vector[Double], blocks: Vector[Int] = Vector.empty): EventTerm =
    EventTerm(
      events = Vector(Event.factor(Vector.fill(onsets.length)("A"), "condition")),
      onsets = onsets.map(Seconds(_)),
      durations = durations.map(Seconds(_)),
      blockIds = if blocks.isEmpty then Vector.fill(onsets.length)(0) else blocks,
      termTag = Some("task")
    )

  private def maxAbs(data: Array[Double]): Double = data.iterator.map(math.abs).max

  test("event unit-peak normalization is per duration and retains duration receipts"):
    val source = term(Vector(0.0, 2.0, 4.0), Vector(0.0, 0.2, 0.5))
    val result = EventResponseConvolution.convolve(source, kernel, fineFrame, unitPeak).fold(error => fail(error.message), identity)
    assertEquals(result.eventPeakScales.map(_.duration.value), Vector(0.0, 0.2, 0.5))
    assert(result.eventPeakScales.map(_.divisors).distinct.length > 1, "different pulse durations retain distinct response scales")
    assertEqualsDouble(maxAbs(result.convolved.data.data), 1.0, 1e-12)
    assertEquals(result.convolved.columnConditions, Vector(Some("condition.A")))

  test("overlapping events add and events do not leak across runs"):
    val single = EventResponseConvolution.convolve(term(Vector(0.0), Vector(0.0)), kernel, fineFrame).fold(error => fail(error.message), identity)
    val doubled = EventResponseConvolution.convolve(term(Vector(0.0, 0.0), Vector(0.0, 0.0)), kernel, fineFrame).fold(error => fail(error.message), identity)
    single.convolved.data.data.indices.foreach { index =>
      assertEqualsDouble(doubled.convolved.data.data(index), 2.0 * single.convolved.data.data(index), 1e-12)
    }

    val frame = SamplingFrame(blockLens = Seq(5, 5), tr = Seq(1.0), startTime = Seq(0.0))
    val perRun = EventResponseConvolution.convolve(term(Vector(0.0, 0.0), Vector(0.0, 0.0), Vector(0, 1)), kernel, frame).fold(error => fail(error.message), identity)
    assertEqualsDouble(perRun.convolved.data.data(0), 1.0, 1e-12)
    assertEqualsDouble(perRun.convolved.data.data(5), 1.0, 1e-12)

  test("final column scaling remains separate from event normalization"):
    val result = EventResponseConvolution.convolve(
      term(Vector(0.0), Vector(1.0)),
      kernel,
      fineFrame,
      normalization = EventResponseNormalization.PreservePulseScale,
      scaling = HrfColumnScaling.UnitMaximumAbsolute
    ).fold(error => fail(error.message), identity)
    assertEquals(result.eventPeakScales.head.divisors, Vector(1.0))
    assertEqualsDouble(maxAbs(result.convolved.data.data), 1.0, 1e-12)
    assertEquals(result.convolved.columnScales.head.policy, HrfColumnScaling.UnitMaximumAbsolute)
