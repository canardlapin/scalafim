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

    // A microtime grid aligned with the onsets makes the boxcar sample exactly
    // 1 at its onset; the default 0.3 s grid would split the impulse between bins.
    val frame = SamplingFrame(blockLens = Seq(5, 5), tr = Seq(1.0), startTime = Seq(0.0))
    val perRun = EventResponseConvolution.convolve(term(Vector(0.0, 0.0), Vector(0.0, 0.0), Vector(0, 1)), kernel, frame, precision = 0.1.s).fold(error => fail(error.message), identity)
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

  private val twoRuns = SamplingFrame(blockLens = Seq(30, 30), tr = Seq(1.0, 1.0))

  private def mixed(durations: Vector[Double]): EventTerm =
    // Onsets before each run start (-3 s) and an event near a run boundary.
    val onsets = Vector(-3.0, 2.0, 9.5, -3.0, 4.0, 27.0)
    EventTerm(
      events = Vector(Event.factor(Vector("a", "b", "a", "b", "a", "b"), "condition")),
      onsets = onsets.map(Seconds(_)),
      durations = durations.map(Seconds(_)),
      blockIds = Vector(0, 0, 0, 1, 1, 1),
      termTag = Some("task")
    )

  test("as-convolved is exactly the plain convolution and records no per-event scales"):
    val source = mixed(Vector(0.0, 2.0, 5.0, 1.0, 0.0, 3.0))
    Vector(0.3.s, 0.07.s).foreach: precision =>
      val plain = source.convolve(Hrfs.SPMG3, twoRuns, precision = precision)
      val declared = EventResponseConvolution
        .convolve(source, Hrfs.SPMG3, twoRuns, EventResponseNormalization.PreservePulseScale, precision = precision)
        .fold(error => fail(error.message), identity)
      assertEquals(declared.convolved.data.data.toVector, plain.data.data.toVector)
      assert(declared.convolved.eventPeakScales.isEmpty)
      assert(declared.convolved.columnEventScales.isEmpty)
      assert(ConvolvedBasisOrthogonalization(declared.convolved).isRight, "as-convolved must not block basis orthogonalization")

  test("unit-peak honors the caller's precision and equals the plain path divided by one shared scale"):
    // One duration: each basis column has one divisor, so unit-peak must be the
    // plain convolution (same microtime grid, span truncation and onset window)
    // divided column-wise by that divisor.
    val source = mixed(Vector.fill(6)(2.0))
    Vector(0.3.s, 0.05.s).foreach: precision =>
      val plain = source.convolve(Hrfs.SPMG2, twoRuns, precision = precision)
      val normalized = EventResponseConvolution.convolve(source, Hrfs.SPMG2, twoRuns, unitPeak, precision = precision)
        .fold(error => fail(error.message), identity)
      val divisors = normalized.eventPeakScales.head.divisors
      assert(normalized.eventPeakScales.forall(_.divisors == divisors))
      val cols = plain.data.cols
      plain.data.data.indices.foreach: index =>
        val basis = (index % cols) / 2
        assertEqualsDouble(normalized.convolved.data.data(index), plain.data.data(index) / divisors(basis), 1e-14)
      assert(normalized.convolved.columnEventScales.forall(_.uniformDivisor.isDefined))
    val coarse = EventResponseConvolution.convolve(source, Hrfs.SPMG2, twoRuns, unitPeak, precision = 0.3.s).toOption.get.convolved.data.data
    val fine = EventResponseConvolution.convolve(source, Hrfs.SPMG2, twoRuns, unitPeak, precision = 0.05.s).toOption.get.convolved.data.data
    assert(coarse.indices.exists(i => math.abs(coarse(i) - fine(i)) > 1e-6), "precision must reach the convolution")

  test("multi-basis unit-peak scales each event and each basis by its own duration peak"):
    val durations = Vector(0.0, 2.0, 6.0)
    val frame = SamplingFrame(blockLens = Seq(1200), tr = Seq(0.05), startTime = Seq(0.0), precision = 0.01)
    val step = EventResponseNormalization.unitPeak(0.01.s).fold(error => fail(error.message), identity)
    def single(duration: Double, onset: Double) =
      EventTerm(Vector(Event.factor(Vector("a"), "condition")), Vector(Seconds(onset)), Vector(Seconds(duration)), Vector(0), Some("task"))
    val combined = EventTerm(
      Vector(Event.factor(Vector.fill(3)("a"), "condition")),
      Vector(0.0, 20.0, 40.0).map(Seconds(_)), durations.map(Seconds(_)), Vector(0, 0, 0), Some("task")
    )
    val result = EventResponseConvolution.convolve(combined, Hrfs.SPMG3, frame, step, precision = 0.01.s).fold(error => fail(error.message), identity)
    val divisors = result.eventPeakScales.map(_.divisors)
    (0 until 3).foreach: basis =>
      assertEquals(divisors.map(_(basis)).distinct.length, 3, s"basis ${basis + 1} has a duration-specific divisor")
    // Each event alone peaks at |1| in every basis column (to the reference-grid accuracy).
    durations.zip(Vector(0.0, 20.0, 40.0)).foreach: (duration, onset) =>
      val alone = EventResponseConvolution.convolve(single(duration, onset), Hrfs.SPMG3, frame, step, precision = 0.01.s).toOption.get.convolved.data
      (0 until 3).foreach: basis =>
        val peak = (0 until alone.rows).map(r => math.abs(alone(r, basis))).max
        assertEqualsDouble(peak, 1.0, 2e-3, s"duration $duration basis ${basis + 1}")
    // Linearity: the term is the sum of its separately normalized events.
    val parts = durations.zip(Vector(0.0, 20.0, 40.0)).map((d, o) => EventResponseConvolution.convolve(single(d, o), Hrfs.SPMG3, frame, step, precision = 0.01.s).toOption.get.convolved.data)
    result.convolved.data.data.indices.foreach: i =>
      assertEqualsDouble(result.convolved.data.data(i), parts.map(_.data(i)).sum, 1e-12)
    assertEquals(result.convolved.columnEventScales.map(_.divisors.length), Vector(3, 3, 3))

  test("unbounded-support kernels are truncated at span plus duration exactly as plain convolution"):
    val source = EventTerm(Vector(Event.factor(Vector("a"), "condition")), Vector(0.s), Vector(Seconds(4.0)), Vector(0), Some("task"))
    val frame = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))
    assertEquals(Hrfs.SPMG1.support, Support.Unbounded)
    val plain = source.convolve(Hrfs.SPMG1, frame, precision = 0.1.s)
    val normalized = EventResponseConvolution.convolve(source, Hrfs.SPMG1, frame, unitPeak, precision = 0.1.s).toOption.get
    val divisor = normalized.eventPeakScales.head.divisors.head
    (0 until 40).foreach: row =>
      assertEqualsDouble(normalized.convolved.data(row, 0), plain.data(row, 0) / divisor, 1e-14)
    // Beyond span (24 s) + duration (4 s) plus one microtime step both are exactly zero.
    (29 until 40).foreach: row =>
      assertEqualsDouble(normalized.convolved.data(row, 0), 0.0, 0.0)

  test("onsets before the run start follow the plain path's onset window under unit-peak"):
    val source = mixed(Vector.fill(6)(1.0))
    val plain = source.convolve(Hrfs.SPMG1, twoRuns, precision = 0.1.s)
    val normalized = EventResponseConvolution.convolve(source, Hrfs.SPMG1, twoRuns, unitPeak, precision = 0.1.s).toOption.get
    val divisor = normalized.eventPeakScales.head.divisors.head
    plain.data.data.indices.foreach: i =>
      assertEqualsDouble(normalized.convolved.data.data(i), plain.data.data(i) / divisor, 1e-14)
    // The first run's pre-start event has a negative global onset and is excluded,
    // so it does not count among the column's contributing events either.
    assert(normalized.convolved.columnEventScales.forall(_.divisors == Vector(divisor)))

  test("invalid precision and amplitudes are typed errors rather than exceptions"):
    val source = term(Vector(0.0), Vector(0.0))
    assertEquals(
      EventResponseConvolution.convolve(source, kernel, fineFrame, precision = Seconds(0.0)).left.toOption,
      Some(EventResponseConvolutionError.InvalidPrecision(0.0))
    )
