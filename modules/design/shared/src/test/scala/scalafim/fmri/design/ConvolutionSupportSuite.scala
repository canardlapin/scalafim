package scalafim.fmri.design

import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class ConvolutionSupportSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = SamplingFrame(blockLens = Seq(5), tr = Seq(1.0), startTime = Seq(0.0))
  private val scans = (1 to 5).map(ScanIndex.unsafeOneBased).toVector

  test("resolved FIR support retains source and original scan identities after censoring"):
    val term = EventTerm(Vector(Event.factor(Vector("A", "A", "A"), "condition")),
      Vector(-1.s, 3.s, 4.5.s), sourceRows = Vector(7, 9, 12))
    val convolved = term.convolve(Hrfs.fir(nBasis = 2, span = 4.s), frame, precision = 0.25.s)
    val all = checked(convolved.sampledSupport(scans))
    assertEquals(all.contributions.groupBy(_.eventIndex).toVector.sortBy(_._1).map(_._2.map(_.actual.sampleCount)),
      Vector(Vector(1, 2), Vector(2, 0), Vector(0, 0)))
    assertEquals(all.contributions.map(_.sourceRow).distinct, Vector(Some(7), Some(9), Some(12)))
    assert(all.contributions.find(_.eventIndex == 1).get.actual.window.exists(_.endsAfterGrid))
    assert(all.contributions.head.actual.window.exists(_.startsBeforeGrid))
    val retained = Vector(scans(0), scans(2))
    val censored = checked(convolved.sampledSupport(retained))
    assertEquals(censored.retainedScans, retained)
    val first = censored.contributions.filter(_.eventIndex == 0)
    assertEquals(first.map(_.actual.firstScan), Vector(Some(scans(0)), Some(scans(2))))
    assertEquals(first.map(_.actual.sampleCount), Vector(1, 1))
    assert(checked(convolved.sampledSupport(Vector.empty)).contributions.forall(_.actual.sampleCount == 0))

  test("per-event assignments retain zero modulation separately from temporal potential"):
    val term = EventTerm(Vector(Event.variable(Vector(1.0, 0.0), "amplitude")), Vector(0.s, 0.s))
    val convolved = term.convolvePerEvent(Vector(Hrfs.fir(nBasis = 1, span = 1.s),
      Hrfs.fir(nBasis = 1, span = 3.s)), frame, dropEmpty = false)
    val support = checked(convolved.sampledSupport(scans))
    assertEquals(support.contributions.map(_.actual.sampleCount), Vector(1, 0))
    assertEquals(support.contributions.map(_.unitInput.sampleCount), Vector(1, 3))
    assertEquals(support.contributions.map(_.inputAmplitude), Vector(1.0, 0.0))
    assert(support.contributions.forall(_.sourceRow.isEmpty))

  test("per-cell mixed basis widths map to their actual design columns"):
    val term = EventTerm(Vector(Event.factor(Vector("A", "B"), "condition")), Vector(0.s, 0.s))
    val convolved = term.convolveByCondition(Vector(Hrfs.fir(nBasis = 1, span = 1.s),
      Hrfs.fir(nBasis = 2, span = 4.s)), frame, precision = 0.25.s)
    val support = checked(convolved.sampledSupport(scans)).contributions
    val nonzero = support.filter(_.actual.sampleCount > 0)
    assertEquals(nonzero.map(x => (x.eventIndex, x.columnIndex, x.actual.sampleCount)),
      Vector((0, 0, 1), (1, 1, 2), (1, 2, 2)))
    assert(nonzero.forall(x => x.columnName == convolved.columnNames(x.columnIndex)))

  test("duration averaging and final column scaling are retained exactly"):
    val term = EventTerm(Vector(Event.variable(Vector(3.0), "amplitude")), Vector(0.s), durations = Vector(3.s))
    val convolved = term.convolvePerEvent(Vector(Hrfs.fir(nBasis = 1, span = 2.s)), frame,
      summate = false, scaling = HrfColumnScaling.UnitMaximumAbsolute)
    val support = checked(convolved.sampledSupport(scans)).contributions.head
    // Unit box overlap peaks at 2; averaging divides by duration 3, amplitude is 3.
    assertEqualsDouble(support.actual.maximumAbsoluteResponse, 2.0, 1e-12)
    assertEqualsDouble(support.unitInput.maximumAbsoluteResponse, 2.0 / 3.0, 1e-12)
    assertEqualsDouble(support.columnScale.divisor, 2.0, 1e-12)
    assertEqualsDouble(support.scaledMaximumAbsoluteResponse, 1.0, 1e-12)

  test("a completely censored run retains unsupported event records without borrowing another run"):
    val multi = SamplingFrame(blockLens = Seq(5, 5), tr = Seq(1.0), startTime = Seq(0.0))
    val term = EventTerm(Vector(Event.factor(Vector("A", "A"), "condition")), Vector(0.s, 0.s),
      blockIds = Vector(0, 1), sourceRows = Vector(3, 8))
    val convolved = term.convolve(Hrfs.fir(nBasis = 1, span = 2.s), multi, precision = 0.25.s)
    val support = checked(convolved.sampledSupport(scans)).contributions
    assertEquals(support.map(_.actual.sampleCount), Vector(2, 0))
    assertEquals(support.map(_.blockId), Vector(0, 1))
    assertEquals(support.map(_.sourceRow), Vector(Some(3), Some(8)))

  test("missing recipes, replaced matrices and invalid selections fail explicitly"):
    val term = EventTerm(Vector(Event.factor(Vector("A"), "condition")), Vector(0.s))
    val convolved = term.convolve(Hrfs.fir(nBasis = 1, span = 2.s), frame)
    assertEquals(convolved.copy(convolutionRecipe = None).sampledSupport(scans), Left(ConvolutionSupportError.MissingRecipe))
    assertEquals(convolved.copy(data = Mat.zeros(5, 1)).sampledSupport(scans), Left(ConvolutionSupportError.ChangedTerm))
    assert(convolved.sampledSupport(Vector(scans(1), scans(0))).isLeft)
    assert(convolved.sampledSupport(Vector(scans(0), scans(0))).isLeft)
    assert(convolved.sampledSupport(Vector(ScanIndex.unsafeOneBased(6))).isLeft)
    assert(convolved.sampledSupport(scans, -1.0).isLeft)

  test("resolved recipes distinguish timing potential even when zero amplitudes hide it in the matrix"):
    val term = EventTerm(Vector(Event.variable(Vector(1.0, 0.0), "amplitude")), Vector(0.s, 0.s))
    val narrow = Hrfs.fir(nBasis = 1, span = 1.s)
    val a = term.convolvePerEvent(Vector(narrow, narrow), frame, dropEmpty = false)
    val b = term.convolvePerEvent(Vector(narrow, Hrfs.fir(nBasis = 1, span = 3.s)), frame, dropEmpty = false)
    assertEquals(a.data.data.toVector, b.data.data.toVector)
    assertNotEquals(a.convolutionRecipe.get.canonical, b.convolutionRecipe.get.canonical)
    val modelA = EventModel.build(Vector(a), frame)
    val modelB = EventModel.build(Vector(b), frame)
    assertNotEquals(modelA.designSchema.fingerprint, modelB.designSchema.fingerprint)
    assert(modelB.designSchema.audit.convolutionReceipts.nonEmpty)

  test("in-place matrix or event-amplitude mutations invalidate a captured recipe"):
    def compiled = EventTerm(Vector(Event.variable(Vector(2.0), "amplitude")), Vector(0.s))
      .convolve(Hrfs.fir(nBasis = 1, span = 2.s), frame)
    val changedMatrix = compiled
    changedMatrix.data.data(0) = 17.0
    assertEquals(changedMatrix.sampledSupport(scans), Left(ConvolutionSupportError.ChangedTerm))
    val changedInput = compiled
    changedInput.term.events.head.asInstanceOf[ContinuousEvent].value.data(0) = 19.0
    assertEquals(changedInput.sampledSupport(scans), Left(ConvolutionSupportError.ChangedTerm))

  test("per-basis event divisors retain the actual basis and amplitude before column scaling"):
    val term = EventTerm(Vector(Event.variable(Vector(6.0), "amplitude")), Vector(0.s))
    val convolved = term.convolveWithEventDivisors(Hrfs.fir(nBasis = 2, span = 4.s), frame,
      precision = 0.25.s, dropEmpty = false, summate = true,
      scaling = HrfColumnScaling.UnitMaximumAbsolute, eventDivisors = Vector(Vector(2.0, 3.0)))
    val support = checked(convolved.sampledSupport(scans)).contributions
    assertEquals(support.map(_.columnIndex), Vector(0, 1))
    assertEquals(support.map(_.actual.firstScan), Vector(Some(scans(0)), Some(scans(2))))
    assertEquals(support.map(_.actual.sampleCount), Vector(2, 2))
    for (row, amplitude) <- support.zip(Vector(3.0, 2.0)) do
      assertEqualsDouble(row.inputAmplitude, amplitude, 1e-12)
      assertEqualsDouble(row.actual.maximumAbsoluteResponse, amplitude, 1e-12)
      assertEqualsDouble(row.scaledMaximumAbsoluteResponse, 1.0, 1e-12)

  test("source-row remapping preserves numeric support and detects arbitrary copied provenance"):
    val term = EventTerm(Vector(Event.factor(Vector("A"), "condition")), Vector(0.s), sourceRows = Vector(1))
    val convolved = term.convolve(Hrfs.fir(nBasis = 1, span = 2.s), frame)
    val remapped = convolved.remapSourceRows(Vector(3, 8))
    assertEquals(checked(remapped.sampledSupport(scans)).contributions.map(_.sourceRow), Vector(Some(8)))
    assertEquals(remapped.data.data.toVector, convolved.data.data.toVector)
    val copied = convolved.copy(term = term.copy(sourceRows = Vector(8)))
    assertEquals(copied.sampledSupport(scans), Left(ConvolutionSupportError.ChangedTerm))

  test("zero shared amplitudes keep potential support without sampling until inspection"):
    var evaluations = 0
    val kernel = Hrf.scalar("counted", span = 2.s) { lag =>
      evaluations += 1
      if lag.value >= 0.0 && lag.value < 2.0 then 1.0 else 0.0
    }
    val term = EventTerm(Vector(Event.variable(Vector(0.0), "amplitude")), Vector(0.s))
    val convolved = term.convolve(kernel, frame, precision = 0.25.s, dropEmpty = false)
    assertEquals(evaluations, 0)
    val support = checked(convolved.sampledSupport(scans)).contributions.head
    assertEquals(support.actual.sampleCount, 0)
    assertEquals(support.unitInput.sampleCount, 2)
