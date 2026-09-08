package scalafim.fmri.hrf

import munit.FunSuite
import scalafim.fmri.hrf.regressor.*

class SampledEventSupportSuite extends FunSuite:
  private def checked[E, A](value: Either[E, A]): A =
    value.fold(e => fail(e.toString), identity)
  private val fir = Hrfs.fir(nBasis = 2, span = 4.s)
  private val grid = (0 to 4).map(i => Seconds(i.toDouble)).toVector

  test("signed and late events retain event identity and independent FIR bin support"):
    val reg = checked(Regressor.validated(Seq(-1.0, 3.0, 4.5), hrf = fir))
    for method <- Vector(Regressor.EvalMethod.Conv, Regressor.EvalMethod.Loop) do
      val receipt = checked(SampledEventSupport.assess(reg, grid, 0.25.s, method))
      assertEquals(receipt.events.map(_.eventIndex), Vector(0, 1, 2))
      assertEquals(receipt.events.map(_.basis.map(_.sampleCount)), Vector(Vector(1, 2), Vector(2, 0), Vector(0, 0)))
      assertEquals(receipt.events.head.basis.map(_.firstSample), Vector(Some(0), Some(1)))
      assertEquals(receipt.events.head.basis.map(_.lastSample), Vector(Some(0), Some(2)))
      assert(receipt.events.head.envelopeStartsBeforeGrid)
      assert(!receipt.events.head.envelopeEndsAfterGrid)
      assert(receipt.events(1).envelopeEndsAfterGrid)
      assert(!receipt.events(2).hasSampledResponse)

  test("retained irregular sample indices are not replaced by nominal acquisition bounds"):
    val reg = checked(Regressor.validated(Seq(0.0), hrf = fir))
    val retained = Vector(0.s, 3.s, 8.s)
    val receipt = checked(SampledEventSupport.assess(reg, retained, 0.25.s))
    assertEquals(receipt.grid, retained)
    assertEquals(receipt.events.head.basis.map(_.sampleCount), Vector(1, 1))
    assertEquals(receipt.events.head.basis.map(_.firstSample), Vector(Some(0), Some(1)))

  test("opposing events have support even when the aggregate cancels; zero amplitude stays unsupported"):
    val reg = checked(Regressor.validated(Seq(0.0, 0.0, 0.0), hrf = fir, amplitude = Seq(1.0, -1.0, 0.0)))
    val total = Regressor.evaluate(reg, grid.map(_.value), precision = 0.25)
    assert(total.data.forall(_ == 0.0))
    val receipt = checked(SampledEventSupport.assess(reg, grid, 0.25.s))
    assertEquals(receipt.events.map(_.hasSampledResponse), Vector(true, true, false))
    assertEquals(receipt.events.last.basis.map(_.firstSample), Vector(None, None))
    assertEquals(receipt.events.last.basis.map(_.lastSample), Vector(None, None))

  test("sustained box overlap follows its analytic response, including threshold units"):
    val reg = checked(Regressor.validated(Seq(-10.0), hrf = Hrfs.fir(nBasis = 1, span = 2.s), duration = Seq(11.0)))
    // Integral of unit boxes: overlap lengths at t=0,1,2,3 are 2,2,1,0.
    val receipt = checked(SampledEventSupport.assess(reg, grid.take(4), 0.01.s, Regressor.EvalMethod.Loop))
    assertEquals(receipt.events.head.basis.head.sampleCount, 3)
    assertEqualsDouble(receipt.events.head.basis.head.maximumAbsoluteResponse, 2.0, 1e-12)
    val thresholded = checked(SampledEventSupport.assess(reg, grid.take(4), 0.01.s,
      Regressor.EvalMethod.Loop, absoluteTolerance = 1.5))
    assertEquals(thresholded.events.head.basis.head.sampleCount, 2)
    assertEquals(thresholded.events.head.basis.head.lastSample, Some(1))

  test("per-event HRFs retain assignment after unsupported earlier events"):
    val narrow = Hrfs.fir(nBasis = 1, span = 1.s)
    val wide = Hrfs.fir(nBasis = 1, span = 3.s)
    val reg = checked(Regressor.fromEvents(
      Vector(checked(StimulusEvent(20.0)), checked(StimulusEvent(0.0)), checked(StimulusEvent(0.0))),
      HrfAssignment.PerEvent(Vector(wide, narrow, wide)), 3.s, summate = true))
    val receipt = checked(SampledEventSupport.assess(reg, grid, 0.25.s))
    assertEquals(receipt.events.map(_.basis.head.sampleCount), Vector(0, 1, 3))

  test("clock translation preserves support and sample identity"):
    val reg = checked(Regressor.validated(Seq(-1.0, 3.0), hrf = fir))
    val shifted = checked(Regressor.validated(Seq(99.0, 103.0), hrf = fir))
    val a = checked(SampledEventSupport.assess(reg, grid, 0.25.s))
    val b = checked(SampledEventSupport.assess(shifted, grid.map(t => Seconds(t.value + 100.0)), 0.25.s))
    assertEquals(a.events.map(_.basis), b.events.map(_.basis))

  test("invalid grids and numeric policies fail rather than reorder or hide samples"):
    val reg = checked(Regressor.validated(Seq(0.0), hrf = fir))
    for bad <- Vector(Vector.empty, Vector(1.s, 0.s), Vector(0.s, 0.s)) do
      assert(SampledEventSupport.assess(reg, bad).isLeft)
    assert(SampledEventSupport.assess(reg, grid, 0.s).isLeft)
    assert(SampledEventSupport.assess(reg, grid, absoluteTolerance = -1.0).isLeft)
    assert(SampledEventSupport.assess(reg, grid, absoluteTolerance = Double.NaN).isLeft)
    val empty = checked(Regressor.validated(Seq.empty, hrf = fir, duration = Seq.empty, amplitude = Seq.empty))
    assertEquals(checked(SampledEventSupport.assess(empty, grid)).events, Vector.empty)

  test("FFT support has an explicit roundoff tolerance and agrees on analytic FIR bins"):
    val reg = checked(Regressor.validated(Seq(-1.0, 3.0, 4.5), hrf = fir))
    val receipt = checked(SampledEventSupport.assess(reg, grid, 0.25.s,
      Regressor.EvalMethod.FFT, absoluteTolerance = 1e-10))
    assertEquals(receipt.events.map(_.basis.map(_.sampleCount)), Vector(Vector(1, 2), Vector(2, 0), Vector(0, 0)))
    assertEqualsDouble(receipt.absoluteTolerance, 1e-10, 1e-20)

  test("non-finite responses and overflowing envelopes cannot masquerade as no support"):
    val bad = Hrf.scalar("nonfinite", span = 4.s)(_ => Double.NaN)
    val reg = checked(Regressor.validated(Seq(0.0), hrf = bad))
    assert(SampledEventSupport.assess(reg, grid).left.exists {
      case SampleSupportError.NonFiniteResponse(0, _, 0) => true
      case _ => false
    })
    val overflow = checked(Regressor.validated(Seq(Double.MaxValue), hrf = fir, duration = Seq(Double.MaxValue)))
    assertEquals(SampledEventSupport.assess(overflow, grid), Left(SampleSupportError.NonFiniteEnvelope(0)))
