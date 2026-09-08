package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.regressor.*
import scalafim.fmri.hrf.TestUtils.*

class RegressorParitySuite extends munit.FunSuite:

  private val box: Hrf =
    Hrf.scalar("box", span = 1.0.s)(t => if t.value >= 0.0 && t.value <= 1.0 then 1.0 else 0.0)

  test("Regressor validates finite clock readings and duration/span contracts") {
    assertEquals(Regressor(Seq(-1.0), box).onsets.map(_.value), Vector(-1.0))
    intercept[IllegalArgumentException] { Regressor(Seq(Double.NaN), box) }
    intercept[IllegalArgumentException] { Regressor(Seq(1.0), box, span = Some(0.0)) }
    intercept[IllegalArgumentException] { Regressor(Seq(1.0), box, span = Some(Double.PositiveInfinity)) }
    intercept[IllegalArgumentException] {
      Regressor(Seq(0.0, 1.0), box, duration = Seq(1.0, 2.0, 3.0))
    }
    intercept[IllegalArgumentException] {
      Regressor(Seq(0.0, 1.0), box, amplitude = Seq(1.0, 2.0, 3.0))
    }
  }

  test("single-event regressor behaves like single_trial_regressor") {
    val st = Regressor(Seq(5.0), box, duration = Seq(2.0), amplitude = Seq(3.0))
    assertEquals(st.onsets.length, 1)
    assertEquals(st.onsets.head.value, 5.0)
    assertEquals(st.durations.head.value, 2.0)
    assertEquals(st.amplitudes.head, 3.0)
  }

  test("span defaults to HRF span when not provided") {
    val reg = Regressor(Seq(0.0, 10.0), Hrfs.Gamma)
    assertEquals(reg.span.value, Hrfs.Gamma.span.value)
  }

  test("evaluate validates grid and precision") {
    val reg = Regressor(Seq(0.0), box)
    intercept[IllegalArgumentException] { Regressor.evaluate(reg, Seq.empty) }
    intercept[IllegalArgumentException] { Regressor.evaluate(reg, Seq(0.0, Double.NaN)) }
    intercept[IllegalArgumentException] { Regressor.evaluate(reg, Seq(0.0, 1.0), precision = 0.0) }
    intercept[IllegalArgumentException] { Regressor.evaluate(reg, Seq(0.0, 1.0), precision = -1.0) }
  }

  test("per-event regressors accept list of HRFs") {
    val h1 = Hrfs.boxcar(4.0.s, normalize = true)
    val h2 = Hrfs.boxcar(6.0.s, normalize = true)
    val h3 = Hrfs.boxcar(8.0.s, normalize = true)
    val reg = Regressor.perEvent(Seq(10.0, 30.0, 50.0), Seq(h1, h2, h3))
    assert(reg.hrf.isInstanceOf[HrfAssignment.PerEvent])
    assertEquals(reg.onsets.length, 3)
    reg.hrf match
      case HrfAssignment.PerEvent(hrfs) => assertEquals(hrfs.length, 3)
      case _ => fail("expected per-event HRFs")
  }

  test("per-event regressor evaluates correctly") {
    val h1 = Hrfs.boxcar(4.0.s, normalize = true)
    val h2 = Hrfs.boxcar(4.0.s, normalize = true)
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(h1, h2))
    val t = (0 to 100).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Loop).data.toVector
    def anyPos(lo: Double, hi: Double): Boolean =
      t.zip(res).exists { case (tt, v) => tt >= lo && tt <= hi && v > 0.0 }
    assert(anyPos(10.0, 14.0))
    assert(anyPos(30.0, 34.0))
    assertEquals(res(t.indexOf(0.0)), 0.0)
    assertEquals(res(t.indexOf(50.0)), 0.0)
  }

  test("per-event span is max of individual spans") {
    val h1 = Hrfs.boxcar(4.0.s)
    val h2 = Hrfs.boxcar(10.0.s)
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(h1, h2))
    assertEquals(reg.span.value, 10.0)
    val t = (0 to 100).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Loop).data.toVector
    def at(x: Double): Double = res(t.indexOf(x))
    assert(at(12.0) > 0.0)
    assert(at(38.0) > 0.0)
  }

  test("per-event with weighted HRFs works") {
    val h1 = Hrfs.weighted(
      weights = Vector(0.1, 0.5, 0.3, 0.1),
      times = Some(Vector(0.0, 2.0, 4.0, 6.0).map(_.s)),
      normalize = true
    )
    val h2 = Hrfs.weighted(
      weights = Vector(0.4, 0.3, 0.2, 0.1),
      times = Some(Vector(0.0, 2.0, 4.0, 6.0).map(_.s)),
      normalize = true
    )
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(h1, h2))
    val t = (0 to 100).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Loop).data
    assert(res.exists(_ > 0.0))
  }

  test("per-event recycles single HRF") {
    val h = Hrfs.boxcar(4.0.s)
    val reg = Regressor.perEvent(Seq(10.0, 20.0, 30.0), Seq(h))
    reg.hrf match
      case HrfAssignment.PerEvent(hrfs) => assertEquals(hrfs.length, 3)
      case _ => fail("expected per-event HRFs")
  }

  test("per-event validates HRF list length") {
    val h1 = Hrfs.boxcar(4.0.s)
    val h2 = Hrfs.boxcar(6.0.s)
    intercept[IllegalArgumentException] {
      Regressor.perEvent(Seq(10.0, 20.0, 30.0), Seq(h1, h2))
    }
  }

  test("per-event keeps HRFs aligned with events across a zero amplitude") {
    val h1 = Hrfs.boxcar(4.0.s)
    val h2 = Hrfs.boxcar(6.0.s)
    val h3 = Hrfs.boxcar(8.0.s)
    val reg = Regressor.perEvent(
      Seq(10.0, 20.0, 30.0),
      Seq(h1, h2, h3),
      amplitude = Seq(1.0, 0.0, 1.0)
    )
    assertEquals(reg.onsets.map(_.value), Vector(10.0, 20.0, 30.0))
    reg.hrf match
      case HrfAssignment.PerEvent(hrfs) =>
        assertEquals(hrfs.length, 3)
        // event i still gets kernel i — the trial-wise index contract
        assertEquals(hrfs.map(_.span.value), Vector(h1.span.value, h2.span.value, h3.span.value))
      case _ => fail("expected per-event HRFs")
  }

  test("windowing out an onset does not re-bind per-event HRFs") {
    val h1 = Hrfs.boxcar(2.0.s)
    val h2 = Hrfs.boxcar(4.0.s)
    val h3 = Hrfs.boxcar(8.0.s)
    val reg = Regressor.perEvent(Seq(5.0, 40.0, 60.0), Seq(h1, h2, h3), span = Some(10.0))

    // grid starts at 30, so onset 5.0 falls outside [gridStart - span, gridEnd]
    val grid = (30 to 80).map(_.toDouble)
    val windowed = Regressor.evaluate(reg, grid, method = Regressor.EvalMethod.Loop)

    val ref = Regressor.perEvent(Seq(40.0, 60.0), Seq(h2, h3), span = Some(10.0))
    val expected = Regressor.evaluate(ref, grid, method = Regressor.EvalMethod.Loop)

    var i = 0
    while i < windowed.data.length do
      assertEqualsDouble(windowed.data(i), expected.data(i), 1e-12, s"wrong per-event HRF at $i")
      i += 1
  }

  test("per-event works with all evaluation methods") {
    val h1 = Hrfs.boxcar(4.0.s, normalize = true)
    val h2 = Hrfs.boxcar(4.0.s, normalize = true)
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(h1, h2))
    val t = (0 to 100).map(_ * 0.5).toVector
    val loop = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Loop)
    val conv = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Conv)
    val fft  = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.FFT)
    assert(loop.approxEquals(conv))
    assert(loop.approxEquals(fft))
  }

  test("nbasis handles per-event HRFs") {
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(Hrfs.SPMG1, Hrfs.SPMG1))
    assertEquals(reg.nbasis, 1)
  }

  test("per-event supports mixed HRF types") {
    val reg = Regressor.perEvent(Seq(10.0, 30.0), Seq(Hrfs.SPMG1, Hrfs.boxcar(6.0.s, normalize = true)))
    val t = (0 to 120).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t, method = Regressor.EvalMethod.Loop).data.toVector
    def anyPos(lo: Double, hi: Double): Boolean =
      t.zip(res).exists { case (tt, v) => tt >= lo && tt <= hi && v > 0.0 }
    assert(anyPos(10.0, 25.0))
    assert(anyPos(30.0, 36.0))
  }
