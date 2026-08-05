package scalafim.fmri.hrf

import scalafim.fmri.hrf.regressor.*
import scalafim.fmri.hrf.TestUtils.*

class RegressorSuite extends munit.FunSuite:

  private val box: Hrf =
    Hrf.scalar("box", span = 1.0.s)(t => if t.value >= 0.0 && t.value <= 1.0 then 1.0 else 0.0)

  test("regressor retains zero-amplitude events so event indices stay stable") {
    val reg = Regressor(Seq(1.0, 2.0, 3.0), box, amplitude = Seq(1.0, 0.0, 2.0), span = Some(1.0))
    assertEquals(reg.onsets.map(_.value), Vector(1.0, 2.0, 3.0))
    assertEquals(reg.amplitudes, Vector(1.0, 0.0, 2.0))
  }

  test("a zero-amplitude event occupies an index but contributes no signal") {
    val grid = (0 to 10).map(_.toDouble)
    val withZero = Regressor(Seq(1.0, 2.0, 3.0), box, amplitude = Seq(1.0, 0.0, 2.0), span = Some(1.0))
    val without = Regressor(Seq(1.0, 3.0), box, amplitude = Seq(1.0, 2.0), span = Some(1.0))

    assertEquals(withZero.events.length, 3)
    assertEquals(without.events.length, 2)

    for method <- Seq(Regressor.EvalMethod.Loop, Regressor.EvalMethod.Conv, Regressor.EvalMethod.FFT) do
      val a = Regressor.evaluate(withZero, grid, method = method)
      val b = Regressor.evaluate(without, grid, method = method)
      var i = 0
      while i < a.data.length do
        assertEqualsDouble(a.data(i), b.data(i), 1e-12, s"$method differs at $i")
        i += 1
  }

  test("validated regressor stores typed stimulus events") {
    val reg = Regressor
      .validated(Seq(1.0, 2.0, 3.0), box, duration = Seq(0.5), amplitude = Seq(1.0, 0.0, 2.0), span = Some(1.0))
      .fold(err => fail(err.message), identity)

    assertEquals(reg.events.length, 3)
    assertEquals(reg.events.map(_.onsetSeconds.value), Vector(1.0, 2.0, 3.0))
    assertEquals(reg.events.map(_.durationSeconds.value), Vector(0.5, 0.5, 0.5))
    assert(Regressor.validated(Seq(Double.NaN), box).isLeft)
  }

  test("invalid inputs are rejected") {
    intercept[IllegalArgumentException] {
      Regressor(Seq(-1.0, 1.0), box)
    }
    intercept[IllegalArgumentException] {
      Regressor(Seq(1.0), box, duration = Seq(-2.0))
    }
    intercept[IllegalArgumentException] {
      Regressor(Seq(1.0), box, amplitude = Seq(Double.PositiveInfinity))
    }
  }

  test("shift moves onsets") {
    val reg = Regressor(Seq(0.0, 2.0), box)
    val shifted = reg.shift(5.0.s)
    assertEquals(shifted.onsets.map(_.value), Vector(5.0, 7.0))
  }

  test("evaluate conv computes expected response") {
    val reg = Regressor(Seq(0.0, 2.0), box, span = Some(1.0))
    val grid = Seq(0.0, 1.0, 2.0, 3.0, 4.0)
    val res = Regressor.evaluate(reg, grid, precision = 1.0, method = Regressor.EvalMethod.Conv)
    assertEquals(res.data.toVector, Vector(1.0, 1.0, 1.0, 1.0, 0.0))
  }

  test("unsorted grid yields same as sorted") {
    val reg = Regressor(Seq(0.0), box, span = Some(1.0))
    val g1 = Seq(3.0, 0.0, 1.0)
    val out1 = Regressor.evaluate(reg, g1, precision = 1.0, method = Regressor.EvalMethod.Conv)
    val out2 = Regressor.evaluate(reg, g1.sorted, precision = 1.0, method = Regressor.EvalMethod.Conv)
    assert(out1.approxEquals(out2))
  }
