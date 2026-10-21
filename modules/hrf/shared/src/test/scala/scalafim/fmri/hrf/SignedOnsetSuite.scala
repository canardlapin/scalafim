package scalafim.fmri.hrf

import scalafim.fmri.hrf.regressor.*

class SignedOnsetSuite extends munit.FunSuite:
  test("finite signed onsets preserve time identity while durations remain nonnegative") {
    val event = StimulusEvent(-1.0).fold(e => fail(e.message), identity)
    assertEquals(event.onsetSeconds.value, -1.0)
    assertEquals(event.shift((-2).s).toOption.get.onsetSeconds.value, -3.0)
    assert(StimulusEvent(Double.NaN).isLeft)
    assert(StimulusEvent(Double.NegativeInfinity).isLeft)
    assert(StimulusEvent(-1.0, duration = -0.1).isLeft)
  }

  test("pre-acquisition FIR impulse retains its observed response in all evaluators") {
    val reg = Regressor(Seq(-1.0), Hrfs.fir(nBasis = 2, span = 4.s))
    val expected = Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(0.0, 1.0),
      Vector(0.0, 0.0), Vector(0.0, 0.0))
    for method <- Regressor.EvalMethod.values do
      val actual = Regressor.evaluate(reg, (0 to 4).map(_.toDouble), precision = 0.25, method = method)
      for row <- expected.indices; col <- 0 until 2 do
        assertEqualsDouble(actual(row, col), expected(row)(col), 1e-10, s"$method row $row basis $col")
  }

  test("sustained pre-acquisition events agree with analytic interval overlap") {
    // Unit-height stimulus [-10,1] convolved with a unit box on [0,2).
    // At times 0,1,2,3 the overlap lengths are exactly 2,2,1,0 seconds.
    val reg = Regressor(Seq(-10.0), Hrfs.fir(nBasis = 1, span = 2.s), duration = Seq(11.0))
    val expected = Vector(2.0, 2.0, 1.0, 0.0)
    for method <- Regressor.EvalMethod.values do
      val actual = Regressor.evaluate(reg, Seq(0.0, 1.0, 2.0, 3.0), precision = 0.01, method = method)
      val tolerance = if method == Regressor.EvalMethod.Loop then 1e-10 else 0.011
      expected.indices.foreach(i => assertEqualsDouble(actual(i, 0), expected(i), tolerance, s"$method sample $i"))
  }

  test("an onset before nominal run end can still have no sampled response") {
    val reg = Regressor(Seq(4.5), Hrfs.fir(nBasis = 2, span = 4.s))
    for method <- Regressor.EvalMethod.values do
      assert(Regressor.evaluate(reg, Seq(0.0, 1.0, 2.0, 3.0, 4.0), method = method).data.forall(_ == 0.0))
  }
