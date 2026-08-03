package scalafim.fmri.hrf

import scala.compiletime.testing.typeChecks

class LagSuite extends munit.FunSuite:

  test("a lag is the displacement from an onset to a clock reading") {
    assertEqualsDouble(Lag.between(Seconds(10.0), Seconds(13.5)).value, 3.5, 1e-12)
    assertEqualsDouble(Lag.between(Seconds(10.0), Seconds(10.0)).value, 0.0, 1e-12)
    // before the event: negative, and every kernel must read zero there
    assertEqualsDouble(Lag.between(Seconds(10.0), Seconds(8.0)).value, -2.0, 1e-12)
    assert(!Lag.between(Seconds(10.0), Seconds(8.0)).isCausal)
  }

  test("an absolute time is not a lag, and a lag is not a width") {
    // The whole point of the type: handing a kernel a clock reading, or a
    // span, used to typecheck and silently answer a different question.
    assert(
      !typeChecks("""
        val onset: Seconds = Seconds(10.0)
        Hrfs.SPMG1(onset)
      """),
      "an absolute Seconds should not be accepted as a lag"
    )
    assert(
      !typeChecks("""
        val lag: Lag = Lag(3.0)
        Hrfs.boxcar(width = lag)
      """),
      "a Lag should not be accepted where a width is required"
    )
    assert(
      typeChecks("""
        Hrfs.SPMG1(Lag.between(Seconds(10.0), Seconds(13.5)))
      """),
      "the boundary conversion should be the way through"
    )
  }

  test("shifting a lag tracks a delayed kernel") {
    val base = Hrfs.SPMG1
    val delayed = HrfCombinators.lag(base)(Seconds(3.0))
    // h_delayed(l) = h(l - 3)
    for x <- Seq(0.0, 2.0, 5.0, 9.0, 20.0) do
      assertEqualsDouble(
        delayed(Lag(x)).data(0),
        base(Lag(x).rewound(Seconds(3.0))).data(0),
        1e-12,
        s"at lag $x"
      )
  }

  test("Lag rejects non-finite values and reports why") {
    assert(Lag.fromDouble(Double.NaN).isLeft)
    assert(Lag.fromDouble(Double.PositiveInfinity, "lag").isLeft)
    assertEquals(
      Lag.fromDouble(Double.NaN, "lag").left.map(_.message),
      Left("lag must be finite, got NaN")
    )
    intercept[IllegalArgumentException](Lag(Double.NaN))
  }
