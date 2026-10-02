package scalafim.fmri.hrf

class EventResponseNormalizationSuite extends munit.FunSuite:
  private val ramp = Hrf.scalar("ramp", span = Seconds(2.0), support = Support.Compact(Seconds(2.0))) { lag =>
    lag.value
  }

  test("unit peak scaling is per event duration and not kernel normalization"):
    val unitPeak = EventResponseNormalization.unitPeak(Seconds(0.001)).toOption.getOrElse(fail("reference step"))
    val short = EventResponse.prepare(ramp, Pulse.box(Seconds(1.0)).toOption.getOrElse(fail("short pulse")), unitPeak, Integration.Trapezoid).toOption.getOrElse(fail("short response"))
    val long = EventResponse.prepare(ramp, Pulse.box(Seconds(2.0)).toOption.getOrElse(fail("long pulse")), unitPeak, Integration.Trapezoid).toOption.getOrElse(fail("long response"))
    val unscaled = EventResponse.prepare(ramp, Pulse.box(Seconds(2.0)).toOption.getOrElse(fail("long pulse"))).toOption.getOrElse(fail("unscaled response"))

    assertEqualsDouble(unscaled.at(Lag(2.0)).data(0), 2.0, 1e-12)
    val grid = (0 to 4000).map(index => Lag(index * 0.001)).toVector
    val shortPeak = short.evaluate(grid).data.map(math.abs).max
    val longPeak = long.evaluate(grid).data.map(math.abs).max
    assertEqualsDouble(longPeak, 1.0, 1e-12)
    assertEqualsDouble(shortPeak, 1.0, 1e-12)
    assert(long.peakScales(0) > short.peakScales(0), "duration-specific peak scales should differ")

  test("unit peak reference step is validated at the typed boundary"):
    assert(EventResponseNormalization.unitPeak(Seconds(0.0)).isLeft)

  test("unit peak refuses an impractically dense reference grid before allocation"):
    val tinyStep = EventResponseNormalization.unitPeak(Seconds(1e-9)).toOption.getOrElse(fail("reference step"))
    assert(EventResponse.prepare(ramp, Pulse.Impulse, tinyStep).isLeft)

  test("unit peak rejects non-finite response values"):
    val nonFinite = Hrf.scalar("non-finite", span = Seconds(1.0), support = Support.Compact(Seconds(1.0))) { _ => Double.NaN }
    val policy = EventResponseNormalization.unitPeak(Seconds(0.1)).toOption.getOrElse(fail("reference step"))
    assert(EventResponse.prepare(nonFinite, Pulse.Impulse, policy).isLeft)
