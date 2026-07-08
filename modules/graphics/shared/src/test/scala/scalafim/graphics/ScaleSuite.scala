package scalafim.graphics

class ScaleSuite extends munit.FunSuite:

  test("continuous ranges train as an associative union over finite values") {
    val left =
      ContinuousRange.empty
        .train(Vector(2.0, Double.NaN, 8.0))
        .train(Vector(-1.0, Double.PositiveInfinity))
        .requireTrained
        .toOption
        .get

    val right =
      ContinuousRange.empty
        .train(Vector(2.0, Double.NaN, 8.0, -1.0, Double.PositiveInfinity))
        .requireTrained
        .toOption
        .get

    assertEquals(left, right)
    assertEquals(left.lower, -1.0)
    assertEquals(left.upper, 8.0)
  }

  test("continuous scale maps through transform, rescale, and out-of-bounds policy") {
    val scale =
      ContinuousScale
        .train("x", Vector(1.0, 10.0, 100.0), Palette.numeric, transform = Transform.log10)
        .toOption
        .get

    val mapped = scale.mapValues(Vector(1.0, 10.0, 100.0, 1000.0))

    assertEqualsDouble(mapped(0).get, 0.0, 1e-12)
    assertEqualsDouble(mapped(1).get, 0.5, 1e-12)
    assertEqualsDouble(mapped(2).get, 1.0, 1e-12)
    assertEquals(mapped(3), None)
    assertEquals(scale.breaks, Vector(1.0, 10.0, 100.0))
    assertEquals(scale.breaks.flatMap(scale.mapValue), Vector(0.0, 0.5, 1.0))
  }

  test("squish keeps out-of-bounds values by clamping to palette endpoints") {
    val scale =
      ContinuousScale
        .train(
          "x",
          Vector(0.0, 10.0),
          Palette.numeric,
          oob = OobPolicy.Squish
        )
        .toOption
        .get

    assertEquals(scale.mapValues(Vector(-5.0, 15.0)), Vector(Some(0.0), Some(1.0)))
  }

  test("transforms advertise round-trip laws over their domains") {
    assert(Transform.identity.roundTrips(-4.0, 1e-12))
    assert(Transform.sqrt.roundTrips(9.0, 1e-12))
    assert(Transform.log10.roundTrips(100.0, 1e-12))
    assert(Transform.log10.transform(-1.0).isLeft)
  }

  test("discrete domains preserve declared ordering and append new levels") {
    val trained =
      DiscreteDomain
        .ordered(Vector("low", "mid"))
        .flatMap(_.train(Vector("high", "low")))
        .toOption
        .get

    assertEquals(trained.levels, Vector("low", "mid", "high"))
  }

  test("discrete scale maps levels with a total palette over trained domain") {
    val domain = DiscreteDomain.ordered(Vector("A", "B", "C")).toOption.get
    val palette = DiscretePalette.valuesUnsafe(Vector(Rgba.Black, Rgba.White))
    val scale = DiscreteScale("condition", domain, palette).toOption.get

    assertEquals(scale.mapLevels(Vector("A", "B", "C", "D")), Vector(Some(Rgba.Black), Some(Rgba.White), Some(Rgba.Black), None))
  }

  test("break generators are deterministic functions of intervals") {
    val breaks = Breaks.width(2.0).toOption.get

    assertEquals(breaks(Interval.unsafe(1.0, 6.0)), Vector(2.0, 4.0, 6.0))
    assertEquals(Breaks.countUnsafe(3)(Interval.unsafe(0.0, 10.0)), Vector(0.0, 5.0, 10.0))
  }
