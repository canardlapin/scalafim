package scalafim.image.view

import scalafim.graphics.*

class ColorizerSuite extends munit.FunSuite:

  test("display windows validate and scalar colors clamp predictably") {
    assert(DisplayWindow.make(1.0, 1.0).isLeft)
    assert(DisplayWindow.make(Double.NaN, 1.0).isLeft)

    val colorizer = ScalarColorizer(DisplayWindow.unsafe(-1.0, 1.0))
    val low = colorizer.color(-2.0)
    val middle = colorizer.color(0.0)
    val high = colorizer.color(2.0)

    assertEquals((low.red, low.green, low.blue, low.alpha), (0, 0, 0, 255))
    assertEquals((middle.red, middle.green, middle.blue, middle.alpha), (128, 128, 128, 255))
    assertEquals((high.red, high.green, high.blue, high.alpha), (255, 255, 255, 255))
    assertEquals(colorizer.color(Double.NaN).alpha, 0)
  }

  test("ramps interpolate every RGBA channel") {
    val ramp = ColorRamp(
      Rgba32.unsafe(0, 20, 40, 60),
      Rgba32.unsafe(100, 120, 140, 160)
    )
    val middle = ramp.colorAt(0.5)

    assertEquals((middle.red, middle.green, middle.blue, middle.alpha), (50, 70, 90, 110))
  }

  test("ramp hot path is bit-exact against independently checked channel interpolation") {
    val low = Rgba32.unsafe(213, 17, 91, 240)
    val high = Rgba32.unsafe(4, 231, 52, 11)
    val ramp = ColorRamp(low, high)
    var step = -32
    while step <= 288 do
      val fraction = step.toDouble / 255.0
      val t = math.max(0.0, math.min(1.0, fraction))
      def channel(from: Int, to: Int): Int =
        math.round(from + (to - from) * t).toInt
      val expected = Rgba32.unsafe(
        channel(low.red, high.red),
        channel(low.green, high.green),
        channel(low.blue, high.blue),
        channel(low.alpha, high.alpha)
      )
      assertEquals(ramp.colorAt(fraction), expected)
      step += 1
  }

  test("label and mask colorizers keep missing data transparent by default") {
    val red = Rgba32.unsafe(255, 0, 0, 192)
    val labels = LabelColorizer(Map(7 -> red))
    val mask = MaskColorizer(red)

    assertEquals(labels.color(7), red)
    assertEquals(labels.color(8).alpha, 0)
    assertEquals(mask.color(true), red)
    assertEquals(mask.color(false).alpha, 0)
  }

  test("layer identifiers and opacities reject invalid states") {
    assert(LayerId.make("  ").isLeft)
    assertEquals(LayerId.make(" anatomy ").toOption.get.asString, "anatomy")
    assert(LayerOpacity.make(-0.1).isLeft)
    assert(LayerOpacity.make(Double.PositiveInfinity).isLeft)
    assertEqualsDouble(LayerOpacity.make(0.4).toOption.get.toDouble, 0.4, 0.0)
  }
