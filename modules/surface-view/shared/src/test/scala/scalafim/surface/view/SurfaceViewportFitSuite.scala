package scalafim.surface.view

class SurfaceViewportFitSuite extends munit.FunSuite:
  private val left = SurfaceViewSlot(SurfaceId.unsafe("left"), SurfaceViewport(0.0, 0.0, 0.5, 1.0), 4.0, -2.0, 7.0)
  private val right = SurfaceViewSlot(SurfaceId.unsafe("right"), SurfaceViewport(0.5, 0.0, 0.5, 1.0), -3.0, 6.0, 2.0)
  private val slots = Vector(left, right)

  test("wide bilateral canvas contains two adjacent physical squares at its center"):
    val fitted = SurfaceViewportFit.Contain(2.0).resolve(slots, 1400.0, 300.0)
    val a = fitted(0).viewport
    val b = fitted(1).viewport
    assertEqualsDouble(a.x * 1400.0, 400.0, 1e-9)
    assertEqualsDouble(b.x * 1400.0, 700.0, 1e-9)
    assertEqualsDouble(a.width * 1400.0, 300.0, 1e-9)
    assertEqualsDouble(b.width * 1400.0, 300.0, 1e-9)
    assertEqualsDouble(a.height * 300.0, 300.0, 1e-9)
    assertEquals(fitted.map(s => s.copy(viewport = slots.find(_.surface == s.surface).get.viewport)), slots)

  test("narrow bilateral canvas preserves equal scale and centers the row vertically"):
    val fitted = SurfaceViewportFit.Contain(2.0).resolve(slots, 400.0, 800.0)
    assertEqualsDouble(fitted.head.viewport.x, 0.0, 1e-12)
    assertEqualsDouble(fitted.head.viewport.y * 800.0, 300.0, 1e-9)
    assertEqualsDouble(fitted.head.viewport.width * 400.0, 200.0, 1e-9)
    assertEqualsDouble(fitted.head.viewport.height * 800.0, 200.0, 1e-9)
    assertEqualsDouble(fitted.last.viewport.x * 400.0, 200.0, 1e-9)

  test("single and bilateral fitting preserve order, square pixel scale and containment across aspect ratios"):
    for
      width <- Vector(320.0, 720.0, 1600.0, 2560.0)
      height <- Vector(240.0, 700.0, 1200.0)
      count <- Vector(1, 2)
    do
      val source = slots.take(count).zipWithIndex.map: (slot, index) =>
        slot.copy(viewport = SurfaceViewport(index.toDouble / count, 0.0, 1.0 / count, 1.0))
      val fitted = SurfaceViewportFit.Contain(count.toDouble).resolve(source, width, height)
      assertEquals(fitted.map(_.surface), source.map(_.surface))
      fitted.foreach: slot =>
        val v = slot.viewport
        assert(v.x >= 0.0 && v.y >= 0.0 && v.x + v.width <= 1.0 + 1e-12 && v.y + v.height <= 1.0 + 1e-12)
        assertEqualsDouble(v.width * width, math.min(width / count, height), 1e-9)
        assertEqualsDouble(v.width * width, v.height * height, 1e-9)
      assertEqualsDouble(fitted.head.viewport.x + fitted.last.viewport.x + fitted.last.viewport.width, 1.0, 1e-12)

  test("literal viewport coordinates remain available and invalid fitting inputs are refused"):
    assertEquals(SurfaceViewportFit.Fill.resolve(slots, 1400.0, 300.0), slots)
    for bad <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      intercept[IllegalArgumentException](SurfaceViewportFit.Contain(bad))
      intercept[IllegalArgumentException](SurfaceViewportFit.Contain(2.0).resolve(slots, bad, 300.0))
      intercept[IllegalArgumentException](SurfaceViewportFit.Contain(2.0).resolve(slots, 300.0, bad))
