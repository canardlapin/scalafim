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

  test("portrait pairing uses two full-width tiles instead of a short centered row"):
    val fitted = SurfaceViewportFit.Pack(1.0).resolve(slots, 400.0, 800.0)
    assertEquals(fitted.map(_.viewport), Vector(
      SurfaceViewport(0.0, 0.0, 1.0, 0.5), SurfaceViewport(0.0, 0.5, 1.0, 0.5)))
    val old = SurfaceViewportFit.Contain(2.0).resolve(slots, 400.0, 800.0).head.viewport
    val next = fitted.head.viewport
    assertEqualsDouble(next.width * next.height / (old.width * old.height), 4.0, 1e-12)

  test("wide pairing keeps the centered row and a square canvas deterministically prefers a row"):
    val wide = SurfaceViewportFit.Pack(1.0).resolve(slots, 1400.0, 300.0)
    assertEqualsDouble(wide.head.viewport.x * 1400.0, 400.0, 1e-9)
    assertEqualsDouble(wide.last.viewport.x * 1400.0, 700.0, 1e-9)
    wide.foreach: slot =>
      assertEqualsDouble(slot.viewport.y, 0.0, 1e-12)
      assertEqualsDouble(slot.viewport.width * 1400.0, 300.0, 1e-9)
      assertEqualsDouble(slot.viewport.height * 300.0, 300.0, 1e-9)
    val tied = SurfaceViewportFit.Pack(1.0).resolve(slots, 600.0, 600.0)
    assertEquals(tied.map(_.viewport), Vector(
      SurfaceViewport(0.0, 0.25, 0.5, 0.5), SurfaceViewport(0.5, 0.25, 0.5, 0.5)))

  test("pairing attains the analytic maximum without distortion, overlap or identity changes"):
    for
      width <- Vector(319.0, 640.0, 1023.0, 1800.0)
      height <- Vector(247.0, 700.0, 1201.0)
      aspect <- Vector(0.7, 1.0, 1.37, 2.1)
      source <- Vector(slots, slots.reverse)
    do
      val fitted = SurfaceViewportFit.Pack(aspect).resolve(source, width, height)
      val horizontalWidth = math.min(width / 2.0, height * aspect)
      val verticalWidth = math.min(width, height * aspect / 2.0)
      val expectedWidth = math.max(horizontalWidth, verticalWidth)
      fitted.zip(source).foreach: (slot, original) =>
        val v = slot.viewport
        assertEquals(slot.copy(viewport = original.viewport), original)
        assert(v.x >= -1e-12 && v.y >= -1e-12 && v.x + v.width <= 1.0 + 1e-12 && v.y + v.height <= 1.0 + 1e-12)
        assertEqualsDouble(v.width * width, expectedWidth, 1e-9)
        assertEqualsDouble(v.width * width / (v.height * height), aspect, 1e-12)
      val a = fitted.head.viewport
      val b = fitted.last.viewport
      assert(a.x + a.width <= b.x + 1e-12 || a.y + a.height <= b.y + 1e-12)
      assertEqualsDouble(a.x + b.x + b.width, 1.0, 1e-12)
      assertEqualsDouble(a.y + b.y + b.height, 1.0, 1e-12)

  test("packing centers a partial last row at the same physical scale"):
    val source = Vector.tabulate(5)(i => left.copy(surface = SurfaceId.unsafe(s"surface-$i")))
    val fitted = SurfaceViewportFit.Pack(1.0).resolve(source, 600.0, 400.0)
    assertEquals(fitted.map(_.surface), source.map(_.surface))
    fitted.zip(Vector((0.0, 0.0), (200.0, 0.0), (400.0, 0.0), (100.0, 200.0), (300.0, 200.0))).foreach:
      case (slot, (x, y)) =>
        assertEqualsDouble(slot.viewport.x * 600.0, x, 1e-9)
        assertEqualsDouble(slot.viewport.y * 400.0, y, 1e-9)
        assertEqualsDouble(slot.viewport.width * 600.0, 200.0, 1e-9)
        assertEqualsDouble(slot.viewport.height * 400.0, 200.0, 1e-9)

  test("single focus fills its available aspect and returning to a pair is deterministic"):
    val fit = SurfaceViewportFit.Pack(1.5)
    val paired = fit.resolve(slots, 400.0, 800.0)
    val focused = fit.resolve(Vector(right), 400.0, 800.0).head
    assertEqualsDouble(focused.viewport.width * 400.0, 400.0, 1e-9)
    assertEqualsDouble(focused.viewport.height * 800.0, 400.0 / 1.5, 1e-9)
    assertEquals(focused.copy(viewport = right.viewport), right)
    assertEquals(fit.resolve(slots, 400.0, 800.0), paired)
    assertEquals(fit.resolve(Vector.empty, 400.0, 800.0), Vector.empty)
    for bad <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      intercept[IllegalArgumentException](SurfaceViewportFit.Pack(bad))
      intercept[IllegalArgumentException](fit.resolve(slots, bad, 300.0))
      intercept[IllegalArgumentException](fit.resolve(slots, 300.0, bad))
