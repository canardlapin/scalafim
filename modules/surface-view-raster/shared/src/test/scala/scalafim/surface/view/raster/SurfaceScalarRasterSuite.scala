package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.view.*

class SurfaceScalarRasterSuite extends munit.FunSuite:
  private val identity = new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
  private val base = Rgba32.unsafe(184, 184, 184)
  private val dimensions = RasterDimensions.unsafe(127, 129)
  private val style = SurfaceRasterStyle(culling = TriangleCulling.None)

  // Independent closed-form palette: ramp knot at scalar -0.5, limits [-1,1].
  private def expected(value: Double, threshold: Boolean): Rgba32 =
    if threshold && value > -0.25 && value < 0.25 then base
    else if value <= -1 then Rgba32.unsafe(0, 0, 200)
    else if value >= 1 then Rgba32.unsafe(200, 0, 0)
    else if value <= -0.5 then
      val t = 2 * (value + 1)
      Rgba32.unsafe(0, math.round(200 * t).toInt, math.round(200 * (1 - t)).toInt)
    else
      val t = (value + 0.5) / 1.5
      Rgba32.unsafe(math.round(200 * t).toInt, math.round(200 * (1 - t)).toInt, 0)

  test("pixels match the analytic coordinate field under clipping and perspective"):
    for perspective <- Vector(false, true); threshold <- Vector(false, true) do
      val projection = if !perspective then identity else
        new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0.5, 0, 0, 1))
      val original = SurfaceScalarFixture.plan(if threshold then SurfaceScalarFixture.thresholded else SurfaceScalarFixture.mapping)
      val clip = SurfaceClipping.worldPlanes(Vector(WorldClipPlane.unsafe(1, 0, 0, 0.6, ClipKeepSide.Positive))).toOption.get
      val plan = original.copy(camera = SurfaceCameraPacket(identity, projection, 0, 0, 1),
        viewportFit = SurfaceViewportFit.Fill, clipping = clip)
      val rendered = SurfaceRasterizer.render(plan, dimensions, style).toOption.get
      var checked = 0
      for y <- 0 until dimensions.height; x <- 0 until dimensions.width do
        val u = 2 * (x + 0.5) / dimensions.width - 1
        val v = 1 - 2 * (y + 0.5) / dimensions.height
        val worldX = if perspective then u / (1 - 0.5 * u) else u
        val worldY = if perspective then v * (1 + 0.5 * worldX) else v
        if worldX > -0.58 && worldX < 0.98 && math.abs(worldY) < 0.98 then
          val want = expected(1 + 3 * worldX, threshold)
          val actual = rendered.image.pixelUnsafe(x, y)
          assert(math.abs(actual.red - want.red) <= 1 && math.abs(actual.green - want.green) <= 1 && math.abs(actual.blue - want.blue) <= 1,
            s"perspective=$perspective threshold=$threshold ($x,$y) x=$worldX actual=$actual expected=$want")
          val pick = rendered.pick(x, y).toOption.flatten.get
          val reconstructedX = if pick.face == 0 then -pick.barycentricA + pick.barycentricB + pick.barycentricC
            else -pick.barycentricA + pick.barycentricB - pick.barycentricC
          assertEqualsDouble(reconstructedX, worldX, 2e-7)
          checked += 1
      assert(checked > 5000)
      assert(rendered.receipt.scalarFragmentsEvaluated >= checked)
      assertEquals(rendered.receipt.colorValuesComposited, 0)

  test("nonlinear mapping differs from legacy vertex color interpolation"):
    val scalar = SurfaceScalarFixture.model()
    val legacy = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
      SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, SurfaceScalarFixture.mapping.colorizer).toOption.get
    val old = SurfaceViewerModel.make(scalar.surfaces, Vector(legacy)).toOption.get
    def render(model: SurfaceViewerModel): SurfaceRasterResult =
      val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
      SurfaceRasterizer.render(plan.copy(camera = SurfaceCameraPacket(identity, identity, 0, 0, 1), viewportFit = SurfaceViewportFit.Fill), dimensions, style).toOption.get
    val actual = render(scalar)
    val before = render(old)
    assertEquals(actual.image.pixelUnsafe(63, 64), Rgba32.unsafe(200, 0, 0))
    assertNotEquals(actual.image.pixelUnsafe(63, 64), before.image.pixelUnsafe(63, 64))

  test("invalid samples do not renormalize interiors over the remaining vertices"):
    val invalid = Rgba32.unsafe(240, 0, 240)
    val model = SurfaceScalarFixture.model(SurfaceScalarFixture.mapping.copy(invalid = invalid), Array(-2, Double.NaN, 4, -2))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val result = SurfaceRasterizer.render(plan, dimensions, style).toOption.get
    var checked = 0
    for y <- 0 until dimensions.height; x <- 0 until dimensions.width; pick <- result.pick(x, y).toOption.flatten do
      if pick.face == 0 && math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.05 then
        assertEquals(result.image.pixelUnsafe(x, y), invalid)
        checked += 1
    assert(checked > 1000)

  test("split scales and opacity are evaluated at fragments before composition"):
    val blue = Rgba32.unsafe(0, 0, 200)
    val white = Rgba32.unsafe(200, 200, 200)
    val red = Rgba32.unsafe(200, 0, 0)
    val mapping = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-2, 4), 0, -1, 1,
      ScalarRamp.linear(blue, white), ScalarRamp.linear(white, red)).toOption.get)
    val model = SurfaceScalarFixture.model(mapping)
    val state = SurfaceViewer.reduce(model, SurfaceFaceFixture.state(model),
      SurfaceViewerAction.SetLayerOpacity(SurfaceScalarFixture.Layer, DisplayOpacity.unsafe(0.5))).toOption.get
    val plan = SurfaceCompiler.compile(model, state).toOption.get.copy(camera = SurfaceCameraPacket(identity, identity, 0, 0, 1), viewportFit = SurfaceViewportFit.Fill)
    val result = SurfaceRasterizer.render(plan, dimensions, style).toOption.get
    // At x=0, scalar=1 is the visible boundary of the positive tail.
    assertEquals(result.image.pixelUnsafe(63, 64), DisplayBlendMode.Normal.composite(base, white, DisplayOpacity.unsafe(0.5)))
    // x=-20/127 gives scalar within (-1,1), so the underlying surface is visible.
    assertEquals(result.image.pixelUnsafe(53, 64), base)
    val corrupt = plan.copy(layers = Vector(plan.layers.head.copy(scalarField = None)))
    assert(SurfaceRasterizer.render(corrupt, dimensions, style).isLeft)
