package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceFragmentCompositionSuite extends munit.FunSuite:
  private val surface = SurfaceScalarFixture.Surface
  private val geometry = SurfaceFaceFixture.geometry
  private val identity = new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
  private def gray(value: Int): Rgba32 = Rgba32.unsafe(value, value, value)
  private def fixed(value: Int): ScalarMapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
    ScalarRamp.linear(gray(value), gray(value))))
  private def plan(layers: Vector[SurfaceLayer]): SurfaceRenderPlan =
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers).toOption.get
    SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get.copy(
      camera = SurfaceCameraPacket(identity, identity, 0, 0, 1), viewportFit = SurfaceViewportFit.Fill)

  test("all blend modes compose independently mapped layers at the fragment"):
    val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("under"), surface, geometry,
      Array.fill(4)(0.5), fixed(100)).toOption.get
    for (mode, expected) <- Vector(DisplayBlendMode.Normal -> 150, DisplayBlendMode.Additive -> 178,
        DisplayBlendMode.Multiply -> 89, DisplayBlendMode.Screen -> 161) do
      val over = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("over"), surface, geometry,
        Array.fill(4)(0.5), fixed(200), opacity = DisplayOpacity.unsafe(0.5), blendMode = mode).toOption.get
      val result = SurfaceRasterizer.render(plan(Vector(under, over)), RasterDimensions.unsafe(65, 65),
        SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
      assertEquals(result.image.pixelUnsafe(32, 32), gray(expected))
      assert(result.receipt.scalarFragmentsEvaluated > result.receipt.shadedPixels)

  test("thresholded scalar overlay reveals the independently interpolated curvature underlay"):
    val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("curvature"), surface, geometry,
      Array(0.0, 1.0, 1.0, 0.0), ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
        ScalarRamp.linear(gray(0), gray(200))))).toOption.get
    val overlay = SurfaceScalarFixture.model(SurfaceScalarFixture.thresholded).layers.head
    val result = SurfaceRasterizer.render(plan(Vector(under, overlay)), RasterDimensions.unsafe(127, 127),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    var checked = 0
    for y <- 10 until 117; x <- 10 until 117 do
      val worldX = 2 * (x + 0.5) / 127 - 1
      val scalar = 1 + 3 * worldX
      if math.abs(scalar) < 0.2 then
        val expected = math.round(100 * (1 + worldX)).toInt
        val actual = result.image.pixelUnsafe(x, y)
        assert(math.abs(actual.red - expected) <= 1 && actual.red == actual.green && actual.green == actual.blue)
        checked += 1
    assert(checked > 500)

  test("nearest regions do not flatten a coexisting color-interpolated layer"):
    val gradient = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("gradient"), surface, geometry,
      Vector(gray(0), gray(200), gray(200), gray(0))).toOption.get
    val transparentNearest = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("nearest"), surface, geometry,
      Vector.fill(4)(Rgba32.unsafe(0, 0, 0, 0)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val compiled = plan(Vector(gradient, transparentNearest))
    assert(compiled.meshes.head.nearestPartition.nonEmpty)
    val result = SurfaceRasterizer.render(compiled, RasterDimensions.unsafe(127, 127),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    for y <- 5 until 122; x <- 5 until 122 do
      val expected = math.round(200 * (x + 0.5) / 127).toInt
      assert(math.abs(result.image.pixelUnsafe(x, y).red - expected) <= 1)
      assert(result.pick(x, y).toOption.flatten.nonEmpty)

  test("lighting uses the normalized interpolated normal after composition"):
    val layer = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("lit"), surface, geometry,
      Array.fill(4)(0.5), fixed(150)).toOption.get
    val compiled = plan(Vector(layer))
    val mesh = compiled.meshes.head.copy(normals = new FloatBufferView(Array[Float](1,0,0, 0,1,0, 0,0,1, 0,0,1)))
    val lighting = SurfaceLighting.directional(0.2, 0.8, 0, 0, 1).toOption.get
    val evaluator = new SurfaceFragmentEvaluator(mesh, compiled.layers, lighting, gray(184))
    val expected = math.round(150 * (0.2 + 0.8 / math.sqrt(3))).toInt
    assertEquals(evaluator.color(0, 0, 1, 2, 1.0/3, 1.0/3, 1.0/3), gray(expected))
    assertNotEquals(expected, 70) // Interpolating already-lit corners would give 70.

  test("face samples compose with scalar fields without face-to-vertex conversion"):
    val under = SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe("faces"), surface,
      SurfaceFaceField.make(geometry, Array(gray(40), gray(160))).toOption.get)
    val over = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("scalar"), surface, geometry,
      Array.fill(4)(0.5), fixed(200), opacity = DisplayOpacity.unsafe(0.5)).toOption.get
    val result = SurfaceRasterizer.render(plan(Vector(under, over)), RasterDimensions.unsafe(65, 65),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    for y <- 5 until 60; x <- 5 until 60; pick <- result.pick(x, y).toOption.flatten do
      assertEquals(result.image.pixelUnsafe(x, y), gray(if pick.face == 0 then 120 else 180))

  test("network pixels keep their own color and identity over a scalar and nearest surface"):
    val scalar = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("scalar"), surface, geometry,
      Array.fill(4)(0.5), fixed(100).copy(invalid = Rgba32.unsafe(255, 0, 255))).toOption.get
    val nearest = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("nearest"), surface, geometry,
      Vector.fill(4)(gray(40)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val nodes = Vector(0, 2).map(v => SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"v$v"),
      surface, VertexId(v), geometry).toOption.get)
    val network = SurfaceNetwork.make(nodes, Vector(SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 1))).toOption.get
    val display = SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.1), sides = 8).toOption.get).toOption.get
    val attached = SurfaceNetworkCompiler.attach(plan(Vector(nearest, scalar)), surface,
      SurfaceLayerId.unsafe("network"), display).toOption.get.plan
    val result = SurfaceRasterizer.render(attached, RasterDimensions.unsafe(127, 127),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    var networkPixels = 0
    var surfacePixels = 0
    for y <- 10 until 117; x <- 10 until 117; pick <- result.pick(x, y).toOption.flatten do
      val isNetwork = pick.face >= geometry.faceCount
      assertEquals(result.image.pixelUnsafe(x, y), if isNetwork then display.edges.head.color else gray(100))
      if isNetwork then
        assert(pick.vertex >= geometry.vertexCount)
        networkPixels += 1
      else surfacePixels += 1
    assert(networkPixels > 500 && surfacePixels > 5000)
