package scalafim.surface.view.raster

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceRasterizerSuite extends munit.FunSuite:
  private def assertStrips(compiled: SurfaceRenderPlan, size: RasterDimensions): Unit =
    val style = SurfaceRasterStyle(culling = TriangleCulling.None)
    val full = SurfaceRasterizer.render(compiled, size, style).toOption.get.image
    for rows <- Vector(1, 7, 64, size.height + 1) do
      var nextRow = 0
      val receipt = SurfaceRasterizer.renderStrips(compiled, size, SurfaceStripConfig(rows), style): strip =>
        assertEquals(strip.firstRow, nextRow)
        assertEquals(strip.fullDimensions, size)
        assertEquals(strip.image.width, size.width)
        assert(strip.image.height <= rows)
        for y <- 0 until strip.image.height; x <- 0 until size.width do
          assertEquals(strip.image.pixelUnsafe(x, y), full.pixelUnsafe(x, nextRow + y))
        nextRow += strip.image.height
        Right(())
      assertEquals(nextRow, size.height)
      assertEquals(receipt.toOption.get.delivered, (size.height + rows - 1) / rows)
      assert(!receipt.toOption.get.capabilities.supports(SurfaceBackendFeature.NativePicking))
      assert(receipt.toOption.get.capabilities.supports(SurfaceBackendFeature.Lighting))

  test("render-only strips preserve face data and thresholded scalar fragment meaning"):
    assertStrips(SurfaceFaceFixture.plan, RasterDimensions.unsafe(67, 71))
    assertStrips(SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded), RasterDimensions.unsafe(79, 61))

  test("CPU projection and signed network tubes render through the reference backend"):
    val plan = SurfaceFeatureFixture.plan
    val rendered = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(160, 160)).toOption.get
    assertEquals(plan.meshes.length, 1)
    assertEquals(plan.layers.length, 2)
    assert(rendered.receipt.trianglesInput > 4)
    assert(rendered.receipt.shadedPixels > 0)
    assertStrips(plan, RasterDimensions.unsafe(83, 59))

  private val dimensions = RasterDimensions.unsafe(64, 64)
  private val surfaceId = SurfaceId.unsafe("asymmetric-left")

  test("reference backend publishes stable capabilities and explicit caveats"):
    val capabilities = SurfaceRasterizer.capabilities
    assertEquals(capabilities.id.value, "reference-raster")
    assert(capabilities.supports(SurfaceBackendFeature.DeterministicPixels))
    assert(capabilities.supports(SurfaceBackendFeature.WorldClipping))
    assert(!capabilities.caveats.exists(_.contains("world clipping")))

  private def geometry(
    vertices: Seq[Seq[Double]],
    faces: Seq[(Int, Int, Int)],
    hemisphere: Hemisphere = Hemisphere.Left,
    transform: DMat = DMat.eye(4)
  ): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(vertices, faces), hemisphere, SurfaceKind.Inflated, transform)

  private def plan(
    geometry: SurfaceGeometry,
    layers: Vector[SurfaceLayer],
    viewpoint: SurfaceViewpoint = SurfaceViewpoint.Dorsal
  ): SurfaceRenderPlan =
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, geometry).toOption.get),
      layers
    ).toOption.get
    val initial = SurfaceViewerState.initial(model)
    val viewed = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetViewpoint(viewpoint)).toOption.get
    val unlit = SurfaceViewer.reduce(model, viewed, SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)).toOption.get
    // Raster pixel/clipping oracles use an explicit projection, independent of
    // automatic perspective camera fitting in the interactive viewer.
    val fixed = SurfaceViewer.reduce(model, unlit,
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.25)))).toOption.get
    SurfaceCompiler.compile(model, fixed).toOption.get

  private def triangleGeometry(faces: Seq[(Int, Int, Int)] = Seq((0, 1, 2))): SurfaceGeometry =
    geometry(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.6, 0.8, 0.0)),
      faces
    )

  private def packedLayer(
    geometry: SurfaceGeometry,
    id: String,
    colors: Vector[Rgba32],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blend: DisplayBlendMode = DisplayBlendMode.Normal
  ): SurfaceLayer =
    SurfaceLayer.packedRgba(
      SurfaceLayerId.unsafe(id), surfaceId, geometry, colors,
      opacity = opacity, blendMode = blend
    ).toOption.get

  private def hash(image: RasterImage): Int =
    var result = 1
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        result = 31 * result + image.pixelUnsafe(x, y).toPackedInt
        x += 1
      y += 1
    result

  test("unlit asymmetric landmark image and picks are deterministic"):
    val mesh = triangleGeometry()
    val layer = packedLayer(mesh, "landmarks", Vector(
      Rgba32.unsafe(255, 0, 0),
      Rgba32.unsafe(0, 255, 0),
      Rgba32.unsafe(0, 0, 255)
    ))
    val renderPlan = plan(mesh, Vector(layer))
    val first = SurfaceRasterizer.render(renderPlan, dimensions).toOption.get
    val second = SurfaceRasterizer.render(renderPlan, dimensions).toOption.get
    assertEquals(first.image, second.image)
    assertEquals(hash(first.image), hash(second.image))
    // Orthographic scale 1.25 puts the triangle at pixel coordinates
    // (6.4,55.04), (57.6,55.04), (16.64,8.96). Independent half-plane
    // enumeration gives 1175 covered centers; barycentrics at (30.5,40.5)
    // give RGB (71,104,80), within one byte of floating-point quantization.
    assertEquals(first.receipt.shadedPixels, 1175)
    val sample = first.image.pixelUnsafe(30, 40)
    assert(math.abs(sample.red - 71) <= 1 && math.abs(sample.green - 104) <= 1 && math.abs(sample.blue - 80) <= 1)
    assertEquals(first.receipt.trianglesInput, 1)
    assertEquals(first.receipt.trianglesAfterClipping, 1)
    assertEquals(first.receipt.trianglesCulled, 0)

    val observed = SurfaceRasterizer.renderObserved(
      renderPlan,
      dimensions,
      path = SurfaceAdmissionPath.Snapshot
    ).toOption.get
    assertEquals(SurfaceBackendAdmission.validate(observed.observation), Vector.empty)
    assertEquals(observed.observation.geometryUploads, 0)
    assertEquals(observed.observation.layerUploads, 0)
    assertEquals(observed.observation.drawCalls, renderPlan.receipt.drawPassCount)

    val left = first.pick(9, 54).toOption.flatten.get
    val right = first.pick(54, 54).toOption.flatten.get
    val superior = first.pick(17, 14).toOption.flatten.get
    assertEquals(left.vertex, 0)
    assertEquals(right.vertex, 1)
    assertEquals(superior.vertex, 2)
    assertEquals(left.face, 0)
    assertEqualsDouble(left.barycentricA + left.barycentricB + left.barycentricC, 1.0, 1e-5)
    assert(first.pick(-1, 0).isLeft)

  test("world translation preserves framing, pixels, and picks"):
    val base = triangleGeometry()
    val translation = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 10.0),
      Vector(0.0, 1.0, 0.0, 20.0),
      Vector(0.0, 0.0, 1.0, 30.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val moved = SurfaceGeometry(base.mesh, base.hemisphere, base.kind, translation)
    val colors = Vector(
      Rgba32.unsafe(255, 0, 0),
      Rgba32.unsafe(0, 255, 0),
      Rgba32.unsafe(0, 0, 255)
    )
    val baseResult = SurfaceRasterizer.render(
      plan(base, Vector(packedLayer(base, "base", colors))),
      dimensions
    ).toOption.get
    val movedResult = SurfaceRasterizer.render(
      plan(moved, Vector(packedLayer(moved, "moved", colors))),
      dimensions
    ).toOption.get
    assertEquals(movedResult.image, baseResult.image)
    Vector((9, 54), (54, 54), (17, 14)).foreach: (x, y) =>
      val basePick = baseResult.pick(x, y).toOption.flatten.get
      val movedPick = movedResult.pick(x, y).toOption.flatten.get
      assertEquals(movedPick.surface, basePick.surface)
      assertEquals(movedPick.face, basePick.face)
      assertEquals(movedPick.vertex, basePick.vertex)

  test("depth buffering keeps the nearer face independent of draw order"):
    val mesh = geometry(
      Seq(
        Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.6, 0.8, 0.0),
        Seq(-1.0, -1.0, -1.0), Seq(1.0, -1.0, -1.0), Seq(-0.6, 0.8, -1.0)
      ),
      Seq((0, 1, 2), (3, 4, 5))
    )
    val layer = packedLayer(mesh, "depth", Vector(
      Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(255, 0, 0),
      Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(0, 0, 255)
    ))
    val result = SurfaceRasterizer.render(plan(mesh, Vector(layer)), dimensions).toOption.get
    val center = result.image.pixelUnsafe(30, 40)
    assert(center.red > 240)
    assert(center.blue < 15)
    assert(result.receipt.depthRejectedPixels > 0)
    assertEquals(result.pick(30, 40).toOption.flatten.map(_.face), Some(0))

  test("back-face culling is explicit and reversed winding remains renderable when disabled"):
    val reversed = triangleGeometry(Seq((0, 2, 1)))
    val layer = packedLayer(reversed, "reversed", Vector.fill(3)(Rgba32.unsafe(255, 0, 0)))
    val renderPlan = plan(reversed, Vector(layer))
    val culled = SurfaceRasterizer.render(renderPlan, dimensions).toOption.get
    assertEquals(culled.receipt.trianglesCulled, 1)
    assertEquals(culled.receipt.shadedPixels, 0)
    val twoSided = SurfaceRasterizer.render(
      renderPlan,
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).toOption.get
    assert(twoSided.receipt.shadedPixels > 1000)

  test("homogeneous clipping triangulates visible polygon without per-pixel allocation semantics"):
    val mesh = geometry(
      Seq(Seq(-10.0, -1.0, 0.0), Seq(10.0, -1.0, 0.0), Seq(0.0, 10.0, 0.0)),
      Seq((0, 1, 2))
    )
    val layer = packedLayer(mesh, "clipped", Vector.fill(3)(Rgba32.unsafe(240, 120, 20)))
    val result = SurfaceRasterizer.render(
      plan(mesh, Vector(layer)),
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).toOption.get
    assertEquals(result.receipt.trianglesInput, 1)
    assert(result.receipt.trianglesAfterClipping >= 2)
    assert(result.receipt.shadedPixels > 0)

  test("world clipping uses normalized planes and explicit keep-side semantics"):
    val mesh = triangleGeometry()
    val layer = packedLayer(mesh, "world-clipped", Vector.fill(3)(Rgba32.unsafe(220, 60, 20)))
    val unclippedPlan = plan(mesh, Vector(layer))
    val clipping = SurfaceClipping.worldPlanes(Vector(
      WorldClipPlane.unsafe(1.0, 0.0, 0.0, 0.0, ClipKeepSide.Positive)
    )).toOption.get
    val clipped = SurfaceRasterizer.render(
      unclippedPlan.copy(clipping = clipping),
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).toOption.get
    val full = SurfaceRasterizer.render(
      unclippedPlan,
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).toOption.get
    assert(clipped.receipt.shadedPixels > 0)
    assert(clipped.receipt.shadedPixels < full.receipt.shadedPixels)
    assert(clipped.receipt.trianglesAfterClipping >= 1)
    assertEquals(clipped.pick(9, 54).toOption.flatten, None)
    assert(clipped.pick(54, 54).toOption.flatten.nonEmpty)
    assertStrips(unclippedPlan.copy(clipping = clipping), RasterDimensions.unsafe(65, 67))

  test("overlays composite in model order with exact packed source-over semantics"):
    val mesh = triangleGeometry()
    val red = packedLayer(mesh, "red", Vector.fill(3)(Rgba32.unsafe(255, 0, 0)))
    val blue = packedLayer(mesh, "blue", Vector.fill(3)(Rgba32.unsafe(0, 0, 255, 128)))
    val result = SurfaceRasterizer.render(plan(mesh, Vector(red, blue)), dimensions).toOption.get
    assertEquals(result.image.pixelUnsafe(30, 40), Rgba32.unsafe(127, 0, 128))
    assertEquals(result.receipt.colorValuesComposited, 6)
    assertStrips(plan(mesh, Vector(red, blue)), RasterDimensions.unsafe(65, 67))

  test("bilateral slots keep left and right surface picks distinct"):
    val leftId = SurfaceId.unsafe("left")
    val rightId = SurfaceId.unsafe("right")
    val left = triangleGeometry()
    val right = geometry(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.6, 0.8, 0.0)),
      Seq((0, 1, 2)),
      Hemisphere.Right
    )
    val viewer = SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, left).toOption.get,
        SurfaceAsset.make(rightId, right).toOption.get
      ),
      Vector.empty
    ).toOption.get
    val initial = SurfaceViewerState.initial(viewer)
    val laidOut = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(leftId, rightId))
    ).toOption.get
    val dorsal = SurfaceViewer.reduce(viewer, laidOut, SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)).toOption.get
    val result = SurfaceRasterizer.render(
      SurfaceCompiler.compile(viewer, dorsal).toOption.get,
      dimensions
    ).toOption.get
    assertEquals(result.pick(15, 40).toOption.flatten.map(_.surface), Some(leftId))
    assertEquals(result.pick(47, 40).toOption.flatten.map(_.surface), Some(rightId))

  test("raster receipts expose phase timings and exact work counters"):
    val mesh = triangleGeometry()
    val layer = packedLayer(mesh, "receipt", Vector.fill(3)(Rgba32.unsafe(10, 20, 30)))
    val result = SurfaceRasterizer.render(plan(mesh, Vector(layer)), dimensions).toOption.get
    assert(result.receipt.setupNanos >= 0L)
    assert(result.receipt.renderNanos >= 0L)
    assertEquals(result.receipt.colorValuesComposited, 3)
    assertEquals(result.receipt.trianglesClippedAway, 0)

  test("compact bilateral raster placement and picking share the same physical viewport centers"):
    val ids = Vector(SurfaceId.unsafe("left"), SurfaceId.unsafe("right"))
    val assets = ids.zip(Vector(Hemisphere.Left, Hemisphere.Right)).map: (id, hemisphere) =>
      val quad = geometry(Seq(Seq(-1.0,-1.0,0.0), Seq(1.0,-1.0,0.0), Seq(1.0,1.0,0.0), Seq(-1.0,1.0,0.0)),
        Seq((0,1,2),(0,2,3)), hemisphere)
      SurfaceAsset.make(id, quad).toOption.get
    val model = SurfaceViewerModel.make(assets, Vector.empty).toOption.get
    val state = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(ids(0), ids(1)))).foldLeft(SurfaceViewerState.initial(model)):
      (state, action) => SurfaceViewer.reduce(model, state, action).toOption.get
    val compiled = SurfaceCompiler.compile(model, state).toOption.get
    val rendered = SurfaceRasterizer.render(compiled, RasterDimensions.unsafe(600,160)).toOption.get
    assertEquals(rendered.pick(220,80).toOption.flatten.map(_.surface), Some(ids(0)))
    assertEquals(rendered.pick(380,80).toOption.flatten.map(_.surface), Some(ids(1)))
    assertEquals(rendered.pick(150,80).toOption.flatten, None)
    assertEquals(rendered.pick(450,80).toOption.flatten, None)
    assertStrips(compiled, RasterDimensions.unsafe(601, 163))

  test("strip preflight bounds primitive buffers against declared 3GiB heap headroom"):
    val compiled = plan(triangleGeometry(), Vector.empty)
    val size = RasterDimensions.unsafe(3600, 2032)
    val config = SurfaceStripConfig.forHeap(3L * 1024 * 1024 * 1024, 2L * 1024 * 1024 * 1024).toOption.get
    val preflight = SurfaceRasterizer.preflightStrips(compiled, size, config).toOption.get
    assertEquals(preflight.maxBufferBytes, 3600L * 64 * 24 + 3L * 4)
    assertEquals(preflight.strips, 32)
    assert(SurfaceStripConfig.forHeap(3, 3).isLeft)
    var calls = 0
    val refused = SurfaceRasterizer.renderStrips(compiled, size, SurfaceStripConfig(64, 1)): _ =>
      calls += 1
      Right(())
    assert(refused.left.toOption.exists:
      case SurfaceRasterError.MemoryBudgetExceeded(_, _) => true
      case _ => false)
    assertEquals(calls, 0)

  test("strip cancellation and sink failure never admit a partial image"):
    val compiled = plan(triangleGeometry(), Vector.empty)
    var delivered = 0
    var cancel = true
    def sink(strip: SurfaceRasterStrip): Either[String, Unit] =
      assertEquals(strip.firstRow, 0)
      delivered += 1
      cancel = true
      Right(())
    assertEquals(SurfaceRasterizer.renderStrips(compiled, dimensions, cancelled = () => cancel)(sink),
      Left(SurfaceRasterError.Cancelled))
    assertEquals(delivered, 0)
    cancel = false
    assertEquals(SurfaceRasterizer.renderStrips(compiled, dimensions, SurfaceStripConfig(7), cancelled = () => cancel)(sink),
      Left(SurfaceRasterError.Cancelled))
    assertEquals(delivered, 1)
    var checks = 0
    val during = SurfaceRasterizer.renderStrips(compiled, dimensions,
      cancelled = () => { checks += 1; checks >= 3 })(_ => fail("Cancelled strip reached sink"))
    assertEquals(during, Left(SurfaceRasterError.Cancelled))
    val refused = SurfaceRasterizer.renderStrips(compiled, dimensions)(_ => Left("output unavailable"))
    assertEquals(refused, Left(SurfaceRasterError.SinkFailure("output unavailable")))
    val thrown = SurfaceRasterizer.renderStrips(compiled, dimensions)(_ => throw IllegalStateException("sink closed"))
    assertEquals(thrown, Left(SurfaceRasterError.SinkFailure("sink closed")))
