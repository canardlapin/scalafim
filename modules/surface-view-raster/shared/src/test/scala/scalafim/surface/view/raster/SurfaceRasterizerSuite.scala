package scalafim.surface.view.raster

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceRasterizerSuite extends munit.FunSuite:
  test("CPU projection and signed network tubes render through the reference backend"):
    val plan = SurfaceFeatureFixture.plan
    val rendered = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(160, 160)).toOption.get
    assertEquals(plan.meshes.length, 1)
    assertEquals(plan.layers.length, 2)
    assert(rendered.receipt.trianglesInput > 4)
    assert(rendered.receipt.shadedPixels > 0)

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
    SurfaceCompiler.compile(model, unlit).toOption.get

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
    assertEquals(hash(first.image), -243859455)
    assert(first.receipt.shadedPixels > 1000)
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

  test("overlays composite in model order with exact packed source-over semantics"):
    val mesh = triangleGeometry()
    val red = packedLayer(mesh, "red", Vector.fill(3)(Rgba32.unsafe(255, 0, 0)))
    val blue = packedLayer(mesh, "blue", Vector.fill(3)(Rgba32.unsafe(0, 0, 255, 128)))
    val result = SurfaceRasterizer.render(plan(mesh, Vector(red, blue)), dimensions).toOption.get
    assertEquals(result.image.pixelUnsafe(30, 40), Rgba32.unsafe(127, 0, 128))
    assertEquals(result.receipt.colorValuesComposited, 6)

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
