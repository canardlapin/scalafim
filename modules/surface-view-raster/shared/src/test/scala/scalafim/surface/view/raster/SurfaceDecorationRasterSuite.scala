package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceDecorationRasterSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(e => fail(s"$e"), identity)
  private val red = Rgba32.unsafe(255, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 255)
  private val size = RasterDimensions.unsafe(128, 128)
  private val id = SurfaceId.unsafe("anchor-surface")
  private val markerId = ok(SurfaceDecorationId.make("point"))
  private val anchor = SurfaceDecorationAnchor.Face(ok(SurfaceFaceAnchor.make(id, FaceId(0), 0, .5, .5)))

  private def composition(occluder: Boolean = false): SurfaceCompositionPlan =
    def asset(name: SurfaceId, x0: Double, z: Double, color: Rgba32): (SurfaceAsset, SurfaceLayer) =
      val mesh = SurfaceGeometry(TriangleMesh.fromRows(
        Seq(Seq(x0, -1.0, z), Seq(1.0, -1.0, z), Seq(x0, 1.0, z), Seq(1.0, 1.0, z)),
        Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
      (ok(SurfaceAsset.make(name, mesh)), ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(name.value + "-color"), name, mesh, Vector.fill(4)(color))))
    val entries = Vector(asset(id, -1, 0, blue)) ++
      (if occluder then Vector(asset(SurfaceId.unsafe("occluder"), 0, .4, blue)) else Vector.empty)
    val sources = entries.map: (asset, layer) =>
      val model = ok(SurfaceViewerModel.make(Vector(asset), Vector(layer)))
      val state = SurfaceViewerState.initial(model).copy(camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal,
        CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))), lighting = SurfaceLighting.Unlit)
      ok(SurfaceCompiler.compile(model, state))
    val resources = SurfaceCompositionResources(sources.flatMap(_.meshes).map(m => m.resourceKey -> m).toMap,
      sources.flatMap(_.layers).map(l => l.resourceKey -> l).toMap)
    val instances = sources.zipWithIndex.map: (source, index) =>
      SurfaceCompositionInstance(ok(SurfaceInstanceId.make(s"instance-$index")), source.meshes.head.surface,
        source.meshes.head.resourceKey, source.layers.map(_.resourceKey))
    val view = SurfaceCompositionView(ok(SurfaceViewId.make("view")), SurfaceViewport(0, 0, 1, 1), sources.head.camera,
      instances, SurfaceLighting.Unlit, SurfaceClipping.Disabled)
    ok(SurfaceCompositionPlan.make(Vector(view), resources))

  private def glyph(plan: SurfaceCompositionPlan, depth: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest): SurfaceDecorationGeometry =
    val marker = SurfaceMarkerDecoration(markerId, anchor, SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(12)), red, depth)
    ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector(marker), plan)), plan, size.width, size.height))

  test("pixel disk draws at its declared size and returns decoration identity without changing scientific buffers"):
    val plan = composition()
    val geometry = glyph(plan)
    val rendered = ok(SurfaceDecorationRaster.render(plan, geometry, size))
    val points = for y <- 0 until 128; x <- 0 until 128 if rendered.image.pixelUnsafe(x, y) == red yield (x, y)
    assertEquals(points.map(_._1).max - points.map(_._1).min + 1, 24)
    assertEquals(points.map(_._2).max - points.map(_._2).min + 1, 24)
    val hit = ok(rendered.pick(64, 64)).get
    assert(hit.isInstanceOf[SurfaceDisplayHit.Decoration[?]])
    hit match
      case SurfaceDisplayHit.Decoration(value) =>
        assertEquals(value.id.decoration, markerId)
        assertEquals(value.target, SurfaceDecorationTarget.Anchor(anchor))
      case _ => fail("scientific ID used for a decoration")
    assertEquals(ok(rendered.scientific.pick(64, 64)).get.surface, id)
    assertEquals(rendered.scientific.image, ok(SurfaceRasterizer.render(plan, size)).image)

  test("foreground surface occludes a depth-tested marker but explicit overlay remains visible"):
    val plan = composition(occluder = true)
    val depth = ok(SurfaceDecorationRaster.render(plan, glyph(plan), size))
    val overlay = ok(SurfaceDecorationRaster.render(plan, glyph(plan, SurfaceDecorationDepthPolicy.Overlay), size))
    assertEquals(depth.image.pixelUnsafe(69, 64), blue)
    assertEquals(overlay.image.pixelUnsafe(69, 64), red)
    assert(ok(depth.pick(69, 64)).get.isInstanceOf[SurfaceDisplayHit.Scientific[?]])
    assert(ok(overlay.pick(69, 64)).get.isInstanceOf[SurfaceDisplayHit.Decoration[?]])

  test("transparent label texels preserve the surface and labels pass through picking"):
    val plan = composition()
    val label = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("label")), anchor, "A",
      ok(SurfaceDecorationSize.pixels(8)), red))
    val geometry = ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector(label), plan)), plan, 128, 128))
    assert(SurfaceDecorationRaster.render(plan, geometry, size).isLeft)
    val rasterizer = new SurfaceLabelRasterizer:
      def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
        val pixels = Array.fill(25)(red.toPackedInt)
        for y <- 1 to 3; x <- 1 to 3 do pixels(y * 5 + x) = 0
        Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(5, 5), pixels), 1, 2.5, 2.5))
    val resolved = ok(SurfaceLabelLayout.resolve(geometry, plan, rasterizer))
    val rendered = ok(SurfaceDecorationRaster.render(plan, resolved, size))
    assertEquals(rendered.image.pixelUnsafe(64, 64), blue)
    assertEquals(rendered.image.pixelUnsafe(45, 64), red)
    assert(ok(rendered.pick(45, 64)).get.isInstanceOf[SurfaceDisplayHit.Scientific[?]])

  test("stale camera/size geometry and invalid buffers fail before rendering"):
    val plan = composition()
    val geometry = glyph(plan)
    assert(SurfaceDecorationRaster.render(plan, geometry, RasterDimensions.unsafe(256, 128)).isLeft)
    val view = plan.views.head
    val p = Array.tabulate(16)(view.camera.projectionMatrix(_))
    p(0) *= 2
    val changed = ok(SurfaceCompositionPlan.make(Vector(view.copy(camera = view.camera.copy(projectionMatrix = new FloatBufferView(p)))), plan.resources))
    assert(SurfaceDecorationRaster.render(changed, geometry, size).isLeft)
    intercept[IllegalArgumentException]:
      geometry.copy(meshes = geometry.meshes.map(_.copy(indices = new IntBufferView(Array(999, 0, 1)))))

  test("shared annotation ray picking agrees with raster visibility and coplanar tie ownership"):
    val plan = composition(occluder = true)
    for policy <- Vector(SurfaceDecorationDepthPolicy.DepthTest, SurfaceDecorationDepthPolicy.Overlay) do
      val geometry = glyph(plan, policy)
      val rendered = ok(SurfaceDecorationRaster.render(plan, geometry, size))
      var compared = 0
      for y <- 52 until 76; x <- 52 until 76 do
        val nativeDepth = ok(rendered.scientific.pick(x, y)).map(_.depth)
        val analytic = SurfaceDecorationPicking.pick(geometry, plan, x + .5, y + .5, nativeDepth)
        val raster = ok(rendered.pick(x, y)).collect { case SurfaceDisplayHit.Decoration(value) => value }
        assertEquals(analytic.map(_.id), raster.map(_.id), s"$policy at $x,$y native=$nativeDepth analytic=$analytic")
        if analytic.nonEmpty then compared += 1
      assert(compared > 100)
