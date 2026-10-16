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

  private def orderedPair(plan: SurfaceCompositionPlan): SurfaceDecorationGeometry =
    val style = SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(4)), red,
      Some(SurfaceContourHalo(Rgba32.unsafe(255, 0, 0, 153), ok(SurfaceDecorationSize.pixels(20)))),
      mode = SurfaceContourMode.RoundedStrokes)
    val contour = SurfaceContourDecoration(markerId, id, Vector(0, 1, 1, 1), SurfaceContourPolicy.Transitions, style)
    val source = ok(SurfaceDecorationCompiler.compile(Vector(contour), plan))
    def stroke(name: String, z: Float, color: Rgba32): SurfaceDecorationMesh =
      val packet = source.contours.head.copy(id = SurfaceDecorationElementId(ok(SurfaceDecorationId.make(name)), "stroke"),
        start = SurfaceDecorationWorldPoint(-.3, 0, z), end = SurfaceDecorationWorldPoint(.3, 0, z),
        style = style.copy(halo = style.halo.map(_.copy(color = color))))
      ok(SurfaceDecorationGeometry.lower(source.copy(contours = Vector(packet)), plan, size.width, size.height)).meshes.last
    glyph(plan).copy(meshes = Vector(stroke("near-red", .3f, Rgba32.unsafe(255, 0, 0, 153)),
      stroke("far-green", .1f, Rgba32.unsafe(0, 255, 0, 153))))

  test("ordered depth-tested strokes blend by declaration without writing depth and picks follow the visible last stroke"):
    val plan = composition()
    val geometry = orderedPair(plan)
    val result = ok(SurfaceDecorationRaster.render(plan, geometry, size))
    assertEquals(result.image.pixelUnsafe(64, 64), Rgba32.unsafe(61, 153, 41))
    assertEquals(ok(result.pick(64, 64)).collect { case SurfaceDisplayHit.Decoration(hit) => hit.id }, Some(geometry.meshes.last.id))
    val depth = ok(result.scientific.pick(64, 64)).map(_.depth)
    assertEquals(SurfaceDecorationPicking.pick(geometry, plan, 64.5, 64.5, depth).map(_.id), Some(geometry.meshes.last.id))
    val reversed = ok(SurfaceDecorationRaster.render(plan, geometry.copy(meshes = geometry.meshes.reverse), size))
    assertEquals(reversed.image.pixelUnsafe(64, 64), Rgba32.unsafe(153, 61, 41))
    assert(SurfaceDecorationRaster.renderChecked(plan, geometry, size, SurfaceRasterStyle(), 1).isLeft)

  test("ordered strokes respect scientific and nonpickable opaque decoration depth while overlays remain final"):
    val plan = composition(occluder = true)
    val geometry = orderedPair(plan)
    val result = ok(SurfaceDecorationRaster.render(plan, geometry, size))
    assertEquals(result.image.pixelUnsafe(69, 64), blue)
    assertEquals(result.image.pixelUnsafe(59, 64), Rgba32.unsafe(61, 153, 41))
    val plain = glyph(plan).meshes.head
    val blocker = plain.copy(pickable = false,
      positions = new FloatBufferView(Array.tabulate(plain.positions.length)(i =>
        if i % 3 == 2 then .5f else plain.positions(i))))
    val hidden = geometry.copy(meshes = Vector(blocker) ++ geometry.meshes)
    val hiddenResult = ok(SurfaceDecorationRaster.render(plan, hidden, size))
    assertEquals(hiddenResult.image.pixelUnsafe(59, 64), red)
    assertEquals(SurfaceDecorationPicking.pick(hidden, plan, 59.5, 64.5,
      ok(hiddenResult.scientific.pick(59, 64)).map(_.depth)), None)
    assert(ok(hiddenResult.pick(59, 64)).get.isInstanceOf[SurfaceDisplayHit.Scientific[?]])
    val overlay = blocker.copy(pickable = true, color = Rgba32.unsafe(255, 255, 255), depthPolicy = SurfaceDecorationDepthPolicy.Overlay)
    val top = ok(SurfaceDecorationRaster.render(plan, geometry.copy(meshes = Vector(overlay) ++ geometry.meshes), size))
    assertEquals(top.image.pixelUnsafe(69, 64), overlay.color)

  test("ordered strokes refuse translucent scientific policy and transparent base overrides"):
    val plan = composition()
    val view = plan.views.head
    val translucent = ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = view.instances.map(
      _.copy(opacity = ok(DisplayOpacity.make(.5)))))), plan.resources))
    assert(SurfaceDecorationRaster.render(translucent, orderedPair(translucent), size).isLeft)
    assert(SurfaceDecorationRaster.render(plan, orderedPair(plan), size,
      SurfaceRasterStyle(surfaceBase = Rgba32.unsafe(0, 0, 0, 0))).isLeft)

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

  test("default opaque legacy surface occludes a depth-tested marker but explicit overlay remains visible"):
    val plan = composition(occluder = true)
    val depth = ok(SurfaceDecorationRaster.render(plan, glyph(plan), size))
    val overlay = ok(SurfaceDecorationRaster.render(plan, glyph(plan, SurfaceDecorationDepthPolicy.Overlay), size))
    assertEquals(depth.image.pixelUnsafe(69, 64), blue)
    assertEquals(overlay.image.pixelUnsafe(69, 64), red)
    assert(ok(depth.pick(69, 64)).get.isInstanceOf[SurfaceDisplayHit.Scientific[?]])
    assert(ok(overlay.pick(69, 64)).get.isInstanceOf[SurfaceDisplayHit.Decoration[?]])

  test("collected-policy coplanar marker is visible and owns the display pick"):
    val base = composition()
    val view = base.views.head
    val plan = ok(SurfaceCompositionPlan.make(Vector(view.copy(
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)), base.resources.copy(
        layers = base.resources.layers.map((key, layer) => key -> layer.copy(sampleColors = Some(layer.colors))))))
    val original = glyph(plan).meshes.head
    val positions = new FloatBufferView(Array(
      -.25f, -.25f, 0f,
       .25f, -.25f, 0f,
       0f, .25f, 0f
    ))
    val normals = new FloatBufferView(Array.fill(9)(0f))
    val mesh = original.copy(positions = positions, normals = normals,
      indices = new IntBufferView(Array(0, 1, 2)), color = red)
    val geometry = glyph(plan).copy(meshes = Vector(mesh))
    val rendered = ok(SurfaceDecorationRaster.render(plan, geometry, size))
    assertEquals(rendered.image.pixelUnsafe(64, 64), red)
    assert(ok(rendered.pick(64, 64)).get.isInstanceOf[SurfaceDisplayHit.Decoration[?]])

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
