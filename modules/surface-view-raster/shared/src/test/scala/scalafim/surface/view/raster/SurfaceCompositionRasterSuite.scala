package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceCompositionRasterSuite extends munit.FunSuite:
  private val red = Rgba32.unsafe(255, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 255)
  private val size = RasterDimensions.unsafe(64, 64)

  private def plan(name: String, slope: Double, color: Rgba32): SurfaceRenderPlan =
    val id = SurfaceId.unsafe(name)
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, -slope), Seq(1.0, -1.0, slope),
        Seq(-1.0, 1.0, -slope), Seq(1.0, 1.0, slope)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"$name-color"), id, geometry,
      Vector.fill(4)(color)).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, geometry).toOption.get), Vector(layer)).toOption.get
    val state = SurfaceViewerState.initial(model).copy(
      camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal,
        CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))),
      lighting = SurfaceLighting.Unlit)
    SurfaceCompiler.compile(model, state).toOption.get

  private def instance(id: String, plan: SurfaceRenderPlan,
    opacity: DisplayOpacity = DisplayOpacity.Opaque): SurfaceCompositionInstance =
    SurfaceCompositionInstance(SurfaceInstanceId.make(id).toOption.get, plan.meshes.head.surface,
      plan.meshes.head.resourceKey, plan.layers.map(_.resourceKey), opacity = opacity)

  private def resources(plans: SurfaceRenderPlan*): SurfaceCompositionResources =
    SurfaceCompositionResources(plans.flatMap(_.meshes).map(m => m.resourceKey -> m).toMap,
      plans.flatMap(_.layers).map(l => l.resourceKey -> l).toMap)

  private def view(id: String, box: SurfaceViewport, camera: SurfaceCameraPacket,
    instances: Vector[SurfaceCompositionInstance]): SurfaceCompositionView =
    SurfaceCompositionView(SurfaceViewId.make(id).toOption.get, box, camera, instances,
      SurfaceLighting.Unlit, SurfaceClipping.Disabled)

  test("two crossing surfaces share depth and identify the original instance independent of submission order"):
    val a = plan("red", .4, red)
    val b = plan("blue", -.4, blue)
    val instances = Vector(instance("red-instance", a), instance("blue-instance", b))
    def render(order: Vector[SurfaceCompositionInstance]): SurfaceRasterResult =
      val scene = SurfaceCompositionPlan.make(Vector(view("both", SurfaceViewport(0, 0, 1, 1), a.camera, order)), resources(a, b)).toOption.get
      SurfaceRasterizer.render(scene, size).toOption.get
    val first = render(instances)
    val reversed = render(instances.reverse)
    assertEquals(first.image, reversed.image)
    assertEquals(first.image.pixelUnsafe(16, 32), blue)
    assertEquals(first.image.pixelUnsafe(48, 32), red)
    val left = first.pick(16, 32).toOption.flatten.get
    val right = first.pick(48, 32).toOption.flatten.get
    assertEquals(left.surface, b.meshes.head.surface)
    assertEquals(right.surface, a.meshes.head.surface)
    assertEquals(left.origin.get.instance.value, "blue-instance")
    assertEquals(right.origin.get.instance.value, "red-instance")
    assertEquals(left.origin.get.view.value, "both")
    assertEqualsDouble(left.barycentricA + left.barycentricB + left.barycentricC, 1.0, 1e-6)

  test("translucent surfaces resolve far to near independent of submission order and retain transparent backgrounds"):
    val near = plan("near-red", 0, red)
    val far = plan("far-blue", 0, blue)
    val alpha = DisplayOpacity.unsafe(.5)
    val entries = Vector(instance("near", near, alpha), instance("far", far, alpha))
    def render(order: Vector[SurfaceCompositionInstance]): SurfaceRasterResult =
      val scene = SurfaceCompositionPlan.make(Vector(view("transparent", SurfaceViewport(0, 0, 1, 1), near.camera, order)),
        resources(near, far)).toOption.get
      SurfaceRasterizer.render(scene, size, SurfaceRasterStyle(background = Rgba32.unsafe(0, 0, 0, 0))).toOption.get
    val first = render(entries)
    val reversed = render(entries.reverse)
    assertEquals(first.image, reversed.image)
    val center = first.image.pixelUnsafe(32, 32)
    assertEquals(center.alpha, 191)
    assert(center.red > center.blue, s"expected nearer red to dominate: $center")
    assertEquals(first.pick(32, 32).toOption.flatten.map(_.origin.map(_.instance.value)), Some(Some("near")))

  test("transparent crossing triangles retain distinct-depth ordering in either submission order"):
    val redPlane = plan("transparent-red", .4, red)
    val bluePlane = plan("transparent-blue", -.4, blue)
    val alpha = DisplayOpacity.unsafe(.5)
    val entries = Vector(instance("red", redPlane, alpha), instance("blue", bluePlane, alpha))
    def render(order: Vector[SurfaceCompositionInstance]): SurfaceRasterResult =
      val scene = SurfaceCompositionPlan.make(Vector(view("crossing", SurfaceViewport(0, 0, 1, 1), redPlane.camera, order)),
        resources(redPlane, bluePlane)).toOption.get
      SurfaceRasterizer.render(scene, size, SurfaceRasterStyle(background = Rgba32.unsafe(0, 0, 0, 0))).toOption.get
    val first = render(entries)
    val reversed = render(entries.reverse)
    assertEquals(first.image, reversed.image)
    assert(first.image.pixelUnsafe(16, 32).blue > first.image.pixelUnsafe(16, 32).red)
    assert(first.image.pixelUnsafe(48, 32).red > first.image.pixelUnsafe(48, 32).blue)

  test("checked composition refuses fragment collection above its explicit capacity"):
    val source = plan("capacity", 0, red)
    val scene = SurfaceCompositionPlan.make(Vector(view("view", SurfaceViewport(0, 0, 1, 1), source.camera,
      Vector(instance("one", source)))), resources(source)).toOption.get
    assertEquals(SurfaceRasterizer.renderChecked(scene, size, SurfaceRasterStyle(), 1),
      Left(SurfaceRasterError.FragmentCapacityExceeded(1)))

  test("positive instance opacity remains a scientific contribution before final byte packing"):
    val source = plan("tiny-opacity", 0, red)
    val tiny = DisplayOpacity.unsafe(1e-8)
    val scene = SurfaceCompositionPlan.make(Vector(view("view", SurfaceViewport(0, 0, 1, 1), source.camera,
      Vector(instance("tiny", source, tiny)))), resources(source)).toOption.get
    val rendered = SurfaceRasterizer.render(scene, size,
      SurfaceRasterStyle(background = Rgba32.unsafe(0, 0, 0, 0))).toOption.get
    assertEquals(rendered.image.pixelUnsafe(32, 32).alpha, 0)
    assert(rendered.pick(32, 32).toOption.flatten.nonEmpty)

  test("world clipping uses translated surface coordinates"):
    val source = plan("translated-clip", 0, red)
    val translated = instance("translated", source).copy(translationX = .5)
    val clipping = SurfaceClipping.WorldPlanes(Vector(WorldClipPlane.unsafe(1, 0, 0, 0)))
    val scene = SurfaceCompositionPlan.make(Vector(SurfaceCompositionView(
      SurfaceViewId.make("clip").toOption.get, SurfaceViewport(0, 0, 1, 1), source.camera,
      Vector(translated), SurfaceLighting.Unlit, clipping)), resources(source)).toOption.get
    val rendered = SurfaceRasterizer.render(scene, size).toOption.get
    // Pixel centers x=30.5 and x=36.5 project to world x=-0.0586 and
    // x=+0.1758. Both have negative pre-translation local x; only the
    // second survives a plane declared in displayed world coordinates.
    assert(rendered.pick(30, 32).toOption.flatten.isEmpty)
    assert(rendered.pick(36, 32).toOption.flatten.nonEmpty)

  test("four views reuse one mesh and preserve distinct view and instance picks"):
    val source = plan("single", 0, red)
    val views = (0 until 4).map: index =>
      view(s"view-$index", SurfaceViewport((index % 2) * .5, (index / 2) * .5, .5, .5),
        source.camera, Vector(instance(s"instance-$index", source)))
    .toVector
    val scene = SurfaceCompositionPlan.make(views, resources(source)).toOption.get
    assertEquals(scene.resources.meshes.size, 1)
    val result = SurfaceRasterizer.render(scene, size).toOption.get
    for index <- 0 until 4 do
      val hit = result.pick((index % 2) * 32 + 16, (index / 2) * 32 + 16).toOption.flatten.get
      assertEquals(hit.surface, source.meshes.head.surface)
      assertEquals(hit.origin.get.view.value, s"view-$index")
      assertEquals(hit.origin.get.instance.value, s"instance-$index")

  test("changing one camera leaves the other panel's pixels unchanged"):
    val source = plan("single", 0, red)
    val left = view("left", SurfaceViewport(0, 0, .5, 1), source.camera, Vector(instance("left-copy", source)))
    val right = view("right", SurfaceViewport(.5, 0, .5, 1), source.camera, Vector(instance("right-copy", source)))
    def render(rightView: SurfaceCompositionView): SurfaceRasterResult =
      SurfaceRasterizer.render(SurfaceCompositionPlan.make(Vector(left, rightView), resources(source)).toOption.get, size).toOption.get
    val original = render(right)
    val matrix = Array.tabulate(16)(source.camera.viewMatrix(_))
    matrix(3) += .75f
    val changed = render(right.copy(camera = source.camera.copy(viewMatrix = new FloatBufferView(matrix))))
    var rightChanges = 0
    for y <- 0 until 64; x <- 0 until 64 do
      if x < 32 then assertEquals(changed.image.pixelUnsafe(x, y), original.image.pixelUnsafe(x, y))
      else if changed.image.pixelUnsafe(x, y) != original.image.pixelUnsafe(x, y) then rightChanges += 1
    assert(rightChanges > 0)

  test("legacy lowering preserves contained viewport pixels and scientific picks on a wide canvas"):
    val source = plan("legacy", .4, red)
    val wide = RasterDimensions.unsafe(128, 64)
    val legacy = SurfaceRasterizer.render(source, wide).toOption.get
    val lowered = SurfaceRasterizer.render(SurfaceCompositionPlan.fromLegacy(source).toOption.get, wide).toOption.get
    assertEquals(lowered.image, legacy.image)
    val oldPick = legacy.pick(64, 32).toOption.flatten.get
    val newPick = lowered.pick(64, 32).toOption.flatten.get
    assertEquals(newPick.copy(origin = None), oldPick)
    assert(newPick.origin.nonEmpty)
