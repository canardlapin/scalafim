package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceOpaqueCompositionSuite extends munit.FunSuite:
  private def ok[A](result: Either[?, A]): A = result.fold(e => fail(s"$e"), identity)
  private val size = RasterDimensions.unsafe(65, 67)
  private val style = SurfaceRasterStyle(background = Rgba32.unsafe(17, 23, 31, 0), culling = TriangleCulling.None)
  private val opaqueStyle = style.copy(background = Rgba32.unsafe(17, 23, 31))
  private val identityMatrix = new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
  private val projection = new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1))
  private val camera = SurfaceCameraPacket(identityMatrix, projection, 0, 0, 1)

  private def equivalent(plan: SurfaceCompositionPlan, expectedCollection: Boolean = false,
      rasterStyle: SurfaceRasterStyle = opaqueStyle): SurfaceRasterResult =
    val actual = ok(SurfaceRasterizer.render(plan, size, rasterStyle))
    val reference = ok(SurfaceRasterizer.renderChecked(plan, size, rasterStyle,
      SurfaceRasterizer.DefaultFragmentCapacity, forceCollection = true))
    assertEquals(actual.collected.nonEmpty, expectedCollection)
    assertEquals(actual.image, reference.image)
    for y <- 0 until size.height; x <- 0 until size.width do
      assertEquals(actual.pick(x, y), reference.pick(x, y), s"pick $x,$y")
    actual

  private def equivalentDecorated(plan: SurfaceCompositionPlan, geometry: SurfaceDecorationGeometry,
      rasterStyle: SurfaceRasterStyle = opaqueStyle): SurfaceDecoratedRasterResult =
    val fast = ok(SurfaceDecorationRaster.render(plan, geometry, size, rasterStyle))
    val collected = ok(SurfaceDecorationRaster.renderChecked(plan, geometry, size, rasterStyle,
      SurfaceRasterizer.DefaultFragmentCapacity))
    assert(fast.scientific.collected.nonEmpty)
    assertEquals(fast.image, collected.image)
    for y <- 0 until size.height; x <- 0 until size.width do
      assertEquals(fast.pick(x, y), collected.pick(x, y), s"decorated pick $x,$y")
    fast

  private def markers(plan: SurfaceCompositionPlan, surface: SurfaceId): SurfaceDecorationGeometry =
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(0)))
    val values = Vector(
      SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("depth")), anchor, SurfaceMarkerShape.Disk,
        ok(SurfaceDecorationSize.pixels(5)), Rgba32.unsafe(255, 40, 20), SurfaceDecorationDepthPolicy.DepthTest),
      SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("overlay-a")), anchor, SurfaceMarkerShape.Disk,
        ok(SurfaceDecorationSize.pixels(3)), Rgba32.unsafe(20, 255, 40), SurfaceDecorationDepthPolicy.Overlay),
      SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("overlay-b")), anchor, SurfaceMarkerShape.Disk,
        ok(SurfaceDecorationSize.pixels(2)), Rgba32.unsafe(20, 40, 255), SurfaceDecorationDepthPolicy.Overlay)
    )
    ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(values, plan)), plan, size.width, size.height))

  private def copied(plan: SurfaceCompositionPlan, surface: SurfaceId,
      values: Vector[(String, Double, Rgba32, SurfaceDecorationDepthPolicy)]): SurfaceDecorationGeometry =
    val source = markers(plan, surface)
    val base = source.meshes.head
    source.copy(meshes = values.map: (name, z, color, policy) =>
      val positions = Array.tabulate(base.positions.length): index =>
        base.positions(index) + (if index % 3 == 2 then z.toFloat else 0f)
      val texture = SurfaceDecorationTexture(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(1, 1),
        Array(color.toPackedInt)), new FloatBufferView(Array.fill(base.positions.length / 3 * 2)(0f)))
      base.copy(id = SurfaceDecorationElementId(ok(SurfaceDecorationId.make(name)), "mesh"),
        positions = new FloatBufferView(positions), color = color, depthPolicy = policy,
        texture = Some(texture), pickable = false))

  private def composition(plans: Vector[SurfaceRenderPlan]): SurfaceCompositionPlan =
    val sources = plans.map(p => ok(SurfaceCompositionPlan.fromLegacy(p)))
    val instances = sources.flatMap(_.views.head.instances).map(i => i.copy(id = ok(SurfaceInstanceId.make(i.surface.value))))
    ok(SurfaceCompositionPlan.make(Vector(sources.head.views.head.copy(instances = instances)),
      SurfaceCompositionResources(sources.flatMap(_.resources.meshes).toMap, sources.flatMap(_.resources.layers).toMap)))

  private def layered(layers: Vector[SurfaceLayer]): SurfaceRenderPlan =
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(SurfaceFaceFixture.Surface,
      SurfaceFaceFixture.geometry))), layers))
    ok(SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model).copy(
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight))).copy(camera = camera, viewportFit = SurfaceViewportFit.Fill)

  private def planes(name: String, depths: Vector[Double]): SurfaceRenderPlan =
    val id = SurfaceId.unsafe(name)
    val vertices = depths.flatMap(z => Vector(Vector(-.9, -.9, z), Vector(.9, -.9, z), Vector(0.0, .9, z)))
    val faces = depths.indices.map(i => (i * 3, i * 3 + 1, i * 3 + 2))
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(vertices, faces), Hemisphere.Left, SurfaceKind.Pial)
    val colors = depths.indices.map(i => Rgba32.unsafe(40 + i * 60, 80, 170)).toArray
    val layer = SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe(s"$name-colors"), id,
      ok(SurfaceFaceField.make(geometry, colors)))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, geometry))), Vector(layer)))
    ok(SurfaceCompiler.compile(model, SurfaceViewerState.initial(model).copy(lighting = SurfaceLighting.Unlit,
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight))).copy(camera = camera, viewportFit = SurfaceViewportFit.Fill)

  test("opaque visibility fixtures retain exact collected colors and full scientific picks"):
    for name <- SurfaceVisibilityFixture.Cases do
      val plan = ok(SurfaceCompositionPlan.fromLegacy(SurfaceVisibilityFixture.plan(name)))
      val result = equivalent(plan)
      assert(result.receipt.shadedPixels > 0, name)

  test("packed duplicate-surface views retain ordered raster and pick identities"):
    val base = ok(SurfaceCompositionPlan.fromLegacy(planes("packed", Vector(0))))
    val source = base.views.head
    val first = source.copy(id = ok(SurfaceViewId.make("upper")), viewport = SurfaceViewport(0, 0, .5, 1),
      viewportFit = SurfaceViewportFit.Pack(1), instances = Vector(source.instances.head.copy(id = ok(SurfaceInstanceId.make("upper-instance")))))
    val second = source.copy(id = ok(SurfaceViewId.make("lower")), viewport = SurfaceViewport(.5, 0, .5, 1),
      viewportFit = SurfaceViewportFit.Pack(1), instances = Vector(source.instances.head.copy(id = ok(SurfaceInstanceId.make("lower-instance")))))
    val plan = ok(SurfaceCompositionPlan.make(Vector(first, second), base.resources))
    val dimensions = RasterDimensions.unsafe(80, 160)
    val raster = ok(SurfaceRasterizer.render(plan, dimensions, opaqueStyle))
    val byOrigin = (for
      y <- 0 until dimensions.height
      x <- 0 until dimensions.width
      pick <- ok(raster.pick(x, y)).toVector
      origin <- pick.origin
    yield origin -> y).groupMap(_._1)(_._2)
    assertEquals(byOrigin.keySet, Set(SurfaceDisplayOrigin(first.id, first.instances.head.id),
      SurfaceDisplayOrigin(second.id, second.instances.head.id)))
    assert(byOrigin(SurfaceDisplayOrigin(first.id, first.instances.head.id)).max <
      byOrigin(SurfaceDisplayOrigin(second.id, second.instances.head.id)).min)

  test("packed portrait and wide views keep pixel decorations sized and identified per view"):
    val base = ok(SurfaceCompositionPlan.fromLegacy(planes("decoration-pack", Vector(0))))
    val source = base.views.head
    val first = source.copy(id = ok(SurfaceViewId.make("first")), viewport = SurfaceViewport(0, 0, .2, 1),
      viewportFit = SurfaceViewportFit.Pack(1), instances = Vector(source.instances.head.copy(id = ok(SurfaceInstanceId.make("first-instance")))))
    val second = source.copy(id = ok(SurfaceViewId.make("second")), viewport = SurfaceViewport(.8, 0, .2, 1),
      viewportFit = SurfaceViewportFit.Pack(1), instances = Vector(source.instances.head.copy(id = ok(SurfaceInstanceId.make("second-instance")))))
    val plan = ok(SurfaceCompositionPlan.make(Vector(first, second), base.resources))
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(source.instances.head.surface, VertexId(0)))
    val marker = SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("packed-marker")), anchor, SurfaceMarkerShape.Disk,
      ok(SurfaceDecorationSize.pixels(10)), Rgba32.unsafe(255, 0, 0), SurfaceDecorationDepthPolicy.Overlay)
    val label = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("packed-label")), anchor, "L",
      ok(SurfaceDecorationSize.pixels(10)), Rgba32.unsafe(0, 255, 0)))
    val point = SurfaceDecorationWorldPoint(0, 0, 0)
    val compiled = ok(SurfaceDecorationCompiler.compile(Vector(marker, label), plan))
    val packets = compiled.copy(markers = compiled.markers.map(_.copy(point = point)), labels = compiled.labels.map(_.copy(point = point)))
    val rasterizer = new SurfaceLabelRasterizer:
      def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
        Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(1, 1), Array(color.toPackedInt)), 1, .5, .5))
    def check(width: Int, height: Int, centers: Map[SurfaceViewId, (Double, Double)]): Unit =
      val geometry = ok(SurfaceDecorationGeometry.lower(packets, plan, width, height))
      val labels = ok(SurfaceLabelLayout.resolve(geometry, plan, rasterizer))
      assertEquals(geometry.meshes.map(_.origin.view), Vector(first.id, second.id))
      geometry.meshes.zipWithIndex.foreach: (mesh, index) =>
        val origin = mesh.origin
        val projected = SurfaceDecorationPicking.project(plan, origin.view, point, width, height).get
        val expected = centers(origin.view)
        assertEqualsDouble(projected.x, expected._1, 1e-6)
        assertEqualsDouble(projected.y, expected._2, 1e-6)
        assertEquals(SurfaceDecorationPicking.pick(geometry, plan, expected._1, expected._2, None).map(_.origin), Some(origin))
        val labelMesh = labels.meshes.find(mesh => mesh.origin == origin && mesh.texture.nonEmpty).get
        val left = SurfaceDecorationPicking.project(plan, origin.view,
          SurfaceDecorationWorldPoint(labelMesh.positions(0), labelMesh.positions(1), labelMesh.positions(2)), width, height).get
        val right = SurfaceDecorationPicking.project(plan, origin.view,
          SurfaceDecorationWorldPoint(labelMesh.positions(3), labelMesh.positions(4), labelMesh.positions(5)), width, height).get
        assertEqualsDouble(math.abs(right.x - left.x), 10.0, 1e-6)
        assertEqualsDouble(math.abs(mesh.positions(3) - packets.markers(index).point.x), .05, 1e-6)
    check(400, 800, Map(first.id -> (200.0, 200.0), second.id -> (200.0, 600.0)))
    check(800, 400, Map(first.id -> (200.0, 200.0), second.id -> (600.0, 200.0)))

  test("opaque scientific winners feed decorated depth and overlay resolution exactly once per pixel"):
    val plan = composition(Vector(planes("opaque-a", Vector(0, 1e-14, -.15)), planes("opaque-z", Vector(0))))
    val fast = equivalentDecorated(plan, markers(plan, SurfaceId.unsafe("opaque-a")))
    assert(fast.scientific.collected.get.forall(_.length <= 1))
    val top = Rgba32.unsafe(20, 40, 255)
    val pixel = (for y <- 0 until size.height; x <- 0 until size.width
      if fast.image.pixelUnsafe(x, y) == top yield (x, y)).headOption.getOrElse(fail("overlay top color is absent"))
    ok(fast.pick(pixel._1, pixel._2)) match
      case Some(SurfaceDisplayHit.Decoration(hit)) => assertEquals(hit.id.decoration.value, "overlay-b")
      case other => fail(s"expected final overlay pick, got $other")

  test("partial-alpha depth decorations distinguish front equal and five-epsilon-behind depths"):
    val plan = composition(Vector(planes("depth", Vector(0))))
    val geometry = copied(plan, SurfaceId.unsafe("depth"), Vector(
      ("behind", -5e-15, Rgba32.unsafe(255, 0, 0, 128), SurfaceDecorationDepthPolicy.DepthTest),
      ("equal", 0.0, Rgba32.unsafe(0, 255, 0, 128), SurfaceDecorationDepthPolicy.DepthTest),
      ("front", 5e-15, Rgba32.unsafe(0, 0, 255, 128), SurfaceDecorationDepthPolicy.DepthTest)
    ))
    val rendered = equivalentDecorated(plan, geometry)
    assertNotEquals(rendered.image, rendered.scientific.image)
    assert((for y <- 0 until size.height; x <- 0 until size.width yield ok(rendered.pick(x, y))).forall(
      _.forall(_.isInstanceOf[SurfaceDisplayHit.Scientific[?]])))

  test("partial-alpha overlays use declared mesh order when their depths oppose"):
    val plan = composition(Vector(planes("overlay", Vector(0))))
    val geometry = copied(plan, SurfaceId.unsafe("overlay"), Vector(
      ("overlay-near", 5e-15, Rgba32.unsafe(255, 0, 0, 128), SurfaceDecorationDepthPolicy.Overlay),
      ("overlay-far-last", -5e-15, Rgba32.unsafe(0, 0, 255, 128), SurfaceDecorationDepthPolicy.Overlay)
    ))
    val rendered = equivalentDecorated(plan, geometry)
    val reversed = equivalentDecorated(plan, geometry.copy(meshes = geometry.meshes.reverse))
    assertNotEquals(rendered.image, reversed.image)

  test("transparent backgrounds and explicit capacities keep the full scientific collector"):
    val plan = composition(Vector(planes("transparent", Vector(0, -.1, -.2))))
    val geometry = markers(plan, SurfaceId.unsafe("transparent"))
    val transparent = ok(SurfaceDecorationRaster.render(plan, geometry, size, style))
    assert(transparent.scientific.collected.exists(_.exists(_.length > 1)))
    assertEquals(SurfaceDecorationRaster.renderChecked(plan, geometry, size, opaqueStyle, size.pixelCount),
      Left(SurfaceRasterError.FragmentCapacityExceeded(size.pixelCount)))

  test("texture alpha holes retain scientific winners while opaque texels use the same decorated resolve"):
    val plan = composition(Vector(planes("textured", Vector(0))))
    val source = markers(plan, SurfaceId.unsafe("textured"))
    val base = source.meshes.head
    val texels = Array(Rgba32.unsafe(255, 0, 0).toPackedInt, 0, 0, Rgba32.unsafe(255, 0, 0).toPackedInt)
    val vertices = base.positions.length / 3
    val uv = Array.tabulate(vertices * 2): index =>
      if index % 2 == 0 then ((index / 2) % 2).toFloat else ((index / 4) % 2).toFloat
    val textured = source.copy(meshes = Vector(base.copy(
      texture = Some(SurfaceDecorationTexture(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(2, 2), texels),
        new FloatBufferView(uv))), pickable = false)))
    val rendered = equivalentDecorated(plan, textured)
    assertNotEquals(rendered.image, rendered.scientific.image)
    val picks = for y <- 0 until size.height; x <- 0 until size.width yield ok(rendered.pick(x, y))
    assert(picks.exists(_.exists(_.isInstanceOf[SurfaceDisplayHit.Scientific[?]])))

  test("translucent science retains the full stack and reveals a behind decoration"):
    val plan = SurfaceVisibilityFixture.alphaComposition(.5, reverse = false)
    val geometry = copied(plan, SurfaceId.unsafe("back-blue"), Vector(
      ("behind-translucent", 0.0, Rgba32.unsafe(0, 255, 0, 128), SurfaceDecorationDepthPolicy.DepthTest)
    ))
    val rendered = equivalentDecorated(plan, geometry)
    assert(rendered.scientific.collected.exists(_.exists(_.length > 1)))
    assertNotEquals(rendered.image, rendered.scientific.image)

  test("offscreen decorations preserve resolved translucent pixels and scientific picks exactly"):
    val plan = SurfaceVisibilityFixture.alphaComposition(.5, reverse = false)
    val original = copied(plan, SurfaceId.unsafe("back-blue"), Vector(
      ("offscreen", 0.0, Rgba32.unsafe(0, 255, 0, 128), SurfaceDecorationDepthPolicy.DepthTest)))
    val offscreen = original.copy(meshes = original.meshes.map: mesh =>
      mesh.copy(positions = new FloatBufferView(Array.tabulate(mesh.positions.length): i =>
        mesh.positions(i) + (if i % 3 == 0 then 100f else 0f))))
    for background <- Vector(style, opaqueStyle) do
      val scientific = ok(SurfaceRasterizer.render(plan, size, background))
      val decorated = ok(SurfaceDecorationRaster.render(plan, offscreen, size, background))
      assertEquals(decorated.image, scientific.image)
      for y <- 0 until size.height; x <- 0 until size.width do
        assertEquals(decorated.pick(x, y), scientific.pick(x, y).map(_.map(SurfaceDisplayHit.Scientific.apply)))

  test("all blend modes, invalid scalar values and partial alpha preserve opaque visibility"):
    val geometry = SurfaceFaceFixture.geometry
    val id = SurfaceFaceFixture.Surface
    val under = ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("under"), id, geometry,
      Vector(Rgba32.unsafe(0, 255, 0, 0), Rgba32.unsafe(200, 80, 20, 128),
        Rgba32.unsafe(0, 40, 220), Rgba32.unsafe(240, 240, 240))))
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
      ScalarRamp.linear(Rgba32.unsafe(0, 30, 255, 80), Rgba32.unsafe(255, 40, 0, 190))),
      visibility = ScalarVisibility.Outside(ok(ScalarInterval.make(-.25, .25, ScalarEndpointInclusion.Both))),
      invalid = Rgba32.unsafe(255, 0, 255, 0))
    for mode <- Vector(DisplayBlendMode.Normal, DisplayBlendMode.Additive, DisplayBlendMode.Multiply,
        DisplayBlendMode.Screen, DisplayBlendMode.PlusLighter) do
      val scalar = ok(SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("scalar"), id, geometry,
        Array(Double.NaN, -.25, .25, 1.0), mapping, opacity = DisplayOpacity.unsafe(.6), blendMode = mode))
      equivalent(composition(Vector(layered(Vector(under, scalar)))))

  test("mixed nearest and face samples preserve partition boundaries and clipped perspective picks"):
    val model = SurfaceNearestFixture.model
    val mixed = ok(SurfaceViewerModel.make(model.surfaces, SurfaceFaceFixture.model.layers ++ model.layers))
    val state = SurfaceNearestFixture.state(mixed).copy(
      camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))),
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight,
      clipping = SurfaceClipping.WorldPlanes(Vector(WorldClipPlane.unsafe(1, 0, 0, .2))))
    equivalent(composition(Vector(ok(SurfaceCompiler.compile(mixed, state)))))

  test("coplanar instance winners are stable under reversed submission order"):
    val original = composition(Vector(planes("a", Vector(0)), planes("z", Vector(0))))
    val reversed = ok(SurfaceCompositionPlan.make(original.views.map(v => v.copy(instances = v.instances.reverse)), original.resources))
    val first = equivalent(original)
    val second = equivalent(reversed)
    assertEquals(first.image, second.image)
    assertEquals(ok(first.pick(32, 33)).get.surface.value, "z")
    assertEquals(ok(first.pick(32, 33)), ok(second.pick(32, 33)))

  test("same-instance depth tolerance keeps the first duplicate even when hidden by another instance"):
    for delta <- Vector(1e-14, 2e-14, 3e-14) do
      val same = planes("same", Vector(0, delta, 3 * delta))
      equivalent(composition(Vector(same)))
    val hidden = composition(Vector(planes("occluder", Vector(7.5e-15)), planes("same", Vector(0, 1.5e-14))))
    val result = equivalent(hidden)
    assertEquals(ok(result.pick(32, 33)).get.surface.value, "occluder")

  test("repeated instances in disjoint views retain view identity"):
    val source = composition(Vector(SurfaceVisibilityFixture.plan("threshold-edge-opacity")))
    val views = Vector(0, 1).map: i =>
      source.views.head.copy(id = ok(SurfaceViewId.make(s"view-$i")), viewport = SurfaceViewport(i * .5, 0, .5, 1),
        instances = source.views.head.instances.map(instance => instance.copy(id = ok(SurfaceInstanceId.make(s"copy-$i")))))
    equivalent(ok(SurfaceCompositionPlan.make(views, source.resources)))

  test("translucent instances and effective bases retain collection"):
    equivalent(SurfaceVisibilityFixture.alphaComposition(.5, false), expectedCollection = true)
    val source = composition(Vector(SurfaceVisibilityFixture.plan("gray")))
    equivalent(source, expectedCollection = true,
      rasterStyle = style.copy(surfaceBase = Rgba32.unsafe(20, 30, 40, 0)))

  test("viewport boundary ties retain the complete face and vertex order even with aliased identity strings"):
    val source = composition(Vector(layered(SurfaceFaceFixture.model.layers)))
    val original = source.views.head
    val views = Vector(
      original.copy(id = ok(SurfaceViewId.make("a")), viewport = SurfaceViewport(0, 0, .5, 1),
        instances = original.instances.map(_.copy(id = ok(SurfaceInstanceId.make("b/c"))))),
      original.copy(id = ok(SurfaceViewId.make("a/b")), viewport = SurfaceViewport(.5, 0, .5, 1),
        instances = original.instances.map(_.copy(id = ok(SurfaceInstanceId.make("c"))))))
    equivalent(ok(SurfaceCompositionPlan.make(views, source.resources)))
    equivalent(ok(SurfaceCompositionPlan.make(views.reverse, source.resources)))

  test("empty decoration plans preserve collected legacy coplanar ties"):
    val source = composition(Vector(planes("a", Vector(0)), planes("z", Vector(0))))
    val legacy = ok(SurfaceCompositionPlan.make(source.views.map(_.copy(colorPipeline = SurfaceColorPipeline.LegacyDisplayBytes)), source.resources))
    val geometry = ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector.empty, legacy)),
      legacy, size.width, size.height))
    val rendered = ok(SurfaceDecorationRaster.render(legacy, geometry, size, style.copy(background = Rgba32.unsafe(255, 255, 255))))
    val reference = ok(SurfaceRasterizer.renderChecked(legacy, size, style.copy(background = Rgba32.unsafe(255, 255, 255)),
      SurfaceRasterizer.DefaultFragmentCapacity, forceCollection = true))
    assertEquals(rendered.image, reference.image)
    assertEquals(rendered.scientific.pick(32, 33), reference.pick(32, 33))

  test("explicit capacity and forced decoration collection retain their checked behavior"):
    val source = composition(Vector(planes("capacity", Vector(0, -.1, -.2))))
    assertEquals(SurfaceRasterizer.renderChecked(source, size, style, size.pixelCount),
      Left(SurfaceRasterError.FragmentCapacityExceeded(size.pixelCount)))
    val geometry = ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector.empty, source)),
      source, size.width, size.height))
    val empty = ok(SurfaceDecorationRaster.render(source, geometry, size, style))
    assert(empty.scientific.collected.nonEmpty)
    assertEquals(empty.image, equivalent(source, expectedCollection = true, rasterStyle = style).image)
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(source.views.head.instances.head.surface, VertexId(0)))
    val marker = SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("marker")), anchor,
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(3)), Rgba32.unsafe(255, 0, 0))
    val decorated = ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector(marker), source)),
      source, size.width, size.height))
    assert(ok(SurfaceDecorationRaster.render(source, decorated, size, style)).scientific.collected.nonEmpty)
