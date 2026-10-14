package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Native captures and actual picked identities; pixel assertions live in the
  * Node runner, independently of the geometry lowering implementation. */
object ThreeDecorationBrowserProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private val red = Rgba32.unsafe(255, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 255)
  private val id = SurfaceId.unsafe("anchor")
  private val anchor = SurfaceDecorationAnchor.Face(ok(SurfaceFaceAnchor.make(id, FaceId(0), 0, .5, .5)))

  private def source(surface: SurfaceId, x0: Double, z: Double): SurfaceRenderPlan =
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(x0, -1.0, z), Seq(1.0, -1.0, z), Seq(x0, 1.0, z), Seq(1.0, 1.0, z)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val layer = ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(surface.value + "-color"), surface, geometry, Vector.fill(4)(blue)))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(surface, geometry))), Vector(layer)))
    val state = SurfaceViewerState.initial(model).copy(camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal,
      CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))), lighting = SurfaceLighting.Unlit)
    ok(SurfaceCompiler.compile(model, state))


  private def networkGeometry(plan: SurfaceCompositionPlan, width: Int, height: Int,
      shape: SurfaceNetworkShape): SurfaceDecorationGeometry =
    val scientific = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-1.0, 1.0, 0.0), Seq(1.0, 1.0, 0.0)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val nodes = Vector.tabulate(4)(i => ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"node-$i"),
      id, VertexId(i), scientific)))
    val network = ok(SurfaceNetwork.make(nodes, Vector(
      SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(3).id, .5),
      SurfaceNetworkEdge.unsafe(nodes(2).id, nodes(1).id, -1))))
    val style = shape match
      case SurfaceNetworkShape.Line => SurfaceNetworkStyle.line(SurfaceNetworkRadius.unsafe(.035))
      case SurfaceNetworkShape.Tube => ok(SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(.035), 12))
    val display = ok(SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All, style,
      SurfaceNetworkPalette(red, Rgba32.unsafe(255, 255, 0))))
    val geometries = plan.views.map: view =>
      ok(SurfaceNetworkDecorations.lower(display, plan,
        SurfaceNetworkDecorationMapping(view.id, Map(id -> view.instances.find(_.surface == id).get.id)),
        SurfaceMarkerShape.Sphere, ok(SurfaceDecorationSize.world(.12)), width, height,
        Some(ok(SurfaceNetworkDecorationRadius.byAbsoluteWeight(SurfaceNetworkRadius.unsafe(.07),
          SurfaceNetworkRadius.unsafe(.01), SurfaceNetworkRadius.unsafe(.1)))), red))
    geometries.head.copy(meshes = geometries.flatMap(_.meshes))

  @JSExportTopLevel("runScalafimThreeDecorationProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val size = ThreeCanvasSize.unsafe(500, 250)
    val base = source(id, -1, 0)
    val cover = source(SurfaceId.unsafe("cover"), 0, .4)
    val resources = SurfaceCompositionResources((base.meshes ++ cover.meshes).map(m => m.resourceKey -> m).toMap,
      (base.layers ++ cover.layers).map(l => l.resourceKey -> l).toMap)
    val views = Vector.tabulate(2): index =>
      def instance(name: String, p: SurfaceRenderPlan) = SurfaceCompositionInstance(ok(SurfaceInstanceId.make(s"$name-$index")),
        p.meshes.head.surface, p.meshes.head.resourceKey, p.layers.map(_.resourceKey))
      SurfaceCompositionView(ok(SurfaceViewId.make(s"view-$index")), SurfaceViewport(index * .5, 0, .5, 1), base.camera,
        Vector(instance("anchor", base), instance("cover", cover)), SurfaceLighting.Unlit, SurfaceClipping.Disabled)
    val plan = ok(SurfaceCompositionPlan.make(views, resources))
    val runtime = ok(ThreeJsRuntime.create(three, canvas))
    val backend = ok(ThreeSurfaceBackend.create(runtime))
    def marker(name: String, shape: SurfaceMarkerShape, measure: SurfaceDecorationSize, policy: SurfaceDecorationDepthPolicy,
        color: Rgba32 = red): SurfaceDecoration =
      SurfaceMarkerDecoration(ok(SurfaceDecorationId.make(name)), anchor, shape, measure, color, policy)
    val text = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("label")), anchor, "ROI", ok(SurfaceDecorationSize.pixels(28)), red))
    val rasterizer = new CanvasSurfaceLabelRasterizer("Arial")
    def geometry(decorations: Vector[SurfaceDecoration], target: SurfaceCompositionPlan) =
      ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(decorations, target)),
        target, size.width, size.height)), target, rasterizer))
    val captures = new js.Array[js.Any]()
    try
      val initial = ok(backend.render(plan, size))
      val cases = Vector(
        "disk" -> Vector(marker("disk", SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(18)), SurfaceDecorationDepthPolicy.DepthTest)),
        "sphere" -> Vector(marker("sphere", SurfaceMarkerShape.Sphere, ok(SurfaceDecorationSize.world(.25)), SurfaceDecorationDepthPolicy.DepthTest)),
        "crosshair" -> Vector(marker("crosshair", SurfaceMarkerShape.Crosshair, ok(SurfaceDecorationSize.pixels(18)), SurfaceDecorationDepthPolicy.Overlay)),
        "label" -> Vector(text),
        "gray" -> Vector(marker("gray", SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(18)), SurfaceDecorationDepthPolicy.Overlay, Rgba32.unsafe(128, 128, 128))),
        "overlay-text" -> Vector(text, marker("overlay", SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(22)), SurfaceDecorationDepthPolicy.Overlay, Rgba32.unsafe(0, 255, 0))))
      val allCases = cases ++ Vector("network-line" -> Vector.empty, "network-tube" -> Vector.empty)
      allCases.foreach: (name, decos) =>
        val lowered = if name.startsWith("network-") then
          networkGeometry(plan, size.width, size.height, if name == "network-line" then SurfaceNetworkShape.Line else SurfaceNetworkShape.Tube)
        else geometry(decos, plan)
        val receipt = ok(backend.renderDecorated(plan, lowered, size))
        require(receipt.geometryUploads == 0 && receipt.geometryUpdates == 0 && receipt.colorUploads == 0, "decorations reuploaded scientific resources")
        val picks = new js.Array[js.Any]()
        for y <- 20 until 230 by 3; x <- (20 until 230 by 3) ++ (270 until 480 by 3) do
          val hit = ok(backend.pickDisplay(x + .5, y + .5))
          val value = hit match
            case Some(SurfaceDisplayHit.Decoration(deco)) => js.Dynamic.literal(kind = "decoration", id = deco.id.decoration.value,
              view = deco.origin.view.value, instance = deco.origin.instance.value)
            case Some(SurfaceDisplayHit.Scientific(surface)) => js.Dynamic.literal(kind = "scientific", id = surface.surface.value,
              view = surface.origin.map(_.view.value).getOrElse(""), instance = surface.origin.map(_.instance.value).getOrElse(""))
            case None => js.Dynamic.literal(kind = "none")
          picks.push(js.Dynamic.literal(x = x, y = y, hit = value))
        captures.push(js.Dynamic.literal(name = name, png = canvas.applyDynamic("toDataURL")("image/png"), picks = picks))
      val alphaLabel = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("alpha-oracle")), anchor, "oracle", ok(SurfaceDecorationSize.pixels(20)), red))
      val alphaProvider = new SurfaceLabelRasterizer:
        def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
          val pixels = Array.tabulate(12): i =>
            if i % 6 < 2 then 0
            else if i % 6 < 4 then Rgba32.unsafe(255, 0, 0, 128).toPackedInt
            else Rgba32.unsafe(0, 255, 0).toPackedInt
          Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(6, 2), pixels), 1, 3, 1))
      val alphaGeometry = ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(
        ok(SurfaceDecorationCompiler.compile(Vector(alphaLabel), plan)), plan, size.width, size.height)), plan, alphaProvider))
      ok(backend.renderDecorated(plan, alphaGeometry, size))
      captures.push(js.Dynamic.literal(name = "alpha-oracle", png = canvas.applyDynamic("toDataURL")("image/png"), picks = js.Array()))
      val decos = cases.head._2
      val firstGeometry = geometry(decos, plan)
      ok(backend.renderDecorated(plan, firstGeometry, size))
      val before = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val m = Array.tabulate(16)(views.head.camera.viewMatrix(_))
      m(3) += .3f
      val changed = ok(SurfaceCompositionPlan.make(views.updated(0, views.head.copy(camera = views.head.camera.copy(viewMatrix = new FloatBufferView(m)))), resources))
      val staleRejected = backend.renderDecorated(changed, firstGeometry, size).isLeft
      val moved = ok(backend.renderDecorated(changed, geometry(decos, changed), size))
      val after = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      ok(backend.render(changed, size, forceDraw = true))
      val plain = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      js.Dynamic.literal(captures = captures, initialGeometryUploads = initial.geometryUploads,
        staleRejected = staleRejected, cameraGeometryUploads = moved.geometryUploads, cameraGeometryUpdates = moved.geometryUpdates,
        cameraColorUploads = moved.colorUploads, before = before, after = after, plain = plain)
    finally backend.dispose()
