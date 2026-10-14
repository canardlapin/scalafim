package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import scalafim.surface.view.*

/** Browser-only composition proof. It reports raw frame captures and semantic
  * pick receipts; the Node runner owns pixel-region assertions.
  */
object ThreeCompositionBrowserProbe:
  @JSExportTopLevel("runScalafimThreeCompositionProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val size = ThreeCanvasSize.unsafe(256, 256)
    val base = SurfaceFaceFixture.plan
    val alternate =
      val model = SurfaceFaceFixture.model
      val state = SurfaceViewer.reduce(model, SurfaceFaceFixture.state(model),
        SurfaceViewerAction.OrbitBy(30.0, 15.0)).toOption.get
      SurfaceCompiler.compile(model, state).toOption.get
    val composition = panelComposition(base, alternate.camera)
    val runtime = ThreeJsRuntime.create(three, canvas).toOption.get
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    try
      val reuse = backend.render(reuseComposition(base), size).toOption.get
      val initial = backend.render(composition, size).toOption.get
      val panelOrigins = observedPanelOrigins(backend, composition, size)
      val before = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val changed = composition.views.updated(0, composition.views.head.copy(camera = alternate.camera))
      val cameraOnly = SurfaceCompositionPlan.make(changed, composition.resources).toOption.get
      val redraw = backend.render(cameraOnly, size).toOption.get
      val after = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val repeated = depthComposition(base, reverse = false)
      backend.render(repeated, size).toOption.get
      val repeatedPick = backend.pick(160.0, 150.0).toOption.flatten
      val reversed = depthComposition(base, reverse = true)
      backend.render(reversed, size).toOption.get
      val reversedPick = backend.pick(160.0, 150.0).toOption.flatten
      val beforeFailure = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val failure = three.selectDynamic("__compositionFailure")
      failure.updateDynamic("remaining")(1)
      val failureRejected = backend.render(repeated, size, forceDraw = true).isLeft
      failure.updateDynamic("remaining")(null)
      require(failureRejected, "injected native construction failure must be typed")
      backend.render(reversed, size, forceDraw = true).toOption.get
      val failureRecoverySamePixels = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String] == beforeFailure
      val nearestPlan = SurfaceCompositionPlan.fromLegacy(SurfaceNearestFixture.plan).toOption.get
      backend.render(nearestPlan, size).toOption.get
      val nearestPicks = nearestFaceOnePicks(backend)
      js.Dynamic.literal(
        status = "pass",
        failureRejected = failureRejected,
        failureRecoverySamePixels = failureRecoverySamePixels,
        reuseGeometryUploads = reuse.geometryUploads,
        initialGeometryUploads = initial.geometryUploads,
        initialColorUploads = initial.colorUploads,
        cameraGeometryUploads = redraw.geometryUploads,
        cameraGeometryUpdates = redraw.geometryUpdates,
        cameraColorUploads = redraw.colorUploads,
        before = before,
        after = after,
        repeatedPick = pickLiteral(repeatedPick),
        reversedPick = pickLiteral(reversedPick),
        nearestPicks = nearestPicks,
        panelOrigins = panelOrigins,
        viewCount = composition.views.length,
        instanceCount = repeated.receipt.instanceCount
      )
    finally backend.dispose()

  private def reuseComposition(base: SurfaceRenderPlan): SurfaceCompositionPlan =
    val lowered = SurfaceCompositionPlan.fromLegacy(base).toOption.get
    val template = lowered.views.head
    val boxes = Vector(
      SurfaceViewport(0.0, 0.0, 0.5, 0.5), SurfaceViewport(0.5, 0.0, 0.5, 0.5),
      SurfaceViewport(0.0, 0.5, 0.5, 0.5), SurfaceViewport(0.5, 0.5, 0.5, 0.5)
    )
    val views = boxes.zipWithIndex.map: (box, index) =>
      val instance = template.instances.head.copy(id = SurfaceInstanceId.make(s"composition-$index").toOption.get)
      template.copy(id = SurfaceViewId.make(s"composition-view-$index").toOption.get, viewport = box,
        instances = Vector(instance))
    SurfaceCompositionPlan.make(views, lowered.resources).toOption.get

  private def panelComposition(base: SurfaceRenderPlan, alternate: SurfaceCameraPacket): SurfaceCompositionPlan =
    val (redMesh, redLayer, blueMesh, blueLayer) = coloredTriangles(base)
    val resources = SurfaceCompositionResources(Map(redMesh.resourceKey -> redMesh, blueMesh.resourceKey -> blueMesh),
      Map(redLayer.resourceKey -> redLayer, blueLayer.resourceKey -> blueLayer))
    def item(id: String, mesh: SurfaceMeshPacket, layer: SurfaceLayerPacket, x: Double = 0.0, z: Double = 0.0) =
      SurfaceCompositionInstance(SurfaceInstanceId.make(id).toOption.get, mesh.surface, mesh.resourceKey, Vector(layer.resourceKey), x, 0.0, z)
    val boxes = Vector(SurfaceViewport(0, 0, .5, .5), SurfaceViewport(.5, 0, .5, .5),
      SurfaceViewport(0, .5, .5, .5), SurfaceViewport(.5, .5, .5, .5))
    val camera = base.camera
    val views = Vector(
      SurfaceCompositionView(SurfaceViewId.make("red").toOption.get, boxes(0), camera, Vector(item("red-only", redMesh, redLayer)), base.lighting, base.clipping),
      SurfaceCompositionView(SurfaceViewId.make("blue").toOption.get, boxes(1), camera, Vector(item("blue-only", blueMesh, blueLayer)), base.lighting, base.clipping),
      SurfaceCompositionView(SurfaceViewId.make("split").toOption.get, boxes(2), camera, Vector(item("split-red", redMesh, redLayer, -.3), item("split-blue", blueMesh, blueLayer, .3)), base.lighting, base.clipping),
      SurfaceCompositionView(SurfaceViewId.make("camera").toOption.get, boxes(3), alternate, Vector(item("camera-red", redMesh, redLayer)), base.lighting, base.clipping)
    )
    SurfaceCompositionPlan.make(views, resources).toOption.get

  private def depthComposition(base: SurfaceRenderPlan, reverse: Boolean): SurfaceCompositionPlan =
    val (redMesh, redLayer, blueMesh, blueLayer) = coloredTriangles(base)
    val resources = SurfaceCompositionResources(Map(redMesh.resourceKey -> redMesh, blueMesh.resourceKey -> blueMesh),
      Map(redLayer.resourceKey -> redLayer, blueLayer.resourceKey -> blueLayer))
    def item(id: String, mesh: SurfaceMeshPacket, layer: SurfaceLayerPacket, z: Double) =
      SurfaceCompositionInstance(SurfaceInstanceId.make(id).toOption.get, mesh.surface, mesh.resourceKey, Vector(layer.resourceKey), 0.0, 0.0, z)
    val back = item("back-red", redMesh, redLayer, 0.0)
    val front = item("front-blue", blueMesh, blueLayer, 0.25)
    val instances = if reverse then Vector(front, back) else Vector(back, front)
    val view = SurfaceCompositionView(SurfaceViewId.make("depth").toOption.get, SurfaceViewport(0, 0, 1, 1),
      base.camera, instances, base.lighting, base.clipping)
    SurfaceCompositionPlan.make(Vector(view), resources).toOption.get

  private def coloredTriangles(base: SurfaceRenderPlan): (SurfaceMeshPacket, SurfaceLayerPacket, SurfaceMeshPacket, SurfaceLayerPacket) =
    val mesh = base.meshes.head
    val layer = base.layers.head
    def copyFace(surface: SurfaceId, meshKey: String, layerKey: String, face: Int): (SurfaceMeshPacket, SurfaceLayerPacket) =
      val positions = new Array[Float](9)
      val normals = new Array[Float](9)
      val colors = new Array[Int](3)
      var corner = 0
      while corner < 3 do
        val source = mesh.indices(face * 3 + corner)
        var axis = 0
        while axis < 3 do
          positions(corner * 3 + axis) = mesh.positions(source * 3 + axis)
          normals(corner * 3 + axis) = mesh.normals(source * 3 + axis)
          axis += 1
        colors(corner) = layer.colors(source)
        corner += 1
      val copied = SurfaceMeshPacket(surface, SurfaceResourceKey(meshKey), new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(Array(0, 1, 2)))
      copied -> layer.copy(surface = surface, resourceKey = SurfaceResourceKey(layerKey), colors = new IntBufferView(colors))
    val red = copyFace(SurfaceId.unsafe("proof-red"), "proof-red-mesh", "proof-red-layer", 0)
    // Same scientific triangle with a blue face colour creates a strict depth oracle.
    val blue = copyFace(SurfaceId.unsafe("proof-blue"), "proof-blue-mesh", "proof-blue-layer", 1)
    val blueMesh = red._1.copy(surface = SurfaceId.unsafe("proof-blue"), resourceKey = SurfaceResourceKey("proof-blue-mesh"))
    (red._1, red._2, blueMesh, blue._2)

  private def nearestFaceOnePicks(backend: ThreeSurfaceBackend): js.Array[js.Any] =
    val values = new js.Array[js.Any]()
    val needed = Set(0, 2, 3)
    var found = Set.empty[Int]
    var y = 50
    while y < 205 && found != needed do
      var x = 50
      while x < 205 && found != needed do
        val hit = backend.pick(x + 0.5, y + 0.5).toOption.flatten
        hit match
          case Some(pick) if pick.face == 1 && needed(pick.vertex) && !found(pick.vertex) =>
            val (a, b, c) = pick.barycentric.get
            // Face one has source corners (0, 2, 3), ordered as in
            // SurfaceNearestFixture. These weights are from its source-space
            // coordinates, not the lowered display partition.
            val expected = ((1.0 - pick.worldY) / 2.0, (pick.worldX + 1.0) / 2.0, (pick.worldY - pick.worldX) / 2.0)
            require(math.abs(a - expected._1) < 1e-6 && math.abs(b - expected._2) < 1e-6 && math.abs(c - expected._3) < 1e-6)
            values.push(js.Dynamic.literal(face = pick.face, vertex = pick.vertex, barycentric = js.Array(a, b, c), expected = js.Array(expected._1, expected._2, expected._3)))
            found += pick.vertex
          case _ => ()
        x += 8
      y += 8
    require(found == needed, s"missing original face-one vertices: $found")
    values

  /** Pick the mounted composition at a deterministic grid in every viewport.
    * The result intentionally comes from Three's native raycaster rather than
    * reconstructing instance ownership from the composition input.
    */
  private def observedPanelOrigins(backend: ThreeSurfaceBackend, composition: SurfaceCompositionPlan,
      size: ThreeCanvasSize): js.Array[js.Any] =
    val observed = new js.Array[js.Any]()
    var viewIndex = 0
    while viewIndex < composition.views.length do
      val view = composition.views(viewIndex)
      val instances = scala.collection.mutable.LinkedHashSet.empty[String]
      val views = scala.collection.mutable.LinkedHashSet.empty[String]
      val left = (view.viewport.x * size.width).toInt
      val top = (view.viewport.y * size.height).toInt
      val right = ((view.viewport.x + view.viewport.width) * size.width).toInt
      val bottom = ((view.viewport.y + view.viewport.height) * size.height).toInt
      var y = top + 8
      while y < bottom - 8 do
        var x = left + 8
        while x < right - 8 do
          backend.pick(x + 0.5, y + 0.5).toOption.flatten match
            case Some(hit) =>
              hit.origin match
                case Some(origin) =>
                  views += origin.view.value
                  instances += origin.instance.value
                case None => ()
            case None => ()
          x += 8
        y += 8
      observed.push(js.Dynamic.literal(view = view.id.value,
        pickViews = js.Array(views.toVector*), instances = js.Array(instances.toVector*)))
      viewIndex += 1
    observed

  private def pickLiteral(pick: Option[ThreePick]): js.Dynamic = pick match
    case None => js.Dynamic.literal(found = false)
    case Some(value) =>
      val bary: js.Any = value.barycentric match
        case Some((a, b, c)) => js.Array(a, b, c)
        case None => null
      val origin: js.Any = value.origin match
        case Some(display) => js.Dynamic.literal(view = display.view.value, instance = display.instance.value)
        case None => null
      js.Dynamic.literal(found = true, surface = value.surface.value, face = value.face, vertex = value.vertex,
        barycentric = bary, world = js.Array(value.worldX, value.worldY, value.worldZ),
        origin = origin)
