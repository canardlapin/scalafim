package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Browser specimen for the two native depth-projection policies.  The Node
  * runner independently checks the returned scientific ray hits. */
object ThreeDepthPolicyBrowserProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(s"$error"), identity)
  private val surface = SurfaceId.unsafe("depth-plane")

  private def legacy(perspective: Boolean): SurfaceRenderPlan =
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-1.0, 1.0, 0.0), Seq(1.0, 1.0, 0.0)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val layer = ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("depth-color"), surface, geometry,
      Vector.fill(4)(Rgba32.unsafe(0, 0, 255))))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(surface, geometry))), Vector(layer)))
    val projection = if perspective then CameraProjection.Perspective(FieldOfViewDegrees.unsafe(45))
      else CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))
    val state = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal), SurfaceViewerAction.SetProjection(projection),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)).foldLeft(SurfaceViewerState.initial(model))((s, a) => ok(SurfaceViewer.reduce(model, s, a)))
    ok(SurfaceCompiler.compile(model, state))

  private def composition(plan: SurfaceRenderPlan): SurfaceCompositionPlan =
    val base = ok(SurfaceCompositionPlan.fromLegacy(plan))
    val view = base.views.head
    val instance = view.instances.head
    val translated = instance.copy(id = SurfaceInstanceId.make("depth-translated").toOption.get, translationX = .4, translationZ = .15)
    ok(SurfaceCompositionPlan.make(Vector(view.copy(id = SurfaceViewId.make("depth-view").toOption.get, instances = Vector(instance, translated))), base.resources))

  private def decorations(plan: SurfaceCompositionPlan, width: Int, height: Int): SurfaceDecorationGeometry =
    val marker = SurfaceMarkerDecoration(SurfaceDecorationId.make("depth-marker").toOption.get,
      SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(0))), SurfaceMarkerShape.Disk,
      SurfaceDecorationSize.pixels(12).toOption.get, Rgba32.unsafe(255, 0, 0), SurfaceDecorationDepthPolicy.DepthTest)
    ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector(marker), plan)), plan, width, height))

  @JSExportTopLevel("runScalafimThreeDepthPolicyProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val captures = new js.Array[js.Any]()
    for (precision, policyIndex) <- Vector(ThreeDepthPrecision.ConditionOccupiedRange, ThreeDepthPrecision.PreserveDepthProjection).zipWithIndex
    do
      // dispose() deliberately loses its WebGL context; each policy owns a fresh canvas.
      val policyCanvas = if policyIndex == 0 then canvas else canvas.selectDynamic("ownerDocument").applyDynamic("createElement")("canvas")
      val runtime = ok(ThreeJsRuntime.create(three, policyCanvas, ThreeJsRuntimeOptions(Rgba32.unsafe(255, 255, 255), precision)))
      val backend = ok(ThreeSurfaceBackend.create(runtime))
      try
        for perspective <- Vector(false, true); size <- Vector(ThreeCanvasSize.unsafe(240, 180), ThreeCanvasSize.unsafe(360, 240)) do
          val raw = legacy(perspective)
          val composed = composition(raw)
          val decoration = decorations(composed, size.width, size.height)
          val legacyReceipt = ok(backend.render(raw, size, forceDraw = true))
          val legacyPick = ok(backend.pick(size.width / 2.0, size.height / 2.0))
          val legacyProjections = runtime.projectionReceipts
          val composedReceipt = ok(backend.renderDecorated(composed, decoration, size, forceDraw = true))
          val composedPick = ok(backend.pick(size.width / 2.0, size.height / 2.0))
          val receipts = runtime.projectionReceipts
          require(receipts.nonEmpty && receipts.forall(_.depthPrecision == precision), "projection receipt policy drift")
          captures.push(js.Dynamic.literal(policy = precision.toString, perspective = perspective, width = size.width, height = size.height,
            legacyDraws = legacyReceipt.drawCalls, compositionDraws = composedReceipt.drawCalls,
            // Surface packets are row-major; Three uniforms/receipts are column-major.
            sourceProjection = Vector.tabulate(16)(index => raw.camera.projectionMatrix((index % 4) * 4 + index / 4)).toJSArray,
            legacyReceipts = legacyProjections.map(r => js.Dynamic.literal(view = r.view.map(_.value).orNull, matrix = r.columnMajorProjection.toJSArray,
              viewport = js.Array(r.viewport._1, r.viewport._2, r.viewport._3, r.viewport._4))).toJSArray,
            receipts = receipts.map(r => js.Dynamic.literal(view = r.view.map(_.value).orNull, matrix = r.columnMajorProjection.toJSArray,
              viewport = js.Array(r.viewport._1, r.viewport._2, r.viewport._3, r.viewport._4))).toJSArray,
            legacyPick = pick(legacyPick), compositionPick = pick(composedPick), png = policyCanvas.applyDynamic("toDataURL")("image/png")))
      finally backend.dispose()
    js.Dynamic.literal(captures = captures)

  private def pick(value: Option[ThreePick]): js.Any = value match
    case None => null
    case Some(hit) => js.Dynamic.literal(surface = hit.surface.value, face = hit.face, vertex = hit.vertex,
      world = js.Array(hit.worldX, hit.worldY, hit.worldZ))
