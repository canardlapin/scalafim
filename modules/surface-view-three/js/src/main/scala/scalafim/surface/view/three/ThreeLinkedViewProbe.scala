package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scalafim.surface.{SurfaceKind, VertexId}
import scalafim.surface.view.*

/** A real host integration replay. Flat-map and native picks feed the same
  * reducer; fractional playback retains its temporal binding across restore.
  */
object ThreeLinkedViewProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(s"$error"), identity)

  @JSExportTopLevel("runScalafimLinkedViewProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic, flatCanvas: js.Dynamic): js.Dynamic =
    val fixture = SurfaceLinkedFixture
    val backend = ok(ThreeSurfaceBackend.create(ok(ThreeJsRuntime.create(three, canvas))))
    val flat = ok(CanvasSurfaceFlatMapHost.attach(flatCanvas))
    var playback = fixture.playback(0)
    var model = fixture.model(playback)
    var state = fixture.state(model, 0, 0)
    val initialNavigation = fixture.navigation(model, state)
    var navigation = ok(SurfaceCompositionState.make(initialNavigation.views,
      Vector(Set(initialNavigation.views(1).id, initialNavigation.views(2).id))))
    val captures = new js.Array[js.Any]()
    val size = ThreeCanvasSize.unsafe(512, 512)
    def capture(name: String): Unit =
      val plan = ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), navigation.views))
      ok(backend.render(plan, size, forceDraw = true))
      val surfaceImage = canvas.applyDynamic("toDataURL")("image/png")
      val packets = ok(SurfaceDecorationCompiler.compile(fixture.marker(state), plan))
      val geometry = ok(SurfaceDecorationGeometry.lower(packets, plan, 512, 512))
      ok(backend.renderDecorated(plan, geometry, size, forceDraw = true))
      ok(flat.sync(model, state))
      val selected = state.selection.get.vertex.index
      val time = playback.playhead.value
      val low = math.floor(time).toInt
      val high = math.min(2, low + 1)
      val alpha = time - low
      val expected = fixture.Values(selected)(low) * (1 - alpha) + fixture.Values(selected)(high) * alpha
      require(plan.views.forall(view => math.abs(view.readouts.head.layerValues.head._2.toDouble - expected) < 1e-10))
      val morph = state.geometryPresentations(fixture.Surface) match
        case SurfaceGeometryPresentation.Morphing(_, _, value) => value.value
        case SurfaceGeometryPresentation.Fixed(SurfaceKind.Inflated) => 1.0
        case _ => 0.0
      val point = fixture.Vertices(selected).map(_ * (1 + .2 * morph))
      require(packets.markers.size == 4)
      require(packets.markers.forall(marker => math.abs(marker.point.x - point(0)) < 1e-6 &&
        math.abs(marker.point.y - point(1)) < 1e-6 && math.abs(marker.point.z - point(2)) < 1e-6))
      captures.push(js.Dynamic.literal(name = name, time = time, morph = morph, selected = selected,
        readout = expected, surfaceImage = surfaceImage, decoratedImage = canvas.applyDynamic("toDataURL")("image/png"),
        flatImage = flatCanvas.applyDynamic("toDataURL")("image/png"), markerCount = packets.markers.size,
        cameraKeys = plan.receipt.cameraKeys.toJSArray))
    try
      ok(flat.render(fixture.FlatMap, ok(SurfaceFlatMapViewport.make(200, 200, 20))))
      capture("initial")
      ok(flat.selectAt(180, 20)).get match
        case SurfaceFlatMapInteraction.Select(action) => state = ok(SurfaceViewer.reduce(model, state, action))
        case other => throw new IllegalStateException(s"unexpected selection $other")
      require(state.selection.get.vertex.index == 2)
      capture("flat-to-four-views")
      playback = ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Play))
      playback = ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(.5))))
      model = fixture.model(playback)
      capture("interpolated-time")
      playback = ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Pause))
      require(ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(1)))) == playback)
      state = ok(SurfaceViewer.reduce(model, state,
        SurfaceViewerAction.SetGeometryMorphFraction(fixture.Surface, SurfaceMorphFraction.unsafe(.5))))
      capture("paused-morph-midpoint")
      val pick = ok(backend.pick(64, 192)).getOrElse(throw new IllegalStateException("3D pick missed"))
      state = ok(SurfaceViewer.reduce(model, state, SurfaceViewerAction.Select(pick.surface, VertexId(pick.vertex))))
      require(pick.vertex == 0)
      capture("three-to-flat")
      state = ok(SurfaceViewer.reduce(model, state,
        SurfaceViewerAction.SetGeometryMorphFraction(fixture.Surface, SurfaceMorphFraction.unsafe(1))))
      navigation = ok(navigation.navigate(navigation.views.head.id, SurfaceCompositionNavigation.PanBy(.15, .05)))
      capture("morph-endpoint")
      val temporalJson = SurfacePlaybackDocument.encode(fixture.document(playback))
      val sceneJson = SurfaceCompositionCodec.encode(ok(SurfaceCompositionDocument.capture(model, state,
        navigation, fixture.Bindings, fixture.Provenance)))
      val temporal = ok(SurfacePlaybackDocument.decode(temporalJson))
      val layer = ok(temporal.prepare(fixture.Field, fixture.Reference, fixture.Mapping, SurfaceFrameCache.Disabled)).layer
      model = ok(SurfaceViewerModel.make(Vector(fixture.Asset), Vector(layer)))
      val document = ok(SurfaceCompositionCodec.decode(sceneJson))
      val display = ok(SurfaceDisplayFrame.declare(model, fixture.Surface, fixture.Frame))
      val restored = ok(document.restore(model, fixture.Bindings, Map(fixture.Surface -> display)))
      require(restored.navigation.cameraLinks == navigation.cameraLinks)
      require(restored.navigation.views.map(view => (view.id, view.viewport, view.instances.map(_.id), view.pose.eye.coordinates, view.pose.target.coordinates)) ==
        navigation.views.map(view => (view.id, view.viewport, view.instances.map(_.id), view.pose.eye.coordinates, view.pose.target.coordinates)))
      navigation = restored.navigation
      state = ok(document.scene.restore(model, fixture.Bindings)).copy(geometryPresentations = restored.presentation.geometryPresentations)
      playback = temporal.state
      require(playback.playhead.value == .5 && playback.status == SurfacePlaybackStatus.Paused)
      capture("restored")
      playback = ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Play))
      playback = ok(SurfacePlayback.reduce(fixture.Axis, playback, SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(.25))))
      require(playback.playhead.value == .75)
      model = fixture.model(playback)
      capture("resumed")
      js.Dynamic.literal(captures = captures, playbackDocument = temporalJson, compositionDocument = sceneJson)
    finally
      flat.dispose()
      backend.dispose()
