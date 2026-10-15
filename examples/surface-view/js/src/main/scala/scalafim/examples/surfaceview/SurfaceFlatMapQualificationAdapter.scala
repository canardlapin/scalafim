package scalafim.examples.surfaceview

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

import scalafim.surface.VertexId
import scalafim.surface.view.*
import scalafim.surface.view.three.*

/** Actual Canvas and Three hosts for the frozen C11 interaction endpoints. */
object SurfaceFlatMapQualificationAdapter:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(String.valueOf(error)), identity)

  @JSExportTopLevel("captureScalafimFlatMapQualification")
  def capture(three: js.Dynamic, flatCanvas: js.Dynamic, linkedCanvas: js.Dynamic): js.Dynamic =
    val fixture = ok(SurfaceFlatMapQualificationFixture.decode)
    val style = CanvasSurfaceFlatMapStyle(fixture.style.background, fixture.style.fill, fixture.style.stroke,
      fixture.style.hover, fixture.style.selected, fixture.style.roi, fixture.style.pointRadius)
    val host = ok(CanvasSurfaceFlatMapHost.attach(flatCanvas))
    val captures = new js.Array[js.Dynamic]()
    var backend = Option.empty[ThreeSurfaceBackend]
    def point(vertex: Int) = ok(fixture.map.project(VertexId(vertex), fixture.viewport))
    def roi(mode: SurfaceFlatMapRoiMode, points: Vector[SurfaceFlatMapPoint]): SurfaceFlatMapRoi =
      val screen = points.map(fixture.map.projectMapPoint(_, fixture.viewport))
      ok(host.beginRoi(mode, screen.head.x, screen.head.y))
      screen.tail.foreach(p => ok(host.appendRoiPoint(p.x, p.y)))
      ok(host.finishRoi(SurfaceFlatMapQualificationFixture.RoiName)) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected C11 ROI event: $other")
    def linkedImage(): js.Any =
      val value = backend.getOrElse:
        val runtime = ThreeJsRuntime.create(three, linkedCanvas).fold(error => throw new IllegalStateException(error.message), identity)
        val next = ThreeSurfaceBackend.create(runtime).fold(error => throw new IllegalStateException(error.message), identity)
        backend = Some(next)
        next
      val decorations = ok(fixture.linkedMarker)
      val packets = ok(SurfaceDecorationCompiler.compile(decorations, fixture.source.plan))
      val geometry = ok(SurfaceDecorationGeometry.lower(packets, fixture.source.plan, fixture.source.width, fixture.source.height))
      value.renderDecorated(fixture.source.plan, geometry, ThreeCanvasSize.unsafe(fixture.source.width, fixture.source.height), forceDraw = true)
        .fold(error => throw new IllegalStateException(error.message), identity)
      linkedCanvas.applyDynamic("toDataURL")("image/png")
    def endpoint(name: String, mode: SurfaceFlatMapRoiMode, points: Vector[SurfaceFlatMapPoint], time: Boolean, marker: Boolean): Unit =
      ok(host.render(fixture.map, fixture.viewport, style))
      val hoverPoint = point(3)
      val hovered = ok(host.hoverAt(hoverPoint.x, hoverPoint.y))
      val selectedPoint = point(0)
      val selected = ok(host.selectAt(selectedPoint.x, selectedPoint.y)).getOrElse(
        throw new IllegalStateException("C11 select action missed vertex 0"))
      val state0 = selected match
        case SurfaceFlatMapInteraction.Select(action) => ok(SurfaceViewer.reduce(fixture.model, fixture.initial, action))
        case other => throw new IllegalStateException(s"unexpected C11 select event: $other")
      val state = if time then ok(SurfaceViewer.reduce(fixture.model, state0, SurfaceViewerAction.SetTimepoint(1))) else state0
      ok(host.sync(fixture.model, state))
      val created = roi(mode, points)
      captures.push(js.Dynamic.literal(
        checkpoint = name,
        image = flatCanvas.applyDynamic("toDataURL")("image/png"),
        linkedDisplay = if marker then linkedImage() else null,
        hover = hovered.toString,
        selection = state.selection.map(_.vertex.index).getOrElse(-1),
        timepoint = state.timepoint,
        roiVertices = created.vertices.map(_.index).toJSArray,
        roiOutline = created.outline.map(p => js.Dynamic.literal(x = p.x, y = p.y)).toJSArray,
        route = "actual CanvasSurfaceFlatMapHost actions"))
    try
      endpoint("hover", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("pick", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("polygon", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("lasso", SurfaceFlatMapRoiMode.Lasso, SurfaceFlatMapQualificationFixture.Lasso, time = false, marker = false)
      endpoint("linked-marker", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = true)
      endpoint("linked-time", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = true, marker = false)
      js.Dynamic.literal(schema = "surface-flatmap-qualification.v1", sourceJson = SurfaceFlatMapQualificationFixture.SourceJson,
        width = fixture.source.width, height = fixture.source.height, padding = fixture.viewport.padding,
        fit = "contain", style = js.Dynamic.literal(background = fixture.style.background, fill = fixture.style.fill,
          stroke = fixture.style.stroke, hover = fixture.style.hover, selected = fixture.style.selected,
          roi = fixture.style.roi, pointRadius = fixture.style.pointRadius), captures = captures)
    finally
      host.dispose()
      backend.foreach(value => ok(value.dispose()))
