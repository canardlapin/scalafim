package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

import scalafim.surface.VertexId
import scalafim.surface.view.*

/** Browser probe for reference FlatMapView comparisons. It exercises uniform
  * drawing, scientific selections, ROI persistence across resize, and state
  * synchronization at a new timepoint. */
object ThreeFlatMapProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(String.valueOf(error)), identity)

  @JSExportTopLevel("runScalafimThreeFlatMapProbe")
  def run(canvas: js.Dynamic): js.Dynamic =
    val surface = SurfaceFaceFixture.Surface
    val map = ok(SurfaceFlatMap.make(
      surface,
      Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(2, 0), SurfaceFlatMapPoint(2, 1), SurfaceFlatMapPoint(0, 1)),
      Vector((0, 1, 2), (0, 2, 3))
    ))
    val first = ok(SurfaceFlatMapViewport.make(100, 100, 10, SurfaceFlatMapFit.Contain))
    val resized = ok(SurfaceFlatMapViewport.make(200, 100, 10, SurfaceFlatMapFit.Contain))
    val style = CanvasSurfaceFlatMapStyle()
    val captures = new js.Array[js.Dynamic]()
    def capture(stage: String): Unit =
      captures.push(js.Dynamic.literal(
        stage = stage,
        width = canvas.selectDynamic("width").asInstanceOf[Int],
        height = canvas.selectDynamic("height").asInstanceOf[Int],
        image = canvas.applyDynamic("toDataURL")("image/png")
      ))
    val host = ok(CanvasSurfaceFlatMapHost.attach(canvas))
    try
      ok(host.render(map, first, style))
      capture("plain")
      val hover = ok(host.hoverAt(10, 70))
      capture("hover")
      val click = ok(host.selectAt(10, 70)).getOrElse(throw new IllegalStateException("flat-map click missed vertex"))
      capture("selected")
      ok(host.beginRoi(SurfaceFlatMapRoiMode.Polygon, 8, 72))
      ok(host.appendRoiPoint(92, 72))
      ok(host.appendRoiPoint(92, 28))
      ok(host.appendRoiPoint(8, 28))
      val roi = ok(host.finishRoi("resize-stable")) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected ROI event: $other")
      capture("polygon-roi")
      ok(host.resize(resized))
      ok(host.updateRoi(roi))
      val outlineAfterResize = map.projectMapPoint(roi.outline.head, resized)
      capture("resized")
      val model = SurfaceFaceFixture.model
      val state0 = SurfaceFaceFixture.state(model)
      val state1 = ok(SurfaceViewer.reduce(model, state0, SurfaceViewerAction.SetTimepoint(1)))
      val selectedState = ok(SurfaceViewer.reduce(model, state1, SurfaceViewerAction.Select(surface, VertexId(2))))
      ok(host.sync(model, selectedState))
      capture("synced-time-selection")
      ok(host.beginRoi(SurfaceFlatMapRoiMode.Lasso, 16, 94))
      ok(host.appendRoiPoint(104, 94))
      ok(host.appendRoiPoint(104, 6))
      ok(host.appendRoiPoint(16, 6))
      val lasso = ok(host.finishRoi("lasso-resize-stable")) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected lasso event: $other")
      capture("lasso-roi")
      js.Dynamic.literal(
        hover = hover.toString,
        click = click.toString,
        polygonRoiVertices = roi.vertices.map(_.index).toJSArray,
        lassoRoiVertices = lasso.vertices.map(_.index).toJSArray,
        roiOutlineAfterResize = js.Dynamic.literal(x = outlineAfterResize.x, y = outlineAfterResize.y),
        timepoint = selectedState.timepoint,
        selection = selectedState.selection.map(_.vertex.index).getOrElse(-1),
        style = js.Dynamic.literal(background = style.background, fill = style.fill, stroke = style.stroke, hover = style.hover,
          selected = style.selected, roi = style.roi, pointRadius = style.pointRadius),
        captures = captures
      )
    finally host.dispose()
