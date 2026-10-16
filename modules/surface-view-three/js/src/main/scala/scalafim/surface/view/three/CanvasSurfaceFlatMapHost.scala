package scalafim.surface.view.three

import scala.scalajs.js
import scala.util.control.NonFatal

import scalafim.surface.VertexId
import scalafim.surface.view.*

final case class CanvasSurfaceFlatMapStyle(
  background: String = "transparent",
  fill: String = "rgba(160, 164, 170, 0.35)",
  stroke: String = "rgba(255, 255, 255, 0.22)",
  hover: String = "#58c4ff",
  selected: String = "#ffd166",
  roi: String = "#ffd166",
  pointRadius: Double = 4.0
):
  require(pointRadius.isFinite && pointRadius > 0.0)

enum CanvasSurfaceFlatMapHostError:
  case Disposed, NoFrame
  case Pointer(reason: String)
  case Map(error: SurfaceFlatMapError)
  case Interaction(error: SurfaceFlatMapInteractionError)
  case Canvas(reason: String)

  def message: String = this match
    case Disposed => "Canvas flat-map host is disposed"
    case NoFrame => "Canvas flat-map host has no published flat map"
    case Pointer(reason) => s"invalid Canvas flat-map pointer: $reason"
    case Map(error) => error.toString
    case Interaction(error) => error.message
    case Canvas(reason) => s"Canvas flat-map publication failed: $reason"

/** Canvas2D reference host for [[SurfaceFlatMap]]. It owns no DOM listeners
  * or canvas lifetime; browser adapters own their event lifecycle. */
final class CanvasSurfaceFlatMapHost private (canvas: js.Dynamic, context: js.Dynamic):
  private var map = Option.empty[SurfaceFlatMap]
  private var viewport = Option.empty[SurfaceFlatMapViewport]
  private var style = CanvasSurfaceFlatMapStyle()
  private var hover = Option.empty[VertexId]
  private var selected = Option.empty[VertexId]
  private var rois = Vector.empty[SurfaceFlatMapRoi]
  private var drawing = Option.empty[(SurfaceFlatMapRoiMode, Vector[SurfaceFlatMapPoint])]
  private var closed = false

  def renderedRois: Vector[SurfaceFlatMapRoi] = rois

  def render(value: SurfaceFlatMap, target: SurfaceFlatMapViewport,
      nextStyle: CanvasSurfaceFlatMapStyle = CanvasSurfaceFlatMapStyle()): Either[CanvasSurfaceFlatMapHostError, Unit] =
    if closed then Left(CanvasSurfaceFlatMapHostError.Disposed)
    else
      // Outlines belong to this map geometry, even when a replacement reuses
      // the scientific surface id. Resize reuses the map and preserves them.
      if !map.contains(value) then
        rois = Vector.empty
        drawing = None
      val sameScientificDomain = map.exists(_.surface == value.surface)
      if !sameScientificDomain then
        hover = None
        selected = None
      else
        hover = hover.filter(_.index < value.vertexCount)
        selected = selected.filter(_.index < value.vertexCount)
      draw(value, target, nextStyle).map: _ =>
        map = Some(value)
        viewport = Some(target)
        style = nextStyle

  /** Synchronizes selection and time-bearing viewer state. The reference
    * flat-map fill remains uniform across timepoints. */
  def sync(model: SurfaceViewerModel, state: SurfaceViewerState): Either[CanvasSurfaceFlatMapHostError, Unit] =
    frame.flatMap: (value, _) =>
      SurfaceFlatMapInteraction.selectedVertex(value, model, state) match
        case Left(error) =>
          selected = None
          redraw().flatMap(_ => Left(CanvasSurfaceFlatMapHostError.Interaction(error)))
        case Right(selection) =>
          selected = selection
          redraw()

  def hoverAt(x: Double, y: Double, radiusPixels: Double = 6.0): Either[CanvasSurfaceFlatMapHostError, SurfaceFlatMapInteraction] =
    pick(x, y, radiusPixels).flatMap: vertex =>
      hover = vertex
      redraw().map(_ => SurfaceFlatMapInteraction.Hover(map.get.surface, vertex))

  def selectAt(x: Double, y: Double, radiusPixels: Double = 6.0): Either[CanvasSurfaceFlatMapHostError, Option[SurfaceFlatMapInteraction]] =
    pick(x, y, radiusPixels).flatMap: vertex =>
      selected = vertex
      redraw().map: _ =>
        vertex.map(value => SurfaceFlatMapInteraction.Select(SurfaceViewerAction.Select(map.get.surface, value)))

  /** Starts a polygon/lasso draft in Canvas coordinates. Completed outlines
    * are stored in map coordinates so resize preserves their semantics. */
  def beginRoi(mode: SurfaceFlatMapRoiMode, x: Double, y: Double): Either[CanvasSurfaceFlatMapHostError, Unit] =
    pointer(x, y).flatMap: point =>
      drawing = Some(mode -> Vector(point))
      redraw()

  def appendRoiPoint(x: Double, y: Double): Either[CanvasSurfaceFlatMapHostError, Unit] =
    pointer(x, y).flatMap: point =>
      drawing match
        case None => Left(CanvasSurfaceFlatMapHostError.Pointer("ROI drawing has not started"))
        case Some((mode, points)) =>
          drawing = Some(mode -> (points :+ point))
          redraw()

  def finishRoi(id: String): Either[CanvasSurfaceFlatMapHostError, SurfaceFlatMapInteraction] =
    frame.flatMap: (value, target) =>
      drawing.toRight(CanvasSurfaceFlatMapHostError.Pointer("ROI drawing has not started")).flatMap: (_, points) =>
        if points.length < 3 then Left(CanvasSurfaceFlatMapHostError.Pointer("ROI polygon requires at least three points"))
        else
          val screen = points.map(value.projectMapPoint(_, target))
          value.verticesInPolygon(screen, target).left.map(CanvasSurfaceFlatMapHostError.Map.apply).flatMap: vertices =>
            SurfaceFlatMapRoi.make(id, value, vertices, points).left.map(CanvasSurfaceFlatMapHostError.Map.apply).flatMap: roi =>
              rois = rois.filterNot(_.id == roi.id) :+ roi
              drawing = None
              redraw().map(_ => SurfaceFlatMapInteraction.RoiCreated(roi))

  def cancelRoi(): Either[CanvasSurfaceFlatMapHostError, Unit] =
    if closed then Left(CanvasSurfaceFlatMapHostError.Disposed)
    else
      drawing = None
      redraw()

  def addRoi(roi: SurfaceFlatMapRoi): Either[CanvasSurfaceFlatMapHostError, Unit] =
    frame.flatMap: (value, _) =>
      if roi.surface != value.surface then Left(CanvasSurfaceFlatMapHostError.Pointer("ROI surface does not match flat map"))
      else if roi.vertices.exists(vertex => vertex.index < 0 || vertex.index >= value.vertexCount) then
        Left(CanvasSurfaceFlatMapHostError.Pointer("ROI vertex is outside flat-map domain"))
      else
        rois = rois.filterNot(_.id == roi.id) :+ roi
        redraw()

  /** Replaces an existing ROI with the same scientific id. Its outline is in
    * map coordinates, so callers may reuse it after a viewport resize. */
  def updateRoi(roi: SurfaceFlatMapRoi): Either[CanvasSurfaceFlatMapHostError, Unit] =
    addRoi(roi)

  def resize(target: SurfaceFlatMapViewport): Either[CanvasSurfaceFlatMapHostError, Unit] =
    frame.flatMap((value, _) => render(value, target, style))

  def dispose(): Unit =
    if !closed then
      closed = true
      clearPublished()
      rois = Vector.empty
      drawing = None

  private def frame: Either[CanvasSurfaceFlatMapHostError, (SurfaceFlatMap, SurfaceFlatMapViewport)] =
    if closed then Left(CanvasSurfaceFlatMapHostError.Disposed)
    else map.zip(viewport).toRight(CanvasSurfaceFlatMapHostError.NoFrame)

  private def pointer(x: Double, y: Double): Either[CanvasSurfaceFlatMapHostError, SurfaceFlatMapPoint] =
    if !x.isFinite || !y.isFinite then Left(CanvasSurfaceFlatMapHostError.Pointer("coordinates must be finite"))
    else frame.map((value, target) => value.unproject(SurfaceFlatMapPoint(x, y), target))

  private def pick(x: Double, y: Double, radiusPixels: Double): Either[CanvasSurfaceFlatMapHostError, Option[VertexId]] =
    if !x.isFinite || !y.isFinite || !radiusPixels.isFinite || radiusPixels <= 0.0 then
      Left(CanvasSurfaceFlatMapHostError.Pointer("coordinates and radius must be finite; radius must be positive"))
    else frame.flatMap: (value, target) =>
      value.closestVertex(SurfaceFlatMapPoint(x, y), target, radiusPixels)
        .left.map(CanvasSurfaceFlatMapHostError.Map.apply).map(_.map(_.value))

  private def redraw(): Either[CanvasSurfaceFlatMapHostError, Unit] =
    frame.flatMap((value, target) => draw(value, target, style))

  private def draw(value: SurfaceFlatMap, target: SurfaceFlatMapViewport,
      nextStyle: CanvasSurfaceFlatMapStyle): Either[CanvasSurfaceFlatMapHostError, Unit] =
    val width = math.round(target.width)
    val height = math.round(target.height)
    if width < 1L || height < 1L || width > Int.MaxValue || height > Int.MaxValue then
      clearPublished()
      Left(CanvasSurfaceFlatMapHostError.Canvas("viewport cannot be represented by Canvas pixel dimensions"))
    else try
      canvas.updateDynamic("width")(width.toInt)
      canvas.updateDynamic("height")(height.toInt)
      context.applyDynamic("clearRect")(0, 0, target.width, target.height)
      context.updateDynamic("fillStyle")(nextStyle.background)
      context.applyDynamic("fillRect")(0, 0, target.width, target.height)
      context.updateDynamic("fillStyle")(nextStyle.fill)
      context.updateDynamic("strokeStyle")(nextStyle.stroke)
      context.updateDynamic("lineWidth")(1)
      value.triangles.foreach: (a, b, c) =>
        for
          pa <- value.project(a, target)
          pb <- value.project(b, target)
          pc <- value.project(c, target)
        do
          context.applyDynamic("beginPath")()
          context.applyDynamic("moveTo")(pa.x, pa.y)
          context.applyDynamic("lineTo")(pb.x, pb.y)
          context.applyDynamic("lineTo")(pc.x, pc.y)
          context.applyDynamic("closePath")()
          context.applyDynamic("fill")()
          context.applyDynamic("stroke")()
      rois.filter(_.surface == value.surface).foreach(roi =>
        drawOutline(roi.outline, value, target, nextStyle.roi, closed = true, label = Some(roi.id)))
      drawing.foreach: (_, points) =>
        drawOutline(points, value, target, nextStyle.hover, closed = false, label = None)
      drawMarker(hover, value, target, nextStyle.hover, nextStyle.pointRadius)
      drawMarker(selected, value, target, nextStyle.selected, nextStyle.pointRadius + 1.0)
      Right(())
    catch case NonFatal(error) =>
      clearPublished()
      Left(CanvasSurfaceFlatMapHostError.Canvas(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private def drawOutline(points: Vector[SurfaceFlatMapPoint], value: SurfaceFlatMap,
      target: SurfaceFlatMapViewport, color: String, closed: Boolean, label: Option[String]): Unit =
    points.headOption.foreach: first =>
      val start = value.projectMapPoint(first, target)
      context.applyDynamic("beginPath")()
      context.applyDynamic("moveTo")(start.x, start.y)
      points.tail.foreach: point =>
        val projected = value.projectMapPoint(point, target)
        context.applyDynamic("lineTo")(projected.x, projected.y)
      if closed then context.applyDynamic("closePath")()
      if closed then
        context.updateDynamic("fillStyle")(transparentColor(color, 0.18))
        context.applyDynamic("fill")()
      context.updateDynamic("strokeStyle")(color)
      context.updateDynamic("lineWidth")(if closed then 2 else 1.5)
      context.applyDynamic("stroke")()
      label.foreach: text =>
        val center = SurfaceFlatMapPoint(points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)
        val projected = value.projectMapPoint(center, target)
        context.updateDynamic("fillStyle")(color)
        context.updateDynamic("font")("12px sans-serif")
        context.updateDynamic("textAlign")("center")
        context.updateDynamic("textBaseline")("middle")
        context.applyDynamic("fillText")(text, projected.x, projected.y)

  private def drawMarker(vertex: Option[VertexId], value: SurfaceFlatMap,
      target: SurfaceFlatMapViewport, color: String, radius: Double): Unit =
    vertex.foreach: id =>
      value.project(id, target).foreach: point =>
        context.updateDynamic("fillStyle")(color)
        context.applyDynamic("beginPath")()
        context.applyDynamic("arc")(point.x, point.y, radius, 0, 2 * math.Pi)
        context.applyDynamic("fill")()

  private def clearPublished(): Unit =
    map = None
    viewport = None
    hover = None
    selected = None

  private def transparentColor(color: String, alpha: Double): String =
    if color.matches("(?i)^#[0-9a-f]{6}$") then
      val red = Integer.parseInt(color.substring(1, 3), 16)
      val green = Integer.parseInt(color.substring(3, 5), 16)
      val blue = Integer.parseInt(color.substring(5, 7), 16)
      s"rgba($red, $green, $blue, $alpha)"
    else color

object CanvasSurfaceFlatMapHost:
  def attach(canvas: js.Dynamic): Either[CanvasSurfaceFlatMapHostError, CanvasSurfaceFlatMapHost] =
    if canvas == null || js.isUndefined(canvas) then Left(CanvasSurfaceFlatMapHostError.Canvas("canvas is unavailable"))
    else
      try
        val context = canvas.applyDynamic("getContext")("2d")
        if context == null || js.isUndefined(context) then Left(CanvasSurfaceFlatMapHostError.Canvas("Canvas2D context is unavailable"))
        else Right(new CanvasSurfaceFlatMapHost(canvas, context))
      catch case NonFatal(error) => Left(CanvasSurfaceFlatMapHostError.Canvas(Option(error.getMessage).getOrElse(error.getClass.getName)))
