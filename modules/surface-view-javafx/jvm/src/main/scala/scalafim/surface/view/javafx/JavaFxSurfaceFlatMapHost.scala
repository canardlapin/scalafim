package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.scene.canvas.Canvas
import javafx.scene.paint.Color
import javafx.scene.text.{Font, TextAlignment}
import javafx.geometry.VPos
import scala.util.control.NonFatal

import scalafim.surface.VertexId
import scalafim.surface.view.*

final case class JavaFxSurfaceFlatMapStyle(
  background: String = "transparent",
  fill: String = "rgba(160, 164, 170, 0.35)",
  stroke: String = "rgba(255, 255, 255, 0.22)",
  hover: String = "#58c4ff",
  selected: String = "#ffd166",
  roi: String = "#ffd166",
  pointRadius: Double = 4.0
):
  require(pointRadius.isFinite && pointRadius > 0.0)

enum JavaFxSurfaceFlatMapHostError:
  case Disposed, NoFrame, WrongThread
  case Pointer(reason: String)
  case Map(error: SurfaceFlatMapError)
  case Interaction(error: SurfaceFlatMapInteractionError)
  case Fx(reason: String)

  def message: String = this match
    case Disposed => "JavaFX flat-map host is disposed"
    case NoFrame => "JavaFX flat-map host has no published flat map"
    case WrongThread => "JavaFX flat-map host must be called on the Application Thread"
    case Pointer(reason) => s"invalid JavaFX flat-map pointer: $reason"
    case Map(error) => error.toString
    case Interaction(error) => error.message
    case Fx(reason) => s"JavaFX flat-map publication failed: $reason"

/** JavaFX Canvas reference host for [[SurfaceFlatMap]]. It has no input-event
  * bindings: callers own listener lifetime and pass Canvas-local coordinates.
  * Every public operation that touches the Canvas explicitly requires the FX
  * Application Thread. */
final class JavaFxSurfaceFlatMapHost private ():
  val canvas = new Canvas()
  private val graphics = canvas.getGraphicsContext2D
  private var map = Option.empty[SurfaceFlatMap]
  private var viewport = Option.empty[SurfaceFlatMapViewport]
  private var style = JavaFxSurfaceFlatMapStyle()
  private var hover = Option.empty[VertexId]
  private var selected = Option.empty[VertexId]
  private var rois = Vector.empty[SurfaceFlatMapRoi]
  private var drawing = Option.empty[(SurfaceFlatMapRoiMode, Vector[SurfaceFlatMapPoint])]
  private var closed = false

  def renderedRois: Either[JavaFxSurfaceFlatMapHostError, Vector[SurfaceFlatMapRoi]] =
    ready.map(_ => rois)

  def render(value: SurfaceFlatMap, target: SurfaceFlatMapViewport,
      nextStyle: JavaFxSurfaceFlatMapStyle = JavaFxSurfaceFlatMapStyle()): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    ready.flatMap: _ =>
      // A surface id alone does not identify the flat-map coordinate geometry.
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

  def sync(model: SurfaceViewerModel, state: SurfaceViewerState): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    frame.flatMap: (value, _) =>
      SurfaceFlatMapInteraction.selectedVertex(value, model, state) match
        case Left(error) =>
          selected = None
          redraw().flatMap(_ => Left(JavaFxSurfaceFlatMapHostError.Interaction(error)))
        case Right(selection) =>
          selected = selection
          redraw()

  def hoverAt(x: Double, y: Double, radiusPixels: Double = 6.0): Either[JavaFxSurfaceFlatMapHostError, SurfaceFlatMapInteraction] =
    pick(x, y, radiusPixels).flatMap: vertex =>
      hover = vertex
      redraw().map(_ => SurfaceFlatMapInteraction.Hover(map.get.surface, vertex))

  def selectAt(x: Double, y: Double, radiusPixels: Double = 6.0): Either[JavaFxSurfaceFlatMapHostError, Option[SurfaceFlatMapInteraction]] =
    pick(x, y, radiusPixels).flatMap: vertex =>
      selected = vertex
      redraw().map: _ =>
        vertex.map(value => SurfaceFlatMapInteraction.Select(SurfaceViewerAction.Select(map.get.surface, value)))

  def beginRoi(mode: SurfaceFlatMapRoiMode, x: Double, y: Double): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    pointer(x, y).flatMap: point =>
      drawing = Some(mode -> Vector(point))
      redraw()

  def appendRoiPoint(x: Double, y: Double): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    pointer(x, y).flatMap: point =>
      drawing match
        case None => Left(JavaFxSurfaceFlatMapHostError.Pointer("ROI drawing has not started"))
        case Some((mode, points)) =>
          drawing = Some(mode -> (points :+ point))
          redraw()

  def finishRoi(id: String): Either[JavaFxSurfaceFlatMapHostError, SurfaceFlatMapInteraction] =
    frame.flatMap: (value, target) =>
      drawing.toRight(JavaFxSurfaceFlatMapHostError.Pointer("ROI drawing has not started")).flatMap: (_, points) =>
        if points.length < 3 then Left(JavaFxSurfaceFlatMapHostError.Pointer("ROI polygon requires at least three points"))
        else
          val screen = points.map(value.projectMapPoint(_, target))
          value.verticesInPolygon(screen, target).left.map(JavaFxSurfaceFlatMapHostError.Map.apply).flatMap: vertices =>
            SurfaceFlatMapRoi.make(id, value, vertices, points).left.map(JavaFxSurfaceFlatMapHostError.Map.apply).flatMap: roi =>
              rois = rois.filterNot(_.id == roi.id) :+ roi
              drawing = None
              redraw().map(_ => SurfaceFlatMapInteraction.RoiCreated(roi))

  def cancelRoi(): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    ready.flatMap: _ =>
      drawing = None
      redraw()

  def addRoi(roi: SurfaceFlatMapRoi): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    frame.flatMap: (value, _) =>
      if roi.surface != value.surface then Left(JavaFxSurfaceFlatMapHostError.Pointer("ROI surface does not match flat map"))
      else if roi.vertices.exists(vertex => vertex.index < 0 || vertex.index >= value.vertexCount) then
        Left(JavaFxSurfaceFlatMapHostError.Pointer("ROI vertex is outside flat-map domain"))
      else
        rois = rois.filterNot(_.id == roi.id) :+ roi
        redraw()

  def updateRoi(roi: SurfaceFlatMapRoi): Either[JavaFxSurfaceFlatMapHostError, Unit] = addRoi(roi)

  def resize(target: SurfaceFlatMapViewport): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    frame.flatMap((value, _) => render(value, target, style))

  def dispose(): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    ready.map: _ =>
      closed = true
      clearPublished()
      rois = Vector.empty
      drawing = None
      graphics.clearRect(0, 0, canvas.getWidth, canvas.getHeight)

  private def ready: Either[JavaFxSurfaceFlatMapHostError, Unit] =
    if closed then Left(JavaFxSurfaceFlatMapHostError.Disposed)
    else if !Platform.isFxApplicationThread then Left(JavaFxSurfaceFlatMapHostError.WrongThread)
    else Right(())

  private def frame: Either[JavaFxSurfaceFlatMapHostError, (SurfaceFlatMap, SurfaceFlatMapViewport)] =
    ready.flatMap(_ => map.zip(viewport).toRight(JavaFxSurfaceFlatMapHostError.NoFrame))

  private def pointer(x: Double, y: Double): Either[JavaFxSurfaceFlatMapHostError, SurfaceFlatMapPoint] =
    if !x.isFinite || !y.isFinite then Left(JavaFxSurfaceFlatMapHostError.Pointer("coordinates must be finite"))
    else frame.map((value, target) => value.unproject(SurfaceFlatMapPoint(x, y), target))

  private def pick(x: Double, y: Double, radiusPixels: Double): Either[JavaFxSurfaceFlatMapHostError, Option[VertexId]] =
    if !x.isFinite || !y.isFinite || !radiusPixels.isFinite || radiusPixels <= 0.0 then
      Left(JavaFxSurfaceFlatMapHostError.Pointer("coordinates and radius must be finite; radius must be positive"))
    else frame.flatMap: (value, target) =>
      value.closestVertex(SurfaceFlatMapPoint(x, y), target, radiusPixels)
        .left.map(JavaFxSurfaceFlatMapHostError.Map.apply).map(_.map(_.value))

  private def redraw(): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    frame.flatMap((value, target) => draw(value, target, style))

  private def draw(value: SurfaceFlatMap, target: SurfaceFlatMapViewport,
      nextStyle: JavaFxSurfaceFlatMapStyle): Either[JavaFxSurfaceFlatMapHostError, Unit] =
    val width = math.round(target.width)
    val height = math.round(target.height)
    if width < 1L || height < 1L || width > Int.MaxValue || height > Int.MaxValue then
      clearPublished()
      Left(JavaFxSurfaceFlatMapHostError.Fx("viewport cannot be represented by Canvas pixel dimensions"))
    else
      try
        canvas.setWidth(width.toDouble)
        canvas.setHeight(height.toDouble)
        graphics.clearRect(0, 0, target.width, target.height)
        graphics.setFill(color(nextStyle.background))
        graphics.fillRect(0, 0, target.width, target.height)
        graphics.setFill(color(nextStyle.fill))
        graphics.setStroke(color(nextStyle.stroke))
        graphics.setLineWidth(1)
        value.triangles.foreach: (a, b, c) =>
          for
            pa <- value.project(a, target)
            pb <- value.project(b, target)
            pc <- value.project(c, target)
          do
            graphics.beginPath()
            graphics.moveTo(pa.x, pa.y)
            graphics.lineTo(pb.x, pb.y)
            graphics.lineTo(pc.x, pc.y)
            graphics.closePath()
            graphics.fill()
            graphics.stroke()
        rois.filter(_.surface == value.surface).foreach(roi =>
          drawOutline(roi.outline, value, target, nextStyle.roi, closed = true, label = Some(roi.id)))
        drawing.foreach: (_, points) =>
          drawOutline(points, value, target, nextStyle.hover, closed = false, label = None)
        drawMarker(hover, value, target, nextStyle.hover, nextStyle.pointRadius)
        drawMarker(selected, value, target, nextStyle.selected, nextStyle.pointRadius + 1.0)
        Right(())
      catch case NonFatal(error) =>
        clearPublished()
        Left(JavaFxSurfaceFlatMapHostError.Fx(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private def drawOutline(points: Vector[SurfaceFlatMapPoint], value: SurfaceFlatMap,
      target: SurfaceFlatMapViewport, colorValue: String, closed: Boolean, label: Option[String]): Unit =
    points.headOption.foreach: first =>
      val start = value.projectMapPoint(first, target)
      graphics.beginPath()
      graphics.moveTo(start.x, start.y)
      points.tail.foreach: point =>
        val projected = value.projectMapPoint(point, target)
        graphics.lineTo(projected.x, projected.y)
      if closed then graphics.closePath()
      if closed then
        graphics.setFill(alphaColor(colorValue, 0.18))
        graphics.fill()
      graphics.setStroke(color(colorValue))
      graphics.setLineWidth(if closed then 2 else 1.5)
      graphics.stroke()
      label.foreach: text =>
        val center = SurfaceFlatMapPoint(points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)
        val projected = value.projectMapPoint(center, target)
        graphics.setFill(color(colorValue))
        graphics.setFont(Font.font("sans-serif", 12))
        graphics.setTextAlign(TextAlignment.CENTER)
        graphics.setTextBaseline(VPos.CENTER)
        graphics.fillText(text, projected.x, projected.y)

  private def drawMarker(vertex: Option[VertexId], value: SurfaceFlatMap,
      target: SurfaceFlatMapViewport, colorValue: String, radius: Double): Unit =
    vertex.foreach: id =>
      value.project(id, target).foreach: point =>
        graphics.setFill(color(colorValue))
        graphics.fillOval(point.x - radius, point.y - radius, radius * 2, radius * 2)

  private def color(value: String): Color =
    if value == "transparent" then Color.TRANSPARENT else Color.web(value)

  private def alphaColor(value: String, alpha: Double): Color =
    val base = color(value)
    Color.color(base.getRed, base.getGreen, base.getBlue, alpha)

  private def clearPublished(): Unit =
    map = None
    viewport = None
    hover = None
    selected = None

object JavaFxSurfaceFlatMapHost:
  def create(): Either[JavaFxSurfaceFlatMapHostError, JavaFxSurfaceFlatMapHost] =
    if !Platform.isFxApplicationThread then Left(JavaFxSurfaceFlatMapHostError.WrongThread)
    else
      try Right(new JavaFxSurfaceFlatMapHost)
      catch case NonFatal(error) => Left(JavaFxSurfaceFlatMapHostError.Fx(Option(error.getMessage).getOrElse(error.getClass.getName)))
