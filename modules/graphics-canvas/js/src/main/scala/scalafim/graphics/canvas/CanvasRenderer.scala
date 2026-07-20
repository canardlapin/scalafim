package scalafim.graphics.canvas

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8ClampedArray
import scala.collection.mutable
import scalafim.graphics.*

final case class CanvasOptions private (width: Int, height: Int)

object CanvasOptions:
  val default: CanvasOptions =
    unsafe()

  def apply(width: Int = 640, height: Int = 480): Either[CanvasRenderError, CanvasOptions] =
    if width <= 0 || height <= 0 then Left(CanvasRenderError.InvalidCanvasSize(width, height))
    else Right(new CanvasOptions(width, height))

  def unsafe(width: Int = 640, height: Int = 480): CanvasOptions =
    apply(width, height).orThrow

enum CanvasRenderError:
  case InvalidCanvasSize(width: Int, height: Int)
  case Graphics(error: GraphicsError)

  def message: String =
    this match
      case InvalidCanvasSize(width, height) =>
        s"Canvas size must be positive: ${width}x$height"
      case Graphics(error) =>
        error.message

object CanvasRenderError:
  extension [A](either: Either[CanvasRenderError, A])
    def orThrow: A =
      either match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error.message)

final case class CanvasColor(css: String, alpha: Double)

object CanvasColor:
  def fromRgba(color: Rgba): CanvasColor =
    def channel(value: Int): String =
      val encoded = value.toHexString
      if encoded.length == 1 then "0" + encoded else encoded
    CanvasColor(
      "#" + channel(color.red) + channel(color.green) + channel(color.blue),
      color.alpha
    )

enum CanvasLineDash:
  case Solid
  case Pattern(values: Vector[Double])

object CanvasLineDash:
  def fromLineType(lineType: LineType): CanvasLineDash =
    lineType match
      case LineType.Solid  => CanvasLineDash.Solid
      case LineType.Dashed => CanvasLineDash.Pattern(Vector(6.0, 4.0))
      case LineType.Dotted => CanvasLineDash.Pattern(Vector(1.0, 3.0))

final case class CanvasPaint(
    stroke: Option[CanvasColor],
    fill: Option[CanvasColor],
    lineWidth: Double,
    dash: CanvasLineDash,
    lineCap: LineCap,
    lineJoin: LineJoin,
    opacity: Double
)

object CanvasPaint:
  def fromGraphicParams(gp: GraphicParams): CanvasPaint =
    CanvasPaint(
      gp.stroke.map(CanvasColor.fromRgba),
      gp.fill.map(CanvasColor.fromRgba),
      gp.lineWidth,
      CanvasLineDash.fromLineType(gp.lineType),
      gp.lineCap,
      gp.lineJoin,
      gp.alpha
    )

  def text(gp: GraphicParams): CanvasPaint =
    val color = gp.fill.orElse(gp.stroke).getOrElse(Rgba.Black)
    CanvasPaint(None, Some(CanvasColor.fromRgba(color)), 0.0, CanvasLineDash.Solid, gp.lineCap, gp.lineJoin, gp.alpha)

/** Deterministic Canvas 2D operations in device coordinates. Group effects
  * deliberately record rotation before clipping: the clip is installed in
  * the rotated local coordinate system, matching the corresponding SVG group.
  */
enum CanvasCommand:
  case Save(name: Option[GraphicsName])
  case Rotate(degrees: Double, pivotX: Double, pivotY: Double)
  case ClipRect(x: Double, y: Double, width: Double, height: Double)
  case Disc(centerX: Double, centerY: Double, radius: Double, paint: CanvasPaint, name: Option[GraphicsName])
  case Polyline(points: Vector[DevicePoint], closed: Boolean, paint: CanvasPaint, name: Option[GraphicsName])
  case Rectangle(
      x: Double,
      y: Double,
      width: Double,
      height: Double,
      paint: CanvasPaint,
      name: Option[GraphicsName]
  )
  case Text(
      label: String,
      x: Double,
      y: Double,
      horizontal: HJust,
      vertical: VJust,
      rotationDegrees: Double,
      fontSizePx: Double,
      fontFamily: Option[String],
      paint: CanvasPaint,
      name: Option[GraphicsName]
  )
  case Image(
      image: RasterImage,
      x: Double,
      y: Double,
      width: Double,
      height: Double,
      interpolation: RasterInterpolation,
      alpha: Double,
      name: Option[GraphicsName]
  )
  case Restore(name: Option[GraphicsName])

final case class CanvasProgram private (
    width: Int,
    height: Int,
    commands: Vector[CanvasCommand]
)

object CanvasProgram:
  private[canvas] def fromDevice(scene: DeviceScene): CanvasProgram =
    val out = Vector.newBuilder[CanvasCommand]
    scene.elements.foreach(appendElement(_, out))
    new CanvasProgram(scene.width.toInt, scene.height.toInt, out.result())

  def validate(program: CanvasProgram): Option[String] =
    var stack = List.empty[Option[GraphicsName]]
    var idx = 0
    var problem: Option[String] = None
    while idx < program.commands.length && problem.isEmpty do
      val command = program.commands(idx)
      command match
        case CanvasCommand.Save(name) =>
          stack = name :: stack
        case CanvasCommand.Restore(name) =>
          stack match
            case expected :: rest if expected == name => stack = rest
            case expected :: _ => problem = Some(s"restore marker $name does not match save marker $expected")
            case Nil           => problem = Some("restore without a matching save")
        case other =>
          problem = firstInvalidNumber(other)
      idx += 1
    problem.orElse(if stack.nonEmpty then Some(s"${stack.length} canvas save operations were not restored") else None)

  private def appendElement(element: DeviceElement, out: scala.collection.mutable.Builder[CanvasCommand, Vector[CanvasCommand]]): Unit =
    element match
      case DeviceElement.Mark(primitive) =>
        out += fromPrimitive(primitive)
      case DeviceElement.Group(name, clip, rotation, children) =>
        out += CanvasCommand.Save(name)
        rotation.foreach(value => out += CanvasCommand.Rotate(value.degrees, value.pivotX, value.pivotY))
        clip.foreach(value => out += CanvasCommand.ClipRect(value.x, value.y, value.width, value.height))
        children.foreach(appendElement(_, out))
        out += CanvasCommand.Restore(name)

  private def fromPrimitive(primitive: DevicePrimitive): CanvasCommand =
    primitive match
      case DevicePrimitive.Disc(centerX, centerY, radius, gp, name) =>
        CanvasCommand.Disc(centerX, centerY, radius, CanvasPaint.fromGraphicParams(gp), name)
      case DevicePrimitive.Polyline(points, closed, gp, name) =>
        CanvasCommand.Polyline(points, closed, CanvasPaint.fromGraphicParams(gp), name)
      case DevicePrimitive.RectShape(x, y, width, height, gp, name) =>
        CanvasCommand.Rectangle(x, y, width, height, CanvasPaint.fromGraphicParams(gp), name)
      case DevicePrimitive.TextRun(label, x, y, horizontal, vertical, rotation, fontSize, fontFamily, gp, name) =>
        CanvasCommand.Text(
          label,
          x,
          y,
          horizontal,
          vertical,
          rotation,
          fontSize,
          fontFamily,
          CanvasPaint.text(gp),
          name
        )
      case DevicePrimitive.Image(image, x, y, width, height, interpolation, alpha, name) =>
        CanvasCommand.Image(image, x, y, width, height, interpolation, alpha, name)

  private def firstInvalidNumber(command: CanvasCommand): Option[String] =
    val values = command match
      case CanvasCommand.Rotate(degrees, pivotX, pivotY) =>
        Vector(degrees, pivotX, pivotY)
      case CanvasCommand.ClipRect(x, y, width, height) =>
        Vector(x, y, width, height)
      case CanvasCommand.Disc(centerX, centerY, radius, paint, _) =>
        Vector(centerX, centerY, radius, paint.lineWidth, paint.opacity)
      case CanvasCommand.Polyline(points, _, paint, _) =>
        points.flatMap(point => Vector(point.x, point.y)) ++ Vector(paint.lineWidth, paint.opacity)
      case CanvasCommand.Rectangle(x, y, width, height, paint, _) =>
        Vector(x, y, width, height, paint.lineWidth, paint.opacity)
      case CanvasCommand.Text(_, x, y, _, _, rotation, fontSize, _, paint, _) =>
        Vector(x, y, rotation, fontSize, paint.opacity)
      case CanvasCommand.Image(_, x, y, width, height, _, alpha, _) =>
        Vector(x, y, width, height, alpha)
      case CanvasCommand.Save(_) | CanvasCommand.Restore(_) =>
        Vector.empty
    if values.forall(_.isFinite) then None else Some(s"non-finite numeric value in $command")

@js.native
trait CanvasImageData extends js.Object:
  val data: Uint8ClampedArray = js.native

@js.native
trait CanvasImageSource extends js.Object

@js.native
trait CanvasElement extends CanvasImageSource:
  var width: Int = js.native
  var height: Int = js.native
  val ownerDocument: js.Dynamic = js.native
  def getContext(kind: String): CanvasRenderingContext2D = js.native

@js.native
trait CanvasRenderingContext2D extends js.Object:
  val canvas: CanvasElement = js.native
  var strokeStyle: js.Any = js.native
  var fillStyle: js.Any = js.native
  var globalAlpha: Double = js.native
  var lineWidth: Double = js.native
  var lineCap: String = js.native
  var lineJoin: String = js.native
  var font: String = js.native
  var textAlign: String = js.native
  var textBaseline: String = js.native
  var imageSmoothingEnabled: Boolean = js.native

  def save(): Unit = js.native
  def restore(): Unit = js.native
  def beginPath(): Unit = js.native
  def closePath(): Unit = js.native
  def moveTo(x: Double, y: Double): Unit = js.native
  def lineTo(x: Double, y: Double): Unit = js.native
  def rect(x: Double, y: Double, width: Double, height: Double): Unit = js.native
  def arc(x: Double, y: Double, radius: Double, startAngle: Double, endAngle: Double, counterclockwise: Boolean): Unit = js.native
  def fill(): Unit = js.native
  def stroke(): Unit = js.native
  def clip(): Unit = js.native
  def translate(x: Double, y: Double): Unit = js.native
  def rotate(angleRadians: Double): Unit = js.native
  def setLineDash(segments: js.Array[Double]): Unit = js.native
  def fillText(text: String, x: Double, y: Double): Unit = js.native
  def createImageData(width: Int, height: Int): CanvasImageData = js.native
  def putImageData(image: CanvasImageData, x: Double, y: Double): Unit = js.native
  def drawImage(image: CanvasImageSource, x: Double, y: Double, width: Double, height: Double): Unit = js.native

trait CanvasRasterFactory:
  def create(image: RasterImage, target: CanvasRenderingContext2D): CanvasImageSource

object CanvasRasterFactory:
  given browser: CanvasRasterFactory with
    override def create(image: RasterImage, target: CanvasRenderingContext2D): CanvasImageSource =
      val canvas = target.canvas.ownerDocument.createElement("canvas").asInstanceOf[CanvasElement]
      canvas.width = image.width
      canvas.height = image.height
      val imageContext = canvas.getContext("2d")
      val imageData = imageContext.createImageData(image.width, image.height)
      var idx = 0
      while idx < image.dimensions.pixelCount do
        val pixel = image.packedAt(idx)
        val offset = idx * 4
        imageData.data(offset) = pixel.red
        imageData.data(offset + 1) = pixel.green
        imageData.data(offset + 2) = pixel.blue
        imageData.data(offset + 3) = pixel.alpha
        idx += 1
      imageContext.putImageData(imageData, 0.0, 0.0)
      canvas

object CanvasRenderer:
  def compile(scene: Scene, options: CanvasOptions = CanvasOptions.default): Either[CanvasRenderError, CanvasProgram] =
    for
      device <- DeviceContext(options.width.toDouble, options.height.toDouble).left.map(CanvasRenderError.Graphics(_))
      resolved <- DeviceScene.fromScene(scene, device).left.map(CanvasRenderError.Graphics(_))
    yield CanvasProgram.fromDevice(resolved)

  def render(
      scene: Scene,
      context: CanvasRenderingContext2D,
      options: CanvasOptions = CanvasOptions.default
  )(using factory: CanvasRasterFactory): Either[CanvasRenderError, CanvasProgram] =
    compile(scene, options).map { program =>
      draw(program, context)
      program
    }

  def draw(program: CanvasProgram, context: CanvasRenderingContext2D)(using factory: CanvasRasterFactory): Unit =
    var openGroups = 0
    val images = mutable.HashMap.empty[RasterImage, CanvasImageSource]
    try
      program.commands.foreach {
        case command @ CanvasCommand.Save(_) =>
          execute(command, context, images)
          openGroups += 1
        case command @ CanvasCommand.Restore(_) =>
          execute(command, context, images)
          openGroups -= 1
        case command =>
          execute(command, context, images)
      }
    finally
      while openGroups > 0 do
        context.restore()
        openGroups -= 1

  private def execute(
      command: CanvasCommand,
      context: CanvasRenderingContext2D,
      images: mutable.Map[RasterImage, CanvasImageSource]
  )(using factory: CanvasRasterFactory): Unit =
    command match
      case CanvasCommand.Save(_) =>
        context.save()
      case CanvasCommand.Restore(_) =>
        context.restore()
      case CanvasCommand.Rotate(degrees, pivotX, pivotY) =>
        context.translate(pivotX, pivotY)
        context.rotate(degrees * math.Pi / 180.0)
        context.translate(-pivotX, -pivotY)
      case CanvasCommand.ClipRect(x, y, width, height) =>
        context.beginPath()
        context.rect(x, y, width, height)
        context.clip()
      case CanvasCommand.Disc(centerX, centerY, radius, paint, _) =>
        withSaved(context) {
          context.beginPath()
          context.arc(centerX, centerY, radius, 0.0, math.Pi * 2.0, false)
          paintPath(context, paint)
        }
      case CanvasCommand.Polyline(points, closed, paint, _) =>
        withSaved(context) {
          context.beginPath()
          context.moveTo(points.head.x, points.head.y)
          points.tail.foreach(point => context.lineTo(point.x, point.y))
          if closed then context.closePath()
          paintPath(context, paint)
        }
      case CanvasCommand.Rectangle(x, y, width, height, paint, _) =>
        withSaved(context) {
          context.beginPath()
          context.rect(x, y, width, height)
          paintPath(context, paint)
        }
      case CanvasCommand.Text(label, x, y, horizontal, vertical, rotation, fontSize, fontFamily, paint, _) =>
        withSaved(context) {
          val color = paint.fill.getOrElse(CanvasColor.fromRgba(Rgba.Black))
          context.fillStyle = color.css
          context.globalAlpha = paint.opacity * color.alpha
          context.font = s"${fontSize}px ${canvasFontFamily(fontFamily)}"
          context.textAlign = textAlign(horizontal)
          context.textBaseline = textBaseline(vertical)
          if rotation == 0.0 then context.fillText(label, x, y)
          else
            context.translate(x, y)
            context.rotate(rotation * math.Pi / 180.0)
            context.fillText(label, 0.0, 0.0)
        }
      case CanvasCommand.Image(image, x, y, width, height, interpolation, alpha, _) =>
        withSaved(context) {
          context.globalAlpha = alpha
          context.imageSmoothingEnabled = interpolation == RasterInterpolation.Smooth
          val source = images.getOrElseUpdate(image, factory.create(image, context))
          context.drawImage(source, x, y, width, height)
        }

  private def withSaved(context: CanvasRenderingContext2D)(body: => Unit): Unit =
    context.save()
    try body
    finally context.restore()

  private def paintPath(context: CanvasRenderingContext2D, paint: CanvasPaint): Unit =
    paint.fill.foreach { color =>
      context.fillStyle = color.css
      context.globalAlpha = paint.opacity * color.alpha
      context.fill()
    }
    paint.stroke.foreach { color =>
      context.strokeStyle = color.css
      context.globalAlpha = paint.opacity * color.alpha
      context.lineWidth = paint.lineWidth
      context.lineCap = canvasLineCap(paint.lineCap)
      context.lineJoin = canvasLineJoin(paint.lineJoin)
      val dash = paint.dash match
        case CanvasLineDash.Solid           => js.Array[Double]()
        case CanvasLineDash.Pattern(values) => js.Array(values*)
      context.setLineDash(dash)
      context.stroke()
    }

  private def canvasLineCap(value: LineCap): String =
    value match
      case LineCap.Butt   => "butt"
      case LineCap.Round  => "round"
      case LineCap.Square => "square"

  private def canvasLineJoin(value: LineJoin): String =
    value match
      case LineJoin.Miter => "miter"
      case LineJoin.Round => "round"
      case LineJoin.Bevel => "bevel"

  private def canvasFontFamily(value: Option[String]): String =
    value match
      case Some(family) => "\"" + family.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
      case None         => "sans-serif"

  private def textAlign(value: HJust): String =
    value match
      case HJust.Left   => "left"
      case HJust.Center => "center"
      case HJust.Right  => "right"

  private def textBaseline(value: VJust): String =
    value match
      case VJust.Bottom => "bottom"
      case VJust.Center => "middle"
      case VJust.Top    => "top"
