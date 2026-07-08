package scalafim.graphics

enum LengthUnit:
  case Npc
  case Native
  case Cm
  case Mm
  case Inch
  case Point
  case Line

final case class Length private (value: Double, unit: LengthUnit):
  require(value.isFinite, "`value` must be finite")

object Length:
  def apply(value: Double, unit: LengthUnit): Either[GraphicsError, Length] =
    if value.isFinite then Right(new Length(value, unit))
    else Left(GraphicsError.InvalidLength(value))

  def unsafe(value: Double, unit: LengthUnit): Length =
    apply(value, unit).orThrow

  def npc(value: Double): Either[GraphicsError, Length] =
    apply(value, LengthUnit.Npc)

  def npcUnsafe(value: Double): Length =
    npc(value).orThrow

  def native(value: Double): Either[GraphicsError, Length] =
    apply(value, LengthUnit.Native)

  def nativeUnsafe(value: Double): Length =
    native(value).orThrow

  def points(value: Double): Either[GraphicsError, Length] =
    apply(value, LengthUnit.Point)

  def pointsUnsafe(value: Double): Length =
    points(value).orThrow

sealed trait LengthExpr:
  def +(that: LengthExpr): LengthExpr =
    LengthExpr.Add(this, that)

  def -(that: LengthExpr): LengthExpr =
    LengthExpr.Sub(this, that)

  def times(factor: Double): Either[GraphicsError, LengthExpr] =
    if factor.isFinite then Right(LengthExpr.Mul(factor, this))
    else Left(GraphicsError.InvalidLength(factor))

object LengthExpr:
  final case class Const(length: Length) extends LengthExpr
  final case class Add private[graphics] (left: LengthExpr, right: LengthExpr) extends LengthExpr
  final case class Sub private[graphics] (left: LengthExpr, right: LengthExpr) extends LengthExpr
  final case class Mul private[graphics] (factor: Double, value: LengthExpr) extends LengthExpr:
    require(factor.isFinite, "`factor` must be finite")

  def apply(length: Length): LengthExpr =
    Const(length)

  def npc(value: Double): Either[GraphicsError, LengthExpr] =
    Length.npc(value).map(Const(_))

  def npcUnsafe(value: Double): LengthExpr =
    npc(value).orThrow

  def native(value: Double): Either[GraphicsError, LengthExpr] =
    Length.native(value).map(Const(_))

  def nativeUnsafe(value: Double): LengthExpr =
    native(value).orThrow

final case class Point(x: LengthExpr, y: LengthExpr)

object Point:
  def npc(x: Double, y: Double): Either[GraphicsError, Point] =
    for
      px <- LengthExpr.npc(x)
      py <- LengthExpr.npc(y)
    yield Point(px, py)

  def npcUnsafe(x: Double, y: Double): Point =
    npc(x, y).orThrow

  def native(x: Double, y: Double): Either[GraphicsError, Point] =
    for
      px <- LengthExpr.native(x)
      py <- LengthExpr.native(y)
    yield Point(px, py)

  def nativeUnsafe(x: Double, y: Double): Point =
    native(x, y).orThrow

final case class Size(width: LengthExpr, height: LengthExpr)

object Size:
  def npc(width: Double, height: Double): Either[GraphicsError, Size] =
    for
      w <- LengthExpr.npc(width)
      h <- LengthExpr.npc(height)
    yield Size(w, h)

  def npcUnsafe(width: Double, height: Double): Size =
    npc(width, height).orThrow

enum HJust:
  case Left
  case Center
  case Right

enum VJust:
  case Bottom
  case Center
  case Top

final case class Anchor(horizontal: HJust, vertical: VJust)

object Anchor:
  val Center: Anchor =
    Anchor(HJust.Center, VJust.Center)

  val BottomLeft: Anchor =
    Anchor(HJust.Left, VJust.Bottom)

final case class Rgba private (red: Int, green: Int, blue: Int, alpha: Double):
  require(red >= 0 && red <= 255, "`red` must be in [0, 255]")
  require(green >= 0 && green <= 255, "`green` must be in [0, 255]")
  require(blue >= 0 && blue <= 255, "`blue` must be in [0, 255]")
  require(alpha.isFinite && alpha >= 0.0 && alpha <= 1.0, "`alpha` must be in [0, 1]")

  def withAlpha(value: Double): Either[GraphicsError, Rgba] =
    Rgba(red, green, blue, value)

object Rgba:
  def apply(red: Int, green: Int, blue: Int, alpha: Double = 1.0): Either[GraphicsError, Rgba] =
    if red < 0 || red > 255 then Left(GraphicsError.InvalidColorChannel("red", red))
    else if green < 0 || green > 255 then Left(GraphicsError.InvalidColorChannel("green", green))
    else if blue < 0 || blue > 255 then Left(GraphicsError.InvalidColorChannel("blue", blue))
    else if !alpha.isFinite || alpha < 0.0 || alpha > 1.0 then Left(GraphicsError.InvalidAlpha(alpha))
    else Right(new Rgba(red, green, blue, alpha))

  def unsafe(red: Int, green: Int, blue: Int, alpha: Double = 1.0): Rgba =
    apply(red, green, blue, alpha).orThrow

  val Black: Rgba =
    unsafe(0, 0, 0)

  val White: Rgba =
    unsafe(255, 255, 255)

  val Transparent: Rgba =
    unsafe(0, 0, 0, 0.0)

final case class GraphicParams(
    stroke: Option[Rgba] = Some(Rgba.Black),
    fill: Option[Rgba] = None,
    lineWidth: Double = 1.0,
    lineType: LineType = LineType.Solid,
    alpha: Double = 1.0,
    fontFamily: Option[String] = None,
    fontSize: Length = Length.pointsUnsafe(12.0)
):
  require(lineWidth.isFinite && lineWidth >= 0.0, "`lineWidth` must be finite and >= 0")
  require(alpha.isFinite && alpha >= 0.0 && alpha <= 1.0, "`alpha` must be in [0, 1]")

object GraphicParams:
  def checked(
      stroke: Option[Rgba] = Some(Rgba.Black),
      fill: Option[Rgba] = None,
      lineWidth: Double = 1.0,
      lineType: LineType = LineType.Solid,
      alpha: Double = 1.0,
      fontFamily: Option[String] = None,
      fontSize: Length = Length.pointsUnsafe(12.0)
  ): Either[GraphicsError, GraphicParams] =
    if !lineWidth.isFinite || lineWidth < 0.0 then Left(GraphicsError.InvalidLineWidth(lineWidth))
    else if !alpha.isFinite || alpha < 0.0 || alpha > 1.0 then Left(GraphicsError.InvalidAlpha(alpha))
    else Right(GraphicParams(stroke, fill, lineWidth, lineType, alpha, fontFamily, fontSize))

  def unsafe(
      stroke: Option[Rgba] = Some(Rgba.Black),
      fill: Option[Rgba] = None,
      lineWidth: Double = 1.0,
      lineType: LineType = LineType.Solid,
      alpha: Double = 1.0,
      fontFamily: Option[String] = None,
      fontSize: Length = Length.pointsUnsafe(12.0)
  ): GraphicParams =
    checked(stroke, fill, lineWidth, lineType, alpha, fontFamily, fontSize).orThrow

final case class Viewport(
    origin: Point = Point.npcUnsafe(0.0, 0.0),
    size: Size = Size.npcUnsafe(1.0, 1.0),
    xScale: Interval = Interval.unsafe(0.0, 1.0),
    yScale: Interval = Interval.unsafe(0.0, 1.0),
    clip: Clip = Clip.On,
    angleDegrees: Double = 0.0
):
  require(angleDegrees.isFinite, "`angleDegrees` must be finite")

sealed trait Grob:
  def name: Option[GraphicsName]
  def gp: GraphicParams
  def viewport: Option[Viewport]
  def children: Vector[Grob] =
    Vector.empty

object Grob:
  final case class Points private[graphics] (
      points: Vector[Point],
      size: LengthExpr,
      shape: PointShape,
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob:
    require(points.nonEmpty, "`points` must be non-empty")

  final case class Lines private[graphics] (
      points: Vector[Point],
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob:
    require(points.nonEmpty, "`points` must be non-empty")

  final case class Segments private[graphics] (
      segments: Vector[(Point, Point)],
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob:
    require(segments.nonEmpty, "`segments` must be non-empty")

  final case class Rect private[graphics] (
      center: Point,
      size: Size,
      anchor: Anchor,
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob

  final case class Circle private[graphics] (
      center: Point,
      radius: LengthExpr,
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob

  final case class Text private[graphics] (
      label: String,
      at: Point,
      anchor: Anchor,
      rotationDegrees: Double,
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob:
    require(rotationDegrees.isFinite, "`rotationDegrees` must be finite")

  final case class Group private[graphics] (
      override val children: Vector[Grob],
      gp: GraphicParams,
      viewport: Option[Viewport],
      name: Option[GraphicsName]
  ) extends Grob

  def points(
      points: Vector[Point],
      size: LengthExpr = LengthExpr(Length.pointsUnsafe(4.0)),
      shape: PointShape = PointShape.Circle,
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Either[GraphicsError, Grob] =
    if points.isEmpty then Left(GraphicsError.EmptyGeometry("points"))
    else Right(Points(points, size, shape, gp, viewport, name))

  def lines(
      points: Vector[Point],
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Either[GraphicsError, Grob] =
    if points.isEmpty then Left(GraphicsError.EmptyGeometry("lines"))
    else Right(Lines(points, gp, viewport, name))

  def segments(
      segments: Vector[(Point, Point)],
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Either[GraphicsError, Grob] =
    if segments.isEmpty then Left(GraphicsError.EmptyGeometry("segments"))
    else Right(Segments(segments, gp, viewport, name))

  def rect(
      center: Point,
      size: Size,
      anchor: Anchor = Anchor.Center,
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Grob =
    Rect(center, size, anchor, gp, viewport, name)

  def circle(
      center: Point,
      radius: LengthExpr,
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Grob =
    Circle(center, radius, gp, viewport, name)

  def text(
      label: String,
      at: Point,
      anchor: Anchor = Anchor.Center,
      rotationDegrees: Double = 0.0,
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Grob =
    Text(label, at, anchor, rotationDegrees, gp, viewport, name)

  def group(
      children: Vector[Grob],
      gp: GraphicParams = GraphicParams.unsafe(),
      viewport: Option[Viewport] = None,
      name: Option[GraphicsName] = None
  ): Grob =
    Group(children, gp, viewport, name)

final case class Scene private (grobs: Vector[Grob]):
  def append(grob: Grob): Scene =
    copy(grobs = grobs :+ grob)

  def ++(that: Scene): Scene =
    Scene(grobs ++ that.grobs)

  def isEmpty: Boolean =
    grobs.isEmpty

  def size: Int =
    grobs.length

object Scene:
  val empty: Scene =
    Scene(Vector.empty)

  def apply(grobs: Vector[Grob]): Scene =
    new Scene(grobs)
