package scalafim.graphics

opaque type GraphicsName = String

object GraphicsName:
  def apply(value: String, kind: String = "graphics"): Either[GraphicsError, GraphicsName] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(GraphicsError.BlankName(kind))
    else Right(trimmed)

  def unsafe(value: String, kind: String = "graphics"): GraphicsName =
    apply(value, kind).orThrow

extension (name: GraphicsName)
  def value: String =
    name

enum Aesthetic[A](val label: String):
  case X extends Aesthetic[Double]("x")
  case Y extends Aesthetic[Double]("y")
  case Color extends Aesthetic[Rgba]("color")
  case Fill extends Aesthetic[Rgba]("fill")
  case Alpha extends Aesthetic[Double]("alpha")
  case Size extends Aesthetic[Double]("size")
  case Label extends Aesthetic[String]("label")
  case Group extends Aesthetic[String]("group")

enum PointShape:
  case Circle
  case Square
  case Triangle
  case Cross

enum LineType:
  case Solid
  case Dashed
  case Dotted

enum Clip:
  case On
  case Off
