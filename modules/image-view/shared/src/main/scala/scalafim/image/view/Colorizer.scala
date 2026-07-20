package scalafim.image.view

import scalafim.graphics.*

enum ColorizerError:
  case InvalidWindow(lower: Double, upper: Double)

  def message: String =
    this match
      case InvalidWindow(lower, upper) =>
        s"display window must have finite lower < upper; got [$lower, $upper]"

final case class DisplayWindow private (lower: Double, upper: Double):
  def width: Double =
    upper - lower

  def normalize(value: Double): Double =
    math.max(0.0, math.min(1.0, (value - lower) / width))

object DisplayWindow:
  def make(lower: Double, upper: Double): Either[ColorizerError, DisplayWindow] =
    if lower.isFinite && upper.isFinite && lower < upper then Right(new DisplayWindow(lower, upper))
    else Left(ColorizerError.InvalidWindow(lower, upper))

  def unsafe(lower: Double, upper: Double): DisplayWindow =
    make(lower, upper).fold(err => throw new IllegalArgumentException(err.message), identity)

trait Colorizer[A]:
  def color(value: A): Rgba32

  def supportsWindow: Boolean =
    false

  def withWindow(window: DisplayWindow): Option[Colorizer[A]] =
    None

final case class ColorRamp(low: Rgba32, high: Rgba32):
  def colorAt(fraction: Double): Rgba32 =
    val t = math.max(0.0, math.min(1.0, fraction))
    def channel(from: Int, to: Int): Int =
      math.round(from + (to - from) * t).toInt
    Rgba32.unsafe(
      channel(low.red, high.red),
      channel(low.green, high.green),
      channel(low.blue, high.blue),
      channel(low.alpha, high.alpha)
    )

object ColorRamp:
  val Grayscale: ColorRamp =
    ColorRamp(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(255, 255, 255))

  val Heat: ColorRamp =
    ColorRamp(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(255, 96, 0))

final case class ScalarColorizer(
  window: DisplayWindow,
  ramp: ColorRamp = ColorRamp.Grayscale,
  invalid: Rgba32 = Rgba32.unsafe(0, 0, 0, 0)
) extends Colorizer[Double]:
  def color(value: Double): Rgba32 =
    if value.isFinite then ramp.colorAt(window.normalize(value)) else invalid

  override def supportsWindow: Boolean =
    true

  override def withWindow(value: DisplayWindow): Option[Colorizer[Double]] =
    Some(copy(window = value))

final case class LabelColorizer(
  colors: Map[Int, Rgba32],
  fallback: Rgba32 = Rgba32.unsafe(0, 0, 0, 0)
) extends Colorizer[Int]:
  def color(value: Int): Rgba32 =
    colors.getOrElse(value, fallback)

final case class MaskColorizer(
  foreground: Rgba32,
  background: Rgba32 = Rgba32.unsafe(0, 0, 0, 0)
) extends Colorizer[Boolean]:
  def color(value: Boolean): Rgba32 =
    if value then foreground else background

object Colorizer:
  def constant[A](pixel: Rgba32): Colorizer[A] =
    new Colorizer[A]:
      def color(value: A): Rgba32 = pixel
