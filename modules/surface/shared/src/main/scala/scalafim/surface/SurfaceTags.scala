package scalafim.surface

enum CorticalHemisphere:
  case Left, Right

  def tag: Hemisphere =
    this match
      case Left => Hemisphere.Left
      case Right => Hemisphere.Right

  def code: String =
    this match
      case Left => "lh"
      case Right => "rh"

object CorticalHemisphere:
  def fromTag(tag: Hemisphere): Either[SurfaceError, CorticalHemisphere] =
    tag match
      case Hemisphere.Left => scala.util.Right(Left)
      case Hemisphere.Right => scala.util.Right(Right)
      case other => scala.util.Left(SurfaceError.InvalidHemisphereTag(other))

  def fromStringEither(value: String): Either[SurfaceError, CorticalHemisphere] =
    Hemisphere.fromStringEither(value).flatMap(fromTag)

  def fromString(value: String): CorticalHemisphere =
    fromStringEither(value).fold(error => throw new IllegalArgumentException(error.message), identity)

enum Hemisphere:
  case Left, Right, Both, Unknown

  def code: String =
    this match
      case Left => "lh"
      case Right => "rh"
      case Both => "both"
      case Unknown => "unknown"

  def toCortical: Either[SurfaceError, CorticalHemisphere] =
    CorticalHemisphere.fromTag(this)

  def isCortical: Boolean =
    this == Left || this == Right

object Hemisphere:
  def fromStringEither(value: String): Either[SurfaceError, Hemisphere] =
    value.trim.toLowerCase match
      case "l" | "lh" | "left" => scala.util.Right(Left)
      case "r" | "rh" | "right" => scala.util.Right(Right)
      case "both" | "bilateral" => scala.util.Right(Both)
      case "" | "unknown" => scala.util.Right(Unknown)
      case other => scala.util.Left(SurfaceError.ReadFailure(value, s"unknown hemisphere: $other"))

  def fromString(value: String): Hemisphere =
    fromStringEither(value).fold(error => throw new IllegalArgumentException(error.message), identity)

enum SurfaceKind:
  case White, Pial, Inflated, Sphere, SmoothWm, Midthickness
  case Custom(value: String)

  def label: String =
    this match
      case White => "white"
      case Pial => "pial"
      case Inflated => "inflated"
      case Sphere => "sphere"
      case SmoothWm => "smoothwm"
      case Midthickness => "midthickness"
      case Custom(value) => value

object SurfaceKind:
  def fromString(value: String): SurfaceKind =
    val label = value.trim
    label.toLowerCase match
      case "white" => White
      case "pial" => Pial
      case "inflated" => Inflated
      case "sphere" | "spherical" => Sphere
      case "smoothwm" | "smooth-wm" | "smooth_wm" => SmoothWm
      case "midthickness" | "mid-thickness" | "mid_thickness" => Midthickness
      case other =>
        require(other.nonEmpty, "surface kind label must be non-empty")
        Custom(label)
