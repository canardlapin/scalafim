package scalafim.surface

enum Hemisphere:
  case Left, Right, Both, Unknown

  def code: String =
    this match
      case Left => "lh"
      case Right => "rh"
      case Both => "both"
      case Unknown => "unknown"

object Hemisphere:
  def fromString(value: String): Hemisphere =
    value.trim.toLowerCase match
      case "l" | "lh" | "left" => Left
      case "r" | "rh" | "right" => Right
      case "both" | "bilateral" => Both
      case "" | "unknown" => Unknown
      case other => throw new IllegalArgumentException(s"unknown hemisphere: $other")

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
