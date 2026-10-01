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

/** Named cortical surface realizations, plus an escape hatch for unnamed ones.
  *
  * `label` is the canonical serialized spelling used by saved identities (scene
  * documents, domain keys). `VeryInflated` is a display realization like
  * `Inflated`: its canonical label is the TemplateFlow/BIDS spelling
  * `veryinflated`; the GIFTI `GeometricType` spelling `VeryInflated` and the HCP
  * filename spelling `very_inflated` parse to the same case.
  *
  * `Custom` is reserved for kinds without a named case. Always decode through
  * [[SurfaceKind.fromString]] so a spelling of a named kind can never become a
  * parallel `Custom` identity.
  */
enum SurfaceKind:
  case White, Pial, Inflated, VeryInflated, Sphere, SmoothWm, Midthickness
  case Custom(value: String)

  def label: String =
    this match
      case White => "white"
      case Pial => "pial"
      case Inflated => "inflated"
      case VeryInflated => "veryinflated"
      case Sphere => "sphere"
      case SmoothWm => "smoothwm"
      case Midthickness => "midthickness"
      case Custom(value) => value

object SurfaceKind:
  /** The named kind spelled by `value` (case-insensitive, trimmed), if any. */
  def named(value: String): Option[SurfaceKind] =
    value.trim.toLowerCase match
      case "white" => Some(White)
      case "pial" => Some(Pial)
      case "inflated" => Some(Inflated)
      case "veryinflated" | "very-inflated" | "very_inflated" => Some(VeryInflated)
      case "sphere" | "spherical" => Some(Sphere)
      case "smoothwm" | "smooth-wm" | "smooth_wm" => Some(SmoothWm)
      case "midthickness" | "mid-thickness" | "mid_thickness" => Some(Midthickness)
      case _ => None

  def fromString(value: String): SurfaceKind =
    val label = value.trim
    named(label).getOrElse:
      require(label.nonEmpty, "surface kind label must be non-empty")
      Custom(label)

  /** Infer a kind from a surface file name (FreeSurfer, HCP or TemplateFlow style).
    *
    * The base name is split on `.`, `_` and `-`; the right-most token naming a kind
    * wins, ignoring trailing digits. The adjacent token pair `very`, `inflated`
    * (HCP `very_inflated`) names `VeryInflated`, never `Inflated`. Names without a
    * kind token fall back to `Custom` of their last token.
    */
  def fromFileName(fileName: String): SurfaceKind =
    val base = fileName
      .stripSuffix(".asc")
      .stripSuffix(".surf.gii")
      .stripSuffix(".gii")
    val tokens = base.split("[._-]").toVector.map(_.trim).filter(_.nonEmpty)
    val keys = tokens.map(_.toLowerCase.replaceAll("\\d+$", ""))
    keys.indices.reverseIterator
      .flatMap: i =>
        if keys(i) == "inflated" && i > 0 && keys(i - 1) == "very" then Some(VeryInflated)
        else named(keys(i))
      .nextOption()
      .getOrElse(Custom(tokens.lastOption.getOrElse("surface")))
