package scalafim.surface.reference

import image4s.geometry.{Affine, D3, Frame, Grid}
import scalafim.image.{SomeMaskVolume, SomeSampleSpace}
import scalafim.image.SampleSpaces

/** Failures admitting a spatial reference. Admission never guesses: an
  * ambiguous or incomplete reference is refused rather than normalized.
  */
enum ReferenceError:
  case InvalidTemplateId(value: String, reason: String)
  case AmbiguousTemplate(value: String)
  case InvalidRelease(value: String)
  case InvalidCohort(value: String)
  case InvalidVolumeGrid(reason: String)
  case SupportGridMismatch
  case InvalidCorticalMesh(reason: String)
  case InvalidMedialWall(reason: String)
  case InvalidAnatomy(reason: String)
  case InvalidDisplay(reason: String)
  case InvalidBridge(reason: String)
  case InvalidProvenance(reason: String)
  case InvalidFrameBasis(reason: String)
  case DigestMismatch(archivePath: String, declared: String, actual: String)
  case DeclarationConflict(reason: String)
  case AssetReadFailure(reason: String)
  case InvalidEvidence(reason: String)

  def message: String =
    this match
      case InvalidTemplateId(value, reason) => s"invalid template identifier '$value': $reason"
      case AmbiguousTemplate(value) =>
        s"template '$value' names a family, not an exact variant; declare e.g. MNI152NLin2009cAsym or MNI152NLin6Asym"
      case InvalidRelease(value) => s"template release must be a non-blank exact version or digest; got '$value'"
      case InvalidCohort(value) => s"template cohort must be non-blank when declared; got '$value'"
      case InvalidVolumeGrid(reason) => s"invalid volume reference grid: $reason"
      case SupportGridMismatch => "analysis support mask does not share the volume reference grid"
      case InvalidCorticalMesh(reason) => s"invalid cortical mesh reference: $reason"
      case InvalidMedialWall(reason) => s"invalid medial-wall mask: $reason"
      case InvalidAnatomy(reason) => s"invalid sampling anatomy: $reason"
      case InvalidDisplay(reason) => s"invalid display surface: $reason"
      case InvalidBridge(reason) => s"invalid frame bridge: $reason"
      case InvalidProvenance(reason) => s"invalid asset provenance: $reason"
      case InvalidFrameBasis(reason) => s"invalid frame basis: $reason"
      case DigestMismatch(path, declared, actual) => s"$path has sha256 $actual; its declaration requires $declared"
      case DeclarationConflict(reason) => s"surface file contradicts the requested reading: $reason"
      case AssetReadFailure(reason) => s"declared asset could not be read: $reason"
      case InvalidEvidence(reason) => s"invalid frame evidence: $reason"

/** Exact TemplateFlow-style template identifier such as `MNI152NLin2009cAsym`.
  * Family names (`MNI`, `MNI152`, `ICBM152`, ...) are refused: they do not
  * identify a coordinate frame.
  */
opaque type TemplateId = String

object TemplateId:
  private val ambiguous = Set("mni", "mni152", "mnispace", "mni152space", "icbm", "icbm152", "standard",
    "template", "tal", "talairach")

  /** MNI/ICBM-family names are admitted only as exact TemplateFlow variants. */
  private val mniVariants = Set("MNI152Lin", "MNI152NLin6Sym", "MNI152NLin6Asym", "MNI152NLin2009aSym",
    "MNI152NLin2009aAsym", "MNI152NLin2009bSym", "MNI152NLin2009bAsym", "MNI152NLin2009cSym", "MNI152NLin2009cAsym",
    "MNI305", "MNIColin27", "MNIInfant", "MNIPediatricAsym")

  def make(value: String): Either[ReferenceError, TemplateId] =
    val lower = value.toLowerCase
    if value.isEmpty then Left(ReferenceError.InvalidTemplateId(value, "empty"))
    else if !value.head.isLetter || !value.forall(_.isLetterOrDigit) then
      Left(ReferenceError.InvalidTemplateId(value, "expected an alphanumeric TemplateFlow identifier"))
    else if ambiguous.contains(lower) then Left(ReferenceError.AmbiguousTemplate(value))
    else if (lower.startsWith("mni") || lower.startsWith("icbm")) && !mniVariants.contains(value) then
      Left(ReferenceError.AmbiguousTemplate(value))
    else Right(value)

  def unsafe(value: String): TemplateId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: TemplateId)
    def value: String = id

/** Exact version (release tag or content digest) of the template asset that
  * defines a coordinate frame.
  */
opaque type TemplateRelease = String

object TemplateRelease:
  def make(value: String): Either[ReferenceError, TemplateRelease] =
    if value.trim.isEmpty || value.trim != value then Left(ReferenceError.InvalidRelease(value)) else Right(value)

  def unsafe(value: String): TemplateRelease =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (release: TemplateRelease)
    def value: String = release

/** A world coordinate frame defined by an exact template variant, optional
  * cohort and release. Frames are compared exactly; there are no aliases.
  */
final case class TemplateFrame private (template: TemplateId, cohort: Option[String], release: TemplateRelease):
  def display: String =
    s"${template.value}${cohort.fold("")(c => s"[cohort-$c]")}@${release.value}"

object TemplateFrame:
  def make(template: TemplateId, release: TemplateRelease, cohort: Option[String] = None): Either[ReferenceError, TemplateFrame] =
    cohort match
      case Some(value) if value.trim.isEmpty || value.trim != value => Left(ReferenceError.InvalidCohort(value))
      case _ => Right(TemplateFrame(template, cohort, release))

  def unsafe(template: String, release: String, cohort: Option[String] = None): TemplateFrame =
    TemplateId.make(template)
      .flatMap(id => TemplateRelease.make(release).flatMap(r => make(id, r, cohort)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

/** The declared reference of a group-analysis volume: its template frame, its
  * exact voxel grid (dims and voxel-to-world affine in millimetres, RAS) and the
  * optional analysis support outside which values are not interpreted.
  *
  * The grid is the admitted image4s grid itself, so its identity carries the
  * grid's frame and persistent key rather than a detached copy of dims and
  * affine. Finite, invertible, homogeneous affines are guaranteed by `Affine`.
  */
final case class VolumeReference private (
  frame: TemplateFrame,
  grid: Grid[? <: Frame[D3], D3],
  support: Option[SomeMaskVolume]
):
  def dims: Vector[Int] = grid.shape

  def voxelToWorld: Affine[D3] = grid.indexToFrame

  /** Exact grid identity: the same live grid, or a grid with the same persistent
    * key (frame, spatial dims and bitwise-equal voxel-to-world affine).
    */
  def sharesGrid(other: Grid[?, ?]): Boolean =
    grid.sameRuntimeOwnerAs(other) || grid.samePersistentKeyAs(other)

object VolumeReference:
  def make(frame: TemplateFrame, space: SomeSampleSpace, support: Option[SomeMaskVolume] = None): Either[ReferenceError, VolumeReference] =
    SampleSpaces.requireVolumeD3(space).left.map(error => ReferenceError.InvalidVolumeGrid(error.message)).flatMap: spatial =>
      val reference = VolumeReference(frame, spatial.grid, support)
      support match
        case Some(mask) if !reference.sharesGrid(mask.grid) => Left(ReferenceError.SupportGridMismatch)
        case _ => Right(reference)
