package scalafim.surface.reference

enum EvidenceAxis:
  case X, Y, Z

/** A candidate placement of the declared anatomy in the target frame. */
enum Placement:
  /** Declared frame, carried into the target frame by the route's bridge. */
  case Bridged
  /** Anatomy coordinates used unchanged, as if already in the target frame. */
  case Raw
  /** The bridge applied in the opposite direction. */
  case Reversed
  /** The bridged placement translated by `millimetres` along `axis`. */
  case Shifted(axis: EvidenceAxis, millimetres: Double)

  def name: String =
    this match
      case Bridged => "bridged"
      case Raw => "raw"
      case Reversed => "reversed"
      case Shifted(axis, mm) =>
        // Platform-independent decimal text (Double.toString differs between JVM and Scala.js).
        val text = java.math.BigDecimal.valueOf(mm).stripTrailingZeros.toPlainString
        s"shift-${axis.toString.toLowerCase}${if mm >= 0.0 then "+" else ""}${text}mm"

/** Grey-matter probability sampled at the cortical vertices of one placement. */
final case class PlacementScore private (
  placement: Placement,
  meanGreyMatterProbability: Double,
  fractionAboveHalf: Double,
  scoredVertices: Int,
  excludedVertices: Int
)

object PlacementScore:
  def make(placement: Placement, meanGreyMatterProbability: Double, fractionAboveHalf: Double, scoredVertices: Int,
      excludedVertices: Int): Either[ReferenceError, PlacementScore] =
    def unit(value: Double) = value.isFinite && value >= 0.0 && value <= 1.0
    if !unit(meanGreyMatterProbability) then
      Left(ReferenceError.InvalidEvidence(s"${placement.name}: mean GM probability must lie in [0, 1]"))
    else if !unit(fractionAboveHalf) then
      Left(ReferenceError.InvalidEvidence(s"${placement.name}: fraction above 0.5 must lie in [0, 1]"))
    else if scoredVertices <= 0 || excludedVertices < 0 then
      Left(ReferenceError.InvalidEvidence(s"${placement.name}: need scored > 0 and excluded >= 0 vertices"))
    else Right(PlacementScore(placement, meanGreyMatterProbability, fractionAboveHalf, scoredVertices, excludedVertices))

/** Margins declared before any score is seen. */
final case class FrameEvidenceThresholds private (minimumGainOverRaw: Double, minimumGainOverReversed: Double,
    shiftMillimetres: Double):
  /** Placements every receipt must score. */
  def requiredPlacements: Vector[Placement] =
    Vector(Placement.Bridged, Placement.Raw, Placement.Reversed) ++
      (for axis <- EvidenceAxis.values.toVector; sign <- Vector(-1.0, 1.0)
       yield Placement.Shifted(axis, sign * shiftMillimetres))

object FrameEvidenceThresholds:
  /** The WS2 plan's budgets: +0.03 over raw, +0.05 over reversed, every ±3 mm shift lower. */
  val Default: FrameEvidenceThresholds = FrameEvidenceThresholds(0.03, 0.05, 3.0)

  def make(minimumGainOverRaw: Double, minimumGainOverReversed: Double,
      shiftMillimetres: Double): Either[ReferenceError, FrameEvidenceThresholds] =
    if !(minimumGainOverRaw.isFinite && minimumGainOverRaw >= 0.0 && minimumGainOverReversed.isFinite &&
        minimumGainOverReversed >= 0.0) then
      Left(ReferenceError.InvalidEvidence("score margins must be finite and non-negative"))
    else if !(shiftMillimetres.isFinite && shiftMillimetres > 0.0) then
      Left(ReferenceError.InvalidEvidence("shift distance must be finite and positive"))
    else Right(FrameEvidenceThresholds(minimumGainOverRaw, minimumGainOverReversed, shiftMillimetres))

enum FrameEvidenceFailure:
  case MissingPlacement(placement: Placement)
  case VertexPopulationDiffers(placement: Placement, expected: Int, actual: Int)
  case InsufficientGainOverRaw(gain: Double, required: Double)
  case InsufficientGainOverReversed(gain: Double, required: Double)
  case ShiftNotWorse(placement: Placement, shifted: Double, bridged: Double)

  def message: String =
    this match
      case MissingPlacement(p) => s"no score for placement ${p.name}"
      case VertexPopulationDiffers(p, expected, actual) =>
        s"${p.name} scores $actual cortical vertices (scored + excluded); bridged scores $expected"
      case InsufficientGainOverRaw(gain, required) => s"bridged beats raw by $gain; required >= $required"
      case InsufficientGainOverReversed(gain, required) => s"bridged beats reversed by $gain; required >= $required"
      case ShiftNotWorse(p, shifted, bridged) => s"${p.name} scores $shifted, not below bridged $bridged"

enum FrameEvidenceVerdict:
  case Pass
  case Fail(failures: Vector[FrameEvidenceFailure])

/** Disconfirmation receipt for a frame declaration: grey-matter probability,
  * from a map in the target frame, sampled at the declared anatomy under
  * competing placements. The declarations are the assets scored together
  * (e.g. both hemispheres of a mesh) and must declare one frame. Passing does
  * not prove the declaration; failing refutes it at the declared margins.
  */
final case class FrameEvidence private (
  declarations: Vector[FrameDeclaration],
  probabilityMap: AssetProvenance,
  thresholds: FrameEvidenceThresholds,
  scores: Vector[PlacementScore]
):
  def scoreOf(placement: Placement): Option[PlacementScore] = scores.find(_.placement == placement)

  /** Total: every receipt yields Pass or the complete list of failures. */
  def verdict: FrameEvidenceVerdict =
    val missing = thresholds.requiredPlacements.filterNot(p => scores.exists(_.placement == p))
      .map(FrameEvidenceFailure.MissingPlacement.apply)
    val judged = scoreOf(Placement.Bridged).fold(Vector.empty[FrameEvidenceFailure]): bridged =>
      val population = bridged.scoredVertices + bridged.excludedVertices
      val populations = scores.collect:
        case s if s.scoredVertices + s.excludedVertices != population =>
          FrameEvidenceFailure.VertexPopulationDiffers(s.placement, population, s.scoredVertices + s.excludedVertices)
      val mean = bridged.meanGreyMatterProbability
      val raw = scoreOf(Placement.Raw).map(_.meanGreyMatterProbability).collect:
        case r if mean - r < thresholds.minimumGainOverRaw =>
          FrameEvidenceFailure.InsufficientGainOverRaw(mean - r, thresholds.minimumGainOverRaw)
      val reversed = scoreOf(Placement.Reversed).map(_.meanGreyMatterProbability).collect:
        case r if mean - r < thresholds.minimumGainOverReversed =>
          FrameEvidenceFailure.InsufficientGainOverReversed(mean - r, thresholds.minimumGainOverReversed)
      val shifts = scores.collect:
        case PlacementScore(p: Placement.Shifted, shifted, _, _, _) if shifted >= mean =>
          FrameEvidenceFailure.ShiftNotWorse(p, shifted, mean)
      populations ++ raw ++ reversed ++ shifts
    val failures = missing ++ judged
    if failures.isEmpty then FrameEvidenceVerdict.Pass else FrameEvidenceVerdict.Fail(failures)

object FrameEvidence:
  def make(declarations: Vector[FrameDeclaration], probabilityMap: AssetProvenance, thresholds: FrameEvidenceThresholds,
      scores: Vector[PlacementScore]): Either[ReferenceError, FrameEvidence] =
    val duplicated = scores.groupBy(_.placement).collect { case (p, group) if group.size > 1 => p.name }
    if declarations.isEmpty then Left(ReferenceError.InvalidEvidence("at least one declaration is required"))
    else if declarations.map(_.frame).distinct.size != 1 then
      Left(ReferenceError.InvalidEvidence("scored declarations must declare one frame"))
    else if duplicated.nonEmpty then Left(ReferenceError.InvalidEvidence(s"duplicate placements: ${duplicated.mkString(", ")}"))
    else Right(FrameEvidence(declarations, probabilityMap, thresholds, scores))
