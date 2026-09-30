package scalafim.fmri.threshold

import scalafim.image.{SomeMaskVolume, SomeScalarVolume}

/** Threshold methods with an implemented decision path. Methods are added
  * here only together with their implementation.
  */
enum ThresholdMethod:
  case HierScan, MaxT

sealed trait ThresholdCutoff:
  def toLegacyDouble: Double

  /** Apply this cutoff to one finite score without erasing its comparison
    * semantics.
    */
  def rejects(score: Double): Either[ThresholdError, Boolean]

object ThresholdCutoff:
  final case class Inclusive private[ThresholdCutoff] (value: Double) extends ThresholdCutoff:
    require(value.isFinite, "inclusive threshold cutoff must be finite")

    override def toLegacyDouble: Double =
      value

    override def rejects(score: Double): Either[ThresholdError, Boolean] =
      finiteDecision(score, score >= value)

  /** A finite boundary that is not itself rejected.
    *
    * The legacy inclusive-double representation is the next representable
    * value. This preserves `score > value` exactly for finite `Double` scores.
    */
  final case class Exclusive private[ThresholdCutoff] (value: Double) extends ThresholdCutoff:
    require(value.isFinite, "exclusive threshold cutoff must be finite")

    override def toLegacyDouble: Double =
      Math.nextUp(value)

    override def rejects(score: Double): Either[ThresholdError, Boolean] =
      finiteDecision(score, score > value)

  case object NoRejections extends ThresholdCutoff:
    override def toLegacyDouble: Double =
      Double.PositiveInfinity

    override def rejects(score: Double): Either[ThresholdError, Boolean] =
      finiteDecision(score, false)

  def inclusive(value: Double): Either[ThresholdError, ThresholdCutoff] =
    if value.isFinite then Right(Inclusive(value))
    else Left(ThresholdError.NonFiniteData("threshold cutoff"))

  def exclusive(value: Double): Either[ThresholdError, ThresholdCutoff] =
    if value.isFinite then Right(Exclusive(value))
    else Left(ThresholdError.NonFiniteData("threshold cutoff"))

  /** Import a legacy threshold whose consumer convention is `score >= value`.
    * A finite number therefore becomes an inclusive cutoff; `+Infinity`
    * remains the no-rejection sentinel.
    */
  def fromLegacy(value: Double): Either[ThresholdError, ThresholdCutoff] =
    if value.isPosInfinity then Right(NoRejections)
    else inclusive(value)

  private def finiteDecision(
      score: Double,
      decision: => Boolean
  ): Either[ThresholdError, Boolean] =
    if score.isFinite then Right(decision)
    else Left(ThresholdError.NonFiniteData("threshold score"))

enum ThresholdPValues:
  case NotComputed
  case Unadjusted(values: SomeScalarVolume[Double])
  case Adjusted(values: SomeScalarVolume[Double], policy: CorrectionPolicy)

  def valuesOption: Option[SomeScalarVolume[Double]] =
    this match
      case NotComputed =>
        None
      case Unadjusted(values) =>
        Some(values)
      case Adjusted(values, _) =>
        Some(values)

object ThresholdPValues:
  def fromOption(values: Option[SomeScalarVolume[Double]]): ThresholdPValues =
    values match
      case Some(map) => Unadjusted(map)
      case None      => NotComputed

/** A thresholded map. `reject` is the decision. Only voxelwise results carry
  * a scalar cutoff; region-set procedures such as HierScan decide by region
  * and have no voxel-scale threshold that reproduces their mask.
  */
sealed trait ThresholdResult:
  def method: ThresholdMethod
  def reject: SomeMaskVolume
  def pValueSemantics: ThresholdPValues
  def params: Map[String, String]

  def pValues: Option[SomeScalarVolume[Double]] =
    pValueSemantics.valuesOption

/** A voxelwise result whose `cutoff` applies to each voxel's statistic. */
final case class MapThresholdResult(
    method: ThresholdMethod,
    reject: SomeMaskVolume,
    pValueSemantics: ThresholdPValues,
    cutoff: ThresholdCutoff,
    params: Map[String, String] = Map.empty
) extends ThresholdResult:

  /** Legacy inclusive-double view. Consumers apply `score >= threshold`; code
    * that retains the cutoff should use its typed decision method.
    */
  def threshold: Double =
    cutoff.toLegacyDouble

object MapThresholdResult:
  def fromLegacy(
    method: ThresholdMethod,
    reject: SomeMaskVolume,
    pValues: Option[SomeScalarVolume[Double]],
    threshold: Double,
    params: Map[String, String] = Map.empty
  ): Either[ThresholdError, MapThresholdResult] =
    ThresholdCutoff.fromLegacy(threshold).map { cutoff =>
      MapThresholdResult(method, reject, ThresholdPValues.fromOption(pValues), cutoff, params)
    }

/** A hierarchical scan. `reject` is exactly the union of the significant
  * regions. Node scores are set scores, not voxel statistics, so there is no
  * voxel cutoff: a voxel's own statistic does not decide its rejection.
  */
final case class HierScanResult(
    reject: SomeMaskVolume,
    significantRegions: Vector[HierScanRegionHit],
    nodeTests: Vector[HierScanNodeTest],
    params: Map[String, String] = Map.empty
) extends ThresholdResult:
  override def method: ThresholdMethod =
    ThresholdMethod.HierScan

  override def pValueSemantics: ThresholdPValues =
    ThresholdPValues.NotComputed

/** A fixed family of null actions applied to the observed data.
  *
  * `draw(index)` returns the raw, unoriented statistic field of one action, in
  * mask space (compact index order). Consumers apply the analysis alternative
  * themselves, so a draw must not be pre-negated or pre-absoluted. The same
  * index must return the same field on every call: hierarchical procedures
  * revisit draws and rely on one common null action per index. `reference`
  * declares whether the family is a Monte Carlo sample or a complete
  * enumeration that includes the identity action.
  */
trait NullDraw:
  def nPermutations: PermutationCount
  def reference: NullReference
  def draw(index: Int): Either[ThresholdError, Array[Double]]
