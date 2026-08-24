package scalafim.fmri.threshold

import scalafim.image.{SomeMaskVolume, SomeScalarVolume}

enum ThresholdMethod:
  case HierScan, Tfce, ClusterFdr, RftPeak, RftCluster, MaxT

sealed trait ThresholdCutoff:
  def toLegacyDouble: Double

object ThresholdCutoff:
  final case class Inclusive private[ThresholdCutoff] (value: Double) extends ThresholdCutoff:
    require(value.isFinite, "inclusive threshold cutoff must be finite")

    override def toLegacyDouble: Double =
      value

  case object NoRejections extends ThresholdCutoff:
    override def toLegacyDouble: Double =
      Double.PositiveInfinity

  def inclusive(value: Double): Either[ThresholdError, ThresholdCutoff] =
    if value.isFinite then Right(Inclusive(value))
    else Left(ThresholdError.NonFiniteData("threshold cutoff"))

  def fromLegacy(value: Double): Either[ThresholdError, ThresholdCutoff] =
    if value.isPosInfinity then Right(NoRejections)
    else inclusive(value)

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

sealed trait ThresholdResult:
  def method: ThresholdMethod
  def reject: SomeMaskVolume
  def pValueSemantics: ThresholdPValues
  def cutoff: ThresholdCutoff
  def params: Map[String, String]

  def pValues: Option[SomeScalarVolume[Double]] =
    pValueSemantics.valuesOption

  def threshold: Double =
    cutoff.toLegacyDouble

final case class MapThresholdResult(
    method: ThresholdMethod,
    reject: SomeMaskVolume,
    pValueSemantics: ThresholdPValues,
    cutoff: ThresholdCutoff,
    params: Map[String, String] = Map.empty
) extends ThresholdResult

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

final case class HierScanResult(
    reject: SomeMaskVolume,
    significantRegions: Vector[HierScanRegionHit],
    nodeTests: Vector[HierScanNodeTest],
    cutoff: ThresholdCutoff,
    params: Map[String, String] = Map.empty
) extends ThresholdResult:
  override def method: ThresholdMethod =
    ThresholdMethod.HierScan

  override def pValueSemantics: ThresholdPValues =
    ThresholdPValues.NotComputed

trait NullDraw:
  def nPermutations: PermutationCount
  def draw(index: Int): Either[ThresholdError, Array[Double]]
