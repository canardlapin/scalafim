package scalafim.fmri.threshold

import scalafim.image.NeuroVol

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
  case Unadjusted(values: NeuroVol[Double])
  case Adjusted(values: NeuroVol[Double], policy: CorrectionPolicy)

  def valuesOption: Option[NeuroVol[Double]] =
    this match
      case NotComputed =>
        None
      case Unadjusted(values) =>
        Some(values)
      case Adjusted(values, _) =>
        Some(values)

object ThresholdPValues:
  def fromOption(values: Option[NeuroVol[Double]]): ThresholdPValues =
    values match
      case Some(map) => Unadjusted(map)
      case None      => NotComputed

sealed trait ThresholdResult:
  def method: ThresholdMethod
  def reject: NeuroVol[Boolean]
  def pValueSemantics: ThresholdPValues
  def cutoff: ThresholdCutoff
  def params: Map[String, String]

  def pValues: Option[NeuroVol[Double]] =
    pValueSemantics.valuesOption

  def threshold: Double =
    cutoff.toLegacyDouble

final case class MapThresholdResult(
    method: ThresholdMethod,
    reject: NeuroVol[Boolean],
    pValueSemantics: ThresholdPValues,
    cutoff: ThresholdCutoff,
    params: Map[String, String] = Map.empty
) extends ThresholdResult

object MapThresholdResult:
  def fromLegacy(
    method: ThresholdMethod,
    reject: NeuroVol[Boolean],
    pValues: Option[NeuroVol[Double]],
    threshold: Double,
    params: Map[String, String] = Map.empty
  ): Either[ThresholdError, MapThresholdResult] =
    ThresholdCutoff.fromLegacy(threshold).map { cutoff =>
      MapThresholdResult(method, reject, ThresholdPValues.fromOption(pValues), cutoff, params)
    }

final case class HierScanResult(
    reject: NeuroVol[Boolean],
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
