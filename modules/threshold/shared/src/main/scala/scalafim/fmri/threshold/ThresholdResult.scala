package scalafim.fmri.threshold

import scalafim.image.NeuroVol

enum ThresholdMethod:
  case HierScan, Tfce, ClusterFdr, RftPeak, RftCluster, MaxT

sealed trait ThresholdResult:
  def method: ThresholdMethod
  def reject: NeuroVol[Boolean]
  def pValues: Option[NeuroVol[Double]]
  def threshold: Double
  def params: Map[String, String]

final case class MapThresholdResult(
    method: ThresholdMethod,
    reject: NeuroVol[Boolean],
    pValues: Option[NeuroVol[Double]],
    threshold: Double,
    params: Map[String, String] = Map.empty
) extends ThresholdResult:
  require(threshold.isFinite || threshold.isPosInfinity, "threshold must be finite or +Inf")

final case class HierScanResult(
    reject: NeuroVol[Boolean],
    significantRegions: Vector[HierScanRegionHit],
    nodeTests: Vector[HierScanNodeTest],
    threshold: Double,
    params: Map[String, String] = Map.empty
) extends ThresholdResult:
  require(threshold.isFinite || threshold.isPosInfinity, "threshold must be finite or +Inf")

  override def method: ThresholdMethod =
    ThresholdMethod.HierScan

  override def pValues: Option[NeuroVol[Double]] =
    None

trait NullDraw:
  def nPermutations: PermutationCount
  def draw(index: Int): Either[ThresholdError, Array[Double]]
