package scalafim.fmri.threshold

import scalafim.image.NeuroVol

final case class StatisticMap private (
    volume: NeuroVol[Double],
    kind: StatKind
):
  def orientation: EvidenceOrientation =
    kind.orientation

object StatisticMap:

  def apply(volume: NeuroVol[Double], kind: StatKind): Either[ThresholdError, StatisticMap] =
    Right(unsafe(volume, kind))

  def z(volume: NeuroVol[Double]): StatisticMap =
    unsafe(volume, StatKind.Z)

  def t(volume: NeuroVol[Double], df: DegreesOfFreedom): StatisticMap =
    unsafe(volume, StatKind.T(df))

  def negLog10P(volume: NeuroVol[Double], pSide: PSide): StatisticMap =
    unsafe(volume, StatKind.NegLog10P(pSide))

  private[threshold] def unsafe(volume: NeuroVol[Double], kind: StatKind): StatisticMap =
    new StatisticMap(volume, kind)

final case class StatisticField private (
    statistic: StatisticMap,
    evidence: MaskedField,
    alternative: ThresholdAlternative
):
  def size: Int =
    evidence.size

object StatisticField:

  def fromMap(
    statistic: StatisticMap,
    alternative: ThresholdAlternative = ThresholdAlternative.Greater
  ): Either[ThresholdError, StatisticField] =
    MaskedField.fromStatisticMap(statistic, alternative).map { field =>
      StatisticField(statistic, field, alternative)
    }

  def fromMap(
    statistic: StatisticMap,
    mask: NeuroVol[Boolean],
    alternative: ThresholdAlternative
  ): Either[ThresholdError, StatisticField] =
    MaskedField.fromStatisticMap(statistic, mask, alternative).map { field =>
      StatisticField(statistic, field, alternative)
    }
