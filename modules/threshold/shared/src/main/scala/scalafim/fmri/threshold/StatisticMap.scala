package scalafim.fmri.threshold

import scalafim.image.{SomeMaskVolume, SomeScalarVolume}

final case class StatisticMap private (
    volume: SomeScalarVolume[Double],
    kind: StatKind
):
  def orientation: EvidenceOrientation =
    kind.orientation

object StatisticMap:

  def apply(volume: SomeScalarVolume[Double], kind: StatKind): Either[ThresholdError, StatisticMap] =
    Right(unsafe(volume, kind))

  def z(volume: SomeScalarVolume[Double]): StatisticMap =
    unsafe(volume, StatKind.Z)

  def t(volume: SomeScalarVolume[Double], df: DegreesOfFreedom): StatisticMap =
    unsafe(volume, StatKind.T(df))

  def negLog10P(volume: SomeScalarVolume[Double], pSide: PSide): StatisticMap =
    unsafe(volume, StatKind.NegLog10P(pSide))

  private[threshold] def unsafe(volume: SomeScalarVolume[Double], kind: StatKind): StatisticMap =
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
    mask: SomeMaskVolume,
    alternative: ThresholdAlternative
  ): Either[ThresholdError, StatisticField] =
    MaskedField.fromStatisticMap(statistic, mask, alternative).map { field =>
      StatisticField(statistic, field, alternative)
    }
