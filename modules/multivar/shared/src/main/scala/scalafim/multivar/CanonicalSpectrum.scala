package scalafim.multivar

import gale.linalg.DMat

opaque type ManovaRoot = Double

object ManovaRoot:
  def apply(value: Double): Either[MultivarError, ManovaRoot] =
    if !value.isFinite then Left(MultivarError.NonFiniteValue("MANOVA root", 0, value))
    else if value < 0.0 then Left(MultivarError.NonPositiveSemiDefinite("MANOVA root", value))
    else Right(value)

  private[multivar] def unsafe(value: Double): ManovaRoot =
    require(value.isFinite && value >= 0.0, "MANOVA root must be finite and non-negative")
    value

  extension (root: ManovaRoot)
    inline def value: Double = root

enum ManovaStatistic:
  case RoyLargestRoot
  case WilksLambda
  case PillaiTrace
  case HotellingLawleyTrace

  def label: String =
    this match
      case RoyLargestRoot       => "RoyLargestRoot"
      case WilksLambda          => "WilksLambda"
      case PillaiTrace          => "PillaiTrace"
      case HotellingLawleyTrace => "HotellingLawleyTrace"

  def evaluate(roots: CanonicalRootSpectrum): Double =
    this match
      case RoyLargestRoot => roots.values.head.value
      case WilksLambda =>
        var product = 1.0
        var index = 0
        while index < roots.values.length do
          product /= 1.0 + roots.values(index).value
          index += 1
        product
      case PillaiTrace =>
        var total = 0.0
        var index = 0
        while index < roots.values.length do
          val root = roots.values(index).value
          total += root / (1.0 + root)
          index += 1
        total
      case HotellingLawleyTrace =>
        var total = 0.0
        var index = 0
        while index < roots.values.length do
          total += roots.values(index).value
          index += 1
        total

final case class CanonicalRootSpectrum private (values: Vector[ManovaRoot]):
  require(values.nonEmpty, "canonical root spectrum must be non-empty")
  require(values.map(_.value).sliding(2).forall:
    case Vector(left, right) => left >= right
    case _                   => true
  , "canonical roots must be ordered from largest to smallest")

  def rank: Int = values.length

  def statistic(estimand: ManovaStatistic): Double =
    estimand.evaluate(this)

object CanonicalRootSpectrum:
  private[multivar] def unsafe(values: Vector[ManovaRoot]): CanonicalRootSpectrum =
    new CanonicalRootSpectrum(values)

final case class ManovaStatistics(
    royLargestRoot: Double,
    wilksLambda: Double,
    pillaiTrace: Double,
    hotellingLawleyTrace: Double
):
  require(royLargestRoot.isFinite && royLargestRoot >= 0.0, "Roy's root must be finite and non-negative")
  require(wilksLambda.isFinite && wilksLambda > 0.0 && wilksLambda <= 1.0, "Wilks' lambda must be in (0, 1]")
  require(pillaiTrace.isFinite && pillaiTrace >= 0.0, "Pillai trace must be finite and non-negative")
  require(hotellingLawleyTrace.isFinite && hotellingLawleyTrace >= 0.0, "Hotelling-Lawley trace must be finite and non-negative")

  def apply(estimand: ManovaStatistic): Double =
    estimand match
      case ManovaStatistic.RoyLargestRoot       => royLargestRoot
      case ManovaStatistic.WilksLambda          => wilksLambda
      case ManovaStatistic.PillaiTrace          => pillaiTrace
      case ManovaStatistic.HotellingLawleyTrace => hotellingLawleyTrace

object ManovaStatistics:
  def from(roots: CanonicalRootSpectrum): ManovaStatistics =
    ManovaStatistics(
      roots.statistic(ManovaStatistic.RoyLargestRoot),
      roots.statistic(ManovaStatistic.WilksLambda),
      roots.statistic(ManovaStatistic.PillaiTrace),
      roots.statistic(ManovaStatistic.HotellingLawleyTrace)
    )

/** One numerically repeated-root block. Individual axes inside the block are
  * not identified; the Euclidean projector is the stable estimand.
  */
final case class CanonicalRootCluster(
    firstRoot: Int,
    multiplicity: Int,
    representative: ManovaRoot,
    projector: DMat
):
  require(firstRoot >= 0, "root-cluster offset must be non-negative")
  require(multiplicity > 0, "root-cluster multiplicity must be positive")
  require(projector.rows == projector.cols, "root-cluster projector must be square")

final case class CanonicalSpectrumDiagnostics(
    effectRank: Int,
    residualRank: Int,
    regularizedResidualCondition: Double,
    clusters: Vector[CanonicalRootCluster],
    generalizedResidual: Double,
    bOrthonormalityError: Double
):
  require(effectRank >= 0 && residualRank >= 0, "numerical ranks must be non-negative")
  require(clusters.nonEmpty, "canonical spectrum must contain a root cluster")
  require(regularizedResidualCondition >= 0.0 && !regularizedResidualCondition.isNaN, "condition estimate must be non-negative")
  require(generalizedResidual.isFinite && generalizedResidual >= 0.0, "generalized residual must be finite and non-negative")
  require(bOrthonormalityError.isFinite && bOrthonormalityError >= 0.0, "orthonormality error must be finite and non-negative")

final case class CanonicalSpectrumFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    roots: CanonicalRootSpectrum,
    statistics: ManovaStatistics,
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    regularization: ResidualRegularizationFit,
    diagnostics: CanonicalSpectrumDiagnostics,
    provenance: CanonicalEffectProvenance
):
  def denseFrame: Either[MultivarError, DMat] =
    functionalFrame.weights.toDense.left.map(error => MultivarError.SolverFailed(error.message))
