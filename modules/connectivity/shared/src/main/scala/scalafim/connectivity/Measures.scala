package scalafim.connectivity

sealed trait ConnectivityValueScale:
  def label: String

object ConnectivityValueScale:
  case object EdgeWeight extends ConnectivityValueScale:
    def label: String =
      "edge-weight"

  case object CorrelationR extends ConnectivityValueScale:
    def label: String =
      "correlation-r"

  case object FisherZ extends ConnectivityValueScale:
    def label: String =
      "fisher-z"

  case object Covariance extends ConnectivityValueScale:
    def label: String =
      "covariance"

  case object Precision extends ConnectivityValueScale:
    def label: String =
      "precision"

  case object PartialCorrelation extends ConnectivityValueScale:
    def label: String =
      "partial-correlation"

  case object Distance extends ConnectivityValueScale:
    def label: String =
      "distance"

  case object Similarity extends ConnectivityValueScale:
    def label: String =
      "similarity"

  final case class External private[connectivity] (label: String) extends ConnectivityValueScale

  def external(label: String): Either[ConnectivityError, ConnectivityValueScale] =
    ConnectivityIdentifier.validate("value scale", label).map(External.apply)

final class ConnectivityMeasure private (
    val id: String,
    val scale: ConnectivityValueScale,
    val symmetric: Boolean,
    val supportsRectangular: Boolean
):
  def description: String =
    s"$id:${scale.label}:symmetric=$symmetric:rectangular=$supportsRectangular"

  def withScale(idSuffix: String, scale: ConnectivityValueScale): Either[ConnectivityError, ConnectivityMeasure] =
    ConnectivityIdentifier.validate("measure suffix", idSuffix).map: suffix =>
      new ConnectivityMeasure(s"$id.$suffix", scale, symmetric, supportsRectangular)

object ConnectivityMeasure:
  val edgeWeight: ConnectivityMeasure =
    new ConnectivityMeasure("edge-weight", ConnectivityValueScale.EdgeWeight, symmetric = true, supportsRectangular = true)

  val correlation: ConnectivityMeasure =
    new ConnectivityMeasure("correlation", ConnectivityValueScale.CorrelationR, symmetric = true, supportsRectangular = true)

  val fisherZCorrelation: ConnectivityMeasure =
    new ConnectivityMeasure("correlation.fisher-z", ConnectivityValueScale.FisherZ, symmetric = true, supportsRectangular = true)

  val covariance: ConnectivityMeasure =
    new ConnectivityMeasure("covariance", ConnectivityValueScale.Covariance, symmetric = true, supportsRectangular = true)

  val precision: ConnectivityMeasure =
    new ConnectivityMeasure("precision", ConnectivityValueScale.Precision, symmetric = true, supportsRectangular = false)

  val partialCorrelation: ConnectivityMeasure =
    new ConnectivityMeasure("partial-correlation", ConnectivityValueScale.PartialCorrelation, symmetric = true, supportsRectangular = false)

  val distance: ConnectivityMeasure =
    new ConnectivityMeasure("distance", ConnectivityValueScale.Distance, symmetric = true, supportsRectangular = true)

  def external(
      id: String,
      scale: ConnectivityValueScale,
      symmetric: Boolean,
      supportsRectangular: Boolean = true
  ): Either[ConnectivityError, ConnectivityMeasure] =
    ConnectivityIdentifier.validate("measure", id).map: validId =>
      new ConnectivityMeasure(validId, scale, symmetric, supportsRectangular)

enum DiagonalPolicy:
  case StructuralZero
  case Unit
  case Observed
  case NotApplicable

  def label: String =
    this match
      case StructuralZero => "structural-zero"
      case Unit           => "unit"
      case Observed       => "observed"
      case NotApplicable  => "not-applicable"

  def materializedValue: Option[Double] =
    this match
      case StructuralZero => Some(0.0)
      case Unit           => Some(1.0)
      case Observed       => None
      case NotApplicable  => None
