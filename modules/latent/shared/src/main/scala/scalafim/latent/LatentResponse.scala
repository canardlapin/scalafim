package scalafim.latent

import scalafim.linalg.DoubleMatrix

opaque type LatentMetadata = Map[String, String]

object LatentMetadata:
  val Empty: LatentMetadata = Map.empty

  def apply(values: Map[String, String]): Either[LatentError, LatentMetadata] =
    if values.keys.exists(_.trim.isEmpty) then Left(LatentError.EmptyIdentifier("metadata key"))
    else Right(values)

  def unsafe(values: Map[String, String]): LatentMetadata =
    apply(values).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (metadata: LatentMetadata)
    def values: Map[String, String] = metadata
    def get(key: String): Option[String] = metadata.get(key)

opaque type TimepointCount = Int

object TimepointCount:
  def apply(value: Int): Either[LatentError, TimepointCount] =
    if value <= 0 then Left(LatentError.NonPositiveDimension("timepoint count", value))
    else Right(value)

  def unsafe(value: Int): TimepointCount =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (count: TimepointCount)
    def value: Int = count

opaque type SampleCount = Int

object SampleCount:
  def apply(value: Int): Either[LatentError, SampleCount] =
    if value <= 0 then Left(LatentError.NonPositiveDimension("sample count", value))
    else Right(value)

  def unsafe(value: Int): SampleCount =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (count: SampleCount)
    def value: Int = count

opaque type CoefficientCount = Int

object CoefficientCount:
  def apply(value: Int): Either[LatentError, CoefficientCount] =
    if value <= 0 then Left(LatentError.NonPositiveDimension("coefficient count", value))
    else Right(value)

  def unsafe(value: Int): CoefficientCount =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (count: CoefficientCount)
    def value: Int = count

final case class LatentShape(
    timepoints: Int,
    samples: Int,
    coefficients: Int
):
  require(timepoints > 0, "timepoint count must be positive")
  require(samples > 0, "sample count must be positive")
  require(coefficients > 0, "coefficient count must be positive")

  def timepointCount: TimepointCount =
    TimepointCount.unsafe(timepoints)

  def sampleCount: SampleCount =
    SampleCount.unsafe(samples)

  def coefficientCount: CoefficientCount =
    CoefficientCount.unsafe(coefficients)

object LatentShape:
  def checked(
      timepoints: Int,
      samples: Int,
      coefficients: Int
  ): Either[LatentError, LatentShape] =
    for
      t <- TimepointCount(timepoints)
      s <- SampleCount(samples)
      c <- CoefficientCount(coefficients)
    yield LatentShape(t.value, s.value, c.value)

trait LatentResponse:
  def shape: LatentShape
  def sourceDomain: DomainId
  def targetDomain: DomainId
  def label: String
  def metadata: Map[String, String]

  def typedMetadata: LatentMetadata =
    LatentMetadata.unsafe(metadata)

  /** Coefficient time series with rows as timepoints and columns as latent coefficients. */
  def coefTime: DoubleMatrix

  /** Decode coefficient columns from coefficient-space rows to target-sample rows. */
  def decodeCoefficients(coefficients: DoubleMatrix): Either[LatentError, DoubleMatrix]

  /** Reconstruct dense response data with rows as timepoints and columns as selected samples. */
  def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DoubleMatrix]
