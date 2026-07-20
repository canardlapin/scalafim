package scalafim.latent

import gale.linalg.DMat

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

opaque type LatentLabel = String

object LatentLabel:
  val Empty: LatentLabel = ""

  def apply(value: String): Either[LatentError, LatentLabel] =
    val normalized = value.trim
    if normalized.isEmpty then Left(LatentError.EmptyIdentifier("latent label"))
    else Right(normalized)

  def optional(value: String): Either[LatentError, LatentLabel] =
    val normalized = value.trim
    if normalized.isEmpty then Right(Empty)
    else Right(normalized)

  def unsafe(value: String): LatentLabel =
    optional(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (label: LatentLabel)
    def value: String = label
    def isEmpty: Boolean = label == ""
    def nonEmpty: Boolean = label != ""
    def toOption: Option[String] =
      if label == "" then None else Some(label)

final case class LatentAnnotation(
    label: LatentLabel,
    metadata: LatentMetadata
):
  def labelValue: String =
    label.value

  def metadataValues: Map[String, String] =
    metadata.values

object LatentAnnotation:
  val Empty: LatentAnnotation =
    LatentAnnotation(LatentLabel.Empty, LatentMetadata.Empty)

  def apply(
      label: String,
      metadata: Map[String, String]
  ): Either[LatentError, LatentAnnotation] =
    for
      typedLabel <- LatentLabel.optional(label)
      typedMetadata <- LatentMetadata(metadata)
    yield LatentAnnotation(typedLabel, typedMetadata)

  def unsafe(
      label: String,
      metadata: Map[String, String]
  ): LatentAnnotation =
    apply(label, metadata).fold(error => throw IllegalArgumentException(error.message), identity)

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

enum LatentMaterializationTerm:
  case LinearCoefficients
  case SampleOffset
  case ResidualEvents

final case class LatentDecodeSemantics(
    coefficientDecode: Set[LatentMaterializationTerm],
    reconstruction: Set[LatentMaterializationTerm]
):
  def coefficientDecodeIncludes(term: LatentMaterializationTerm): Boolean =
    coefficientDecode.contains(term)

  def reconstructionIncludes(term: LatentMaterializationTerm): Boolean =
    reconstruction.contains(term)

  def coefficientDecodeIsLinearOnly: Boolean =
    coefficientDecode == Set(LatentMaterializationTerm.LinearCoefficients)

object LatentDecodeSemantics:
  val LinearOnly: LatentDecodeSemantics =
    linear(offset = false)

  def linear(offset: Boolean): LatentDecodeSemantics =
    LatentDecodeSemantics(
      coefficientDecode = Set(LatentMaterializationTerm.LinearCoefficients),
      reconstruction =
        if offset then Set(LatentMaterializationTerm.LinearCoefficients, LatentMaterializationTerm.SampleOffset)
        else Set(LatentMaterializationTerm.LinearCoefficients)
    )

  def boldZip(offset: Boolean, residualEvents: Boolean): LatentDecodeSemantics =
    LatentDecodeSemantics(
      coefficientDecode = Set(LatentMaterializationTerm.LinearCoefficients),
      reconstruction =
        Set(LatentMaterializationTerm.LinearCoefficients) ++
          Option.when(offset)(LatentMaterializationTerm.SampleOffset) ++
          Option.when(residualEvents)(LatentMaterializationTerm.ResidualEvents)
    )

trait LatentResponse:
  def shape: LatentShape
  def sourceDomain: DomainId
  def targetDomain: DomainId
  def latentLabel: LatentLabel
  def typedMetadata: LatentMetadata
  def decodeSemantics: LatentDecodeSemantics

  def label: String =
    latentLabel.value

  def metadata: Map[String, String] =
    typedMetadata.values

  /** Coefficient time series with rows as timepoints and columns as latent coefficients. */
  def coefTime: DMat

  /** Decode coefficient columns from coefficient-space rows to target-sample rows. */
  def decodeCoefficients(coefficients: DMat): Either[LatentError, DMat]

  /** Reconstruct dense response data with rows as timepoints and columns as selected samples. */
  def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DMat]
