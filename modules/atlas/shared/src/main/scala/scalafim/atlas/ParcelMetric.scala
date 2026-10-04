package scalafim.atlas

import locus4s.SpaceMismatch
import locus4s.data.Field

enum ParcelMetricError:
  case WrongOwner(error: SpaceMismatch)
  case InvalidSchema(details: String)
  case InvalidDocument(details: String)
  case InvalidOrigin(details: String)
  case DomainMismatch
  case IntegrityMismatch
  case InvalidValue(parcelOrdinal: Int, details: String)
  case EvaluationFailed(details: String)

  def message: String = this match
    case WrongOwner(error) => error.message
    case InvalidSchema(details) => s"invalid metric schema: $details"
    case InvalidDocument(details) => s"invalid metric document: $details"
    case InvalidOrigin(details) => s"invalid metric origin: $details"
    case DomainMismatch => "metric canonical ordered parcel domain differs from target"
    case IntegrityMismatch => "metric payload SHA-256 differs from the expected digest"
    case InvalidValue(ordinal, details) => s"invalid Float64 metric at ordinal $ordinal: $details"
    case EvaluationFailed(details) => s"metric field evaluation failed: $details"

/** V1 stores one Float64 scalar per canonical parcel. Attributes describe the
  * scientific measure; they do not change the value encoding or parcel owner.
  */
final class ParcelMetricSchema private (
    val name: String,
    val unit: Option[String],
    val attributes: RegionAttributes
)

object ParcelMetricSchema:
  def from(
      name: String,
      unit: Option[String] = None,
      attributes: Map[String, String] = Map.empty
  ): Either[ParcelMetricError, ParcelMetricSchema] =
    if name.trim.isEmpty then Left(ParcelMetricError.InvalidSchema("name must be non-empty"))
    else if unit.exists(_.trim.isEmpty) then Left(ParcelMetricError.InvalidSchema("unit must be non-empty when present"))
    else RegionAttributes.from(attributes)
      .left.map(error => ParcelMetricError.InvalidSchema(error.message))
      .map(new ParcelMetricSchema(name, unit, _))

/** A compact reference to original support, without a second geometry payload. */
final case class ParcelMetricDomainReference private[atlas] (
    descriptorId: String,
    descriptorVersion: String,
    size: Int,
    structuralFingerprint: String
)

enum ParcelMetricSupport:
  case Volume(reference: ParcelMetricDomainReference, coordinateSpaceId: String, shape: Vector[Int])
  case Surface(reference: ParcelMetricDomainReference, coordinateSpaceId: String, hemisphere: String, vertexCount: Int)

  def domain: ParcelMetricDomainReference = this match
    case Volume(value, _, _) => value
    case Surface(value, _, _, _) => value

final case class ParcelMetricAssignmentReference private[atlas] (
    source: ParcelMetricDomainReference,
    target: ParcelMetricDomainReference,
    sha256: String
)

/** Evidence of where the saved values came from. Restoring changes the live
  * field owner, while preserving this origin; it does not recompute a measure.
  * Support and assignment fingerprints are references, not embedded assets.
  */
final class ParcelMetricOrigin private[atlas] (
    val provenance: NeuropublishAtlasProvenanceV1,
    val representation: AtlasRepresentation,
    val identityPolicy: ParcelIdentity,
    val source: Option[String],
    val parcelMetadata: Vector[NeuropublishParcelMetadataV1],
    val displayOrder: Vector[String],
    val support: Vector[ParcelMetricSupport],
    val assignments: Vector[ParcelMetricAssignmentReference]
)

final case class RestoredParcelMetric[P] private[atlas] (
    values: Field[P, Double],
    schema: ParcelMetricSchema,
    origin: ParcelMetricOrigin,
    payloadSha256: String
)
