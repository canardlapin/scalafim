package scalafim.estimates

import scalafim.archive.ContentDigest

enum EstimateError:
  case Invalid(detail: String)
  case Unsupported(detail: String)
  case Integrity(detail: String)
  case Io(detail: String)
  case Conflict(detail: String)
  case Cancelled
  case Closed

  def message: String = this match
    case Invalid(detail) => detail
    case Unsupported(detail) => detail
    case Integrity(detail) => detail
    case Io(detail) => detail
    case Conflict(detail) => detail
    case Cancelled => "Estimate operation cancelled; discard the destination"
    case Closed => "Estimate resource is closed"

private[estimates] object Invariants:
  def text(value: String): Boolean = value.nonEmpty && value == value.trim && !value.exists(_.isControl)
  def uuid(value: String): Boolean =
    value.matches("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")
  def unique[A](values: Vector[A]): Boolean = values.nonEmpty && values.distinct.size == values.size

/** Paths are dataset-root relative, independent of a manifest's location. */
final case class FileReference(path: String, digest: ContentDigest, bytes: Long):
  require(path.nonEmpty && !path.startsWith("/") && !path.contains('\\') && !path.contains(':'))
  require(!path.exists(_.isControl) && path.split("/", -1).forall(s => s.nonEmpty && s != "." && s != ".."))
  require(digest.algorithm == "sha256" && digest.value.matches("[0-9a-f]{64}"))
  require(bytes >= 0L)

final case class ParticipantId(dataset: DatasetId, label: String):
  require(Invariants.text(label))

final case class DatasetId(value: String):
  require(Invariants.uuid(value), "DatasetId must be a lowercase UUID")

final case class ModelRevisionId(value: String):
  require(Invariants.uuid(value), "ModelRevisionId must be a lowercase UUID")

final case class UnitId(value: String):
  require(Invariants.uuid(value), "UnitId must be a lowercase UUID")

final case class UnitRevisionId(value: String):
  require(Invariants.uuid(value), "UnitRevisionId must be a lowercase UUID")

final case class CollectionRevisionId(value: String):
  require(Invariants.uuid(value), "CollectionRevisionId must be a lowercase UUID")

final case class EstimandId(value: String):
  require(Invariants.text(value), "EstimandId must be nonempty text without control characters")

final case class ProductId(value: String):
  require(Invariants.text(value), "ProductId must be nonempty text without control characters")

final case class ObservationId(value: String):
  require(Invariants.text(value), "ObservationId must be nonempty text without control characters")

final case class ColumnId(value: String):
  require(Invariants.text(value), "ColumnId must be nonempty text without control characters")

final case class AcquisitionId(value: String):
  require(Invariants.text(value), "AcquisitionId must be nonempty text without control characters")

final case class RepresentationId(value: String):
  require(Invariants.text(value), "RepresentationId must be nonempty text without control characters")
