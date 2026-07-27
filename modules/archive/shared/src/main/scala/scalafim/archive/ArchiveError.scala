package scalafim.archive

enum ArchiveValidationLayer:
  case Structure, Descriptors, References, Shapes, Checksum

final case class ArchiveValidationIssue(
    layer: ArchiveValidationLayer,
    message: String,
    path: Option[ArchivePath] = None
):
  def render: String =
    val prefix = path.fold("")(p => s"${p.value}: ")
    s"$layer: $prefix$message"

enum ArchiveError:
  case InvalidArchive(detail: String)
  case InvalidPath(path: String, detail: String)
  case MissingPayload(path: ArchivePath)
  case ShapeMismatch(detail: String)
  case NonFiniteValue(index: Int)
  case UnsupportedTransform(kind: String)
  case UnsupportedStorage(detail: String)
  case UnsupportedRepresentation(
      found: RepresentationKey,
      supported: Vector[RepresentationKey]
  )
  case UnsupportedPayloadPlan(driver: String, operation: String)
  case IncompletePublication(detail: String)
  case ReceiptMismatch(detail: String)
  case ValidationFailed(issues: Vector[ArchiveValidationIssue])

  def message: String =
    this match
      case InvalidArchive(detail) =>
        s"invalid archive: $detail"
      case InvalidPath(path, detail) =>
        s"invalid archive path '$path': $detail"
      case MissingPayload(path) =>
        s"archive payload missing at '${path.value}'"
      case ShapeMismatch(detail) =>
        s"archive shape mismatch: $detail"
      case NonFiniteValue(index) =>
        s"archive payload contains non-finite value at linear index $index"
      case UnsupportedTransform(kind) =>
        s"unsupported archive transform: $kind"
      case UnsupportedStorage(detail) =>
        s"unsupported archive storage: $detail"
      case UnsupportedRepresentation(found, supported) =>
        val rendered =
          if supported.isEmpty then "none installed"
          else supported.map(_.value).mkString(", ")
        s"unsupported archive representation '${found.value}'; supported: $rendered"
      case UnsupportedPayloadPlan(driver, operation) =>
        s"archive driver '$driver' cannot execute payload operation '$operation'"
      case IncompletePublication(detail) =>
        s"incomplete archive publication: $detail"
      case ReceiptMismatch(detail) =>
        s"archive read receipt does not conform: $detail"
      case ValidationFailed(issues) =>
        s"archive validation failed: ${issues.map(_.render).mkString("; ")}"
