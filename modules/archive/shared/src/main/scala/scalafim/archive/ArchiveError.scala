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
      case ValidationFailed(issues) =>
        s"archive validation failed: ${issues.map(_.render).mkString("; ")}"
