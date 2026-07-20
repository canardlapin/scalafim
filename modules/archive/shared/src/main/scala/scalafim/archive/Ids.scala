package scalafim.archive

private[archive] def checkedNonEmpty(value: String, label: String): String =
  val out = value.trim
  require(out.nonEmpty, s"$label must be non-empty")
  out

private[archive] def nonEmpty(value: String, label: String): Either[ArchiveError, String] =
  val out = value.trim
  if out.isEmpty then Left(ArchiveError.InvalidArchive(s"$label must be non-empty"))
  else Right(out)

opaque type ArchivePath = String

object ArchivePath:
  def parse(value: String): Either[ArchiveError, ArchivePath] =
    val out = value.replace('\\', '/').trim
    if out.isEmpty then Left(ArchiveError.InvalidPath(value, "ArchivePath must be non-empty"))
    else if !out.startsWith("/") then Left(ArchiveError.InvalidPath(value, "ArchivePath must be absolute"))
    else if out.contains("//") then Left(ArchiveError.InvalidPath(value, "ArchivePath must not contain empty segments"))
    else if out.split('/').contains("..") then Left(ArchiveError.InvalidPath(value, "ArchivePath must not contain '..'"))
    else Right(out)

  def apply(value: String): ArchivePath =
    unsafe(value)

  def unsafe(value: String): ArchivePath =
    parse(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (path: ArchivePath)
    def value: String = path
    def segments: Vector[String] =
      path.stripPrefix("/").split('/').filter(_.nonEmpty).toVector
    def child(child: String): Either[ArchiveError, ArchivePath] =
      val suffix = child.replace('\\', '/').trim
      if suffix.isEmpty then Left(ArchiveError.InvalidPath(child, "ArchivePath child must be non-empty"))
      else if suffix.startsWith("/") then Left(ArchiveError.InvalidPath(child, "ArchivePath child must be relative"))
      else ArchivePath.parse(s"$path/$suffix")
    def /(child: String): ArchivePath =
      path.child(child).fold(err => throw IllegalArgumentException(err.message), identity)
    def parent: Option[ArchivePath] =
      val idx = path.lastIndexOf('/')
      if idx <= 0 then None else Some(ArchivePath(path.substring(0, idx)))

final case class ArchiveDatasetPath(path: ArchivePath):
  require(path.segments.nonEmpty, "ArchiveDatasetPath must identify a dataset path")

  def value: String = path.value
  def segments: Vector[String] = path.segments
  def runScope: Option[RunScopedPath] = RunScopedPath.from(this)

object ArchiveDatasetPath:
  def parse(value: String): Either[ArchiveError, ArchiveDatasetPath] =
    ArchivePath.parse(value).map(ArchiveDatasetPath(_))

  def from(value: String): ArchiveDatasetPath =
    parse(value).fold(err => throw IllegalArgumentException(err.message), identity)

opaque type RunLabel = String

object RunLabel:
  private val Pattern = "^[A-Za-z0-9][A-Za-z0-9_.-]*$".r

  def parse(value: String): Either[ArchiveError, RunLabel] =
    val out = value.trim
    if out.isEmpty then Left(ArchiveError.InvalidArchive("RunLabel must be non-empty"))
    else if !Pattern.matches(out) then Left(ArchiveError.InvalidArchive(s"RunLabel '$out' contains invalid characters"))
    else Right(out)

  def apply(value: String): RunLabel =
    unsafe(value)

  def unsafe(value: String): RunLabel =
    parse(value).fold(err => throw IllegalArgumentException(err.message), identity)

  def indexedChecked(index: Int): Either[ArchiveError, RunLabel] =
    if index < 0 then Left(ArchiveError.InvalidArchive("run index must be non-negative"))
    else parse(f"run-${index + 1}%02d")

  def indexed(index: Int): RunLabel =
    indexedChecked(index).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (label: RunLabel)
    def value: String = label

final case class RunScopedPath(label: RunLabel, dataset: ArchiveDatasetPath):
  require(
    dataset.segments.lift(0).contains("scans") && dataset.segments.lift(1).contains(label.value),
    "RunScopedPath dataset must be under /scans/<run-label>/"
  )

  def path: ArchivePath = dataset.path
  def value: String = dataset.value

object RunScopedPath:
  def checked(label: RunLabel, dataset: ArchiveDatasetPath): Either[ArchiveError, RunScopedPath] =
    if dataset.segments.lift(0).contains("scans") && dataset.segments.lift(1).contains(label.value) then
      Right(RunScopedPath(label, dataset))
    else Left(ArchiveError.InvalidPath(dataset.value, "RunScopedPath dataset must be under /scans/<run-label>/"))

  def from(path: ArchivePath): Option[RunScopedPath] =
    from(ArchiveDatasetPath(path))

  def from(path: ArchivePath, label: RunLabel): Option[RunScopedPath] =
    from(path).filter(_.label == label)

  def from(dataset: ArchiveDatasetPath): Option[RunScopedPath] =
    dataset.segments match
      case "scans" +: rawLabel +: _ =>
        RunLabel.parse(rawLabel).toOption.flatMap(label => checked(label, dataset).toOption)
      case _ =>
        None

final class DatasetShape private (val dims: Vector[Int]):
  def rank: Int = dims.length
  def entries: Int = dims.product
  def apply(index: Int): Int = dims(index)
  def head: Int = dims.head
  def toVector: Vector[Int] = dims
  def mkString(sep: String): String = dims.mkString(sep)

  override def equals(other: Any): Boolean =
    other match
      case that: DatasetShape => dims == that.dims
      case _                  => false

  override def hashCode(): Int =
    dims.hashCode()

  override def toString: String =
    s"DatasetShape(${dims.mkString("x")})"

object DatasetShape:
  def apply(dims: Vector[Int]): Either[ArchiveError, DatasetShape] =
    if dims.isEmpty then Left(ArchiveError.InvalidArchive("dataset shape must be non-empty"))
    else if dims.exists(_ <= 0) then Left(ArchiveError.InvalidArchive("dataset shape dimensions must be positive"))
    else Right(new DatasetShape(dims))

  def unsafe(dims: Vector[Int]): DatasetShape =
    apply(dims).fold(err => throw IllegalArgumentException(err.message), identity)

opaque type CreatorId = String

object CreatorId:
  def apply(value: String): Either[ArchiveError, CreatorId] =
    nonEmpty(value, "creator id")

  def unsafe(value: String): CreatorId =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (id: CreatorId)
    def value: String = id

opaque type TransformName = String

object TransformName:
  def apply(value: String): Either[ArchiveError, TransformName] =
    val out = value.trim
    if out.isEmpty then Left(ArchiveError.InvalidArchive("transform name must be non-empty"))
    else if !out.endsWith(".json") then Left(ArchiveError.InvalidArchive("transform name must end in .json"))
    else if out.exists(c => c.isControl || c == '/' || c == '\\') then
      Left(ArchiveError.InvalidArchive("transform name must not contain path separators or control characters"))
    else Right(out)

  def unsafe(value: String): TransformName =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (name: TransformName)
    def value: String = name

opaque type TransformPort = String

object TransformPort:
  private val Pattern = "^[A-Za-z0-9][A-Za-z0-9_.-]*$".r

  def apply(value: String): Either[ArchiveError, TransformPort] =
    val out = value.trim
    if out.isEmpty then Left(ArchiveError.InvalidArchive("transform port must be non-empty"))
    else if !Pattern.matches(out) then Left(ArchiveError.InvalidArchive(s"transform port '$out' contains invalid characters"))
    else Right(out)

  def unsafe(value: String): TransformPort =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (port: TransformPort)
    def value: String = port

opaque type ArchiveStorageFormatId = String

object ArchiveStorageFormatId:
  def apply(value: String): Either[ArchiveError, ArchiveStorageFormatId] =
    val out = value.trim
    if out.isEmpty then Left(ArchiveError.InvalidArchive("archive storage format id must be non-empty"))
    else if out.exists(c => c.isWhitespace || c.isControl) then
      Left(ArchiveError.InvalidArchive("archive storage format id must not contain whitespace or control characters"))
    else Right(out)

  def unsafe(value: String): ArchiveStorageFormatId =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (id: ArchiveStorageFormatId)
    def value: String = id
