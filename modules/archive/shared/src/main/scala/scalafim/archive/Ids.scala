package scalafim.archive

private[archive] def checkedNonEmpty(value: String, label: String): String =
  val out = value.trim
  require(out.nonEmpty, s"$label must be non-empty")
  out

opaque type ArchivePath = String

object ArchivePath:
  def apply(value: String): ArchivePath =
    val out = checkedNonEmpty(value.replace('\\', '/'), "ArchivePath")
    require(out.startsWith("/"), "ArchivePath must be absolute")
    require(!out.contains("//"), "ArchivePath must not contain empty segments")
    require(!out.split('/').contains(".."), "ArchivePath must not contain '..'")
    out

  extension (path: ArchivePath)
    def value: String = path
    def segments: Vector[String] =
      path.stripPrefix("/").split('/').filter(_.nonEmpty).toVector
    def /(child: String): ArchivePath =
      val suffix = checkedNonEmpty(child.replace('\\', '/'), "ArchivePath child")
      require(!suffix.startsWith("/"), "ArchivePath child must be relative")
      ArchivePath(s"$path/$suffix")
    def parent: Option[ArchivePath] =
      val idx = path.lastIndexOf('/')
      if idx <= 0 then None else Some(ArchivePath(path.substring(0, idx)))

final case class ArchiveDatasetPath(path: ArchivePath):
  require(path.segments.nonEmpty, "ArchiveDatasetPath must identify a dataset path")

  def value: String = path.value
  def segments: Vector[String] = path.segments
  def runScope: Option[RunScopedPath] = RunScopedPath.from(this)

object ArchiveDatasetPath:
  def from(value: String): ArchiveDatasetPath =
    ArchiveDatasetPath(ArchivePath(value))

opaque type RunLabel = String

object RunLabel:
  private val Pattern = "^[A-Za-z0-9][A-Za-z0-9_.-]*$".r

  def apply(value: String): RunLabel =
    val out = checkedNonEmpty(value, "RunLabel")
    require(Pattern.matches(out), s"RunLabel '$out' contains invalid characters")
    out

  def indexed(index: Int): RunLabel =
    require(index >= 0, "run index must be non-negative")
    RunLabel(f"run-${index + 1}%02d")

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
  def from(path: ArchivePath): Option[RunScopedPath] =
    from(ArchiveDatasetPath(path))

  def from(path: ArchivePath, label: RunLabel): Option[RunScopedPath] =
    from(path).filter(_.label == label)

  def from(dataset: ArchiveDatasetPath): Option[RunScopedPath] =
    dataset.segments match
      case "scans" +: rawLabel +: _ =>
        try Some(RunScopedPath(RunLabel(rawLabel), dataset))
        catch case _: IllegalArgumentException => None
      case _ =>
        None

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
