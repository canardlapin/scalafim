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
    def /(child: String): ArchivePath =
      val suffix = checkedNonEmpty(child.replace('\\', '/'), "ArchivePath child")
      require(!suffix.startsWith("/"), "ArchivePath child must be relative")
      ArchivePath(s"$path/$suffix")
    def parent: Option[ArchivePath] =
      val idx = path.lastIndexOf('/')
      if idx <= 0 then None else Some(ArchivePath(path.substring(0, idx)))

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
