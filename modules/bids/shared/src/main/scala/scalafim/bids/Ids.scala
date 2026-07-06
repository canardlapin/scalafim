package scalafim.bids

private def checkedNonEmpty(value: String, label: String): String =
  val out = value.trim
  require(out.nonEmpty, s"$label must be non-empty")
  out

opaque type BidsPath = String

object BidsPath:
  def apply(value: String): BidsPath =
    checkedNonEmpty(value.replace('\\', '/'), "BidsPath")

  def from(value: String): Either[BidsError, BidsPath] =
    val clean = value.replace('\\', '/').trim
    if clean.isEmpty then Left(BidsError.EmptyPath)
    else Right(clean)

  def relative(value: String): Either[BidsError, BidsPath] =
    val clean = value.replace('\\', '/').trim
    if clean.isEmpty then Left(BidsError.EmptyPath)
    else if clean.startsWith("/") then Left(BidsError.InvalidPath(value, "relative path must not be absolute"))
    else if clean.matches("^[A-Za-z]:.*") then Left(BidsError.InvalidPath(value, "relative path must not contain a drive prefix"))
    else
      val parts =
        clean
          .split('/')
          .toVector
          .filterNot(part => part.isEmpty || part == ".")
      if parts.isEmpty then Left(BidsError.EmptyPath)
      else if parts.exists(_ == "..") then Left(BidsError.InvalidPath(value, "relative path must not contain '..'"))
      else Right(parts.mkString("/"))

  extension (path: BidsPath)
    def value: String = path
    def fileName: String = path.split('/').lastOption.getOrElse(path)
    def parent: Option[BidsPath] =
      val idx = path.lastIndexOf('/')
      if idx < 0 then None else Some(BidsPath(path.substring(0, idx)))
    def startsWithPath(prefix: BidsPath): Boolean =
      path == prefix.value || path.startsWith(prefix.value + "/")

opaque type DatasetName = String

object DatasetName:
  val Current: DatasetName = ""

  def apply(value: String): DatasetName = value.trim

  extension (name: DatasetName)
    def value: String = name
    def isCurrent: Boolean = name.isEmpty

opaque type PipelineName = String

object PipelineName:
  def from(value: String): Either[BidsError, PipelineName] =
    val clean = value.trim
    if clean.isEmpty then Left(BidsError.InvalidPath(value, "pipeline name must be non-empty"))
    else Right(clean)

  def apply(value: String): PipelineName =
    checkedNonEmpty(value, "PipelineName")

  extension (name: PipelineName)
    def value: String = name
