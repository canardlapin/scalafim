package scalafim.archive.lna

import scalafim.archive.ArchiveError
import gale.linalg.DMat

opaque type SharedBasisId = String

object SharedBasisId:
  def apply(value: String): Either[ArchiveError, SharedBasisId] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(ArchiveError.InvalidArchive("shared basis id must be non-empty"))
    else if trimmed.length > 256 then Left(ArchiveError.InvalidArchive("shared basis id must be at most 256 characters"))
    else if trimmed.exists(_.isControl) then Left(ArchiveError.InvalidArchive("shared basis id must not contain control characters"))
    else if trimmed.exists(c => c == '/' || c == '\\') then Left(ArchiveError.InvalidArchive("shared basis id must not contain path separators"))
    else Right(trimmed)

  def unsafe(value: String): SharedBasisId =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (id: SharedBasisId)
    def value: String = id

opaque type SharedBasisChecksum = String

object SharedBasisChecksum:
  private val Pattern = "^sha256:[0-9a-f]{64}$".r

  def apply(value: String): Either[ArchiveError, SharedBasisChecksum] =
    val normalized = value.trim.toLowerCase
    if Pattern.matches(normalized) then Right(normalized)
    else Left(ArchiveError.InvalidArchive(s"shared basis checksum must be sha256:<64 lowercase hex>, got '$value'"))

  def fromHex(hex: String): Either[ArchiveError, SharedBasisChecksum] =
    apply(s"sha256:${hex.trim.toLowerCase}")

  def unsafe(value: String): SharedBasisChecksum =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (checksum: SharedBasisChecksum)
    def value: String = checksum
    def hex: String = checksum.stripPrefix("sha256:")
    def filename: String = s"sha256-${checksum.hex}.lna_basis.h5"

opaque type SharedBasisLocator = String

object SharedBasisLocator:
  def apply(value: String): Either[ArchiveError, SharedBasisLocator] =
    val normalized = value.trim.replace('\\', '/')
    if normalized.isEmpty then Left(ArchiveError.InvalidPath(value, "shared basis locator must be non-empty"))
    else if normalized.startsWith("/") then Left(ArchiveError.InvalidPath(value, "shared basis locator must be relative"))
    else if normalized.exists(_.isControl) then Left(ArchiveError.InvalidPath(value, "shared basis locator must not contain control characters"))
    else
      val parts = normalized.split('/').toVector
      if parts.exists(part => part.isEmpty || part == "." || part == "..") then
        Left(ArchiveError.InvalidPath(value, "shared basis locator must not contain empty, '.', or '..' path segments"))
      else Right(normalized)

  def unsafe(value: String): SharedBasisLocator =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (locator: SharedBasisLocator)
    def value: String = locator

final case class SharedBasisRef(
    basisId: SharedBasisId,
    checksum: SharedBasisChecksum,
    locator: Option[SharedBasisLocator] = None
)

opaque type SharedBasisKind = String

object SharedBasisKind:
  def apply(value: String): Either[ArchiveError, SharedBasisKind] =
    val normalized = value.trim
    if normalized.isEmpty then Left(ArchiveError.InvalidArchive("shared basis kind must be non-empty"))
    else if normalized.exists(_.isControl) then Left(ArchiveError.InvalidArchive("shared basis kind must not contain control characters"))
    else Right(normalized)

  def unsafe(value: String): SharedBasisKind =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (kind: SharedBasisKind)
    def value: String = kind

final case class SharedBasisMask(
    dims: Vector[Int],
    values: Vector[Boolean]
):
  require(dims.nonEmpty, "shared basis mask dims must be non-empty")
  require(dims.forall(_ > 0), "shared basis mask dims must be positive")
  require(dims.product == values.length, "shared basis mask length must match dims product")

  def activeCount: Int =
    values.count(identity)

object SharedBasisMask:
  def checked(
      dims: Vector[Int],
      values: Vector[Boolean]
  ): Either[ArchiveError, SharedBasisMask] =
    if dims.isEmpty then Left(ArchiveError.InvalidArchive("shared basis mask dims must be non-empty"))
    else if dims.exists(_ <= 0) then Left(ArchiveError.InvalidArchive("shared basis mask dims must be positive"))
    else if dims.product != values.length then Left(ArchiveError.ShapeMismatch("shared basis mask length must match dims product"))
    else Right(SharedBasisMask(dims, values))

final case class SharedBasisArtifact(
    loadings: DMat,
    mask: SharedBasisMask,
    kind: String,
    params: Map[String, String] = Map.empty,
    created: Option[String] = None
):
  require(loadings.rows > 0 && loadings.cols > 0, "shared basis loadings must be non-empty")
  require(mask.activeCount == loadings.rows, "shared basis active mask count must match loading rows")
  require(kind.trim.nonEmpty, "shared basis kind must be non-empty")
  require(params.keys.forall(_.trim.nonEmpty), "shared basis params keys must be non-empty")
  require(created.forall(_.trim.nonEmpty), "shared basis created timestamp must be non-empty when provided")

  def nVoxels: Int = loadings.rows
  def nAtoms: Int = loadings.cols
  def checksum: SharedBasisChecksum = SharedBasisArtifact.checksum(this)
  def maskChecksum: SharedBasisChecksum = SharedBasisArtifact.maskChecksum(mask)
  def contentAddressedFilename: String = checksum.filename

object SharedBasisArtifact:
  val ChecksumAlgorithm: String = "sha256:lna-shared-basis-v1"
  val MaskChecksumAlgorithm: String = "sha256:lna-shared-basis-mask-v1"

  def checked(
      loadings: DMat,
      mask: SharedBasisMask,
      kind: String,
      params: Map[String, String] = Map.empty,
      created: Option[String] = None
  ): Either[ArchiveError, SharedBasisArtifact] =
    for
      checkedKind <- SharedBasisKind(kind)
      _ <-
        if loadings.rows > 0 && loadings.cols > 0 then Right(())
        else Left(ArchiveError.ShapeMismatch("shared basis loadings must be non-empty"))
      _ <-
        if mask.activeCount == loadings.rows then Right(())
        else Left(ArchiveError.ShapeMismatch("shared basis active mask count must match loading rows"))
      _ <-
        if params.keys.forall(_.trim.nonEmpty) then Right(())
        else Left(ArchiveError.InvalidArchive("shared basis params keys must be non-empty"))
      _ <-
        if created.forall(_.trim.nonEmpty) then Right(())
        else Left(ArchiveError.InvalidArchive("shared basis created timestamp must be non-empty when provided"))
      artifact = SharedBasisArtifact(loadings, mask, checkedKind.value, params, created)
      finite <- validateFinite(artifact)
    yield finite

  def checksum(artifact: SharedBasisArtifact): SharedBasisChecksum =
    val hex =
      SharedBasisDigest.sha256Hex { digest =>
        digest.string("lna_shared_basis_v1")
        digest.string(artifact.kind.trim)
        digest.string(SharedBasisParamsCodec.render(artifact.params))
        writeMask(digest, artifact.mask)
        writeLoadings(digest, artifact.loadings)
      }
    SharedBasisChecksum.fromHex(hex).fold(err => throw IllegalStateException(err.message), identity)

  def maskChecksum(mask: SharedBasisMask): SharedBasisChecksum =
    val hex =
      SharedBasisDigest.sha256Hex { digest =>
        digest.string("lna_shared_basis_mask_v1")
        writeMask(digest, mask)
      }
    SharedBasisChecksum.fromHex(hex).fold(err => throw IllegalStateException(err.message), identity)

  def validateFinite(artifact: SharedBasisArtifact): Either[ArchiveError, SharedBasisArtifact] =
    var index = 0
    var bad = Option.empty[Int]
    while index < artifact.loadings.rows * artifact.loadings.cols && bad.isEmpty do
      val row = index / artifact.loadings.cols
      val col = index % artifact.loadings.cols
      if !artifact.loadings(row, col).isFinite then bad = Some(index)
      index += 1
    bad match
      case Some(i) => Left(ArchiveError.NonFiniteValue(i))
      case None    => Right(artifact)

  private def writeMask(digest: SharedBasisDigest.Writer, mask: SharedBasisMask): Unit =
    digest.string("mask")
    digest.intLE(mask.dims.length)
    mask.dims.foreach(digest.intLE)
    mask.values.foreach(value => digest.byte(if value then 1 else 0))

  private def writeLoadings(digest: SharedBasisDigest.Writer, loadings: DMat): Unit =
    digest.string("dense")
    digest.intLE(loadings.rows)
    digest.intLE(loadings.cols)
    var col = 0
    while col < loadings.cols do
      var row = 0
      while row < loadings.rows do
        digest.doubleLE(loadings(row, col))
        row += 1
      col += 1

final case class SharedBasisRegistryEntry(
    checksum: SharedBasisChecksum,
    filename: String,
    kind: String,
    maskChecksum: SharedBasisChecksum,
    nAtoms: Int,
    nVoxels: Int,
    created: String
):
  require(filename == checksum.filename, "shared basis registry filename must match checksum")
  require(kind.trim.nonEmpty, "shared basis registry kind must be non-empty")
  require(nAtoms > 0, "shared basis registry nAtoms must be positive")
  require(nVoxels > 0, "shared basis registry nVoxels must be positive")
  require(created.trim.nonEmpty, "shared basis registry created timestamp must be non-empty")

object SharedBasisRegistryEntry:
  def fromArtifact(artifact: SharedBasisArtifact, created: String): SharedBasisRegistryEntry =
    SharedBasisRegistryEntry(
      checksum = artifact.checksum,
      filename = artifact.contentAddressedFilename,
      kind = artifact.kind,
      maskChecksum = artifact.maskChecksum,
      nAtoms = artifact.nAtoms,
      nVoxels = artifact.nVoxels,
      created = created
    )

final case class SharedBasisRegistry(entries: Map[SharedBasisId, SharedBasisRegistryEntry] = Map.empty):
  def updated(id: SharedBasisId, entry: SharedBasisRegistryEntry): SharedBasisRegistry =
    copy(entries = entries.updated(id, entry))

  def get(id: SharedBasisId): Option[SharedBasisRegistryEntry] =
    entries.get(id)

  def aliasesFor(checksum: SharedBasisChecksum): Vector[SharedBasisId] =
    entries.toVector.collect { case (id, entry) if entry.checksum == checksum => id }.sortBy(_.value)

object SharedBasisRegistry:
  val FormatVersion: String = "scalafim-lna-basis-registry-0"

object SharedBasisParamsCodec:
  def render(values: Map[String, String]): String =
    Json.obj(values.toVector.sortBy(_._1).map { case (key, value) => key -> Json.str(value) })

  def parse(text: String): Either[ArchiveError, Map[String, String]] =
    Json.parse(text).flatMap {
      case Json.J.Obj(fields) =>
        traverse(fields.toVector) { case (key, value) =>
          value match
            case Json.J.Str(text) => Right(key -> text)
            case _ => Left(ArchiveError.InvalidArchive(s"shared basis param '$key' must be a string"))
        }.map(_.toMap)
      case _ =>
        Left(ArchiveError.InvalidArchive("shared basis params must be a JSON object"))
    }

object SharedBasisRegistryCodec:
  def render(registry: SharedBasisRegistry): String =
    Json.obj(
      Vector(
        "version" -> Json.str(SharedBasisRegistry.FormatVersion),
        "bases" -> Json.obj(
          registry.entries.toVector.sortBy(_._1.value).map { case (id, entry) =>
            id.value -> entryJson(entry)
          }
        )
      )
    )

  def parse(text: String): Either[ArchiveError, SharedBasisRegistry] =
    Json.parse(text).flatMap {
      case Json.J.Obj(root) =>
        for
          version <- stringField(root, "version")
          _ <-
            if version == SharedBasisRegistry.FormatVersion then Right(())
            else Left(ArchiveError.InvalidArchive(s"unsupported shared basis registry version '$version'"))
          basesObj <- objectField(root, "bases")
          pairs <- traverse(basesObj.toVector) { case (idText, value) =>
            for
              id <- SharedBasisId(idText)
              entry <- parseEntry(value)
            yield id -> entry
          }
        yield SharedBasisRegistry(pairs.toMap)
      case _ =>
        Left(ArchiveError.InvalidArchive("shared basis registry must be a JSON object"))
    }

  private def entryJson(entry: SharedBasisRegistryEntry): String =
    Json.obj(
      Vector(
        "checksum" -> Json.str(entry.checksum.value),
        "filename" -> Json.str(entry.filename),
        "kind" -> Json.str(entry.kind),
        "mask_checksum" -> Json.str(entry.maskChecksum.value),
        "n_atoms" -> entry.nAtoms.toString,
        "n_voxels" -> entry.nVoxels.toString,
        "created" -> Json.str(entry.created)
      )
    )

  private def parseEntry(value: Json.J): Either[ArchiveError, SharedBasisRegistryEntry] =
    value match
      case Json.J.Obj(fields) =>
        for
          checksum <- stringField(fields, "checksum").flatMap(SharedBasisChecksum.apply)
          filename <- stringField(fields, "filename")
          kind <- stringField(fields, "kind")
          maskChecksum <- stringField(fields, "mask_checksum").flatMap(SharedBasisChecksum.apply)
          nAtoms <- intField(fields, "n_atoms")
          nVoxels <- intField(fields, "n_voxels")
          created <- stringField(fields, "created")
          entry <- catchInvalid("shared basis registry entry")(
            SharedBasisRegistryEntry(checksum, filename, kind, maskChecksum, nAtoms, nVoxels, created)
          )
        yield entry
      case _ =>
        Left(ArchiveError.InvalidArchive("shared basis registry entry must be a JSON object"))

  private def stringField(obj: Map[String, Json.J], key: String): Either[ArchiveError, String] =
    obj.get(key) match
      case Some(Json.J.Str(value)) => Right(value)
      case Some(_) => Left(ArchiveError.InvalidArchive(s"shared basis registry field '$key' must be a string"))
      case None    => Left(ArchiveError.InvalidArchive(s"shared basis registry missing '$key'"))

  private def objectField(obj: Map[String, Json.J], key: String): Either[ArchiveError, Map[String, Json.J]] =
    obj.get(key) match
      case Some(Json.J.Obj(fields)) => Right(fields)
      case Some(_) => Left(ArchiveError.InvalidArchive(s"shared basis registry field '$key' must be an object"))
      case None    => Left(ArchiveError.InvalidArchive(s"shared basis registry missing '$key'"))

  private def intField(obj: Map[String, Json.J], key: String): Either[ArchiveError, Int] =
    obj.get(key) match
      case Some(Json.J.Num(value)) if value.isValidInt && value == value.toInt.toDouble => Right(value.toInt)
      case Some(_) => Left(ArchiveError.InvalidArchive(s"shared basis registry field '$key' must be an integer"))
      case None    => Left(ArchiveError.InvalidArchive(s"shared basis registry missing '$key'"))

private[lna] object Json:
  enum J:
    case Obj(fields: Map[String, J])
    case Arr(values: Vector[J])
    case Str(value: String)
    case Num(value: Double)
    case Bool(value: Boolean)
    case Null

  def parse(text: String): Either[ArchiveError, J] =
    val parser = Parser(text)
    parser.parseValue().flatMap { value =>
      parser.skipWhitespace()
      if parser.atEnd then Right(value)
      else Left(ArchiveError.InvalidArchive(s"unexpected trailing JSON input at offset ${parser.offset}"))
    }

  def obj(fields: Vector[(String, String)]): String =
    fields.map { case (key, value) => s"${str(key)}:$value" }.mkString("{", ",", "}")

  def str(value: String): String =
    val out = new StringBuilder("\"")
    value.foreach {
      case '"'  => out.append("\\\"")
      case '\\' => out.append("\\\\")
      case '\b' => out.append("\\b")
      case '\f' => out.append("\\f")
      case '\n' => out.append("\\n")
      case '\r' => out.append("\\r")
      case '\t' => out.append("\\t")
      case c if c < ' ' => out.append(f"\\u${c.toInt}%04x")
      case c => out.append(c)
    }
    out.append('"').toString

  private final class Parser(input: String):
    private var index = 0

    def offset: Int = index
    def atEnd: Boolean = index >= input.length

    def skipWhitespace(): Unit =
      while !atEnd && input.charAt(index).isWhitespace do index += 1

    def parseValue(): Either[ArchiveError, J] =
      skipWhitespace()
      if atEnd then Left(ArchiveError.InvalidArchive("unexpected end of JSON input"))
      else
        input.charAt(index) match
          case '{' => parseObject()
          case '[' => parseArray()
          case '"' => parseString().map(J.Str.apply)
          case 't' => parseLiteral("true", J.Bool(true))
          case 'f' => parseLiteral("false", J.Bool(false))
          case 'n' => parseLiteral("null", J.Null)
          case c if c == '-' || c.isDigit => parseNumber()
          case c => Left(ArchiveError.InvalidArchive(s"unexpected JSON character '$c' at offset $index"))

    private def parseObject(): Either[ArchiveError, J] =
      index += 1
      skipWhitespace()
      var fields = Map.empty[String, J]
      if consume('}') then return Right(J.Obj(fields))

      var done = false
      while !done do
        skipWhitespace()
        val key = parseString() match
          case Left(err) => return Left(err)
          case Right(k)  => k
        skipWhitespace()
        if !consume(':') then return Left(ArchiveError.InvalidArchive(s"expected ':' after JSON object key at offset $index"))
        val value = parseValue() match
          case Left(err) => return Left(err)
          case Right(v)  => v
        fields = fields.updated(key, value)
        skipWhitespace()
        if consume('}') then done = true
        else if !consume(',') then return Left(ArchiveError.InvalidArchive(s"expected ',' or '}' at JSON offset $index"))

      Right(J.Obj(fields))

    private def parseArray(): Either[ArchiveError, J] =
      index += 1
      skipWhitespace()
      val values = Vector.newBuilder[J]
      if consume(']') then return Right(J.Arr(Vector.empty))

      var done = false
      while !done do
        parseValue() match
          case Left(err) => return Left(err)
          case Right(v)  => values += v
        skipWhitespace()
        if consume(']') then done = true
        else if !consume(',') then return Left(ArchiveError.InvalidArchive(s"expected ',' or ']' at JSON offset $index"))

      Right(J.Arr(values.result()))

    private def parseString(): Either[ArchiveError, String] =
      if !consume('"') then return Left(ArchiveError.InvalidArchive(s"expected JSON string at offset $index"))
      val out = new StringBuilder
      while !atEnd do
        val c = input.charAt(index)
        index += 1
        c match
          case '"' => return Right(out.toString)
          case '\\' =>
            if atEnd then return Left(ArchiveError.InvalidArchive("unterminated JSON escape sequence"))
            val esc = input.charAt(index)
            index += 1
            esc match
              case '"'  => out.append('"')
              case '\\' => out.append('\\')
              case '/'  => out.append('/')
              case 'b'  => out.append('\b')
              case 'f'  => out.append('\f')
              case 'n'  => out.append('\n')
              case 'r'  => out.append('\r')
              case 't'  => out.append('\t')
              case 'u' =>
                if index + 4 > input.length then return Left(ArchiveError.InvalidArchive("short JSON unicode escape"))
                val hex = input.substring(index, index + 4)
                if !hex.forall(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) then
                  return Left(ArchiveError.InvalidArchive(s"invalid JSON unicode escape '$hex'"))
                out.append(Integer.parseInt(hex, 16).toChar)
                index += 4
              case other =>
                return Left(ArchiveError.InvalidArchive(s"invalid JSON escape '\\$other'"))
          case other if other < ' ' =>
            return Left(ArchiveError.InvalidArchive("control character in JSON string"))
          case other =>
            out.append(other)
      Left(ArchiveError.InvalidArchive("unterminated JSON string"))

    private def parseNumber(): Either[ArchiveError, J] =
      val start = index
      if consume('-') then ()
      digits()
      if consume('.') then digits()
      if !atEnd && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
        index += 1
        if !atEnd && (input.charAt(index) == '+' || input.charAt(index) == '-') then index += 1
        digits()
      val text = input.substring(start, index)
      try Right(J.Num(text.toDouble))
      catch case _: NumberFormatException => Left(ArchiveError.InvalidArchive(s"invalid JSON number '$text'"))

    private def digits(): Unit =
      while !atEnd && input.charAt(index).isDigit do index += 1

    private def parseLiteral(text: String, value: J): Either[ArchiveError, J] =
      if input.regionMatches(index, text, 0, text.length) then
        index += text.length
        Right(value)
      else Left(ArchiveError.InvalidArchive(s"expected '$text' at JSON offset $index"))

    private def consume(c: Char): Boolean =
      if !atEnd && input.charAt(index) == c then
        index += 1
        true
      else false

private def traverse[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
  val out = Vector.newBuilder[B]
  val it = values.iterator
  var error = Option.empty[ArchiveError]
  while it.hasNext && error.isEmpty do
    f(it.next()) match
      case Left(err) => error = Some(err)
      case Right(ok) => out += ok
  error.fold(Right(out.result()))(Left(_))

private def catchInvalid[A](label: String)(body: => A): Either[ArchiveError, A] =
  try Right(body)
  catch case e: IllegalArgumentException => Left(ArchiveError.InvalidArchive(s"$label: ${e.getMessage}"))
