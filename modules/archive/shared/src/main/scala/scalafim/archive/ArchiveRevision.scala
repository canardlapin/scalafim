package scalafim.archive

private[archive] object ArchiveIdentity:
  def checked(value: String, label: String): Either[ArchiveError, String] =
    val normalized = value.trim
    if normalized.isEmpty then
      Left(ArchiveError.InvalidArchive(s"$label must be non-empty"))
    else if normalized.exists(character => character.isWhitespace || character.isControl) then
      Left(ArchiveError.InvalidArchive(
        s"$label must not contain whitespace or control characters"
      ))
    else Right(normalized)

opaque type ArchiveRevisionId = String

object ArchiveRevisionId:
  def fromString(value: String): Either[ArchiveError, ArchiveRevisionId] =
    ArchiveIdentity.checked(value, "archive revision id")

  def unsafe(value: String): ArchiveRevisionId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ArchiveRevisionId)
    inline def value: String = id

opaque type ArchiveFormatKey = String

object ArchiveFormatKey:
  val LogicalV1: ArchiveFormatKey =
    "neuroarchive-logical@1"

  def fromString(value: String): Either[ArchiveError, ArchiveFormatKey] =
    ArchiveIdentity.checked(value, "archive format key").flatMap: checked =>
      parse(checked).map(_ => checked)

  def from(
      name: String,
      majorVersion: Int
  ): Either[ArchiveError, ArchiveFormatKey] =
    if !validName(name) then
      Left(ArchiveError.InvalidArchive(
        s"archive format name '$name' must be a lowercase kebab-case identifier"
      ))
    else if majorVersion <= 0 then
      Left(ArchiveError.InvalidArchive(
        "archive format major version must be positive"
      ))
    else Right(s"$name@$majorVersion")

  def unsafe(value: String): ArchiveFormatKey =
    fromString(value).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

  extension (key: ArchiveFormatKey)
    inline def value: String = key

    def name: String =
      key.substring(0, key.lastIndexOf('@'))

    def majorVersion: Int =
      key.substring(key.lastIndexOf('@') + 1).toInt

  private def parse(
      value: String
  ): Either[ArchiveError, (String, Int)] =
    val at = value.lastIndexOf('@')
    if at <= 0 || at == value.length - 1 then
      Left(ArchiveError.InvalidArchive(
        "archive format key must have the form name@major"
      ))
    else
      val name = value.substring(0, at)
      val versionText = value.substring(at + 1)
      versionText.toIntOption match
        case Some(majorVersion)
            if validName(name) &&
              majorVersion > 0 &&
              versionText == majorVersion.toString =>
          Right((name, majorVersion))
        case _ =>
          Left(ArchiveError.InvalidArchive(
            s"invalid major-versioned archive format key '$value'"
          ))

  private def validName(value: String): Boolean =
    value.nonEmpty &&
      value.head.isLower &&
      value.last.isLetterOrDigit &&
      value.forall(character =>
        character.isLower || character.isDigit || character == '-'
      )

opaque type ObjectTypeId = String

object ObjectTypeId:
  def fromString(value: String): Either[ArchiveError, ObjectTypeId] =
    ArchiveIdentity.checked(value, "object type id").flatMap: checked =>
      val slash = checked.indexOf('/')
      if slash <= 0 ||
          slash == checked.length - 1 ||
          checked.indexOf('/', slash + 1) >= 0
      then
        Left(ArchiveError.InvalidArchive(
          "object type id must have the form namespace/name"
        ))
      else
        val namespace = checked.substring(0, slash)
        val name = checked.substring(slash + 1)
        if validNamespace(namespace) && validName(name) then Right(checked)
        else Left(ArchiveError.InvalidArchive(
          s"invalid namespaced object type id '$checked'"
        ))

  def unsafe(value: String): ObjectTypeId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ObjectTypeId)
    inline def value: String = id

    def namespace: String =
      id.substring(0, id.indexOf('/'))

    def name: String =
      id.substring(id.indexOf('/') + 1)

  private def validNamespace(value: String): Boolean =
    val parts = value.split('.')
    parts.length >= 2 &&
      parts.forall(part =>
        part.nonEmpty &&
          part.head.isLetterOrDigit &&
          part.last.isLetterOrDigit &&
          part.forall(character =>
            character.isLetterOrDigit || character == '-'
          )
      )

  private def validName(value: String): Boolean =
    value.nonEmpty &&
      value.head.isLower &&
      value.last.isLetterOrDigit &&
      value.forall(character =>
        character.isLower || character.isDigit || character == '-'
      )

opaque type RepresentationKey = String

object RepresentationKey:
  def fromString(value: String): Either[ArchiveError, RepresentationKey] =
    ArchiveIdentity.checked(value, "representation key").flatMap: checked =>
      parse(checked).map(_ => checked)

  def from(
      namespace: String,
      name: String,
      majorVersion: Int
  ): Either[ArchiveError, RepresentationKey] =
    if !validNamespace(namespace) then
      Left(ArchiveError.InvalidArchive(
        s"representation namespace '$namespace' must be a dot-separated identifier"
      ))
    else if !validName(name) then
      Left(ArchiveError.InvalidArchive(
        s"representation name '$name' must be a lowercase kebab-case identifier"
      ))
    else if majorVersion <= 0 then
      Left(ArchiveError.InvalidArchive(
        "representation major version must be positive"
      ))
    else Right(s"$namespace/$name@$majorVersion")

  def unsafe(value: String): RepresentationKey =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (key: RepresentationKey)
    inline def value: String = key

    def namespace: String =
      key.substring(0, key.indexOf('/'))

    def name: String =
      val slash = key.indexOf('/')
      key.substring(slash + 1, key.lastIndexOf('@'))

    def majorVersion: Int =
      key.substring(key.lastIndexOf('@') + 1).toInt

  private def parse(
      value: String
  ): Either[ArchiveError, (String, String, Int)] =
    val slash = value.indexOf('/')
    val at = value.lastIndexOf('@')
    if slash <= 0 || at <= slash + 1 || at == value.length - 1 ||
        value.indexOf('/', slash + 1) >= 0
    then
      Left(ArchiveError.InvalidArchive(
        "representation key must have the form namespace/name@major"
      ))
    else
      val namespace = value.substring(0, slash)
      val name = value.substring(slash + 1, at)
      val versionText = value.substring(at + 1)
      versionText.toIntOption match
        case Some(majorVersion)
            if validNamespace(namespace) &&
              validName(name) &&
              majorVersion > 0 &&
              versionText == majorVersion.toString =>
          Right((namespace, name, majorVersion))
        case _ =>
          Left(ArchiveError.InvalidArchive(
            s"invalid namespaced major-versioned representation key '$value'"
          ))

  private def validNamespace(value: String): Boolean =
    val parts = value.split('.')
    parts.length >= 2 && parts.forall(validIdentifierPart)

  private def validName(value: String): Boolean =
    value.nonEmpty &&
      value.head.isLower &&
      value.last.isLetterOrDigit &&
      value.forall(character =>
        character.isLower || character.isDigit || character == '-'
      )

  private def validIdentifierPart(value: String): Boolean =
    value.nonEmpty &&
      value.head.isLetterOrDigit &&
      value.last.isLetterOrDigit &&
      value.forall(character =>
        character.isLetterOrDigit || character == '-'
      )

object RepresentationMetadata:
  val DescriptorHeader: String =
    "rra.representation.descriptor-v1"

  val OutputSchemaHeader: String =
    "rra.response.schema-v1"

  val DescriptorAttribute: CanonicalKey =
    CanonicalKey.unsafe("representation-descriptor")

  val OutputSchemaAttribute: CanonicalKey =
    CanonicalKey.unsafe("response-schema")

opaque type PayloadId = String

object PayloadId:
  def fromString(value: String): Either[ArchiveError, PayloadId] =
    if value.isEmpty then Left(ArchiveError.InvalidArchive("payload id must be non-empty"))
    else if value.exists(_.isControl) then
      Left(ArchiveError.InvalidArchive("payload id must not contain control characters"))
    else Right(value)

  def unsafe(value: String): PayloadId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: PayloadId)
    inline def value: String = id

opaque type PayloadRoleId = String

object PayloadRoleId:
  def fromString(value: String): Either[ArchiveError, PayloadRoleId] =
    ArchiveIdentity.checked(value, "payload role id")

  def from(
      namespace: String,
      name: String
  ): Either[ArchiveError, PayloadRoleId] =
    RepresentationKey
      .from(namespace, name, 1)
      .map(_.value.stripSuffix("@1"))

  def unsafe(value: String): PayloadRoleId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: PayloadRoleId)
    inline def value: String = id

    def isNamespaced: Boolean =
      val slash = id.indexOf('/')
      slash > 0 && slash < id.length - 1

opaque type ScalarTypeId = String

object ScalarTypeId:
  def fromString(value: String): Either[ArchiveError, ScalarTypeId] =
    ArchiveIdentity.checked(value, "scalar type id")

  def unsafe(value: String): ScalarTypeId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ScalarTypeId)
    inline def value: String = id

opaque type CanonicalKey = String

object CanonicalKey:
  def fromString(value: String): Either[ArchiveError, CanonicalKey] =
    if value.isEmpty then Left(ArchiveError.InvalidArchive("canonical object key must be non-empty"))
    else if value.exists(_.isControl) then
      Left(ArchiveError.InvalidArchive("canonical object key must not contain control characters"))
    else Right(value)

  def unsafe(value: String): CanonicalKey =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (key: CanonicalKey)
    inline def value: String = key

sealed trait CanonicalValue

object CanonicalValue:
  case object Null extends CanonicalValue

  final case class Boolean private[archive] (value: scala.Boolean) extends CanonicalValue

  final case class String private[archive] (value: scala.Predef.String)
      extends CanonicalValue

  final case class Int64 private[archive] (value: Long) extends CanonicalValue

  final case class Float64Bits private[archive] (bits: Long) extends CanonicalValue:
    def value: Double = java.lang.Double.longBitsToDouble(bits)

  final case class Array private[archive] (values: Vector[CanonicalValue])
      extends CanonicalValue

  final class Object private[archive] (
      val entries: Vector[(CanonicalKey, CanonicalValue)]
  ) extends CanonicalValue:
    override def equals(other: Any): scala.Boolean =
      other match
        case that: Object => entries == that.entries
        case _ => false

    override def hashCode(): Int = entries.hashCode()

  object Object:
    def from(
        entries: Iterable[(CanonicalKey, CanonicalValue)]
    ): Either[ArchiveError, Object] =
      val copied = entries.toVector
      val duplicate =
        copied
          .groupBy(_._1.value)
          .collectFirst:
            case (key, values) if values.lengthCompare(1) > 0 => key
      duplicate match
        case Some(key) =>
          Left(ArchiveError.InvalidArchive(s"duplicate canonical object key '$key'"))
        case None =>
          Right(new Object(copied.sortWith: (left, right) =>
            compareUtf8(left._1.value, right._1.value) < 0
          ))

    def empty: Object =
      new Object(Vector.empty)

  def boolean(value: scala.Boolean): CanonicalValue =
    Boolean(value)

  def string(value: scala.Predef.String): CanonicalValue =
    String(value)

  def int64(value: Long): CanonicalValue =
    Int64(value)

  def float64(value: Double): CanonicalValue =
    Float64Bits(java.lang.Double.doubleToRawLongBits(value))

  def float64Bits(bits: Long): CanonicalValue =
    Float64Bits(bits)

  def array(values: Iterable[CanonicalValue]): CanonicalValue =
    Array(values.toVector)

  def obj(
      entries: Iterable[(CanonicalKey, CanonicalValue)]
  ): Either[ArchiveError, CanonicalValue] =
    Object.from(entries)

  private def compareUtf8(left: scala.Predef.String, right: scala.Predef.String): Int =
    val leftBytes = left.getBytes("UTF-8")
    val rightBytes = right.getBytes("UTF-8")
    val common = math.min(leftBytes.length, rightBytes.length)
    var index = 0
    while index < common do
      val comparison =
        java.lang.Integer.compare(
          java.lang.Byte.toUnsignedInt(leftBytes(index)),
          java.lang.Byte.toUnsignedInt(rightBytes(index))
        )
      if comparison != 0 then return comparison
      index += 1
    java.lang.Integer.compare(leftBytes.length, rightBytes.length)

final case class ContentDigest private (
    algorithm: String,
    value: String
):
  def render: String = s"$algorithm:$value"

object ContentDigest:
  def from(
      algorithm: String,
      value: String
  ): Either[ArchiveError, ContentDigest] =
    for
      checkedAlgorithm <- ArchiveIdentity.checked(algorithm.toLowerCase, "digest algorithm")
      checkedValue <- ArchiveIdentity.checked(value.toLowerCase, "digest value")
      _ <-
        if checkedValue.forall(character => Character.digit(character, 16) >= 0) then Right(())
        else Left(ArchiveError.InvalidArchive("digest value must be hexadecimal"))
    yield ContentDigest(checkedAlgorithm, checkedValue)

  def sha256(value: String): Either[ArchiveError, ContentDigest] =
    if value.length != 64 then
      Left(ArchiveError.InvalidArchive("SHA-256 digest must contain 64 hexadecimal digits"))
    else from("sha256", value)

  def unsafeSha256(value: String): ContentDigest =
    sha256(value).fold(error => throw new IllegalArgumentException(error.message), identity)

opaque type IncompleteReason = String

object IncompleteReason:
  def fromString(value: String): Either[ArchiveError, IncompleteReason] =
    val normalized = value.trim
    if normalized.isEmpty then
      Left(ArchiveError.InvalidArchive("incomplete publication reason must be non-empty"))
    else Right(normalized)

  def unsafe(value: String): IncompleteReason =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (reason: IncompleteReason)
    inline def value: String = reason

enum PublicationStatus:
  case Staging
  case Published(rootDigest: ContentDigest)
  case Incomplete(reason: IncompleteReason)

final case class ObjectKey private (
    objectType: ObjectTypeId,
    schemaMajor: Int,
    representation: Option[RepresentationKey]
)

object ObjectKey:
  def from(
      objectType: ObjectTypeId,
      schemaMajor: Int,
      representation: Option[RepresentationKey]
  ): Either[ArchiveError, ObjectKey] =
    if schemaMajor <= 0 then
      Left(ArchiveError.InvalidArchive("object schema major version must be positive"))
    else Right(ObjectKey(objectType, schemaMajor, representation))

  def unsafe(
      objectType: ObjectTypeId,
      schemaMajor: Int,
      representation: Option[RepresentationKey]
  ): ObjectKey =
    from(objectType, schemaMajor, representation)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class PersistedRepresentation(
    key: RepresentationKey,
    descriptor: CanonicalValue,
    outputSchema: CanonicalValue
)

final case class LogicalPayloadIdentity(
    id: PayloadId,
    role: PayloadRoleId,
    scalarType: ScalarTypeId,
    shape: Vector[Long]
)

final case class PayloadDescriptor private (
    id: PayloadId,
    role: PayloadRoleId,
    scalarType: ScalarTypeId,
    shape: Vector[Long],
    attributes: CanonicalValue
):
  def logicalIdentity: LogicalPayloadIdentity =
    LogicalPayloadIdentity(id, role, scalarType, shape)

object PayloadDescriptor:
  def from(
      id: PayloadId,
      role: PayloadRoleId,
      scalarType: ScalarTypeId,
      shape: Iterable[Long],
      attributes: CanonicalValue = CanonicalValue.Object.empty
  ): Either[ArchiveError, PayloadDescriptor] =
    val copied = shape.toVector
    if copied.isEmpty then Left(ArchiveError.InvalidArchive("payload shape must be non-empty"))
    else if copied.exists(_ <= 0L) then
      Left(ArchiveError.InvalidArchive("payload shape dimensions must be positive"))
    else Right(PayloadDescriptor(id, role, scalarType, copied, attributes))

final case class IntegrityEntry(
    payload: PayloadId,
    digest: ContentDigest
)

final case class IntegrityManifest private (
    entries: Vector[IntegrityEntry]
)

object IntegrityManifest:
  val Empty: IntegrityManifest = IntegrityManifest(Vector.empty)

  def from(entries: Iterable[IntegrityEntry]): Either[ArchiveError, IntegrityManifest] =
    val copied = entries.toVector
    val ids = copied.map(_.payload.value)
    if ids.distinct.length != ids.length then
      Left(ArchiveError.InvalidArchive("integrity manifest contains duplicate payload ids"))
    else Right(IntegrityManifest(copied.sortBy(_.payload.value)))

final case class ArchiveManifest private (
    format: ArchiveFormatKey,
    key: ObjectKey,
    representation: Option[PersistedRepresentation],
    attributes: CanonicalValue,
    payloads: Vector[PayloadDescriptor],
    integrity: IntegrityManifest
):
  def payload(id: PayloadId): Option[PayloadDescriptor] =
    payloads.find(_.id == id)

object ArchiveManifest:
  def from(
      format: ArchiveFormatKey,
      key: ObjectKey,
      representation: Option[PersistedRepresentation],
      attributes: CanonicalValue,
      payloads: Iterable[PayloadDescriptor],
      integrity: IntegrityManifest
  ): Either[ArchiveError, ArchiveManifest] =
    validateRepresentation(key, representation).flatMap: _ =>
      checked(
        format,
        key,
        representation,
        attributes,
        payloads,
        integrity
      )

  def from(
      key: ObjectKey,
      attributes: CanonicalValue,
      payloads: Iterable[PayloadDescriptor],
      integrity: IntegrityManifest = IntegrityManifest.Empty
  ): Either[ArchiveError, ArchiveManifest] =
    val representation =
      key.representation.map: representationKey =>
        PersistedRepresentation(
          representationKey,
          attribute(
            attributes,
            RepresentationMetadata.DescriptorAttribute
          ).getOrElse(CanonicalValue.Null),
          attribute(
            attributes,
            RepresentationMetadata.OutputSchemaAttribute
          ).getOrElse(CanonicalValue.Null)
        )
    checked(
      ArchiveFormatKey.LogicalV1,
      key,
      representation,
      attributes,
      payloads,
      integrity
    )

  private def checked(
      format: ArchiveFormatKey,
      key: ObjectKey,
      representation: Option[PersistedRepresentation],
      attributes: CanonicalValue,
      payloads: Iterable[PayloadDescriptor],
      integrity: IntegrityManifest
  ): Either[ArchiveError, ArchiveManifest] =
    val copied = payloads.toVector
    val ids = copied.map(_.id.value)
    val declared = ids.toSet
    val unknownIntegrity =
      integrity.entries.collectFirst:
        case entry if !declared.contains(entry.payload.value) => entry.payload
    if ids.distinct.length != ids.length then
      Left(ArchiveError.InvalidArchive("archive manifest contains duplicate payload ids"))
    else unknownIntegrity match
      case Some(payload) =>
        Left(ArchiveError.InvalidArchive(
          s"integrity entry references undeclared payload '${payload.value}'"
        ))
      case None =>
        Right(ArchiveManifest(
          format,
          key,
          representation,
          attributes,
          copied.sortBy(_.id.value),
          integrity
        ))

  private def validateRepresentation(
      key: ObjectKey,
      representation: Option[PersistedRepresentation]
  ): Either[ArchiveError, Unit] =
    (key.representation, representation) match
      case (None, None) =>
        Right(())
      case (Some(expected), Some(found)) if expected == found.key =>
        Right(())
      case (Some(expected), Some(found)) =>
        Left(ArchiveError.InvalidArchive(
          s"object representation '${expected.value}' does not match " +
            s"descriptor representation '${found.key.value}'"
        ))
      case (Some(expected), None) =>
        Left(ArchiveError.InvalidArchive(
          s"object representation '${expected.value}' lacks a descriptor envelope"
        ))
      case (None, Some(found)) =>
        Left(ArchiveError.InvalidArchive(
          s"descriptor representation '${found.key.value}' is absent from the object key"
        ))

  private def attribute(
      attributes: CanonicalValue,
      key: CanonicalKey
  ): Option[CanonicalValue] =
    attributes match
      case value: CanonicalValue.Object =>
        value.entries.find(_._1 == key).map(_._2)
      case _ =>
        None

final case class ArchiveProvenance(
    creator: CreatorId,
    attributes: CanonicalValue = CanonicalValue.Object.empty
)

final class ArchiveRevision private (
    val id: ArchiveRevisionId,
    val manifest: ArchiveManifest,
    val publication: PublicationStatus,
    val provenance: ArchiveProvenance
):
  def requireRepresentation(
      supported: Iterable[RepresentationKey]
  ): Either[ArchiveError, RepresentationKey] =
    manifest.key.representation match
      case Some(found) if supported.iterator.contains(found) =>
        Right(found)
      case Some(found) =>
        Left(ArchiveError.UnsupportedRepresentation(found, supported.toVector))
      case None =>
        Left(ArchiveError.InvalidArchive("archive object has no representation key"))

  override def equals(other: Any): scala.Boolean =
    other match
      case that: ArchiveRevision =>
        id == that.id &&
          manifest == that.manifest &&
          publication == that.publication &&
          provenance == that.provenance
      case _ => false

  override def hashCode(): Int =
    var result = id.hashCode()
    result = 31 * result + manifest.hashCode()
    result = 31 * result + publication.hashCode()
    31 * result + provenance.hashCode()

object ArchiveRevision:
  def from(
      id: ArchiveRevisionId,
      manifest: ArchiveManifest,
      publication: PublicationStatus,
      provenance: ArchiveProvenance
  ): Either[ArchiveError, ArchiveRevision] =
    Right(new ArchiveRevision(id, manifest, publication, provenance))
