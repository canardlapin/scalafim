package scalafim.fmri.mvpa

import scala.collection.mutable.ArrayBuilder

enum AxisIdentityError:
  case InvalidProtocol(found: String)
  case InvalidIdentifier(kind: String, value: String, detail: String)
  case InvalidText(kind: String, value: String, detail: String)
  case EmptyAxis
  case DuplicateKey(key: String)
  case DuplicateDescriptorField(name: String)
  case InvalidScale(detail: String)
  case InvalidFingerprint(value: String)
  case FingerprintMismatch(expected: AxisFingerprint, actual: AxisFingerprint)

  def message: String =
    this match
      case InvalidProtocol(found) =>
        s"axis identity protocol '$found' is not ${AxisIdentity.Protocol}"
      case InvalidIdentifier(kind, value, detail) =>
        s"invalid $kind '$value': $detail"
      case InvalidText(kind, value, detail) =>
        s"invalid $kind '$value': $detail"
      case EmptyAxis =>
        "axis identity must contain at least one ordered key"
      case DuplicateKey(key) =>
        s"axis identity contains duplicate ordered key '$key'"
      case DuplicateDescriptorField(name) =>
        s"axis descriptor contains duplicate field '$name'"
      case InvalidScale(detail) =>
        s"invalid axis scale: $detail"
      case InvalidFingerprint(value) =>
        s"invalid axis fingerprint '$value'"
      case FingerprintMismatch(expected, actual) =>
        s"axis fingerprint ${actual.value} does not match canonical identity ${expected.value}"

opaque type AxisId = String

object AxisId:
  def apply(value: String): Either[AxisIdentityError, AxisId] =
    AxisText.identifier("axis id", value)

  private[mvpa] def unsafe(value: String): AxisId =
    value

  extension (id: AxisId) inline def value: String = id

opaque type AxisPurpose = String

object AxisPurpose:
  val Samples: AxisPurpose = "samples"
  val NeuralFeatures: AxisPurpose = "neural-features"
  val Effects: AxisPurpose = "effects"
  val Partitions: AxisPurpose = "partitions"
  val Components: AxisPurpose = "components"
  val Covariates: AxisPurpose = "covariates"
  val Classes: AxisPurpose = "classes"

  def named(value: String): Either[AxisIdentityError, AxisPurpose] =
    AxisText
      .identifier("axis purpose", value)
      .flatMap: valid =>
        if valid == valid.toLowerCase then Right(valid)
        else Left(AxisIdentityError.InvalidIdentifier("axis purpose", value, "must be lowercase"))

  private[mvpa] def unsafe(value: String): AxisPurpose =
    value

  extension (purpose: AxisPurpose) inline def value: String = purpose

opaque type AxisKey = String

object AxisKey:
  def apply(value: String): Either[AxisIdentityError, AxisKey] =
    AxisText.exact("axis key", value)

  private[mvpa] def unsafe(value: String): AxisKey =
    value

  extension (key: AxisKey) inline def value: String = key

opaque type AxisUnits = String

object AxisUnits:
  def apply(value: String): Either[AxisIdentityError, AxisUnits] =
    AxisText.exact("axis units", value)

  private[mvpa] def unsafe(value: String): AxisUnits =
    value

  extension (units: AxisUnits) inline def value: String = units

opaque type AxisFingerprint = String

object AxisFingerprint:
  private val Prefix = "scalafim-mvpa-axis-v1-"

  def apply(value: String): Either[AxisIdentityError, AxisFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(AxisIdentityError.InvalidFingerprint(value))

  private[mvpa] def fromDigest(digest: String): AxisFingerprint =
    Prefix + digest

  private[mvpa] def unsafe(value: String): AxisFingerprint =
    value

  extension (fingerprint: AxisFingerprint) inline def value: String = fingerprint

final case class AxisDescriptorField private (name: String, value: String)

object AxisDescriptorField:
  def apply(name: String, value: String): Either[AxisIdentityError, AxisDescriptorField] =
    for
      validName <- AxisText.identifier("axis descriptor field name", name).map(_.toLowerCase)
      validValue <- AxisText.exact("axis descriptor field value", value)
    yield new AxisDescriptorField(validName, validValue)

  private[mvpa] def unsafe(name: String, value: String): AxisDescriptorField =
    new AxisDescriptorField(name.toLowerCase, value)

final class CoordinateBasis private (
    val kind: String,
    val fields: Vector[AxisDescriptorField]
):
  override def equals(other: Any): Boolean =
    other match
      case that: CoordinateBasis => kind == that.kind && fields == that.fields
      case _                     => false

  override def hashCode(): Int =
    31 * kind.hashCode + fields.hashCode

  override def toString: String =
    s"CoordinateBasis($kind,${fields.mkString("[", ",", "]")})"

object CoordinateBasis:
  def apply(
      kind: String,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[AxisIdentityError, CoordinateBasis] =
    for
      validKind <- AxisText.identifier("coordinate basis kind", kind).map(_.toLowerCase)
      validFields <- descriptorFields(fields)
    yield new CoordinateBasis(validKind, validFields)

  private[mvpa] def unsafe(kind: String, fields: (String, String)*): CoordinateBasis =
    new CoordinateBasis(
      kind.toLowerCase,
      fields.iterator
        .map((name, value) => AxisDescriptorField.unsafe(name, value))
        .toVector
        .sortBy(_.name)
    )

  private[mvpa] def descriptorFields(
      fields: Seq[(String, String)]
  ): Either[AxisIdentityError, Vector[AxisDescriptorField]] =
    val parsed = Vector.newBuilder[AxisDescriptorField]
    val iterator = fields.iterator
    while iterator.hasNext do
      val (name, value) = iterator.next()
      AxisDescriptorField(name, value) match
        case Left(error)  => return Left(error)
        case Right(field) => parsed += field
    val ordered = parsed.result().sortBy(_.name)
    ordered
      .sliding(2)
      .collectFirst:
        case Vector(left, right) if left.name == right.name => left.name
    match
      case Some(duplicate) => Left(AxisIdentityError.DuplicateDescriptorField(duplicate))
      case None            => Right(ordered)

sealed trait AxisScale:
  def kind: String
  private[mvpa] def record: AxisScaleRecord

object AxisScale:
  private case object NominalScale extends AxisScale:
    override val kind: String = "nominal"
    override private[mvpa] val record: AxisScaleRecord = AxisScaleRecord.Nominal

  private final case class AffineScale(origin: Double, step: Double) extends AxisScale:
    override val kind: String = "affine"
    override private[mvpa] val record: AxisScaleRecord = AxisScaleRecord.Affine(origin, step)

  private final case class NamedScale(
      override val kind: String,
      fields: Vector[AxisDescriptorField]
  ) extends AxisScale:
    override private[mvpa] val record: AxisScaleRecord =
      AxisScaleRecord.Named(kind, fields.map(field => field.name -> field.value))

  val nominal: AxisScale = NominalScale

  def affine(origin: Double, step: Double): Either[AxisIdentityError, AxisScale] =
    if !origin.isFinite then Left(AxisIdentityError.InvalidScale("affine origin must be finite"))
    else if !step.isFinite || step == 0.0 then
      Left(AxisIdentityError.InvalidScale("affine step must be finite and non-zero"))
    else Right(AffineScale(canonicalZero(origin), canonicalZero(step)))

  def named(
      kind: String,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[AxisIdentityError, AxisScale] =
    for
      validKind <- AxisText.identifier("axis scale kind", kind).map(_.toLowerCase)
      _ <-
        if validKind == "nominal" || validKind == "affine" then
          Left(AxisIdentityError.InvalidScale(s"reserved scale kind '$validKind' must use its typed constructor"))
        else Right(())
      validFields <- CoordinateBasis.descriptorFields(fields)
    yield NamedScale(validKind, validFields)

  private def canonicalZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

final case class CoordinateProvenance private (
    source: String,
    revision: String,
    derivation: Vector[String]
)

object CoordinateProvenance:
  def apply(
      source: String,
      revision: String,
      derivation: Seq[String] = Vector.empty
  ): Either[AxisIdentityError, CoordinateProvenance] =
    for
      validSource <- AxisText.exact("coordinate provenance source", source)
      validRevision <- AxisText.exact("coordinate provenance revision", revision)
      validDerivation <- AxisText.exactVector("coordinate provenance derivation", derivation)
    yield new CoordinateProvenance(validSource, validRevision, validDerivation)

  private[mvpa] def unsafe(source: String, revision: String, derivation: String*): CoordinateProvenance =
    new CoordinateProvenance(source, revision, derivation.toVector)

enum AxisScaleRecord:
  case Nominal
  case Affine(origin: Double, step: Double)
  case Named(kind: String, fields: Vector[(String, String)])

/** Language-neutral trust-boundary record. Unlike [[AxisIdentity]], this value may be malformed and must pass
  * [[AxisIdentity.decode]].
  */
final case class AxisIdentityRecord(
    protocol: String,
    id: String,
    purpose: String,
    orderedKeys: Vector[String],
    basisKind: String,
    basisFields: Vector[(String, String)],
    units: Option[String],
    scale: AxisScaleRecord,
    coordinateSource: String,
    coordinateRevision: String,
    coordinateDerivation: Vector[String],
    fingerprint: String
)

/** Descriptive evidence that deliberately does not participate in axis compatibility.
  */
final case class AxisMetadata(
    valueProvenance: Vector[String] = Vector.empty,
    tags: Set[String] = Set.empty
)

final class AxisIdentity private (
    val id: AxisId,
    val purpose: AxisPurpose,
    val orderedKeys: Vector[AxisKey],
    val basis: CoordinateBasis,
    val units: Option[AxisUnits],
    val scale: AxisScale,
    val coordinateProvenance: CoordinateProvenance,
    val fingerprint: AxisFingerprint
):
  def size: Int = orderedKeys.length

  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(AxisIdentity.canonicalBytes(this))

  def canonicalHex: String =
    AxisDigest.hex(AxisIdentity.canonicalBytes(this))

  def toRecord: AxisIdentityRecord =
    AxisIdentityRecord(
      protocol = AxisIdentity.Protocol,
      id = id.value,
      purpose = purpose.value,
      orderedKeys = orderedKeys.map(_.value),
      basisKind = basis.kind,
      basisFields = basis.fields.map(field => field.name -> field.value),
      units = units.map(_.value),
      scale = scale.record,
      coordinateSource = coordinateProvenance.source,
      coordinateRevision = coordinateProvenance.revision,
      coordinateDerivation = coordinateProvenance.derivation,
      fingerprint = fingerprint.value
    )

  override def equals(other: Any): Boolean =
    other match
      case that: AxisIdentity =>
        id == that.id &&
        purpose == that.purpose &&
        orderedKeys == that.orderedKeys &&
        basis == that.basis &&
        units == that.units &&
        scale == that.scale &&
        coordinateProvenance == that.coordinateProvenance &&
        fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

  override def toString: String =
    s"AxisIdentity(${id.value},${purpose.value},$size,${fingerprint.value})"

object AxisIdentity:
  val Protocol = "scalafim-mvpa-axis/v1"
  private val ReindexingProtocol = "scalafim-mvpa-axis-reindex/v1"

  def apply(
      id: AxisId,
      purpose: AxisPurpose,
      orderedKeys: Seq[AxisKey],
      basis: CoordinateBasis,
      units: Option[AxisUnits],
      scale: AxisScale,
      coordinateProvenance: CoordinateProvenance
  ): Either[AxisIdentityError, AxisIdentity] =
    val keys = orderedKeys.toVector
    if keys.isEmpty then Left(AxisIdentityError.EmptyAxis)
    else
      firstDuplicate(keys.map(_.value)) match
        case Some(duplicate) => Left(AxisIdentityError.DuplicateKey(duplicate))
        case None            =>
          val provisional = new AxisIdentity(
            id,
            purpose,
            keys,
            basis,
            units,
            scale,
            coordinateProvenance,
            AxisFingerprint.fromDigest("0" * 64)
          )
          val digest = AxisDigest.sha256Hex(canonicalBytes(provisional))
          Right(
            new AxisIdentity(
              id,
              purpose,
              keys,
              basis,
              units,
              scale,
              coordinateProvenance,
              AxisFingerprint.fromDigest(digest)
            )
          )

  def decode(record: AxisIdentityRecord): Either[AxisIdentityError, AxisIdentity] =
    if record.protocol != Protocol then Left(AxisIdentityError.InvalidProtocol(record.protocol))
    else
      for
        id <- AxisId(record.id)
        purpose <- AxisPurpose.named(record.purpose)
        keys <- parseKeys(record.orderedKeys)
        basis <- CoordinateBasis(record.basisKind, record.basisFields)
        units <- parseUnits(record.units)
        scale <- parseScale(record.scale)
        provenance <- CoordinateProvenance(
          record.coordinateSource,
          record.coordinateRevision,
          record.coordinateDerivation
        )
        decodedFingerprint <- AxisFingerprint(record.fingerprint)
        identity <- AxisIdentity(id, purpose, keys, basis, units, scale, provenance)
        _ <-
          if decodedFingerprint == identity.fingerprint then Right(())
          else Left(AxisIdentityError.FingerprintMismatch(identity.fingerprint, decodedFingerprint))
      yield identity

  private def parseKeys(values: Vector[String]): Either[AxisIdentityError, Vector[AxisKey]] =
    val out = Vector.newBuilder[AxisKey]
    var index = 0
    while index < values.length do
      AxisKey(values(index)) match
        case Left(error) => return Left(error)
        case Right(key)  => out += key
      index += 1
    Right(out.result())

  private def parseUnits(value: Option[String]): Either[AxisIdentityError, Option[AxisUnits]] =
    value match
      case None        => Right(None)
      case Some(units) => AxisUnits(units).map(Some(_))

  private def parseScale(record: AxisScaleRecord): Either[AxisIdentityError, AxisScale] =
    record match
      case AxisScaleRecord.Nominal =>
        Right(AxisScale.nominal)
      case AxisScaleRecord.Affine(origin, step) =>
        AxisScale.affine(origin, step)
      case AxisScaleRecord.Named(kind, fields) =>
        AxisScale.named(kind, fields)

  private def firstDuplicate(values: Vector[String]): Option[String] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    var index = 0
    while index < values.length do
      val value = values(index)
      if seen.contains(value) then return Some(value)
      seen += value
      index += 1
    None

  /** Derive an axis identity from an exact, kind-preserving finite reindexing. The human-facing parent id is not
    * overloaded as a content address; the child id is a fixed-size digest of the parent identity, operation kind, and
    * complete ordinal map.
    */
  private[mvpa] def reindexed(
      parent: AxisIdentity,
      kind: String,
      orderedKeys: Vector[AxisKey],
      sourcePositions: IArray[Int]
  ): Either[AxisIdentityError, AxisIdentity] =
    val writer = CanonicalWriter()
    writer.string(ReindexingProtocol)
    writer.string(parent.fingerprint.value)
    writer.string(kind)
    writer.int(sourcePositions.length)
    var position = 0
    while position < sourcePositions.length do
      writer.int(sourcePositions(position))
      position += 1
    val derivedId = AxisId.unsafe(s"reindexed-${AxisDigest.sha256Hex(writer.result())}")
    AxisIdentity(
      derivedId,
      parent.purpose,
      orderedKeys,
      parent.basis,
      parent.units,
      parent.scale,
      parent.coordinateProvenance
    )

  /** Canonical v1 wire image.
    *
    * Integers and IEEE-754 bit patterns are little-endian. Strings are UTF-8 prefixed by their byte length. Optional
    * and sum values begin with a one-byte discriminator. Descriptor fields are sorted by validated field name before
    * they reach this encoder.
    */
  private def canonicalBytes(identity: AxisIdentity): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(identity.id.value)
    writer.string(identity.purpose.value)
    writer.int(identity.orderedKeys.length)
    identity.orderedKeys.foreach(key => writer.string(key.value))
    writer.string(identity.basis.kind)
    writer.fields(identity.basis.fields)
    identity.units match
      case None =>
        writer.byte(0)
      case Some(units) =>
        writer.byte(1)
        writer.string(units.value)
    identity.scale.record match
      case AxisScaleRecord.Nominal =>
        writer.byte(0)
      case AxisScaleRecord.Affine(origin, step) =>
        writer.byte(1)
        writer.double(origin)
        writer.double(step)
      case AxisScaleRecord.Named(kind, fields) =>
        writer.byte(2)
        writer.string(kind)
        writer.rawFields(fields)
    writer.string(identity.coordinateProvenance.source)
    writer.string(identity.coordinateProvenance.revision)
    writer.int(identity.coordinateProvenance.derivation.length)
    identity.coordinateProvenance.derivation.foreach(writer.string)
    writer.result()

private[mvpa] object AxisText:
  def identifier(kind: String, value: String): Either[AxisIdentityError, String] =
    if value.isEmpty then Left(AxisIdentityError.InvalidIdentifier(kind, value, "must be non-empty"))
    else if !value.forall(isIdentifierCharacter) then
      Left(
        AxisIdentityError.InvalidIdentifier(
          kind,
          value,
          "may contain only letters, digits, '.', '_', and '-'"
        )
      )
    else Right(value)

  def exact(kind: String, value: String): Either[AxisIdentityError, String] =
    if value.isEmpty then Left(AxisIdentityError.InvalidText(kind, value, "must be non-empty"))
    else if value != value.trim then
      Left(AxisIdentityError.InvalidText(kind, value, "must not contain leading or trailing whitespace"))
    else if value.exists(character => character == 0.toChar || character.isControl) then
      Left(AxisIdentityError.InvalidText(kind, value, "must not contain control characters"))
    else Right(value)

  def exactVector(kind: String, values: Seq[String]): Either[AxisIdentityError, Vector[String]] =
    val out = Vector.newBuilder[String]
    val iterator = values.iterator
    while iterator.hasNext do
      exact(kind, iterator.next()) match
        case Left(error)  => return Left(error)
        case Right(value) => out += value
    Right(out.result())

  def isLowerHexDigit(character: Char): Boolean =
    character >= '0' && character <= '9' || character >= 'a' && character <= 'f'

  private def isIdentifierCharacter(character: Char): Boolean =
    character.isLetterOrDigit || character == '.' || character == '_' || character == '-'

private[mvpa] final class CanonicalWriter:
  private val builder = ArrayBuilder.make[Byte]

  def byte(value: Int): Unit =
    builder += value.toByte

  def int(value: Int): Unit =
    byte(value)
    byte(value >>> 8)
    byte(value >>> 16)
    byte(value >>> 24)

  def double(value: Double): Unit =
    val bits = java.lang.Double.doubleToLongBits(value)
    byte(bits.toInt)
    byte((bits >>> 8).toInt)
    byte((bits >>> 16).toInt)
    byte((bits >>> 24).toInt)
    byte((bits >>> 32).toInt)
    byte((bits >>> 40).toInt)
    byte((bits >>> 48).toInt)
    byte((bits >>> 56).toInt)

  def string(value: String): Unit =
    val bytes = value.getBytes("UTF-8")
    int(bytes.length)
    builder ++= bytes

  def fields(values: Vector[AxisDescriptorField]): Unit =
    int(values.length)
    values.foreach: field =>
      string(field.name)
      string(field.value)

  def rawFields(values: Vector[(String, String)]): Unit =
    int(values.length)
    values.foreach: (name, value) =>
      string(name)
      string(value)

  def result(): Array[Byte] =
    builder.result()

private[mvpa] object CanonicalWriter:
  def apply(): CanonicalWriter =
    new CanonicalWriter

private[mvpa] object AxisDigest:
  def sha256Hex(bytes: Array[Byte]): String =
    val digest = Sha256()
    digest.update(bytes)
    hex(digest.finish())

  def hex(bytes: Array[Byte]): String =
    bytes.map(value => f"${value & 0xff}%02x").mkString

  private final class Sha256:
    private val state = Array(
      0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    )
    private val block = new Array[Byte](64)
    private val words = new Array[Int](64)
    private var blockLength = 0
    private var totalLength = 0L
    private var finished = false

    def update(bytes: Array[Byte]): Unit =
      var index = 0
      while index < bytes.length do
        updateByte(bytes(index))
        index += 1

    def finish(): Array[Byte] =
      require(!finished, "SHA-256 digest is already finalized")
      val bitLength = totalLength * 8L
      updateByte(0x80)
      while blockLength != 56 do updateByte(0)
      var shift = 56
      while shift >= 0 do
        updateByte((bitLength >>> shift).toInt)
        shift -= 8

      val out = new Array[Byte](32)
      var index = 0
      while index < state.length do
        val value = state(index)
        out(index * 4) = (value >>> 24).toByte
        out(index * 4 + 1) = (value >>> 16).toByte
        out(index * 4 + 2) = (value >>> 8).toByte
        out(index * 4 + 3) = value.toByte
        index += 1
      finished = true
      out

    private def updateByte(value: Int): Unit =
      require(!finished, "SHA-256 digest is already finalized")
      block(blockLength) = value.toByte
      blockLength += 1
      totalLength += 1L
      if blockLength == 64 then
        compress()
        blockLength = 0

    private def compress(): Unit =
      var index = 0
      while index < 16 do
        val offset = index * 4
        words(index) = ((block(offset) & 0xff) << 24) |
          ((block(offset + 1) & 0xff) << 16) |
          ((block(offset + 2) & 0xff) << 8) |
          (block(offset + 3) & 0xff)
        index += 1
      while index < 64 do
        words(index) = smallSigma1(words(index - 2)) + words(index - 7) +
          smallSigma0(words(index - 15)) + words(index - 16)
        index += 1

      var a = state(0)
      var b = state(1)
      var c = state(2)
      var d = state(3)
      var e = state(4)
      var f = state(5)
      var g = state(6)
      var h = state(7)

      index = 0
      while index < 64 do
        val t1 = h + bigSigma1(e) + choose(e, f, g) + Constants(index) + words(index)
        val t2 = bigSigma0(a) + majority(a, b, c)
        h = g
        g = f
        f = e
        e = d + t1
        d = c
        c = b
        b = a
        a = t1 + t2
        index += 1

      state(0) += a
      state(1) += b
      state(2) += c
      state(3) += d
      state(4) += e
      state(5) += f
      state(6) += g
      state(7) += h

    private inline def rotateRight(value: Int, bits: Int): Int =
      (value >>> bits) | (value << (32 - bits))

    private inline def choose(x: Int, y: Int, z: Int): Int =
      (x & y) ^ (~x & z)

    private inline def majority(x: Int, y: Int, z: Int): Int =
      (x & y) ^ (x & z) ^ (y & z)

    private inline def bigSigma0(x: Int): Int =
      rotateRight(x, 2) ^ rotateRight(x, 13) ^ rotateRight(x, 22)

    private inline def bigSigma1(x: Int): Int =
      rotateRight(x, 6) ^ rotateRight(x, 11) ^ rotateRight(x, 25)

    private inline def smallSigma0(x: Int): Int =
      rotateRight(x, 7) ^ rotateRight(x, 18) ^ (x >>> 3)

    private inline def smallSigma1(x: Int): Int =
      rotateRight(x, 17) ^ rotateRight(x, 19) ^ (x >>> 10)

  private object Sha256:
    def apply(): Sha256 =
      new Sha256

  private val Constants = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98,
    0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8,
    0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819,
    0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2
  )
