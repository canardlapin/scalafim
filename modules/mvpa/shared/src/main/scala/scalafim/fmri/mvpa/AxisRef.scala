package scalafim.fmri.mvpa

import locus4s.{DomainError, DomainResolution}
import multivar.core.{MultivarError, SemanticError, SemanticSpace, SpaceEvidence, SpaceRef, SpaceRole}
import resample4s.core.{DesignError, DigestError, DomainMismatch, FingerprintError, UnknownUnit}
import scalafim.locus.{DomainFactory, DomainFactoryError, FiniteSpace, Point, SpaceKey}
import scalafim.response.{DomainId, IdentityError, IndexError}

enum EvidenceError:
  case InvalidAxis(field: String, detail: String)
  case AxisMismatch(boundary: String, expected: String, actual: String)
  case InvalidOrdinal(index: Int, size: Int)
  case ShapeMismatch(boundary: String, expected: Int, actual: Int)
  case InvalidSource(detail: String)
  case MultivarFailure(error: MultivarError)
  case SemanticFailure(error: SemanticError)
  case LocusIdentityFailure(error: DomainError)
  case LocusFailure(error: DomainFactoryError)
  case ResponseIdentityFailure(error: IdentityError)
  case ResponseIndexFailure(error: IndexError)
  case ResampleFailure(error: DesignError)
  case ResampleDigestFailure(error: DigestError)
  case ResampleFingerprintFailure(error: FingerprintError)
  case ReindexingCompositionFailure(error: DomainMismatch)
  case UnknownResampleUnit(error: UnknownUnit)
  case GroupingFailure(error: GroupingError)

  def message: String =
    this match
      case InvalidAxis(field, detail) =>
        s"invalid $field: $detail"
      case AxisMismatch(boundary, expected, actual) =>
        s"$boundary axis $actual does not match expected axis $expected"
      case InvalidOrdinal(index, size) =>
        s"axis ordinal $index is outside 0 until $size"
      case ShapeMismatch(boundary, expected, actual) =>
        s"$boundary expected size $expected, got $actual"
      case InvalidSource(detail) =>
        s"invalid evidence source: $detail"
      case MultivarFailure(error) =>
        error.message
      case SemanticFailure(error) =>
        error.message
      case LocusIdentityFailure(error) =>
        error.message
      case LocusFailure(error) =>
        error.message
      case ResponseIdentityFailure(error) =>
        error.message
      case ResponseIndexFailure(error) =>
        error.message
      case ResampleFailure(error) =>
        error.message
      case ResampleDigestFailure(error) =>
        error match
          case DigestError.InvalidAlgorithmId(value) =>
            s"invalid digest algorithm id '$value'"
          case DigestError.EmptyDigestValue =>
            "digest provider returned an empty value"
          case DigestError.InvalidCanonicalText(reason) =>
            s"invalid canonical digest text: $reason"
          case DigestError.ProviderFailure(detail) =>
            s"digest provider failed: $detail"
      case ResampleFingerprintFailure(error) =>
        error match
          case FingerprintError.InvalidSourceIdentity(uri, version) =>
            s"invalid source identity uri='$uri' version='$version'"
          case FingerprintError.InvalidPolicyId(value) =>
            s"invalid fingerprint policy id '$value'"
      case ReindexingCompositionFailure(error) =>
        s"cannot compose reindexings: left domain ${error.leftDomain}, right codomain ${error.rightCodomain}"
      case UnknownResampleUnit(error) =>
        s"unknown resampling unit ${error.key}; plan shape is ${error.shape}"
      case GroupingFailure(error) =>
        error.message

opaque type AxisSignature = String

object AxisSignature:
  private[mvpa] def from(value: String): Either[EvidenceError, AxisSignature] =
    if value.matches("[0-9a-f]{64}") then Right(value)
    else Left(EvidenceError.InvalidAxis("coordinate signature", "expected 64 lowercase hexadecimal digits"))

  private[mvpa] def unsafe(value: String): AxisSignature =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (signature: AxisSignature)
    inline def value: String =
      signature

/** Serializable, untrusted axis metadata. `coordinateSignature` is compact;
  * `stableKeys` are retained so a decoder can recompute and verify it.
  */
final case class AxisRecord(
    namespace: String,
    role: SpaceRole,
    stableKeys: Vector[String],
    basis: String,
    units: String,
    scale: String,
    lineage: Vector[String],
    coordinateSignature: String
)

final case class AxisDescriptor private (
    namespace: String,
    role: SpaceRole,
    size: Int,
    coordinateSignature: AxisSignature,
    basis: String,
    units: String,
    scale: String,
    lineage: Vector[String]
):
  def stableKey: String =
    s"axis-sha256-${coordinateSignature.value}"

object AxisDescriptor:
  private[mvpa] def create(
      namespace: String,
      role: SpaceRole,
      stableKeys: Vector[String],
      basis: String,
      units: String,
      scale: String,
      lineage: Vector[String]
  ): Either[EvidenceError, AxisDescriptor] =
    for
      _ <- validateField("axis namespace", namespace, 1024)
      _ <- validateField("axis basis", basis, 1024)
      _ <- validateField("axis units", units, 1024)
      _ <- validateField("axis scale", scale, 1024)
      _ <- validateKeys(stableKeys)
      _ <- validateLineage(lineage)
      signature = AxisSignature.unsafe(
        AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.mvpa.axis.v1")
          writer.string(namespace)
          writer.string(role.label)
          writer.string(basis)
          writer.string(units)
          writer.string(scale)
          writer.intLE(lineage.length)
          lineage.foreach(writer.string)
          writer.intLE(stableKeys.length)
          stableKeys.foreach(writer.string)
      )
    yield new AxisDescriptor(
      namespace,
      role,
      stableKeys.length,
      signature,
      basis,
      units,
      scale,
      lineage
    )

  private[mvpa] def decode(record: AxisRecord): Either[EvidenceError, AxisDescriptor] =
    for
      expected <- create(
        record.namespace,
        record.role,
        record.stableKeys,
        record.basis,
        record.units,
        record.scale,
        record.lineage
      )
      declared <- AxisSignature.from(record.coordinateSignature)
      _ <-
        if declared == expected.coordinateSignature then Right(())
        else
          Left(
            EvidenceError.InvalidAxis(
              "coordinate signature",
              s"declared ${declared.value}, recomputed ${expected.coordinateSignature.value}"
            )
          )
    yield expected

  private def validateField(field: String, value: String, maximumLength: Int): Either[EvidenceError, Unit] =
    if value.isEmpty then Left(EvidenceError.InvalidAxis(field, "must be non-empty"))
    else if value != value.trim then Left(EvidenceError.InvalidAxis(field, "must not have surrounding whitespace"))
    else if value.length > maximumLength then
      Left(EvidenceError.InvalidAxis(field, s"must contain at most $maximumLength characters"))
    else if value.exists(_.isControl) then Left(EvidenceError.InvalidAxis(field, "must not contain control characters"))
    else Right(())

  private[mvpa] def validateKeys(keys: Vector[String]): Either[EvidenceError, Unit] =
    if keys.isEmpty then Left(EvidenceError.InvalidAxis("coordinate keys", "must be non-empty"))
    else
      val seen = scala.collection.mutable.HashSet.empty[String]
      var ordinal = 0
      while ordinal < keys.length do
        val key = keys(ordinal)
        validateField(s"coordinate key $ordinal", key, 4096) match
          case Left(error) => return Left(error)
          case Right(_)    => ()
        if seen.contains(key) then
          return Left(EvidenceError.InvalidAxis("coordinate keys", s"duplicate stable key '$key'"))
        seen += key
        ordinal += 1
      Right(())

  private def validateLineage(lineage: Vector[String]): Either[EvidenceError, Unit] =
    if lineage.length > 4096 then
      Left(EvidenceError.InvalidAxis("axis lineage", "must contain at most 4096 entries"))
    else
      var total = 0L
      var index = 0
      while index < lineage.length do
        val value = lineage(index)
        validateField(s"axis lineage entry $index", value, 4096) match
          case Left(error) => return Left(error)
          case Right(_)    => ()
        total += value.length.toLong
        if total > 1024L * 1024L then
          return Left(EvidenceError.InvalidAxis("axis lineage", "must contain at most 1048576 characters"))
        index += 1
      Right(())

trait AxisIndex[K]:
  def size: Int
  def keyAt(ordinal: Int): Either[EvidenceError, K]
  def stableKeyAt(ordinal: Int): Either[EvidenceError, String]
  def ordinalOf(key: K): Option[Int]

object AxisIndex:
  def from[K](keys: Vector[K])(stableKey: K => String): Either[EvidenceError, AxisIndex[K]] =
    val stable = keys.map(stableKey)
    AxisDescriptor.validateKeys(stable).flatMap(_ => fromPrepared(keys, stable))

  private[mvpa] def fromPrepared[K](
      keys: Vector[K],
      stable: Vector[String]
  ): Either[EvidenceError, AxisIndex[K]] =
    if keys.isEmpty then Left(EvidenceError.InvalidAxis("coordinate keys", "must be non-empty"))
    else if keys.distinct.length != keys.length then
      Left(EvidenceError.InvalidAxis("coordinate keys", "semantic keys must be unique"))
    else if stable.length != keys.length then
      Left(EvidenceError.ShapeMismatch("stable coordinate keys", keys.length, stable.length))
    else
      Right(
        new AxisIndex[K]:
          private val ordinals = keys.iterator.zipWithIndex.toMap

          def size: Int =
            keys.length

          def keyAt(ordinal: Int): Either[EvidenceError, K] =
            if ordinal < 0 || ordinal >= keys.length then Left(EvidenceError.InvalidOrdinal(ordinal, keys.length))
            else Right(keys(ordinal))

          def stableKeyAt(ordinal: Int): Either[EvidenceError, String] =
            if ordinal < 0 || ordinal >= stable.length then Left(EvidenceError.InvalidOrdinal(ordinal, stable.length))
            else Right(stable(ordinal))

          def ordinalOf(key: K): Option[Int] =
            ordinals.get(key)
      )

  def stableStrings(keys: Vector[String]): Either[EvidenceError, AxisIndex[String]] =
    from(keys)(identity)

final class AxisRef[K] private (
    val descriptor: AxisDescriptor,
    val index: AxisIndex[K],
    private val recordValue: AxisRecord,
    val semantic: SpaceRef,
    val domain: DomainResolution,
    val responseDomain: DomainId[semantic.Id]
):
  type Id = semantic.Id
  type Locus = domain.S

  val evidence: SpaceEvidence[Id] =
    semantic.evidence

  val locus: FiniteSpace[Locus] =
    domain.space

  def size: Int =
    descriptor.size

  def keyAt(ordinal: Int): Either[EvidenceError, K] =
    index.keyAt(ordinal)

  def keyAtPoint(point: Point[Locus]): Either[EvidenceError, K] =
    index.keyAt(point.ordinal)

  def ordinalOf(key: K): Option[Int] =
    index.ordinalOf(key)

  def toRecord: AxisRecord =
    recordValue

  def sameIdentityAs[L](that: AxisRef[L]): Boolean =
    descriptor == that.descriptor && recordValue.stableKeys == that.recordValue.stableKeys

  private[mvpa] def validateDeclared(boundary: String, declared: AxisRecord): Either[EvidenceError, Unit] =
    AxisDescriptor.decode(declared).flatMap: decoded =>
      if decoded == descriptor && declared.stableKeys == recordValue.stableKeys then Right(())
      else
        Left(
          EvidenceError.AxisMismatch(
            boundary,
            descriptor.stableKey,
            decoded.stableKey
          )
        )

object AxisRef:
  def fromKeys[K](
      namespace: String,
      role: SpaceRole,
      keys: Vector[K],
      basis: String,
      units: String,
      scale: String,
      lineage: Vector[String] = Vector.empty
  )(stableKey: K => String): Either[EvidenceError, AxisRef[K]] =
    val stableKeys = keys.map(stableKey)
    for
      index <- AxisIndex.fromPrepared(keys, stableKeys)
      descriptor <- AxisDescriptor.create(namespace, role, stableKeys, basis, units, scale, lineage)
      axis <- build(descriptor, index, stableKeys)
    yield axis

  def fromStableKeys(
      namespace: String,
      role: SpaceRole,
      keys: Vector[String],
      basis: String,
      units: String,
      scale: String,
      lineage: Vector[String] = Vector.empty
  ): Either[EvidenceError, AxisRef[String]] =
    fromKeys(namespace, role, keys, basis, units, scale, lineage)(identity)

  def decode[K](record: AxisRecord, index: AxisIndex[K]): Either[EvidenceError, AxisRef[K]] =
    for
      descriptor <- AxisDescriptor.decode(record)
      _ <- validateIndex(record, index)
      axis <- build(descriptor, index, record.stableKeys)
    yield axis

  def restore(record: AxisRecord): Either[EvidenceError, AxisRef[String]] =
    AxisIndex.stableStrings(record.stableKeys).flatMap(decode(record, _))

  private def validateIndex[K](record: AxisRecord, index: AxisIndex[K]): Either[EvidenceError, Unit] =
    if index.size != record.stableKeys.length then
      Left(EvidenceError.ShapeMismatch("axis index", record.stableKeys.length, index.size))
    else
      var ordinal = 0
      while ordinal < index.size do
        index.stableKeyAt(ordinal) match
          case Left(error) => return Left(error)
          case Right(actual) if actual != record.stableKeys(ordinal) =>
            return Left(
              EvidenceError.InvalidAxis(
                "axis index",
                s"stable key at ordinal $ordinal is '$actual', expected '${record.stableKeys(ordinal)}'"
              )
            )
          case Right(_) => ()
        index.keyAt(ordinal) match
          case Left(error) => return Left(error)
          case Right(key) if index.ordinalOf(key) != Some(ordinal) =>
            return Left(
              EvidenceError.InvalidAxis(
                "axis index",
                s"key lookup at ordinal $ordinal does not round-trip"
              )
            )
          case Right(_) => ()
        ordinal += 1
      Right(())

  private def build[K](
      descriptor: AxisDescriptor,
      index: AxisIndex[K],
      stableKeys: Vector[String]
  ): Either[EvidenceError, AxisRef[K]] =
    val record = AxisRecord(
      descriptor.namespace,
      descriptor.role,
      stableKeys,
      descriptor.basis,
      descriptor.units,
      descriptor.scale,
      descriptor.lineage,
      descriptor.coordinateSignature.value
    )
    for
      semantic <- SpaceRef
        .of(descriptor.stableKey, descriptor.role, descriptor.size)
        .left
        .map(EvidenceError.MultivarFailure.apply)
      key <- SpaceKey
        .make(s"scalafim-mvpa-${descriptor.stableKey}")
        .left
        .map(EvidenceError.LocusIdentityFailure.apply)
      domain <- DomainFactory
        .restore(key, descriptor.size, Some(descriptor.namespace))
        .left
        .map(EvidenceError.LocusFailure.apply)
      responseDomain <- DomainId
        .fromString[semantic.Id](descriptor.stableKey)
        .left
        .map(EvidenceError.ResponseIdentityFailure.apply)
    yield new AxisRef(descriptor, index, record, semantic, domain, responseDomain)

private[mvpa] object AxisDigest:
  def sha256Hex(write: Writer => Unit): String =
    val writer = Writer()
    write(writer)
    val bytes = writer.digest()
    val hexadecimal = "0123456789abcdef"
    val characters = new Array[Char](bytes.length * 2)
    var index = 0
    while index < bytes.length do
      val value = bytes(index) & 0xff
      characters(index * 2) = hexadecimal.charAt(value >>> 4)
      characters(index * 2 + 1) = hexadecimal.charAt(value & 0x0f)
      index += 1
    new String(characters)

  final class Writer private[AxisDigest] ():
    private val sha = Sha256()

    def string(value: String): Unit =
      val bytes = value.getBytes("UTF-8")
      intLE(bytes.length)
      sha.update(bytes)

    def intLE(value: Int): Unit =
      sha.updateByte(value)
      sha.updateByte(value >>> 8)
      sha.updateByte(value >>> 16)
      sha.updateByte(value >>> 24)

    def digest(): Array[Byte] =
      sha.digest()

  private final class Sha256:
    private val state = Array(
      0x6a09e667,
      0xbb67ae85,
      0x3c6ef372,
      0xa54ff53a,
      0x510e527f,
      0x9b05688c,
      0x1f83d9ab,
      0x5be0cd19
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

    def updateByte(value: Int): Unit =
      require(!finished, "SHA-256 digest is already finalized")
      block(blockLength) = value.toByte
      blockLength += 1
      totalLength += 1L
      if blockLength == 64 then
        compress()
        blockLength = 0

    def digest(): Array[Byte] =
      require(!finished, "SHA-256 digest is already finalized")
      val bitLength = totalLength * 8L
      updateByte(0x80)
      while blockLength != 56 do updateByte(0)
      var shift = 56
      while shift >= 0 do
        updateByte((bitLength >>> shift).toInt)
        shift -= 8

      val output = new Array[Byte](32)
      var index = 0
      while index < state.length do
        val value = state(index)
        output(index * 4) = (value >>> 24).toByte
        output(index * 4 + 1) = (value >>> 16).toByte
        output(index * 4 + 2) = (value >>> 8).toByte
        output(index * 4 + 3) = value.toByte
        index += 1
      finished = true
      output

    private def compress(): Unit =
      var index = 0
      while index < 16 do
        val offset = index * 4
        words(index) =
          ((block(offset) & 0xff) << 24) |
            ((block(offset + 1) & 0xff) << 16) |
            ((block(offset + 2) & 0xff) << 8) |
            (block(offset + 3) & 0xff)
        index += 1
      while index < 64 do
        words(index) =
          smallSigma1(words(index - 2)) + words(index - 7) + smallSigma0(words(index - 15)) + words(index - 16)
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
        val first = h + bigSigma1(e) + choose(e, f, g) + Constants(index) + words(index)
        val second = bigSigma0(a) + majority(a, b, c)
        h = g
        g = f
        f = e
        e = d + first
        d = c
        c = b
        b = a
        a = first + second
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

  private val Constants = Array(
    0x428a2f98,
    0x71374491,
    0xb5c0fbcf,
    0xe9b5dba5,
    0x3956c25b,
    0x59f111f1,
    0x923f82a4,
    0xab1c5ed5,
    0xd807aa98,
    0x12835b01,
    0x243185be,
    0x550c7dc3,
    0x72be5d74,
    0x80deb1fe,
    0x9bdc06a7,
    0xc19bf174,
    0xe49b69c1,
    0xefbe4786,
    0x0fc19dc6,
    0x240ca1cc,
    0x2de92c6f,
    0x4a7484aa,
    0x5cb0a9dc,
    0x76f988da,
    0x983e5152,
    0xa831c66d,
    0xb00327c8,
    0xbf597fc7,
    0xc6e00bf3,
    0xd5a79147,
    0x06ca6351,
    0x14292967,
    0x27b70a85,
    0x2e1b2138,
    0x4d2c6dfc,
    0x53380d13,
    0x650a7354,
    0x766a0abb,
    0x81c2c92e,
    0x92722c85,
    0xa2bfe8a1,
    0xa81a664b,
    0xc24b8b70,
    0xc76c51a3,
    0xd192e819,
    0xd6990624,
    0xf40e3585,
    0x106aa070,
    0x19a4c116,
    0x1e376c08,
    0x2748774c,
    0x34b0bcb5,
    0x391c0cb3,
    0x4ed8aa4a,
    0x5b9cca4f,
    0x682e6ff3,
    0x748f82ee,
    0x78a5636f,
    0x84c87814,
    0x8cc70208,
    0x90befffa,
    0xa4506ceb,
    0xbef9a3f7,
    0xc67178f2
  )
