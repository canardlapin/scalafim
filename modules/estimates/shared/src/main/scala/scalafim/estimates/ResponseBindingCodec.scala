package scalafim.estimates

import java.nio.charset.StandardCharsets
import scalafim.archive.ContentDigest
import scalafim.image.SampleSpaces.*

/** Portable SHA-256 and the canonical digests a ScalaFIM binder derives at its
  * own boundary. Nothing here is read from, or written to, a persisted schema.
  */
private[scalafim] object ResponseDigests:
  val FeaturesSchema: String = "scalafim.response-features/1"

  /** Length-prefixed token: the prefix counts UTF-8 bytes, so any
    * reimplementation that frames by bytes agrees. Unambiguous for any text.
    */
  def token(value: String): String = s"${value.getBytes(StandardCharsets.UTF_8).length}:$value"

  def number(value: Double): String = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))

  def provider(schema: String, canonical: String): ProviderDigest =
    ProviderDigest(schema, ContentDigest.unsafeSha256(sha256Hex(canonical.getBytes(StandardCharsets.UTF_8))))

  /** Ordered physical sample IDs (the domain support, in order) plus the exact
    * domain identity. A permutation of an identical sample set differs.
    */
  def features(domain: EstimateDomain): ProviderDigest =
    val affine = domain.space.affineD3.fold(error => throw new IllegalArgumentException(error.message), identity)
    val canonical = new java.lang.StringBuilder()
    canonical.append(token("dimensions")).append(token(domain.dimensions.mkString(",")))
    canonical.append(token("affine")).append(token(affine.rowMajor.map(number).mkString(",")))
    canonical.append(token("frame")).append(token(domain.worldFrame))
    canonical.append(token("support")).append(token(domain.support.mkString(",")))
    provider(FeaturesSchema, canonical.toString)

  private val K: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  def sha256Hex(input: Array[Byte]): String =
    val bitLength = input.length.toLong * 8L
    val padded = new Array[Byte]((((input.length + 8) / 64) + 1) * 64)
    System.arraycopy(input, 0, padded, 0, input.length)
    padded(input.length) = 0x80.toByte
    var shift = 0
    while shift < 8 do
      padded(padded.length - 1 - shift) = (bitLength >>> (8 * shift)).toByte
      shift += 1
    val h = Array(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
    val w = new Array[Int](64)
    var block = 0
    while block < padded.length do
      var t = 0
      while t < 16 do
        val i = block + 4 * t
        w(t) = ((padded(i) & 0xff) << 24) | ((padded(i + 1) & 0xff) << 16) | ((padded(i + 2) & 0xff) << 8) | (padded(i + 3) & 0xff)
        t += 1
      while t < 64 do
        val s0 = Integer.rotateRight(w(t - 15), 7) ^ Integer.rotateRight(w(t - 15), 18) ^ (w(t - 15) >>> 3)
        val s1 = Integer.rotateRight(w(t - 2), 17) ^ Integer.rotateRight(w(t - 2), 19) ^ (w(t - 2) >>> 10)
        w(t) = w(t - 16) + s0 + w(t - 7) + s1
        t += 1
      var a = h(0); var b = h(1); var c = h(2); var d = h(3)
      var e = h(4); var f = h(5); var g = h(6); var k = h(7)
      t = 0
      while t < 64 do
        val sum1 = Integer.rotateRight(e, 6) ^ Integer.rotateRight(e, 11) ^ Integer.rotateRight(e, 25)
        val choice = (e & f) ^ (~e & g)
        val temp1 = k + sum1 + choice + K(t) + w(t)
        val sum0 = Integer.rotateRight(a, 2) ^ Integer.rotateRight(a, 13) ^ Integer.rotateRight(a, 22)
        val majority = (a & b) ^ (a & c) ^ (b & c)
        val temp2 = sum0 + majority
        k = g; g = f; f = e; e = d + temp1
        d = c; c = b; b = a; a = temp1 + temp2
        t += 1
      h(0) += a; h(1) += b; h(2) += c; h(3) += d
      h(4) += e; h(5) += f; h(6) += g; h(7) += k
      block += 64
    val out = new java.lang.StringBuilder(64)
    h.foreach: word =>
      val hex = Integer.toHexString(word)
      var pad = 8 - hex.length
      while pad > 0 do
        out.append('0')
        pad -= 1
      out.append(hex)
    out.toString

/** An untrusted binding read from bytes. It carries the same fields as a
  * [[ResponseSourceBinding]] but can never become one: no conversion exists.
  */
final case class DecodedBindingClaim(
    unit: UnitRevisionId,
    observation: ObservationId,
    design: ProviderDigest,
    preparation: ProviderDigest,
    noise: ProviderDigest,
    runCombination: ProviderDigest,
    readout: ProviderDigest,
    readoutAxis: ReadoutAxis,
    columns: Vector[ColumnId],
    selected: Vector[ColumnId],
    features: ProviderDigest,
    realizedNoise: RealizedNoise,
    realizedCombination: RealizedCombination
):
  require(Invariants.unique(columns), "claimed columns must be unique and nonempty")
  require(Invariants.unique(selected) && selected.toSet.subsetOf(columns.toSet), "claimed selection must be a unique nonempty subset")
  require(realizedNoise.arOrder.forall(_ >= 1), "claimed AR order must be positive")
  require(WellFormed.text(observation.value) && columns.forall(c => WellFormed.text(c.value)), "claimed text must be well-formed")

object DecodedBindingClaim:
  def of(b: ResponseSourceBinding): DecodedBindingClaim =
    DecodedBindingClaim(b.unit, b.observation, b.design, b.preparation, b.noise, b.runCombination, b.readout,
      b.readoutAxis, b.columns, b.selected, b.features, b.realizedNoise, b.realizedCombination)

enum BindingCodecError:
  case UnsupportedFormat(found: String)
  case Malformed(detail: String)
  case InvalidAxis(defect: ReadoutDefect)
  case NonCanonical

  def message: String = this match
    case UnsupportedFormat(found) => s"unsupported response binding format '$found'"
    case Malformed(detail) => s"malformed response binding: $detail"
    case InvalidAxis(defect) => s"invalid readout axis: ${defect.message}"
    case NonCanonical => "response binding bytes are not in canonical form"

/** Deterministic, length-prefixed UTF-8 encoding of a binding. Persistence of
  * these bytes is out of scope for v1 (OD-3); the codec exists so that bytes
  * are reproducible and can only be decoded to an untrusted claim.
  */
object ResponseBindingCodec:
  val Format: String = "scalafim.response-source-binding/1"

  def encode(binding: ResponseSourceBinding): Array[Byte] = encode(DecodedBindingClaim.of(binding))

  def encode(claim: DecodedBindingClaim): Array[Byte] =
    val out = new java.lang.StringBuilder()
    def put(value: String): Unit =
      val _ = out.append(ResponseDigests.token(value))
    def digest(value: ProviderDigest): Unit =
      put(value.schema); put(value.digest.algorithm); put(value.digest.value)
    def ids(values: Vector[String]): Unit =
      put(values.size.toString); values.foreach(put)
    put(Format)
    put(claim.unit.value)
    put(claim.observation.value)
    Vector(claim.design, claim.preparation, claim.noise, claim.runCombination, claim.readout).foreach(digest)
    put(claim.readoutAxis.rows.size.toString)
    claim.readoutAxis.rows.foreach(row => { put(row.condition.value); put(row.bin.toString) })
    ids(claim.columns.map(_.value))
    ids(claim.selected.map(_.value))
    digest(claim.features)
    claim.realizedNoise match
      case RealizedNoise.White => put("White")
      case RealizedNoise.FixedAr(order, pooling, exact) =>
        put("FixedAr"); put(order.toString); put(pooling.toString)
        exact match
          case Some(value) => put("Some"); digest(value)
          case None => put("None")
      case RealizedNoise.EstimatedAr(order, pooling) => put("EstimatedAr"); put(order.toString); put(pooling.toString)
      case RealizedNoise.Robust => put("Robust")
      case RealizedNoise.LearnedSubspace => put("LearnedSubspace")
      case RealizedNoise.Unrecorded => put("Unrecorded")
    claim.realizedCombination match
      case RealizedCombination.SingleRun => put("SingleRun")
      case RealizedCombination.FixedWeights(value) => put("FixedWeights"); digest(value)
      case RealizedCombination.EstimatedWeights => put("EstimatedWeights")
      case RealizedCombination.Unrecorded => put("Unrecorded")
    out.toString.getBytes(StandardCharsets.UTF_8)

  /** Decode to an untrusted claim. Bytes must be exactly the canonical encoding
    * of the claim they decode to.
    */
  def decode(bytes: Array[Byte]): Either[BindingCodecError, DecodedBindingClaim] =
    val cursor = new Cursor(bytes)
    val decoded =
      for
        format <- cursor.token
        _ <- if format == Format then Right(()) else Left(BindingCodecError.UnsupportedFormat(format))
        unit <- cursor.token.flatMap(value => checked(UnitRevisionId(value)))
        observation <- cursor.token.flatMap(value => checked(ObservationId(value)))
        design <- cursor.digest
        preparation <- cursor.digest
        noise <- cursor.digest
        runCombination <- cursor.digest
        readout <- cursor.digest
        rowCount <- cursor.count(minimumItemBytes = 4)
        rows <- cursor.repeat(rowCount)(for
          condition <- cursor.token.flatMap(value => checked(ConditionLevelId(value)))
          bin <- cursor.number
        yield ReadoutRowId(condition, bin))
        axis <- ReadoutAxis.parse(rows).left.map(BindingCodecError.InvalidAxis.apply)
        columns <- cursor.ids
        selected <- cursor.ids
        features <- cursor.digest
        realizedNoise <- cursor.noise
        realizedCombination <- cursor.combination
        _ <- if cursor.atEnd then Right(()) else Left(BindingCodecError.Malformed("trailing bytes"))
        claim <- checked(DecodedBindingClaim(unit, observation, design, preparation, noise, runCombination, readout, axis,
          columns, selected, features, realizedNoise, realizedCombination))
      yield claim
    // Invalid UTF-8 decodes to replacement characters, so it fails this check.
    decoded.flatMap(claim => if java.util.Arrays.equals(encode(claim), bytes) then Right(claim) else Left(BindingCodecError.NonCanonical))

  private def checked[A](value: => A): Either[BindingCodecError, A] =
    try Right(value)
    catch case error: IllegalArgumentException => Left(BindingCodecError.Malformed(Option(error.getMessage).getOrElse("invalid field")))

  /** Frames by UTF-8 byte counts; every declared length or count is bounded
    * by the remaining input before anything is allocated or iterated.
    */
  private final class Cursor(bytes: Array[Byte]):
    private var position = 0

    def atEnd: Boolean = position == bytes.length
    private def remaining: Int = bytes.length - position

    def token: Either[BindingCodecError, String] =
      var colon = position
      while colon < bytes.length && colon - position <= 9 && bytes(colon) != ':'.toByte do colon += 1
      if colon >= bytes.length || bytes(colon) != ':'.toByte then Left(BindingCodecError.Malformed("missing token"))
      else
        val length = new String(bytes, position, colon - position, StandardCharsets.US_ASCII)
        if !length.matches("0|[1-9][0-9]{0,8}") then Left(BindingCodecError.Malformed("invalid token length"))
        else if length.toLong > (bytes.length - colon - 1).toLong then Left(BindingCodecError.Malformed("truncated token"))
        else
          val start = colon + 1
          val end = start + length.toInt
          position = end
          Right(new String(bytes, start, end - start, StandardCharsets.UTF_8))

    def number: Either[BindingCodecError, Int] =
      token.flatMap(value =>
        if value.matches("0|[1-9][0-9]{0,8}") then Right(value.toInt) else Left(BindingCodecError.Malformed("invalid number")))

    /** A count of items, each occupying at least `minimumItemBytes`. */
    def count(minimumItemBytes: Int): Either[BindingCodecError, Int] =
      number.flatMap(n =>
        if n.toLong * minimumItemBytes > remaining.toLong then Left(BindingCodecError.Malformed("count exceeds remaining input"))
        else Right(n))

    def repeat[A](n: Int)(read: => Either[BindingCodecError, A]): Either[BindingCodecError, Vector[A]] =
      val out = Vector.newBuilder[A]
      var i = 0
      var failure: Option[BindingCodecError] = None
      while failure.isEmpty && i < n do
        read match
          case Right(value) => out += value
          case Left(error) => failure = Some(error)
        i += 1
      failure.toLeft(out.result())

    def ids: Either[BindingCodecError, Vector[ColumnId]] =
      count(minimumItemBytes = 2).flatMap(n => repeat(n)(token.flatMap(value => checked(ColumnId(value)))))

    def digest: Either[BindingCodecError, ProviderDigest] =
      for
        schema <- token
        algorithm <- token
        value <- token
        _ <- if algorithm == "sha256" then Right(()) else Left(BindingCodecError.Malformed(s"unsupported digest algorithm '$algorithm'"))
        content <- ContentDigest.sha256(value).left.map(error => BindingCodecError.Malformed(error.message))
        digest <- checked(ProviderDigest(schema, content))
      yield digest

    def scope: Either[BindingCodecError, NoiseScope] =
      token.flatMap(value => NoiseScope.values.find(_.toString == value).toRight(BindingCodecError.Malformed(s"unknown noise scope '$value'")))

    def noise: Either[BindingCodecError, RealizedNoise] =
      token.flatMap:
        case "White" => Right(RealizedNoise.White)
        case "FixedAr" =>
          for
            order <- number
            pooling <- scope
            flag <- token
            exact <- flag match
              case "Some" => digest.map(Some(_))
              case "None" => Right(None)
              case other => Left(BindingCodecError.Malformed(s"unknown option tag '$other'"))
          yield RealizedNoise.FixedAr(order, pooling, exact)
        case "EstimatedAr" => for order <- number; pooling <- scope yield RealizedNoise.EstimatedAr(order, pooling)
        case "Robust" => Right(RealizedNoise.Robust)
        case "LearnedSubspace" => Right(RealizedNoise.LearnedSubspace)
        case "Unrecorded" => Right(RealizedNoise.Unrecorded)
        case other => Left(BindingCodecError.Malformed(s"unknown realized noise '$other'"))

    def combination: Either[BindingCodecError, RealizedCombination] =
      token.flatMap:
        case "SingleRun" => Right(RealizedCombination.SingleRun)
        case "FixedWeights" => digest.map(RealizedCombination.FixedWeights.apply)
        case "EstimatedWeights" => Right(RealizedCombination.EstimatedWeights)
        case "Unrecorded" => Right(RealizedCombination.Unrecorded)
        case other => Left(BindingCodecError.Malformed(s"unknown realized combination '$other'"))
