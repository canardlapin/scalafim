package scalafim.phrfcmp.exec

import java.math.BigInteger
import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import java.security.spec.{NamedParameterSpec, XECPrivateKeySpec, XECPublicKeySpec}
import java.security.{GeneralSecurityException, KeyFactory, MessageDigest, SecureRandom}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, KeyAgreement, Mac}

/** Why a seal, a read-back or a store operation failed. Messages never carry names, values or lengths. */
enum SealError(val message: String):
  case BadName extends SealError("bad logical blob name")
  case PayloadTooLarge extends SealError("payload exceeds the 1 GiB blob limit")
  case InvalidRecipientKey(detail: String) extends SealError(s"invalid recipient key: $detail")
  case FingerprintMismatch extends SealError("recipient key fingerprint does not match the owner-confirmed fingerprint")
  case LowOrderPoint extends SealError("invalid X25519 key (low-order point or zero shared secret)")
  case ReadBackMismatch(stage: String) extends SealError(s"read-back failed ($stage); nothing published")
  case StoreClosed extends SealError("store is already closed")
  case Io(detail: String) extends SealError(s"i/o failure: $detail")

/** HKDF-SHA256 (RFC 5869) from HMAC; the JDK has no public KDF API before 24. */
object Hkdf:
  def sha256(ikm: Array[Byte], salt: Array[Byte], info: Array[Byte], length: Int): Array[Byte] =
    require(length >= 0 && length <= 255 * 32, "HKDF length out of range")
    val hashLen = 32
    val effectiveSalt = if salt.isEmpty then new Array[Byte](hashLen) else salt
    val prk = mac(effectiveSalt).doFinal(ikm)
    val out = new Array[Byte](length)
    var t = Array.emptyByteArray
    var pos = 0
    var i = 1
    while pos < length do
      val m = mac(prk)
      m.update(t)
      m.update(info)
      m.update(i.toByte)
      t = m.doFinal()
      val n = math.min(hashLen, length - pos)
      System.arraycopy(t, 0, out, pos, n)
      pos += n
      i += 1
    out

  private def mac(key: Array[Byte]): Mac =
    val m = Mac.getInstance("HmacSHA256")
    m.init(new SecretKeySpec(key, "HmacSHA256"))
    m

/** Raw X25519 (RFC 7748) over the JDK's XDH provider. */
object X25519:
  private val params = NamedParameterSpec.X25519
  val BasePoint: Array[Byte] = Array.tabulate[Byte](32)(i => if i == 0 then 9 else 0)

  private def factory = KeyFactory.getInstance("XDH")

  private def publicKey(raw: Array[Byte]) =
    val be = raw.reverse
    be(0) = (be(0) & 0x7f).toByte // RFC 7748: mask the most significant bit of the u-coordinate
    factory.generatePublic(new XECPublicKeySpec(params, new BigInteger(1, be)))

  private def privateKey(raw: Array[Byte]) = factory.generatePrivate(new XECPrivateKeySpec(params, raw))

  /** Shared secret; a low-order point or an all-zero result is an error, not an exception. */
  def agree(privateRaw: Array[Byte], publicRaw: Array[Byte]): Either[SealError, Array[Byte]] =
    if privateRaw.length != 32 || publicRaw.length != 32 then Left(SealError.InvalidRecipientKey("length"))
    else
      try
        val ka = KeyAgreement.getInstance("XDH")
        ka.init(privateKey(privateRaw))
        ka.doPhase(publicKey(publicRaw), true)
        val s = ka.generateSecret()
        if s.forall(_ == 0) then Left(SealError.LowOrderPoint) else Right(s)
      catch case _: GeneralSecurityException => Left(SealError.LowOrderPoint)

  /** Public key of a private scalar: X25519(priv, 9). */
  def publicOf(privateRaw: Array[Byte]): Array[Byte] =
    agree(privateRaw, BasePoint).fold(e => throw new IllegalArgumentException(e.message), identity)

/** The owner's raw 32-byte X25519 public key. */
final class RecipientKey private (private val raw: Array[Byte]):
  def bytes: Array[Byte] = raw.clone()
  def id: Array[Byte] = MessageDigest.getInstance("SHA-256").digest(raw).take(8)

  /** SHA-256 of the raw key, 16 groups of 4 hex characters. */
  def fingerprint: String =
    val h = Fs.hex(MessageDigest.getInstance("SHA-256").digest(raw))
    h.grouped(4).mkString(" ")

  def checkFingerprint(expected: String): Either[SealError, Unit] =
    if fingerprint.replace(" ", "") == expected.replace(" ", "").toLowerCase then Right(()) else Left(SealError.FingerprintMismatch)

  override def toString: String = s"RecipientKey($fingerprint)"

object RecipientKey:
  private val SpkiPrefix = Fs.unhex("302a300506032b656e032100")

  def fromRaw(bytes: Array[Byte]): Either[SealError, RecipientKey] =
    if bytes.length == 32 then Right(new RecipientKey(bytes.clone())) else Left(SealError.InvalidRecipientKey("length"))

  /** PEM (SubjectPublicKeyInfo) or 64 hex characters of the raw key. */
  def parse(text: String): Either[SealError, RecipientKey] =
    val s = text.trim
    if s.startsWith("-----BEGIN") then
      try
        val body = s.linesIterator.filterNot(_.startsWith("-----")).mkString
        val der = java.util.Base64.getDecoder.decode(body)
        if der.length == 44 && der.take(12).sameElements(SpkiPrefix) then fromRaw(der.drop(12))
        else Left(SealError.InvalidRecipientKey("not X25519"))
      catch case _: IllegalArgumentException => Left(SealError.InvalidRecipientKey("pem"))
    else if s.matches("[0-9a-fA-F]{64}") then fromRaw(Fs.unhex(s))
    else Left(SealError.InvalidRecipientKey("unrecognised format"))

/** Per-blob randomness: a fresh ephemeral X25519 private key and a 12-byte nonce. Production uses a CSPRNG; the
  * `kind` and `name` arguments exist only so tests can derive deterministic draws.
  */
trait SealEntropy:
  def draw(kind: Int, name: String): SealDraw

final class SealDraw(val ephemeralPrivate: Array[Byte], val nonce: Array[Byte]):
  require(ephemeralPrivate.length == 32 && nonce.length == 12, "32-byte key and 12-byte nonce")

object SealEntropy:
  val secure: SealEntropy =
    val rng = new SecureRandom()
    (_, _) =>
      val k = new Array[Byte](32)
      val n = new Array[Byte](12)
      rng.nextBytes(k)
      rng.nextBytes(n)
      new SealDraw(k, n)

/** phrf-sealed/1 constants, padding and envelope (docs/plans/phrf-pilot-sealed-format.md sections 3 and 4). */
object SealFormat:
  val Format = "phrf-sealed/1"
  val Magic: Array[Byte] = Array(0x50, 0x48, 0x52, 0x46, 0x53, 0x42, 0x01, 0x00).map(_.toByte)
  val HeaderLen = 60
  val Info: Array[Byte] = "phrf-sealed/1 blob".getBytes(US_ASCII)
  val KindData = 0
  val KindClose = 1
  val CloseName = "CLOSE"
  val BucketMin = 4096L
  val Gib = 1L << 30
  val MaxName = 1024

  def bucket(n: Long): Long =
    if n <= BucketMin then BucketMin
    else if n <= Gib then java.lang.Long.highestOneBit(n - 1) << 1
    else ((n + Gib - 1) / Gib) * Gib

  def validName(name: String): Boolean =
    name.nonEmpty && !name.contains('\u0000') && !name.contains('\\') && !name.startsWith("/") &&
      name.split("/", -1).forall(c => c.nonEmpty && c != "." && c != "..") &&
      utf8Len(name).exists(_ <= MaxName)

  private def utf8Len(s: String): Option[Int] =
    try Some(UTF_8.newEncoder().encode(java.nio.CharBuffer.wrap(s)).remaining())
    catch case _: java.nio.charset.CharacterCodingException => None

  /** The padded plaintext envelope. */
  def envelope(kind: Int, name: String, data: Array[Byte]): Either[SealError, Array[Byte]] =
    if !validName(name) then Left(SealError.BadName)
    else
      val nb = name.getBytes(UTF_8)
      val len = 1L + 2 + nb.length + 8 + data.length
      if len > Gib then Left(SealError.PayloadTooLarge)
      else
        val out = new Array[Byte](bucket(len).toInt)
        val bb = java.nio.ByteBuffer.wrap(out)
        bb.put(kind.toByte).putShort(nb.length.toShort).put(nb).putLong(data.length.toLong).put(data)
        Right(out)

  /** A parsed envelope; compared, never printed. */
  final class Opened(val kind: Int, val name: String, val data: Array[Byte]):
    def matches(k: Int, n: String, d: Array[Byte]): Boolean = kind == k && name == n && java.util.Arrays.equals(data, d)
    override def toString: String = "Opened(redacted)"

  def parseEnvelope(pt: Array[Byte]): Either[SealError, Opened] =
    def bad = Left(SealError.ReadBackMismatch("envelope"))
    if pt.length < BucketMin then bad
    else
      val bb = java.nio.ByteBuffer.wrap(pt)
      val kind = bb.get() & 0xff
      val nl = bb.getShort() & 0xffff
      if 3L + nl + 8 > pt.length then bad
      else
        val nameBytes = new Array[Byte](nl)
        bb.get(nameBytes)
        val dl = bb.getLong()
        val end = 3L + nl + 8 + dl
        if dl < 0 || end > pt.length || bucket(end) != pt.length.toLong || (kind != KindData && kind != KindClose) then bad
        else if pt.drop(end.toInt).exists(_ != 0) then bad
        else
          val name = new String(nameBytes, UTF_8)
          if !validName(name) then bad
          else
            val data = new Array[Byte](dl.toInt)
            bb.get(data)
            Right(new Opened(kind, name, data))

/** Encryption and writer-side read-back. The writer never holds the owner private key. */
object Sealer:
  final class Sealed(val blob: Array[Byte], private[exec] val ephemeralPrivate: Array[Byte])

  private def aead(shared: Array[Byte], recipientRaw: Array[Byte], ephPub: Array[Byte]): SecretKeySpec =
    new SecretKeySpec(Hkdf.sha256(shared, Array.emptyByteArray, SealFormat.Info ++ recipientRaw ++ ephPub, 32), "AES")

  private def gcm(mode: Int, key: SecretKeySpec, nonce: Array[Byte], header: Array[Byte]): Cipher =
    val c = Cipher.getInstance("AES/GCM/NoPadding")
    c.init(mode, key, new GCMParameterSpec(128, nonce))
    c.updateAAD(header)
    c

  /** Encrypts one envelope. On success the caller owns (and must zero) the ephemeral private key in the result; on
    * any failure, a `Left` or a thrown exception, the key is zeroed here. The shared secret is always zeroed.
    */
  def seal(recipient: RecipientKey, kind: Int, name: String, data: Array[Byte], entropy: SealEntropy): Either[SealError, Sealed] =
    SealFormat.envelope(kind, name, data).flatMap { env =>
      val draw = entropy.draw(kind, name)
      var handedOver = false
      try
        val ephPub = X25519.publicOf(draw.ephemeralPrivate)
        X25519.agree(draw.ephemeralPrivate, recipient.bytes).map { shared =>
          try
            val header = SealFormat.Magic ++ ephPub ++ recipient.id ++ draw.nonce
            val ct = gcm(Cipher.ENCRYPT_MODE, aead(shared, recipient.bytes, ephPub), draw.nonce, header).doFinal(env)
            val sealedBlob = new Sealed(header ++ ct, draw.ephemeralPrivate)
            handedOver = true
            sealedBlob
          finally java.util.Arrays.fill(shared, 0.toByte)
        }
      finally if !handedOver then java.util.Arrays.fill(draw.ephemeralPrivate, 0.toByte)
    }

  /** Decrypts with the writer's own ephemeral private key and the recipient PUBLIC key (the shared secret is
    * symmetric) and compares kind, name and data with the inputs.
    */
  def verifyReadback(recipient: RecipientKey, ephemeralPrivate: Array[Byte], blob: Array[Byte], kind: Int, name: String, data: Array[Byte]): Either[SealError, Unit] =
    val mismatch = Left(SealError.ReadBackMismatch("decrypt"))
    if blob.length < SealFormat.HeaderLen + 16 then mismatch
    else
      val header = blob.take(SealFormat.HeaderLen)
      val ephRaw = header.slice(8, 40)
      if !ephRaw.sameElements(X25519.publicOf(ephemeralPrivate)) then Left(SealError.ReadBackMismatch("ephemeral"))
      else
        X25519.agree(ephemeralPrivate, recipient.bytes).flatMap { shared =>
          try
            val pt = gcm(Cipher.DECRYPT_MODE, aead(shared, recipient.bytes, ephRaw), header.slice(48, 60), header).doFinal(blob, SealFormat.HeaderLen, blob.length - SealFormat.HeaderLen)
            SealFormat.parseEnvelope(pt).flatMap(o => if o.matches(kind, name, data) then Right(()) else mismatch)
          catch case _: GeneralSecurityException => mismatch
        }
