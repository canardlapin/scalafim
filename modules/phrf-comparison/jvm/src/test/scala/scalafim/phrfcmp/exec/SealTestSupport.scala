package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.jdk.CollectionConverters.*

/** Test-only fixed draw, for golden vectors. */
final class FixedEntropy(eph: Array[Byte], nonce: Array[Byte]) extends SealEntropy:
  def draw(kind: Int, name: String): SealDraw = new SealDraw(eph.clone(), nonce.clone())

/** Test-only deterministic draw derived from (seed, kind, name): lets the sealed-tree digest be compared across
  * runs with different interleavings. Never used in production code.
  */
final class DerivedEntropy(seed: String) extends SealEntropy:
  def draw(kind: Int, name: String): SealDraw =
    def h(tag: String) = MessageDigest.getInstance("SHA-256").digest(s"$seed|$kind|$name|$tag".getBytes(UTF_8))
    new SealDraw(h("eph"), h("nonce").take(12))

/** A throwaway owner key pair. The private key exists only in tests. */
final class TestOwner(val priv: Array[Byte]):
  val pub: Array[Byte] = X25519.publicOf(priv)
  val recipient: RecipientKey = RecipientKey.fromRaw(pub).toOption.get
  def fingerprint: String = recipient.fingerprint

object TestOwner:
  def random(): TestOwner =
    val b = new Array[Byte](32)
    new java.security.SecureRandom().nextBytes(b)
    new TestOwner(b)

/** Owner-side reader (the part of phrf-sealed/1 that production code must never contain). */
object OwnerReader:
  def decrypt(priv: Array[Byte], blob: Array[Byte]): Either[SealError, SealFormat.Opened] =
    if blob.length < SealFormat.HeaderLen + 16 || !blob.take(8).sameElements(SealFormat.Magic) then Left(SealError.ReadBackMismatch("magic"))
    else
      val header = blob.take(SealFormat.HeaderLen)
      val eph = header.slice(8, 40)
      val pub = X25519.publicOf(priv)
      if !header.slice(40, 48).sameElements(RecipientKey.fromRaw(pub).toOption.get.id) then Left(SealError.ReadBackMismatch("recipient"))
      else
        X25519.agree(priv, eph).flatMap { shared =>
          val key = new SecretKeySpec(Hkdf.sha256(shared, Array.emptyByteArray, SealFormat.Info ++ pub ++ eph, 32), "AES")
          val c = Cipher.getInstance("AES/GCM/NoPadding")
          c.init(Cipher.DECRYPT_MODE, key, new GCMParameterSpec(128, header.slice(48, 60)))
          c.updateAAD(header)
          try SealFormat.parseEnvelope(c.doFinal(blob, SealFormat.HeaderLen, blob.length - SealFormat.HeaderLen))
          catch case _: java.security.GeneralSecurityException => Left(SealError.ReadBackMismatch("tag"))
        }

  /** name -> data after the reader checks of format spec section 6 (file names, duplicates, CLOSE, receipt). */
  def readAll(store: Path, priv: Array[Byte], allowPartial: Boolean = false): Either[String, Map[String, Array[Byte]]] =
    val files = Files.list(store.resolve("blobs")).iterator().asScala.toVector.filterNot(_.getFileName.toString.endsWith(".tmp")).sortBy(_.toString)
    final case class Acc(items: Map[String, Array[Byte]], dataHashes: Vector[String], closes: Vector[String])
    val folded = files.foldLeft[Either[String, Acc]](Right(Acc(Map.empty, Vector.empty, Vector.empty))) { (accE, p) =>
      accE.flatMap { acc =>
        val raw = Files.readAllBytes(p)
        val h = Fs.sha256(raw)
        if p.getFileName.toString != s"$h.enc" then Left("file name does not match content")
        else
          decrypt(priv, raw).left.map(_.message).flatMap { o =>
            if o.kind == SealFormat.KindClose then Right(acc.copy(closes = acc.closes :+ new String(o.data, UTF_8)))
            else
              acc.items.get(o.name) match
                case Some(prev) if !java.util.Arrays.equals(prev, o.data) => Left("logical name sealed twice with different plaintext")
                case _ => Right(acc.copy(items = acc.items + (o.name -> o.data), dataHashes = acc.dataHashes :+ h))
          }
      }
    }
    folded.flatMap { acc =>
      val (items, dataHashes, closes) = (acc.items, acc.dataHashes, acc.closes)
      if closes.length > 1 then Left("multiple CLOSE records")
      else if closes.isEmpty && !allowPartial then Left("no CLOSE record: partial store")
      else
        val closeOk = closes.headOption.forall { c =>
          val j = ujson.read(c)
          val hs = dataHashes.sorted
          j("n_blobs").num.toInt == hs.length && j("blobs_digest").str == Fs.sha256(hs.mkString("\n").getBytes(UTF_8))
        }
        if !closeOk then Left("blob set does not match the CLOSE record")
        else
          val receipt = store.resolve("SEALED")
          if Files.exists(receipt) && ujson.read(Files.readString(receipt))("sealed_tree_digest").str != SealedStore.digest(store) then Left("receipt digest mismatch")
          else Right(items)
    }

object Repo:
  /** The repository root: nearest ancestor of the working directory that holds the custody tooling. */
  lazy val root: Path =
    Iterator.iterate(Paths.get(sys.props("user.dir")).toAbsolutePath)(_.getParent).takeWhile(_ != null)
      .find(p => Files.isDirectory(p.resolve("tools/phrf-comparison/custody/phrf_custody"))).getOrElse(sys.error("repository root not found"))
  lazy val custody: Path = root.resolve("tools/phrf-comparison/custody")

/** Runs the Python custody tooling. Interop is mandatory: set `PHRF_REQUIRE_INTEROP=1` to fail instead of cancel
  * when no interpreter with `cryptography` is available.
  */
object Py:
  lazy val interpreter: Option[String] =
    Seq("python3.12", "python3").find { exe =>
      try
        val p = new ProcessBuilder(exe, "-c", "import cryptography").redirectErrorStream(true).start()
        p.getInputStream.readAllBytes()
        p.waitFor() == 0
      catch case _: java.io.IOException => false
    }

  def required: Boolean = sys.env.get("PHRF_REQUIRE_INTEROP").contains("1")

  def exe(using munit.Location): String =
    if interpreter.isEmpty && required then munit.Assertions.fail("python with cryptography is required for interop (PHRF_REQUIRE_INTEROP=1)")
    munit.Assertions.assume(interpreter.isDefined, "no python3 with cryptography; interop test skipped")
    interpreter.get

  def run(python: String, args: String*): (Int, String) =
    val pb = new ProcessBuilder((python +: args)*).directory(Repo.custody.toFile).redirectErrorStream(true)
    pb.environment().put("PYTHONPATH", Repo.custody.toString)
    pb.environment().put("PYTHONDONTWRITEBYTECODE", "1")
    ChildEnvironment.scrub(pb)
    val p = pb.start()
    val out = new String(p.getInputStream.readAllBytes(), UTF_8)
    (p.waitFor(), out)
