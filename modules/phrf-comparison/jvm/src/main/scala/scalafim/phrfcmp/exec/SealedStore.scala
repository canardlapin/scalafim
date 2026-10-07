package scalafim.phrfcmp.exec

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*

/** Fault-injection stages inside `append`. */
enum StoreStage:
  case TmpWritten, Published

/** Test seams for the sealed store; production uses [[StoreHook.none]]. `corruptBlob` runs between encryption and
  * the read-back check; `corruptDisk` runs after the durable write and before the re-read.
  */
trait StoreHook:
  def at(stage: StoreStage, name: String): Unit = ()
  def corruptBlob(blob: Array[Byte]): Array[Byte] = blob
  def corruptDisk(path: Path): Unit = ()

object StoreHook:
  val none: StoreHook = new StoreHook {}

final case class SealReceipt(format: String, recipientFp: String, sealedTreeDigest: String)

/** Append-only encrypted blob directory (phrf-sealed/1). `append` is the only way raw results reach disk: every
  * blob is encrypted to the owner's public key with a fresh ephemeral key, decrypted back before publication and
  * re-read after the durable write. Temporary files hold ciphertext only. Writers are stateless and thread-safe.
  */
final class SealedStore private (val dir: Path, recipient: RecipientKey, entropy: SealEntropy, hook: StoreHook):
  private val blobsDir = dir.resolve("blobs")
  private val closed = new AtomicBoolean(false)

  def hasBlobs: Boolean = SealedStore.listBlobs(blobsDir).nonEmpty || SealedStore.listTmp(blobsDir).nonEmpty

  /** The owner key's fingerprint (bound into the runner's stamp). */
  def recipientFingerprint: String = recipient.fingerprint

  /** Seals one blob and returns its file hash. Nothing is published on any failure. */
  def append(name: String, data: Array[Byte]): Either[SealError, String] =
    if closed.get() then Left(SealError.StoreClosed)
    else publish(SealFormat.KindData, name, data)

  /** The ephemeral private key is zeroed on every path, a throwing hook or read-back included. */
  private def publish(kind: Int, name: String, data: Array[Byte]): Either[SealError, String] =
    Sealer.seal(recipient, kind, name, data, entropy).flatMap { sb =>
      val checked =
        try
          val blob = hook.corruptBlob(sb.blob)
          Sealer.verifyReadback(recipient, sb.ephemeralPrivate, blob, kind, name, data).map(_ => blob)
        finally java.util.Arrays.fill(sb.ephemeralPrivate, 0.toByte)
      checked.flatMap(writeDurable(_, name))
    }

  private def writeDurable(blob: Array[Byte], name: String): Either[SealError, String] =
    val hash = Fs.sha256(blob)
    val path = blobsDir.resolve(s"$hash.enc")
    val tmp = blobsDir.resolve(s"$hash.enc.tmp")
    try
      SealedStore.writeFsync(tmp, blob)
      hook.at(StoreStage.TmpWritten, name)
      val _ = Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      SealedStore.fsyncDir(blobsDir)
      hook.at(StoreStage.Published, name)
      hook.corruptDisk(path)
      if java.util.Arrays.equals(Files.readAllBytes(path), blob) then Right(hash)
      else
        Files.deleteIfExists(path): Unit
        Left(SealError.ReadBackMismatch("disk"))
    catch case e: IOException => Left(SealError.Io(e.getClass.getSimpleName))

  /** Writes the CLOSE record and the SEALED receipt; exactly once, by the custodian's final process. */
  def close(): Either[SealError, SealReceipt] =
    if Files.exists(dir.resolve("SEALED")) || !closed.compareAndSet(false, true) then Left(SealError.StoreClosed)
    else
      try
        SealedStore.listTmp(blobsDir).foreach(p => Files.deleteIfExists(p): Unit)
        val hs = SealedStore.listBlobs(blobsDir).map(_.getFileName.toString.stripSuffix(".enc")).sorted
        val digest = Fs.sha256(hs.mkString("\n").getBytes(UTF_8))
        val info = s"""{"blobs_digest": "$digest", "format": "${SealFormat.Format}", "n_blobs": ${hs.length}}"""
        for
          _ <- publish(SealFormat.KindClose, SealFormat.CloseName, info.getBytes(UTF_8))
          tree = SealedStore.digest(dir)
          receipt = SealReceipt(SealFormat.Format, recipient.fingerprint, tree)
          text =
            s"""{
               | "format": "${receipt.format}",
               | "recipient_fp": "${receipt.recipientFp}",
               | "sealed_tree_digest": "${receipt.sealedTreeDigest}"
               |}
               |""".stripMargin
          _ <- try
            SealedStore.writeFsync(dir.resolve("SEALED.tmp"), text.getBytes(UTF_8))
            Files.move(dir.resolve("SEALED.tmp"), dir.resolve("SEALED"), StandardCopyOption.ATOMIC_MOVE)
            SealedStore.fsyncDir(dir)
            Right(())
          catch case e: IOException => Left(SealError.Io(e.getClass.getSimpleName))
        yield receipt
      catch case e: IOException => Left(SealError.Io(e.getClass.getSimpleName))

object SealedStore:
  /** Opens (or creates) a store; the fingerprint is checked before any byte is written. */
  def open(
      dir: Path,
      recipient: RecipientKey,
      expectFingerprint: String,
      entropy: SealEntropy = SealEntropy.secure,
      hook: StoreHook = StoreHook.none
  ): Either[SealError, SealedStore] =
    recipient.checkFingerprint(expectFingerprint).flatMap { _ =>
      try
        if Files.exists(dir.resolve("SEALED")) then Left(SealError.StoreClosed)
        else
          Files.createDirectories(dir.resolve("blobs"))
          Right(new SealedStore(dir, recipient, entropy, hook))
      catch case e: IOException => Left(SealError.Io(e.getClass.getSimpleName))
    }

  private def listDir(d: Path): Vector[Path] =
    if !Files.isDirectory(d) then Vector.empty
    else
      val s = Files.list(d)
      try s.iterator().asScala.toVector.filter(Files.isRegularFile(_))
      finally s.close()

  private[exec] def listBlobs(d: Path): Vector[Path] = listDir(d).filter(_.getFileName.toString.endsWith(".enc"))
  private[exec] def listTmp(d: Path): Vector[Path] = listDir(d).filter(_.getFileName.toString.endsWith(".tmp"))

  private[exec] def writeFsync(path: Path, bytes: Array[Byte]): Unit = Fs.writeFsync(path, bytes)

  private[exec] def fsyncDir(d: Path): Unit = Fs.fsyncDir(d)

  /** SHA-256 of the sorted `sha256sum`-style listing (`<hex>  <relative path>\n`), excluding `SEALED` and `*.tmp`. */
  def digest(store: Path): String =
    val files = Fs.listFiles(store).flatMap { p =>
      val rel = store.relativize(p).toString.replace('\\', '/')
      if Files.isSymbolicLink(p) then throw new IOException("symlink in tree")
      if rel == "SEALED" || rel.endsWith(".tmp") then None else Some(rel -> p)
    }
    val sorted = files.sortBy(_._1)(using Ordering.fromLessThan((a, b) => java.util.Arrays.compareUnsigned(a.getBytes(UTF_8), b.getBytes(UTF_8)) < 0))
    Fs.sha256(sorted.map((rel, p) => s"${Fs.sha256File(p)}  $rel\n").mkString.getBytes(UTF_8))
