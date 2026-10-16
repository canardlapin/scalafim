package scalafim.dataset.io

import scalafim.dataset.DatasetError
import java.io.{BufferedInputStream, InputStream, InterruptedIOException}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.GZIPInputStream
import java.util.concurrent.locks.ReentrantLock
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** Expansion, aggregate storage and free-space bounds; eviction remains caller policy. */
final case class NiftiStagingLimits(
    maxExpandedBytes: Long = Long.MaxValue,
    minimumFreeBytes: Long = 0L,
    maxCacheBytes: Long = Long.MaxValue
):
  require(maxExpandedBytes >= 352L, "expanded NIfTI limit must allow its header")
  require(maxCacheBytes >= 417L, "cache limit must allow an image header and receipt")
  require(minimumFreeBytes >= 0L, "free-space floor must be non-negative")

/** Content-keyed, verified staging. Cache entries are immutable source revisions.
  * A digest receipt verifies the expanded bytes on every reuse. OS file locks and
  * bounded JVM lock stripes coordinate publishers; lock files are retained.
  * Cancellation retains the interrupt flag and removes only this call's partials.
  */
final class NiftiStagingCache private (val root: Path, val limits: NiftiStagingLimits):
  import NiftiStagingCache.*

  def stage(path: Path): Either[DatasetError, Path] =
    val source = path.toAbsolutePath.normalize()
    if !source.toString.endsWith(".gz") then Right(source)
    else
      try
        checkCancellation()
        val sourceHash = digestFile(source)
        val target = root.resolve(s"$sourceHash.nii")
        val localLock = stripe(root)
        localLock.lockInterruptibly()
        try
          checkCancellation()
          Using.resource(FileChannel.open(root.resolve(".staging.lock"),
            StandardOpenOption.CREATE, StandardOpenOption.WRITE)) { channel =>
            val lock = channel.lock()
            try stageLocked(source, sourceHash, target)
            finally lock.release()
          }
        finally localLock.unlock()
      catch
        case _: InterruptedException =>
          Thread.currentThread().interrupt()
          Left(DatasetError.StorageFailure("NIfTI staging cancelled while waiting for cache entry"))
        case NonFatal(error) =>
          Left(DatasetError.StorageFailure(s"failed to stage compressed NIfTI '$source': ${error.getMessage}"))

  /** Current bytes in this cache, including partials and receipts. */
  def sizeBytes: Either[DatasetError, Long] =
    try Right(usedBytes())
    catch case NonFatal(error) => Left(DatasetError.StorageFailure(error.getMessage))

  private def usedBytes(): Long =
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator().asScala.filter(p => Files.isRegularFile(p, java.nio.file.LinkOption.NOFOLLOW_LINKS))
        .foldLeft(0L)((total, path) => Math.addExact(total, Files.size(path)))
    }

  private def stageLocked(source: Path, sourceHash: String, target: Path): Either[DatasetError, Path] =
    val receipt = root.resolve(s"$sourceHash.sha256")
    val existingBytes = usedBytes()
    if existingBytes > limits.maxCacheBytes then
      Left(DatasetError.StorageFailure(s"cache exceeds ${limits.maxCacheBytes} byte aggregate limit; clear unused staged images"))
    else if Files.isRegularFile(target) && Files.size(target) > limits.maxExpandedBytes then
      Left(DatasetError.StorageFailure(s"expanded NIfTI exceeds ${limits.maxExpandedBytes} byte limit"))
    else if validEntry(target, receipt) then Right(target)
    else
      var partials = Vector.empty[Path]
      try
        val temporary = Files.createTempFile(root, ".nifti-stage-", ".partial")
        partials :+= temporary
        val compressedDigest = MessageDigest.getInstance("SHA-256")
        val expandedDigest = MessageDigest.getInstance("SHA-256")
        Using.Manager { use =>
          val raw = use(new DigestInputStream(Files.newInputStream(source), compressedDigest))
          // Match the bounded copy window: the default 512-byte decoder buffer
          // otherwise causes tiny reads and repeats every pre-write space check.
          val input = use(new GZIPInputStream(new BufferedInputStream(raw), 64 * 1024))
          val output = use(Files.newOutputStream(temporary))
          val buffer = new Array[Byte](64 * 1024)
          var total = 0L
          var count = input.read(buffer)
          while count >= 0 do
            checkCancellation()
            if count > limits.maxExpandedBytes - total then
              throw new IllegalArgumentException(s"expanded NIfTI exceeds ${limits.maxExpandedBytes} byte limit")
            if Files.getFileStore(root).getUsableSpace - count < limits.minimumFreeBytes then
              throw new IllegalArgumentException(s"staging would cross ${limits.minimumFreeBytes} byte free-space floor")
            if total > limits.maxCacheBytes - existingBytes - 65L - count then
              throw new IllegalArgumentException(s"staging exceeds ${limits.maxCacheBytes} byte aggregate cache limit")
            output.write(buffer, 0, count)
            expandedDigest.update(buffer, 0, count)
            total += count
            count = input.read(buffer)
          // Include trailing compressed-source bytes not needed by the decoder.
          drain(raw)
          if total < 352L then throw new IllegalArgumentException("staged NIfTI is smaller than its header")
        }.fold(error => throw error, identity)
        if hex(compressedDigest.digest()) != sourceHash then
          throw new IllegalArgumentException("compressed source changed while staging; retry with a stable source")
        checkCancellation()
        val receiptTemp = Files.createTempFile(root, ".nifti-receipt-", ".partial")
        partials :+= receiptTemp
        Files.writeString(receiptTemp, hex(expandedDigest.digest()) + "\n")
        publish(temporary, target)
        publish(receiptTemp, receipt)
        Right(target)
      finally partials.foreach(Files.deleteIfExists)

  private def validEntry(target: Path, receipt: Path): Boolean =
    Files.isRegularFile(target) && Files.size(target) >= 352L &&
      Files.isRegularFile(receipt) && Files.size(receipt) == 65L &&
      Files.readString(receipt).trim == digestFile(target)

object NiftiStagingCache:
  private val stripes = Array.fill(64)(new ReentrantLock)
  private def stripe(path: Path): ReentrantLock = stripes((path.toString.hashCode & Int.MaxValue) % stripes.length)
  private def checkCancellation(): Unit =
    if Thread.currentThread().isInterrupted then throw new InterruptedIOException("NIfTI staging cancelled")

  private def drain(input: InputStream): Unit =
    val bytes = new Array[Byte](64 * 1024)
    var count = input.read(bytes)
    while count >= 0 do
      checkCancellation()
      count = input.read(bytes)

  private def digestFile(path: Path): String =
    val digest = MessageDigest.getInstance("SHA-256")
    Using.resource(new DigestInputStream(Files.newInputStream(path), digest))(drain)
    hex(digest.digest())

  private def hex(bytes: Array[Byte]): String = bytes.iterator.map(b => f"${b & 0xff}%02x").mkString

  private def publish(source: Path, target: Path): Unit =
    try Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: java.nio.file.AtomicMoveNotSupportedException =>
        Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)
    ()

  def make(root: Path): Either[DatasetError, NiftiStagingCache] = make(root, NiftiStagingLimits())

  def make(root: Path, limits: NiftiStagingLimits): Either[DatasetError, NiftiStagingCache] =
    val normalized = root.toAbsolutePath.normalize()
    try
      Files.createDirectories(normalized)
      Right(new NiftiStagingCache(normalized.toRealPath(), limits))
    catch
      case NonFatal(error) =>
        Left(DatasetError.StorageFailure(s"cannot create NIfTI staging root '$normalized': ${error.getMessage}"))

  def unsafe(root: Path): NiftiStagingCache = unsafe(root, NiftiStagingLimits())

  def unsafe(root: Path, limits: NiftiStagingLimits): NiftiStagingCache =
    make(root, limits).fold(error => throw new IllegalArgumentException(error.message), identity)
