package scalafim.archive.io

import java.io.OutputStream
import java.nio.channels.FileChannel
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.security.MessageDigest
import java.util.UUID
import scala.util.control.NonFatal
import scalafim.archive.ContentDigest

enum LocalStoreError:
  case Invalid(detail: String)
  case Integrity(detail: String)
  case Conflict(detail: String)
  case Io(detail: String)

  def message: String = this match
    case Invalid(detail) => detail
    case Integrity(detail) => detail
    case Conflict(detail) => detail
    case Io(detail) => detail

final class StagedFile private[io] (val path: Path, private[io] val owner: LocalObjectStore)

final case class VerifiedFileObject(path: String, digest: ContentDigest, bytes: Long)

/** Local-filesystem immutable objects. Streaming callbacks own only their write
  * interval. No payload is retained by this class. Publication uses an exclusive
  * hard link followed by directory fsync, rather than a replacing move.
  *
  * Directory fsync and hard links must be supported by the destination filesystem.
  * Callers retain their owned staging on failure for explicit recovery. This class
  * never deletes unreachable objects or another writer's staging.
  */
final class LocalObjectStore private (val root: Path, val bufferBytes: Int):
  private val noFollow = LinkOption.NOFOLLOW_LINKS

  private def protect[A](body: => Either[LocalStoreError, A]): Either[LocalStoreError, A] =
    try body
    catch
      case _: java.nio.file.FileAlreadyExistsException => Left(LocalStoreError.Conflict("immutable destination already exists"))
      case _: java.nio.channels.OverlappingFileLockException => Left(LocalStoreError.Conflict("publisher lock is held"))
      case NonFatal(error) => Left(LocalStoreError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private def resolve(relative: String): Either[LocalStoreError, Path] =
    val segments = relative.split("/", -1).toVector
    if relative.isEmpty || relative.startsWith("/") || relative.contains('\\') || relative.contains(':') ||
        relative.exists(_.isControl) || segments.exists(s => s.isEmpty || s == "." || s == "..") then
      Left(LocalStoreError.Invalid("object paths must be safe dataset-root-relative paths"))
    else
      var path = root
      var failure: Option[LocalStoreError] = None
      segments.foreach: segment =>
        path = path.resolve(segment)
        if Files.isSymbolicLink(path) then failure = Some(LocalStoreError.Invalid("symbolic links are not admitted in object paths"))
      failure.toLeft(path)

  private def syncDirectory(path: Path): Unit =
    val channel = FileChannel.open(path, StandardOpenOption.READ)
    try channel.force(true)
    finally channel.close()

  private def createParents(path: Path): Unit =
    val parent = path.getParent
    if parent != root && !Files.exists(parent, noFollow) then
      createParents(parent)
      try Files.createDirectory(parent)
      catch case _: java.nio.file.FileAlreadyExistsException => ()
      syncDirectory(parent.getParent)
    require(Files.isDirectory(parent, noFollow), "object parent is not a directory")

  /** A uniquely owned, same-filesystem staging path; only this returned path may
    * be filled by an external physical writer before publishStaged is called.
    */
  def stage(suffix: String): Either[LocalStoreError, StagedFile] = protect:
    if !suffix.matches("[.a-zA-Z0-9_-]*") then Left(LocalStoreError.Invalid("invalid staging suffix"))
    else
      val directory = root.resolve(".staging")
      if Files.isSymbolicLink(directory) then Left(LocalStoreError.Invalid("staging directory is a symlink"))
      else
        Files.createDirectories(directory)
        val owned = Files.createDirectory(directory.resolve(UUID.randomUUID().toString))
        syncDirectory(directory)
        Right(new StagedFile(owned.resolve("payload" + suffix), this))

  def write(relative: String)(produce: OutputStream => Unit): Either[LocalStoreError, VerifiedFileObject] =
    stage(".bin").flatMap: staging =>
      protect:
        val stream = Files.newOutputStream(staging.path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        try produce(stream)
        finally stream.close()
        publishStaged(staging, relative)

  def inspect(relative: String): Either[LocalStoreError, VerifiedFileObject] = protect:
    resolve(relative).flatMap: path =>
      if !Files.isRegularFile(path, noFollow) then Left(LocalStoreError.Integrity(s"missing regular object $relative"))
      else
        val digest = MessageDigest.getInstance("SHA-256")
        val input = Files.newInputStream(path)
        val buffer = new Array[Byte](bufferBytes)
        var count = 0L
        try
          var n = input.read(buffer)
          while n >= 0 do
            digest.update(buffer, 0, n)
            count = Math.addExact(count, n.toLong)
            n = input.read(buffer)
        finally input.close()
        val hex = digest.digest().iterator.map(b => f"${b & 0xff}%02x").mkString
        Right(VerifiedFileObject(relative, ContentDigest.unsafeSha256(hex), count))

  def verify(reference: VerifiedFileObject): Either[LocalStoreError, Unit] =
    inspect(reference.path).flatMap: actual =>
      if actual == reference then Right(()) else Left(LocalStoreError.Integrity(s"digest or length mismatch for ${reference.path}"))

  /** Requires a path from this store's staging namespace. The provider has
    * already closed its writer; this method fsyncs and publishes exact bytes.
    */
  def publishStaged(staging: StagedFile, relative: String): Either[LocalStoreError, VerifiedFileObject] = protect:
    val normalized = staging.path.toAbsolutePath.normalize()
    val stagingRoot = root.resolve(".staging")
    if (staging.owner ne this) || !normalized.startsWith(stagingRoot) || normalized.getNameCount != stagingRoot.getNameCount + 2 ||
        Files.isSymbolicLink(normalized.getParent) || !Files.isRegularFile(normalized, noFollow) then
      Left(LocalStoreError.Invalid("publication requires a regular store-owned staging file"))
    else resolve(relative).flatMap: destination =>
      val channel = FileChannel.open(normalized, StandardOpenOption.WRITE)
      try channel.force(true)
      finally channel.close()
      createParents(destination)
      Files.createLink(destination, normalized)
      syncDirectory(destination.getParent)
      // The immutable name is now durable. Remove this staging alias so it cannot
      // accidentally become an alternate writable name for the published inode.
      Files.delete(normalized)
      syncDirectory(normalized.getParent)
      inspect(relative)

  /** Serialize discovery-pointer replacement and reject a stale expected digest.
    * Immutable object publication remains independent of this lock.
    */
  def compareAndSwapPointer(
      relative: String,
      expected: Option[ContentDigest],
      bytes: Array[Byte]
  ): Either[LocalStoreError, VerifiedFileObject] = protect:
    resolve(relative).flatMap: destination =>
      val lockPath = root.resolve(".publisher.lock")
      if Files.isSymbolicLink(lockPath) then Left(LocalStoreError.Invalid("publisher lock is a symlink"))
      else
        val channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        try
          val lock = channel.tryLock()
          if lock == null then Left(LocalStoreError.Conflict("publisher lock is held"))
          else try
            val current = if Files.exists(destination, noFollow) then inspect(relative).map(x => Some(x.digest)) else Right(None)
            current.flatMap: actual =>
              if actual != expected then Left(LocalStoreError.Conflict("stale discovery pointer; re-read and merge compatible additions"))
              else stage(".json").flatMap: temporary =>
                Files.write(temporary.path, bytes, StandardOpenOption.CREATE_NEW)
                val output = FileChannel.open(temporary.path, StandardOpenOption.WRITE)
                try output.force(true)
                finally output.close()
                createParents(destination)
                Files.move(temporary.path, destination, java.nio.file.StandardCopyOption.ATOMIC_MOVE,
                  java.nio.file.StandardCopyOption.REPLACE_EXISTING)
                syncDirectory(destination.getParent)
                inspect(relative)
          finally lock.release()
        finally channel.close()

object LocalObjectStore:
  def open(root: Path, bufferBytes: Int = 65536): Either[LocalStoreError, LocalObjectStore] =
    if bufferBytes <= 0 then Left(LocalStoreError.Invalid("buffer size must be positive"))
    else
      try
        val absolute = root.toAbsolutePath.normalize()
        if Files.isSymbolicLink(absolute) then Left(LocalStoreError.Invalid("dataset root is a symlink"))
        else
          Files.createDirectories(absolute)
          Right(new LocalObjectStore(absolute.toRealPath(), bufferBytes))
      catch case NonFatal(error) => Left(LocalStoreError.Io(error.getMessage))
