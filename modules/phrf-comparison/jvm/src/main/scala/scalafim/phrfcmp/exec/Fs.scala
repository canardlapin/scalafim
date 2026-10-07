package scalafim.phrfcmp.exec

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Small file helpers: SHA-256 and durable atomic writes (fsynced temp file in the same directory, `ATOMIC_MOVE`,
  * fsynced directory).
  */
object Fs:
  def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  def unhex(s: String): Array[Byte] = s.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray

  def sha256(bytes: Array[Byte]): String = hex(MessageDigest.getInstance("SHA-256").digest(bytes))

  def sha256File(path: Path): String = sha256(Files.readAllBytes(path))

  /** Writes `bytes` to a temp file beside `path`, fsyncs it, renames it atomically and fsyncs the directory, so
    * readers never see a torn file and the rename survives a power loss (the CPU state in `cost.json` depends on it).
    */
  def writeAtomic(path: Path, bytes: Array[Byte]): Unit =
    val dir = path.toAbsolutePath.getParent
    Files.createDirectories(dir)
    val tmp = Files.createTempFile(dir, path.getFileName.toString + ".", ".tmp")
    try
      writeFsync(tmp, bytes)
      val _ = Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      fsyncDir(dir)
    finally Files.deleteIfExists(tmp): Unit

  /** Writes and forces `bytes` (data and metadata) to `path`. */
  def writeFsync(path: Path, bytes: Array[Byte]): Unit =
    val ch = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
    try
      val bb = ByteBuffer.wrap(bytes)
      while bb.hasRemaining do ch.write(bb): Unit
      ch.force(true)
    finally ch.close()

  /** Forces a directory's entries (a rename or a create) to disk; best effort where the platform refuses it. */
  def fsyncDir(d: Path): Unit =
    try
      val ch = FileChannel.open(d, StandardOpenOption.READ)
      try ch.force(true)
      finally ch.close()
    catch case _: IOException => ()

  def writeAtomic(path: Path, text: String): Unit = writeAtomic(path, text.getBytes(UTF_8))

  /** Moves `from` onto `to` atomically (same file system). */
  def moveAtomic(from: Path, to: Path): Unit =
    Files.createDirectories(to.getParent)
    val _ = Files.move(from, to, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)

  def shaPath(data: Path): Path = data.resolveSibling(data.getFileName.toString + ".sha256")

  /** `<sha256>  <name>` written after the data it covers. */
  def writeSha(data: Path): Unit =
    writeAtomic(shaPath(data), s"${sha256File(data)}  ${data.getFileName}\n")

  /** True iff the sha file exists and matches the data; false if either is missing; throws if they disagree. */
  def shaState(data: Path): ShaState =
    val sha = shaPath(data)
    if !Files.exists(data) then ShaState.NoData
    else if !Files.exists(sha) then ShaState.NoSha
    else if new String(Files.readAllBytes(sha), UTF_8).trim.split("\\s+").head == sha256File(data) then ShaState.Valid
    else ShaState.Mismatch

  enum ShaState:
    case NoData, NoSha, Valid, Mismatch

  def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val s = Files.walk(root)
      try s.sorted(java.util.Comparator.reverseOrder[Path]()).iterator().asScala.foreach(p => Files.deleteIfExists(p): Unit)
      finally s.close()

  def listFiles(root: Path): Vector[Path] =
    if !Files.exists(root) then Vector.empty
    else
      val s = Files.walk(root)
      try s.iterator().asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString)
      finally s.close()
