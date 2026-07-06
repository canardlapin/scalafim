package scalafim.atlas.io

import java.io.InputStream
import java.net.URI
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import scalafim.atlas.Digest

final case class AtlasAsset(
  key: String,
  fileName: String,
  uri: URI,
  minBytes: Long = 1L,
  sha256: Option[String] = None
):
  require(key.trim.nonEmpty, "asset key must be non-empty")
  require(fileName.trim.nonEmpty, "asset fileName must be non-empty")
  require(minBytes >= 0L, "asset minBytes must be non-negative")

enum AssetPolicy:
  case CacheOnly, CacheOrDownload, Refresh

trait AtlasStore:
  def resolve(asset: AtlasAsset, policy: AssetPolicy = AssetPolicy.CacheOrDownload): Path

final case class FileAtlasStore(root: Path) extends AtlasStore:
  Files.createDirectories(root)

  def resolve(asset: AtlasAsset, policy: AssetPolicy = AssetPolicy.CacheOrDownload): Path =
    val path = root.resolve(asset.fileName)
    val cachedOk = Files.exists(path) && validate(path, asset)
    policy match
      case AssetPolicy.CacheOnly =>
        if cachedOk then path
        else throw new IllegalStateException(s"atlas asset '${asset.key}' is not present in cache: $path")
      case AssetPolicy.CacheOrDownload =>
        if cachedOk then path else download(asset, path)
      case AssetPolicy.Refresh =>
        download(asset, path)

  private def download(asset: AtlasAsset, path: Path): Path =
    Files.createDirectories(path.getParent)
    val tmp = Files.createTempFile(path.getParent, s".${asset.key}.", ".tmp")
    try
      val in = asset.uri.toURL.openStream()
      try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      if !validate(tmp, asset) then
        throw new IllegalStateException(s"downloaded atlas asset '${asset.key}' failed validation")
      Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      path
    finally
      Files.deleteIfExists(tmp)

  private def validate(path: Path, asset: AtlasAsset): Boolean =
    Files.exists(path) &&
      Files.size(path) >= asset.minBytes &&
      asset.sha256.forall(expected => AtlasAsset.sha256(path).equalsIgnoreCase(expected))

object AtlasAsset:
  def digest(path: Path): Digest =
    Digest.sha256(sha256(path))

  def sha256(path: Path): String =
    val md = MessageDigest.getInstance("SHA-256")
    val in: InputStream = Files.newInputStream(path)
    try
      val buf = Array.ofDim[Byte](8192)
      var n = in.read(buf)
      while n >= 0 do
        if n > 0 then md.update(buf, 0, n)
        n = in.read(buf)
    finally in.close()
    md.digest().map(b => f"$b%02x").mkString

object FileAtlasStore:
  def default: FileAtlasStore =
    val base = Option(System.getenv("XDG_CACHE_HOME"))
      .filter(_.trim.nonEmpty)
      .map(Path.of(_))
      .getOrElse(Path.of(System.getProperty("user.home"), ".cache"))
    FileAtlasStore(base.resolve("scalafim").resolve("atlas"))
