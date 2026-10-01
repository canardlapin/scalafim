package scalafim.surface.reference

import scalafim.surface.SurfaceError

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Reads a canonical `templateflow4s.point-map/1` directory: `manifest.json`
  * plus one `stage-<i>-displacement.nii` per displacement stage. The manifest
  * bytes are checked against the caller's expected SHA-256 before parsing;
  * every stage file is read once and verified against the manifest digest and
  * header before it is decoded; a quarantined manifest is refused.
  */
object DeclaredPointMapReader:
  def parse(text: String): Either[String, ManifestFields] = PointMapManifest.parse(text)

  def read(directory: Path, expectedSourceSha256: String, expectedManifestSha256: String): Either[ReferenceError, DeclaredPointMap] =
    for
      bytes <- attempt(directory)(Files.readAllBytes(directory.resolve("manifest.json")))
      manifest <- PointMapManifest.fromBytes(bytes, expectedManifestSha256)
      declared <- DeclaredPointMap.fromManifest(manifest, expectedSourceSha256, name =>
        val file = directory.resolve(name)
        if name.contains('/') || name.contains('\\') || !Files.isRegularFile(file) then None
        else scala.util.Try(Files.readAllBytes(file)).toOption)
    yield declared

  private def attempt[A](path: Path)(body: => A): Either[ReferenceError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(ReferenceError.AssetReadFailure(s"$path: ${SurfaceError.reason(error)}"))
