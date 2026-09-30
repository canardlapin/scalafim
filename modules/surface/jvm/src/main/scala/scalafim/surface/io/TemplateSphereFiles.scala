package scalafim.surface.io

import java.nio.file.{Files, Path}
import java.security.MessageDigest

import scalafim.surface.*

import scala.util.control.NonFatal

/** Loads pinned TemplateFlow registration spheres (GIFTI) as typed [[TemplateSphere]]s. A file whose bytes differ from
  * the pinned SHA-256 is refused, so parity evidence always refers to the coordinates in use.
  */
object TemplateSphereFiles:
  /** `surface` on `registration`, from `path`. */
  def load[R <: SphereRegistration & Singleton](
      surface: TemplateSurface,
      registration: R,
      path: Path
  ): Either[SurfaceError, TemplateSphere[R]] =
    for
      asset <- TemplateSphereAssets
        .find(surface, registration)
        .toRight(SurfaceError.ReadFailure(path.toString, s"TemplateFlow publishes no ${surface.display} sphere on $registration"))
      digest <- sha256(path)
      _ <-
        if digest.equalsIgnoreCase(asset.sha256) then Right(())
        else Left(SurfaceError.ReadFailure(path.toString, s"SHA-256 $digest is not the pinned ${asset.relativePath} (${asset.sha256})"))
      geometry <- GiftiSurfaceReader.readEither(path, surface.hemisphere.tag, SurfaceKind.Sphere)
      sphere   <- TemplateSphere.admit[R](surface, registration, geometry.mesh, asset.identity)
    yield sphere

  /** `surface` on `registration`, from a local TemplateFlow cache ([[TemplateFlowCache.roots]] by default). */
  def loadCached[R <: SphereRegistration & Singleton](
      surface: TemplateSurface,
      registration: R,
      roots: Vector[Path] = TemplateFlowCache.roots
  ): Either[SurfaceError, TemplateSphere[R]] =
    TemplateSphereAssets
      .relativePath(surface, registration)
      .toRight(SurfaceError.ReadFailure(surface.display, s"TemplateFlow publishes no ${surface.display} sphere on $registration"))
      .flatMap: relative =>
        TemplateFlowCache
          .locate(relative, roots)
          .toRight(SurfaceError.ReadFailure(relative, s"not in any TemplateFlow cache (${roots.mkString(", ")})"))
      .flatMap(load[R](surface, registration, _))

  private def sha256(path: Path): Either[SurfaceError, String] =
    try
      val digest = MessageDigest.getInstance("SHA-256")
      val in = Files.newInputStream(path)
      try
        val buffer = new Array[Byte](1 << 16)
        var n = in.read(buffer)
        while n >= 0 do
          if n > 0 then digest.update(buffer, 0, n)
          n = in.read(buffer)
      finally in.close()
      Right(digest.digest().map(b => f"${b & 0xff}%02x").mkString)
    catch case NonFatal(error) => Left(SurfaceError.ReadFailure(path.toString, SurfaceError.reason(error)))
