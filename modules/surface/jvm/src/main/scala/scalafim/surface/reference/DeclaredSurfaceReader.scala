package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.io.{GiftiReader, GiftiSurfaceReader}

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Loads a GIFTI surface under a frame declaration. The file is read once;
  * its bytes are hashed and checked against the declared asset digest before
  * those same bytes are decoded.
  */
object DeclaredSurfaceReader:
  def read(
    path: Path,
    declaration: FrameDeclaration,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Either[ReferenceError, DeclaredSurface] =
    for
      bytes <-
        try Right(Files.readAllBytes(path))
        catch case NonFatal(error) => Left(ReferenceError.AssetReadFailure(s"$path: ${SurfaceError.reason(error)}"))
      _ <- declaration.checkDigest(bytes)
      decoded <- GiftiReader.read(bytes).flatMap(GiftiSurfaceReader.declared(_, hemisphere, kind))
        .left.map(error => ReferenceError.AssetReadFailure(s"$path: ${error.message}"))
      surface <- DeclaredSurface.verified(declaration, bytes, decoded)
    yield surface
