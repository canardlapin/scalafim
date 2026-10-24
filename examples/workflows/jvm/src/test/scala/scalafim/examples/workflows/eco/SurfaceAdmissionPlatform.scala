package scalafim.examples.workflows.eco

import java.nio.file.Files
import neurosurf4s.surface.{Hemisphere, SurfaceKind}
import neurosurf4s.surface.gifti.{DeclaredGiftiSurface, GiftiTransformSelection}
import neurosurf4s.surface.io.GiftiSurfaceReader
import scala.concurrent.Future

object SurfaceAdmissionPlatform:
  def gifti(xml: String): Future[Either[String, DeclaredGiftiSurface]] =
    val path = Files.createTempFile("eco-surface-", ".surf.gii")
    try
      Files.writeString(path, xml)
      Future.successful(GiftiSurfaceReader.readDeclaredEither(path, Hemisphere.Left, SurfaceKind.Pial,
        GiftiTransformSelection.NativeCoordinates).left.map(_.message))
    finally Files.deleteIfExists(path)
