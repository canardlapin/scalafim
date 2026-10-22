package scalafim.examples.workflows.eco

import neurosurf4s.surface.{Hemisphere, SurfaceKind}
import neurosurf4s.surface.gifti.{DeclaredGiftiSurface, GiftiTransformSelection}
import neurosurf4s.surface.io.GiftiSurfaceReader
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object SurfaceAdmissionPlatform:
  def gifti(xml: String): Future[Either[String, DeclaredGiftiSurface]] =
    GiftiSurfaceReader.readDeclaredString(xml, Hemisphere.Left, SurfaceKind.Pial,
      GiftiTransformSelection.NativeCoordinates).map(_.left.map(_.message))
