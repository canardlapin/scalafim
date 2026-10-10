package scalafim.surface.io

import scalafim.surface.*
import scalafim.surface.gifti.*
import scala.concurrent.Future

class GiftiPlacementReaderSuite extends GiftiPlacementReaderChecks:
  def decode(document: GiftiDocument, selection: GiftiTransformSelection) =
    Future.successful(GiftiSurfaceReader.declared(document, Hemisphere.Left, SurfaceKind.Pial, selection))
  def convenience(document: GiftiDocument) =
    Future.successful(GiftiSurfaceReader.geometry(document, Hemisphere.Left, SurfaceKind.Pial))
