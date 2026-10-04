package scalafim.surface.io

import scalafim.surface.*
import scalafim.surface.gifti.*

class GiftiPlacementReaderJsSuite extends GiftiPlacementReaderChecks:
  def decode(document: GiftiDocument, selection: GiftiTransformSelection) =
    GiftiSurfaceReader.placedGeometry(document, Hemisphere.Left, SurfaceKind.Pial, selection)
  def convenience(document: GiftiDocument) =
    GiftiSurfaceReader.geometry(document, Hemisphere.Left, SurfaceKind.Pial)
