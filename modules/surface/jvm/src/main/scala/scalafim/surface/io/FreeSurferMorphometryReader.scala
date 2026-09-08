package scalafim.surface.io

import java.nio.file.{Files, Path}
import scalafim.surface.*
import scalafim.surface.freesurfer.FreeSurferMorphometryCodec
import scala.util.control.NonFatal

object FreeSurferMorphometryReader:
  /** The scalar file does not encode vertex correspondence. Supply the reviewed
    * matching geometry; a recognizable opposite-hemisphere filename is rejected.
    */
  def readEither(path: Path, geometry: SurfaceGeometry,
      label: String = ""): Either[SurfaceError, SurfaceField[Double]] =
    try
      val hemisphere = FreeSurferSurfaceReader.inferHemisphere(path)
      if hemisphere != Hemisphere.Unknown && hemisphere != geometry.hemisphere then
        Left(SurfaceError.ReadFailure(path.toString, "morphometry filename hemisphere differs from geometry"))
      else
        val size = Files.size(path)
        val expectedModern = 15L + geometry.vertexCount.toLong * 4L
        val expectedLegacy = 6L + geometry.vertexCount.toLong * 2L
        if size != expectedModern && size != expectedLegacy then
          Left(SurfaceError.ReadFailure(path.toString, "morphometry file size differs from supplied geometry"))
        else FreeSurferMorphometryCodec.decode(Files.readAllBytes(path), geometry, label)
    catch case NonFatal(error) => Left(SurfaceError.ReadFailure(path.toString, SurfaceError.reason(error)))

  def read(path: Path, geometry: SurfaceGeometry, label: String = ""): SurfaceField[Double] =
    readEither(path, geometry, label).fold(error => throw IllegalArgumentException(error.message), identity)
