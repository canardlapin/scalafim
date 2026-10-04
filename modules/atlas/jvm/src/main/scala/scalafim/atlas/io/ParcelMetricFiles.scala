package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import locus4s.data.Field
import scala.util.control.NonFatal
import scalafim.atlas.*

enum ParcelMetricFileError:
  case Codec(error: ParcelMetricError)
  case Io(path: Path, details: String)

  def message: String = this match
    case Codec(error) => error.message
    case Io(path, details) => s"metric file $path: $details"

/** UTF-8 file boundary. Writes create a new file and never overwrite one. */
object ParcelMetricFiles:
  def write(realization: AtlasRealization)(
      path: Path,
      values: Field[realization.P, Double],
      schema: ParcelMetricSchema
  ): Either[ParcelMetricFileError, Unit] =
    ParcelMetricJson.encode(realization)(values, schema)
      .left.map(ParcelMetricFileError.Codec.apply)
      .flatMap(writeDocument(path, _))

  def write(realization: AtlasRealization)(
      path: Path,
      metric: RestoredParcelMetric[realization.P]
  ): Either[ParcelMetricFileError, Unit] =
    ParcelMetricJson.encode(realization)(metric)
      .left.map(ParcelMetricFileError.Codec.apply)
      .flatMap(writeDocument(path, _))

  private def writeDocument(path: Path, document: String): Either[ParcelMetricFileError, Unit] =
    try
      Files.writeString(path, document, StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
      Right(())
    catch
      case NonFatal(error) => Left(ParcelMetricFileError.Io(path, error.toString))

  def read(target: AtlasRealization)(
      path: Path,
      expectedSha256: Option[String] = None
  ): Either[ParcelMetricFileError, RestoredParcelMetric[target.P]] =
    try
      ParcelMetricJson.decode(target)(Files.readString(path, StandardCharsets.UTF_8), expectedSha256)
        .left.map(ParcelMetricFileError.Codec.apply)
    catch
      case NonFatal(error) => Left(ParcelMetricFileError.Io(path, error.toString))
