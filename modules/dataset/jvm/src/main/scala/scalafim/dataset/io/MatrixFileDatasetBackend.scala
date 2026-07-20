package scalafim.dataset.io

import scalafim.dataset.{DataSelection, DatasetBackend, DatasetError, DatasetId, DatasetMetadata, DatasetShape, FmriSeries, VoxelDomain}
import scalafim.image.{DMat, Mask, NeuroSpace}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.io.Source
import scala.util.control.NonFatal

enum MatrixFileDelimiter:
  case Comma, Tab, Whitespace

final case class MatrixFileDatasetBackend(
    id: DatasetId,
    path: Path,
    space: NeuroSpace,
    delimiter: MatrixFileDelimiter = MatrixFileDelimiter.Comma,
    hasHeader: Boolean = false,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:

  private lazy val loadedEither: Either[DatasetError, MatrixFileDatasetBackend.LoadedMatrix] =
    MatrixFileDatasetBackend.load(path, space, delimiter, hasHeader)

  override lazy val shape: DatasetShape =
    loadedEither.fold(error => throw new IllegalArgumentException(error.message), _.shape)

  override lazy val mask: Mask.MaskVol =
    Mask.all(shape.space)

  override lazy val voxelDomain: VoxelDomain =
    VoxelDomain.fullUnsafe(shape)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      loaded <- loadedEither
      resolved <- selection.resolveEither(loaded.shape, voxelDomain)
      series <- FmriSeries.make(
        data = DMat.fromRows(
          resolved.timepoints.map { r =>
            resolved.voxels.map(c => loaded.data(r, c))
          }
        ),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = loaded.shape,
        metadata = metadata
      )
    yield series

object MatrixFileDatasetBackend:
  private final case class LoadedMatrix(data: DMat, shape: DatasetShape)

  private def load(
      path: Path,
      space: NeuroSpace,
      delimiter: MatrixFileDelimiter,
      hasHeader: Boolean
  ): Either[DatasetError, LoadedMatrix] =
    try
      val rows = readRows(path, delimiter, hasHeader)
      if rows.isEmpty then Left(DatasetError.StorageFailure(s"matrix file '$path' did not contain any data rows"))
      else
        val cols = rows.head.length
        if rows.exists(_.length != cols) then Left(DatasetError.StorageFailure(s"matrix file '$path' contains ragged rows"))
        else
          for
            shape <- DatasetShape.make(space, rows.length)
            loaded <-
              if cols != shape.spatialSize then
                Left(DatasetError.ShapeMismatch(s"matrix file '$path' has $cols columns but space has ${shape.spatialSize} voxels"))
              else Right(LoadedMatrix(DMat.fromRows(rows), shape))
          yield loaded
    catch case NonFatal(e) => Left(DatasetError.StorageFailure(e.getMessage))

  def readRows(
      path: Path,
      delimiter: MatrixFileDelimiter = MatrixFileDelimiter.Comma,
      hasHeader: Boolean = false
  ): Vector[Vector[Double]] =
    val source = Source.fromFile(path.toFile, StandardCharsets.UTF_8.name())
    try
      val parsed = source
        .getLines()
        .zipWithIndex
        .collect {
          case (line, index) if line.trim.nonEmpty && !line.trim.startsWith("#") =>
            (line, index + 1)
        }
        .toVector

      val dataLines = if hasHeader && parsed.nonEmpty then parsed.tail else parsed
      dataLines.map { case (line, lineNumber) =>
        parseLine(line, lineNumber, delimiter)
      }
    finally source.close()

  private def parseLine(
      line: String,
      lineNumber: Int,
      delimiter: MatrixFileDelimiter
  ): Vector[Double] =
    val tokens =
      delimiter match
        case MatrixFileDelimiter.Comma =>
          line.split(",", -1).iterator.map(_.trim).toVector
        case MatrixFileDelimiter.Tab =>
          line.split("\t", -1).iterator.map(_.trim).toVector
        case MatrixFileDelimiter.Whitespace =>
          line.trim.split("\\s+").iterator.toVector

    require(tokens.nonEmpty && tokens.forall(_.nonEmpty), s"matrix file line $lineNumber contains an empty field")
    tokens.map { token =>
      val value =
        try token.toDouble
        catch
          case _: NumberFormatException =>
            throw new IllegalArgumentException(s"matrix file line $lineNumber has non-numeric value '$token'")
      require(value.isFinite, s"matrix file line $lineNumber has non-finite value '$token'")
      value
    }
