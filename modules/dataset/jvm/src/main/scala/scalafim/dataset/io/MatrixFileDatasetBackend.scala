package scalafim.dataset.io

import scalafim.dataset.{DataSelection, DatasetBackend, DatasetId, DatasetMetadata, DatasetShape, FmriSeries}
import scalafim.image.{DMat, Mask, NeuroSpace}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.io.Source

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

  private lazy val data: DMat =
    val rows = MatrixFileDatasetBackend.readRows(path, delimiter, hasHeader)
    require(rows.nonEmpty, s"matrix file '$path' did not contain any data rows")
    val cols = rows.head.length
    require(rows.forall(_.length == cols), s"matrix file '$path' contains ragged rows")
    require(cols == space.spatialDims.product, s"matrix file '$path' has $cols columns but space has ${space.spatialDims.product} voxels")
    DMat.fromRows(rows)

  override lazy val shape: DatasetShape =
    DatasetShape(space, data.rows)

  override lazy val mask: Mask.MaskVol =
    Mask.all(space)

  override def read(selection: DataSelection = DataSelection.All): FmriSeries =
    val resolved = selection.resolve(shape)
    val rows = resolved.timepoints.map { r =>
      resolved.voxels.map(c => data(r, c))
    }
    FmriSeries(
      data = DMat.fromRows(rows),
      voxelIndices = resolved.voxels,
      timepoints = resolved.timepoints,
      shape = shape,
      metadata = metadata
    )

object MatrixFileDatasetBackend:

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
