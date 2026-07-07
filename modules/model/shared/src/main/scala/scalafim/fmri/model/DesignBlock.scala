package scalafim.fmri.model

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.linalg.Mat

enum DesignColumnSource:
  case Event, Baseline

final case class DesignColumn private[model] (
    id: ColumnId,
    name: String,
    source: DesignColumnSource
)

object DesignColumn:
  def make(name: String, source: DesignColumnSource): Either[ModelError, DesignColumn] =
    ColumnId(name)
      .left
      .map(error => ModelError.InvalidId("column", name, error.message))
      .map(id => DesignColumn(id, name, source))

final class DesignBlock private (
    val matrix: Mat,
    val columns: Vector[DesignColumn]
):
  require(matrix.cols == columns.length, "design block columns must match matrix columns")
  require(columns.nonEmpty, "design block must contain at least one column")

  def rows: Int = matrix.rows
  def cols: Int = matrix.cols
  def columnNames: Vector[String] = columns.map(_.name)

object DesignBlock:
  def make(
      matrix: Mat,
      columnNames: Vector[String],
      source: DesignColumnSource
  ): Either[ModelError, DesignBlock] =
    if matrix.cols != columnNames.length then
      Left(ModelError.DesignColumnMismatch(source.toString.toLowerCase, matrix.cols, columnNames.length))
    else if columnNames.isEmpty then Left(ModelError.EmptyDesignBlock)
    else
      val out = Vector.newBuilder[DesignColumn]
      out.sizeHint(columnNames.length)
      var i = 0
      while i < columnNames.length do
        DesignColumn.make(columnNames(i), source) match
          case Left(error) => return Left(error)
          case Right(column) => out += column
        i += 1
      fromColumns(matrix, out.result())

  def fromModel(
      eventModel: EventModel,
      baselineModel: BaselineModel,
      expectedRows: Int
  ): Either[ModelError, DesignBlock] =
    if eventModel.designMatrix.rows != expectedRows then
      Left(ModelError.DesignRowMismatch("event", expectedRows, eventModel.designMatrix.rows))
    else if baselineModel.designMatrix.rows != expectedRows then
      Left(ModelError.DesignRowMismatch("baseline", expectedRows, baselineModel.designMatrix.rows))
    else if eventModel.designMatrix.cols != eventModel.columnNames.length then
      Left(ModelError.DesignColumnMismatch("event", eventModel.designMatrix.cols, eventModel.columnNames.length))
    else if baselineModel.designMatrix.cols != baselineModel.columnNames.length then
      Left(ModelError.DesignColumnMismatch("baseline", baselineModel.designMatrix.cols, baselineModel.columnNames.length))
    else
      for
        eventColumns <- bindColumns(eventModel.columnNames, DesignColumnSource.Event)
        baselineColumns <- bindColumns(baselineModel.columnNames, DesignColumnSource.Baseline)
        columns = eventColumns ++ baselineColumns
        block <-
          if columns.isEmpty then Left(ModelError.EmptyDesignBlock)
          else fromColumns(eventModel.designMatrix ++ baselineModel.designMatrix, columns)
      yield block

  private def bindColumns(
      columnNames: Vector[String],
      source: DesignColumnSource
  ): Either[ModelError, Vector[DesignColumn]] =
    val columns = Vector.newBuilder[DesignColumn]
    columns.sizeHint(columnNames.length)
    var i = 0
    while i < columnNames.length do
      DesignColumn.make(columnNames(i), source) match
        case Left(error) => return Left(error)
        case Right(column) => columns += column
      i += 1
    Right(columns.result())

  private def fromColumns(matrix: Mat, columns: Vector[DesignColumn]): Either[ModelError, DesignBlock] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    var i = 0
    while i < columns.length do
      val id = columns(i).id.value
      if seen.contains(id) then return Left(ModelError.DuplicateColumnId(id))
      seen += id
      i += 1
    Right(new DesignBlock(matrix, columns))
