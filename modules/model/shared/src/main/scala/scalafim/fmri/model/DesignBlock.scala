package scalafim.fmri.model

import scalafim.fmri.design.{ColumnId, DesignFingerprint, DesignSchema, StructuralColumn}
import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.linalg.Mat

enum DesignColumnSource:
  case Event, Baseline

final case class DesignColumn private[model] (
    id: ColumnId,
    name: String,
    source: DesignColumnSource,
    structural: Option[StructuralColumn] = None
)

object DesignColumn:
  def make(name: String, source: DesignColumnSource): Either[ModelError, DesignColumn] =
    ColumnId(name)
      .left
      .map(error => ModelError.InvalidId("column", name, error.message))
      .map(id => DesignColumn(id, name, source))

  def fromStructural(
      column: StructuralColumn,
      source: DesignColumnSource
  ): DesignColumn =
    DesignColumn(column.id, column.renderedLabel, source, Some(column))

final class DesignBlock private (
    val matrix: Mat,
    val columns: Vector[DesignColumn],
    val schema: Option[DesignSchema]
):
  require(matrix.cols == columns.length, "design block columns must match matrix columns")
  require(columns.nonEmpty, "design block must contain at least one column")

  def rows: Int = matrix.rows
  def cols: Int = matrix.cols
  def columnNames: Vector[String] = columns.map(_.name)
  def columnIds: Vector[ColumnId] = columns.map(_.id)
  def structuralColumns: Vector[StructuralColumn] = columns.flatMap(_.structural)
  def designFingerprint: Option[DesignFingerprint] = schema.map(_.fingerprint)

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
      fromColumns(matrix, out.result(), schema = None)

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
        eventSchema0 <- Right(eventModel.designSchema)
        baselineSchema0 <- Right(baselineModel.designSchema)
        // The structural schema is authoritative, but keep the public
        // rendered labels supplied by a compatibility copy of the model.
        // This permits legacy callers to rename labels without changing the
        // column identity or provenance used by the model and fit layers.
        eventSchema <-
          if eventSchema0.columns.map(_.renderedLabel) == eventModel.columnNames then Right(eventSchema0)
          else eventSchema0.withRenderedLabels(eventModel.columnNames).left.map(ModelError.fromDesignError)
        baselineSchema <-
          if baselineSchema0.columns.map(_.renderedLabel) == baselineModel.columnNames then Right(baselineSchema0)
          else baselineSchema0.withRenderedLabels(baselineModel.columnNames).left.map(ModelError.fromDesignError)
        schema <- DesignSchema.combine(eventSchema, baselineSchema).left.map(ModelError.fromDesignError)
        eventColumns = eventSchema.columns.map(DesignColumn.fromStructural(_, DesignColumnSource.Event))
        baselineColumns = baselineSchema.columns.map(DesignColumn.fromStructural(_, DesignColumnSource.Baseline))
        columns = eventColumns ++ baselineColumns
        block <-
          if columns.isEmpty then Left(ModelError.EmptyDesignBlock)
          else fromColumns(schema.matrix, columns, schema = Some(schema))
      yield block

  private def fromColumns(
      matrix: Mat,
      columns: Vector[DesignColumn],
      schema: Option[DesignSchema]
  ): Either[ModelError, DesignBlock] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    var i = 0
    while i < columns.length do
      val id = columns(i).id.value
      if seen.contains(id) then return Left(ModelError.DuplicateColumnId(id))
      seen += id
      i += 1
    schema match
      case Some(value) if value.matrix.rows != matrix.rows || value.matrix.cols != matrix.cols =>
        Left(ModelError.BuildFailed("design schema matrix does not match DesignBlock matrix"))
      case _ => Right(new DesignBlock(matrix, columns, schema))
