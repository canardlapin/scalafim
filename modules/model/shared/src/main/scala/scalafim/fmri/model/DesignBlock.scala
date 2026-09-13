package scalafim.fmri.model

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.design.{ColumnId, DesignError, DesignFingerprint, DesignSchema, StructuralColumn}
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
    val matrixValues: DMat,
    val columns: Vector[DesignColumn],
    val schema: Option[DesignSchema]
):
  require(matrixValues.cols == columns.length, "design block columns must match matrix columns")
  require(columns.nonEmpty, "design block must contain at least one column")

  /** Detached compatibility export. Execution reads [[matrixValues]] directly. */
  def matrix: Mat =
    val data = new Array[Double](rows * cols)
    matrixValues.copyRowMajorTo(data)
    Mat.unsafe(rows, cols, data)

  def rows: Int = matrixValues.rows
  def cols: Int = matrixValues.cols
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
      fromColumns(Matrix.tabulate(matrix.rows, matrix.cols)(matrix.apply), out.result(), schema = None)

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
        _ <- validateSourceMatrix(eventModel.designMatrix, eventSchema0, "event")
        _ <- validateSourceMatrix(baselineModel.designMatrix, baselineSchema0, "baseline")
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
        columns = schema.columns.zipWithIndex.map { case (column, index) =>
          val source = if index < eventSchema.columns.length then DesignColumnSource.Event else DesignColumnSource.Baseline
          DesignColumn.fromStructural(column, source)
        }
        block <-
          if columns.isEmpty then Left(ModelError.EmptyDesignBlock)
          else fromColumns(schema.matrixValues, columns, schema = Some(schema))
      yield block

  private def validateSourceMatrix(matrix: Mat, schema: DesignSchema, source: String): Either[ModelError, Unit] =
    val values = schema.matrixValues
    def mismatch: Either[ModelError, Unit] =
      Left(ModelError.DesignFailure(DesignError.InvalidSchema(
        s"$source design matrix differs from its compiled schema; rebuild the schema when changing numerical columns"
      )))
    if matrix.rows != values.rows || matrix.cols != values.cols then mismatch
    else
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < matrix.cols do
          if java.lang.Double.doubleToLongBits(matrix(row, col)) != java.lang.Double.doubleToLongBits(values(row, col)) then
            return mismatch
          col += 1
        row += 1
      Right(())

  private def fromColumns(
      matrix: DMat,
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
      case Some(value) if value.matrixValues.rows != matrix.rows || value.matrixValues.cols != matrix.cols =>
        Left(ModelError.BuildFailed("design schema matrix does not match DesignBlock matrix"))
      case _ => Right(new DesignBlock(matrix, columns, schema))
