package scalafim.bids

import cats.syntax.all.*

opaque type ColumnName = String

object ColumnName:
  def from(value: String): Either[BidsError, ColumnName] =
    val clean = value.trim
    if clean.isEmpty then Left(BidsError.InvalidTable("column name must be non-empty"))
    else Right(clean)

  def unsafe(value: String): ColumnName =
    val clean = value.trim
    require(clean.nonEmpty, "column name must be non-empty")
    clean

  extension (name: ColumnName)
    def value: String = name

final case class BidsColumn(name: String, values: Vector[Option[String]]):
  def nrows: Int = values.length

  def numeric: Either[BidsError, Vector[Option[Double]]] =
    BidsEither.traverse(values) {
      case None => Right(None)
      case Some(value) =>
        value.trim.toDoubleOption match
          case Some(number) if number.isFinite => Right(Some(number))
          case _ => Left(BidsError.InvalidTable(s"column '$name' contains non-numeric value '$value'"))
    }

final case class BidsTable private (columns: Vector[String], rows: Vector[Vector[Option[String]]]):
  def nrows: Int = rows.length
  def ncols: Int = columns.length

  def column(name: String): Either[BidsError, Vector[Option[String]]] =
    columnNamed(name).map(_.values)

  def columnNamed(name: String): Either[BidsError, BidsColumn] =
    val idx = columns.indexOf(name)
    if idx < 0 then Left(BidsError.InvalidTable(s"unknown column '$name'"))
    else Right(BidsColumn(name, rows.map(_(idx))))

  def columnAt(index: Int): Either[BidsError, BidsColumn] =
    if index < 0 || index >= columns.length then Left(BidsError.InvalidTable(s"column index $index out of bounds"))
    else Right(BidsColumn(columns(index), rows.map(_(index))))

  def typedColumns: Vector[BidsColumn] =
    columns.indices.toVector.map(index => BidsColumn(columns(index), rows.map(_(index))))

  def select(names: Vector[String]): Either[BidsError, BidsTable] =
    val indexes = names.map(columns.indexOf)
    val missing = names.zip(indexes).collect { case (name, -1) => name }
    if missing.nonEmpty then
      Left(BidsError.InvalidTable(s"unknown columns: ${missing.mkString(", ")}"))
    else
      BidsTable.fromRows(
        names,
        rows.map(row => indexes.map(row))
      )

object BidsTable:
  private val MissingTokens = Set("", "n/a", "NA", "N/A")

  def column(name: String, values: Vector[Option[String]]): Either[BidsError, BidsColumn] =
    if name.trim.isEmpty then Left(BidsError.InvalidTable("column name must be non-empty"))
    else Right(BidsColumn(name.trim, values))

  def fromRows(columns: Vector[String], rows: Vector[Vector[Option[String]]]): Either[BidsError, BidsTable] =
    fromRowsChecked(columns, rows).left.map(report => BidsError.InvalidTable(report.primary.message))

  def fromRowsChecked(
      columns: Vector[String],
      rows: Vector[Vector[Option[String]]]
  ): Either[BidsIssueReport, BidsTable] =
    val cleanColumns = columns.map(_.trim)
    val nonEmptyChecks =
      BidsValidation.all(
        cleanColumns.zipWithIndex.collect { case (name, index) if name.isEmpty =>
          BidsValidation.invalid(
            BidsIssue.error(
              BidsIssueCode.InvalidTable,
              None,
              Some(s"columns[$index]"),
              "column name must be non-empty"
            )
          )
        }
      )
    val duplicateChecks =
      BidsValidation.all(
        cleanColumns.zipWithIndex
          .groupBy(_._1)
          .toVector
          .collect { case (name, occurrences) if name.nonEmpty && occurrences.lengthCompare(1) > 0 =>
            val indexes = occurrences.map(_._2).sorted.mkString(", ")
            BidsValidation.invalid(
              BidsIssue.error(
                BidsIssueCode.InvalidTable,
                None,
                Some(name),
                s"column name '$name' is duplicated at indexes $indexes"
              )
            )
          }
      )
    val widthChecks =
      BidsValidation.all(
        rows.zipWithIndex.collect { case (row, index) if row.length != cleanColumns.length =>
          BidsValidation.invalid(
            BidsIssue.error(
              BidsIssueCode.InvalidTable,
              None,
              Some(s"rows[$index]"),
              s"row width ${row.length} does not match header width ${cleanColumns.length}"
            )
          )
        }
      )

    BidsValidation.toEither(
      (nonEmptyChecks, duplicateChecks, widthChecks).mapN((_, _, _) => BidsTable(cleanColumns, rows))
    )

  def parse(text: String): Either[BidsError, BidsTable] =
    parseChecked(text).left.map(report => BidsError.InvalidTable(report.primary.message))

  def parseChecked(text: String): Either[BidsIssueReport, BidsTable] =
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toVector
    if lines.isEmpty then
      Left(BidsIssueReport.unsafe(Vector(
        BidsIssue.error(BidsIssueCode.InvalidTable, None, None, "input is empty")
      )))
    else
      val tabDelimited = lines.head.contains('\t')
      val split: String => Vector[String] =
        if tabDelimited then line => line.split("\t", -1).toVector.map(_.trim)
        else line => line.trim.split("\\s+").toVector.map(_.trim)
      val columns = split(lines.head)
      val rows = lines.tail.map { line =>
        split(line).map(cell => Option.when(!MissingTokens(cell))(cell))
      }
      fromRowsChecked(columns, rows)

object BidsEvents:
  def readTable(text: String): Either[BidsError, BidsTable] =
    BidsTable.parse(text)

  def readEventsTable(text: String): Either[BidsError, EventsTable] =
    BidsTable.parse(text).flatMap(EventsTable.from)

final class EventsTable private (
    val table: BidsTable,
    val onset: BidsColumn,
    val duration: BidsColumn,
    val onsetSeconds: Vector[Option[Double]],
    val durationSeconds: Vector[Option[Double]]
):
  def nrows: Int = table.nrows
  def trialType: Option[BidsColumn] =
    table.columnNamed("trial_type").toOption

object EventsTable:
  val OnsetColumn: ColumnName = ColumnName.unsafe("onset")
  val DurationColumn: ColumnName = ColumnName.unsafe("duration")

  def from(table: BidsTable): Either[BidsError, EventsTable] =
    for
      onset <- table.columnNamed(OnsetColumn.value)
      duration <- table.columnNamed(DurationColumn.value)
      onsetSeconds <- onset.numeric
      durationSeconds <- duration.numeric
    yield new EventsTable(table, onset, duration, onsetSeconds, durationSeconds)

final case class BidsEventTableFile(context: BidsTableContext, events: EventsTable):
  def path: BidsPath = context.path
  def subject: Option[String] = context.subject
  def session: Option[String] = context.session
  def task: Option[String] = context.task
  def run: Option[String] = context.run
  def table: BidsTable = events.table

object BidsEventTableFile:
  def from(file: BidsFile, events: EventsTable): BidsEventTableFile =
    BidsEventTableFile(BidsTableContext.fromFile(file), events)

final case class BidsTableContext(
    path: BidsPath,
    entities: BidsEntities,
    scope: BidsScope,
    pipeline: Option[PipelineName],
    datatype: Option[String],
    kind: Option[String]
):
  def subject: Option[String] = entities.get(EntityKey.Subject)
  def session: Option[String] = entities.get(EntityKey.Session)
  def task: Option[String] = entities.get(EntityKey.Task)
  def run: Option[String] = entities.get(EntityKey.Run)

object BidsTableContext:
  def fromFile(file: BidsFile): BidsTableContext =
    BidsTableContext(
      path = file.path,
      entities = file.entities,
      scope = file.scope,
      pipeline = file.pipeline,
      datatype = file.datatype,
      kind = file.parsed.map(_.kind)
    )

final case class BidsTableFile(context: BidsTableContext, table: BidsTable):
  def path: BidsPath = context.path
  def subject: Option[String] = context.subject
  def session: Option[String] = context.session
  def task: Option[String] = context.task
  def run: Option[String] = context.run

object BidsTableFile:
  def from(file: BidsFile, table: BidsTable): BidsTableFile =
    BidsTableFile(BidsTableContext.fromFile(file), table)
