package scalafim.bids

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
    val cleanColumns = columns.map(_.trim)
    if cleanColumns.exists(_.isEmpty) then Left(BidsError.InvalidTable("column names must be non-empty"))
    else if cleanColumns.distinct.length != cleanColumns.length then Left(BidsError.InvalidTable("column names must be unique"))
    else if rows.exists(_.length != cleanColumns.length) then Left(BidsError.InvalidTable("all rows must have the same width as the header"))
    else Right(BidsTable(cleanColumns, rows))

  def parse(text: String): Either[BidsError, BidsTable] =
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toVector
    if lines.isEmpty then Left(BidsError.InvalidTable("input is empty"))
    else
      val tabDelimited = lines.head.contains('\t')
      val split: String => Vector[String] =
        if tabDelimited then line => line.split("\t", -1).toVector.map(_.trim)
        else line => line.trim.split("\\s+").toVector.map(_.trim)
      val columns = split(lines.head)
      val rows = lines.tail.map { line =>
        split(line).map(cell => Option.when(!MissingTokens(cell))(cell))
      }
      fromRows(columns, rows)

object BidsEvents:
  def readTable(text: String): Either[BidsError, BidsTable] =
    BidsTable.parse(text)

  def readEventsTable(text: String): Either[BidsError, EventsTable] =
    BidsTable.parse(text).flatMap(EventsTable.from)

final case class EventsTable private (
    table: BidsTable,
    onset: BidsColumn,
    duration: BidsColumn
):
  def nrows: Int = table.nrows
  def trialType: Option[BidsColumn] =
    table.columnNamed("trial_type").toOption

  def onsetSeconds: Vector[Option[Double]] =
    onset.numeric.toOption.getOrElse(Vector.empty)

  def durationSeconds: Vector[Option[Double]] =
    duration.numeric.toOption.getOrElse(Vector.empty)

object EventsTable:
  val OnsetColumn: ColumnName = ColumnName.unsafe("onset")
  val DurationColumn: ColumnName = ColumnName.unsafe("duration")

  def from(table: BidsTable): Either[BidsError, EventsTable] =
    for
      onset <- table.columnNamed(OnsetColumn.value)
      duration <- table.columnNamed(DurationColumn.value)
      _ <- onset.numeric
      _ <- duration.numeric
    yield EventsTable(table, onset, duration)

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
