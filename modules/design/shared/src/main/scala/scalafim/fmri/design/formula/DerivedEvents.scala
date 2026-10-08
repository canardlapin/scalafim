package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, PortableNumber, RunIndex}
import scalafim.fmri.design.data.{Column, DataTable}

/** Declared types make even an entirely missing derived column inspectable. */
enum EventValueType:
  case Number, Text, Logical

/** What happens to an event row where a declared derived column has no value.
  *
  *  - `Reject` fails the build and names the rows.
  *  - `Drop` removes the row before compilation.
  *  - `PreserveNumeric` keeps missing numeric values (as `NaN`, for the
  *    term's modulator policy) and fails like `Reject` on a missing text or
  *    logical value, which never acquires a synthetic categorical level.
  */
enum DerivedMissingRows:
  case Reject, Drop, PreserveNumeric

  /** Stable portable name, used by JSON documents and audit receipts. */
  def label: String = this match
    case Reject => "reject"
    case Drop => "drop"
    case PreserveNumeric => "preserve-numeric"

object DerivedMissingRows:
  def fromLabel(label: String): Option[DerivedMissingRows] = values.find(_.label == label)

enum DerivedEventError:
  case InvalidDefinition(detail: String)
  case Expression(column: ColumnId, cause: EventExpressionError)
  case WrongType(column: ColumnId, row: Int, expected: EventValueType, actual: EventExpressionValue)
  /** The expression's static type differs from the declared type. */
  case DeclaredType(column: ColumnId, declared: EventValueType, inferred: EventValueType)
  case MissingValues(columns: Vector[ColumnId], rows: Vector[Int])
  case InvalidBins(detail: String)
  /** The missing-row policy removed every event row. */
  case NoRetainedRows(policy: DerivedMissingRows, dropped: Int)

  def message: String = this match
    case InvalidDefinition(detail) => detail
    case Expression(column, cause) => s"${column.value}: ${cause.message}"
    case WrongType(column, row, expected, actual) => s"${column.value} at row $row: expected $expected, got $actual"
    case DeclaredType(column, declared, inferred) => s"${column.value}: declared $declared, but the expression has type $inferred"
    case MissingValues(columns, rows) => s"missing derived values in ${columns.map(_.value).mkString(", ")} at rows ${rows.mkString(", ")}"
    case InvalidBins(detail) => detail
    case NoRetainedRows(policy, dropped) => s"missing-row policy '${policy.label}' dropped all $dropped event rows; no events remain to model"

/** One typed derived-column declaration.
  *
  * Instances are always printable: [[DerivedColumn.from]] rejects expressions the
  * admitted grammar cannot represent losslessly (for example a non-finite
  * numeric literal), so [[text]] is total and `DerivedColumn.parse(c.text)`
  * restores `c`.
  */
final case class DerivedColumn private (id: ColumnId, valueType: EventValueType, expression: ArgValue):
  def text: String = DerivedColumn.render(id, valueType, expression) match
    case Right(value) => value
    // Unreachable: every instance passed `render` in `from`.
    case Left(error) => throw IllegalStateException(error.message)

object DerivedColumn:
  /** Validate that the declaration prints losslessly in the admitted grammar. */
  def from(id: ColumnId, valueType: EventValueType, expression: ArgValue): Either[DerivedEventError, DerivedColumn] =
    render(id, valueType, expression).map(_ => new DerivedColumn(id, valueType, expression))

  private def render(id: ColumnId, valueType: EventValueType, expression: ArgValue): Either[DerivedEventError, String] =
    def invalid(error: FormulaParser.ParseError) = DerivedEventError.InvalidDefinition(error.message)
    for
      name <- FormulaPrinter.expressionTextEither(ArgValue.Ident(id)).left.map(invalid)
      value <- FormulaPrinter.expressionTextEither(expression).left.map(invalid)
    yield
      val kind = valueType match
        case EventValueType.Number => "number"
        case EventValueType.Text => "text"
        case EventValueType.Logical => "logical"
      s"$name: $kind = $value"

  /** One explicit declaration, for example `reward: number = gain - loss`. */
  def parse(text: String): Either[DerivedEventError, DerivedColumn] =
    // The shared formula lexer validates identifiers, including Unicode names.
    // Avoid Unicode regex families, which need a newer JavaScript target.
    val declaration = "(?s)^\\s*(`(?:\\\\.|[^`])*`|[^\\s:]+)\\s*:\\s*(number|text|logical)\\s*=\\s*(.+)$".r
    text match
      case declaration(name, kind, expression) =>
        for
          identifier <- FormulaParser.parseExpression(name).left.map(error => DerivedEventError.InvalidDefinition(error.getMessage))
          id <- identifier match
            case ArgValue.Ident(value) => Right(value)
            case _ => Left(DerivedEventError.InvalidDefinition("declaration name must be an identifier"))
          value <- FormulaParser.parseExpression(expression).left.map(error => DerivedEventError.InvalidDefinition(error.getMessage))
          column <- from(id, kind match
            case "number" => EventValueType.Number
            case "text" => EventValueType.Text
            case _ => EventValueType.Logical
          , value)
        yield column
      case _ => Left(DerivedEventError.InvalidDefinition("expected name: number|text|logical = expression"))

/** Rows are always zero-based indices in the caller's input table.
  * `droppedColumns(i)` names the declared columns missing at `droppedRows(i)`.
  */
final case class MaterializedEvents(
    table: DataTable,
    retainedRows: Vector[Int],
    droppedRows: Vector[Int],
    droppedColumns: Vector[Vector[ColumnId]]
):
  require(droppedColumns.length == droppedRows.length, "dropped-row columns must parallel dropped rows")

final case class DerivedEventTable private[formula] (
    source: DataTable,
    definitions: Vector[DerivedColumn],
    values: Map[ColumnId, Vector[EventExpressionValue]]
):
  /** Materialize only the columns used by one term. Missing text/logical values
    * never acquire a synthetic categorical level. Numeric missing values may
    * explicitly be preserved for the term's modulator policy.
    */
  def materialize(required: Vector[ColumnId], missing: DerivedMissingRows): Either[DerivedEventError, MaterializedEvents] =
    val selected = required.distinct
    selected.find(id => !values.contains(id)) match
      case Some(id) => Left(DerivedEventError.InvalidDefinition(s"unknown derived column '${id.value}'"))
      case None =>
        val types = definitions.map(value => value.id -> value.valueType).toMap
        val incomplete = selected.filter(id => missing != DerivedMissingRows.PreserveNumeric || types(id) != EventValueType.Number)
        val dropped = sourceRows.filter(row => incomplete.exists(id => values(id)(row) == EventExpressionValue.Missing))
        if dropped.nonEmpty && missing != DerivedMissingRows.Drop then Left(DerivedEventError.MissingValues(incomplete, dropped))
        else
          val retained = sourceRows.filterNot(dropped.toSet)
          val base = source.filterRows(sourceRows.map(retained.toSet))
          val columns = selected.map: id =>
            val cells = retained.map(values(id))
            val column = types(id) match
              case EventValueType.Number => Column.Doubles(cells.map:
                case EventExpressionValue.Num(value) => value
                case _ => Double.NaN
              )
              case EventValueType.Text => Column.Strings(cells.collect { case EventExpressionValue.Text(value) => value })
              case EventValueType.Logical => Column.Bools(cells.collect { case EventExpressionValue.Logical(value) => value })
            id.value -> column
          val reasons = dropped.map(row => incomplete.filter(id => values(id)(row) == EventExpressionValue.Missing))
          Right(MaterializedEvents(DataTable(retained.size, base.columns ++ columns), retained, dropped, reasons))

  private def sourceRows: Vector[Int] = Vector.range(0, source.nrows)

/** Derived-column declarations a build evaluates against its event table
  * before compiling, with the one missing-row policy applied to every declared
  * column. Declarations are evaluated in order; ids are distinct.
  */
final case class DerivedEventPlan private (columns: Vector[DerivedColumn], missingRows: DerivedMissingRows):
  def isEmpty: Boolean = columns.isEmpty
  def ids: Vector[ColumnId] = columns.map(_.id)

  /** Evaluate every declaration and materialize all declared columns. This is
    * exactly what a build does with the plan, exposed so a caller can inspect
    * or reproduce the event table the compiler sees.
    */
  def materialize(data: DataTable): Either[DerivedEventError, MaterializedEvents] =
    DerivedEvents.evaluate(data, columns).flatMap(_.materialize(ids, missingRows))

object DerivedEventPlan:
  val empty: DerivedEventPlan = new DerivedEventPlan(Vector.empty, DerivedMissingRows.Reject)

  def from(columns: Vector[DerivedColumn], missingRows: DerivedMissingRows): Either[DerivedEventError, DerivedEventPlan] =
    val ids = columns.map(_.id)
    ids.diff(ids.distinct).headOption match
      case Some(id) => Left(DerivedEventError.InvalidDefinition(s"column '${id.value}' is declared more than once"))
      case None => Right(new DerivedEventPlan(columns, missingRows))

  /** Parse declarations such as `reward: number = gain - loss`. */
  def parse(declarations: Vector[String], missingRows: DerivedMissingRows): Either[DerivedEventError, DerivedEventPlan] =
    declarations.foldLeft[Either[DerivedEventError, Vector[DerivedColumn]]](Right(Vector.empty)): (acc, text) =>
      for previous <- acc; column <- DerivedColumn.parse(text) yield previous :+ column
    .flatMap(from(_, missingRows))

/** One event row a build removed because a declared derived column had no
  * value there. `sourceRow` is the zero-based row of the caller's event table.
  */
final case class DerivedRowDrop(sourceRow: Int, run: RunIndex, missingColumns: Vector[ColumnId]):
  require(sourceRow >= 0, "dropped derived row must be non-negative")
  require(missingColumns.nonEmpty, "a dropped derived row names its missing columns")

  def canonical: String = s"$sourceRow@run=${run.oneBased}@missing=${missingColumns.map(_.value).mkString("|")}"

/** Build evidence for derived declarations. `retainedRows(i)` is the caller's
  * event-table row behind compiled event row `i`, so every compiled row index
  * (event provenance, missing-value receipts) maps back to the caller's table.
  */
final case class DerivedRowsReceipt(
    policy: DerivedMissingRows,
    columns: Vector[ColumnId],
    retainedRows: Vector[Int],
    dropped: Vector[DerivedRowDrop]
):
  require(columns.nonEmpty, "derived-row receipt requires declared columns")
  require(retainedRows.toSet.intersect(dropped.map(_.sourceRow).toSet).isEmpty, "a row is either retained or dropped")

  def canonical: String =
    s"policy=${policy.label}:columns=${columns.map(_.value).mkString(",")}:retained=${retainedRows.mkString(",")}:dropped=${dropped.map(_.canonical).mkString(",")}"

object DerivedEvents:
  /** Declarations are evaluated in order. Forward references and overwrites fail. */
  def evaluate(data: DataTable, definitions: Vector[DerivedColumn]): Either[DerivedEventError, DerivedEventTable] =
    definitions.foldLeft[Either[DerivedEventError, DerivedEventTable]](Right(DerivedEventTable(data, Vector.empty, Map.empty))): (acc, definition) =>
      acc.flatMap: previous =>
        if data.contains(definition.id) || previous.values.contains(definition.id) then
          Left(DerivedEventError.InvalidDefinition(s"column '${definition.id.value}' already exists"))
        else
          val types = previous.definitions.map(value => value.id -> value.valueType).toMap
          val typed = previous.values.map((id, values) => id -> DerivedValues(types(id), values))
          val checked = EventExpressions.typeOf(definition.expression, data, types).left.map(DerivedEventError.Expression(definition.id, _)).flatMap:
            case Some(inferred) if inferred != definition.valueType => Left(DerivedEventError.DeclaredType(definition.id, definition.valueType, inferred))
            case _ => Right(())
          checked.flatMap(_ => EventExpressions.evaluateDerived(definition.expression, data, typed).left.map(DerivedEventError.Expression(definition.id, _))).flatMap: values =>
            values.zipWithIndex.collectFirst { case (value, row) if !conforms(value, definition.valueType) =>
              DerivedEventError.WrongType(definition.id, row, definition.valueType, value)
            } match
              case Some(error) => Left(error)
              case None => Right(previous.copy(definitions = previous.definitions :+ definition, values = previous.values.updated(definition.id, values)))

  private def conforms(value: EventExpressionValue, expected: EventValueType): Boolean = (value, expected) match
    case (EventExpressionValue.Missing, _) => true
    case (EventExpressionValue.Num(_), EventValueType.Number) => true
    case (EventExpressionValue.Text(_), EventValueType.Text) => true
    case (EventExpressionValue.Logical(_), EventValueType.Logical) => true
    case _ => false

/** Explicit left-closed, right-open cuts. Quantile construction uses observed
  * finite values from exactly the caller's selected population (e.g. a term's
  * filtered events pooled over runs), with no implicit run or subset choice.
  */
final case class EventBins private (breaks: Vector[Double], labels: Vector[String]):
  def assign(values: Vector[Double]): Vector[Option[String]] = values.map: value =>
    if !value.isFinite then None
    else
      val index = breaks.sliding(2).indexWhere(pair => value >= pair.head && value < pair.last)
      if index < 0 then None else Some(labels(index))

  def breaksText: String = breaks.map {
    case value if value == Double.NegativeInfinity => "-Inf"
    case value if value == Double.PositiveInfinity => "Inf"
    case value => PortableNumber.format(value)
  }.mkString("c(", ", ", ")")

object EventBins:
  def explicit(breaks: Vector[Double], labels: Vector[String]): Either[DerivedEventError, EventBins] =
    if breaks.size < 2 || breaks.exists(_.isNaN) || breaks.sliding(2).exists(pair => pair.head >= pair.last) then
      Left(DerivedEventError.InvalidBins("breaks must contain at least two strictly increasing non-NaN values"))
    else if labels.size != breaks.size - 1 || labels.distinct.size != labels.size || labels.exists(_.isEmpty) then
      Left(DerivedEventError.InvalidBins("provide one distinct nonempty label per interval"))
    else Right(EventBins(breaks, labels))

  /** R type-7 quantiles; repeated cut points fail rather than silently merge bins. */
  def quantiles(values: Vector[Double], count: Int): Either[DerivedEventError, EventBins] =
    val observed = values.filter(_.isFinite).sorted
    if count < 1 || count > observed.size then Left(DerivedEventError.InvalidBins("bin count must be positive and no larger than the observed population"))
    else
      val interior = Vector.tabulate(count - 1): index =>
        val h = (observed.size - 1).toDouble * (index + 1).toDouble / count.toDouble
        val low = math.floor(h).toInt
        val fraction = h - low
        if fraction == 0.0 then observed(low)
        else observed(low) * (1.0 - fraction) + observed(low + 1) * fraction
      explicit(Vector(Double.NegativeInfinity) ++ interior ++ Vector(Double.PositiveInfinity), Vector.tabulate(count)(i => s"bin${i + 1}"))
