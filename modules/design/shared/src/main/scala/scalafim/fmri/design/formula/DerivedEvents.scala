package scalafim.fmri.design.formula

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.data.{Column, DataTable}

/** Declared types make even an entirely missing derived column inspectable. */
enum EventValueType:
  case Number, Text, Logical

enum DerivedMissingRows:
  case Reject, Drop, PreserveNumeric

enum DerivedEventError:
  case InvalidDefinition(detail: String)
  case Expression(column: ColumnId, cause: EventExpressionError)
  case WrongType(column: ColumnId, row: Int, expected: EventValueType, actual: EventExpressionValue)
  case MissingValues(columns: Vector[ColumnId], rows: Vector[Int])
  case InvalidBins(detail: String)

  def message: String = this match
    case InvalidDefinition(detail) => detail
    case Expression(column, cause) => s"${column.value}: ${cause.message}"
    case WrongType(column, row, expected, actual) => s"${column.value} at row $row: expected $expected, got $actual"
    case MissingValues(columns, rows) => s"missing derived values in ${columns.map(_.value).mkString(", ")} at rows ${rows.mkString(", ")}"
    case InvalidBins(detail) => detail

final case class DerivedColumn(id: ColumnId, valueType: EventValueType, expression: ArgValue):
  def text: String =
    val name = FormulaPrinter.expressionText(ArgValue.Ident(id))
    val kind = valueType match
      case EventValueType.Number => "number"
      case EventValueType.Text => "text"
      case EventValueType.Logical => "logical"
    s"$name: $kind = ${FormulaPrinter.expressionText(expression)}"

object DerivedColumn:
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
        yield DerivedColumn(id, kind match
          case "number" => EventValueType.Number
          case "text" => EventValueType.Text
          case _ => EventValueType.Logical
        , value)
      case _ => Left(DerivedEventError.InvalidDefinition("expected name: number|text|logical = expression"))

/** Rows are always zero-based indices in the caller's input table. */
final case class MaterializedEvents(table: DataTable, retainedRows: Vector[Int], droppedRows: Vector[Int])

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
          Right(MaterializedEvents(DataTable(retained.size, base.columns ++ columns), retained, dropped))

  private def sourceRows: Vector[Int] = Vector.range(0, source.nrows)

object DerivedEvents:
  /** Declarations are evaluated in order. Forward references and overwrites fail. */
  def evaluate(data: DataTable, definitions: Vector[DerivedColumn]): Either[DerivedEventError, DerivedEventTable] =
    definitions.foldLeft[Either[DerivedEventError, DerivedEventTable]](Right(DerivedEventTable(data, Vector.empty, Map.empty))): (acc, definition) =>
      acc.flatMap: previous =>
        if data.contains(definition.id) || previous.values.contains(definition.id) then
          Left(DerivedEventError.InvalidDefinition(s"column '${definition.id.value}' already exists"))
        else
          EventExpressions.evaluateDerived(definition.expression, data, previous.values).left.map(DerivedEventError.Expression(definition.id, _)).flatMap: values =>
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
    case value => value.toString
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
