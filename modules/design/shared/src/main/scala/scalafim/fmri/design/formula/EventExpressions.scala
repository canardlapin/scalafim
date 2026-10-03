package scalafim.fmri.design.formula

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.data.{Column, DataTable}

enum EventExpressionValue:
  case Num(value: Double)
  case Text(value: String)
  case Logical(value: Boolean)
  case Missing

/** Earlier derived values together with their declared type.
  *
  * The declared type, not the observed values, is what static checking sees, so
  * an entirely missing derived column still has a type.
  */
final case class DerivedValues(valueType: EventValueType, values: Vector[EventExpressionValue])

enum EventExpressionError:
  case InvalidDerivedColumn(column: ColumnId, expectedRows: Int, actualRows: Int)
  case InvalidDerivedValue(column: ColumnId, row: Int, detail: String)
  case UnknownColumn(path: Vector[String], column: ColumnId)
  case UnsupportedColumn(path: Vector[String], column: ColumnId, actual: String)
  case UnknownFunction(path: Vector[String], function: String)
  case InvalidArity(path: Vector[String], function: String, expected: String, actual: Int)
  case NamedArgument(path: Vector[String], name: String)
  case TypeMismatch(path: Vector[String], operation: String, expected: String, actual: String)
  case InvalidLevel(path: Vector[String], column: ColumnId, literal: String, levels: Vector[String])
  /** A numeric literal in a hand-built expression is NaN or infinite. */
  case NonFiniteLiteral(path: Vector[String], value: Double)
  /** A source column holds an infinite value. NaN in a numeric column is the
    * table's encoding of an observed missing value; infinity is not.
    */
  case NonFiniteInput(path: Vector[String], column: ColumnId, row: Int)
  /** Arithmetic produced NaN or an infinity (for example `x / 0`, `log(0)`,
    * `log(-1)`) from finite, non-missing operands.
    */
  case NonFiniteResult(path: Vector[String], operation: String, row: Int)

  def message: String = this match
    case InvalidDerivedColumn(column, expectedRows, actualRows) => s"derived column ${column.value} has $actualRows rows, expected $expectedRows"
    case InvalidDerivedValue(column, row, detail) => s"derived column ${column.value} at row $row: $detail"
    case UnknownColumn(path, column) => s"${path.mkString(".")}: unknown column '${column.value}'"
    case UnsupportedColumn(path, column, actual) => s"${path.mkString(".")}: column '${column.value}' is $actual, not a scalar expression value"
    case UnknownFunction(path, function) => s"${path.mkString(".")}: unknown expression function '$function'"
    case InvalidArity(path, function, expected, actual) => s"${path.mkString(".")}: '$function' expects $expected argument(s), got $actual"
    case NamedArgument(path, name) => s"${path.mkString(".")}: expression argument '$name' must be positional"
    case TypeMismatch(path, operation, expected, actual) => s"${path.mkString(".")}: type mismatch: '$operation' needs $expected, got $actual"
    case InvalidLevel(path, column, literal, levels) => s"${path.mkString(".")}: '$literal' is not a level of '${column.value}' (levels: ${levels.mkString(", ")})"
    case NonFiniteLiteral(path, value) => s"${path.mkString(".")}: numeric literal $value is not finite"
    case NonFiniteInput(path, column, row) => s"${path.mkString(".")}: column '${column.value}' is infinite at row $row"
    case NonFiniteResult(path, operation, row) => s"${path.mkString(".")}: '$operation' has no finite result at row $row"

/** Portable evaluator for event-table derived values and subset predicates.
  *
  * Every expression is type-checked statically, from column types and declared
  * derived types, before any row is evaluated: whether an expression is
  * well-typed never depends on which rows happen to be missing.
  *
  * Missing values (NaN in a numeric column, or `missing()`) propagate through
  * arithmetic and comparisons; logical operators and `in` use three-valued
  * Kleene semantics. Arithmetic that leaves the finite range from finite
  * operands (division by zero, `log` of a non-positive value) is an
  * evaluation error naming the expression path and row, never a missing
  * value, so it cannot be silently filtered away.
  *
  * Such an error is raised only for a row whose value can reach the output.
  * `ifelse` evaluates each branch only on the rows that select it (neither
  * branch on a row whose condition is missing); `&` skips its right operand
  * on rows where the left is FALSE, and `|` where the left is TRUE. So guards
  * such as `ifelse(x > 0, log(x), missing())`, `ifelse(d == 0, 0, n / d)`, and
  * `rt > 0 & log(rt) > 1` succeed. A missing left operand of `&` or `|` does
  * not decide the row (`missing & FALSE` is FALSE), so the right operand's
  * errors on such rows still surface. A `cut_quantiles` source is evaluated on
  * every row, because each row's value shapes the breaks.
  *
  * `round(x)` rounds half to even (IEC 60559), as R's `round(x)` does; a
  * `digits` argument is not supported.
  */
object EventExpressions:
  import EventExpressionValue.*

  def evaluate(expression: ArgValue, data: DataTable, checkLevels: Boolean = true): Either[EventExpressionError, Vector[EventExpressionValue]] =
    evaluateDerived(expression, data, Map.empty, checkLevels)

  /** A subset retains only explicit TRUE values. FALSE and Missing are excluded.
    * The expression must be statically logical; a numeric or text expression is
    * rejected even when every row is missing.
    */
  def filter(expression: ArgValue, data: DataTable, checkLevels: Boolean = true): Either[EventExpressionError, Vector[Boolean]] =
    typeOf(expression, data).flatMap {
      case None | Some(EventValueType.Logical) =>
        evaluate(expression, data, checkLevels).map(_.map(_ == Logical(true)))
      case other =>
        Left(EventExpressionError.TypeMismatch(Vector.empty, "filter", "logical values", typeName(other)))
    }

  /** Static result type of `expression`; `None` is the untyped missing literal
    * `missing()` (or an expression built only from it).
    */
  def typeOf(
      expression: ArgValue,
      data: DataTable,
      derivedTypes: Map[ColumnId, EventValueType] = Map.empty
  ): Either[EventExpressionError, Option[EventValueType]] =
    infer(expression, Schema(data, derivedTypes), Vector.empty)

  /** Evaluate against earlier typed derived columns without encoding missing text as a level. */
  def evaluateDerived(
      expression: ArgValue,
      data: DataTable,
      derived: Map[ColumnId, DerivedValues],
      checkLevels: Boolean = true
  ): Either[EventExpressionError, Vector[EventExpressionValue]] =
    for
      _ <- validateDerived(derived, data.nrows)
      _ <- infer(expression, Schema(data, derived.view.mapValues(_.valueType).toMap), Vector.empty)
      values <- eval(expression, EvaluationData(data, derived.view.mapValues(_.values).toMap), Vector.empty, checkLevels, allRows(data.nrows))
    yield values

  private def validateDerived(derived: Map[ColumnId, DerivedValues], nrows: Int): Either[EventExpressionError, Unit] =
    derived.toVector.sortBy(_._1.value).foldLeft[Either[EventExpressionError, Unit]](Right(())) { case (acc, (id, column)) =>
      acc.flatMap { _ =>
        if column.values.length != nrows then Left(EventExpressionError.InvalidDerivedColumn(id, nrows, column.values.length))
        else
          column.values.zipWithIndex.collectFirst {
            case (Num(value), row) if !value.isFinite => EventExpressionError.InvalidDerivedValue(id, row, "non-finite number")
            case (value, row) if value != Missing && !conforms(value, column.valueType) =>
              EventExpressionError.InvalidDerivedValue(id, row, s"expected ${typeName(Some(column.valueType))}, got ${kind(value)}")
          }.toLeft(())
      }
    }

  private def conforms(value: EventExpressionValue, expected: EventValueType): Boolean = (value, expected) match
    case (Num(_), EventValueType.Number) | (Text(_), EventValueType.Text) | (Logical(_), EventValueType.Logical) => true
    case _ => false

  // ---------------------------------------------------------------- static types

  private type Static = Option[EventValueType]

  private final case class Schema(table: DataTable, derived: Map[ColumnId, EventValueType])

  private def typeName(value: Static): String = value match
    case Some(EventValueType.Number) => "numeric"
    case Some(EventValueType.Text) => "text"
    case Some(EventValueType.Logical) => "logical"
    case None => "missing"

  private def infer(expression: ArgValue, schema: Schema, path: Vector[String]): Either[EventExpressionError, Static] = expression match
    case ArgValue.Num(value) =>
      if value.isFinite then Right(Some(EventValueType.Number)) else Left(EventExpressionError.NonFiniteLiteral(path, value))
    case ArgValue.Str(_) => Right(Some(EventValueType.Text))
    case ArgValue.Bool(_) => Right(Some(EventValueType.Logical))
    case ArgValue.Ident(id) => columnType(id, schema, path)
    case ArgValue.Call(function, args) =>
      named(args, path).flatMap { _ =>
        val values = args.map(_.value)
        def arity(expected: Int): Either[EventExpressionError, Unit] =
          if values.size == expected then Right(()) else Left(EventExpressionError.InvalidArity(path, function, expected.toString, values.size))
        def child(index: Int): Either[EventExpressionError, Static] = infer(values(index), schema, path :+ index.toString)
        def all: Either[EventExpressionError, Vector[Static]] = traverse(values.indices.toVector)(child)
        def expect(operation: String, expected: EventValueType, description: String)(actual: Static): Either[EventExpressionError, Unit] =
          if actual.forall(_ == expected) then Right(())
          else Left(EventExpressionError.TypeMismatch(path, operation, description, typeName(actual)))
        val number: Static = Some(EventValueType.Number)
        val logical: Static = Some(EventValueType.Logical)
        function match
          case "cut" | "cut_quantiles" =>
            if (function == "cut" && values.size != 3) || (function == "cut_quantiles" && values.size != 2) then
              Left(EventExpressionError.InvalidArity(path, function, if function == "cut" then "3: values, c(breaks), c(labels)" else "2: values, bin count", values.size))
            else child(0).flatMap { source =>
              if source.forall(_ == EventValueType.Number) then Right(Some(EventValueType.Text))
              else Left(EventExpressionError.TypeMismatch(path, function, "numeric cuts with valid explicit breaks or a positive quantile count", "nonnumeric source"))
            }
          case "missing" => arity(0).map(_ => None)
          case "-" if values.size == 1 =>
            child(0).flatMap(expect("-", EventValueType.Number, "a numeric value")).map(_ => number)
          case "+" | "-" | "*" | "/" =>
            arity(2).flatMap(_ => all).flatMap(types => traverse(types)(expect(function, EventValueType.Number, "numeric values"))).map(_ => number)
          case "neg" | "u-" | "abs" | "log" | "round" =>
            val operation = if function == "neg" || function == "u-" then "-" else function
            arity(1).flatMap(_ => child(0)).flatMap(expect(operation, EventValueType.Number, "a numeric value")).map(_ => number)
          case "!" =>
            arity(1).flatMap(_ => child(0)).flatMap(expect("!", EventValueType.Logical, "a logical value")).map(_ => logical)
          case "&&" | "&" | "||" | "|" =>
            val operation = if function.startsWith("&") then "&&" else "||"
            arity(2).flatMap(_ => all).flatMap(types => traverse(types)(expect(operation, EventValueType.Logical, "logical values"))).map(_ => logical)
          case "==" | "!=" | "<" | "<=" | ">" | ">=" =>
            arity(2).flatMap(_ => all).flatMap { types =>
              (types(0), types(1)) match
                case (None, _) | (_, None) => Right(logical)
                case (Some(EventValueType.Number), Some(EventValueType.Number)) => Right(logical)
                case (Some(left), Some(right)) if left == right =>
                  if function == "==" || function == "!=" then Right(logical)
                  else Left(EventExpressionError.TypeMismatch(path, function, "numeric values or ==/!=", typeName(Some(left))))
                case (left, right) => Left(EventExpressionError.TypeMismatch(path, function, typeName(left), typeName(right)))
            }
          case "ifelse" =>
            arity(3).flatMap(_ => all).flatMap { types =>
              if types(0).exists(_ != EventValueType.Logical) then
                Left(EventExpressionError.TypeMismatch(path, "ifelse", "a logical condition", typeName(types(0))))
              else unify(types(1), types(2)).toRight(
                EventExpressionError.TypeMismatch(path, "ifelse", "branches of one type", s"${typeName(types(1))} and ${typeName(types(2))}"))
            }
          case "is.na" | "isna" => arity(1).flatMap(_ => child(0)).map(_ => logical)
          case "min" | "max" =>
            if values.isEmpty then Left(EventExpressionError.InvalidArity(path, function, "at least 1", 0))
            else all.flatMap(types => traverse(types)(expect(function, EventValueType.Number, "numeric values"))).map(_ => number)
          case "in" =>
            if values.size < 2 then Left(EventExpressionError.InvalidArity(path, "in", "at least 2", values.size))
            else all.flatMap { types =>
              types.tail.foldLeft[Either[EventExpressionError, Static]](Right(types.head)) { (acc, candidate) =>
                acc.flatMap(common => unify(common, candidate).toRight(
                  EventExpressionError.TypeMismatch(path, "in", typeName(common), typeName(candidate))))
              }.map(_ => logical)
            }
          case other => Left(EventExpressionError.UnknownFunction(path, other))
      }

  /** Two static types are compatible when equal or when one is the untyped missing literal. */
  private def unify(left: Static, right: Static): Option[Static] = (left, right) match
    case (None, other) => Some(other)
    case (other, None) => Some(other)
    case (Some(a), Some(b)) if a == b => Some(left)
    case _ => None

  private def columnType(id: ColumnId, schema: Schema, path: Vector[String]): Either[EventExpressionError, Static] =
    schema.derived.get(id) match
      case Some(valueType) => Right(Some(valueType))
      case None => schema.table.column(id).left.map(_ => EventExpressionError.UnknownColumn(path, id)).flatMap {
        case Column.Doubles(_) | Column.Ints(_) => Right(Some(EventValueType.Number))
        case Column.Strings(_) => Right(Some(EventValueType.Text))
        case Column.Bools(_) => Right(Some(EventValueType.Logical))
        case other => Left(EventExpressionError.UnsupportedColumn(path, id, other.typeName))
      }

  // ------------------------------------------------------------------ evaluation

  private final case class EvaluationData(table: DataTable, derived: Map[ColumnId, Vector[EventExpressionValue]]):
    def nrows: Int = table.nrows

  /** Rows whose value can reach the output. Evaluation reports an error only on
    * an active row; the value an inactive row holds is unspecified (callers never
    * read it). A child's mask is always a subset of its parent's.
    */
  private type Active = Array[Boolean]

  private def allRows(nrows: Int): Active = Array.fill(nrows)(true)

  /** The rows of `active` that also satisfy `keep`. */
  private def refine(active: Active)(keep: Int => Boolean): Active =
    val refined = new Array[Boolean](active.length)
    var row = 0
    while row < active.length do
      refined(row) = active(row) && keep(row)
      row += 1
    refined

  /** Runs only after [[infer]] accepted the expression; the type errors kept
    * here are defensive and unreachable for statically checked input.
    */
  private def eval(expression: ArgValue, data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] = expression match
    case ArgValue.Num(value) =>
      if value.isFinite then Right(Vector.fill(data.nrows)(Num(value))) else Left(EventExpressionError.NonFiniteLiteral(path, value))
    case ArgValue.Str(value) => Right(Vector.fill(data.nrows)(Text(value)))
    case ArgValue.Bool(value) => Right(Vector.fill(data.nrows)(Logical(value)))
    case ArgValue.Ident(id) => column(id, data, path, active)
    case ArgValue.Call(function, args) =>
      named(args, path).flatMap { _ =>
        val values = args.map(_.value)
        def unaryOp(f: RowOp1) = unary(function, values, data, path, checkLevels, active)(f)
        def binaryOp(f: RowOp2) = binary(function, values, data, path, checkLevels, active)(f)
        function match
          case "cut" | "cut_quantiles" => cut(function, values, data, path, checkLevels, active)
          case "missing" if values.isEmpty => Right(Vector.fill(data.nrows)(Missing))
          case "-" if values.size == 1 => unaryOp(numericUnary("-", value => -value, path))
          case "+" | "-" | "*" | "/" => binaryOp(numeric(function, _, _, path, _))
          case "neg" | "u-" => unaryOp(numericUnary("-", value => -value, path))
          case "!" => unaryOp((value, _) => not(value, path))
          case "&&" | "&" => kleene(function, values, data, path, checkLevels, active, decisive = false)((left, right) => and(left, right, path))
          case "||" | "|" => kleene(function, values, data, path, checkLevels, active, decisive = true)((left, right) => or(left, right, path))
          case "==" | "!=" | "<" | "<=" | ">" | ">=" => binaryOp((left, right, _) => comparison(function, left, right, path))
          case "ifelse" => ifElse(values, data, path, checkLevels, active)
          case "is.na" | "isna" => unaryOp((value, _) => Right(Logical(value == Missing)))
          case "abs" => unaryOp(numericUnary("abs", value => math.abs(value), path))
          case "log" => unaryOp(numericUnary("log", value => math.log(value), path))
          case "round" => unaryOp(numericUnary("round", roundHalfEven, path))
          case "min" | "max" => extrema(function, values, data, path, checkLevels, active)
          case "in" => membership(values, data, path, checkLevels, active)
          case other => Left(EventExpressionError.UnknownFunction(path, other))
      }

  /** IEC 60559 round-half-to-even, matching R's `round(x)` (digits = 0). */
  private[formula] def roundHalfEven(value: Double): Double = math.rint(value)

  private def named(args: Vector[Arg], path: Vector[String]): Either[EventExpressionError, Unit] =
    args.collectFirst { case Arg(Some(name), _) => name } match
      case Some(name) => Left(EventExpressionError.NamedArgument(path, name))
      case None => Right(())

  private def column(id: ColumnId, data: EvaluationData, path: Vector[String], active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] =
    data.derived.get(id) match
      case Some(values) => Right(values)
      case None => data.table.column(id).left.map(_ => EventExpressionError.UnknownColumn(path, id)).flatMap {
        case Column.Doubles(values) =>
          // Only an infinity on an active row can reach the output.
          mapRows(values.length, active) { row =>
            val value = values(row)
            if value.isInfinite then Left(EventExpressionError.NonFiniteInput(path, id, row))
            else Right(if value.isNaN then Missing else Num(value))
          }
        case Column.Ints(values) => Right(values.map(value => Num(value.toDouble)))
        case Column.Strings(values) => Right(values.map(Text.apply))
        case Column.Bools(values) => Right(values.map(Logical.apply))
        case other => Left(EventExpressionError.UnsupportedColumn(path, id, other.typeName))
      }

  private type RowOp1 = (EventExpressionValue, Int) => Either[EventExpressionError, EventExpressionValue]
  private type RowOp2 = (EventExpressionValue, EventExpressionValue, Int) => Either[EventExpressionError, EventExpressionValue]

  /** Apply `f` to each active row, in row order, stopping at the first error.
    * Inactive rows are Missing and never reach `f`.
    */
  private def mapRows(nrows: Int, active: Active)(f: Int => Either[EventExpressionError, EventExpressionValue]): Either[EventExpressionError, Vector[EventExpressionValue]] =
    val builder = Vector.newBuilder[EventExpressionValue]
    builder.sizeHint(nrows)
    var failure: Option[EventExpressionError] = None
    var row = 0
    while failure.isEmpty && row < nrows do
      if !active(row) then builder += Missing
      else f(row) match
        case Right(value) => builder += value
        case Left(error) => failure = Some(error)
      row += 1
    failure.toLeft(builder.result())

  private def unary(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active)(f: RowOp1): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 1 then Left(EventExpressionError.InvalidArity(path, function, "1", args.size))
    else eval(args.head, data, path :+ "0", checkLevels, active).flatMap(values => mapRows(data.nrows, active)(row => f(values(row), row)))

  private def binary(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active)(f: RowOp2): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 2 then Left(EventExpressionError.InvalidArity(path, function, "2", args.size))
    else for
      left <- eval(args(0), data, path :+ "0", checkLevels, active)
      _ <- validateLevels(function, args(0), args(1), data, path, checkLevels)
      right <- eval(args(1), data, path :+ "1", checkLevels, active)
      result <- mapRows(data.nrows, active)(row => f(left(row), right(row), row))
    yield result

  /** Kleene `&` (`decisive = false`) and `|` (`decisive = true`). A row whose
    * left operand equals `decisive` is decided, so the right operand is not
    * evaluated there. A missing left operand still needs the right one
    * (`missing & FALSE` is FALSE), so the right operand's errors on such rows
    * surface.
    */
  private def kleene(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active, decisive: Boolean)(
      f: (EventExpressionValue, EventExpressionValue) => Either[EventExpressionError, EventExpressionValue]
  ): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 2 then Left(EventExpressionError.InvalidArity(path, function, "2", args.size))
    else eval(args(0), data, path :+ "0", checkLevels, active).flatMap { left =>
      val decided = Logical(decisive)
      val undecided = refine(active)(row => left(row) != decided)
      eval(args(1), data, path :+ "1", checkLevels, undecided).flatMap { right =>
        mapRows(data.nrows, active)(row => f(left(row), if undecided(row) then right(row) else Missing))
      }
    }

  /** Each branch is evaluated only on the rows that select it; a row whose
    * condition is missing is missing and evaluates neither branch.
    */
  private def ifElse(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 3 then Left(EventExpressionError.InvalidArity(path, "ifelse", "3", args.size))
    else for
      condition <- eval(args(0), data, path :+ "0", checkLevels, active)
      yes <- eval(args(1), data, path :+ "1", checkLevels, refine(active)(row => condition(row) == Logical(true)))
      no <- eval(args(2), data, path :+ "2", checkLevels, refine(active)(row => condition(row) == Logical(false)))
      result <- mapRows(data.nrows, active) { row =>
        condition(row) match
          case Logical(true) => Right(yes(row))
          case Logical(false) => Right(no(row))
          case Missing => Right(Missing)
          case other => Left(EventExpressionError.TypeMismatch(path, "ifelse", "a logical condition", kind(other)))
      }
    yield result

  private def evaluateAll(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[Vector[EventExpressionValue]]] =
    args.zipWithIndex.foldLeft[Either[EventExpressionError, Vector[Vector[EventExpressionValue]]]](Right(Vector.empty)) { case (acc, (arg, index)) =>
      for previous <- acc; next <- eval(arg, data, path :+ index.toString, checkLevels, active) yield previous :+ next
    }

  private def extrema(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.isEmpty then Left(EventExpressionError.InvalidArity(path, function, "at least 1", 0))
    else
      evaluateAll(args, data, path, checkLevels, active).flatMap { columns =>
        mapRows(data.nrows, active) { row =>
          val values = columns.map(_(row))
          if values.contains(Missing) then Right(Missing)
          else
            val numbers = values.collect { case Num(value) => value }
            if numbers.size != values.size then Left(EventExpressionError.TypeMismatch(path, function, "numeric values", values.find(value => !value.isInstanceOf[Num]).map(kind).getOrElse("unknown")))
            else Right(Num(if function == "min" then numbers.min else numbers.max))
        }
      }

  /** Kleene membership: TRUE when the value equals some non-missing candidate;
    * otherwise Missing when the value or any candidate is missing; otherwise FALSE.
    */
  private def membership(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size < 2 then Left(EventExpressionError.InvalidArity(path, "in", "at least 2", args.size))
    else
      for
        _ <- validateInLevels(args, data, path, checkLevels)
        columns <- evaluateAll(args, data, path, checkLevels, active)
        result <- mapRows(data.nrows, active) { index =>
          val value = columns.head(index)
          val candidates = columns.tail.map(_(index))
          candidates.find(candidate => candidate != Missing && value != Missing && !sameType(value, candidate)) match
            case Some(candidate) => Left(EventExpressionError.TypeMismatch(path, "in", kind(value), kind(candidate)))
            case None =>
              if value != Missing && candidates.contains(value) then Right(Logical(true))
              else if value == Missing || candidates.contains(Missing) then Right(Missing)
              else Right(Logical(false))
        }
      yield result

  private def cut(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean, active: Active): Either[EventExpressionError, Vector[EventExpressionValue]] =
    def invalid(detail: String): EventExpressionError =
      EventExpressionError.TypeMismatch(path, function, "numeric cuts with valid explicit breaks or a positive quantile count", detail)
    def literal(value: ArgValue): Option[Double] = value match
      case ArgValue.Num(number) => Some(number)
      case ArgValue.Ident(id) if id.value == "Inf" => Some(Double.PositiveInfinity)
      case ArgValue.Call("-", Vector(Arg(None, ArgValue.Ident(id)))) if id.value == "Inf" => Some(Double.NegativeInfinity)
      case _ => None
    if (function == "cut" && args.size != 3) || (function == "cut_quantiles" && args.size != 2) then
      Left(EventExpressionError.InvalidArity(path, function, if function == "cut" then "3: values, c(breaks), c(labels)" else "2: values, bin count", args.size))
    else
      // Quantile breaks depend on every row's source value, so a guard cannot
      // shrink that population: every row of a `cut_quantiles` source reaches
      // the output. Explicit cuts are row-wise and follow the active rows.
      val sourceRows = if function == "cut_quantiles" then allRows(data.nrows) else active
      eval(args.head, data, path :+ "0", checkLevels, sourceRows).flatMap: values =>
        if values.indices.exists(row => sourceRows(row) && values(row) != Missing && !values(row).isInstanceOf[Num]) then Left(invalid("nonnumeric source"))
        else
          val numbers = values.indices.toVector.map: row =>
            values(row) match
              case Num(value) if sourceRows(row) => value
              case _ => Double.NaN
          val bins = if function == "cut_quantiles" then
            args(1) match
              case ArgValue.Num(count) if count.isWhole && count > 0 && count <= Int.MaxValue => EventBins.quantiles(numbers, count.toInt).left.map(error => invalid(error.message))
              case _ => Left(invalid("bin count must be a positive integer literal"))
          else (args(1), args(2)) match
            case (ArgValue.Call("c", breaks), ArgValue.Call("c", labels)) if breaks.forall(_.name.isEmpty) && labels.forall(_.name.isEmpty) =>
              val edges = breaks.flatMap(arg => literal(arg.value))
              val names = labels.collect { case Arg(None, ArgValue.Str(value)) => value }
              if edges.size != breaks.size || names.size != labels.size then Left(invalid("breaks must be numeric literals and labels text literals"))
              else EventBins.explicit(edges, names).left.map(error => invalid(error.message))
            case _ => Left(invalid("expected c(breaks) and c(labels)"))
          bins.map(_.assign(numbers).map(_.fold[EventExpressionValue](Missing)(Text.apply)))

  private def finiteResult(value: Double, operation: String, path: Vector[String], row: Int): Either[EventExpressionError, EventExpressionValue] =
    if value.isFinite then Right(Num(value)) else Left(EventExpressionError.NonFiniteResult(path, operation, row))

  private def numeric(operation: String, left: EventExpressionValue, right: EventExpressionValue, path: Vector[String], row: Int): Either[EventExpressionError, EventExpressionValue] =
    (left, right) match
      case (Missing, _) | (_, Missing) => Right(Missing)
      case (Num(a), Num(b)) =>
        val value = operation match
          case "+" => a + b
          case "-" => a - b
          case "*" => a * b
          case "/" => a / b
          case _ => Double.NaN
        finiteResult(value, operation, path, row)
      case (other, _) if !other.isInstanceOf[Num] => Left(EventExpressionError.TypeMismatch(path, operation, "numeric values", kind(other)))
      case (_, other) => Left(EventExpressionError.TypeMismatch(path, operation, "numeric values", kind(other)))

  private def numericUnary(operation: String, f: Double => Double, path: Vector[String])(value: EventExpressionValue, row: Int): Either[EventExpressionError, EventExpressionValue] = value match
    case Missing => Right(Missing)
    case Num(number) => finiteResult(f(number), operation, path, row)
    case other => Left(EventExpressionError.TypeMismatch(path, operation, "a numeric value", kind(other)))

  private def not(value: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] = value match
    case Missing => Right(Missing)
    case Logical(boolean) => Right(Logical(!boolean))
    case other => Left(EventExpressionError.TypeMismatch(path, "!", "a logical value", kind(other)))

  private def and(left: EventExpressionValue, right: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] =
    if !logicalOrMissing(left) then Left(EventExpressionError.TypeMismatch(path, "&&", "logical values", kind(left)))
    else if !logicalOrMissing(right) then Left(EventExpressionError.TypeMismatch(path, "&&", "logical values", kind(right)))
    else (left, right) match
      case (Logical(false), _) | (_, Logical(false)) => Right(Logical(false))
      case (Logical(true), Logical(true)) => Right(Logical(true))
      case _ => Right(Missing)

  private def or(left: EventExpressionValue, right: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] =
    if !logicalOrMissing(left) then Left(EventExpressionError.TypeMismatch(path, "||", "logical values", kind(left)))
    else if !logicalOrMissing(right) then Left(EventExpressionError.TypeMismatch(path, "||", "logical values", kind(right)))
    else (left, right) match
      case (Logical(true), _) | (_, Logical(true)) => Right(Logical(true))
      case (Logical(false), Logical(false)) => Right(Logical(false))
      case _ => Right(Missing)

  private def comparison(operation: String, left: EventExpressionValue, right: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] =
    (left, right) match
      case (Missing, _) | (_, Missing) => Right(Missing)
      case (Num(a), Num(b)) => Right(Logical(compare(operation, a, b)))
      case (Text(a), Text(b)) if operation == "==" => Right(Logical(a == b))
      case (Text(a), Text(b)) if operation == "!=" => Right(Logical(a != b))
      case (Logical(a), Logical(b)) if operation == "==" => Right(Logical(a == b))
      case (Logical(a), Logical(b)) if operation == "!=" => Right(Logical(a != b))
      case (Text(_), Text(_)) | (Logical(_), Logical(_)) => Left(EventExpressionError.TypeMismatch(path, operation, "numeric values or ==/!=", kind(left)))
      case _ => Left(EventExpressionError.TypeMismatch(path, operation, kind(left), kind(right)))

  private def compare(operation: String, left: Double, right: Double): Boolean = operation match
    case "==" => left == right
    case "!=" => left != right
    case "<" => left < right
    case "<=" => left <= right
    case ">" => left > right
    case ">=" => left >= right
    case _ => false

  private def validateLevels(operation: String, left: ArgValue, right: ArgValue, data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Unit] =
    if !checkLevels || (operation != "==" && operation != "!=") then Right(())
    else validateLiteralLevel(left, right, data, path).flatMap(_ => validateLiteralLevel(right, left, data, path))

  private def validateInLevels(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Unit] =
    if !checkLevels then Right(())
    else args.tail.foldLeft[Either[EventExpressionError, Unit]](Right(()))((acc, literal) => acc.flatMap(_ => validateLiteralLevel(args.head, literal, data, path)))

  private def validateLiteralLevel(reference: ArgValue, literal: ArgValue, data: EvaluationData, path: Vector[String]): Either[EventExpressionError, Unit] = (reference, literal) match
    case (ArgValue.Ident(id), ArgValue.Str(value)) =>
      column(id, data, path, allRows(data.nrows)).flatMap { values =>
        val levels = values.collect { case Text(level) => level }.distinct.sorted
        if levels.nonEmpty && !levels.contains(value) then Left(EventExpressionError.InvalidLevel(path, id, value, levels))
        else Right(())
      }
    case _ => Right(())

  private def traverse[A, B](values: Vector[A])(f: A => Either[EventExpressionError, B]): Either[EventExpressionError, Vector[B]] =
    values.foldLeft[Either[EventExpressionError, Vector[B]]](Right(Vector.empty))((acc, value) => for previous <- acc; next <- f(value) yield previous :+ next)

  private def kind(value: EventExpressionValue): String = value match
    case Num(_) => "numeric"
    case Text(_) => "text"
    case Logical(_) => "logical"
    case Missing => "missing"

  private def sameType(left: EventExpressionValue, right: EventExpressionValue): Boolean = (left, right) match
    case (Num(_), Num(_)) | (Text(_), Text(_)) | (Logical(_), Logical(_)) => true
    case _ => false

  private def logicalOrMissing(value: EventExpressionValue): Boolean = value match
    case Logical(_) | Missing => true
    case _ => false
