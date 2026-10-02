package scalafim.fmri.design.formula

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.data.{Column, DataTable}

enum EventExpressionValue:
  case Num(value: Double)
  case Text(value: String)
  case Logical(value: Boolean)
  case Missing

enum EventExpressionError:
  case InvalidDerivedColumn(column: ColumnId, expectedRows: Int, actualRows: Int)
  case UnknownColumn(path: Vector[String], column: ColumnId)
  case UnsupportedColumn(path: Vector[String], column: ColumnId, actual: String)
  case UnknownFunction(path: Vector[String], function: String)
  case InvalidArity(path: Vector[String], function: String, expected: String, actual: Int)
  case NamedArgument(path: Vector[String], name: String)
  case TypeMismatch(path: Vector[String], operation: String, expected: String, actual: String)
  case InvalidLevel(path: Vector[String], column: ColumnId, literal: String, levels: Vector[String])

  def message: String = this match
    case InvalidDerivedColumn(column, expectedRows, actualRows) => s"derived column ${column.value} has $actualRows rows, expected $expectedRows"
    case UnknownColumn(path, column) => s"${path.mkString(".")}: unknown column '${column.value}'"
    case UnsupportedColumn(path, column, actual) => s"${path.mkString(".")}: column '${column.value}' is $actual, not a scalar expression value"
    case UnknownFunction(path, function) => s"${path.mkString(".")}: unknown expression function '$function'"
    case InvalidArity(path, function, expected, actual) => s"${path.mkString(".")}: '$function' expects $expected argument(s), got $actual"
    case NamedArgument(path, name) => s"${path.mkString(".")}: expression argument '$name' must be positional"
    case TypeMismatch(path, operation, expected, actual) => s"${path.mkString(".")}: type mismatch: '$operation' needs $expected, got $actual"
    case InvalidLevel(path, column, literal, levels) => s"${path.mkString(".")}: '$literal' is not a level of '${column.value}' (levels: ${levels.mkString(", ")})"

/** Portable evaluator for event-table derived values and subset predicates.
  * Missing values propagate through arithmetic and comparisons; logical
  * operators use three-valued Kleene semantics.
  */
object EventExpressions:
  import EventExpressionValue.*

  def evaluate(expression: ArgValue, data: DataTable, checkLevels: Boolean = true): Either[EventExpressionError, Vector[EventExpressionValue]] =
    evaluateDerived(expression, data, Map.empty, checkLevels)

  /** A subset retains only explicit TRUE values. FALSE and Missing are excluded. */
  def filter(expression: ArgValue, data: DataTable, checkLevels: Boolean = true): Either[EventExpressionError, Vector[Boolean]] =
    evaluate(expression, data, checkLevels).flatMap { values =>
      val invalid = values.indexWhere {
        case Logical(_) | Missing => false
        case _ => true
      }
      if invalid >= 0 then Left(EventExpressionError.TypeMismatch(Vector.empty, "filter", "logical values", kind(values(invalid))))
      else Right(values.map(_ == Logical(true)))
    }

  /** Evaluate against earlier typed derived columns without encoding missing text as a level. */
  def evaluateDerived(
      expression: ArgValue,
      data: DataTable,
      derived: Map[ColumnId, Vector[EventExpressionValue]],
      checkLevels: Boolean = true
  ): Either[EventExpressionError, Vector[EventExpressionValue]] =
    derived.collectFirst { case (id, values) if values.length != data.nrows =>
      EventExpressionError.InvalidDerivedColumn(id, data.nrows, values.length)
    } match
      case Some(error) => Left(error)
      case None =>
        val finiteValues = derived.view.mapValues(_.map {
          case Num(value) if !value.isFinite => Missing
          case value => value
        }).toMap
        eval(expression, EvaluationData(data, finiteValues), Vector.empty, checkLevels)

  private final case class EvaluationData(table: DataTable, derived: Map[ColumnId, Vector[EventExpressionValue]]):
    def nrows: Int = table.nrows

  private def eval(expression: ArgValue, data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Vector[EventExpressionValue]] = expression match
    case ArgValue.Num(value) => Right(Vector.fill(data.nrows)(if value.isFinite then Num(value) else Missing))
    case ArgValue.Str(value) => Right(Vector.fill(data.nrows)(Text(value)))
    case ArgValue.Bool(value) => Right(Vector.fill(data.nrows)(Logical(value)))
    case ArgValue.Ident(id) => column(id, data, path)
    case ArgValue.Call(function, args) =>
      named(args, path).flatMap { _ =>
        val values = args.map(_.value)
        function match
          case "cut" | "cut_quantiles" => cut(function, values, data, path, checkLevels)
          case "missing" if values.isEmpty => Right(Vector.fill(data.nrows)(Missing))
          case "-" if values.size == 1 => unary(function, values, data, path, checkLevels)(numericUnary("-", value => -value, path))
          case "+" | "-" | "*" | "/" => binary(function, values, data, path, checkLevels)(numeric(function, _, _, path))
          case "neg" | "u-" => unary(function, values, data, path, checkLevels)(numericUnary("-", value => -value, path))
          case "!" => unary(function, values, data, path, checkLevels)(not(_, path))
          case "&&" | "&" => binary(function, values, data, path, checkLevels)(and(_, _, path))
          case "||" | "|" => binary(function, values, data, path, checkLevels)(or(_, _, path))
          case "==" | "!=" | "<" | "<=" | ">" | ">=" =>
            binary(function, values, data, path, checkLevels) { (left, right) => comparison(function, left, right, path) }
          case "ifelse" => ifElse(values, data, path, checkLevels)
          case "is.na" | "isna" => unary(function, values, data, path, checkLevels)(value => Right(Logical(value == Missing)))
          case "abs" => unary(function, values, data, path, checkLevels)(numericUnary("abs", value => math.abs(value), path))
          case "log" => unary(function, values, data, path, checkLevels)(numericUnary("log", value => math.log(value), path))
          case "round" => unary(function, values, data, path, checkLevels)(numericUnary("round", value => math.round(value).toDouble, path))
          case "min" | "max" => extrema(function, values, data, path, checkLevels)
          case "in" => membership(values, data, path, checkLevels)
          case other => Left(EventExpressionError.UnknownFunction(path, other))
      }

  private def named(args: Vector[Arg], path: Vector[String]): Either[EventExpressionError, Unit] =
    args.collectFirst { case Arg(Some(name), _) => name } match
      case Some(name) => Left(EventExpressionError.NamedArgument(path, name))
      case None => Right(())

  private def column(id: ColumnId, data: EvaluationData, path: Vector[String]): Either[EventExpressionError, Vector[EventExpressionValue]] =
    data.derived.get(id) match
      case Some(values) => Right(values)
      case None => data.table.column(id).left.map(_ => EventExpressionError.UnknownColumn(path, id)).flatMap {
        case Column.Doubles(values) => Right(values.map(value => if value.isFinite then Num(value) else Missing))
        case Column.Ints(values) => Right(values.map(value => Num(value.toDouble)))
        case Column.Strings(values) => Right(values.map(Text.apply))
        case Column.Bools(values) => Right(values.map(Logical.apply))
        case other => Left(EventExpressionError.UnsupportedColumn(path, id, other.typeName))
      }

  private def unary(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean)(f: EventExpressionValue => Either[EventExpressionError, EventExpressionValue]): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 1 then Left(EventExpressionError.InvalidArity(path, function, "1", args.size))
    else eval(args.head, data, path :+ "0", checkLevels).flatMap(values => traverse(values)(f))

  private def binary(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean)(f: (EventExpressionValue, EventExpressionValue) => Either[EventExpressionError, EventExpressionValue]): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 2 then Left(EventExpressionError.InvalidArity(path, function, "2", args.size))
    else for
      left <- eval(args(0), data, path :+ "0", checkLevels)
      _ <- validateLevels(function, args(0), args(1), data, path, checkLevels)
      right <- eval(args(1), data, path :+ "1", checkLevels)
      result <- traverse(left.zip(right)) { case (first, second) => f(first, second) }
    yield result

  private def ifElse(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size != 3 then Left(EventExpressionError.InvalidArity(path, "ifelse", "3", args.size))
    else for
      condition <- eval(args(0), data, path :+ "0", checkLevels)
      yes <- eval(args(1), data, path :+ "1", checkLevels)
      no <- eval(args(2), data, path :+ "2", checkLevels)
      result <- traverse(condition.zip(yes).zip(no)) { case ((test, whenTrue), whenFalse) =>
        test match
          case Logical(true) => Right(whenTrue)
          case Logical(false) => Right(whenFalse)
          case Missing => Right(Missing)
          case other => Left(EventExpressionError.TypeMismatch(path, "ifelse", "a logical condition", kind(other)))
      }
    yield result

  private def extrema(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.isEmpty then Left(EventExpressionError.InvalidArity(path, function, "at least 1", 0))
    else
      val evaluated = args.zipWithIndex.foldLeft[Either[EventExpressionError, Vector[Vector[EventExpressionValue]]]](Right(Vector.empty)) { case (acc, (arg, index)) =>
        for previous <- acc; next <- eval(arg, data, path :+ index.toString, checkLevels) yield previous :+ next
      }
      evaluated.flatMap { columns =>
        traverse(Vector.tabulate(data.nrows)(row => columns.map(_(row)))) { values =>
          if values.contains(Missing) then Right(Missing)
          else
            val numbers = values.collect { case Num(value) => value }
            if numbers.size != values.size then Left(EventExpressionError.TypeMismatch(path, function, "numeric values", values.find(value => !value.isInstanceOf[Num]).map(kind).getOrElse("unknown")))
            else Right(Num(if function == "min" then numbers.min else numbers.max))
        }
      }

  private def membership(args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Vector[EventExpressionValue]] =
    if args.size < 2 then Left(EventExpressionError.InvalidArity(path, "in", "at least 2", args.size))
    else
      val values = args.zipWithIndex.foldLeft[Either[EventExpressionError, Vector[Vector[EventExpressionValue]]]](Right(Vector.empty)) { case (acc, (arg, index)) =>
        for previous <- acc; next <- eval(arg, data, path :+ index.toString, checkLevels) yield previous :+ next
      }
      for
        _ <- validateInLevels(args, data, path, checkLevels)
        columns <- values
        result <- traverse(Vector.tabulate(data.nrows)(row => columns.map(_(row)))) { row =>
          val value = row.head
          if value == Missing then Right(Missing)
          else if row.tail.contains(Missing) then Right(Missing)
          else if row.tail.forall(candidate => sameType(value, candidate)) then Right(Logical(row.tail.contains(value)))
          else Left(EventExpressionError.TypeMismatch(path, "in", kind(value), row.tail.find(candidate => !sameType(value, candidate)).map(kind).getOrElse("unknown")))
        }
      yield result

  private def cut(function: String, args: Vector[ArgValue], data: EvaluationData, path: Vector[String], checkLevels: Boolean): Either[EventExpressionError, Vector[EventExpressionValue]] =
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
      eval(args.head, data, path :+ "0", checkLevels).flatMap: values =>
        if values.exists(value => value != Missing && !value.isInstanceOf[Num]) then Left(invalid("nonnumeric source"))
        else
          val numbers = values.map:
            case Num(value) => value
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

  private def numeric(operation: String, left: EventExpressionValue, right: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] =
    (left, right) match
      case (Missing, _) | (_, Missing) => Right(Missing)
      case (Num(a), Num(b)) =>
        val value = operation match
          case "+" => a + b
          case "-" => a - b
          case "*" => a * b
          case "/" => a / b
          case _ => Double.NaN
        Right(if value.isFinite then Num(value) else Missing)
      case (other, _) if !other.isInstanceOf[Num] => Left(EventExpressionError.TypeMismatch(path, operation, "numeric values", kind(other)))
      case (_, other) => Left(EventExpressionError.TypeMismatch(path, operation, "numeric values", kind(other)))

  private def numericUnary(operation: String, f: Double => Double, path: Vector[String])(value: EventExpressionValue): Either[EventExpressionError, EventExpressionValue] = value match
    case Missing => Right(Missing)
    case Num(number) =>
      val result = f(number)
      Right(if result.isFinite then Num(result) else Missing)
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
      case (Logical(true), Missing) | (Missing, Logical(true)) | (Missing, Missing) => Right(Missing)
      case _ => Left(EventExpressionError.TypeMismatch(path, "&&", "logical values", "invalid"))

  private def or(left: EventExpressionValue, right: EventExpressionValue, path: Vector[String]): Either[EventExpressionError, EventExpressionValue] =
    if !logicalOrMissing(left) then Left(EventExpressionError.TypeMismatch(path, "||", "logical values", kind(left)))
    else if !logicalOrMissing(right) then Left(EventExpressionError.TypeMismatch(path, "||", "logical values", kind(right)))
    else (left, right) match
      case (Logical(true), _) | (_, Logical(true)) => Right(Logical(true))
      case (Logical(false), Logical(false)) => Right(Logical(false))
      case (Logical(false), Missing) | (Missing, Logical(false)) | (Missing, Missing) => Right(Missing)
      case _ => Left(EventExpressionError.TypeMismatch(path, "||", "logical values", "invalid"))

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
      column(id, data, path).flatMap { values =>
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
