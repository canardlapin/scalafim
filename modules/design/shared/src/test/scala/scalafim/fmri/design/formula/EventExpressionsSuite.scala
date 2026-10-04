package scalafim.fmri.design.formula

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.data.{Column, DataTable}

class EventExpressionsSuite extends munit.FunSuite:
  import EventExpressionValue.*

  private val data = DataTable.fromColumns(
    "amount" -> Column.Doubles(Vector(2.0, Double.NaN, -1.0)),
    "outcome" -> Column.Strings(Vector("go", "stop", "go")),
    "keep" -> Column.Bools(Vector(true, false, true))
  )
  private def id(name: String): ArgValue = ArgValue.Ident(ColumnId.unsafe(name))
  private def call(name: String, values: ArgValue*): ArgValue = ArgValue.Call(name, values.toVector.map(Arg(None, _)))

  test("arithmetic and functions propagate missing numeric values") {
    assertEquals(EventExpressions.evaluate(call("+", id("amount"), ArgValue.Num(3)), data).toOption.get,
      Vector(Num(5), Missing, Num(2)))
    assertEquals(EventExpressions.evaluate(call("==", id("amount"), ArgValue.Num(2)), data).toOption.get,
      Vector(Logical(true), Missing, Logical(false)))
    assertEquals(EventExpressions.evaluate(call("log", call("abs", id("amount"))), data).toOption.get,
      Vector(Num(math.log(2)), Missing, Num(0.0)))
    assertEquals(EventExpressions.evaluate(call("ifelse", call(">", id("amount"), ArgValue.Num(0)), ArgValue.Num(1), ArgValue.Num(0)), data).toOption.get,
      Vector(Num(1), Missing, Num(0)))
    assertEquals(EventExpressions.evaluate(call("is.na", id("amount")), data).toOption.get,
      Vector(Logical(false), Logical(true), Logical(false)))
    assertEquals(EventExpressions.evaluate(call("max", id("amount"), ArgValue.Num(1)), data).toOption.get,
      Vector(Num(2), Missing, Num(1)))
  }

  test("Kleene logic and filters retain only explicit true") {
    val missing = call(">", id("amount"), ArgValue.Num(0))
    assertEquals(EventExpressions.evaluate(call("&&", missing, ArgValue.Bool(false)), data).toOption.get,
      Vector(Logical(false), Logical(false), Logical(false)))
    assertEquals(EventExpressions.evaluate(call("|", missing, ArgValue.Bool(true)), data).toOption.get,
      Vector(Logical(true), Logical(true), Logical(true)))
    assertEquals(EventExpressions.evaluate(call("!", missing), data).toOption.get,
      Vector(Logical(false), Missing, Logical(true)))
    assertEquals(EventExpressions.filter(missing, data).toOption.get, Vector(true, false, false))
  }

  test("type and level errors identify their expression path") {
    EventExpressions.evaluate(call("+", id("outcome"), ArgValue.Num(1)), data) match
      case Left(EventExpressionError.TypeMismatch(path, "+", _, "text")) => assertEquals(path, Vector.empty)
      case other => fail(s"expected text arithmetic error, got $other")
    EventExpressions.evaluate(call("==", id("outcome"), ArgValue.Str("failed_stop")), data) match
      case Left(EventExpressionError.InvalidLevel(path, column, literal, levels)) =>
        assertEquals(path, Vector.empty)
        assertEquals(column.value, "outcome")
        assertEquals(literal, "failed_stop")
        assertEquals(levels, Vector("go", "stop"))
      case other => fail(s"expected level error, got $other")
    assertEquals(EventExpressions.evaluate(call("==", id("outcome"), ArgValue.Str("failed_stop")), data, checkLevels = false).toOption.get,
      Vector(Logical(false), Logical(false), Logical(false)))
    EventExpressions.evaluate(call("in", id("outcome"), ArgValue.Str("go"), ArgValue.Str("failed_stop")), data) match
      case Left(EventExpressionError.InvalidLevel(_, _, literal, _)) => assertEquals(literal, "failed_stop")
      case other => fail(s"expected in-level error, got $other")
  }

  test("comparison and membership require compatible scalar types") {
    assertEquals(EventExpressions.evaluate(call("in", id("outcome"), ArgValue.Str("go"), ArgValue.Str("stop")), data).toOption.get,
      Vector(Logical(true), Logical(true), Logical(true)))
    EventExpressions.evaluate(call("<", id("outcome"), ArgValue.Str("stop")), data) match
      case Left(EventExpressionError.TypeMismatch(_, "<", _, _)) => ()
      case other => fail(s"expected text ordering error, got $other")
    EventExpressions.filter(id("amount"), data) match
      case Left(EventExpressionError.TypeMismatch(_, "filter", _, "numeric")) => ()
      case other => fail(s"expected nonlogical filter error, got $other")
  }

  test("non-finite arithmetic is a typed error naming the row, never a missing value"):
    assertEquals(EventExpressions.evaluate(call("log", id("amount")), data),
      Left(EventExpressionError.NonFiniteResult(Vector.empty, "log", 2)))
    assertEquals(EventExpressions.evaluate(call("/", id("amount"), ArgValue.Num(0)), data),
      Left(EventExpressionError.NonFiniteResult(Vector.empty, "/", 0)))
    assertEquals(EventExpressions.evaluate(call("log", call("-", id("amount"), id("amount"))), data),
      Left(EventExpressionError.NonFiniteResult(Vector.empty, "log", 0)))
    // A subset over x / 0 fails loudly instead of silently excluding the rows.
    assertEquals(EventExpressions.filter(call(">", call("/", id("amount"), ArgValue.Num(0)), ArgValue.Num(1)), data),
      Left(EventExpressionError.NonFiniteResult(Vector("0"), "/", 0)))
    EventExpressions.evaluate(ArgValue.Num(Double.NaN), data) match
      case Left(EventExpressionError.NonFiniteLiteral(Vector(), value)) => assert(value.isNaN)
      case other => fail(s"expected a non-finite literal error, got $other")
    assert(EventExpressions.evaluate(ArgValue.Num(Double.PositiveInfinity), data).left.exists(_.isInstanceOf[EventExpressionError.NonFiniteLiteral]))
    val infinite = DataTable.fromColumns("x" -> Column.Doubles(Vector(1.0, Double.PositiveInfinity)))
    assertEquals(EventExpressions.evaluate(call("+", id("x"), ArgValue.Num(1)), infinite),
      Left(EventExpressionError.NonFiniteInput(Vector("0"), ColumnId.unsafe("x"), 1)))
    // Missing operands still propagate as missing (row 1 is NaN, an observed NA).
    assertEquals(EventExpressions.evaluate(call("/", id("amount"), ArgValue.Num(2)), data).toOption.get, Vector(Num(1), Missing, Num(-0.5)))

  test("round is half-to-even, matching R 4.5.1 round(x)"):
    // R: round(c(0.5, 1.5, 2.5, -0.5, -1.5, -2.5, 2.675 * 100, 0.15 * 10)) == c(0, 2, 2, 0, -2, -2, 268, 2)
    val inputs = Vector(0.5, 1.5, 2.5, -0.5, -1.5, -2.5, 2.675 * 100, 0.15 * 10, 2.4, -2.6)
    val expected = Vector(0.0, 2.0, 2.0, 0.0, -2.0, -2.0, 268.0, 2.0, 2.0, -3.0)
    val table = DataTable.fromColumns("x" -> Column.Doubles(inputs))
    val rounded = EventExpressions.evaluate(call("round", id("x")), table).toOption.get
    rounded.zip(expected).foreach:
      case (Num(actual), wanted) => assertEqualsDouble(actual, wanted, 0.0)
      case (other, _) => fail(s"expected a number, got $other")

  test("in uses Kleene semantics: a match wins over a missing candidate"):
    val table = DataTable.fromColumns("x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0)))
    assertEquals(EventExpressions.evaluate(call("in", id("x"), ArgValue.Num(1), call("missing")), table).toOption.get,
      Vector(Logical(true), Missing, Missing))
    assertEquals(EventExpressions.evaluate(call("in", id("x"), ArgValue.Num(1), ArgValue.Num(2)), table).toOption.get,
      Vector(Logical(true), Missing, Logical(false)))
    assertEquals(EventExpressions.evaluate(call("in", id("outcome"), ArgValue.Str("stop"), call("missing")), data).toOption.get,
      Vector(Missing, Logical(true), Missing))

  test("type checking is static: it never depends on which rows are missing"):
    val allMissing = DataTable.fromColumns(
      "rt" -> Column.Doubles(Vector(Double.NaN, Double.NaN)),
      "label" -> Column.Strings(Vector("a", "b"))
    )
    EventExpressions.filter(id("rt"), allMissing) match
      case Left(EventExpressionError.TypeMismatch(_, "filter", _, "numeric")) => ()
      case other => fail(s"expected a static filter type error, got $other")
    EventExpressions.evaluate(call(">", id("rt"), ArgValue.Str("x")), allMissing) match
      case Left(EventExpressionError.TypeMismatch(Vector(), ">", "numeric", "text")) => ()
      case other => fail(s"expected a static comparison type error, got $other")
    EventExpressions.evaluate(call("ifelse", call("is.na", id("rt")), ArgValue.Num(1), ArgValue.Str("x")), allMissing) match
      case Left(EventExpressionError.TypeMismatch(_, "ifelse", _, _)) => ()
      case other => fail(s"expected an ifelse branch type error, got $other")
    EventExpressions.evaluate(call("in", id("rt"), ArgValue.Num(1), ArgValue.Str("a")), allMissing) match
      case Left(EventExpressionError.TypeMismatch(_, "in", _, _)) => ()
      case other => fail(s"expected an in type error, got $other")
    assertEquals(EventExpressions.typeOf(call("+", id("rt"), ArgValue.Num(1)), allMissing), Right(Some(EventValueType.Number)))
    assertEquals(EventExpressions.typeOf(call("missing"), allMissing), Right(None))
    assertEquals(EventExpressions.filter(call("missing"), allMissing), Right(Vector(false, false)))
    // Declared derived types are what static checking sees.
    val derived = Map(ColumnId.unsafe("d") -> DerivedValues(EventValueType.Text, Vector(Missing, Missing)))
    EventExpressions.evaluateDerived(call("+", id("d"), ArgValue.Num(1)), allMissing, derived) match
      case Left(EventExpressionError.TypeMismatch(_, "+", _, "text")) => ()
      case other => fail(s"expected a derived type error, got $other")
    assertEquals(
      EventExpressions.evaluateDerived(id("d"), allMissing, Map(ColumnId.unsafe("d") -> DerivedValues(EventValueType.Number, Vector(Text("a"), Missing)))),
      Left(EventExpressionError.InvalidDerivedValue(ColumnId.unsafe("d"), 0, "expected numeric, got text")))

  private def expr(text: String): ArgValue = FormulaParser.parseExpression(text).fold(error => fail(error.getMessage), identity)

  private val guarded = DataTable.fromColumns(
    "x" -> Column.Doubles(Vector(2.0, 0.0, -1.0, Double.NaN)),
    "n" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0)),
    "d" -> Column.Doubles(Vector(2.0, 0.0, 4.0, Double.NaN)),
    "rt" -> Column.Doubles(Vector(10.0, 0.0, -2.0, 1.0)),
    "keep" -> Column.Bools(Vector(true, false, true, true))
  )

  test("ifelse evaluates each branch only on the rows that select it"):
    assertEquals(EventExpressions.evaluate(expr("ifelse(x > 0, log(x), missing())"), guarded),
      Right(Vector(Num(math.log(2)), Missing, Missing, Missing)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(d == 0, 0, n / d)"), guarded),
      Right(Vector(Num(0.5), Num(0), Num(0.75), Missing)))
    // Without the guard the same arithmetic fails on the first offending row.
    assertEquals(EventExpressions.evaluate(expr("log(x)"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector.empty, "log", 1)))
    assertEquals(EventExpressions.evaluate(expr("n / d"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector.empty, "/", 1)))
    // A wrong guard selects the offending rows, so the branch still fails there.
    assertEquals(EventExpressions.evaluate(expr("ifelse(x <= 0, log(x), missing())"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("1"), "log", 1)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(d != 0, 0, n / d)"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("2"), "/", 1)))

  test("a missing ifelse condition evaluates neither branch"):
    val table = DataTable.fromColumns(
      "m" -> Column.Doubles(Vector(1.0, Double.NaN)),
      "z" -> Column.Doubles(Vector(1.0, 0.0))
    )
    assertEquals(EventExpressions.evaluate(expr("ifelse(m > 0, log(z), log(z))"), table), Right(Vector(Num(0), Missing)))
    val observed = DataTable.fromColumns("m" -> Column.Doubles(Vector(1.0, -1.0)), "z" -> Column.Doubles(Vector(1.0, 0.0)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(m > 0, log(z), log(z))"), observed),
      Left(EventExpressionError.NonFiniteResult(Vector("2"), "log", 1)))

  test("& and | ignore errors on rows the left operand decides"):
    assertEquals(EventExpressions.filter(expr("rt > 0 & log(rt) > 1"), guarded), Right(Vector(true, false, false, false)))
    assertEquals(EventExpressions.evaluate(expr("rt <= 0 | log(rt) > 1"), guarded),
      Right(Vector(Logical(true), Logical(true), Logical(true), Logical(false))))
    assertEquals(EventExpressions.filter(expr("log(rt) > 1"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("0"), "log", 1)))
    // The guard must come first: the left operand is always evaluated.
    assertEquals(EventExpressions.filter(expr("log(rt) > 1 & rt > 0"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("0", "0"), "log", 1)))
    // A TRUE left operand does not decide &, nor FALSE decide |.
    assertEquals(EventExpressions.filter(expr("rt <= 0 & log(rt) > 1"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("1", "0"), "log", 1)))

  test("a missing left operand of & still needs the right one, so its errors surface"):
    val table = DataTable.fromColumns(
      "m" -> Column.Doubles(Vector(1.0, Double.NaN)),
      "z" -> Column.Doubles(Vector(1.0, 0.0))
    )
    // Kleene: missing & FALSE is FALSE, so the right operand is evaluated on row 1.
    assertEquals(EventExpressions.filter(expr("m > 0 & log(z) > 0"), table),
      Left(EventExpressionError.NonFiniteResult(Vector("1", "0"), "log", 1)))
    assertEquals(EventExpressions.evaluate(expr("m > 0 & z > 0"), table), Right(Vector(Logical(true), Logical(false))))

  test("nested guards: errors are reported only for rows active at the failing node"):
    assertEquals(EventExpressions.evaluate(expr("d != 0 & ifelse(n > 1, n / d, 0) > 0.6"), guarded),
      Right(Vector(Logical(false), Logical(false), Logical(true), Missing)))
    assertEquals(EventExpressions.filter(expr("d != 0 & ifelse(n > 1, n / d, 0) > 0.6"), guarded),
      Right(Vector(false, false, true, false)))
    assertEquals(EventExpressions.filter(expr("ifelse(n > 1, n / d, 0) > 0.6"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("0", "1"), "/", 1)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(keep, ifelse(d == 0, missing(), n / d), n / d)"), guarded),
      Left(EventExpressionError.NonFiniteResult(Vector("2"), "/", 1)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(keep, n / d, ifelse(d == 0, 0, n / d))"), guarded),
      Right(Vector(Num(0.5), Num(0), Num(0.75), Missing)))

  test("an infinite input is an error only on rows that reach the output"):
    val table = DataTable.fromColumns(
      "x" -> Column.Doubles(Vector(1.0, Double.PositiveInfinity)),
      "keep" -> Column.Bools(Vector(true, false))
    )
    assertEquals(EventExpressions.filter(expr("keep & x > 0"), table), Right(Vector(true, false)))
    assertEquals(EventExpressions.evaluate(expr("ifelse(keep, x + 1, 0)"), table), Right(Vector(Num(2), Num(0))))
    assertEquals(EventExpressions.evaluate(expr("ifelse(keep, 0, x + 1)"), table),
      Left(EventExpressionError.NonFiniteInput(Vector("2", "0"), ColumnId.unsafe("x"), 1)))
