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
    assertEquals(EventExpressions.evaluate(ArgValue.Num(Double.NaN), data).toOption.get,
      Vector.fill(3)(Missing))
    assertEquals(EventExpressions.evaluate(call("==", id("amount"), ArgValue.Num(2)), data).toOption.get,
      Vector(Logical(true), Missing, Logical(false)))
    assertEquals(EventExpressions.evaluate(call("log", id("amount")), data).toOption.get,
      Vector(Num(math.log(2)), Missing, Missing))
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
