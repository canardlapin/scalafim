package scalafim.fmri.design.formula

import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.data.{Column, DataTable}

class DerivedEventsSuite extends munit.FunSuite:
  private def id(value: String): ColumnId = ColumnId.unsafe(value)
  private val data = DataTable.fromColumns("gain" -> Column.Doubles(Vector(2.0, Double.NaN, 6.0)))

  test("declaration identifiers use the portable formula lexer"):
    Vector("récompense: number = gain", "`reward: value`: number = gain").foreach: source =>
      val parsed = DerivedColumn.parse(source).fold(error => fail(error.message), identity)
      assertEquals(DerivedColumn.parse(parsed.text), Right(parsed))
    assert(DerivedColumn.parse("x+y: number = gain").isLeft)

  test("typed declarations round trip and compose with missing text and logical values"):
    val definitions = Vector(
      "reward: number = gain * 2",
      "category: text = ifelse(reward > 5, \"high\", \"low\")",
      "chosen: logical = category == \"high\""
    ).map(DerivedColumn.parse(_).toOption.get)
    definitions.foreach(value => assertEquals(DerivedColumn.parse(value.text), Right(value)))
    val result = DerivedEvents.evaluate(data, definitions).toOption.get
    assertEquals(result.values(id("category")), Vector(EventExpressionValue.Text("low"), EventExpressionValue.Missing, EventExpressionValue.Text("high")))
    assert(result.materialize(Vector(id("category")), DerivedMissingRows.Reject).isLeft)
    val selected = result.materialize(Vector(id("category"), id("chosen")), DerivedMissingRows.Drop).toOption.get
    assertEquals(selected.retainedRows, Vector(0, 2))
    assertEquals(selected.droppedRows, Vector(1))
    assertEquals(selected.table.get[String](id("category")).toOption.get, Vector("low", "high"))
    val numeric = result.materialize(Vector(id("reward")), DerivedMissingRows.PreserveNumeric).toOption.get
    assertEquals(numeric.table.nrows, 3)
    assert(numeric.table.get[Double](id("reward")).toOption.get(1).isNaN)

  test("declarations reject overwrites, forward references, wrong types and invalid levels"):
    assert(DerivedEvents.evaluate(data, Vector(DerivedColumn.parse("gain: number = 1").toOption.get)).isLeft)
    assert(DerivedEvents.evaluate(data, Vector(DerivedColumn.parse("x: number = later + 1").toOption.get)).isLeft)
    assert(DerivedEvents.evaluate(data, Vector(DerivedColumn.parse("x: text = gain").toOption.get)).isLeft)
    val definitions = Vector("a: text = ifelse(gain > 3, \"high\", \"low\")", "b: logical = a == \"typo\"").map(DerivedColumn.parse(_).toOption.get)
    assert(DerivedEvents.evaluate(data, definitions).isLeft)
    val allMissing = DerivedColumn.parse("x: logical = missing()").toOption.get
    assertEquals(DerivedEvents.evaluate(data, Vector(allMissing)).toOption.get.values(id("x")), Vector.fill(3)(EventExpressionValue.Missing))

  test("type-7 quantiles use observed pooled values and explicit left-closed cuts"):
    val bins = EventBins.quantiles(Vector(0.0, 1.0, 2.0, 3.0, Double.NaN), 4).toOption.get
    assertEquals(bins.breaks, Vector(Double.NegativeInfinity, 0.75, 1.5, 2.25, Double.PositiveInfinity))
    assertEquals(bins.assign(Vector(Double.NaN, 0.74, 0.75, 1.5, 2.25)), Vector(None, Some("bin1"), Some("bin2"), Some("bin3"), Some("bin4")))
    assertEquals(bins.breaksText, "c(-Inf, 0.75, 1.5, 2.25, Inf)")
    assert(EventBins.quantiles(Vector.fill(4)(1.0), 4).isLeft)
    assert(EventBins.quantiles(Vector(Double.NaN), 1).isLeft)
    assert(EventBins.explicit(Vector(0.0, 1.0), Vector("a", "b")).isLeft)

  test("binned derived factors parse and keep missing values outside every bin"):
    val definition = DerivedColumn.parse("bin: text = cut(gain, c(-Inf, 3, Inf), c(\"small\", \"large\"))").toOption.get
    assertEquals(DerivedColumn.parse(definition.text), Right(definition))
    val result = DerivedEvents.evaluate(data, Vector(definition)).toOption.get
    assertEquals(result.values(id("bin")), Vector(EventExpressionValue.Text("small"), EventExpressionValue.Missing, EventExpressionValue.Text("large")))
    assertEquals(result.materialize(Vector(id("bin")), DerivedMissingRows.Drop).toOption.get.droppedRows, Vector(1))
    val quantile = DerivedColumn.parse("bin: text = cut_quantiles(gain, 2)").toOption.get
    assertEquals(DerivedEvents.evaluate(data, Vector(quantile)).toOption.get.values(id("bin")), Vector(EventExpressionValue.Text("bin1"), EventExpressionValue.Missing, EventExpressionValue.Text("bin2")))

  test("derived columns validate printability at construction, so text is total"):
    assert(DerivedColumn.from(id("x"), EventValueType.Number, ArgValue.Num(Double.NaN)).isLeft)
    assert(DerivedColumn.from(id("x"), EventValueType.Number, ArgValue.Call("+", Vector(Arg(None, ArgValue.Num(1)), Arg(None, ArgValue.Num(Double.PositiveInfinity))))).isLeft)
    val column = DerivedColumn.from(id("odd`name"), EventValueType.Number, ArgValue.Call("-", Vector(Arg(None, ArgValue.Num(3))))).fold(error => fail(error.message), identity)
    assertEquals(DerivedColumn.parse(column.text), Right(column))

  test("declared types are checked statically, even for entirely missing sources"):
    val missing = DataTable.fromColumns("gain" -> Column.Doubles(Vector(Double.NaN, Double.NaN)))
    assertEquals(DerivedEvents.evaluate(missing, Vector(DerivedColumn.parse("x: text = gain * 2").toOption.get)),
      Left(DerivedEventError.DeclaredType(id("x"), EventValueType.Text, EventValueType.Number)))
    val chained = Vector("a: number = gain", "b: logical = a == \"x\"").map(DerivedColumn.parse(_).toOption.get)
    assert(DerivedEvents.evaluate(missing, chained).left.exists {
      case DerivedEventError.Expression(column, EventExpressionError.TypeMismatch(_, "==", "numeric", "text")) => column == id("b")
      case _ => false
    })

  test("division by zero in a derived column is an error, not a missing value"):
    val definition = DerivedColumn.parse("ratio: number = gain / (gain - gain)").toOption.get
    assert(DerivedEvents.evaluate(data, Vector(definition)).left.exists {
      case DerivedEventError.Expression(column, EventExpressionError.NonFiniteResult(_, "/", 0)) => column == id("ratio")
      case _ => false
    })

  test("bin breaks print identically on every platform"):
    val bins = EventBins.explicit(Vector(Double.NegativeInfinity, -0.0 + 1e-7, 4.0, 1e21, Double.PositiveInfinity), Vector("a", "b", "c", "d")).toOption.get
    assertEquals(bins.breaksText, "c(-Inf, 1e-7, 4, 1e21, Inf)")

  test("guarded derived declarations evaluate only the selected branch"):
    val table = DataTable.fromColumns("gain" -> Column.Doubles(Vector(2.0, 0.0, Double.NaN)))
    val definitions = Vector(
      "logGain: number = ifelse(gain > 0, log(gain), missing())",
      "inverse: number = ifelse(gain == 0, 0, 1 / gain)",
      "large: logical = gain > 0 & logGain > 0.5"
    ).map(DerivedColumn.parse(_).fold(error => fail(error.message), identity))
    val result = DerivedEvents.evaluate(table, definitions).fold(error => fail(error.message), identity)
    assertEquals(result.values(id("logGain")), Vector(EventExpressionValue.Num(math.log(2)), EventExpressionValue.Missing, EventExpressionValue.Missing))
    assertEquals(result.values(id("inverse")), Vector(EventExpressionValue.Num(0.5), EventExpressionValue.Num(0), EventExpressionValue.Missing))
    assertEquals(result.values(id("large")), Vector(EventExpressionValue.Logical(true), EventExpressionValue.Logical(false), EventExpressionValue.Missing))
    val unguarded = DerivedColumn.parse("logGain: number = log(gain)").fold(error => fail(error.message), identity)
    assertEquals(DerivedEvents.evaluate(table, Vector(unguarded)),
      Left(DerivedEventError.Expression(id("logGain"), EventExpressionError.NonFiniteResult(Vector.empty, "log", 1))))
