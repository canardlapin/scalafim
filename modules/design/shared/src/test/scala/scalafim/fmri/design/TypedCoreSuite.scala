package scalafim.fmri.design

import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.*
import scalafim.fmri.design.formula.*
import scalafim.fmri.design.hrf.{HrfFun, HrfSelection}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class TypedCoreSuite extends munit.FunSuite:

  private val table: DataTable =
    DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 3.0)),
      "cond" -> Column.Strings(Vector("A", "B")),
      "dur" -> Column.Doubles(Vector(0.5, 0.5)),
      "rt" -> Column.Doubles(Vector(1.2, 1.4))
    )

  private def buildError(
      formula: String,
      data: DataTable = table,
      hrfFuns: Map[String, HrfFun] = Map.empty,
      contrastSets: Map[String, ContrastSpec.ContrastSet] = Map.empty
  ): DesignError =
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))
    EventModelBuilder
      .buildEither(
        formula = formula,
        data = data,
        samplingFrame = sf,
        blockIds = Vector.fill(data.nrows)(0),
        hrfFuns = hrfFuns,
        contrastSets = contrastSets
      )
      .left
      .getOrElse(fail(s"expected build error for formula: $formula"))

  test("EventSchedule validates typed event records") {
    val schedule = EventSchedule
      .fromParts(Vector(1.0.s, 3.0.s), durations = Vector(0.5.s, 0.5.s), blockIds = Vector(0, 0))
      .fold(err => fail(err.message), identity)

    assertEquals(schedule.size, 2)
    assertEquals(schedule.onsets.map(_.value), Vector(1.0, 3.0))
    assertEquals(schedule.durations.map(_.value), Vector(0.5, 0.5))
    assert(EventSchedule.fromParts(Vector(1.0.s), durations = Vector((-1.0).s)).isLeft)
  }

  test("events expose typed ids without changing legacy tokens") {
    val ev = Event.factor(Vector("A", "B"), "condition")
    assertEquals(ev.factorId.isDefined, true)
    assertEquals(ev.conditionIds.length, 2)
    assertEquals(ev.conditionTokens, Vector("condition.A", "condition.B"))
  }

  test("BoundFormula binds typed column, basis, and contrast refs") {
    val parsed = FormulaParser.parse("""onset ~ hrf(cond, basis="spmg3", durations=dur, id=task, contrasts=myset)""")
    val bound = BoundFormula
      .bind(parsed, table, availableContrastSets = Set("myset"), requireKnownContrasts = true)
      .fold(err => fail(err.message), identity)

    assertEquals(bound.onset.name, "onset")
    val term = bound.terms.head.asInstanceOf[BoundHrfTerm]
    assertEquals(term.columns.map(_.name), Vector("cond"))
    assertEquals(term.basis.map(_.kind), Some(HrfKind.Spmg3))
    assertEquals(term.duration.map(_.name), Some("dur"))
    assertEquals(term.contrast, Some(ContrastRef("myset")))

    val missingContrast = BoundFormula.bind(parsed, table, availableContrastSets = Set.empty, requireKnownContrasts = true)
    assert(missingContrast.isLeft)
  }

  test("DataTable typed accessors report missing and mistyped columns as DesignError") {
    def id(name: String): ColumnId = ColumnId(name).fold(err => fail(err.message), identity)

    assertEquals(table.get[Double](id("onset")), Right(Vector(1.0, 3.0)))
    assertEquals(table.get[String](id("cond")), Right(Vector("A", "B")))

    assertEquals(table.column(id("missing")), Left(DesignError.MissingColumn("missing")))
    assertEquals(
      table.get[Double](id("cond")),
      Left(DesignError.InvalidColumnType("cond", "numeric", "string"))
    )
  }

  test("FormulaParser parseEither and EventDesignRequest build a model without exceptions") {
    val parsed = FormulaParser.parseEither("""onset ~ hrf(cond, durations=dur, id=task)""")
    assert(parsed.isRight)

    val bad = FormulaParser.parseEither("onset ~ hrf(")
    assert(bad.isLeft)

    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = """onset ~ hrf(cond, durations=dur, id=task)""",
      data = table,
      samplingFrame = sf,
      blockPlan = EventModelBuilder.BlockPlan.SingleBlock
    ).fold(err => fail(err.message), identity)

    val model = EventModelBuilder.buildEither(request).fold(err => fail(err.message), identity)
    assertEquals(model.termKeys, Vector("task"))
    assertEquals(model.designMatrix.rows, 10)
  }

  test("EventModelBuilder buildEither returns DesignError on bad formula inputs") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))
    val bad = EventModelBuilder.buildEither(
      formula = "onset ~ hrf(missing)",
      data = table,
      samplingFrame = sf,
      blockIds = Seq(0, 0)
    )
    assert(bad.isLeft)
  }

  test("EventModelBuilder buildEither exposes staged typed compiler errors") {
    val badDurTable = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 3.0)),
      "cond" -> Column.Strings(Vector("A", "B")),
      "bad_dur" -> Column.Doubles(Vector(0.5, -0.25)),
      "rt" -> Column.Doubles(Vector(1.2, 1.4))
    )

    assertEquals(
      buildError("onset ~ hrf(cond, durations = bad_dur)", data = badDurTable),
      DesignError.InvalidSchedule("durations must be non-negative")
    )

    buildError("""onset ~ hrf(cond, subset = rt < "fast")""") match
      case DesignError.InvalidSubset(detail) =>
        assert(detail.contains("type mismatch"), detail)
      case other =>
        fail(s"expected InvalidSubset, got $other")

    val badHrfFun: HrfFun =
      _ => HrfSelection.perEvent(Vector(Hrfs.SPMG1, Hrfs.SPMG2))
    buildError("onset ~ hrf(cond, hrf_fun = bad)", hrfFuns = Map("bad" -> badHrfFun)) match
      case DesignError.InvalidHrfFun("cond", detail) =>
        assert(detail.contains("same nbasis"), detail)
      case other =>
        fail(s"expected InvalidHrfFun, got $other")

    assertEquals(
      buildError("onset ~ hrf(cond, basis = nope)"),
      DesignError.UnknownBasis("nope")
    )

    assertEquals(
      buildError("onset ~ hrf(cond, contrasts = missing)"),
      DesignError.UnknownContrast("missing", Vector.empty)
    )
  }
