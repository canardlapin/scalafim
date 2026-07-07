package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.*
import scalafim.fmri.design.formula.*
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
