package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm}
import scalafim.fmri.design.formula.*
import scalafim.fmri.hrf.design.SamplingFrame

class TrialwiseFormulaSuite extends munit.FunSuite:
  private val data = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(1.0, 4.0, 2.0, 6.0, 9.0)),
    "phase_onset" -> Column.Doubles(Vector(1.5, 4.5, 2.5, 6.5, 9.5)),
    "duration" -> Column.Doubles(Vector.fill(5)(0.5)),
    "keep" -> Column.Bools(Vector(false, true, true, true, false)),
    "trial_id" -> Column.Strings(Vector("1", "2", "1", "2", "3")),
    "parent" -> Column.Strings(Vector("1", "2", "1", "2", "3"))
  )

  private val frame = SamplingFrame(blockLens = Seq(15, 20), tr = Seq(1.0))
  private val formula =
    "onset ~ trialwise(onsets = phase_onset, subset = keep, durations = duration, phase = probe, parent = parent, id = trial_id, label = memory)"

  test("trialwise options parse and print losslessly") {
    val parsed = FormulaParser.parse(formula)
    val trialwise = parsed.terms.head.asInstanceOf[TrialwiseCall]
    assertEquals(trialwise.onsets, Some(ArgValue.Ident(ColumnId.unsafe("phase_onset"))))
    assertEquals(trialwise.subset, Some(ArgValue.Ident(ColumnId.unsafe("keep"))))
    assertEquals(trialwise.phase, Some(PhaseRef(PhaseId.unsafe("probe"), ColumnId.unsafe("parent"))))
    assertEquals(trialwise.id, Some(ArgValue.Ident(ColumnId.unsafe("trial_id"))))
    assertEquals(FormulaParser.parse(parsed.text), parsed)
  }

  test("trialwise uses selected timing and preserves run-qualified phase provenance") {
    val model = EventModelBuilder.buildEither(
      formula,
      data,
      frame,
      blockIds = Vector(0, 0, 1, 1, 1)
    ).fold(error => fail(error.message), identity)
    val convolved = model.terms.head._2 match
      case term: ConvolvedTerm => term
      case other => fail(s"expected a convolved trialwise term, got $other")

    assertEquals(convolved.term.onsets.map(_.value), Vector(4.5, 2.5, 6.5))
    assertEquals(convolved.term.blockIds0, Vector(0, 1, 1))
    assertEquals(convolved.term.phaseId, Some(PhaseId.unsafe("probe")))
    assertEquals(convolved.term.eventProvenance.map(_.parent.value), Vector("run1:2", "run2:1", "run2:2"))
    val trials = convolved.term.events.head.asInstanceOf[CategoricalEvent]
    assert(trials.levels.contains("run1_memory_2"))
    assert(trials.levels.contains("run2_memory_1"))
    assert(trials.levels.contains("run2_memory_2"))
  }

  test("trialwise identity is local to each run and rejects duplicates within a run") {
    val repeatedAcrossRuns = EventModelBuilder.buildEither(
      formula,
      data,
      frame,
      blockIds = Vector(0, 0, 1, 1, 1)
    )
    assert(repeatedAcrossRuns.isRight)

    val duplicate = DataTable(data.nrows, data.columns.updated("trial_id", Column.Strings(Vector("1", "1", "1", "2", "3"))))
    val error = EventModelBuilder.buildEither(
      formula,
      duplicate,
      frame,
      blockIds = Vector(0, 0, 1, 1, 1)
    ).left.toOption.getOrElse(fail("expected duplicate trial identity error"))
    assertEquals(error, DesignError.InvalidSchedule("id values must be unique within run 1"))
  }

  test("binding ignores modulator option names while still binding its source") {
    val parsed = FormulaParser.parse("onset ~ hrf(modulator(rt, center = global, scale = zscore, missing = zero))")
    val withSource = DataTable(data.nrows, data.columns.updated("rt", Column.Doubles(Vector.fill(5)(1.0))))
    assert(BoundFormula.bind(parsed, withSource).isRight)
    val missingSource = BoundFormula.bind(parsed, data)
    assertEquals(missingSource, Left(DesignError.MissingColumn("rt")))
  }

  test("expression printer uses the formula expression grammar") {
    val expression = ArgValue.Call(
      "&",
      Vector(
        Arg(None, ArgValue.Call("==", Vector(Arg(None, ArgValue.Ident(ColumnId.unsafe("condition"))), Arg(None, ArgValue.Str("go"))))),
        Arg(None, ArgValue.Call("missing", Vector.empty))
      )
    )
    assertEquals(FormulaParser.parseExpression(FormulaPrinter.expressionText(expression)), Right(expression))
  }
