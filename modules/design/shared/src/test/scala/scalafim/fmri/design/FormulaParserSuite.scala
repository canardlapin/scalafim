package scalafim.fmri.design

import scalafim.fmri.design.formula.*

class FormulaParserSuite extends munit.FunSuite:

  private def col(name: String): ArgValue.Ident = ArgValue.Ident(ColumnId.unsafe(name))
  private def term(name: String): TermId = TermId.unsafe(name)

  private def parseError(formula: String): FormulaParser.ParseError =
    FormulaParser.parseEither(formula) match
      case Left(error) => error
      case Right(value) => fail(s"expected parse error, got $value")

  test("FormulaParser parses hrf + covariate calls") {
    val f = FormulaParser.parse("""onset ~ hrf(cond, basis="spmg3", id="task") + covariate(x, y, data=motion, id="motion", prefix="motion")""")
    assertEquals(f.onset.value, "onset")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(vars = Vector(col("cond")), basis = Some("spmg3"), id = Some(term("task"))),
        CovariateCall(
          vars = Vector(col("x"), col("y")),
          data = Some("motion"),
          id = Some(term("motion")),
          prefix = Some(term("motion"))
        )
      )
    )
  }

  test("FormulaParser parses parametric basis calls in hrf vars") {
    val f = FormulaParser.parse("onset ~ hrf(Scale(rt))")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(ArgValue.Call("Scale", Vector(Arg(None, col("rt")))))
        )
      )
    )
  }

  test("FormulaParser retains an ordered additive modulator family") {
    val f = FormulaParser.parse("onset ~ hrf(modulators(center(x), y), id = slopes)")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(
            ArgValue.Call(
              "modulators",
              Vector(
                Arg(None, ArgValue.Call("center", Vector(Arg(None, col("x"))))),
                Arg(None, col("y"))
              )
            )
          ),
          id = Some(term("slopes"))
        )
      )
    )
  }

  test("FormulaParser parses trialwise()") {
    val f = FormulaParser.parse("onset ~ trialwise(basis = \"spmg2\", add_sum = TRUE, label = trialwise)")
    assertEquals(
      f.terms,
      Vector(
        TrialwiseCall(
          basis = Some("spmg2"),
          addSum = Some(true),
          label = Some(term("trialwise"))
        )
      )
    )
  }

  test("FormulaParser parses subset= and hrf_fun= in hrf()") {
    val f = FormulaParser.parse("""onset ~ hrf(cond, subset = !cond_flag, hrf_fun = "hrfs")""")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(col("cond")),
          subset = Some(ArgValue.Call("!", Vector(Arg(None, col("cond_flag"))))),
          hrfFun = Some(ArgValue.Str("hrfs"))
        )
      )
    )
  }

  test("FormulaParser parses contrasts= in hrf()") {
    val f = FormulaParser.parse("onset ~ hrf(cond, id = task, contrasts = myset)")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(col("cond")),
          contrasts = Some("myset"),
          id = Some(term("task"))
        )
      )
    )
  }

  test("FormulaParser parses R hrf parity arguments") {
    val f = FormulaParser.parse("""onset ~ hrf(cond, onsets = stim_onset, durations = "dur", prefix = pre, normalize = TRUE)""")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(col("cond")),
          onsets = Some(col("stim_onset")),
          durations = Some(ArgValue.Str("dur")),
          prefix = Some(term("pre")),
          normalize = Some(true)
        )
      )
    )
  }

  test("FormulaParser parses phase provenance arguments") {
    val f = FormulaParser.parse("onset ~ hrf(cond, onsets = phase_onset, durations = phase_dur, phase = probe, parent = trial_id, id = probe)")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(
          vars = Vector(col("cond")),
          onsets = Some(col("phase_onset")),
          durations = Some(col("phase_dur")),
          phase = Some(
            PhaseRef(
              PhaseId.unsafe("probe"),
              ColumnId.unsafe("trial_id")
            )
          ),
          id = Some(term("probe"))
        )
      )
    )
  }

  test("FormulaParser rejects incomplete phase provenance") {
    val missingParent = FormulaParser.parseEither(
      "onset ~ hrf(cond, phase = probe)"
    )
    val missingPhase = FormulaParser.parseEither(
      "onset ~ hrf(cond, parent = trial_id)"
    )

    assert(missingParent.left.exists(_.message.contains("requires a 'parent'")))
    assert(missingPhase.left.exists(_.message.contains("requires a 'phase'")))
  }

  test("FormulaParser parses trialwise durations and normalize") {
    val f = FormulaParser.parse("onset ~ trialwise(durations = dur, normalize = TRUE)")
    assertEquals(
      f.terms,
      Vector(
        TrialwiseCall(
          durations = Some(col("dur")),
          normalize = Some(true)
        )
      )
    )
  }

  test("FormulaParser parses named HRF column scaling and rejects conflicting compatibility syntax") {
    val formula = FormulaParser.parse(
      "onset ~ hrf(condition, scaling = unit_maximum_absolute, id = task) + " +
        "trialwise(scaling = as_convolved)"
    )
    assertEquals(
      formula.terms.collect { case term: HrfCall => term.scaling },
      Vector(Some(HrfColumnScaling.UnitMaximumAbsolute))
    )
    assertEquals(
      formula.terms.collect { case term: TrialwiseCall => term.scaling },
      Vector(Some(HrfColumnScaling.AsConvolved))
    )
    assert(
      FormulaParser.parseEither(
        "onset ~ hrf(condition, scaling = unit_maximum_absolute, normalize = true)"
      ).isLeft
    )
  }

  test("FormulaParser validates named arguments through shared term schemas") {
    assertEquals(
      parseError("onset ~ hrf(cond, basis = spmg1, basis = spmg2)").message,
      "Duplicate argument 'basis'"
    )
    assertEquals(
      parseError("onset ~ hrf(cond, typo = TRUE)").message,
      "hrf(...) got unknown named args: typo"
    )
    assertEquals(
      parseError("onset ~ hrf(cond, hrf_fun = TRUE)").message,
      "'hrf_fun' must be a string/identifier, found Bool(true)"
    )
    assertEquals(
      parseError("onset ~ trialwise(cond)").message,
      "trialwise(...) does not take positional arguments"
    )
    assertEquals(
      parseError("onset ~ covariate(x, data = FALSE)").message,
      "'data' must be a string/identifier, found Bool(false)"
    )
  }
