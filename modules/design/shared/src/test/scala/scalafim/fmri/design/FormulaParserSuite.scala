package scalafim.fmri.design

import scalafim.fmri.design.formula.*

class FormulaParserSuite extends munit.FunSuite:

  private def parseError(formula: String): FormulaParser.ParseError =
    FormulaParser.parseEither(formula) match
      case Left(error) => error
      case Right(value) => fail(s"expected parse error, got $value")

  test("FormulaParser parses hrf + covariate calls") {
    val f = FormulaParser.parse("""onset ~ hrf(cond, basis="spmg3", id="task") + covariate(x, y, data=motion, id="motion", prefix="motion")""")
    assertEquals(f.onset, "onset")
    assertEquals(
      f.terms,
      Vector(
        HrfCall(vars = Vector(ArgValue.Ident("cond")), basis = Some("spmg3"), id = Some("task")),
        CovariateCall(
          vars = Vector(ArgValue.Ident("x"), ArgValue.Ident("y")),
          data = Some("motion"),
          id = Some("motion"),
          prefix = Some("motion")
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
          vars = Vector(ArgValue.Call("Scale", Vector(Arg(None, ArgValue.Ident("rt")))))
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
          label = Some("trialwise")
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
          vars = Vector(ArgValue.Ident("cond")),
          subset = Some(ArgValue.Call("!", Vector(Arg(None, ArgValue.Ident("cond_flag"))))),
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
          vars = Vector(ArgValue.Ident("cond")),
          contrasts = Some(ArgValue.Ident("myset")),
          id = Some("task")
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
          vars = Vector(ArgValue.Ident("cond")),
          onsets = Some(ArgValue.Ident("stim_onset")),
          durations = Some(ArgValue.Str("dur")),
          prefix = Some("pre"),
          normalize = Some(true)
        )
      )
    )
  }

  test("FormulaParser parses trialwise durations and normalize") {
    val f = FormulaParser.parse("onset ~ trialwise(durations = dur, normalize = TRUE)")
    assertEquals(
      f.terms,
      Vector(
        TrialwiseCall(
          durations = Some(ArgValue.Ident("dur")),
          normalize = Some(true)
        )
      )
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
