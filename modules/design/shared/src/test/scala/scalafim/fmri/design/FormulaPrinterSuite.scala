package scalafim.fmri.design

import scalafim.fmri.design.formula.*

class FormulaPrinterSuite extends munit.FunSuite:
  private val examples = Vector(
    "onset ~ hrf(condition, basis = spmg1, id = localizer, normalize = TRUE)",
    "onset ~ hrf(choice, id = main) + hrf(choice, modulators(center(gain), scale(loss)), id = slopes)",
    "onset ~ hrf(condition, onsets = probe_onset, durations = probe_duration, phase = probe, parent = trial_id)",
    "onset ~ hrf(condition, basis = fir, nbasis = 12, subset = (rt > 0.3 & correct == TRUE) | !excluded)",
    "onset ~ hrf(a, b, basis = spmg3, id = factorial) + covariate(x, y, data = motion) + trialwise(add_sum = TRUE)"
  )

  examples.zipWithIndex.foreach: (input, index) =>
    test(s"canonical formula round trip $index"):
      val formula = FormulaParser.parse(input)
      val tokens = formula.render
      assertEquals(FormulaParser.parse(tokens.map(_.text).mkString), formula)
      assertEquals(FormulaParser.parse(formula.text).text, formula.text)
      assert(tokens.forall(_.status == FormulaTokenStatus.Accepted))
      assert(tokens.exists(_.termIndex.contains(0)))

  test("printer preserves explicit defaults, signed values and string escapes"):
    val formula = FormulaParser.parse("""onset ~ hrf(cond, lag = -1.5, summate = FALSE, id = "a\\b\"c", scaling = "as-convolved")""")
    assertEquals(FormulaParser.parse(formula.text), formula)

  test("quoted identifiers preserve punctuation and reserved names"):
    val formula = ModelFormula(ColumnId.unsafe("scan-onset"), Vector(CovariateCall(Vector(ArgValue.Ident(ColumnId.unsafe("true"))))))
    assertEquals(FormulaParser.parse(formula.text), formula)

  test("source spans address original arguments including nested calls"):
    val input = "onset  ~ hrf(cond, modulators(center(rt), gain), basis = 'spmg3') + trialwise(label = trials)"
    val parsed = FormulaParser.parsePositioned(input).toOption.get
    def text(term: Option[Int], path: Vector[String]): String =
      val span = parsed.nodes.find(n => n.termIndex == term && n.argPath == path).get.span
      input.substring(span.start, span.end)
    assertEquals(text(None, Vector("onset")), "onset")
    assertEquals(text(Some(0), Vector("1", "0", "0")), "rt")
    assertEquals(text(Some(0), Vector("basis")), "basis = 'spmg3'")
    assertEquals(text(Some(1), Vector.empty), "trialwise(label = trials)")

  test("semantic argument errors point to the offending argument"):
    Vector("nbasis = 1.5", "basis = FALSE", "typo = TRUE", "hrf_fun = TRUE").foreach: bad =>
      val input = s"onset ~ hrf(cond, $bad) + trialwise()"
      val error = FormulaParser.parseEither(input).swap.toOption.get
      assertEquals(error.pos, input.indexOf(bad))
    val duplicate = "onset ~ hrf(cond, basis = spmg1, basis = spmg2)"
    assertEquals(FormulaParser.parseEither(duplicate).swap.toOption.get.pos, duplicate.lastIndexOf("basis"))

  test("nonrepresentable typed formula fails explicitly"):
    val invalid = ModelFormula(ColumnId.unsafe("onset"), Vector.empty)
    assert(invalid.renderEither.isLeft)

  test("source spans cover comparison, logical and unary operands"):
    val input = "onset ~ hrf(cond, subset = !(rt > 0.3 & correct == TRUE))"
    val parsed = FormulaParser.parsePositioned(input).toOption.get
    def at(path: String*): String =
      val node = parsed.nodes.find(n => n.termIndex.contains(0) && n.argPath == path.toVector).get
      input.substring(node.span.start, node.span.end)
    assertEquals(at("subset", "0", "0", "0"), "rt")
    assertEquals(at("subset", "0", "0", "1"), "0.3")
    assertEquals(at("subset", "0", "1", "0"), "correct")
    assertEquals(at("subset", "0", "1", "1"), "TRUE")
