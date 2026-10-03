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

  private def column(name: String): ArgValue = ArgValue.Ident(ColumnId.unsafe(name))
  private def op(name: String, values: ArgValue*): ArgValue = ArgValue.Call(name, values.toVector.map(Arg(None, _)))

  test("numbers print byte-identically on JVM and JS"):
    val cases = Vector(
      4.0 -> "4", -4.0 -> "-4", 0.1 -> "0.1", 0.25 -> "0.25", 1e-7 -> "1e-7", 1.5e-7 -> "1.5e-7",
      1e21 -> "1e21", 1e20 -> "100000000000000000000", -0.0 -> "0", 123456.789 -> "123456.789",
      1.7976931348623157e308 -> "1.7976931348623157e308", 0.1 + 0.2 -> "0.30000000000000004"
    )
    cases.foreach: (value, text) =>
      assertEquals(FormulaPrinter.expressionTextEither(ArgValue.Num(value)), Right(text))
      assertEquals(FormulaParser.parseExpression(text), Right(ArgValue.Num(value)))
    val formula = FormulaParser.parse("onset ~ hrf(cond, lag = 4.0, subset = rt > 0.1e-6) + trialwise(lag = 1E21)")
    assertEquals(formula.text, "onset ~ hrf(cond, subset = (rt > 1e-7), lag = 4) + trialwise(lag = 1e21)")
    assertEquals(formula.textEither, Right(formula.text))

  test("non-finite literals are reported, not thrown, by the safe printers"):
    val formula = ModelFormula(ColumnId.unsafe("onset"), Vector(HrfCall(Vector(column("cond")), lag = Some(Double.NaN))))
    assert(formula.renderEither.isLeft)
    assert(formula.textEither.isLeft)
    assert(FormulaPrinter.expressionTextEither(op("+", column("a"), ArgValue.Num(Double.NegativeInfinity))).isLeft)

  test("adversarial expressions round trip through print and parse"):
    val atoms = Vector(
      column("plain"), column("TRUE"), column("false"), column("a`b"), column("a\\b"), column("a\"b"), column("x.y"),
      column("_1"), column("1a"), column("récompense"), column("Inf"),
      ArgValue.Str(""), ArgValue.Str("q\"uote"), ArgValue.Str("back\\slash"), ArgValue.Str("tick`"), ArgValue.Str("line\nbreak"),
      ArgValue.Num(0), ArgValue.Num(-0.0), ArgValue.Num(-2.5), ArgValue.Num(1e-300), ArgValue.Num(-1e21), ArgValue.Num(1.7976931348623157e308),
      ArgValue.Bool(true), ArgValue.Bool(false)
    )
    val wrappers: Vector[ArgValue => ArgValue] = Vector(
      identity,
      value => op("!", value),
      value => op("-", value),
      value => op("!", op("!", value)),
      value => op("-", op("-", value)),
      value => ArgValue.Call("+", Vector(Arg(Some("x"), value), Arg(None, column("b")))),
      value => ArgValue.Call("!", Vector(Arg(Some("TRUE"), value))),
      value => op("==", value, column("b"), column("c")),
      value => ArgValue.Call("`odd` fn", Vector(Arg(Some("a b"), value)))
    )
    val operators = Vector("+", "-", "*", "/", "|", "&", "==", "!=", "<", "<=", ">", ">=")
    val corpus =
      for
        atom <- atoms
        wrap <- wrappers
        expression <- Vector(wrap(atom)) ++ operators.flatMap(o => Vector(op(o, wrap(atom), column("z")), op(o, column("z"), wrap(atom)), op(o, op("!", wrap(atom)), op("-", wrap(atom)))))
      yield expression
    corpus.foreach: expression =>
      val text = FormulaPrinter.expressionTextEither(expression).fold(error => fail(s"$expression did not print: ${error.message}"), identity)
      assertEquals(FormulaParser.parseExpression(text), Right(expression), clue = text)

  test("negation follows R precedence: below comparison, above & and |"):
    def parse(text: String) = FormulaParser.parseExpression(text).toOption.get
    assertEquals(parse("!a == b"), op("!", op("==", column("a"), column("b"))))
    assertEquals(parse("!a & b"), op("&", op("!", column("a")), column("b")))
    assertEquals(parse("!a | b & c"), op("|", op("!", column("a")), op("&", column("b"), column("c"))))
    assertEquals(parse("!a > 1 + 2"), op("!", op(">", column("a"), op("+", ArgValue.Num(1), ArgValue.Num(2)))))
    assertEquals(parse("1 + !a == b"), op("+", ArgValue.Num(1), op("!", op("==", column("a"), column("b")))))
    val left = op("==", op("!", column("a")), column("b"))
    assertEquals(FormulaPrinter.expressionTextEither(left).flatMap(FormulaParser.parseExpression), Right(left))

  test("positional and conflict errors point at the offending argument"):
    Vector(
      "onset ~ hrf(cond, \"bad\")" -> "\"bad\"",
      "onset ~ covariate(x, 2)" -> "2",
      "onset ~ hrf(cond, scaling = as_convolved, normalize = TRUE)" -> "normalize",
      "onset ~ trialwise(scaling = as_convolved, normalize = TRUE)" -> "normalize",
      "onset ~ hrf(cond, id = a, name = b)" -> "name"
    ).foreach: (input, bad) =>
      val error = FormulaParser.parseEither(input).swap.toOption.getOrElse(fail(s"expected a parse error for $input"))
      assertEquals(error.pos, input.indexOf(bad), clue = input)
    assertEquals(FormulaParser.parse("onset ~ hrf(cond, name = b)").terms.collect { case h: HrfCall => h.id.map(_.value) }, Vector(Some("b")))
