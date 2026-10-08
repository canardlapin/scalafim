package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Every admitted [[HrfKind]] parses, prints and builds from `basis =` text. */
class FormulaBasisSuite extends munit.FunSuite:
  private val table = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(2.0, 11.0, 23.0, 37.0)),
    "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
  )
  private val frame = SamplingFrame(blockLens = Seq(30), tr = Seq(2.0))
  private val blockIds = Seq(0, 0, 0, 0)

  private def build(formula: String, defaultHrf: Hrf = Hrfs.SPMG1) =
    EventModelBuilder
      .buildEither(formula, table, frame, blockIds, defaultHrf = defaultHrf, precision = Seconds(0.1))
      .fold(error => fail(s"$formula: ${error.message}"), identity)

  private def assertSameDesign(formula: String, direct: Hrf)(using munit.Location): Unit =
    val viaText = build(s"onset ~ hrf(cond, $formula)").designMatrix
    val viaHrf = build("onset ~ hrf(cond)", defaultHrf = direct).designMatrix
    assertEquals((viaText.rows, viaText.cols), (viaHrf.rows, viaHrf.cols), formula)
    assert(viaText.cols >= 2, formula)
    var peak = 0.0
    for r <- 0 until viaText.rows; c <- 0 until viaText.cols do
      assertEqualsDouble(viaText(r, c), viaHrf(r, c), 1e-12, s"$formula at ($r, $c)")
      peak = math.max(peak, math.abs(viaText(r, c)))
    assert(peak > 0.0, s"$formula built an all-zero design")

  /** One formula per kind, with non-default named parameters, and the same kernel built directly. */
  private val parameterised: Vector[(HrfKind, String, Hrf)] = Vector(
    (HrfKind.Spmg1, "basis = spmg1(P1 = 6, P2 = 16, A1 = 0.01)", Hrfs.spmg1(P1 = 6.0, P2 = 16.0, A1 = 0.01)),
    (HrfKind.Spmg2, "basis = spmg2", Hrfs.SPMG2),
    (HrfKind.Spmg3, "basis = spmg3", Hrfs.SPMG3),
    (HrfKind.Gamma, "basis = gamma(shape = 5, rate = 0.9)", Hrfs.gamma(shape = 5.0, rate = 0.9)),
    (HrfKind.Gaussian, "basis = gaussian(mean = 5, sd = 1.5)", Hrfs.gaussian(mean = 5.0, sd = 1.5)),
    (HrfKind.Lwu, "basis = lwu(tau = 5, sigma = 2, rho = 0.3, normalize = height)",
      Hrfs.lwu(tau = 5.0, sigma = 2.0, rho = 0.3, normalize = HrfFunctions.LwuNormalize.Height)),
    (HrfKind.Cascade34, "basis = cascade34(kappaP = 0.6, kappaU = 0.25, rho = 0.4), span = 30",
      Hrfs.cascade34(Cascade34Params(0.6, 0.25, 0.4), 30.s)),
    (HrfKind.Mexhat, "basis = mexhat(mean = 5, sd = 1.5)", Hrfs.mexhat(mean = 5.0, sd = 1.5)),
    (HrfKind.InvLogit, "basis = inv_logit(mu1 = 5, s1 = 1.2, mu2 = 14, s2 = 1.5, lag = 0.5)",
      Hrfs.invLogit(mu1 = 5.0, s1 = 1.2, mu2 = 14.0, s2 = 1.5, lag = 0.5.s)),
    (HrfKind.HalfCosine, "basis = half_cosine(h1 = 1, h2 = 4, h3 = 6, h4 = 6, f1 = 0.1, f2 = 0.05)",
      Hrfs.halfCosine(h1 = 1.s, h2 = 4.s, h3 = 6.s, h4 = 6.s, f1 = 0.1, f2 = 0.05)),
    (HrfKind.Fir, "basis = fir, nbasis = 6, span = 18", Hrfs.fir(nBasis = 6, span = 18.s)),
    (HrfKind.Bspline, "basis = bspline(degree = 2), nbasis = 5", Hrfs.bspline(nBasis = 5, degree = 2)),
    (HrfKind.Tent, "basis = tent, nbasis = 4", Hrfs.tent(nBasis = 4)),
    (HrfKind.Fourier, "basis = fourier, nbasis = 4", Hrfs.fourier(nBasis = 4)),
    (HrfKind.Daguerre, "basis = daguerre(scale = 3), nbasis = 2", Hrfs.daguerre(nBasis = 2, scale = 3.0)),
    (HrfKind.Sine, "basis = sine, nbasis = 4, span = 20", Hrfs.sine(nBasis = 4, span = 20.s)),
    (HrfKind.Boxcar, "basis = boxcar(width = 4, amplitude = 2)", Hrfs.boxcar(width = 4.s, amplitude = 2.0)),
    (HrfKind.Weighted, "basis = weighted(weights = c(0, 0.5, 1, 0.5), width = 2, method = linear)",
      Hrfs.weighted(Vector(0.0, 0.5, 1.0, 0.5), width = Some(2.s), method = Hrfs.WeightedMethod.Linear))
  )

  test("the table covers every admitted HRF kind"):
    assertEquals(parameterised.map(_._1).toSet, HrfKind.all.toSet)

  parameterised.foreach: (kind, basis, direct) =>
    test(s"${kind.canonicalName} builds from formula text with named parameters"):
      assertSameDesign(basis, direct)

    test(s"${kind.canonicalName} prints losslessly and canonically"):
      // trialwise() has no `span`, so it carries the kind and its parameters only.
      val trialwiseBasis = basis.split(", (nbasis|span) = ").head
      val parsed = FormulaParser.parse(s"onset ~ hrf(cond, $basis) + trialwise($trialwiseBasis)")
      val text = parsed.textEither.fold(error => fail(error.message), identity)
      assertEquals(FormulaParser.parse(text), parsed)
      assertEquals(FormulaParser.parse(text).text, text)
      assert(text.contains(kind.canonicalName), text)

  /** Constructor defaults, so `basis = kind` is the constructor called with no arguments. */
  private val defaults: Vector[(HrfKind, Hrf)] = Vector(
    HrfKind.Spmg1 -> Hrfs.spmg1(),
    HrfKind.Gamma -> Hrfs.gamma(),
    HrfKind.Gaussian -> Hrfs.gaussian(),
    HrfKind.Lwu -> Hrfs.lwu(),
    HrfKind.Cascade34 -> Hrfs.cascade34(),
    HrfKind.Mexhat -> Hrfs.mexhat(),
    HrfKind.InvLogit -> Hrfs.invLogit(),
    HrfKind.HalfCosine -> Hrfs.halfCosine(),
    HrfKind.Bspline -> Hrfs.bspline(),
    HrfKind.Daguerre -> Hrfs.daguerre(),
    HrfKind.Sine -> Hrfs.sine(),
    HrfKind.Fir -> Hrfs.fir()
  )

  defaults.foreach: (kind, direct) =>
    test(s"${kind.canonicalName} defaults are the constructor defaults, bare and spelled out"):
      assertSameDesign(s"basis = ${kind.canonicalName}", direct)
      val explicit = FormulaBasis.params(kind).collect { case BasisParamSpec(name, _, Some(value), _) => s"$name = ${value.label}" }
      if explicit.nonEmpty then assertSameDesign(s"basis = ${kind.canonicalName}(${explicit.mkString(", ")})", direct)

  test("weighted accepts explicit sample times and prints its vectors"):
    assertSameDesign(
      "basis = weighted(weights = c(1, -0.25, 0.5), times = c(0, 3, 7.5), normalize = TRUE)",
      Hrfs.weighted(Vector(1.0, -0.25, 0.5), times = Some(Vector(0.s, 3.s, 7.5.s)), normalize = true)
    )
    val text = FormulaParser.parse("onset ~ hrf(cond, basis = weighted(weights = c(1, -0.25), times = c(0, 3)))").text
    assert(text.contains("basis = weighted(weights = c(1, -0.25), times = c(0, 3))"), text)

  test("aliases parse to and print as the canonical kind name"):
    Vector("spmg" -> "spmg1", "gam" -> "gamma", "bs" -> "bspline", "invlogit" -> "inv_logit", "halfcosine" -> "half_cosine")
      .foreach: (alias, canonical) =>
        val parsed = FormulaParser.parse(s"onset ~ hrf(cond, basis = $alias) + trialwise(basis = '$alias')")
        assertEquals(parsed.terms.head.asInstanceOf[HrfCall].basis, Some(canonical))
        assertEquals(parsed.terms(1).asInstanceOf[TrialwiseCall].basis, Some(canonical))
        assertEquals(parsed.text, s"""onset ~ hrf(cond, basis = "$canonical") + trialwise(basis = "$canonical")""")
    val call = FormulaParser.parse("onset ~ hrf(cond, basis = gam(shape = 5))")
    assertEquals(call.text, "onset ~ hrf(cond, basis = gamma(shape = 5))")
    assertSameDesign("basis = gam(shape = 5)", Hrfs.gamma(shape = 5.0))
    // A hand-built alias prints as the canonical name it parses back to.
    val handBuilt = ModelFormula(ColumnId.unsafe("onset"), Vector(HrfCall(Vector(ArgValue.Ident(ColumnId.unsafe("cond"))), basis = Some("bs"))))
    assertEquals(handBuilt.textEither, Right("""onset ~ hrf(cond, basis = "bspline")"""))

  private def parseError(input: String): FormulaParser.ParseError =
    FormulaParser.parseEither(input).swap.getOrElse(fail(s"expected a parse error for $input"))

  test("unknown kinds and bad kind parameters are positioned parse errors"):
    val unknown = "onset ~ hrf(cond, basis = nope)"
    val error = parseError(unknown)
    assertEquals(error.pos, unknown.indexOf("basis"))
    assert(error.message.contains("Unknown HRF kind 'nope'") && error.message.contains("half_cosine"), error.message)
    val unknownCall = "onset ~ trialwise(basis = nope(width = 2))"
    assertEquals(parseError(unknownCall).pos, unknownCall.indexOf("basis"))

    def at(input: String, fragment: String, contains: String)(using munit.Location): Unit =
      val e = parseError(input)
      assertEquals(e.pos, input.indexOf(fragment), e.message)
      assert(e.message.contains(contains), e.message)
    at("onset ~ hrf(cond, basis = gamma(shape = 5, bogus = 1))", "1))", "unknown parameter 'bogus'")
    at("onset ~ hrf(cond, basis = gamma(5))", "5))", "must be named")
    at("onset ~ hrf(cond, basis = gamma(shape = TRUE))", "TRUE", "must be a number")
    at("onset ~ hrf(cond, basis = lwu(normalize = peak))", "peak", "one of none, height, area")
    at("onset ~ hrf(cond, basis = bspline(degree = 1.5))", "1.5", "non-negative integer")
    at("onset ~ hrf(cond, basis = gamma(shape = 5, shape = 6))", "6))", "more than once")
    at("onset ~ hrf(cond, basis = fir(nbasis = 6))", "6))", "give 'nbasis' on the term")
    at("onset ~ hrf(cond, basis = weighted(weights = c(1, x)))", "x)", "numeric literals")
    at("onset ~ hrf(cond, basis = weighted(weights = c(1, 2), times = c(0, 1), width = 2))", "2))", "either 'times' or 'width'")
    at("onset ~ hrf(cond, basis = boxcar)", "basis", "requires parameter 'width'")
    at("onset ~ hrf(cond, basis = weighted(weights = c(1, 2)))", "basis", "requires 'times' or 'width'")
    at("onset ~ hrf(cond, basis = spmg2(P1 = 5))", "5))", "it takes no parameters")

  test("constructor-rejected values and misplaced span are typed build errors"):
    def buildError(formula: String): DesignError =
      EventModelBuilder.buildEither(formula, table, frame, blockIds).fold(identity, _ => fail(s"expected a failure for $formula"))
    Vector(
      "onset ~ hrf(cond, basis = boxcar(width = 0))" -> "'width' must be > 0",
      "onset ~ hrf(cond, basis = cascade34(kappaP = 0.1, kappaU = 0.2))" -> "kappaP > kappaU",
      "onset ~ hrf(cond, basis = lwu(sigma = -1))" -> "sigma",
      "onset ~ hrf(cond, basis = weighted(weights = c(1, 2), times = c(2, 1)))" -> "strictly increasing",
      "onset ~ hrf(cond, basis = boxcar(width = 3), span = 10)" -> "formula span requires",
      "onset ~ hrf(cond, basis = fir, nbasis = 0)" -> "nbasis"
    ).foreach: (formula, detail) =>
      buildError(formula) match
        case DesignError.FormulaBinding(message) => assert(message.contains(detail), s"$formula: $message")
        case other                               => fail(s"$formula: expected FormulaBinding, got $other")

  test("kind parameters also apply to trialwise terms"):
    val viaText = build("onset ~ trialwise(basis = gamma(shape = 4, rate = 0.8))").designMatrix
    val single = build("onset ~ trialwise(basis = gamma)").designMatrix
    assertEquals(viaText.cols, 4)
    val differs = (0 until viaText.rows).exists(r => math.abs(viaText(r, 0) - single(r, 0)) > 1e-6)
    assert(differs, "named gamma parameters change the trialwise kernel")
