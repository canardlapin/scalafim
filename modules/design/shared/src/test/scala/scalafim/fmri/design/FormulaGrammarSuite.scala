package scalafim.fmri.design

import scalafim.fmri.design.data.*
import scalafim.fmri.design.formula.*
import scalafim.fmri.design.formula.EventModelBuilder.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class FormulaGrammarSuite extends munit.FunSuite:
  private val table = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 30.0)),
    "condition" -> Column.Strings(Vector("a", "b")))
  private val frame = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))
  private def build(formula: String) =
    EventModelBuilder.buildEither(EventDesignRequest.fromText(formula, table, frame).toOption.get)

  test("all-events constant creates one stream and round-trips"):
    val formula = FormulaParser.parse("onset ~ hrf(1, basis = fir, nbasis = 4, span = 8)")
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = build(formula.text).toOption.get
    assertEquals(model.designMatrix.cols, 4)
    // Four 2-second bins over [0, 8), repeated at the second onset.
    (0 until 8).foreach: row =>
      (0 until 4).foreach: column =>
        assertEqualsDouble(model.designMatrix(row, column), if row / 2 == column then 1.0 else 0.0, 1e-12)
    assertEqualsDouble(model.designMatrix(8, 3), 0.0, 1e-12)
    assertEqualsDouble(model.designMatrix(30, 0), 1.0, 1e-12)

  test("kernel normalization preserves relative event counts and differs from column normalization"):
    val raw = build("onset ~ hrf(condition)").toOption.get.designMatrix
    val unit = build("onset ~ hrf(condition, kernel_normalization = unit_peak)").toOption.get.designMatrix
    val canonicalPeak = (0 to 1200).map(i => Hrfs.SPMG1(Lag(i * 0.02))(0)).max
    raw.data.indices.foreach: i =>
      assertEqualsDouble(unit.data(i), raw.data(i) / canonicalPeak, 1e-10)
    val formula = FormulaParser.parse("onset ~ hrf(condition, kernel_normalization = unit_peak)")
    assertEquals(FormulaParser.parse(formula.text), formula)

  test("span and normalization validate instead of silently changing custom kernels"):
    assert(FormulaParser.parseEither("onset ~ hrf(1, span = -1)").isLeft)
    assert(FormulaParser.parseEither("onset ~ hrf(1, kernel_normalization = typo)").isLeft)
    assert(build("onset ~ hrf(1, span = 20)").isLeft)
    assert(build("onset ~ hrf(condition, hrf_fun = callback, kernel_normalization = unit_peak)").isLeft)

  test("explicit main plus modulators retains distinct column provenance"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 24.0)),
      "condition" -> Column.Strings(Vector("a", "b", "a", "b")),
      "amplitude" -> Column.Doubles(Vector(2.0, 3.0, 4.0, 5.0)))
    val text = "onset ~ hrf(condition, modulators(amplitude), include_main = TRUE, id = task)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val req = EventDesignRequest.fromText(text, data, frame).toOption.get
    val paired = EventModelBuilder.buildEither(req).toOption.get
    val conv = paired.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm]
    assertEquals(conv.data.cols, 4)
    assertEquals(conv.columnModulators.count(_.isEmpty), 2)
    assertEquals(conv.columnModulators.count(_.contains(ModulatorId.unsafe("amplitude"))), 2)
    val separate = EventModelBuilder.buildEither(req.copy(formula = FormulaParser.parse(
      "onset ~ hrf(condition, id = main) + hrf(condition, modulators(amplitude), id = slopes)"))).toOption.get
    assertEquals(paired.designMatrix.data.toVector, separate.designMatrix.data.toVector)

  test("formula serial orthogonalization is ordered within each run"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 0.0, 8.0, 16.0)),
      "condition" -> Column.Strings(Vector.fill(6)("task")),
      "first" -> Column.Doubles(Vector(1.0, 2.0, 4.0, 1.0, 3.0, 8.0)),
      "second" -> Column.Doubles(Vector(2.0, 5.0, 9.0, 3.0, 7.0, 15.0))
    )
    val twoRuns = SamplingFrame(blockLens = Seq(30, 30), tr = Seq(1.0, 1.0))
    val text = "onset ~ hrf(condition, modulators(first, second), id = task, orthogonalize = TRUE)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(text, data, twoRuns, blockIds = Seq(0, 0, 0, 1, 1, 1)).toOption.get
    val event = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term.events
      .collectFirst { case value: scalafim.fmri.design.event.ContinuousEvent => value }.get
    Vector(0 until 3, 3 until 6).foreach: rows =>
      val dot = rows.map(row => event.value(row, 0) * event.value(row, 1)).sum
      assertEqualsDouble(dot, 0.0, 1e-10)
    assertEquals(model.orthogonalizationReceipts.length, 1)

  test("centered product modulators retain component identity and center after row selection"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 0.0, 8.0)),
      "condition" -> Column.Strings(Vector.fill(4)("task")),
      "gain" -> Column.Doubles(Vector(1.0, 3.0, 4.0, 8.0)),
      "loss" -> Column.Doubles(Vector(10.0, 14.0, 1.0, 5.0))
    )
    val twoRuns = SamplingFrame(blockLens = Seq(30, 30), tr = Seq(1.0, 1.0))
    val text = "onset ~ hrf(condition, modulators(product(center_within_run(gain), center_within_run(loss))), id = task)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(text, data, twoRuns, blockIds = Seq(0, 0, 1, 1)).toOption.get
    val event = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term.events
      .collectFirst { case value: scalafim.fmri.design.event.ContinuousEvent => value }.get
    assertEquals(Vector.tabulate(4)(event.value(_, 0)), Vector(2.0, 2.0, 4.0, 4.0))
    assert(model.policyReceipts.exists(_.name == "product-modulator"))
    assert(EventModelBuilder.buildEither("onset ~ hrf(product(center_within_run(gain), center_within_run(loss)))", data, twoRuns, Seq(0, 0, 1, 1)).isLeft)
    val selected = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 24.0)),
      "condition" -> Column.Strings(Vector.fill(4)("task")),
      "keep" -> Column.Bools(Vector(false, true, true, true)),
      "gain" -> Column.Doubles(Vector(Double.NaN, 1.0, Double.NaN, 5.0)),
      "loss" -> Column.Doubles(Vector(2.0, 3.0, 4.0, 7.0))
    )
    val dropped = EventModelBuilder.buildEither(
      "onset ~ hrf(condition, modulators(product(center_within_run(gain), center_within_run(loss))), subset = keep, id = task)",
      selected, frame, Seq(0, 0, 0, 0), missingValuePolicy = MissingValuePolicy.DropFromTerm
    ).toOption.get
    val retained = dropped.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term
    assertEquals(retained.onsets.map(_.value), Vector(8.0, 24.0))

  test("shared slopes retain categorical main effects and one slope stream"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 24.0)),
      "condition" -> Column.Strings(Vector("a", "b", "a", "b")),
      "amplitude" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0))
    )
    val text = "onset ~ hrf(condition, modulators(amplitude), include_main = TRUE, shared_slopes = TRUE, id = task)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(EventDesignRequest.fromText(text, data, frame).toOption.get).toOption.get
    val convolved = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm]
    assertEquals(convolved.data.cols, 3)
    assertEquals(convolved.columnModulators.count(_.isEmpty), 2)
    assertEquals(convolved.columnModulators.count(_.contains(ModulatorId.unsafe("amplitude"))), 1)

  test("derivative-basis orthogonalization is post-convolution and auditable"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 24.0)),
      "condition" -> Column.Strings(Vector("a", "a", "b", "b"))
    )
    val text = "onset ~ hrf(condition, basis = spmg2, orthogonalize_basis = TRUE, id = task)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(EventDesignRequest.fromText(text, data, frame).toOption.get).toOption.get
    assertEquals(model.basisOrthogonalizationReceipts.length, 1)
    val receipt = model.basisOrthogonalizationReceipts.head
    assertEquals(receipt.groups.length, 2)
    receipt.groups.foreach: group =>
      val canonical = group.columns.head
      group.columns.tail.foreach: derivative =>
        val dot = (0 until model.designMatrix.rows).map(row => model.designMatrix(row, canonical) * model.designMatrix(row, derivative)).sum
        assertEqualsDouble(dot, 0.0, 1e-8)

  test("portable run scaling aliases lower through observed-modulator policy"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 0.0, 8.0)),
      "condition" -> Column.Strings(Vector.fill(4)("task")),
      "value" -> Column.Doubles(Vector(1.0, 3.0, 4.0, 8.0))
    )
    val twoRuns = SamplingFrame(blockLens = Seq(30, 30), tr = Seq(1.0, 1.0))
    val text = "onset ~ hrf(condition, modulators(scale_within_run(value)), id = task)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(text, data, twoRuns, Seq(0, 0, 1, 1)).toOption.get
    val event = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term.events
      .collectFirst { case value: scalafim.fmri.design.event.ContinuousEvent => value }.get
    assertEqualsDouble(event.value(0, 0), -1.0 / math.sqrt(2.0), 1e-12)
    assertEqualsDouble(event.value(1, 0), 1.0 / math.sqrt(2.0), 1e-12)
    val sd = EventModelBuilder.buildEither("onset ~ hrf(condition, modulators(sd_within_run(value)), id = task)", data, twoRuns, Seq(0, 0, 1, 1)).toOption.get
    val sdEvent = sd.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term.events
      .collectFirst { case value: scalafim.fmri.design.event.ContinuousEvent => value }.get
    assertEqualsDouble(sdEvent.value(0, 0), 1.0 / math.sqrt(2.0), 1e-12)
    assertEqualsDouble(sdEvent.value(1, 0), 3.0 / math.sqrt(2.0), 1e-12)

  test("subset negation and inequality do not retain missing numeric rows"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0)),
      "condition" -> Column.Strings(Vector("a", "a", "a")),
      "rt" -> Column.Doubles(Vector(Double.NaN, 0.0, 1.0)))
    Vector("rt != 0", "!(rt == 0)").foreach: predicate =>
      val req = EventDesignRequest.fromText(s"onset ~ hrf(condition, subset = $predicate)", data, frame).toOption.get
      val model = EventModelBuilder.buildEither(req).toOption.get
      val conv = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm]
      assertEquals(conv.term.onsets.map(_.value), Vector(16.0))

  test("arithmetic precedence, signed literals, and Unicode operators round-trip"):
    val parsed = FormulaParser.parseExpression("1+2×3−4÷2").toOption.get
    val values = EventExpressions.evaluate(parsed, table).toOption.get
    assertEquals(values, Vector.fill(2)(EventExpressionValue.Num(5.0)))
    val formula = FormulaParser.parse("onset ~ hrf(condition, subset = -(onset + 1) / 2 < -0.3, lag = -1e-2)")
    assertEquals(FormulaParser.parse(formula.text), formula)
    assert(FormulaParser.parseExpression("1e+").isLeft)
    assert(FormulaParser.parseExpression("1e999").isLeft)

  test("per-modulator observed policies apply after subset and to the entire paired term"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 0.0, 8.0, 16.0)),
      "condition" -> Column.Strings(Vector("a", "a", "b", "a", "b", "b")),
      "rt" -> Column.Doubles(Vector(1.0, 3.0, Double.NaN, 5.0, 9.0, Double.NaN)))
    val twoRuns = SamplingFrame(blockLens = Seq(30, 30), tr = Seq(1.0, 1.0))
    def request(policy: String, subset: String = "") =
      EventDesignRequest.fromText(
        s"onset ~ hrf(condition, modulators(modulator(rt, center = run, scale = raw, missing = $policy)), include_main = TRUE$subset)",
        data, twoRuns, blockPlan = BlockPlan.explicit(Seq(0, 0, 0, 1, 1, 1))).toOption.get
    val dropped = EventModelBuilder.buildEither(request("drop_from_term")).toOption.get
    val droppedTerm = dropped.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term
    assertEquals(droppedTerm.onsets.map(_.value), Vector(0.0, 8.0, 0.0, 8.0))
    val family = droppedTerm.events.collectFirst { case e: scalafim.fmri.design.event.ContinuousEvent => e }.get
    assertEquals(Vector.tabulate(4)(family.value(_, 1)), Vector(-1.0, 1.0, -2.0, 2.0))
    val zeroed = EventModelBuilder.buildEither(request("zero")).toOption.get
    val zeroTerm = zeroed.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term
    assertEquals(zeroTerm.onsets.length, 6)
    val zeroFamily = zeroTerm.events.collectFirst { case e: scalafim.fmri.design.event.ContinuousEvent => e }.get
    assertEquals(Vector.tabulate(6)(zeroFamily.value(_, 1)), Vector(-1.0, 1.0, 0.0, -2.0, 2.0, 0.0))
    assert(EventModelBuilder.buildEither(request("reject")).isLeft)
    val selected = EventModelBuilder.buildEither(request("zero", ", subset = rt > 2")).toOption.get
    val selectedFamily = selected.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm].term.events
      .collectFirst { case e: scalafim.fmri.design.event.ContinuousEvent => e }.get
    assertEquals(Vector.tabulate(3)(selectedFamily.value(_, 1)), Vector(0.0, -2.0, 2.0))
    val formula = request("zero").formula
    assertEquals(FormulaParser.parse(formula.text), formula)

  test("temporal conventions and duration normalization are explicit and portable"):
    val text = "onset ~ hrf(condition, basis = spmg3, temporal_derivative = \"spm-1s\", event_normalization = \"unit-peak\", event_peak_step = 0.1, durations = 2)"
    val formula = FormulaParser.parse(text)
    assertEquals(FormulaParser.parse(formula.text), formula)
    val model = EventModelBuilder.buildEither(EventDesignRequest.fromText(text, table, frame).toOption.get).toOption.get
    val term = model.terms.head._2.asInstanceOf[scalafim.fmri.design.event.ConvolvedTerm]
    assertEquals(term.eventPeakScales.length, table.nrows)
    assert(term.eventPeakScales.forall(_.divisors.forall(_ > 0)))
    assertEquals(term.hrf.basisElements.map(_.role), Vector(scalafim.fmri.hrf.BasisRole.Canonical, scalafim.fmri.hrf.BasisRole.TemporalDerivative, scalafim.fmri.hrf.BasisRole.DispersionDerivative))
    // spm-1s is SPM12's informed basis on the TR / 16 kernel grid (TR = 1 s here).
    val grid = SpmKernelGrid(Seconds(1.0)).fold(error => fail(error.message), identity)
    val spm = TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG1, 3, grid).fold(error => fail(error.message), identity)
    Vector(0.5, 4.0, 5.0, 12.0).foreach: t =>
      (0 until 3).foreach: c =>
        assertEqualsDouble(term.hrf(Lag(t)).data(c), spm(Lag(t)).data(c), 0.0)
    val dot = grid.times.map(t => term.hrf(Lag(t)).data(0) * term.hrf(Lag(t)).data(1)).sum
    assertEqualsDouble(dot, 0.0, 1e-14)
    assert(model.policyReceipts.exists(r => r.name == "event-response-normalization" && r.detail.contains("policy=unit-peak")))
    val twoTr = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0, 2.0))
    assert(EventModelBuilder.buildEither(EventDesignRequest.fromText(text, table, twoTr, blockPlan = BlockPlan.explicit(Seq(0, 1))).toOption.get).isLeft)
    assert(EventModelBuilder.buildEither(EventDesignRequest.fromText("onset ~ hrf(condition, basis = fir, temporal_derivative = \"spm-1s\")", table, frame).toOption.get).isLeft)
