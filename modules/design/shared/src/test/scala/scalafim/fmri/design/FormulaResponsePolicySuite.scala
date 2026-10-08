package scalafim.fmri.design

import scalafim.fmri.design.data.*
import scalafim.fmri.design.event.{ContinuousEvent, ConvolvedTerm}
import scalafim.fmri.design.formula.*
import scalafim.fmri.design.formula.EventModelBuilder.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Formula-level response conventions, modulator policies and their receipts. */
class FormulaResponsePolicySuite extends munit.FunSuite:
  private val twoRuns = SamplingFrame(blockLens = Seq(40, 40), tr = Seq(1.0, 1.0))
  private val table = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(0.0, 9.0, 18.0, 27.0, 2.0, 11.0, 20.0, 29.0)),
    "condition" -> Column.Strings(Vector("a", "b", "a", "b", "b", "a", "b", "a")),
    "dur" -> Column.Doubles(Vector(0.0, 2.0, 4.0, 2.0, 0.0, 4.0, 2.0, 0.0)),
    "first" -> Column.Doubles(Vector(5.0, 7.0, 6.0, 9.0, 4.0, 8.0, 3.0, 7.5)),
    "second" -> Column.Doubles(Vector(10.0, 1.0, 14.0, 2.5, 3.0, 12.0, 7.0, 13.0))
  )
  private val blocks = BlockPlan.explicit(Seq(0, 0, 0, 0, 1, 1, 1, 1))

  private def build(text: String, options: BuildOptions = BuildOptions()) =
    EventModelBuilder.buildEither(EventDesignRequest.fromText(text, table, twoRuns, blockPlan = blocks).toOption.get.copy(options = options))

  private def convolved(model: scalafim.fmri.design.event.EventModel): ConvolvedTerm =
    model.terms.head._2.asInstanceOf[ConvolvedTerm]

  test("event_normalization = as-convolved is exactly the omitted option, at any precision"):
    Vector(0.3.s, 0.05.s).foreach: precision =>
      val options = BuildOptions(precision = precision)
      val omitted = build("onset ~ hrf(condition, basis = spmg2, durations = dur, id = task)", options).fold(e => fail(e.message), identity)
      val declared = build("onset ~ hrf(condition, basis = spmg2, durations = dur, event_normalization = \"as-convolved\", id = task)", options).fold(e => fail(e.message), identity)
      assertEquals(declared.designMatrix.data.toVector, omitted.designMatrix.data.toVector)
      assert(convolved(declared).eventPeakScales.isEmpty)
      // "Identical to omitting" includes the content identity: no receipt, same fingerprint.
      assert(!declared.policyReceipts.exists(_.name == "event-response-normalization"), declared.policyReceipts.toString)
      assertEquals(declared.designSchema.audit.canonical, omitted.designSchema.audit.canonical)
      assertEquals(declared.designSchema.fingerprint, omitted.designSchema.fingerprint)
    val orthogonal = build("onset ~ hrf(condition, basis = spmg2, durations = dur, event_normalization = \"as-convolved\", orthogonalize_basis = TRUE, id = task)")
    assert(orthogonal.isRight, orthogonal.left.toOption.map(_.message).getOrElse(""))

  test("formula unit-peak honors BuildOptions precision and records per-column event scales in the audit"):
    val text = "onset ~ hrf(condition, basis = spmg2, durations = dur, event_normalization = \"unit-peak\", event_peak_step = 0.05, id = task)"
    val coarse = build(text, BuildOptions(precision = 0.3.s)).fold(e => fail(e.message), identity)
    val fine = build(text, BuildOptions(precision = 0.05.s)).fold(e => fail(e.message), identity)
    assert(coarse.designMatrix.data.indices.exists(i => math.abs(coarse.designMatrix.data(i) - fine.designMatrix.data(i)) > 1e-6))
    val scales = fine.designSchema.audit.eventResponseScales
    assertEquals(scales.length, fine.designMatrix.cols)
    // Condition a mixes durations {0, 4} and b mixes {0, 2}: two divisors per column.
    assert(scales.forall(_.divisors.length == 2), scales.map(_.divisors).toString)
    assert(fine.policyReceipts.exists(r => r.name == "event-response-normalization" && r.detail.contains("precision=0.05")))

  test("formula orthogonalize = TRUE is SPM-like: per run and cell, against an intercept and earlier modulators"):
    val text = "onset ~ hrf(condition, modulators(first, second), include_main = TRUE, orthogonalize = TRUE, id = task)"
    val model = build(text).fold(e => fail(e.message), identity)
    val term = convolved(model).term
    val condition = table.column(ColumnId.unsafe("condition")).toOption.get.asInstanceOf[Column.Strings].values
    val runs = Vector(0, 0, 0, 0, 1, 1, 1, 1)
    val family = term.events.collect { case e: ContinuousEvent => e }.head
    val firstColumn = family.modulatorIds.indexOf(ModulatorId.unsafe("first"))
    val secondColumn = family.modulatorIds.indexOf(ModulatorId.unsafe("second"))
    val groups = (0 until 8).groupBy(row => (runs(row), condition(row))).values
    assertEquals(groups.size, 4)
    groups.foreach: rows =>
      val first = rows.map(family.value(_, firstColumn))
      val second = rows.map(family.value(_, secondColumn))
      assertEqualsDouble(first.sum, 0.0, 1e-12)
      assertEqualsDouble(second.sum, 0.0, 1e-12)
      assertEqualsDouble(first.zip(second).map(_ * _).sum, 0.0, 1e-12)
    // The uncentred inputs had non-zero group means, so pooled-cell or
    // intercept-free residualization would leave these sums non-zero.
    assert(model.policyReceipts.exists(r => r.name == "formula-orthogonalization" && r.detail.contains("groups=run-by-cell")))
    assertEquals(model.orthogonalizationReceipts.head.steps.head.groups.length, 4)
    assertEquals(convolved(model).columnModulators.count(_.isEmpty), 2, "include_main keeps one main-effect column per condition")

  test("formula orthogonalize = TRUE centres and residualizes observed rows only; zero-filled rows stay at the reference"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 6.0, 12.0, 18.0, 24.0, 30.0, 0.0, 6.0, 12.0, 18.0, 24.0, 30.0)),
      "condition" -> Column.Strings(Vector.fill(12)("a")),
      "first" -> Column.Doubles(Vector(5.0, 7.0, Double.NaN, 9.0, 4.0, 8.0, 3.0, 7.5, 6.0, 2.0, 5.5, 9.0)),
      "second" -> Column.Doubles(Vector(10.0, 1.0, 14.0, 2.5, 3.0, 12.0, 7.0, 13.0, Double.NaN, 4.0, 11.0, 6.0))
    )
    val request = EventDesignRequest.fromText(
      "onset ~ hrf(condition, modulators(first, second), include_main = TRUE, orthogonalize = TRUE, id = task)",
      data, twoRuns, blockPlan = BlockPlan.explicit(Seq.fill(6)(0) ++ Seq.fill(6)(1))
    ).toOption.get
    val model = EventModelBuilder.buildEither(request).fold(e => fail(e.message), identity)
    val family = convolved(model).term.events.collect { case e: ContinuousEvent => e }.head
    val firstColumn = family.modulatorIds.indexOf(ModulatorId.unsafe("first"))
    val secondColumn = family.modulatorIds.indexOf(ModulatorId.unsafe("second"))
    val first = (0 until 12).map(family.value(_, firstColumn))
    val second = (0 until 12).map(family.value(_, secondColumn))
    // Missing events carry no modulation: held at the centering reference.
    assertEquals(first(2), 0.0)
    assertEquals(second(8), 0.0)
    Vector(0 until 6, 6 until 12).foreach: run =>
      val firstObserved = run.filterNot(_ == 2)
      val secondObserved = run.filterNot(_ == 8)
      assertEqualsDouble(firstObserved.map(first).sum, 0.0, 1e-12)
      assertEqualsDouble(secondObserved.map(second).sum, 0.0, 1e-12)
      assertEqualsDouble(secondObserved.map(row => first(row) * second(row)).sum, 0.0, 1e-12)
    // Observed-only means: run 1 averages five observed values, not six with a zero.
    val expectedRun1Mean = Vector(5.0, 7.0, 9.0, 4.0, 8.0).sum / 5.0
    assertEqualsDouble(first(0), 5.0 - expectedRun1Mean, 1e-12)
    val receipt = model.policyReceipts.find(_.name == "formula-orthogonalization").getOrElse(fail("formula-orthogonalization receipt"))
    assert(receipt.detail.contains("rows=observed-only;zero-filled=held-at-reference"), receipt.detail)
    assert(receipt.detail.contains(s"run-1|condition=a:${java.lang.Double.doubleToLongBits(expectedRun1Mean)}:n=5"), receipt.detail)

  test("an observed modulator with one event in a run yields an explicit degenerate receipt"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0, 4.0)),
      "condition" -> Column.Strings(Vector.fill(4)("task")),
      "rt" -> Column.Doubles(Vector(0.4, 0.6, 0.9, 0.7))
    )
    val request = EventDesignRequest.fromText(
      "onset ~ hrf(condition, modulators(modulator(rt, center = none, scale = z, missing = zero)), include_main = TRUE, id = task)",
      data, twoRuns, blockPlan = BlockPlan.explicit(Seq(0, 0, 0, 1))
    ).toOption.get
    val model = EventModelBuilder.buildEither(request).fold(e => fail(e.message), identity)
    val observed = model.policyReceipts.filter(_.name == "observed-modulator")
    assertEquals(observed.length, 2)
    assert(observed.forall(_.detail.contains("center=none;effective-center=run;grouping=run;")), "z-scoring promotes center = none to within-run centering")
    assert(observed.exists(r => r.detail.contains("run=1;") && r.detail.contains("degenerate=true")))
    assert(observed.exists(r => r.detail.contains("run=0;") && r.detail.contains("degenerate=false")))
    val degenerate = model.policyReceipts.filter(_.name == "observed-modulator-degenerate")
    assertEquals(degenerate.length, 1)
    assert(degenerate.head.detail.contains("run=1;effective-center=run;grouping=run;"), degenerate.head.detail)
    assert(model.designSchema.audit.policyReceipts.exists(_.name == "observed-modulator-degenerate"))

  test("the missing-value receipt reports per-modulator observed policies, not only the default"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 8.0, 16.0)),
      "condition" -> Column.Strings(Vector.fill(3)("task")),
      "rt" -> Column.Doubles(Vector(0.4, Double.NaN, 0.9))
    )
    val model = EventModelBuilder.buildEither(EventDesignRequest.fromText(
      "onset ~ hrf(condition, modulators(modulator(rt, center = run, scale = raw, missing = drop_from_term)), include_main = TRUE, id = task)",
      data, twoRuns).toOption.get).fold(e => fail(e.message), identity)
    val receipt = model.policyReceipts.find(_.name == "modulator-missing-values").getOrElse(fail("missing-value receipt"))
    assert(receipt.detail.contains("default=zero-contribution"), receipt.detail)
    assert(receipt.detail.contains("effective=rt:drop-from-term"), receipt.detail)

  test("shared slopes produce the numerical main-effect and pooled slope columns"):
    val shared = build("onset ~ hrf(condition, modulators(first), include_main = TRUE, shared_slopes = TRUE, id = task)").fold(e => fail(e.message), identity)
    val reference = build("onset ~ hrf(condition, id = main) + hrf(first, id = slope)").fold(e => fail(e.message), identity)
    assertEquals(shared.designMatrix.cols, 3)
    assertEquals(reference.designMatrix.cols, 3)
    (0 until shared.designMatrix.rows).foreach: row =>
      (0 until 3).foreach: column =>
        assertEqualsDouble(shared.designMatrix(row, column), reference.designMatrix(row, column), 1e-12)
    assert((0 until shared.designMatrix.rows).exists(row => math.abs(shared.designMatrix(row, 2)) > 1e-3))

  test("observed-modulator receipts name the requested centring scope and the rows that share a mean"):
    def receipts(center: String) =
      build(s"onset ~ hrf(condition, modulators(modulator(first, center = $center)), id = task)")
        .fold(e => fail(e.message), identity)
        .policyReceipts.filter(_.name == "observed-modulator")
    val run = receipts("run")
    assertEquals(run.length, 2)
    assert(run.forall(_.detail.contains("center=run;effective-center=run;grouping=run;")), run.toString)
    assert(run.exists(_.detail.startsWith("modulator=first;run=0;center=run;")), run.toString)
    val cell = receipts("cell")
    assertEquals(cell.length, 2)
    assert(cell.forall(_.detail.contains("center=cell;effective-center=cell;grouping=run-by-cell(condition);")), cell.toString)
    val none = receipts("none")
    assert(none.forall(_.detail.contains("center=none;effective-center=none;grouping=none;")), none.toString)

  test("a cell-centred modulator without factors groups by run alone"):
    val model = EventModelBuilder.buildEither(
      EventDesignRequest.fromText("onset ~ hrf(modulators(modulator(first, center = cell)), id = slope)", table, twoRuns, blockPlan = blocks).toOption.get
    ).fold(e => fail(e.message), identity)
    val observed = model.policyReceipts.filter(_.name == "observed-modulator")
    assertEquals(observed.length, 2)
    assert(observed.forall(_.detail.contains("center=cell;effective-center=cell;grouping=run;")), observed.toString)

  test("build receipts name each run's sampling reference and its start time"):
    val defaulted = build("onset ~ hrf(condition, id = task)").fold(e => fail(e.message), identity)
    assertEquals(
      defaulted.policyReceipts.filter(_.name == "sampling-reference").map(_.detail),
      Vector("run=0;reference=mid-volume;start-time=0.5;tr=1", "run=1;reference=mid-volume;start-time=0.5;tr=1")
    )
    assert(defaulted.designSchema.audit.policyReceipts.exists(_.name == "sampling-reference"))
    val frame = SamplingFrame(blockLens = Seq(40, 40), tr = Seq(2.0), startTime = Seq(0.0, 0.7))
    val explicit = EventModelBuilder.buildEither(
      EventDesignRequest.fromText("onset ~ hrf(condition, id = task)", table, frame, blockPlan = blocks).toOption.get
    ).fold(e => fail(e.message), identity)
    assertEquals(
      explicit.policyReceipts.filter(_.name == "sampling-reference").map(_.detail),
      Vector("run=0;reference=volume-onset;start-time=0;tr=2", "run=1;reference=explicit-offset;start-time=0.7;tr=2")
    )
