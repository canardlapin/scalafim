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
      assert(declared.policyReceipts.exists(r => r.name == "event-response-normalization" && r.detail.endsWith("policy=as-convolved")))
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
    assert(observed.forall(_.detail.contains("effective-center=Global")), "z-scoring promotes center = none to global centering")
    assert(observed.exists(r => r.detail.contains("run=1;") && r.detail.contains("degenerate=true")))
    assert(observed.exists(r => r.detail.contains("run=0;") && r.detail.contains("degenerate=false")))
    val degenerate = model.policyReceipts.filter(_.name == "observed-modulator-degenerate")
    assertEquals(degenerate.length, 1)
    assert(degenerate.head.detail.contains("run=1;"))
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
