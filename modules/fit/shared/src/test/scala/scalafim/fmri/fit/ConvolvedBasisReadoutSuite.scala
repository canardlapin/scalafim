package scalafim.fmri.fit

import scalafim.fmri.design.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class ConvolvedBasisReadoutSuite extends munit.FunSuite:
  test("effective point and window readouts transport estimates and variances through a nontrivial basis change"):
    val event = EventTerm(Vector(Event.factor(Vector("task", "task"), "condition")), Vector(0.s, 4.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(12), tr = Seq(1.0))
    val raw = event.convolve(Hrfs.SPMG2, frame)
    val scaled = event.convolve(Hrfs.SPMG2, frame, scaling = HrfColumnScaling.UnitMaximumAbsolute)
    val divisors = scaled.columnScales.map(_.divisor)
    assert(divisors.forall(_ != 1.0))
    val (transformed, receipts) = ConvolvedBasisOrthogonalization(scaled).fold(error => fail(error.message), identity)
    val transform = receipts.head.groups.head.transform
    val effective = transformed.hrfForColumn(0)
    assert(effective.descriptor != Hrfs.SPMG2.descriptor)
    val model = EventModel.build(Vector(transformed), frame)
    val original = EventModel.build(Vector(raw), frame)
    val cell = StructuralHypothesisDsl.term("task").cell(StructuralHypothesisDsl.factor("condition") === "task")
    val gamma = Vector(0.75, -0.25)
    val beta = Vector.tabulate(2)(i => (0 until 2).map(j => transform(i * 2 + j) * gamma(j) / divisors(i)).sum)
    Vector(ResponseFunctional.At(6.s), ResponseFunctional.WindowMean(4.s, 8.s)).foreach: functional =>
      val rule = functional match
        case ResponseFunctional.At(_) => FunctionalDiscretization.Exact
        case _ => FunctionalDiscretization.Trapezoid(PositiveSeconds.unsafe(0.01.s))
      val expression = cell.response(effective, functional, rule).named("effective", "effective transformed response")
      val structural = expression.toStructural.fold(error => fail(error.message), identity)
      assertEquals(structural.responseTerms.head.discretization.get.policy, rule)
      val selected = expression
        .compile(model.designSchema).fold(error => fail(error.message), identity)
      assertEquals(selected.metadata.responseFunctionals.head.discretization, structural.responseTerms.head.discretization)
      val native = cell.response(Hrfs.SPMG2, functional, rule).named("native", "native response")
        .compile(original.designSchema).fold(error => fail(error.message), identity)
      for j <- 0 until 2 do
        val expected = (0 until 2).map(i => native.weights(0, i) / divisors(i) * transform(i * 2 + j)).sum
        assertEqualsDouble(selected.weights(0, j), expected, 1e-10)
      val estimate = (0 until 2).map(j => selected.weights(0, j) * gamma(j)).sum
      val expectedEstimate = (0 until 2).map(i => native.weights(0, i) * beta(i)).sum
      assertEqualsDouble(estimate, expectedEstimate, 1e-10)
      def variance(schema: DesignSchema, weights: gale.linalg.DMat): Double =
        val contrast = ColumnContrast(schema.columnIds.zipWithIndex.map((id, i) => id -> weights(0, i)))
        ContrastDiagnostics.analyze(schema, Vector(contrast), Vector.empty).toOption.get.t.head.varianceOverSigmaSquared.get
      assertEqualsDouble(variance(model.designSchema, selected.weights), variance(original.designSchema, native.weights), 1e-10)
      val old = cell.response(Hrfs.SPMG2, functional, rule).named("old", "old basis response")
      assert(old.compile(model.designSchema).isLeft)
    assert(cell.response(effective, ResponseFunctional.WindowMean(4.s, 8.s)).named("exact", "no unregistered primitive").compile(model.designSchema).isLeft)

  private def eventModel(durations: Vector[Double], normalization: String): EventModel =
    import scalafim.fmri.design.data.*
    import scalafim.fmri.design.formula.*
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 12.0, 24.0)),
      "condition" -> Column.Strings(Vector.fill(3)("task")),
      "dur" -> Column.Doubles(durations)
    )
    val frame = SamplingFrame(blockLens = Seq(50), tr = Seq(1.0))
    val text = s"onset ~ hrf(condition, basis = spmg2, durations = dur$normalization, id = task)"
    EventModelBuilder.buildEither(EventModelBuilder.EventDesignRequest.fromText(text, data, frame).toOption.get).fold(error => fail(error.message), identity)

  test("response readouts refuse columns mixing event unit-peak scales from different durations"):
    val model = eventModel(Vector(0.0, 2.0, 6.0), ", event_normalization = \"unit-peak\"")
    val cell = StructuralHypothesisDsl.term("task").cell(StructuralHypothesisDsl.factor("condition") === "task")
    val compiled = cell.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)).named("peak", "response at 6 s").compile(model.designSchema)
    val error = compiled.left.toOption.getOrElse(fail("mixed event scales must not read out as kernel units"))
    assert(error.message.contains("event-level"), error.message)
    // Coefficient-level hypotheses remain available.
    assert(cell.coefficient(Hrfs.SPMG2, BasisRole.Canonical).named("canonical", "canonical coefficient").compile(model.designSchema).isRight)

  test("one shared event scale transports response readouts exactly to kernel units"):
    val durations = Vector.fill(3)(2.0)
    val normalized = eventModel(durations, ", event_normalization = \"unit-peak\"")
    val plain = eventModel(durations, "")
    val divisors = normalized.designSchema.audit.eventResponseScales.sortBy(_.basis.oneBased).map(_.divisors.head)
    assertEquals(divisors.length, 2)
    val cell = StructuralHypothesisDsl.term("task").cell(StructuralHypothesisDsl.factor("condition") === "task")
    val selected = cell.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)).named("peak", "response").compile(normalized.designSchema).fold(error => fail(error.message), identity)
    val native = cell.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)).named("peak", "response").compile(plain.designSchema).fold(error => fail(error.message), identity)
    for j <- 0 until 2 do
      assertEqualsDouble(selected.weights(0, j), native.weights(0, j) / divisors(j), 1e-14)

  private def twoRunModel(durations: Vector[Double], normalization: String): EventModel =
    import scalafim.fmri.design.data.*
    import scalafim.fmri.design.formula.*
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 12.0, 24.0, 0.0, 12.0, 24.0)),
      "condition" -> Column.Strings(Vector.fill(6)("task")),
      "dur" -> Column.Doubles(durations)
    )
    val frame = SamplingFrame(blockLens = Seq(50, 50), tr = Seq(1.0, 1.0))
    val text = s"onset ~ hrf(condition, basis = spmg2, durations = dur$normalization, id = task)"
    val request = EventModelBuilder.EventDesignRequest.fromText(text, data, frame, blockPlan = EventModelBuilder.BlockPlan.explicit(Seq(0, 0, 0, 1, 1, 1))).toOption.get
    EventModelBuilder.buildEither(request).fold(error => fail(error.message), identity)

  test("event-scale transport is per run: run-local readouts use their run's divisor, shared readouts need one across runs"):
    // Durations differ only between runs, so each run is uniform but the runs differ.
    val durations = Vector(2.0, 2.0, 2.0, 6.0, 6.0, 6.0)
    val normalized = twoRunModel(durations, ", event_normalization = \"unit-peak\"")
    val plain = twoRunModel(durations, "")
    val receipts = normalized.designSchema.audit.eventResponseScales.sortBy(_.basis.oneBased)
    assertEquals(receipts.length, 2)
    receipts.foreach: receipt =>
      assertEquals(receipt.runs.map(_.run.oneBased), Vector(1, 2))
      assert(receipt.runs.forall(_.divisors.length == 1))
      assertEquals(receipt.divisors.length, 2)
    val cell = StructuralHypothesisDsl.term("task").cell(StructuralHypothesisDsl.factor("condition") === "task")
    val hypothesis = cell.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)).named("peak", "response at 6 s")
    val shared = hypothesis.compile(normalized.designSchema)
    val error = shared.left.toOption.getOrElse(fail("a shared coefficient mixing run divisors must be refused"))
    assert(error.message.contains("shared across runs"), error.message)
    val structural = hypothesis.toStructural.fold(error => fail(error.message), identity)
    Vector(1, 2).foreach: oneBased =>
      val run = RunIndex.unsafeOneBased(oneBased)
      def runwise(model: EventModel) =
        val slice = model.designSchema.runwiseSlice(run).fold(error => fail(error.message), identity)
        StructuralHypothesis.compile(slice, structural, StructuralHypothesis.DefaultRankTolerance).fold(error => fail(error.message), identity)
      val selected = runwise(normalized)
      val native = runwise(plain)
      val divisors = receipts.map(_.divisorsIn(run).head)
      assert(selected.weights.cols == native.weights.cols)
      val nonZero = (0 until native.weights.cols).filter(j => native.weights(0, j) != 0.0)
      assertEquals(nonZero.length, 2)
      nonZero.zipWithIndex.foreach: (column, basis) =>
        assertEqualsDouble(selected.weights(0, column), native.weights(0, column) / divisors(basis), 1e-14)
