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
