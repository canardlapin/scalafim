package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.fixtures.FmriAr041RFixture as Reference

class FmriAr041ParitySuite extends munit.FunSuite:
  private def value[A](v: Either[ArError, A]): A = v.fold(e => fail(e.message), identity)
  private def matrix(rows: Vector[Vector[Double]]): DMat = Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))
  private def close(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.shape, expected.shape)
    actual.valuesRowMajor.zip(expected.valuesRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-10))

  test("standard BIC matches the corrected reference on the over-selection regression"):
    val input = Matrix.tabulate(Reference.bicSeries.length, 1)((r, _) => Reference.bicSeries(r))
    val plan = value(ArEstimation.fitNoise(input, TimeSegments.continuous(input.rows), ArFitOptions(order = ArOrder.Auto(6))))
    assertEquals(plan.arOrder, Reference.bicOrder)
    plan.coefficients.head.phi.zip(Reference.bicPhi).foreach((a, e) => assertEqualsDouble(a, e, 1e-10))

  test("mean and median aggregate per-voxel ACFs and run normalization matches R"):
    val input = matrix(Reference.diagnosticRows)
    val mean = AcorrDiagnostics.compute(input, 3, AcfAggregation.Mean)
    val median = AcorrDiagnostics.compute(input, 3, AcfAggregation.Median)
    close(mean.acf, Matrix.tabulate(3, 1)((r, _) => Reference.diagnosticMean(r)))
    close(median.acf, Matrix.tabulate(3, 1)((r, _) => Reference.diagnosticMedian(r)))
    close(AcorrDiagnostics.compute(input, 3, AcfAggregation.None).acf, matrix(Reference.diagnosticNone))
    close(value(AcorrDiagnostics.compute(input, TimeSegments.fromRunLengths(Vector(40, 40)), 3, AcfAggregation.None)).acf, matrix(Reference.diagnosticRunNone))

  test("constant columns are omitted from aggregate diagnostics"):
    val data = matrix(Reference.diagnosticRows)
    val augmented = Matrix.tabulate(data.rows, data.cols + 1)((r, c) => if c == data.cols then 4.0 else data(r, c))
    close(AcorrDiagnostics.compute(augmented, 3).acf, AcorrDiagnostics.compute(data, 3).acf)

  Reference.whiteningCases.foreach: c =>
    test(s"stationary ${c.name} whitening and adjoint match dense Cholesky across short segments"):
      val segments = TimeSegments.fromRunLengths(c.lengths)
      val plan = value(WhiteningPlan.withScope(CoefficientScope.Global(ArmaCoefficients(c.phi, c.theta)), segments, InitialConditionPolicy.Stationary))
      val identity = Matrix.tabulate(plan.nTimepoints, plan.nTimepoints)((r, col) => if r == col then 1.0 else 0.0)
      val actual = value(WhiteningTransform.matrix(plan, identity))
      val expected = matrix(c.operator)
      close(actual, expected)
      close(value(WhiteningTransform.transposeMatrix(plan, identity)), expected.t)
      close(actual * matrix(c.covariance) * actual.t, identity)
      val input = Matrix.tabulate(plan.nTimepoints, 2)((r, col) => math.sin(0.7 * r + col))
      val before = input.valuesRowMajor.toVector
      close(value(WhiteningTransform.matrix(plan, input)), expected * input)
      close(value(WhiteningTransform.transposeMatrix(plan, input)), expected.t * input)
      assertEquals(input.valuesRowMajor.toVector, before)
      val combined = value(WhiteningTransform(plan, input, input))
      close(combined.design, expected * input)
      close(combined.response, expected * input)

  test("stationary policy preserves per-run filter scope and refuses invalid rows"):
    val coefficients = Vector(ArmaCoefficients.ar(0.7), ArmaCoefficients.ar(0.3, -0.1))
    val segments = TimeSegments.fromRunLengths(Vector(3, 5))
    val plan = value(WhiteningPlan.withScope(CoefficientScope.ByRun(coefficients), segments, InitialConditionPolicy.Stationary))
    assert(WhiteningTransform.matrix(plan, Matrix.zeros(7, 1)).isLeft)
    assert(InitialConditionPolicy.Stationary.firstScale(coefficients.head).left.toOption.contains(ArError.StationaryWhiteningRequiresFactor))
    val input = Matrix.tabulate(8, 1)((r, _) => r.toDouble)
    val full = value(WhiteningTransform.matrix(plan, input))
    coefficients.zipWithIndex.foreach: (c, run) =>
      val length = if run == 0 then 3 else 5
      val offset = if run == 0 then 0 else 3
      val localPlan = value(WhiteningPlan.withScope(CoefficientScope.Global(c), TimeSegments.continuous(length), InitialConditionPolicy.Stationary))
      val local = value(WhiteningTransform.matrix(localPlan, Matrix.tabulate(length, 1)((r, _) => input(offset + r, 0))))
      (0 until length).foreach(r => assertEqualsDouble(full(offset + r, 0), local(r, 0), 1e-12))
