package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.Matrix

class CanonicalEffectSuite extends munit.FunSuite:

  test("trace ridge fractions reject zero and non-finite values at construction"):
    Vector(0.0, -0.1, Double.NaN, Double.PositiveInfinity).foreach: value =>
      TraceRidgeFraction(value) match
        case Left(MultivarError.InvalidRegularization("trace ridge fraction", actual, _)) =>
          if value.isNaN then assert(actual.isNaN) else assertEqualsDouble(actual, value, 0.0)
        case other => fail(s"expected invalid regularization for $value, got $other")

  test("analytic rank-one effect returns the nonnegative generalized root and B-normalized direction"):
    val feature = featureSpace("canonical-analytic", 2)
    val effect = matrix(Vector(Vector(4.0, 2.0), Vector(2.0, 1.0)))
    val residual = matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 3.0)))
    val fit = fitted(feature, effect, residual, ResidualRegularization.Unregularized)

    assertEqualsDouble(fit.root.value, 7.0 / 3.0, 1e-10)
    assertEqualsDouble(fit.root.correlation, Math.sqrt(0.7), 1e-10)
    fit.solution match
      case CanonicalEffectSolution.Simple(direction, DirectionOrientation.LargestMagnitudePositive(anchor, _)) =>
        assertEquals(anchor, 0)
        assert(direction(0) > 0.0)
        assertEqualsDouble(direction(1) / direction(0), 1.0 / 3.0, 1e-10)
        assertEqualsDouble(quadratic(direction, residual), 1.0, 1e-10)
        val frame = fit.functionalFrame.weights.toDense.fold(error => fail(error.message), value => value)
        assertEqualsDouble(frame(0, 0), direction(0), 0.0)
        assertEqualsDouble(frame(1, 0), direction(1), 0.0)
      case other => fail(s"expected a simple leading direction, got $other")
    assertEquals(fit.programFit.program.objective.label, "generalized-rayleigh")
    assertEqualsDouble(fit.programFit.objectiveValue, fit.root.value, 0.0)

  test("dense method agrees with a directly checkable diagonal generalized eigenproblem"):
    val feature = featureSpace("canonical-diagonal", 2)
    val fit = fitted(
      feature,
      matrix(Vector(Vector(9.0, 0.0), Vector(0.0, 2.0))),
      matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 2.0))),
      ResidualRegularization.Unregularized
    )

    assertEqualsDouble(fit.root.value, 3.0, 1e-12)
    fit.solution match
      case CanonicalEffectSolution.Simple(direction, _) =>
        assertEqualsDouble(direction(0), 1.0 / Math.sqrt(3.0), 1e-12)
        assertEqualsDouble(direction(1), 0.0, 1e-12)
      case other => fail(s"expected a simple leading direction, got $other")

  test("common scaling leaves the trace-ridge root and oriented direction unchanged"):
    val feature = featureSpace("canonical-scale", 2)
    val ridge = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.2))
    val effect = matrix(Vector(Vector(4.0, 1.0), Vector(1.0, 2.0)))
    val residual = matrix(Vector(Vector(3.0, 0.5), Vector(0.5, 2.0)))
    val original = fitted(feature, effect, residual, ridge)
    val scaled = fitted(feature, scale(effect, 17.0), scale(residual, 17.0), ridge)

    assertEqualsDouble(scaled.root.value, original.root.value, 1e-10)
    assertEqualsDouble(scaled.regularization.ridgeAmount, 17.0 * original.regularization.ridgeAmount, 1e-10)
    assertVectorClose(simpleDirection(scaled.solution), scale(simpleDirection(original.solution), 1.0 / Math.sqrt(17.0)), 1e-9)

  test("orthogonal feature-basis changes transport the canonical direction covariantly"):
    val feature = featureSpace("canonical-rotation", 2)
    val effect = matrix(Vector(Vector(4.0, 1.0), Vector(1.0, 1.0)))
    val residual = matrix(Vector(Vector(2.0, 0.25), Vector(0.25, 1.5)))
    val q = matrix(Vector(Vector(0.0, -1.0), Vector(1.0, 0.0)))
    val original = fitted(feature, effect, residual, ResidualRegularization.Unregularized)
    val rotated = fitted(feature, similarity(q, effect), similarity(q, residual), ResidualRegularization.Unregularized)
    val expected = multiply(transpose(q), simpleDirection(original.solution))

    assertEqualsDouble(rotated.root.value, original.root.value, 1e-10)
    assertVectorCloseUpToSign(simpleDirection(rotated.solution), expected, 1e-9)

  test("a repeated leading root returns an invariant subspace rather than an arbitrary direction"):
    val feature = featureSpace("canonical-repeated", 2)
    val fit = fitted(
      feature,
      matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 2.0))),
      DMat.eye(2),
      ResidualRegularization.Unregularized
    )

    assertEqualsDouble(fit.root.value, 2.0, 1e-12)
    assertEquals(fit.diagnostics.leadingMultiplicity, 2)
    assert(fit.diagnostics.eigengap.isPosInfinity)
    fit.solution match
      case CanonicalEffectSolution.LeadingSubspace(basis, projector, multiplicity) =>
        assertEquals(multiplicity, 2)
        assertEquals(basis.rows, 2)
        assertMatrixClose(projector, DMat.eye(2), 1e-12)
      case other => fail(s"expected a leading invariant subspace, got $other")

  test("an unregularized non-SPD residual fails through the typed Gale adapter"):
    val feature = featureSpace("canonical-non-spd", 2)
    val problem = accepted(
      CanonicalEffectProblem.fromDense(
        feature.evidence,
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.0))),
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.0))),
        ResidualRegularization.Unregularized
      )
    )

    problem.fit match
      case Left(MultivarError.NonInvertibleValue("positive-definite residual geometry", _, _)) => ()
      case other => fail(s"expected a typed non-positive-definite failure, got $other")

  test("trace ridge repairs a rank-deficient residual and records rank, condition, and Gale residual"):
    val feature = featureSpace("canonical-rank-deficient", 2)
    val fit = fitted(
      feature,
      matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.0))),
      matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 0.0))),
      ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.1))
    )

    assertEquals(fit.diagnostics.effectRank, 1)
    assertEquals(fit.diagnostics.residualRank, 1)
    assertEqualsDouble(fit.regularization.traceScale, 1.0, 1e-12)
    assertEqualsDouble(fit.regularization.ridgeAmount, 0.1, 1e-12)
    assert(fit.diagnostics.regularizedResidualCondition.isFinite)
    assert(fit.diagnostics.generalizedResidual <= 1e-9)
    assert(fit.diagnostics.bOrthonormalityError <= 1e-9)
    assertEquals(fit.provenance.solver, "gale.spectral.Eigen.eigSymmetricGeneralized")

  test("non-PSD effect matrices are rejected at the certified construction boundary"):
    val feature = featureSpace("canonical-indefinite", 2)
    CanonicalEffectProblem.fromDense(
      feature.evidence,
      matrix(Vector(Vector(1.0, 0.0), Vector(0.0, -1.0))),
      DMat.eye(2),
      ResidualRegularization.Unregularized
    ) match
      case Left(MultivarError.SolverFailed(detail)) => assert(detail.contains("psd certificate rejected"))
      case other => fail(s"expected a PSD certificate failure, got $other")

  private def fitted(
      feature: SpaceRef,
      effect: DMat,
      residual: DMat,
      regularization: ResidualRegularization
  ): CanonicalEffectFit[feature.Id, ? <: SemanticSpace] =
    accepted(CanonicalEffectProblem.fromDense(feature.evidence, effect, residual, regularization)).fit match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def featureSpace(id: String, dimension: Int): SpaceRef =
    SpaceRef.of(id, SpaceRole.Observed, dimension).fold(error => fail(error.message), value => value)

  private def simpleDirection(solution: CanonicalEffectSolution): DVec =
    solution match
      case CanonicalEffectSolution.Simple(direction, _) => direction
      case other => fail(s"expected simple direction, got $other")

  private def quadratic(vector: DVec, matrix: DMat): Double =
    var result = 0.0
    var row = 0
    while row < vector.length do
      var col = 0
      while col < vector.length do
        result += vector(row) * matrix(row, col) * vector(col)
        col += 1
      row += 1
    result

  private def similarity(q: DMat, value: DMat): DMat =
    multiply(transpose(q), multiply(value, q))

  private def multiply(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, right.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < right.cols do
        var value = 0.0
        var index = 0
        while index < left.cols do
          value += left(row, index) * right(index, col)
          index += 1
        out(row, col) = value
        col += 1
      row += 1
    out.result()

  private def multiply(matrix: DMat, vector: DVec): DVec =
    val values = Array.ofDim[Double](matrix.rows)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        values(row) += matrix(row, col) * vector(col)
        col += 1
      row += 1
    GaleNumerics.vectorFromArray(values)

  private def transpose(matrix: DMat): DMat =
    val out = Matrix.newBuilder(matrix.cols, matrix.rows)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(col, row) = matrix(row, col)
        col += 1
      row += 1
    out.result()

  private def scale(matrix: DMat, factor: Double): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = factor * matrix(row, col)
        col += 1
      row += 1
    out.result()

  private def scale(vector: DVec, factor: Double): DVec =
    GaleNumerics.vectorFromArray(Array.tabulate(vector.length)(index => factor * vector(index)))

  private def assertVectorClose(actual: DVec, expected: DVec, tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1

  private def assertVectorCloseUpToSign(actual: DVec, expected: DVec, tolerance: Double): Unit =
    val sign = if dot(actual, expected) < 0.0 then -1.0 else 1.0
    assertVectorClose(actual, scale(expected, sign), tolerance)

  private def dot(left: DVec, right: DVec): Double =
    var result = 0.0
    var index = 0
    while index < left.length do
      result += left(index) * right(index)
      index += 1
    result

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def accepted[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), value => value)
