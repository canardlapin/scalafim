package scalafim.multivar

import gale.linalg.{DMat, Matrix}

class ConstrainedCanonicalSuite extends munit.FunSuite:
  test("nonnegative canonical root is a distinct coordinate-constrained estimand"):
    val effect = outer(2.0, -1.0)
    val constrained = problem(effect, DMat.eye(2)).fit.toOption.get
    val ordinary = ordinaryProblem(effect, DMat.eye(2)).fit.toOption.get

    assertEqualsDouble(ordinary.root.value, 5.0, 1e-9)
    assertEqualsDouble(constrained.root.value, 4.0, 1e-9)
    assertEqualsDouble(constrained.direction(0), 1.0, 1e-9)
    assertEqualsDouble(constrained.direction(1), 0.0, 1e-9)
    assertEquals(constrained.constraint, CanonicalFrameConstraint.Nonnegative)
    assertEquals(constrained.gauge, ConstrainedCanonicalGauge.CoordinateIdentified)
    assertEquals(constrained.programFit.program.resultSemantics.requestedClaim, RequestedOptimizationClaim.Stationary)
    assert(
      constrained.programFit.achievedGuarantee.isInstanceOf[AchievedOptimizationGuarantee.Stationary]
    )
    assertEquals(constrained.programFit.program.constraints.map(_.feasibleSet), Vector(FeasibleSetKind.NonnegativeOrthant))

  test("inactive nonnegative constraint recovers the ordinary positive solution"):
    val effect = outer(2.0, 1.0)
    val constrained = problem(effect, DMat.eye(2)).fit.toOption.get
    val ordinary = ordinaryProblem(effect, DMat.eye(2)).fit.toOption.get

    assertEqualsDouble(constrained.root.value, ordinary.root.value, 1e-8)
    assert(constrained.direction(0) > 0.0)
    assert(constrained.direction(1) > 0.0)
    assertEqualsDouble(quadratic(constrained.direction, DMat.eye(2)), 1.0, 1e-9)
    assert(constrained.diagnostics.stationarityResidual <= 1e-8)
    assert(constrained.diagnostics.constraintViolation <= 1e-12)
    assert(constrained.diagnostics.normalizationError <= 1e-9)

  test("coordinate permutations preserve the constrained root and permute the frame"):
    val first = problem(outer(3.0, -2.0), diagonal(2.0, 1.0)).fit.toOption.get
    val second = problem(outer(-2.0, 3.0), diagonal(1.0, 2.0)).fit.toOption.get

    assertEqualsDouble(first.root.value, second.root.value, 1e-9)
    assertEqualsDouble(first.direction(0), second.direction(1), 1e-9)
    assertEqualsDouble(first.direction(1), second.direction(0), 1e-9)

  test("solver specification rejects invalid tolerance and iteration budgets"):
    assert(ConstrainedCanonicalSolverSpec.from(0.0, 10).isLeft)
    assert(ConstrainedCanonicalSolverSpec.from(1e-8, 0).isLeft)

  private def problem(
      effect: DMat,
      residual: DMat
  ): ConstrainedCanonicalProblem[? <: SemanticSpace] =
    val space = SpaceRef.of("constrained-canonical-test", SpaceRole.Observed, effect.rows).toOption.get
    ConstrainedCanonicalProblem
      .fromDense(space.evidence, effect, residual, ResidualRegularization.Unregularized)
      .toOption
      .get

  private def ordinaryProblem(
      effect: DMat,
      residual: DMat
  ): CanonicalEffectProblem[? <: SemanticSpace] =
    val space = SpaceRef.of("ordinary-canonical-test", SpaceRole.Observed, effect.rows).toOption.get
    CanonicalEffectProblem
      .fromDense(space.evidence, effect, residual, ResidualRegularization.Unregularized)
      .toOption
      .get

  private def outer(left: Double, right: Double): DMat =
    matrix(Vector(Vector(left * left, left * right), Vector(left * right, right * right)))

  private def diagonal(first: Double, second: Double): DMat =
    matrix(Vector(Vector(first, 0.0), Vector(0.0, second)))

  private def quadratic(value: gale.linalg.DVec, matrix: DMat): Double =
    var result = 0.0
    var row = 0
    while row < value.length do
      var column = 0
      while column < value.length do
        result += value(row) * matrix(row, column) * value(column)
        column += 1
      row += 1
    result

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    val out = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var column = 0
      while column < rows.head.length do
        out(row, column) = rows(row)(column)
        column += 1
      row += 1
    out.result()
