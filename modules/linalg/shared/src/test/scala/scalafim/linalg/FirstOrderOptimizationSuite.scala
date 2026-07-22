package scalafim.linalg

class FirstOrderOptimizationSuite extends munit.FunSuite:

  test("capability selection is deterministic and fails before numerical execution"):
    val capabilities = FirstOrderCapabilities
      .from(Set(FirstOrderMethod.ProjectedGradient, FirstOrderMethod.ProximalGradient))
      .toOption
      .get
    val selected = capabilities.select(
      Vector(FirstOrderMethod.ProjectedGradient, FirstOrderMethod.ProximalGradient),
      SolverMethodRequest.Automatic
    )
    val missing = capabilities.select(
      Vector(FirstOrderMethod.LinearCompositePrimalDual),
      SolverMethodRequest.Require(FirstOrderMethod.LinearCompositePrimalDual)
    )

    assertEquals(selected, Right(FirstOrderMethod.ProximalGradient))
    assert(missing.left.toOption.exists(_.isInstanceOf[FirstOrderError.MissingCapability]))

  test("proximal gradient matches the separable quadratic plus l1 oracle"):
    val center = matrix(Vector(Vector(1.0), Vector(-2.0)))
    val smooth = quadratic(center)
    val l1 = l1Term(2, 0.25)
    val solution = FirstOrderSolvers
      .proximalGradient(smooth, l1, DoubleMatrix.zeros(2, 1))
      .toOption
      .get

    assertMatrixClose(solution.primal, matrix(Vector(Vector(0.75), Vector(-1.75))), 1e-7)
    assertEquals(solution.status, FirstOrderStoppingStatus.Converged)
    assertEquals(solution.certificate.settings.method, FirstOrderMethod.ProximalGradient)
    assert(solution.certificate.binds(solution.primal, solution.dual))
    assert(!solution.certificate.binds(solution.primal.updated(0, 0, 0.0), solution.dual))

  test("projected gradient matches a box-constrained quadratic oracle"):
    val center = matrix(Vector(Vector(2.0), Vector(-3.0)))
    val box = new ProjectionSet:
      val variableRows = 2
      def project(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
        Right(map(at)(value => Math.max(-1.0, Math.min(1.0, value))))
    val solution = FirstOrderSolvers
      .projectedGradient(quadratic(center), box, DoubleMatrix.zeros(2, 1))
      .toOption
      .get

    assertMatrixClose(solution.primal, matrix(Vector(Vector(1.0), Vector(-1.0))), 1e-8)
    assertEquals(solution.status, FirstOrderStoppingStatus.Converged)
    assert(solution.certificate.primalResidual <= 1e-8)

  test("linear-composite primal-dual matches an independent diagonal lasso oracle"):
    val center = matrix(Vector(Vector(1.0), Vector(-2.0)))
    val diagonal = matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0)))
    val operator = BoundedLinearMap.from(new DenseMap(diagonal), Math.sqrt(5.0)).toOption.get
    val solution = FirstOrderSolvers
      .linearCompositePrimalDual(
        proximalQuadratic(center),
        l1Functional(2, 0.25),
        operator,
        DoubleMatrix.zeros(2, 1)
      )
      .toOption
      .get

    assertMatrixClose(solution.primal, matrix(Vector(Vector(0.5), Vector(-1.75))), 1e-6)
    assertEquals(solution.status, FirstOrderStoppingStatus.Converged)
    assert(solution.certificate.primalResidual <= 1e-7)
    assert(solution.certificate.dualResidual <= 1e-7)
    assertEquals(solution.certificate.settings.method, FirstOrderMethod.LinearCompositePrimalDual)

  test("smooth composite primal-dual matches an independent sparse smooth TV oracle"):
    val center = matrix(Vector(Vector(2.0), Vector(0.0)))
    val difference = matrix(Vector(Vector(1.0, -1.0)))
    val operator = BoundedLinearMap.from(new DenseMap(difference), Math.sqrt(2.0)).toOption.get
    val solution = FirstOrderSolvers
      .smoothCompositePrimalDual(
        coupledQuadratic(center, smoothnessWeight = 1.0),
        l1Term(2, weight = 0.25),
        l1Functional(1, weight = 0.25),
        operator,
        DoubleMatrix.zeros(2, 1)
      )
      .toOption
      .get

    assertMatrixClose(solution.primal, matrix(Vector(Vector(1.0), Vector(0.5))), 2e-6)
    assertEquals(solution.status, FirstOrderStoppingStatus.Converged)
    assert(solution.certificate.primalResidual <= 2e-7)
    assert(solution.certificate.dualResidual <= 2e-7)
    assertEquals(solution.certificate.settings.method, FirstOrderMethod.SmoothCompositePrimalDual)

  test("exact linear reduction verifies the declared null-space image"):
    val basis = new DenseMap(matrix(Vector(Vector(1.0), Vector(1.0))))
    val constraint = new DenseMap(matrix(Vector(Vector(1.0, -1.0))))
    val badConstraint = new DenseMap(matrix(Vector(Vector(1.0, 1.0))))
    val proof = ExactLinearReduction.verify(basis, constraint, FirstOrderTolerance.strict).toOption.get

    assertEquals(proof.freeRows, 1)
    assertEquals(proof.semanticRows, 2)
    assertEquals(proof.constraintRows, 1)
    assertEqualsDouble(proof.residual, 0.0, 0.0)
    assert(ExactLinearReduction.verify(basis, badConstraint, FirstOrderTolerance.strict).isLeft)

  test("oracle shape and non-finite failures are typed"):
    val center = matrix(Vector(Vector(1.0), Vector(-2.0)))
    val bad = new ProximalTerm:
      val variableRows = 2
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] = Right(Double.NaN)
      def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] = Right(at)

    FirstOrderSolvers.proximalGradient(quadratic(center), bad, DoubleMatrix.zeros(2, 1)) match
      case Left(FirstOrderError.NonFiniteValue("proximal term", 0, value)) => assert(value.isNaN)
      case other => fail(s"expected typed non-finite oracle failure, got $other")

  private final class DenseMap(value: DoubleMatrix) extends LinearMap:
    val rows: Int = value.rows
    val cols: Int = value.cols
    def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
      if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
      else Right(DoubleMatrix.multiply(value, input))
    def adjoint: LinearMap = new DenseMap(value.transpose)

  private def quadratic(center: DoubleMatrix): SmoothObjective =
    new SmoothObjective:
      val variableRows: Int = center.rows
      val lipschitz: Double = 1.0
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
        Right(0.5 * squaredNorm(subtract(at, center)))
      def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
        Right(subtract(at, center))

  private def proximalQuadratic(center: DoubleMatrix): ProximalObjective =
    new ProximalObjective:
      val variableRows: Int = center.rows
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
        Right(0.5 * squaredNorm(subtract(at, center)))
      def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
        Right(scale(add(at, scale(center, step)), 1.0 / (1.0 + step)))

  private def coupledQuadratic(center: DoubleMatrix, smoothnessWeight: Double): SmoothObjective =
    new SmoothObjective:
      val variableRows: Int = 2
      val lipschitz: Double = 1.0 + 2.0 * smoothnessWeight
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
        val difference = at(0, 0) - at(1, 0)
        Right(0.5 * squaredNorm(subtract(at, center)) + 0.5 * smoothnessWeight * difference * difference)
      def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
        val difference = at(0, 0) - at(1, 0)
        Right(
          matrix(
            Vector(
              Vector(at(0, 0) - center(0, 0) + smoothnessWeight * difference),
              Vector(at(1, 0) - center(1, 0) - smoothnessWeight * difference)
            )
          )
        )

  private def l1Term(rows: Int, weight: Double): ProximalTerm =
    new ProximalTerm:
      val variableRows: Int = rows
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] = Right(weight * l1(at))
      def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
        Right(map(at): value =>
          Math.signum(value) * Math.max(0.0, Math.abs(value) - weight * step)
        )

  private def l1Functional(rows: Int, weight: Double): LinearCompositeFunctional =
    new LinearCompositeFunctional:
      val targetRows: Int = rows
      def value(at: DoubleMatrix): Either[FirstOrderError, Double] = Right(weight * l1(at))
      def proximalConjugate(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
        Right(map(at)(value => Math.max(-weight, Math.min(weight, value))))

  private def matrix(rows: Vector[Vector[Double]]): DoubleMatrix =
    DoubleMatrix.fromRows(rows)

  private def map(value: DoubleMatrix)(function: Double => Double): DoubleMatrix =
    val output = value.copyData
    var index = 0
    while index < output.length do
      output(index) = function(output(index))
      index += 1
    DoubleMatrix.fromRowMajor(value.shape, output).toOption.get

  private def add(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    val output = left.copyData
    val rightData = right.copyData
    var index = 0
    while index < output.length do
      output(index) += rightData(index)
      index += 1
    DoubleMatrix.fromRowMajor(left.shape, output).toOption.get

  private def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    add(left, scale(right, -1.0))

  private def scale(value: DoubleMatrix, factor: Double): DoubleMatrix =
    map(value)(_ * factor)

  private def squaredNorm(value: DoubleMatrix): Double =
    value.copyData.foldLeft(0.0)((result, current) => result + current * current)

  private def l1(value: DoubleMatrix): Double =
    value.copyData.foldLeft(0.0)((result, current) => result + Math.abs(current))

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1
