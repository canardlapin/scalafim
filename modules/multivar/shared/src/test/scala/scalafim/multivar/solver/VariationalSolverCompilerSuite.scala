package scalafim.multivar
package solver

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import scalafim.linalg.FirstOrderCapabilities
import scalafim.linalg.FirstOrderMethod
import scalafim.linalg.GaleMatrixBridge
import scalafim.linalg.SolverMethodRequest

class VariationalSolverCompilerSuite extends munit.FunSuite:

  test("execution-form matching is exhaustive and deterministic"):
    val expected = Vector(
      VariationalExecutionForm.SmoothSeparableProximal -> FirstOrderMethod.ProximalGradient,
      VariationalExecutionForm.SmoothProjection -> FirstOrderMethod.ProjectedGradient,
      VariationalExecutionForm.SmoothLinearComposite -> FirstOrderMethod.SmoothCompositePrimalDual,
      VariationalExecutionForm.LinearComposite -> FirstOrderMethod.LinearCompositePrimalDual,
      VariationalExecutionForm.ExactNullSpace -> FirstOrderMethod.ExactLinearReduction
    )

    expected.foreach: (form, method) =>
      val selected = VariationalSolverCompiler
        .select(form, SolverMethodRequest.Automatic, FirstOrderCapabilities.portable)
        .toOption
        .get
      assertEquals(selected, VariationalSolverSelection(form, method))

  test("missing numerical capability fails before the semantic operator is evaluated"):
    val fixture = matrixFreeFixture("missing-numerical-capability")
    val capabilities = FirstOrderCapabilities
      .from(Set(FirstOrderMethod.ProximalGradient))
      .toOption
      .get
    val result = VariationalSolverCompiler.compileL1(
      fixture.plan,
      fixture.observation,
      capabilities = capabilities
    )

    assertEquals(fixture.operator.applications, 0)
    assert(result.left.toOption.exists:
      case CompositeLoweringError.SolverBoundary(error) =>
        error.isInstanceOf[scalafim.linalg.FirstOrderError.MissingCapability]
      case _ => false
    )

  test("compiled matrix-free programs execute through linalg and bind numerical evidence"):
    val fixture = matrixFreeFixture("matrix-free-execution")
    val compiled = VariationalSolverCompiler
      .compileL1(fixture.plan, fixture.observation)
      .toOption
      .get
    val solution = compiled.solve().toOption.get
    val expected = GaleNumerics.matrixFromRows(Vector(Vector(0.5), Vector(-1.75)))

    assertMatrixClose(solution.parameter, expected, 1e-6)
    assertEquals(fixture.plan.targetOperator.representation, OperatorRepresentation.MatrixFree)
    assertEquals(compiled.selection.method, FirstOrderMethod.LinearCompositePrimalDual)
    assert(solution.numericalCertificate.binds(
      GaleMatrixBridge.fromGale(solution.parameter),
      Some(GaleMatrixBridge.fromGale(solution.dual))
    ))
    assertEquals(
      solution.numericalCertificate.settings.method,
      FirstOrderMethod.LinearCompositePrimalDual
    )
    assert(compiled.provenance.events.exists:
      case SemanticProvenanceEvent.Derived("compile-generic-linear-composite", inputs) =>
        inputs == Vector(fixture.plan.targetOperator.valueIdentity)
      case _ => false
    )
    assert(fixture.operator.applications > 0)

  private final class CountingDiagonal(values: Vector[Double]) extends DoubleLinearOperator:
    var applications: Int = 0
    val rows: Int = values.length
    val cols: Int = values.length

    def applyTo(input: DVec, output: MutableDVec): Unit =
      applications += 1
      var index = 0
      while index < values.length do
        output(index) = values(index) * input(index)
        index += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      applyTo(input, output)

  private final class MatrixFreeFixture(
      val operator: CountingDiagonal,
      val plan: CompositePenaltyPlan[?, ?],
      val observation: DMat
  )

  private def matrixFreeFixture(name: String): MatrixFreeFixture =
    val feature = SpaceRef(MvSpace.of(s"$name-feature", SpaceRole.Observed, 2).toOption.get)
    val target = SpaceRef(MvSpace.of(s"$name-target", SpaceRole.Latent, 2).toOption.get)
    val numerical = new CountingDiagonal(Vector(2.0, 1.0))
    val semantic = Op
      .fromLinearMap(
        numerical,
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(target.evidence),
        OperatorRoleWitness.cross,
        ValueIdentity.source(ValueId.unsafe(s"$name-operator"))
      )
      .toOption
      .get
    val parameter = ParameterId.unsafe(s"$name-parameter")
    val targetExpression = TargetExpression
      .linear(parameter, s"$name-map", semantic)
      .toOption
      .get
    val penalty = PenaltyTerm(
      targetExpression,
      FunctionalKind.L1,
      PenaltyWeight.unsafe(0.25)
    )
    val plan = CompositePenaltyPlan
      .from(
        penalty,
        semantic,
        AuxiliaryVariableId.unsafe(s"$name-auxiliary"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
      .toOption
      .get
    new MatrixFreeFixture(
      numerical,
      plan,
      GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(-2.0)))
    )

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1
