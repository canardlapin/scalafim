package scalafim.multivar
package optimization

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}

class QuadraticRegularizationSuite extends munit.FunSuite:

  test("linear-quadratic pullback equals the independent dense T-star G T oracle"):
    val fixture = graphFixture()
    val lowered = accepted(fixture.lower(QuadraticPlacement.ObjectiveRidge))
    val actual = acceptedSemantic(lowered.pulledBack.toDense)
    val expected = fixture.incidenceDense.t * fixture.geometryDense * fixture.incidenceDense

    assertMatrixClose(actual, expected, 1e-12)
    assertEquals(lowered.pulledBack.certificate.status, EvidenceStatus.Certified)
    assert(lowered.proof.exact)
    assertEquals(lowered.proof.outputEvidence, EvidenceStatus.Certified)

  test("quadratic gradient agrees with a central-difference oracle"):
    val fixture = graphFixture()
    val lowered = accepted(fixture.lower(QuadraticPlacement.ObjectiveRidge))
    val laplacian = acceptedSemantic(lowered.pulledBack.toDense)
    val point = matrix(Vector(Vector(1.0), Vector(2.0), Vector(-1.0)))
    val direction = matrix(Vector(Vector(0.5), Vector(-0.25), Vector(0.75)))
    val epsilon = 1e-6
    val plus = quadratic(add(point, scale(direction, epsilon)), laplacian)
    val minus = quadratic(add(point, scale(direction, -epsilon)), laplacian)
    val finiteDifference = (plus - minus) / (2.0 * epsilon)
    val analytic = 2.0 * inner(direction, laplacian * point)

    assertEqualsDouble(finiteDifference, analytic, 1e-7)

  test("graph Laplacian is symmetric PSD and annihilates the constant null space"):
    val fixture = graphFixture()
    val laplacian = acceptedSemantic(accepted(fixture.lower(QuadraticPlacement.ObjectiveRidge)).pulledBack.toDense)
    val ones = matrix(Vector(Vector(1.0), Vector(1.0), Vector(1.0)))
    val result = laplacian * ones

    assertMatrixClose(laplacian, laplacian.t, 1e-12)
    assertMatrixClose(result, matrix(Vector(Vector(0.0), Vector(0.0), Vector(0.0))), 1e-12)
    assertEqualsDouble(quadratic(matrix(Vector(Vector(1.0), Vector(-2.0), Vector(0.5))), laplacian), 15.25, 1e-12)

  test("objective ridge and denominator loading produce observably distinct effective operators"):
    val fixture = graphFixture()
    val objective = accepted(fixture.lower(QuadraticPlacement.ObjectiveRidge))
    val denominator = accepted(fixture.lower(QuadraticPlacement.DenominatorLoading))
    val base = acceptedSemantic(
      Op.fromDense(
        diagonal(Vector(4.0, 3.0, 2.0)),
        CoordinateEvidence.dual(fixture.feature.evidence),
        CoordinateEvidence.primal(fixture.feature.evidence),
        OperatorRoleWitness.covariance,
        id("quadratic-base")
      )
    )
    val objectiveDense = acceptedSemantic(QuadraticPullback.effective(base, objective).toDense)
    val denominatorDense = acceptedSemantic(QuadraticPullback.effective(base, denominator).toDense)
    val laplacian = fixture.incidenceDense.t * fixture.geometryDense * fixture.incidenceDense

    assertMatrixClose(
      objectiveDense,
      MatrixOps.subtract(diagonal(Vector(4.0, 3.0, 2.0)), MatrixOps.scale(laplacian, 0.5)),
      1e-12
    )
    assertMatrixClose(
      denominatorDense,
      MatrixOps.subtract(diagonal(Vector(4.0, 3.0, 2.0)), MatrixOps.scale(laplacian, -0.5)),
      1e-12
    )
    assert(objectiveDense != denominatorDense)

  test("matrix-free targets remain lazy through pullback and effective assembly"):
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("matrix-free-feature"), SpaceRole.Observed, Dimension.unsafe(3)))
    val target = SpaceRef(MvSpace(SpaceId.unsafe("matrix-free-target"), SpaceRole.Observed, Dimension.unsafe(2)))
    type F = feature.Id
    type Z = target.Id
    val counter = new ApplyCounter
    val operator = CountingOperator(
      matrix(Vector(Vector(-1.0, 1.0, 0.0), Vector(0.0, -1.0, 1.0))),
      counter
    )
    val map = acceptedSemantic(
      Op.fromLinearMap(
        operator,
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(target.evidence),
        OperatorRoleWitness.cross,
        id("matrix-free-target-map")
      )
    )
    val geometry = certifiedMetric(target.evidence, DMat.eye(2), "matrix-free-geometry")
    val term = PenaltyTerm(
      acceptedProgram(TargetExpression.linear(ParameterId.unsafe("w"), "matrix-free-target", map)),
      FunctionalKind.SquaredNorm(geometry.valueIdentity),
      PenaltyWeight.unsafe(1.0)
    )
    val lowered = accepted(
      QuadraticPullback.lower(term, map, geometry, QuadraticFamily.Tikhonov, QuadraticPlacement.ObjectiveRidge)
    )

    assertEquals(counter.applications, 0)
    assertEquals(lowered.pulledBack.representation, OperatorRepresentation.MatrixFree)
    acceptedSemantic(lowered.pulledBack(matrix(Vector(Vector(1.0), Vector(0.0), Vector(-1.0)))))
    assert(counter.applications > 0)

  test("shared squared-norm identity does not relax geometry-bound lowering"):
    val fixture = graphFixture()
    val foreignGeometry = id("foreign-quadratic-geometry")
    val term = fixture.term.copy(functional = FunctionalKind.SquaredNorm(foreignGeometry))

    assertEquals(term.functionalIdentity, PenaltyFunctionalIdentity.SquaredNorm)
    assertEquals(
      QuadraticPullback.lower(
        term,
        fixture.incidence,
        fixture.geometry,
        QuadraticFamily.GraphSmoothness,
        QuadraticPlacement.ObjectiveRidge
      ),
      Left(QuadraticLoweringError.GeometryMismatch(foreignGeometry, fixture.geometry.valueIdentity))
    )

  private final class ApplyCounter:
    var applications: Int = 0

  private final case class CountingOperator(value: DMat, counter: ApplyCounter) extends DoubleLinearOperator:
    val rows: Int = value.rows
    val cols: Int = value.cols
    def applyTo(input: DVec, output: MutableDVec): Unit = multiply(value, input, output)
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = multiply(value.t, input, output)
    private def multiply(current: DMat, input: DVec, output: MutableDVec): Unit =
      counter.applications += 1
      var row = 0
      while row < current.rows do
        var total = 0.0
        var column = 0
        while column < current.cols do
          total += current(row, column) * input(column)
          column += 1
        output(row) = total
        row += 1

  private final class ConcreteGraphFixture(
      val feature: SpaceRef,
      val edge: SpaceRef,
      val incidenceDense: DMat,
      val geometryDense: DMat
  ):
    type F = feature.Id
    type E = edge.Id
    val incidence: Op[Dual[F], Primal[E], CrossOperatorRole, UncheckedEvidence] = acceptedSemantic(
      Op.fromDense(
        incidenceDense,
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(edge.evidence),
        OperatorRoleWitness.cross,
        id("graph-incidence")
      )
    )
    val geometry: Op[Primal[E], Dual[E], MetricOperatorRole, CertifiedSpd] =
      certifiedMetric(edge.evidence, geometryDense, "edge-geometry")
    val term: PenaltyTerm = PenaltyTerm(
      acceptedProgram(TargetExpression.linear(ParameterId.unsafe("w"), "graph-incidence", incidence)),
      FunctionalKind.SquaredNorm(geometry.valueIdentity),
      PenaltyWeight.unsafe(0.5)
    )
    def lower(placement: QuadraticPlacement): Either[QuadraticLoweringError, QuadraticLowering[F]] =
      QuadraticPullback.lower(term, incidence, geometry, QuadraticFamily.GraphSmoothness, placement)

  private def graphFixture(): ConcreteGraphFixture =
    new ConcreteGraphFixture(
      SpaceRef(MvSpace(SpaceId.unsafe("graph-feature"), SpaceRole.Observed, Dimension.unsafe(3))),
      SpaceRef(MvSpace(SpaceId.unsafe("graph-edge"), SpaceRole.Observed, Dimension.unsafe(2))),
      matrix(Vector(Vector(-1.0, 1.0, 0.0), Vector(0.0, -1.0, 1.0))),
      DMat.eye(2)
    )

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): Op[Primal[S], Dual[S], MetricOperatorRole, CertifiedSpd] =
    val unchecked = acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.primal(space),
        CoordinateEvidence.dual(space),
        OperatorRoleWitness.metric,
        id(name)
      )
    )
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(value, CoordinateEvidence.primal(space), CoordinateEvidence.dual(space), id(name))
    )
    acceptedSemantic(Op.certifiedSpd(unchecked, acceptedSemantic(FormCertificates.spd(linear))))

  private def quadratic(value: DMat, form: DMat): Double =
    inner(value, form * value)

  private def inner(left: DMat, right: DMat): Double =
    var result = 0.0
    var row = 0
    while row < left.rows do
      result += left(row, 0) * right(row, 0)
      row += 1
    result

  private def add(left: DMat, right: DMat): DMat = left + right
  private def scale(value: DMat, factor: Double): DMat = MatrixOps.scale(value, factor)

  private def diagonal(values: Vector[Double]): DMat =
    matrix(
      values.indices.toVector.map: row =>
        values.indices.toVector.map: column =>
          if row == column then values(row) else 0.0
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

  private def accepted[A](value: Either[QuadraticLoweringError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedProgram[A](value: Either[ProgramError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
