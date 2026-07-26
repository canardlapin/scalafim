package scalafim.multivar
package optimization

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat

class ParameterizationSuite extends munit.FunSuite:

  test("known-support realization is exactly equivalent to the semantic objective"):
    val fixture = frameFixture("support")
    val parameterization = accepted(
      LinearFrameParameterization.knownSupport(
        fixture.variable,
        fixture.free.evidence,
        IndexSet.unsafe(Vector(0, 2)),
        id("support-embedding")
      )
    )
    val free = matrix(Vector(Vector(2.0), Vector(-1.0)))
    val semantic = accepted(parameterization.realize(free))
    val covariance = diagonal(Vector(3.0, 4.0, 5.0))
    val pulledBack = acceptedSemantic(parameterization.embedding.toDense).t * covariance * acceptedSemantic(parameterization.embedding.toDense)

    assertMatrixClose(semantic, matrix(Vector(Vector(2.0), Vector(0.0), Vector(-1.0))), 1e-12)
    assertEqualsDouble(quadratic(semantic, covariance), quadratic(free, pulledBack), 1e-12)
    assertMatrixClose(accepted(parameterization.invert(semantic)), free, 1e-12)
    assertEquals(parameterization.properties.injectivity, InjectivityClaim.VerifiedInjective)

  test("linear parameterization JVP and VJP obey the differential adjoint law"):
    val fixture = frameFixture("differential")
    val parameterization = accepted(
      LinearFrameParameterization.knownSupport(
        fixture.variable,
        fixture.free.evidence,
        IndexSet.unsafe(Vector(0, 2)),
        id("differential-embedding")
      )
    )
    val tangent = matrix(Vector(Vector(0.5), Vector(-2.0)))
    val cotangent = matrix(Vector(Vector(3.0), Vector(7.0), Vector(-1.0)))
    val jvp = accepted(parameterization.jvp(tangent))
    val vjp = accepted(parameterization.vjp(cotangent))

    assertEqualsDouble(inner(jvp, cotangent), inner(tangent, vjp), 1e-12)

  test("shared and block bases retain distinct exact parameterization semantics"):
    val fixture = frameFixture("basis")
    val support = accepted(
      LinearFrameParameterization.knownSupport(
        fixture.variable,
        fixture.free.evidence,
        IndexSet.unsafe(Vector(0, 2)),
        id("basis-embedding")
      )
    )
    val shared = LinearFrameParameterization.sharedBasis(
      fixture.variable,
      fixture.free.evidence,
      support.embedding,
      support.inverseOnImage
    )
    val blocked = accepted(
      LinearFrameParameterization.blockBasis(
        fixture.variable,
        fixture.free.evidence,
        support.embedding,
        Vector(ParameterId.unsafe("left-block"), ParameterId.unsafe("right-block"))
      )
    )
    val free = matrix(Vector(Vector(1.0), Vector(-2.0)))

    assertMatrixClose(accepted(shared.realize(free)), accepted(blocked.realize(free)), 1e-12)
    assert(shared.descriptor.kind.isInstanceOf[ParameterizationKind.SharedBasis])
    assert(blocked.descriptor.kind.isInstanceOf[ParameterizationKind.BlockDiagonal])
    assertEquals(shared.properties.redundancy, RedundancyClaim.NoRedundancy)
    assertEquals(blocked.properties.redundancy, RedundancyClaim.Unknown)

  test("null-space elimination verifies the constraint residual and records tolerance provenance"):
    val fixture = frameFixture("null")
    val constraintSpace = SpaceRef(MvSpace(SpaceId.unsafe("null-constraint"), SpaceRole.Observed, Dimension.unsafe(1)))
    type Q = constraintSpace.Id
    val basis = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(1.0, 0.0), Vector(-1.0, 1.0), Vector(0.0, -1.0))),
        CoordinateEvidence.dual(fixture.free.evidence),
        CoordinateEvidence.dual(fixture.feature.evidence),
        OperatorRoleWitness.coefficient,
        id("null-basis")
      )
    )
    val constraint = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(1.0, 1.0, 1.0))),
        CoordinateEvidence.dual(fixture.feature.evidence),
        CoordinateEvidence.primal(constraintSpace.evidence),
        OperatorRoleWitness.cross,
        id("sum-constraint")
      )
    )
    val parameterization = accepted(
      LinearFrameParameterization.nullSpace(
        fixture.variable,
        fixture.free.evidence,
        basis,
        constraint,
        CertificateTolerance.strict
      )
    )
    val semantic = accepted(parameterization.realize(matrix(Vector(Vector(2.0), Vector(-3.0)))))
    val residual = acceptedSemantic(constraint(semantic))

    assertMatrixClose(residual, matrix(Vector(Vector(0.0))), 1e-12)
    assertEquals(parameterization.nullSpaceProof.map(_.residual), Some(0.0))
    assertEquals(parameterization.nullSpaceProof.map(_.rankTolerance), Some(CertificateTolerance.strict))
    assertEquals(parameterization.nullSpaceProof.map(_.numericalCertificate.freeRows), Some(2))
    assertEquals(parameterization.nullSpaceProof.map(_.numericalCertificate.semanticRows), Some(3))
    assert(parameterization.provenance.events.exists:
      case SemanticProvenanceEvent.Derived("verify-null-space", _) => true
      case _ => false
    )

  test("general-linear gauge-equivalent fixed-rank factors lift to the same semantic map"):
    val parameterization = accepted(
      FixedRankParameterization.from(3, 2, ComponentCount.unsafe(2), id("rank-two-coefficient"))
    )
    val factors = accepted(
      FactorCoordinates.from(
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0), Vector(1.0, -1.0))),
        matrix(Vector(Vector(2.0, 1.0), Vector(-1.0, 3.0)))
      )
    )
    val transformed = accepted(
      parameterization.gaugeTransform(
        factors,
        diagonal(Vector(2.0, 0.5)),
        diagonal(Vector(0.5, 2.0))
      )
    )

    assertMatrixClose(accepted(parameterization.lift(factors)), accepted(parameterization.lift(transformed)), 1e-12)
    assert(parameterization.properties.redundantCoordinates)
    assertEquals(parameterization.properties.gauge, ParameterizationGauge.GeneralLinear)
    assertEquals(parameterization.invert(accepted(parameterization.lift(factors))), Left(ParameterizationError.InverseUnavailable))

  test("fixed-rank JVP and VJP satisfy finite-difference and adjoint oracles"):
    val parameterization = accepted(
      FixedRankParameterization.from(2, 2, ComponentCount.unsafe(1), id("rank-one-coefficient"))
    )
    val at = accepted(FactorCoordinates.from(matrix(Vector(Vector(1.0), Vector(2.0))), matrix(Vector(Vector(3.0), Vector(-1.0)))))
    val tangent = accepted(FactorCoordinates.from(matrix(Vector(Vector(0.5), Vector(-0.25))), matrix(Vector(Vector(2.0), Vector(1.0)))))
    val cotangent = matrix(Vector(Vector(1.0, -2.0), Vector(0.5, 3.0)))
    val epsilon = 1e-6
    val plus = accepted(
      FactorCoordinates.from(
        MatrixOps.subtract(at.left, MatrixOps.scale(tangent.left, -epsilon)),
        MatrixOps.subtract(at.right, MatrixOps.scale(tangent.right, -epsilon))
      )
    )
    val minus = accepted(
      FactorCoordinates.from(
        MatrixOps.subtract(at.left, MatrixOps.scale(tangent.left, epsilon)),
        MatrixOps.subtract(at.right, MatrixOps.scale(tangent.right, epsilon))
      )
    )
    val finiteDifference = MatrixOps.scale(
      MatrixOps.subtract(accepted(parameterization.realize(plus)), accepted(parameterization.realize(minus))),
      1.0 / (2.0 * epsilon)
    )
    val jvp = accepted(parameterization.jvp(at, tangent))
    val vjp = accepted(parameterization.vjp(at, cotangent))

    assertMatrixClose(finiteDifference, jvp, 1e-6)
    assertEqualsDouble(
      inner(jvp, cotangent),
      inner(tangent.left, vjp.left) + inner(tangent.right, vjp.right),
      1e-12
    )

  test("program result contract reports redundant fixed-rank gauge coordinates"):
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("gauge-feature"), SpaceRole.Observed, Dimension.unsafe(3)))
    val component = SpaceRef(MvSpace(SpaceId.unsafe("gauge-component"), SpaceRole.Latent, Dimension.unsafe(2)))
    type F = feature.Id
    type K = component.Id
    val variable = acceptedProgram(FrameVariable.from(ParameterId.unsafe("w"), feature.evidence, component.evidence))
    val parameterization = acceptedProgram(
      FrameParameterization.fixedRank(variable, ComponentCount.unsafe(1), ParameterizationGauge.GeneralLinear)
    )
    val covariance = acceptedSemantic(
      Op.fromDense(
        DMat.eye(3),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(feature.evidence),
        OperatorRoleWitness.covariance,
        id("gauge-covariance")
      )
    )
    val normalization = FrameNormalization(variable, certifiedCovariance(feature.evidence, "gauge-normalization"))
    val program = acceptedProgram(OperatorPrograms.gpca(parameterization, covariance, normalization))

    assert(program.resultSemantics.parameterIdentifiability.redundantCoordinates)
    assertEquals(program.resultSemantics.parameterIdentifiability.gauges, Vector(ParameterizationGauge.GeneralLinear))

  private final class FrameFixture(
      val feature: SpaceRef,
      val free: SpaceRef,
      val component: SpaceRef,
      val variable: FrameVariable[feature.Id, component.Id]
  )

  private def frameFixture(prefix: String): FrameFixture =
    val feature = SpaceRef(MvSpace(SpaceId.unsafe(s"$prefix-feature"), SpaceRole.Observed, Dimension.unsafe(3)))
    val free = SpaceRef(MvSpace(SpaceId.unsafe(s"$prefix-free"), SpaceRole.Observed, Dimension.unsafe(2)))
    val component = SpaceRef(MvSpace(SpaceId.unsafe(s"$prefix-component"), SpaceRole.Latent, Dimension.unsafe(1)))
    val variable = acceptedProgram(FrameVariable.from(ParameterId.unsafe(s"$prefix-w"), feature.evidence, component.evidence))
    new FrameFixture(feature, free, component, variable)

  private def certifiedCovariance[S <: SemanticSpace](
      space: SpaceEvidence[S],
      name: String
  ): OpCovariance[S, CertifiedSpd] =
    val unchecked = acceptedSemantic(
      Op.fromDense(
        DMat.eye(space.dimension),
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        OperatorRoleWitness.covariance,
        id(name)
      )
    )
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(
        DMat.eye(space.dimension),
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        id(name)
      )
    )
    acceptedSemantic(Op.certifiedSpd(unchecked, acceptedSemantic(FormCertificates.spd(linear))))

  private def quadratic(value: DMat, form: DMat): Double = inner(value, form * value)

  private def inner(left: DMat, right: DMat): Double =
    var result = 0.0
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        result += left(row, column) * right(row, column)
        column += 1
      row += 1
    result

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

  private def accepted[A](value: Either[ParameterizationError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedProgram[A](value: Either[ProgramError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
