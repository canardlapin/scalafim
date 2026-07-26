package scalafim.multivar
package optimization

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat

class OperatorPoliciesSuite extends munit.FunSuite:

  test("linear shrinkage matches the independent covariance formula and is not a ridge term"):
    val feature = space("shrink-feature", 2)
    type F = feature.Id
    val input = certifiedCovariance(feature.evidence, diagonal(Vector(4.0, 2.0)), "shrink-input")
    val target = certifiedCovariance(feature.evidence, DMat.eye(2), "shrink-target")
    val result = acceptedPolicy(
      LinearShrinkagePolicy(
        PolicyId.unsafe("linear-shrinkage-test"),
        input,
        target,
        PolicySelection.Fixed(UnitFraction.unsafe(0.25)),
        ScaleMatching.MatchTrace
      )
    )

    assertMatrixClose(acceptedSemantic(result.operator.toDense), diagonal(Vector(3.75, 2.25)), 1e-12)
    assertEquals(result.operator.certificate.status, EvidenceStatus.Certified)
    assertEquals(result.record.kind, "linear-shrinkage")
    assertEquals(result.record.scope, PolicyScope.SingleOperator)
    assert(result.record.preservation.contains(PreservationClaim.PsdPreserved))

  test("fold-selected shrinkage refuses to execute before training-fold selection"):
    val feature = space("selection-feature", 2)
    val input = certifiedCovariance(feature.evidence, diagonal(Vector(2.0, 1.0)), "selection-input")
    val target = certifiedCovariance(feature.evidence, DMat.eye(2), "selection-target")
    val hook = acceptedPolicy(
      PolicySelectionHook.from(
        PolicyId.unsafe("select-alpha"),
        Vector(UnitFraction.unsafe(0.0), UnitFraction.unsafe(0.5), UnitFraction.unsafe(1.0))
      )
    )
    val result = LinearShrinkagePolicy(
      PolicyId.unsafe("selected-shrinkage"),
      input,
      target,
      PolicySelection.FoldSelected(hook),
      ScaleMatching.None
    )

    assertEquals(result, Left(OperatorPolicyError.RequiresFoldSelection(hook)))

  test("nearest-PSD repair clamps negative spectrum and records its tolerance"):
    val feature = space("repair-feature", 2)
    val unchecked = acceptedSemantic(
      Op.fromDense(
        diagonal(Vector(2.0, -0.5)),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(feature.evidence),
        OperatorRoleWitness.covariance,
        id("repair-input")
      )
    )
    val tolerance = acceptedSemantic(CertificateTolerance.from(1e-7, 1e-6))
    val result = acceptedPolicy(PsdRepairPolicy.nearestPsd(PolicyId.unsafe("repair"), unchecked, tolerance))

    assertMatrixClose(acceptedSemantic(result.operator.toDense), diagonal(Vector(2.0, 0.0)), 1e-10)
    assertEquals(result.operator.certificate.claims.head.context.tolerance, tolerance)
    assert(result.record.preservation.exists:
      case PreservationClaim.EvidenceDowngraded(reason) =>
        reason.contains("clipped minimum") && reason.contains("tolerance")
      case _ => false
    )

  test("support restriction and trace gauge preserve PSD evidence"):
    val full = space("support-full", 3)
    val supported = space("support-small", 2)
    type Full = full.Id
    type Supported = supported.Id
    val input = certifiedCovariance(full.evidence, diagonal(Vector(2.0, 3.0, 4.0)), "support-input")
    val embedding = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.0), Vector(0.0, 1.0))),
        CoordinateEvidence.dual(supported.evidence),
        CoordinateEvidence.dual(full.evidence),
        OperatorRoleWitness.coefficient,
        id("support-embedding")
      )
    )
    val restricted = acceptedPolicy(SupportRestrictionPolicy(PolicyId.unsafe("support"), input, embedding))
    val gauged = acceptedPolicy(GaugeFixingPolicy.traceOne(PolicyId.unsafe("trace-one"), restricted.operator))

    assertMatrixClose(acceptedSemantic(restricted.operator.toDense), diagonal(Vector(2.0, 4.0)), 1e-12)
    assertEqualsDouble(trace(acceptedSemantic(gauged.operator.toDense)), 1.0, 1e-12)
    assert(restricted.record.preservation.contains(PreservationClaim.SupportRestricted))
    assert(gauged.record.preservation.contains(PreservationClaim.GaugeFixed))

  test("LDA within-scatter shrinkage remains a named statistical policy"):
    val feature = space("lda-policy-feature", 2)
    val within = certifiedScatter(feature.evidence, diagonal(Vector(5.0, 1.0)), "lda-within")
    val target = certifiedCometric(feature.evidence, DMat.eye(2), "lda-cometric")
    val result = acceptedPolicy(
      LdaWithinScatterPolicy.shrink(
        PolicyId.unsafe("lda-within-policy"),
        within,
        target,
        PolicySelection.Fixed(UnitFraction.unsafe(0.2)),
        ScaleMatching.MatchTrace
      )
    )

    assertEquals(result.record.kind, "lda-within-scatter-shrinkage")
    assertEquals(result.operator.role.value, OperatorRole.Scatter)
    assertMatrixClose(acceptedSemantic(result.operator.toDense), diagonal(Vector(4.6, 1.4)), 1e-12)

  test("joint shrinkage preserves PSD, adjoint blocks, and shared gauge"):
    val fixture = jointFixture()
    val strength = UnitFraction.unsafe(0.25)
    val (result, record) = acceptedPolicy(
      JointBlockCovariance.shrink(
        PolicyId.unsafe("joint-shrinkage"),
        fixture.input,
        fixture.target,
        PolicySelection.Fixed(strength)
      )
    )
    val expected = matrix(Vector(Vector(1.75, 0.375), Vector(0.375, 1.0)))

    assertMatrixClose(acceptedPolicy(result.dense), expected, 1e-12)
    assert(result.certificate.minimumEigenvalue >= -CertificateTolerance.strict.threshold(1.0))
    assertMatrixClose(acceptedSemantic(result.yx.toDense), acceptedSemantic(result.xy.toDense).t, 1e-12)
    assertEquals(record.scope, PolicyScope.JointSystem)
    assert(record.preservation.contains(PreservationClaim.PsdPreserved))
    assert(record.preservation.contains(PreservationClaim.BlockAdjointsPreserved))
    assert(record.preservation.contains(PreservationClaim.SharedGaugePreserved))

  test("independent block shrinkage visibly downgrades joint evidence"):
    val fixture = jointFixture()
    val result = acceptedPolicy(
      JointBlockCovariance.blockwiseUnsafe(
        PolicyId.unsafe("unsafe-blockwise"),
        fixture.input,
        UnitFraction.unsafe(0.25),
        UnitFraction.unsafe(0.5)
      )
    )

    assertEquals(result.record.scope, PolicyScope.BlockwiseUnsafe)
    assert(result.record.preservation.exists:
      case PreservationClaim.EvidenceDowngraded(reason) =>
        reason.contains("marginal 0.25") && reason.contains("cross 0.5")
      case _ => false
    )
    assertMatrixClose(acceptedSemantic(result.yx.toDense), acceptedSemantic(result.xy.toDense).t, 1e-12)

  private final class JointFixture(
      val x: SpaceRef,
      val y: SpaceRef
  ):
    type X = x.Id
    type Y = y.Id
    private val gauge = id("joint-gauge")
    val input: JointBlockCovariance[X, Y] = acceptedPolicy(
      JointBlockCovariance.from(
        certifiedCovariance(x.evidence, diagonal(Vector(2.0)), "joint-input-xx"),
        cross(x.evidence, y.evidence, matrix(Vector(Vector(0.5))), "joint-input-xy"),
        certifiedCovariance(y.evidence, diagonal(Vector(1.0)), "joint-input-yy"),
        gauge
      )
    )
    val target: JointBlockCovariance[X, Y] = acceptedPolicy(
      JointBlockCovariance.from(
        certifiedCovariance(x.evidence, diagonal(Vector(1.0)), "joint-target-xx"),
        cross(x.evidence, y.evidence, matrix(Vector(Vector(0.0))), "joint-target-xy"),
        certifiedCovariance(y.evidence, diagonal(Vector(1.0)), "joint-target-yy"),
        gauge
      )
    )

  private def jointFixture(): JointFixture =
    new JointFixture(space("joint-x", 1), space("joint-y", 1))

  private def certifiedCovariance[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): Op[Dual[S], Primal[S], CovarianceOperatorRole, CertifiedPsd] =
    certifiedPsd(space, value, OperatorRoleWitness.covariance, name)

  private def certifiedScatter[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): Op[Dual[S], Primal[S], ScatterOperatorRole, CertifiedPsd] =
    certifiedPsd(space, value, OperatorRoleWitness.scatter, name)

  private def certifiedCometric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): Op[Dual[S], Primal[S], CometricOperatorRole, CertifiedPsd] =
    certifiedPsd(space, value, OperatorRoleWitness.cometric, name)

  private def certifiedPsd[S <: SemanticSpace, R <: OperatorRoleTag](
      space: SpaceEvidence[S],
      value: DMat,
      role: OperatorRoleWitness[R],
      name: String
  ): Op[Dual[S], Primal[S], R, CertifiedPsd] =
    val identity = id(name)
    val unchecked = acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        role,
        identity
      )
    )
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(value, CoordinateEvidence.dual(space), CoordinateEvidence.primal(space), identity)
    )
    acceptedSemantic(Op.certifiedPsd(unchecked, acceptedSemantic(FormCertificates.psd(linear))))

  private def cross[X <: SemanticSpace, Y <: SemanticSpace](
      x: SpaceEvidence[X],
      y: SpaceEvidence[Y],
      value: DMat,
      name: String
  ): Op[Dual[Y], Primal[X], CrossOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(y),
        CoordinateEvidence.primal(x),
        OperatorRoleWitness.cross,
        id(name)
      )
    )

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def diagonal(values: Vector[Double]): DMat =
    matrix(
      values.indices.toVector.map: row =>
        values.indices.toVector.map: column =>
          if row == column then values(row) else 0.0
    )

  private def trace(value: DMat): Double =
    var result = 0.0
    var index = 0
    while index < Math.min(value.rows, value.cols) do
      result += value(index, index)
      index += 1
    result

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

  private def acceptedPolicy[A](value: Either[OperatorPolicyError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
