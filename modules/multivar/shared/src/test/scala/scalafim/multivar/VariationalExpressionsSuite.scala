package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class VariationalExpressionsSuite extends munit.FunSuite:

  test("pure typed maps obey identity and associative composition laws"):
    val plusTwo = TypedLinearMap.instance[Int, Int](
      TypedMapDescriptor("plus-two-coordinate-map", MapCapability.Linear, Vector.empty, FrameSymmetry.Orthogonal),
      _ + 2,
      _ - 2
    )
    val timesThree = TypedLinearMap.instance[Int, Int](
      TypedMapDescriptor("times-three-coordinate-map", MapCapability.Linear, Vector.empty, FrameSymmetry.Orthogonal),
      _ * 3,
      _ / 3
    )
    val identity = TypedLinearMap.identity[Int]()

    assertEquals(identity.andThenLinear(plusTwo)(4), plusTwo(4))
    assertEquals(plusTwo.andThenLinear(identity)(4), plusTwo(4))
    assertEquals(plusTwo.andThenLinear(timesThree)(4), timesThree(plusTwo(4)))
    assertEquals(plusTwo.andThenLinear(timesThree).dual(18), plusTwo.dual(timesThree.dual(18)))

  test("only linear maps expose an algebraic dual"):
    val errors = typeCheckErrors(
      """
        val general = scalafim.multivar.TypedGeneralMap.instance[Int, Int](
          scalafim.multivar.TypedMapDescriptor(
            "general",
            scalafim.multivar.MapCapability.General,
            Vector.empty,
            scalafim.multivar.FrameSymmetry.Identity
          ),
          identity
        )
        general.dual
      """
    )
    assert(errors.nonEmpty)

  test("product expressions retain every parameter and composite capability"):
    val left = ParameterExpression[Double](ParameterId.unsafe("left"), "frame")
    val right = ParameterExpression[Double](ParameterId.unsafe("right"), "frame")
    val disagreement = TypedSmoothMap.instance[(Double, Double), Double](
      TypedMapDescriptor("aligned-score-difference", MapCapability.Smooth, Vector.empty, FrameSymmetry.Orthogonal),
      values => values._1 - values._2,
      (_, tangent) => tangent._1 - tangent._2,
      (_, cotangent) => (cotangent, -cotangent)
    )
    val target = left.product(right).through(disagreement)
    val erased = TargetExpression.typed(target)

    assertEquals(erased.parameters.map(_.value), Vector("left", "right"))
    assertEquals(erased.capability, TargetCapability.Smooth)
    assert(erased.operation.contains("aligned-score-difference"))

  test("functional and feasible-set traits are semantic capabilities, not solver nodes"):
    val l1 = TypedFunctional.l1[Double]
    val squared = TypedFunctional.squaredNorm[Double](id("coefficient-geometry"))
    val stiefel = TypedFeasibleSet.stiefel[Double]

    assert(l1.traits.supports(OracleCapability.Proximal))
    assert(!l1.traits.supports(OracleCapability.Gradient))
    assert(squared.traits.supports(OracleCapability.HessianVector))
    assertEquals(stiefel.traits.structure, SetStructure.Manifold)
    assert(!stiefel.traits.supports(SetCapability.Conic))

  test("l1 selects a frame while row-group l21 preserves subspace semantics"):
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("variational-feature"), SpaceRole.Observed, Dimension.unsafe(2)))
    val component = SpaceRef(MvSpace(SpaceId.unsafe("variational-component"), SpaceRole.Latent, Dimension.unsafe(1)))
    type F = feature.Id
    type K = component.Id
    val variable = accepted(FrameVariable.from(ParameterId.unsafe("w"), feature.evidence, component.evidence))
    val parameterization = FrameParameterization.identity(variable)
    val covariance = acceptedSemantic(
      Op.fromDense(
        DMat.eye(2),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(feature.evidence),
        OperatorRoleWitness.covariance,
        id("variational-covariance")
      )
    )
    val normalization = FrameNormalization(variable, certifiedCovariance(feature.evidence))
    val frame = ParameterExpression[DMat](variable.id, "functional-frame")
    val objective = BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, covariance))
    val l1 = accepted(
      OperatorProgram.from(
        Vector(parameterization),
        objective,
        Vector(normalization),
        penalties = Vector(PenaltyTerm.typed(frame, TypedFunctional.l1[DMat], PenaltyWeight.unsafe(0.5)))
      )
    )
    val l21 = accepted(
      OperatorProgram.from(
        Vector(parameterization),
        objective,
        Vector(normalization),
        penalties = Vector(PenaltyTerm.typed(frame, TypedFunctional.groupL21[DMat], PenaltyWeight.unsafe(0.5)))
      )
    )

    assertEquals(
      l1.resultSemantics.equivalence,
      ResultEquivalence.FrameEquivalent(FrameSymmetry.SignedPermutation, CertificateTolerance.strict)
    )
    assert(l21.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.SubspaceEquivalent])

  private def certifiedCovariance[S <: SemanticSpace](space: SpaceEvidence[S]): OpCovariance[S, CertifiedSpd] =
    val unchecked = acceptedSemantic(
      Op.fromDense(
        DMat.eye(space.dimension),
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        OperatorRoleWitness.covariance,
        id("variational-normalization")
      )
    )
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(
        DMat.eye(space.dimension),
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        id("variational-normalization")
      )
    )
    acceptedSemantic(Op.certifiedSpd(unchecked, acceptedSemantic(FormCertificates.spd(linear))))

  private def accepted[A](value: Either[ProgramError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))
