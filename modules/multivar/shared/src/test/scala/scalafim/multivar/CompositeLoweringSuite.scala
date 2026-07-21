package scalafim.multivar

import gale.linalg.DMat

class CompositeLoweringSuite extends munit.FunSuite:

  test("general linear composition lowers to an explicit auxiliary equation and selected capability"):
    val fixture = diagonalFixture("explicit-split")
    val plan = accepted(
      CompositePenaltyPlan.from(
        fixture.penalty(FunctionalKind.L1, 0.25),
        fixture.target,
        AuxiliaryVariableId.unsafe("z"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
    )

    assertEquals(plan.method, SplitMethod.PrimalDual)
    assertEquals(plan.auxiliary.equation, AuxiliaryEquation.TargetCopy)
    assertEquals(plan.auxiliary.rendered, "z = T(theta)")
    assert(plan.provenance.events.exists:
      case SemanticProvenanceEvent.Derived("composite-primaldual-split", inputs) =>
        inputs == Vector(fixture.target.valueIdentity)
      case _ => false
    )

  test("portable primal-dual solution matches a separable closed-form oracle and KKT certificate"):
    val fixture = diagonalFixture("diagonal-oracle")
    val plan = accepted(
      CompositePenaltyPlan.from(
        fixture.penalty(FunctionalKind.L1, 0.25),
        fixture.target,
        AuxiliaryVariableId.unsafe("diagonal-z"),
        SplitRequest.Require(SplitMethod.PrimalDual),
        SplitSolverCapabilities.portableReference
      )
    )
    val observation = matrix(Vector(Vector(1.0), Vector(-2.0)))
    val solution = accepted(PrimalDualL1Reference.solve(plan, observation))
    val expected = matrix(Vector(Vector(0.5), Vector(-1.75)))

    assertMatrixClose(solution.parameter, expected, 1e-6)
    solution.status match
      case SplitStoppingStatus.Converged(certificate) =>
        assert(certificate.stationarity <= 1e-7)
        assert(certificate.dualFeasibility <= 1e-12)
        assert(certificate.complementarity <= 1e-7)
        assert(certificate.primalDualGap <= 1e-7)
      case other => fail(s"expected converged KKT certificate, got $other")

  test("graph total variation matches an independently solved three-node fused-lasso oracle"):
    val feature = space("tv-feature", 3)
    val edges = space("tv-edge", 2)
    type F = feature.Id
    type E = edges.Id
    val incidence = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(-1.0, 1.0, 0.0), Vector(0.0, -1.0, 1.0))),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(edges.evidence),
        OperatorRoleWitness.cross,
        id("tv-incidence")
      )
    )
    val parameter = ParameterId.unsafe("tv-w")
    val target = acceptedProgram(TargetExpression.linear(parameter, "graph-incidence", incidence))
    val penalty = PenaltyTerm(target, FunctionalKind.TotalVariation, PenaltyWeight.unsafe(0.25))
    val plan = accepted(
      CompositePenaltyPlan.from(
        penalty,
        incidence,
        AuxiliaryVariableId.unsafe("tv-differences"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
    )
    val observation = matrix(Vector(Vector(0.0), Vector(2.0), Vector(0.0)))
    val solution = accepted(PrimalDualL1Reference.solve(plan, observation))

    assertMatrixClose(solution.parameter, matrix(Vector(Vector(0.25), Vector(1.5), Vector(0.25))), 1e-6)
    assertMatrixClose(solution.auxiliary, matrix(Vector(Vector(1.25), Vector(-1.25))), 1e-6)

  test("overlapping groups use latent lifted variables and never a naive direct prox"):
    val feature = space("overlap-feature", 3)
    val coordinates = space("overlap-coordinates", 3)
    val chart = acceptedChart(
      FeatureChart.identity(
        feature.evidence,
        coordinates.evidence,
        Vector("a", "b", "c"),
        id("overlap-chart")
      )
    )
    val groups = acceptedChart(
      GroupStructure.from(
        chart,
        Vector(
          CoordinateGroup("left", IndexSet.unsafe(Vector(0, 1))),
          CoordinateGroup("right", IndexSet.unsafe(Vector(1, 2)))
        ),
        id("overlap-groups")
      )
    )
    val parameter = ParameterId.unsafe("overlap-w")
    val penalty = PenaltyTerm(
      chart.target(parameter),
      FunctionalKind.GroupL2(groups.valueIdentity),
      PenaltyWeight.unsafe(0.5)
    )

    val direct = DirectProximalPlan.from(penalty, chart, DirectProximalKind.DisjointGroups(groups))
    assertEquals(direct, Left(ChartError.GroupOverlapUnsupported(groups.valueIdentity)))

    val plan = accepted(
      CompositePenaltyPlan.from(
        penalty,
        chart.forward,
        AuxiliaryVariableId.unsafe("latent-groups"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference,
        Some(groups)
      )
    )
    val lift = accepted(OverlappingGroupLift.from(groups))
    val value = matrix(Vector(Vector(3.0), Vector(6.0), Vector(9.0)))
    val auxiliary = accepted(lift.feasibleLift(value))

    assertEquals(plan.functional, CompositeFunctional.LatentOverlappingGroups(groups))
    assertEquals(plan.auxiliary.equation, AuxiliaryEquation.LatentGroupSum(groups.valueIdentity))
    assertMatrixClose(auxiliary, matrix(Vector(Vector(3.0), Vector(3.0), Vector(3.0), Vector(9.0))), 1e-12)
    assertMatrixClose(accepted(lift.aggregate(auxiliary)), value, 1e-12)
    assert(accepted(lift.proximal(auxiliary, 1.0)) != auxiliary)

  test("aligned-score l1, group, Huber, bounded, and equality forms share one typed multi-input target"):
    val source = space("aligned-source", 2)
    val target = space("aligned-target", 2)
    val entity = space("aligned-entity", 2)
    val sourceMap = linearMap(source.evidence, entity.evidence, matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0))), "aligned-left")
    val targetMap = linearMap(target.evidence, entity.evidence, matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))), "aligned-right")
    val aligned = AlignedScoreTarget.from(
      ParameterId.unsafe("source-w"),
      ParameterId.unsafe("target-w"),
      sourceMap,
      targetMap
    )
    val sourceValue = matrix(Vector(Vector(1.0), Vector(3.0)))
    val targetValue = matrix(Vector(Vector(2.0), Vector(1.0)))
    val evaluated = aligned.map((sourceValue, targetValue))
    val cotangent = matrix(Vector(Vector(0.5), Vector(-2.0)))
    val dual = aligned.map.dual(cotangent)

    assertMatrixClose(evaluated, matrix(Vector(Vector(-3.0), Vector(5.0))), 1e-12)
    assertEqualsDouble(inner(evaluated, cotangent), inner(sourceValue, dual._1) + inner(targetValue, dual._2), 1e-12)
    assertEquals(aligned.erased.parameters, Vector(ParameterId.unsafe("source-w"), ParameterId.unsafe("target-w")))
    assertEquals(aligned.l1(PenaltyWeight.unsafe(1.0)).functional, FunctionalKind.L1)
    assertEquals(aligned.groupL21(PenaltyWeight.unsafe(1.0)).functional, FunctionalKind.GroupL21)
    assert(aligned.huber(PenaltyWeight.unsafe(0.5), PenaltyWeight.unsafe(1.0)).functional.isInstanceOf[FunctionalKind.Huber])
    assert(aligned.bounded(PenaltyWeight.unsafe(2.0)).feasibleSet.isInstanceOf[FeasibleSetKind.NormBall])
    assertEquals(aligned.equality.feasibleSet, FeasibleSetKind.ZeroSubspace)

  test("hard composed constraints retain set intent while using the same auxiliary equation"):
    val fixture = diagonalFixture("constraint-split")
    val target = acceptedProgram(TargetExpression.linear(ParameterId.unsafe("constraint-w"), "constraint-map", fixture.target))
    val equality = ConstraintTerm(target, FeasibleSetKind.ZeroSubspace)
    val bounded = ConstraintTerm(target, FeasibleSetKind.NormBall(PenaltyWeight.unsafe(1.5)))
    val equalityPlan = accepted(
      CompositeConstraintPlan.from(
        equality,
        fixture.target,
        AuxiliaryVariableId.unsafe("constraint-z"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
    )
    val boundedPlan = accepted(
      CompositeConstraintPlan.from(
        bounded,
        fixture.target,
        AuxiliaryVariableId.unsafe("bounded-z"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
    )

    assertEquals(equalityPlan.original.feasibleSet, FeasibleSetKind.ZeroSubspace)
    assert(boundedPlan.original.feasibleSet.isInstanceOf[FeasibleSetKind.NormBall])
    assertEquals(equalityPlan.auxiliary.equation, AuxiliaryEquation.TargetCopy)

  test("iteration limits and contradictory bounds have typed stopping semantics"):
    val fixture = diagonalFixture("typed-stop")
    val plan = accepted(
      CompositePenaltyPlan.from(
        fixture.penalty(FunctionalKind.L1, 0.25),
        fixture.target,
        AuxiliaryVariableId.unsafe("typed-stop-z"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
    )
    val short = PrimalDualConfig(
      IterationBudget.unsafe(1),
      acceptedSemantic(CertificateTolerance.from(1e-14, 0.0)),
      UnitFraction.unsafe(1.0)
    )
    val limited = accepted(PrimalDualL1Reference.solve(plan, matrix(Vector(Vector(1.0), Vector(-2.0))), short))

    assert(limited.status.isInstanceOf[SplitStoppingStatus.IterationLimit])
    val infeasible = ConstraintFeasibility.intersectScalarBoxes(
      Vector(
        "positive" -> acceptedProgram(ClosedInterval.from(1.0, 2.0)),
        "negative" -> acceptedProgram(ClosedInterval.from(-2.0, -1.0))
      )
    )
    infeasible match
      case Left(SplitStoppingStatus.Infeasible(certificate)) =>
        assertEquals(certificate.constraints, Vector("positive", "negative"))
        assert(certificate.witness.exists(_ > 0.0))
      case other => fail(s"expected typed infeasibility, got $other")

  test("an unavailable requested split method fails before numerical execution"):
    val fixture = diagonalFixture("missing-capability")
    val result = CompositePenaltyPlan.from(
      fixture.penalty(FunctionalKind.L1, 0.25),
      fixture.target,
      AuxiliaryVariableId.unsafe("missing-z"),
      SplitRequest.Require(SplitMethod.Admm),
      SplitSolverCapabilities.portableReference
    )

    assertEquals(
      result.left.toOption,
      Some(CompositeLoweringError.MissingSolverCapability(SplitMethod.Admm, Set(SplitMethod.PrimalDual)))
    )

  private final class DiagonalFixture(val feature: SpaceRef, val targetSpace: SpaceRef, name: String):
    type F = feature.Id
    type T = targetSpace.Id
    val target: Op[Dual[F], Primal[T], CrossOperatorRole, UncheckedEvidence] = acceptedSemantic(
      Op.fromDense(
        diagonal(Vector(2.0, 1.0)),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(targetSpace.evidence),
        OperatorRoleWitness.cross,
        id(s"$name-target")
      )
    )
    private val parameter = ParameterId.unsafe(s"$name-w")
    def penalty(functional: FunctionalKind, weight: Double): PenaltyTerm =
      PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-map", target)),
        functional,
        PenaltyWeight.unsafe(weight)
      )

  private def diagonalFixture(name: String): DiagonalFixture =
    new DiagonalFixture(space(s"$name-feature", 2), space(s"$name-target-space", 2), name)

  private def linearMap[Source <: SemanticSpace, Entity <: SemanticSpace](
      source: SpaceEvidence[Source],
      entity: SpaceEvidence[Entity],
      value: DMat,
      name: String
  ): Op[Dual[Source], Primal[Entity], CrossOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(source),
        CoordinateEvidence.primal(entity),
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

  private def accepted[A](value: Either[CompositeLoweringError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedChart[A](value: Either[ChartError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedProgram[A](value: Either[ProgramError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
