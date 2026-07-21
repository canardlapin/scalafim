package scalafim.multivar

import gale.linalg.DMat
import scalafim.linalg.FirstOrderCapabilities
import scalafim.linalg.SolverMethodRequest

class VariationalFunctionalFramesSuite extends munit.FunSuite:

  test("all direct penalty families fit typed frames at their independent convex proximal oracle"):
    val fixture = frameFixture("direct-penalties", Vector(Vector(3.0), Vector(-1.0), Vector(2.0)))
    val groups = GroupStructure
      .from(
        fixture.chart,
        Vector(
          CoordinateGroup("first", IndexSet.unsafe(Vector(0, 1))),
          CoordinateGroup("second", IndexSet.unsafe(Vector(2)))
        ),
        identity("direct-penalty-groups")
      )
      .toOption
      .get
    val fraction = UnitFraction.unsafe(0.5)
    val groupedFactor = 1.0 - 1.0 / Math.sqrt(10.0)
    val sparseFactor = 1.0 - 0.5 / Math.sqrt(6.5)
    val cases = Vector(
      (
        FunctionalKind.L1,
        DirectProximalKind.ElementwiseL1,
        Vector(Vector(2.0), Vector(0.0), Vector(1.0))
      ),
      (
        FunctionalKind.GroupL21,
        DirectProximalKind.FeatureRowsL21,
        Vector(Vector(2.0), Vector(0.0), Vector(1.0))
      ),
      (
        FunctionalKind.GroupL2(groups.valueIdentity),
        DirectProximalKind.DisjointGroups(groups),
        Vector(Vector(3.0 * groupedFactor), Vector(-groupedFactor), Vector(1.0))
      ),
      (
        FunctionalKind.SparseGroup(fraction, groups.valueIdentity),
        DirectProximalKind.SparseGroup(fraction, groups),
        Vector(Vector(2.5 * sparseFactor), Vector(-0.5 * sparseFactor), Vector(1.0))
      ),
      (
        FunctionalKind.ElasticNet(fraction),
        DirectProximalKind.ElasticNet(fraction),
        Vector(Vector(5.0 / 3.0), Vector(-1.0 / 3.0), Vector(1.0))
      )
    )

    cases.foreach: (functional, kind, expectedRows) =>
      val term = PenaltyTerm(fixture.chart.target(fixture.variable.id), functional, PenaltyWeight.unsafe(1.0))
      val plan = DirectProximalPlan.from(term, fixture.chart, kind).toOption.get
      val fit = VariationalFunctionalFrames.directPenalty(fixture.problem, plan).toOption.get
      val weights = fit.frame.weights.toDense.toOption.get

      assertMatrixClose(weights, matrix(expectedRows), 2e-7)
      assertEquals(fit.selection.form, VariationalExecutionForm.SmoothSeparableProximal)
      assertEquals(fit.attainedGuarantee, Some(SolverGuarantee.GlobalConvexOptimum))
      assert(fit.certificate.numerical.binds(scalafim.linalg.GaleMatrixBridge.fromGale(weights), None))
      assertEquals(fit.lowering.parameterization, ParameterizationKind.Identity)
      assertEquals(fit.lowering.chart, Some(ChartKind.Identity))
      assertEquals(fit.lowering.chartIdentity, Some(fixture.chart.valueIdentity))
      assertEquals(fit.lowering.termSymmetry, plan.original.symmetry)
      assertEquals(fit.lowering.gauge, ParameterizationGauge.Unique)
      assert(fit.lowering.resultEquivalence.isInstanceOf[ResultEquivalence.ValueEquivalent])

  test("feature-row l21 couples components within each row"):
    val fixture = frameFixture(
      "row-l21",
      Vector(Vector(3.0, 4.0), Vector(0.0, 2.0)),
      featureCount = 2
    )
    val term = PenaltyTerm(
      fixture.chart.target(fixture.variable.id),
      FunctionalKind.GroupL21,
      PenaltyWeight.unsafe(1.0)
    )
    val plan = DirectProximalPlan
      .from(term, fixture.chart, DirectProximalKind.FeatureRowsL21)
      .toOption
      .get
    val fit = VariationalFunctionalFrames.directPenalty(fixture.problem, plan).toOption.get

    assertMatrixClose(
      fit.frame.weights.toDense.toOption.get,
      matrix(Vector(Vector(2.4, 3.2), Vector(0.0, 1.0))),
      2e-7
    )

  test("a certified orthogonal chart and its law survive lowering and synthesis"):
    val feature = SpaceRef.of("orthogonal-fit-feature", SpaceRole.Observed, 2).toOption.get
    val coordinates = SpaceRef.of("orthogonal-fit-coordinates", SpaceRole.Observed, 2).toOption.get
    val component = SpaceRef.of("orthogonal-fit-component", SpaceRole.Latent, 1).toOption.get
    val variable = FrameVariable
      .from(ParameterId.unsafe("orthogonal-fit-frame"), feature.evidence, component.evidence)
      .toOption
      .get
    val forwardIdentity = identity("orthogonal-fit-forward")
    val swap = matrix(Vector(Vector(0.0, 1.0), Vector(1.0, 0.0)))
    val forward: Op[Dual[feature.Id], Primal[coordinates.Id], ConstraintOperatorRole, UncheckedEvidence] = Op
      .fromDense(
        swap,
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(coordinates.evidence),
        OperatorRoleWitness.derived[ConstraintOperatorRole](OperatorRole.ConstraintMap),
        forwardIdentity
      )
      .toOption
      .get
    val synthesis: Op[Primal[coordinates.Id], Dual[feature.Id], ConstraintOperatorRole, UncheckedEvidence] = Op
      .fromDense(
        swap,
        CoordinateEvidence.primal(coordinates.evidence),
        CoordinateEvidence.dual(feature.evidence),
        OperatorRoleWitness.derived[ConstraintOperatorRole](OperatorRole.ConstraintMap),
        identity("orthogonal-fit-synthesis")
      )
      .toOption
      .get
    val chart = FeatureChart
      .certified(
        feature.evidence,
        coordinates.evidence,
        Vector("f0", "f1"),
        identity("orthogonal-fit-chart"),
        ChartKind.Orthogonal(forwardIdentity),
        forward,
        synthesis
      )
      .toOption
      .get
    val anchor = FunctionalFrame(
      Op
        .fromDense(
          matrix(Vector(Vector(3.0), Vector(-1.0))),
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(feature.evidence),
          OperatorRoleWitness.frame,
          identity("orthogonal-fit-anchor")
        )
        .toOption
        .get
    )
    val problem = ConvexFunctionalFrameProblem.from(variable, anchor).toOption.get
    val term = PenaltyTerm(chart.target(variable.id), FunctionalKind.L1, PenaltyWeight.unsafe(1.0))
    val plan = DirectProximalPlan
      .from(term, chart, DirectProximalKind.ElementwiseL1)
      .toOption
      .get
    val fit = VariationalFunctionalFrames.directPenalty(problem, plan).toOption.get

    assertMatrixClose(fit.frame.weights.toDense.toOption.get, matrix(Vector(Vector(2.0), Vector(0.0))), 2e-7)
    assertEquals(fit.lowering.chart, Some(chart.kind))
    assertEquals(fit.lowering.chartIdentity, Some(chart.valueIdentity))
    assertEquals(fit.lowering.chartLaw, chart.lawCertificate)

  test("nonnegative, box, simplex, and monotone constraints return feasible global projections"):
    val fixture = frameFixture("direct-constraints", Vector(Vector(-1.0), Vector(2.0), Vector(0.5)))
    val cases = Vector(
      FeasibleSetKind.NonnegativeOrthant -> Vector(Vector(0.0), Vector(2.0), Vector(0.5)),
      FeasibleSetKind.Box(ClosedInterval.from(0.0, 1.0).toOption.get) -> Vector(Vector(0.0), Vector(1.0), Vector(0.5)),
      FeasibleSetKind.Simplex -> Vector(Vector(0.0), Vector(1.0), Vector(0.0)),
      FeasibleSetKind.Monotone(identity("direct-monotone-order")) -> Vector(Vector(-1.0), Vector(1.25), Vector(1.25))
    )

    cases.foreach: (set, expectedRows) =>
      val term = ConstraintTerm(fixture.chart.target(fixture.variable.id), set)
      val plan = DirectProjectionPlan.from(term, fixture.chart).toOption.get
      val fit = VariationalFunctionalFrames.directConstraint(fixture.problem, plan).toOption.get

      assertMatrixClose(fit.frame.weights.toDense.toOption.get, matrix(expectedRows), 2e-7)
      assertEquals(fit.selection.form, VariationalExecutionForm.SmoothProjection)
      assertEquals(fit.attainedGuarantee, Some(SolverGuarantee.GlobalConvexOptimum))
      assert(fit.certificate.feasibilityResidual <= 1e-10)

  test("total variation executes through the composite path with KKT and primal-dual-gap evidence"):
    val fixture = frameFixture("total-variation", Vector(Vector(2.0), Vector(0.0)), featureCount = 2)
    val differenceSpace = SpaceRef.of("total-variation-differences", SpaceRole.Observed, 1).toOption.get
    val difference: Op[
      Dual[fixture.feature.Id],
      Primal[differenceSpace.Id],
      ConstraintOperatorRole,
      UncheckedEvidence
    ] = Op
      .fromDense(
        matrix(Vector(Vector(1.0, -1.0))),
        CoordinateEvidence.dual(fixture.feature.evidence),
        CoordinateEvidence.primal(differenceSpace.evidence),
        OperatorRoleWitness.derived[ConstraintOperatorRole](OperatorRole.ConstraintMap),
        identity("total-variation-difference")
      )
      .toOption
      .get
    val target = TargetExpression.linear(fixture.variable.id, "first-difference", difference).toOption.get
    val term = PenaltyTerm(target, FunctionalKind.TotalVariation, PenaltyWeight.unsafe(0.25))
    val plan = CompositePenaltyPlan
      .from(
        term,
        difference,
        AuxiliaryVariableId.unsafe("total-variation-auxiliary"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference
      )
      .toOption
      .get
    val fit = VariationalFunctionalFrames.compositePenalty(fixture.problem, plan).toOption.get

    assertMatrixClose(fit.frame.weights.toDense.toOption.get, matrix(Vector(Vector(1.75), Vector(0.25))), 2e-6)
    assertEquals(fit.selection.form, VariationalExecutionForm.LinearComposite)
    assertEquals(fit.attainedGuarantee, Some(SolverGuarantee.GlobalConvexOptimum))
    assert(fit.certificate.stationarityResidual <= 2e-6)
    assert(fit.certificate.primalDualGap.exists(_ <= 2e-6))

  test("overlapping groups always execute through the exact latent aggregation formulation"):
    val fixture = frameFixture("overlapping-groups", Vector(Vector(3.0), Vector(0.0), Vector(0.0)))
    val groups = GroupStructure
      .from(
        fixture.chart,
        Vector(
          CoordinateGroup("left", IndexSet.unsafe(Vector(0, 1))),
          CoordinateGroup("right", IndexSet.unsafe(Vector(1, 2)))
        ),
        identity("overlapping-group-structure")
      )
      .toOption
      .get
    val term = PenaltyTerm(
      fixture.chart.target(fixture.variable.id),
      FunctionalKind.GroupL2(groups.valueIdentity),
      PenaltyWeight.unsafe(1.0)
    )
    val plan = CompositePenaltyPlan
      .from(
        term,
        fixture.chart.forward,
        AuxiliaryVariableId.unsafe("overlapping-group-auxiliary"),
        SplitRequest.Automatic,
        SplitSolverCapabilities.portableReference,
        Some(groups)
      )
      .toOption
      .get
    val compiled = VariationalSolverCompiler
      .compileOverlappingGroups(plan, fixture.chart, fixture.anchorValues)
      .toOption
      .get
    val numerical = compiled.solve().toOption.get
    val fit = VariationalFunctionalFrames.overlappingGroups(fixture.problem, plan, fixture.chart).toOption.get

    assertEquals(plan.auxiliary.equation, AuxiliaryEquation.LatentGroupSum(groups.valueIdentity))
    assertEquals(compiled.lift.auxiliaryRows, 4)
    assertMatrixClose(numerical.parameter, matrix(Vector(Vector(2.0), Vector(0.0), Vector(0.0))), 3e-6)
    assertMatrixClose(fit.frame.weights.toDense.toOption.get, numerical.parameter, 1e-10)
    assertEquals(fit.attainedGuarantee, Some(SolverGuarantee.GlobalConvexOptimum))
    assert(fit.certificate.primalDualGap.exists(_ <= 3e-6))

  test("aligned-score l1 jointly fits both frames with a coupled KKT and gap certificate"):
    val source = SpaceRef.of("aligned-fit-source", SpaceRole.Observed, 1).toOption.get
    val target = SpaceRef.of("aligned-fit-target", SpaceRole.Observed, 1).toOption.get
    val entity = SpaceRef.of("aligned-fit-entity", SpaceRole.Observed, 1).toOption.get
    val component = SpaceRef.of("aligned-fit-component", SpaceRole.Latent, 1).toOption.get
    val sourceVariable = FrameVariable
      .from(ParameterId.unsafe("aligned-fit-source-frame"), source.evidence, component.evidence)
      .toOption
      .get
    val targetVariable = FrameVariable
      .from(ParameterId.unsafe("aligned-fit-target-frame"), target.evidence, component.evidence)
      .toOption
      .get
    val sourceAnchor = FunctionalFrame(
      Op
        .fromDense(
          matrix(Vector(Vector(2.0))),
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(source.evidence),
          OperatorRoleWitness.frame,
          identity("aligned-fit-source-anchor")
        )
        .toOption
        .get
    )
    val targetAnchor = FunctionalFrame(
      Op
        .fromDense(
          matrix(Vector(Vector(0.0))),
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(target.evidence),
          OperatorRoleWitness.frame,
          identity("aligned-fit-target-anchor")
        )
        .toOption
        .get
    )
    val sourceProblem = ConvexFunctionalFrameProblem.from(sourceVariable, sourceAnchor).toOption.get
    val targetProblem = ConvexFunctionalFrameProblem.from(targetVariable, targetAnchor).toOption.get
    val sourceScores: Op[Dual[source.Id], Primal[entity.Id], CrossOperatorRole, UncheckedEvidence] = Op
      .fromDense(
        matrix(Vector(Vector(1.0))),
        CoordinateEvidence.dual(source.evidence),
        CoordinateEvidence.primal(entity.evidence),
        OperatorRoleWitness.cross,
        identity("aligned-fit-source-scores")
      )
      .toOption
      .get
    val targetScores: Op[Dual[target.Id], Primal[entity.Id], CrossOperatorRole, UncheckedEvidence] = Op
      .fromDense(
        matrix(Vector(Vector(1.0))),
        CoordinateEvidence.dual(target.evidence),
        CoordinateEvidence.primal(entity.evidence),
        OperatorRoleWitness.cross,
        identity("aligned-fit-target-scores")
      )
      .toOption
      .get
    val aligned = AlignedScoreTarget.from(
      sourceVariable.id,
      targetVariable.id,
      sourceScores,
      targetScores
    )
    val cases = Vector(
      (aligned.l1(PenaltyWeight.unsafe(0.25)), 1.75, 0.25),
      (aligned.groupL21(PenaltyWeight.unsafe(0.25)), 1.75, 0.25),
      (aligned.huber(PenaltyWeight.unsafe(2.0), PenaltyWeight.unsafe(0.25)), 1.8, 0.2)
    )

    cases.foreach: (term, expectedSource, expectedTarget) =>
      val fit = VariationalFunctionalFrames
        .alignedScorePenalty(sourceProblem, targetProblem, aligned, term)
        .toOption
        .get

      assertMatrixClose(fit.sourceFrame.weights.toDense.toOption.get, matrix(Vector(Vector(expectedSource))), 2e-6)
      assertMatrixClose(fit.targetFrame.weights.toDense.toOption.get, matrix(Vector(Vector(expectedTarget))), 2e-6)
      assertEquals(fit.selection.form, VariationalExecutionForm.LinearComposite)
      assertEquals(fit.attainedGuarantee, Some(SolverGuarantee.GlobalConvexOptimum))
      assert(fit.certificate.stationarityResidual <= 2e-6)
      assert(fit.certificate.primalDualGap.exists(_ <= 2e-6))
      assertEquals(fit.lowering.termSymmetry, fit.term.symmetry)
      assertEquals(fit.lowering.gauge, ParameterizationGauge.Unique)
      assert(fit.lowering.resultEquivalence.isInstanceOf[ResultEquivalence.ValueEquivalent])

  test("iteration limits withhold an optimum guarantee and contradictory bounds remain typed"):
    val fixture = frameFixture("typed-functional-stop", Vector(Vector(3.0), Vector(-1.0), Vector(2.0)))
    val term = PenaltyTerm(
      fixture.chart.target(fixture.variable.id),
      FunctionalKind.L1,
      PenaltyWeight.unsafe(1.0)
    )
    val plan = DirectProximalPlan
      .from(term, fixture.chart, DirectProximalKind.ElementwiseL1)
      .toOption
      .get
    val short = PrimalDualConfig(
      IterationBudget.unsafe(1),
      CertificateTolerance.from(1e-14, 0.0).toOption.get,
      UnitFraction.unsafe(1.0)
    )
    val limited = VariationalFunctionalFrames.directPenalty(fixture.problem, plan, short).toOption.get

    assertEquals(limited.stopping, VariationalFrameStopping.FirstOrder(scalafim.linalg.FirstOrderStoppingStatus.IterationLimit))
    assertEquals(limited.attainedGuarantee, None)
    assertEquals(limited.certificate.numerical.iterations, 1)

    val infeasible = ConstraintFeasibility.intersectScalarBoxes(
      Vector(
        "positive" -> ClosedInterval.from(1.0, 2.0).toOption.get,
        "negative" -> ClosedInterval.from(-2.0, -1.0).toOption.get
      )
    )
    infeasible match
      case Left(SplitStoppingStatus.Infeasible(certificate)) =>
        assertEquals(certificate.constraints, Vector("positive", "negative"))
        assert(certificate.witness.exists(_ > 0.0))
      case other => fail(s"expected typed infeasibility, got $other")

  test("unsupported PSD, Stiefel, fixed-support, and rank constraints fail during compilation"):
    val fixture = frameFixture("unsupported-constraints", Vector(Vector(1.0), Vector(2.0), Vector(3.0)))
    val unsupported = Vector(
      FeasibleSetKind.PsdCone,
      FeasibleSetKind.Stiefel,
      FeasibleSetKind.FixedSupport(IndexSet.unsafe(Vector(0, 2))),
      FeasibleSetKind.RankBounded(ComponentCount.unsafe(1))
    )

    unsupported.foreach: set =>
      val result = VariationalSolverCompiler.compileConstraint(
        ConstraintTerm(fixture.chart.target(fixture.variable.id), set),
        fixture.chart,
        fixture.anchorValues
      )
      assert(result.left.toOption.exists(_.isInstanceOf[CompositeLoweringError.Chart]))

  private final class FrameFixture(val feature: SpaceRef, val component: SpaceRef, val coordinates: SpaceRef)(
      val variable: FrameVariable[feature.Id, component.Id],
      val chart: FeatureChart[feature.Id, coordinates.Id],
      val anchorValues: DMat,
      val problem: ConvexFunctionalFrameProblem[feature.Id, component.Id, UncheckedEvidence]
  )

  private def frameFixture(
      name: String,
      rows: Vector[Vector[Double]],
      featureCount: Int = 3
  ): FrameFixture =
    val feature = SpaceRef.of(s"$name-feature", SpaceRole.Observed, featureCount).toOption.get
    val component = SpaceRef.of(s"$name-component", SpaceRole.Latent, rows.head.length).toOption.get
    val coordinates = SpaceRef.of(s"$name-coordinates", SpaceRole.Observed, featureCount).toOption.get
    val variable = FrameVariable
      .from(ParameterId.unsafe(s"$name-frame"), feature.evidence, component.evidence)
      .toOption
      .get
    val chart = FeatureChart
      .identity(feature.evidence, coordinates.evidence, Vector.tabulate(featureCount)(index => s"f$index"), identity(s"$name-chart"))
      .toOption
      .get
    val anchorValues = matrix(rows)
    val weights = Op
      .fromDense(
        anchorValues,
        CoordinateEvidence.primal(component.evidence),
        CoordinateEvidence.dual(feature.evidence),
        OperatorRoleWitness.frame,
        identity(s"$name-anchor")
      )
      .toOption
      .get
    val problem = ConvexFunctionalFrameProblem.from(variable, FunctionalFrame(weights)).toOption.get
    new FrameFixture(feature, component, coordinates)(variable, chart, anchorValues, problem)

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

  private def identity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)
