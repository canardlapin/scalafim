package scalafim.multivar
package solver

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*

import gale.linalg.DMat
import scalafim.linalg.FirstOrderMethod

class CompositePenaltyCompilerSuite extends munit.FunSuite:

  test("l1 plus graph smoothness matches an independent analytic minimizer"):
    val fixture = new Fixture("graph-l1", QuadraticFamily.GraphSmoothness)
    val fit = solve(
      fixture,
      matrix(Vector(Vector(2.0), Vector(0.0))),
      smooth = Vector(fixture.smooth(0.5)),
      direct = Vector(fixture.l1(0.25)),
      split = Vector.empty
    )

    assertMatrixClose(
      fit.parameter,
      matrix(Vector(Vector(13.0 / 12.0), Vector(5.0 / 12.0))),
      2e-6
    )
    assertEquals(fit.selection.method, FirstOrderMethod.ProximalGradient)
    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.EpsilonGlobal)
    assert(fit.certificate.stationarityResidual <= 2e-6)
    assert(fit.certificate.primalDualGap <= 2e-6)
    assert(fit.certificate.objectiveAgreement <= 1e-12)

  test("l1 plus graph smoothness plus total variation uses sound three-operator splitting"):
    val fixture = new Fixture("graph-l1-tv", QuadraticFamily.GraphSmoothness)
    val fit = solve(
      fixture,
      matrix(Vector(Vector(2.0), Vector(0.0))),
      smooth = Vector(fixture.smooth(0.5)),
      direct = Vector(fixture.l1(0.25)),
      split = Vector(fixture.tv(0.25))
    )

    assertMatrixClose(fit.parameter, matrix(Vector(Vector(1.0), Vector(0.5))), 3e-6)
    assertEquals(fit.selection.method, FirstOrderMethod.SmoothCompositePrimalDual)
    assert(fit.dual.nonEmpty)
    assert(fit.certificate.stationarityResidual <= 3e-6)
    assert(fit.certificate.dualFeasibilityResidual <= 1e-12)
    assert(fit.certificate.primalDualGap <= 3e-6)
    assertEquals(fit.certificate.splitOperatorNorm.derivation, NormBoundDerivation.StackedRootSumSquares)
    assertEqualsDouble(fit.certificate.splitNormBound, Math.sqrt(2.0), 1e-12)
    assertEqualsDouble(fit.certificate.objective.anchor, 0.625, 3e-6)
    assertEqualsDouble(fit.certificate.objective.smooth.map(_._2).sum, 0.125, 3e-6)
    assertEqualsDouble(fit.certificate.objective.direct.map(_._2).get, 0.375, 3e-6)
    assertEqualsDouble(fit.certificate.objective.split.map(_._2).sum, 0.125, 3e-6)

  test("composite result agrees with an independent exhaustive convex oracle"):
    val fixture = new Fixture("grid-oracle", QuadraticFamily.GraphSmoothness)
    val anchor = matrix(Vector(Vector(2.0), Vector(0.0)))
    val fit = solve(
      fixture,
      anchor,
      Vector(fixture.smooth(0.5)),
      Vector(fixture.l1(0.25)),
      Vector(fixture.tv(0.25))
    )
    val step = 0.01
    var bestFirst = 0.0
    var bestSecond = 0.0
    var bestObjective = Double.PositiveInfinity
    var first = -0.5
    while first <= 2.5 do
      var second = -0.5
      while second <= 2.5 do
        val candidate = independentSparseSmoothObjective(first, second)
        if candidate < bestObjective then
          bestObjective = candidate
          bestFirst = first
          bestSecond = second
        second += step
      first += step

    assert(fit.certificate.objective.total <= bestObjective + 1e-8)
    assert(Math.abs(fit.parameter(0, 0) - bestFirst) <= step + 1e-8)
    assert(Math.abs(fit.parameter(1, 0) - bestSecond) <= step + 1e-8)

  test("derivative, spline, and sparse-group smooth programs retain their declared semantics"):
    val derivative = new Fixture("derivative-l1", QuadraticFamily.DerivativeSmoothness)
    val spline = new Fixture("spline-l1", QuadraticFamily.SplineSmoothness)
    val anchor = matrix(Vector(Vector(2.0), Vector(0.0)))
    val derivativeFit = solve(
      derivative,
      anchor,
      Vector(derivative.smooth(0.5)),
      Vector(derivative.l1(0.25)),
      Vector.empty
    )
    val splineFit = solve(
      spline,
      anchor,
      Vector(spline.smooth(0.5)),
      Vector(spline.l1(0.25)),
      Vector.empty
    )

    assertMatrixClose(derivativeFit.parameter, splineFit.parameter, 2e-7)

    val grouped = new Fixture("sparse-group-smooth", QuadraticFamily.DerivativeSmoothness)
    val groupedFit = solve(
      grouped,
      matrix(Vector(Vector(3.0), Vector(4.0))),
      Vector(grouped.smooth(0.1)),
      Vector(grouped.sparseGroup(weight = 0.5, l1Fraction = 0.4)),
      Vector.empty
    )
    val x = groupedFit.parameter
    val norm = Math.sqrt(x(0, 0) * x(0, 0) + x(1, 0) * x(1, 0))
    val firstKkt = x(0, 0) - 3.0 + 0.2 * (x(0, 0) - x(1, 0)) + 0.2 + 0.3 * x(0, 0) / norm
    val secondKkt = x(1, 0) - 4.0 + 0.2 * (x(1, 0) - x(0, 0)) + 0.2 + 0.3 * x(1, 0) / norm

    assert(x(0, 0) > 0.0 && x(1, 0) > 0.0)
    assertEqualsDouble(firstKkt, 0.0, 2e-6)
    assertEqualsDouble(secondKkt, 0.0, 2e-6)
    assert(groupedFit.certificate.primalDualGap <= 3e-6)

  test("per-block penalties survive a simultaneous block permutation"):
    val original = new Fixture("per-block-original", QuadraticFamily.BlockSmoothness)
    val originalFit = solve(
      original,
      matrix(Vector(Vector(2.0), Vector(-2.0))),
      Vector.empty,
      Vector.empty,
      Vector(original.selectedL1(0, 0.25, "first"), original.selectedL1(1, 0.75, "second"))
    )
    val permuted = new Fixture("per-block-permuted", QuadraticFamily.BlockSmoothness)
    val permutedFit = solve(
      permuted,
      matrix(Vector(Vector(-2.0), Vector(2.0))),
      Vector.empty,
      Vector.empty,
      Vector(permuted.selectedL1(0, 0.75, "first"), permuted.selectedL1(1, 0.25, "second"))
    )

    assertMatrixClose(originalFit.parameter, matrix(Vector(Vector(1.75), Vector(-1.25))), 3e-6)
    assertMatrixClose(permutedFit.parameter, matrix(Vector(Vector(-1.25), Vector(1.75))), 3e-6)
    assertEqualsDouble(originalFit.parameter(0, 0), permutedFit.parameter(1, 0), 3e-6)
    assertEqualsDouble(originalFit.parameter(1, 0), permutedFit.parameter(0, 0), 3e-6)

  test("unsupported proximal sums and incompatible feature dimensions fail before solving"):
    val fixture = new Fixture("reject-composition", QuadraticFamily.GraphSmoothness)
    val direct = fixture.l1(0.25)
    val tooMany = CompositeSparseSmoothProgram.from[fixture.F](
      fixture.parameter,
      matrix(Vector(Vector(1.0), Vector(2.0))),
      id("too-many-anchor"),
      Vector.empty,
      Vector(direct, direct),
      Vector.empty
    )
    val wrongDimension = CompositeSparseSmoothProgram.from[fixture.F](
      fixture.parameter,
      matrix(Vector(Vector(1.0), Vector(2.0), Vector(3.0))),
      id("wrong-dimension-anchor"),
      Vector.empty,
      Vector(direct),
      Vector.empty
    )

    assertEquals(tooMany.left.toOption, Some(CompositePenaltyCompileError.UnsupportedProximalSum(2)))
    assert(wrongDimension.left.exists:
      case CompositePenaltyCompileError.InvalidDefinition(detail) => detail.contains("feature dimension")
      case _ => false
    )

  test("denominator loadings and unsupported split functionals are rejected explicitly"):
    val fixture = new Fixture("reject-semantics", QuadraticFamily.GraphSmoothness)
    val denominator = SmoothQuadraticPenalty.from(fixture.lower(0.5, QuadraticPlacement.DenominatorLoading))
    val huber = FunctionalKind.Huber(PenaltyWeight.unsafe(0.5))
    val huberPlan = fixture.compositePlan(huber, 0.25, "huber")
    val lowering = SplitL1Penalty.from(huberPlan)

    assertEquals(
      denominator.left.toOption,
      Some(CompositePenaltyCompileError.NonObjectiveQuadratic(QuadraticPlacement.DenominatorLoading))
    )
    assertEquals(
      lowering.left.toOption,
      Some(CompositePenaltyCompileError.UnsupportedSplitFunctional(huber))
    )
    val indefinite = acceptedSemantic(
      Lin.fromDenseMatrix(
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, -1.0))),
        CoordinateEvidence.dual(fixture.feature.evidence),
        CoordinateEvidence.primal(fixture.feature.evidence),
        id("indefinite-smoothing")
      )
    )
    assert(FormCertificates.psd(indefinite).isLeft)

  test("operator bounds are derived from operators and include zero and null-space cases"):
    val fixture = new Fixture("derived-bounds", QuadraticFamily.GraphSmoothness)
    val graph = fixture.smooth(0.5)
    val zero = fixture.zeroSmooth(0.5)
    val fit = solve(
      fixture,
      matrix(Vector(Vector(1.0), Vector(1.0))),
      Vector(graph),
      Vector(fixture.l1(0.25)),
      Vector.empty
    )

    assertEqualsDouble(graph.normBound.upperBound.doubleValue, 2.0, 1e-12)
    assertEquals(graph.normBound.evaluatedColumns, 2)
    assertEqualsDouble(zero.normBound.upperBound.doubleValue, 0.0, 0.0)
    assertMatrixClose(fit.parameter, matrix(Vector(Vector(0.75), Vector(0.75))), 2e-6)
    assert(fit.certificate.smoothLipschitzBound >= 1.0)

  test("one-dimensional and extreme-penalty limits obey the exact soft-threshold law"):
    val feature = space("one-dimensional-feature", 1)
    val coordinates = space("one-dimensional-coordinate", 1)
    val parameter = ParameterId.unsafe("one-dimensional-parameter")
    val chart = acceptedChart(
      FeatureChart.identity(
        feature.evidence,
        coordinates.evidence,
        Vector("only"),
        id("one-dimensional-chart")
      )
    )
    def direct(weight: Double): ExactDirectPenalty[feature.Id] =
      val term = PenaltyTerm(chart.target(parameter), FunctionalKind.L1, PenaltyWeight.unsafe(weight))
      ExactDirectPenalty.from(
        acceptedChart(DirectProximalPlan.from(term, chart, DirectProximalKind.ElementwiseL1))
      )
    def fit(weight: Double): CompositeSparseSmoothFit =
      val program = acceptedComposite(
        CompositeSparseSmoothProgram.from[feature.Id](
          parameter,
          matrix(Vector(Vector(3.0))),
          id(s"one-dimensional-anchor-$weight"),
          Vector.empty,
          Vector(direct(weight)),
          Vector.empty
        )
      )
      acceptedComposite(acceptedComposite(program.compile()).solve())

    assertEqualsDouble(fit(0.75).parameter(0, 0), 2.25, 2e-6)
    assertEqualsDouble(fit(1e6).parameter(0, 0), 0.0, 1e-12)

  private final class Fixture(val name: String, val family: QuadraticFamily):
    val feature = space(s"$name-feature", 2)
    val coordinates = space(s"$name-coordinates", 2)
    val edges = space(s"$name-edges", 1)
    type F = feature.Id
    type C = coordinates.Id
    type E = edges.Id

    val parameter: ParameterId = ParameterId.unsafe(s"$name-parameter")
    val chart: FeatureChart[F, C] = acceptedChart(
      FeatureChart.identity(
        feature.evidence,
        coordinates.evidence,
        Vector("left", "right"),
        id(s"$name-chart")
      )
    )
    val difference: Op[Dual[F], Primal[E], CrossOperatorRole, UncheckedEvidence] =
      crossOperator(
        feature.evidence,
        edges.evidence,
        matrix(Vector(Vector(-1.0, 1.0))),
        s"$name-difference"
      )
    val edgeMetric: Op[Primal[E], Dual[E], MetricOperatorRole, CertifiedSpd] =
      certifiedMetric(edges.evidence, DMat.eye(1), s"$name-edge-metric")

    def lower(weight: Double, placement: QuadraticPlacement): QuadraticLowering[F] =
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-smooth-target", difference)),
        FunctionalKind.SquaredNorm(edgeMetric.valueIdentity),
        PenaltyWeight.unsafe(weight)
      )
      acceptedQuadratic(QuadraticPullback.lower(term, difference, edgeMetric, family, placement))

    def smooth(weight: Double): SmoothQuadraticPenalty[F] =
      acceptedComposite(SmoothQuadraticPenalty.from(lower(weight, QuadraticPlacement.ObjectiveRidge)))

    def zeroSmooth(weight: Double): SmoothQuadraticPenalty[F] =
      val zeroTarget = crossOperator(
        feature.evidence,
        edges.evidence,
        matrix(Vector(Vector(0.0, 0.0))),
        s"$name-zero-target"
      )
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-zero-smooth", zeroTarget)),
        FunctionalKind.SquaredNorm(edgeMetric.valueIdentity),
        PenaltyWeight.unsafe(weight)
      )
      val lowering = acceptedQuadratic(
        QuadraticPullback.lower(term, zeroTarget, edgeMetric, family, QuadraticPlacement.ObjectiveRidge)
      )
      acceptedComposite(SmoothQuadraticPenalty.from(lowering))

    def l1(weight: Double): ExactDirectPenalty[F] =
      val term = PenaltyTerm(chart.target(parameter), FunctionalKind.L1, PenaltyWeight.unsafe(weight))
      val plan = acceptedChart(DirectProximalPlan.from(term, chart, DirectProximalKind.ElementwiseL1))
      ExactDirectPenalty.from(plan)

    def sparseGroup(weight: Double, l1Fraction: Double): ExactDirectPenalty[F] =
      val groups = acceptedChart(
        GroupStructure.from(
          chart,
          Vector(CoordinateGroup("all", IndexSet.unsafe(Vector(0, 1)))),
          id(s"$name-groups")
        )
      )
      val fraction = UnitFraction.unsafe(l1Fraction)
      val term = PenaltyTerm(
        chart.target(parameter),
        FunctionalKind.SparseGroup(fraction, groups.valueIdentity),
        PenaltyWeight.unsafe(weight)
      )
      val plan = acceptedChart(
        DirectProximalPlan.from(term, chart, DirectProximalKind.SparseGroup(fraction, groups))
      )
      ExactDirectPenalty.from(plan)

    def tv(weight: Double): SplitL1Penalty[F] =
      acceptedComposite(
        SplitL1Penalty.from(
          compositePlan(FunctionalKind.TotalVariation, weight, "tv")
        )
      )

    def selectedL1(index: Int, weight: Double, suffix: String): SplitL1Penalty[F] =
      val target = space(s"$name-$suffix-target", 1)
      val values =
        if index == 0 then matrix(Vector(Vector(1.0, 0.0)))
        else matrix(Vector(Vector(0.0, 1.0)))
      val operator = crossOperator(
        feature.evidence,
        target.evidence,
        values,
        s"$name-$suffix-selection"
      )
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-$suffix-target-expression", operator)),
        FunctionalKind.L1,
        PenaltyWeight.unsafe(weight)
      )
      val plan = acceptedLowering(
        CompositePenaltyPlan.from(
          term,
          operator,
          AuxiliaryVariableId.unsafe(s"$name-$suffix-auxiliary"),
          SplitRequest.Automatic,
          SplitSolverCapabilities.portableReference
        )
      )
      acceptedComposite(SplitL1Penalty.from(plan))

    def compositePlan(
        functional: FunctionalKind,
        weight: Double,
        suffix: String
    ): CompositePenaltyPlan[F, E] =
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-$suffix-target", difference)),
        functional,
        PenaltyWeight.unsafe(weight)
      )
      acceptedLowering(
        CompositePenaltyPlan.from(
          term,
          difference,
          AuxiliaryVariableId.unsafe(s"$name-$suffix-auxiliary"),
          SplitRequest.Automatic,
          SplitSolverCapabilities.portableReference
        )
      )

  private def solve[Feature <: SemanticSpace](
      fixture: Fixture { type F = Feature },
      anchor: DMat,
      smooth: Vector[SmoothQuadraticPenalty[Feature]],
      direct: Vector[ExactDirectPenalty[Feature]],
      split: Vector[SplitL1Penalty[Feature]]
  ): CompositeSparseSmoothFit =
    val program = acceptedComposite(
      CompositeSparseSmoothProgram.from(
        fixture.parameter,
        anchor,
        id(s"${fixture.name}-anchor"),
        smooth,
        direct,
        split
      )
    )
    acceptedComposite(acceptedComposite(program.compile()).solve())

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

  private def crossOperator[Source <: SemanticSpace, Target <: SemanticSpace](
      source: SpaceEvidence[Source],
      target: SpaceEvidence[Target],
      value: DMat,
      name: String
  ): Op[Dual[Source], Primal[Target], CrossOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(source),
        CoordinateEvidence.primal(target),
        OperatorRoleWitness.cross,
        id(name)
      )
    )

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

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

  private def acceptedComposite[A](value: Either[CompositePenaltyCompileError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedQuadratic[A](value: Either[QuadraticLoweringError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedLowering[A](value: Either[CompositeLoweringError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedChart[A](value: Either[ChartError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedProgram[A](value: Either[ProgramError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)

  private def independentSparseSmoothObjective(first: Double, second: Double): Double =
    val difference = first - second
    0.5 * ((first - 2.0) * (first - 2.0) + second * second) +
      0.5 * difference * difference +
      0.25 * (Math.abs(first) + Math.abs(second)) +
      0.25 * Math.abs(difference)
