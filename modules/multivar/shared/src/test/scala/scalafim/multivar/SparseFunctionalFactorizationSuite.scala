package scalafim.multivar

import gale.linalg.DMat

class SparseFunctionalFactorizationSuite extends munit.FunSuite:

  test("unpenalized Euclidean rank one reduces to the ordinary leading SVD factor"):
    val fixture = new Fixture("ordinary-pca", matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 2.0))))
    val program = fixture.program(fixture.rowEuclidean, fixture.columnEuclidean)
    val positive = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(1.0))),
        fixture.columns.evidence,
        id("ordinary-init")
      )
    )
    val negative = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(-1.0), Vector(-1.0))),
        fixture.columns.evidence,
        id("ordinary-negative-init")
      )
    )
    val fit = accepted(program.solve(positive))
    val signEquivalent = accepted(program.solve(negative))

    assertMatrixClose(dense(fit.rowFactor), matrix(Vector(Vector(1.0), Vector(0.0))), 2e-6)
    assertMatrixClose(dense(fit.columnFactor), matrix(Vector(Vector(1.0), Vector(0.0))), 2e-6)
    assertEqualsDouble(fit.strength, 3.0, 2e-6)
    assertMatrixClose(dense(fit.reconstruction), matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 0.0))), 3e-6)
    assertMatrixClose(dense(signEquivalent.rowFactor), dense(fit.rowFactor), 2e-6)
    assertMatrixClose(dense(signEquivalent.columnFactor), dense(fit.columnFactor), 2e-6)
    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.CoordinatewiseStationary)
    assert(fit.certificate.objectiveHistory.sliding(2).forall:
      case Vector(previous, next) => next + 1e-10 >= previous
      case _ => true
    )

  test("certified row and column metrics reduce to the generalized matrix decomposition"):
    val fixture = new Fixture("generalized-gmd", matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))))
    val row = fixture.rowGeneralized(diagonal(Vector(4.0, 1.0)), "gmd-row-metric")
    val column = fixture.columnGeneralized(diagonal(Vector(1.0, 9.0)), "gmd-column-metric")
    val program = fixture.program(row, column)
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(1.0))),
        fixture.columns.evidence,
        id("gmd-init")
      )
    )
    val fit = accepted(program.solve(initialization))
    val u = dense(fit.rowFactor)
    val v = dense(fit.columnFactor)

    assertMatrixClose(u, matrix(Vector(Vector(0.5), Vector(0.0))), 3e-6)
    assertMatrixClose(v, matrix(Vector(Vector(1.0), Vector(0.0))), 3e-6)
    assertEqualsDouble(fit.strength, 4.0, 4e-6)
    assertEqualsDouble(metricNorm(u, row.operator), 1.0, 2e-6)
    assertEqualsDouble(metricNorm(v, column.operator), 1.0, 2e-6)

  test("quadratic smoothness is part of the factor geometry and functional-PCA limit"):
    val fixture = new Fixture("functional-pca", matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 1.0))))
    val smoothColumn = fixture.smoothedColumn(fixture.columnEuclidean, weight = 10.0)
    val program = fixture.program(fixture.rowEuclidean, smoothColumn)
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(0.2))),
        fixture.columns.evidence,
        id("functional-init")
      )
    )
    val fit = accepted(program.solve(initialization))
    val column = dense(fit.columnFactor)

    assertEquals(smoothColumn.smoothness.map(_.family), Vector(QuadraticFamily.DerivativeSmoothness))
    assertEqualsDouble(metricNorm(column, smoothColumn.operator), 1.0, 2e-6)
    assert(Math.abs(column(0, 0) - column(1, 0)) < 0.35)
    assert(fit.certificate.column.regressionGuarantee.claimClass == OptimizationClaimClass.EpsilonGlobal)

  test("degree-one l1 factors reduce to the ordinary sparse-PCA limit"):
    val fixture = new Fixture("sparse-pca", matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 1.0))))
    val program = fixture.program(
      fixture.rowEuclidean,
      fixture.columnEuclidean,
      fixture.rowL1(0.25),
      fixture.columnL1(0.25)
    )
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(0.1))),
        fixture.columns.evidence,
        id("sparse-pca-init")
      )
    )
    val fit = accepted(program.solve(initialization))

    assertMatrixClose(dense(fit.rowFactor), matrix(Vector(Vector(1.0), Vector(0.0))), 2e-6)
    assertMatrixClose(dense(fit.columnFactor), matrix(Vector(Vector(1.0), Vector(0.0))), 2e-6)
    assertEqualsDouble(fit.strength, 3.0, 2e-6)
    assert(fit.certificate.coordinateResidual <= 3e-6)

  test("two-way sparse-functional factors combine exact l1 blocks with smooth geometries"):
    val fixture = new Fixture(
      "two-way-sparse-functional",
      matrix(Vector(Vector(2.0, 0.0), Vector(2.0, 0.0)))
    )
    val rowGeometry = fixture.smoothedRow(fixture.rowEuclidean, weight = 2.0)
    val columnGeometry = fixture.smoothedColumn(fixture.columnEuclidean, weight = 0.25)
    val rowPenalties = fixture.rowL1(0.2)
    val columnPenalties = fixture.columnL1(0.5)
    val program = fixture.program(rowGeometry, columnGeometry, rowPenalties, columnPenalties)
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(0.1))),
        fixture.columns.evidence,
        id("two-way-init")
      )
    )
    val fit = accepted(program.solve(initialization))
    val row = dense(fit.rowFactor)
    val column = dense(fit.columnFactor)

    assertEqualsDouble(row(0, 0), row(1, 0), 3e-5)
    assert(Math.abs(column(1, 0)) <= 1e-8)
    assertEqualsDouble(metricNorm(row, rowGeometry.operator), 1.0, 3e-6)
    assertEqualsDouble(metricNorm(column, columnGeometry.operator), 1.0, 3e-6)
    assert(fit.certificate.coordinateResidual <= 5e-6)

  test("deterministic sparse-functional simulation recovers the planted rank-one directions"):
    val fixture = new Fixture(
      "sparse-functional-recovery",
      matrix(
        Vector(
          Vector(2.15, 1.95, 0.02, -0.01),
          Vector(1.98, 2.08, -0.02, 0.01),
          Vector(2.04, 1.96, 0.01, 0.02),
          Vector(1.93, 2.06, -0.01, -0.02)
        )
      )
    )
    val rowGeometry = fixture.smoothedRow(fixture.rowEuclidean, weight = 0.25)
    val columnGeometry = fixture.smoothedColumn(fixture.columnEuclidean, weight = 0.1)
    val program = fixture.program(
      rowGeometry,
      columnGeometry,
      fixture.rowL1(0.05),
      fixture.columnL1(0.15)
    )
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(0.8), Vector(0.1), Vector(0.1))),
        fixture.columns.evidence,
        id("sparse-functional-recovery-init")
      )
    )
    val fit = accepted(program.solve(initialization))
    val plantedRow = matrix(Vector.fill(4)(Vector(0.5)))
    val plantedColumn = matrix(
      Vector(Vector(1.0 / Math.sqrt(2.0)), Vector(1.0 / Math.sqrt(2.0)), Vector(0.0), Vector(0.0))
    )

    assert(cosine(dense(fit.rowFactor), plantedRow) >= 0.995)
    assert(cosine(dense(fit.columnFactor), plantedColumn) >= 0.995)
    assert(Math.abs(dense(fit.columnFactor)(2, 0)) <= 1e-8)
    assert(Math.abs(dense(fit.columnFactor)(3, 0)) <= 1e-8)
    assert(fit.certificate.coordinateResidual <= 5e-6)

  test("total variation can be applied independently on both factor sides"):
    val fixture = new Fixture("two-way-tv", matrix(Vector(Vector(2.0, 1.0), Vector(2.0, 1.0))))
    val program = fixture.program(
      fixture.rowEuclidean,
      fixture.columnEuclidean,
      fixture.rowTv(0.25),
      fixture.columnTv(0.25)
    )
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(0.5))),
        fixture.columns.evidence,
        id("tv-init")
      )
    )
    val fit = accepted(program.solve(initialization))

    assert(fit.certificate.row.regression.objective.split.nonEmpty)
    assert(fit.certificate.column.regression.objective.split.nonEmpty)
    assert(fit.certificate.row.regression.dualFeasibilityResidual <= 1e-10)
    assert(fit.certificate.column.regression.dualFeasibilityResidual <= 1e-10)

  test("strong sparsity has an explicit well-behaved zero solution"):
    val fixture = new Fixture("zero-solution", matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 2.0))))
    val program = fixture.program(
      fixture.rowEuclidean,
      fixture.columnEuclidean,
      fixture.rowL1(10.0),
      fixture.columnL1(10.0)
    )
    val initialization = acceptedInitialization(
      RankOneFactorInitialization.from[fixture.C](
        matrix(Vector(Vector(1.0), Vector(1.0))),
        fixture.columns.evidence,
        id("zero-init")
      )
    )
    val fit = accepted(program.solve(initialization))

    assertEquals(fit.status, RankOneFactorStatus.ZeroSolution)
    assertEqualsDouble(fit.strength, 0.0, 0.0)
    assertMatrixClose(dense(fit.reconstruction), DMat.zeros(2, 2), 0.0)
    assertEquals(fit.certificate.stopping, AlternatingFactorizationStopping.ZeroSolution)
    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.CoordinatewiseStationary)

  test("greedy deflation and simultaneous joint rank-k remain different estimands"):
    val fixture = new Fixture("rank-k-boundary", matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 2.0))))
    val rankOne = fixture.program(fixture.rowEuclidean, fixture.columnEuclidean)
    val rank = ComponentCount.unsafe(2)
    val greedy = accepted(GreedyStructuredFactorization.from(rankOne, rank))
    val joint = accepted(JointRankKStructuredFactorization.from(rankOne, rank))
    val initializations = Vector(
      acceptedInitialization(
        RankOneFactorInitialization.from[fixture.C](
          matrix(Vector(Vector(1.0), Vector(0.0))),
          fixture.columns.evidence,
          id("rank-k-first")
        )
      ),
      acceptedInitialization(
        RankOneFactorInitialization.from[fixture.C](
          matrix(Vector(Vector(0.0), Vector(1.0))),
          fixture.columns.evidence,
          id("rank-k-second")
        )
      )
    )
    val fit = accepted(greedy.solve(initializations))
    val jointFit = accepted(joint.solveExact())

    assertEquals(fit.estimand, StructuredRankEstimand.GreedyDeflation)
    assertEquals(fit.ordering, FactorOrderingConvention.ExtractionOrder)
    assertEquals(fit.orthogonality, MetricOrthogonalityConvention.GreedyDeflationNoOrthogonalityClaim)
    assertEquals(joint.estimand, StructuredRankEstimand.JointRankK)
    assertEquals(joint.orthogonality, MetricOrthogonalityConvention.GeneralizedStiefel)
    assertEquals(jointFit.estimand, StructuredRankEstimand.JointRankK)
    assertEquals(jointFit.achievement.claimClass, OptimizationClaimClass.ExactGlobal)
    assertEqualsDouble(fit.components(0).strength, 3.0, 2e-6)
    assertEqualsDouble(fit.components(1).strength, 2.0, 2e-6)
    assertEqualsDouble(jointFit.strengths(0), 3.0, 2e-6)
    assertEqualsDouble(jointFit.strengths(1), 2.0, 2e-6)
    assert(jointFit.certificate.residual <= 2e-8)
    assertMatrixClose(dense(fit.reconstruction), fixture.values, 3e-6)
    assertMatrixClose(dense(jointFit.reconstruction), fixture.values, 3e-6)

  test("joint rank-k exact reduction rejects nonsmooth penalties instead of impersonating deflation"):
    val fixture = new Fixture("rank-k-nonsmooth-boundary", DMat.eye(2))
    val rankOne = fixture.program(
      fixture.rowEuclidean,
      fixture.columnEuclidean,
      fixture.rowL1(0.1),
      fixture.columnL1(0.1)
    )
    val joint = accepted(JointRankKStructuredFactorization.from(rankOne, ComponentCount.unsafe(2)))

    assert(joint.solveExact().left.exists:
      case SparseFunctionalFactorizationError.InvalidDefinition(detail) => detail.contains("no nonsmooth")
      case _ => false
    )

  test("non-homogeneous block penalties are rejected while sparse-group is degree one"):
    val fixture = new Fixture("homogeneity-boundary", DMat.eye(2))
    val elastic = fixture.columnElasticNet(0.5, 0.5)
    val sparseGroup = fixture.columnSparseGroup(0.5, 0.5)

    assert(elastic.left.exists:
      case SparseFunctionalFactorizationError.InvalidDefinition(detail) => detail.contains("degree-one")
      case _ => false
    )
    assert(sparseGroup.isRight)
    assertEquals(
      FunctionalKind.SparseGroup(UnitFraction.unsafe(0.5), fixture.columnGroups.valueIdentity).traits.homogeneity,
      HomogeneityTrait.DegreeOne
    )

  private final class Fixture(val name: String, val values: DMat):
    val rows = space(s"$name-rows", values.rows)
    val columns = space(s"$name-columns", values.cols)
    type R = rows.Id
    type C = columns.Id

    val rowParameter: ParameterId = ParameterId.unsafe(s"$name-row-factor")
    val columnParameter: ParameterId = ParameterId.unsafe(s"$name-column-factor")
    val data: OpTable[R, C, UncheckedEvidence] = acceptedSemantic(
      Op.fromDense(
        values,
        CoordinateEvidence.dual(columns.evidence),
        CoordinateEvidence.primal(rows.evidence),
        OperatorRoleWitness.table,
        id(s"$name-data")
      )
    )
    val rowEuclidean: StructuredFactorGeometry[R] =
      accepted(StructuredFactorGeometry.euclidean(rows.evidence, id(s"$name-row-euclidean")))
    val columnEuclidean: StructuredFactorGeometry[C] =
      accepted(StructuredFactorGeometry.euclidean(columns.evidence, id(s"$name-column-euclidean")))
    val rowChart = acceptedChart(
      FeatureChart.identity(
        rows.evidence,
        space(s"$name-row-chart-coordinates", values.rows).evidence,
        Vector.tabulate(values.rows)(index => s"row-$index"),
        id(s"$name-row-chart")
      )
    )
    val columnChart = acceptedChart(
      FeatureChart.identity(
        columns.evidence,
        space(s"$name-column-chart-coordinates", values.cols).evidence,
        Vector.tabulate(values.cols)(index => s"column-$index"),
        id(s"$name-column-chart")
      )
    )
    val columnGroups = acceptedChart(
      GroupStructure.from(
        columnChart,
        Vector(CoordinateGroup("all-columns", IndexSet.unsafe(Vector.range(0, values.cols)))),
        id(s"$name-column-groups")
      )
    )

    def program(
        rowGeometry: StructuredFactorGeometry[R],
        columnGeometry: StructuredFactorGeometry[C],
        rowPenalties: HomogeneousFactorPenalties[R] = HomogeneousFactorPenalties.empty(rowParameter),
        columnPenalties: HomogeneousFactorPenalties[C] = HomogeneousFactorPenalties.empty(columnParameter)
    ): RankOneStructuredFactorization[R, C, UncheckedEvidence] =
      accepted(
        RankOneStructuredFactorization.from(
          data,
          rowGeometry,
          columnGeometry,
          rowPenalties,
          columnPenalties
        )
      )

    def rowGeneralized(value: DMat, suffix: String): StructuredFactorGeometry[R] =
      StructuredFactorGeometry.generalized(certifiedMetric(rows.evidence, value, suffix))

    def columnGeneralized(value: DMat, suffix: String): StructuredFactorGeometry[C] =
      StructuredFactorGeometry.generalized(certifiedMetric(columns.evidence, value, suffix))

    def smoothedRow(
        base: StructuredFactorGeometry[R],
        weight: Double
    ): StructuredFactorGeometry[R] =
      accepted(StructuredFactorGeometry.addSmoothness(base, rowSmoothness(weight)))

    def smoothedColumn(
        base: StructuredFactorGeometry[C],
        weight: Double
    ): StructuredFactorGeometry[C] =
      accepted(StructuredFactorGeometry.addSmoothness(base, columnSmoothness(weight)))

    def rowL1(weight: Double): HomogeneousFactorPenalties[R] =
      val term = PenaltyTerm(rowChart.target(rowParameter), FunctionalKind.L1, PenaltyWeight.unsafe(weight))
      val direct = ExactDirectPenalty.from(
        acceptedChart(DirectProximalPlan.from(term, rowChart, DirectProximalKind.ElementwiseL1))
      )
      accepted(
        HomogeneousFactorPenalties.from[R](
          rowParameter,
          direct = Vector(direct),
          split = Vector.empty[SplitL1Penalty[R]]
        )
      )

    def columnL1(weight: Double): HomogeneousFactorPenalties[C] =
      val term = PenaltyTerm(columnChart.target(columnParameter), FunctionalKind.L1, PenaltyWeight.unsafe(weight))
      val direct = ExactDirectPenalty.from(
        acceptedChart(DirectProximalPlan.from(term, columnChart, DirectProximalKind.ElementwiseL1))
      )
      accepted(
        HomogeneousFactorPenalties.from[C](
          columnParameter,
          direct = Vector(direct),
          split = Vector.empty[SplitL1Penalty[C]]
        )
      )

    def rowTv(weight: Double): HomogeneousFactorPenalties[R] =
      accepted(
        HomogeneousFactorPenalties.from[R](
          rowParameter,
          direct = Vector.empty[ExactDirectPenalty[R]],
          split = Vector(tv(rows.evidence, rowParameter, weight, "row"))
        )
      )

    def columnTv(weight: Double): HomogeneousFactorPenalties[C] =
      accepted(
        HomogeneousFactorPenalties.from[C](
          columnParameter,
          direct = Vector.empty[ExactDirectPenalty[C]],
          split = Vector(tv(columns.evidence, columnParameter, weight, "column"))
        )
      )

    def columnElasticNet(
        weight: Double,
        fractionValue: Double
    ): Either[SparseFunctionalFactorizationError, HomogeneousFactorPenalties[C]] =
      val fraction = UnitFraction.unsafe(fractionValue)
      val term = PenaltyTerm(
        columnChart.target(columnParameter),
        FunctionalKind.ElasticNet(fraction),
        PenaltyWeight.unsafe(weight)
      )
      val direct = ExactDirectPenalty.from(
        acceptedChart(DirectProximalPlan.from(term, columnChart, DirectProximalKind.ElasticNet(fraction)))
      )
      HomogeneousFactorPenalties.from[C](
        columnParameter,
        direct = Vector(direct),
        split = Vector.empty[SplitL1Penalty[C]]
      )

    def columnSparseGroup(
        weight: Double,
        fractionValue: Double
    ): Either[SparseFunctionalFactorizationError, HomogeneousFactorPenalties[C]] =
      val fraction = UnitFraction.unsafe(fractionValue)
      val term = PenaltyTerm(
        columnChart.target(columnParameter),
        FunctionalKind.SparseGroup(fraction, columnGroups.valueIdentity),
        PenaltyWeight.unsafe(weight)
      )
      val direct = ExactDirectPenalty.from(
        acceptedChart(
          DirectProximalPlan.from(
            term,
            columnChart,
            DirectProximalKind.SparseGroup(fraction, columnGroups)
          )
        )
      )
      HomogeneousFactorPenalties.from[C](
        columnParameter,
        direct = Vector(direct),
        split = Vector.empty[SplitL1Penalty[C]]
      )

    private def rowSmoothness(weight: Double): QuadraticLowering[R] =
      smoothness(rows.evidence, rowParameter, weight, "row")

    private def columnSmoothness(weight: Double): QuadraticLowering[C] =
      smoothness(columns.evidence, columnParameter, weight, "column")

    private def smoothness[S <: SemanticSpace](
        source: SpaceEvidence[S],
        parameter: ParameterId,
        weight: Double,
        suffix: String
    ): QuadraticLowering[S] =
      val edges = space(s"$name-$suffix-smoothness-edges", source.dimension - 1)
      val difference = differenceOperator(source, edges.evidence, s"$name-$suffix-smoothness")
      val geometry = certifiedMetric(edges.evidence, DMat.eye(edges.evidence.dimension), s"$name-$suffix-edge-metric")
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-$suffix-smoothness-target", difference)),
        FunctionalKind.SquaredNorm(geometry.valueIdentity),
        PenaltyWeight.unsafe(weight)
      )
      acceptedQuadratic(
        QuadraticPullback.lower(
          term,
          difference,
          geometry,
          QuadraticFamily.DerivativeSmoothness,
          QuadraticPlacement.ObjectiveRidge
        )
      )

    private def tv[S <: SemanticSpace](
        source: SpaceEvidence[S],
        parameter: ParameterId,
        weight: Double,
        suffix: String
    ): SplitL1Penalty[S] =
      val edges = space(s"$name-$suffix-tv-edges", source.dimension - 1)
      val difference = differenceOperator(source, edges.evidence, s"$name-$suffix-tv")
      val term = PenaltyTerm(
        acceptedProgram(TargetExpression.linear(parameter, s"$name-$suffix-tv-target", difference)),
        FunctionalKind.TotalVariation,
        PenaltyWeight.unsafe(weight)
      )
      val plan = acceptedLowering(
        CompositePenaltyPlan.from(
          term,
          difference,
          AuxiliaryVariableId.unsafe(s"$name-$suffix-tv-auxiliary"),
          SplitRequest.Automatic,
          SplitSolverCapabilities.portableReference
        )
      )
      acceptedComposite(SplitL1Penalty.from(plan))

  private def differenceOperator[Source <: SemanticSpace, Edge <: SemanticSpace](
      source: SpaceEvidence[Source],
      edges: SpaceEvidence[Edge],
      name: String
  ): Op[Dual[Source], Primal[Edge], CrossOperatorRole, UncheckedEvidence] =
    val values = Vector.tabulate(edges.dimension): row =>
      Vector.tabulate(source.dimension): column =>
        if column == row then -1.0
        else if column == row + 1 then 1.0
        else 0.0
    acceptedSemantic(
      Op.fromDense(
        matrix(values),
        CoordinateEvidence.dual(source),
        CoordinateEvidence.primal(edges),
        OperatorRoleWitness.cross,
        id(name)
      )
    )

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): OpMetric[S, CertifiedSpd] =
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

  private def metricNorm[S <: SemanticSpace](value: DMat, geometry: OpMetric[S, CertifiedSpd]): Double =
    val image = acceptedSemantic(geometry(value))
    var squared = 0.0
    var row = 0
    while row < value.rows do
      squared += value(row, 0) * image(row, 0)
      row += 1
    Math.sqrt(squared)

  private def cosine(left: DMat, right: DMat): Double =
    var cross = 0.0
    var leftSquared = 0.0
    var rightSquared = 0.0
    var row = 0
    while row < left.rows do
      cross += left(row, 0) * right(row, 0)
      leftSquared += left(row, 0) * left(row, 0)
      rightSquared += right(row, 0) * right(row, 0)
      row += 1
    Math.abs(cross) / Math.sqrt(leftSquared * rightSquared)

  private def dense[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](operator: Op[From, To, R, E]): DMat =
    acceptedSemantic(operator.toDense)

  private def diagonal(values: Vector[Double]): DMat =
    matrix(
      values.indices.toVector.map: row =>
        values.indices.toVector.map: column =>
          if row == column then values(row) else 0.0
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

  private def accepted[A](value: Either[SparseFunctionalFactorizationError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedInitialization[A](value: Either[SparseFunctionalFactorizationError, A]): A =
    accepted(value)

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
