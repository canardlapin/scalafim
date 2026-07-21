package scalafim.multivar

import gale.linalg.DMat

class SparsitySuite extends munit.FunSuite:

  test("elementwise l1 and row l21 match frozen R reference values"):
    val fixture = chartFixture()
    val input = matrix(SparsityRReferenceFixtures.coefficients)
    val l1 = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.L1, 1.0),
        fixture.chart,
        DirectProximalKind.ElementwiseL1
      )
    )
    val l21 = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.GroupL21, 1.0),
        fixture.chart,
        DirectProximalKind.FeatureRowsL21
      )
    )

    assertMatrixClose(accepted(l1(input, PenaltyWeight.unsafe(1.0))), matrix(SparsityRReferenceFixtures.l1AtOne), 1e-12)
    assertMatrixClose(accepted(l21(input, PenaltyWeight.unsafe(1.0))), matrix(SparsityRReferenceFixtures.rowL21AtOne), 1e-12)

  test("l1 proximal output satisfies the exact convex KKT oracle"):
    val fixture = chartFixture()
    val input = matrix(SparsityRReferenceFixtures.coefficients)
    val plan = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.L1, 1.0),
        fixture.chart,
        DirectProximalKind.ElementwiseL1
      )
    )
    val solution = accepted(plan(input, PenaltyWeight.unsafe(1.0)))

    var row = 0
    while row < input.rows do
      var column = 0
      while column < input.cols do
        val x = input(row, column)
        val y = solution(row, column)
        if y == 0.0 then assert(Math.abs(x) <= 1.0)
        else assertEqualsDouble(y - x + Math.signum(y), 0.0, 1e-12)
        column += 1
      row += 1

  test("disjoint groups, sparse-group, and elastic-net use valid separable proximal laws"):
    val fixture = chartFixture()
    val groups = accepted(
      GroupStructure.from(
        fixture.chart,
        Vector(
          CoordinateGroup("first", IndexSet.unsafe(Vector(0, 1))),
          CoordinateGroup("second", IndexSet.unsafe(Vector(2)))
        ),
        id("disjoint-groups")
      )
    )
    val grouped = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.GroupL2(groups.valueIdentity), 1.0),
        fixture.chart,
        DirectProximalKind.DisjointGroups(groups)
      )
    )
    val fraction = UnitFraction.unsafe(0.5)
    val sparseGroup = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.SparseGroup(fraction, groups.valueIdentity), 1.0),
        fixture.chart,
        DirectProximalKind.SparseGroup(fraction, groups)
      )
    )
    val elastic = accepted(
      DirectProximalPlan.from(
        penalty(fixture.chart, FunctionalKind.ElasticNet(fraction), 1.0),
        fixture.chart,
        DirectProximalKind.ElasticNet(fraction)
      )
    )
    val groupedResult = accepted(grouped(matrix(Vector(Vector(3.0), Vector(4.0), Vector(2.0))), PenaltyWeight.unsafe(1.0)))
    val sparseResult = accepted(sparseGroup(matrix(Vector(Vector(3.0), Vector(4.0), Vector(2.0))), PenaltyWeight.unsafe(1.0)))
    val elasticResult = accepted(elastic(matrix(Vector(Vector(3.0), Vector(-1.0), Vector(0.0))), PenaltyWeight.unsafe(1.0)))

    assertMatrixClose(groupedResult, matrix(Vector(Vector(2.4), Vector(3.2), Vector(1.0))), 1e-12)
    assertEqualsDouble(sparseResult(2, 0), 1.0, 1e-12)
    assertEqualsDouble(sparseResult(0, 0) / sparseResult(1, 0), 2.5 / 3.5, 1e-12)
    assertMatrixClose(elasticResult, matrix(Vector(Vector(5.0 / 3.0), Vector(-1.0 / 3.0), Vector(0.0))), 1e-12)

  test("simplex and monotone projections match R reference fixtures"):
    val fixture = chartFixture()
    val simplex = accepted(
      DirectProjectionPlan.from(
        constraint(fixture.chart, FeasibleSetKind.Simplex),
        fixture.chart
      )
    )
    val monotone = accepted(
      DirectProjectionPlan.from(
        constraint(fixture.chart, FeasibleSetKind.Monotone(id("feature-order"))),
        fixture.chart
      )
    )

    assertMatrixClose(accepted(simplex(matrix(SparsityRReferenceFixtures.simplexInput))), matrix(SparsityRReferenceFixtures.simplexProjection), 1e-12)
    assertMatrixClose(accepted(monotone(matrix(SparsityRReferenceFixtures.monotoneInput))), matrix(SparsityRReferenceFixtures.monotoneProjection), 1e-12)

  test("chart identity mismatch and overlapping groups reject direct lowering"):
    val fixture = chartFixture()
    val foreignCoordinates = SpaceRef(
      MvSpace(SpaceId.unsafe("foreign-sparse-coordinates"), SpaceRole.Observed, Dimension.unsafe(3))
    )
    val foreignChart = accepted(
      FeatureChart.identity(
        fixture.chart.featureSpace,
        foreignCoordinates.evidence,
        fixture.chart.featureIds,
        id("foreign-chart")
      )
    )
    val foreign = accepted(
      GroupStructure.from(
        foreignChart,
        Vector(CoordinateGroup("all", IndexSet.unsafe(Vector(0, 1, 2)))),
        id("foreign-groups")
      )
    )
    val overlapping = accepted(
      GroupStructure.from(
        fixture.chart,
        Vector(
          CoordinateGroup("left", IndexSet.unsafe(Vector(0, 1))),
          CoordinateGroup("right", IndexSet.unsafe(Vector(1, 2)))
        ),
        id("overlapping-groups")
      )
    )
    val foreignResult = DirectProximalPlan.from(
      penalty(fixture.chart, FunctionalKind.GroupL2(foreign.valueIdentity), 1.0),
      fixture.chart,
      DirectProximalKind.DisjointGroups(foreign)
    )
    val overlapResult = DirectProximalPlan.from(
      penalty(fixture.chart, FunctionalKind.GroupL2(overlapping.valueIdentity), 1.0),
      fixture.chart,
      DirectProximalKind.DisjointGroups(overlapping)
    )

    assert(foreignResult.left.exists(_.isInstanceOf[ChartError.FeatureIdentityMismatch]))
    assert(overlapResult.left.exists(_.isInstanceOf[ChartError.GroupOverlapUnsupported]))

  test("general charts cannot claim a direct proximal or projection lowering"):
    val fixture = chartFixture()
    val general = accepted(
      FeatureChart.certified(
        fixture.chart.featureSpace,
        fixture.chart.coordinateSpace,
        fixture.chart.featureIds,
        id("general-chart"),
        ChartKind.General,
        fixture.chart.forward,
        fixture.chart.synthesis
      )
    )
    val result = DirectProximalPlan.from(
      penalty(general, FunctionalKind.L1, 1.0),
      general,
      DirectProximalKind.ElementwiseL1
    )

    assert(result.left.exists(_.isInstanceOf[ChartError.UnsupportedDirectLowering]))

  test("selection-chart proximal lowering preserves unselected coordinates"):
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("selection-feature"), SpaceRole.Observed, Dimension.unsafe(3)))
    val coordinates = SpaceRef(MvSpace(SpaceId.unsafe("selection-coordinates"), SpaceRole.Observed, Dimension.unsafe(2)))
    val chart = accepted(
      FeatureChart.selection(
        feature.evidence,
        coordinates.evidence,
        Vector("a", "b", "c"),
        IndexSet.unsafe(Vector(0, 2)),
        id("selection-chart")
      )
    )
    val plan = accepted(
      DirectProximalPlan.from(
        penalty(chart, FunctionalKind.L1, 1.0),
        chart,
        DirectProximalKind.ElementwiseL1
      )
    )
    val result = accepted(plan(matrix(Vector(Vector(3.0), Vector(100.0), Vector(-4.0))), PenaltyWeight.unsafe(1.0)))

    assertMatrixClose(result, matrix(Vector(Vector(2.0), Vector(100.0), Vector(-3.0))), 1e-12)

  private final class ChartFixture(
      val feature: SpaceRef,
      val coordinates: SpaceRef,
      val chart: FeatureChart[feature.Id, coordinates.Id]
  )

  private def chartFixture(): ChartFixture =
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("sparse-feature"), SpaceRole.Observed, Dimension.unsafe(3)))
    val coordinates = SpaceRef(MvSpace(SpaceId.unsafe("sparse-coordinates"), SpaceRole.Observed, Dimension.unsafe(3)))
    val chart = accepted(
      FeatureChart.identity(
        feature.evidence,
        coordinates.evidence,
        Vector("a", "b", "c"),
        id("identity-feature-chart")
      )
    )
    new ChartFixture(feature, coordinates, chart)

  private def penalty[F <: SemanticSpace, C <: SemanticSpace](
      chart: FeatureChart[F, C],
      functional: FunctionalKind,
      weight: Double
  ): PenaltyTerm =
    PenaltyTerm(chart.target(ParameterId.unsafe("w")), functional, PenaltyWeight.unsafe(weight))

  private def constraint[F <: SemanticSpace, C <: SemanticSpace](
      chart: FeatureChart[F, C],
      feasibleSet: FeasibleSetKind
  ): ConstraintTerm =
    ConstraintTerm(chart.target(ParameterId.unsafe("w")), feasibleSet)

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

  private def accepted[A](value: Either[ChartError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
