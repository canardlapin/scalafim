package scalafim.connectivity.scenarios

import scalafim.connectivity.*
import scalafim.connectivity.fixtures.AriadneCoreFixtures
import gale.linalg.{DMat, Matrix}

class AriadneCorLwScenarioSuite extends munit.FunSuite:

  test("Ariadne weighted cor_lw analysis runs through the typed static connectivity framework") {
    val nodeAxis =
      NodeAxis.fromIdsAndLabels(
        Vector("node-1", "node-2", "node-3").map(NodeId.unsafe),
        Vector("node-1", "node-2", "node-3")
      ).toOption.get
    val timeAxis =
      TimeAxis.fromSeconds(AriadneCoreFixtures.weightedInputRows.length, trSeconds = 0.8).toOption.get
    val series =
      ParcelTimeSeries.from(
        GaleTestMatrix.fromRows(AriadneCoreFixtures.weightedInputRows),
        nodeAxis,
        timeAxis
      ).toOption.get
    val weights =
      FrameWeights.from(AriadneCoreFixtures.frameWeights, timeAxis).toOption.get

    val connectivity =
      ConnectivityEstimators.diagonalShrinkageCorrelation(
        series,
        frameWeights = Some(weights),
        order = VectorizationOrder.AriadneCompatible
      ).toOption.get

    assertEquals(connectivity.estimator.map(_.name), Some("diagonal-shrinkage-correlation"))
    assert(connectivity.estimator.exists(_.requirements.contains(EstimatorRequirement.FrameWeights)))
    assertEqualsDouble(connectivity.diagnostics.metrics("shrinkage.alpha"), 0.2661156233334778, 1e-12)
    assertMatrixClose(connectivity.matrix.values, AriadneCoreFixtures.diagonalShrinkageCorrelation, 1e-12)

    val ariadneEdges = connectivity.matrix.edgeVector.toOption.get.toVector
    assertEqualsDouble(ariadneEdges(0), AriadneCoreFixtures.diagonalShrinkageCorrelation(0)(1), 1e-12)
    assertEqualsDouble(ariadneEdges(1), AriadneCoreFixtures.diagonalShrinkageCorrelation(0)(2), 1e-12)
    assertEqualsDouble(ariadneEdges(2), AriadneCoreFixtures.diagonalShrinkageCorrelation(1)(2), 1e-12)
  }

  private def assertMatrixClose(actual: DMat, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1
