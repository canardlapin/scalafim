package scalafim.fmri.fit.scenarios

import scalafim.scenarios.{ScenarioHarness as CoreHarness, ScenarioMatrix, ScenarioVector}
import gale.linalg.{DMat, DVec}

/** Fit-owned numeric adapters over the shared scenario verdict algebra. */
object ScenarioHarness:
  export CoreHarness.{comparisonMetrics, fact, finite, result, scalar, values}

  def vector(
      name: String,
      actual: DVec,
      expected: DVec,
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    CoreHarness.vector(name, DVecView(actual), DVecView(expected), tolerance)

  def vectorMetrics(
      name: String,
      actual: DVec,
      expected: DVec,
      tolerance: ScenarioComparisonTolerance
  ): Vector[ScenarioObservation] =
    CoreHarness.comparisonMetrics(name, actual.toSeq.toVector, expected.toSeq.toVector, tolerance)

  def matrix(
      name: String,
      actual: DMat,
      expected: DMat,
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    CoreHarness.matrix(name, DMatView(actual), DMatView(expected), tolerance)

  def matrixMetrics(
      name: String,
      actual: DMat,
      expected: DMat,
      tolerance: ScenarioComparisonTolerance
  ): Vector[ScenarioObservation] =
    val actualValues = Vector.tabulate(actual.rows * actual.cols) { index =>
      actual(index / actual.cols, index % actual.cols)
    }
    val expectedValues = Vector.tabulate(expected.rows * expected.cols) { index =>
      expected(index / expected.cols, index % expected.cols)
    }
    Vector(
      CoreHarness.fact(s"$name.metrics.rows", actual.rows == expected.rows, s"actual=${actual.rows} expected=${expected.rows}"),
      CoreHarness.fact(s"$name.metrics.cols", actual.cols == expected.cols, s"actual=${actual.cols} expected=${expected.cols}")
    ) ++ CoreHarness.comparisonMetrics(name, actualValues, expectedValues, tolerance)

  private final case class DVecView(value: DVec) extends ScenarioVector:
    def length: Int = value.length
    def apply(index: Int): Double = value(index)

  private final case class DMatView(value: DMat) extends ScenarioMatrix:
    def rows: Int = value.rows
    def cols: Int = value.cols
    def apply(row: Int, col: Int): Double = value(row, col)
