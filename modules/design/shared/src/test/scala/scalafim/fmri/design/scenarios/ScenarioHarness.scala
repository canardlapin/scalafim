package scalafim.fmri.design.scenarios

import scalafim.fmri.hrf.linalg.Mat
import scalafim.scenarios.{ScenarioHarness as CoreHarness, ScenarioMatrix}

/** Design-owned numeric adapter over the shared scenario verdict algebra. */
object ScenarioHarness:
  export CoreHarness.{comparisonMetrics, fact, finite, result, scalar, values}

  def matrix(
      name: String,
      actual: Mat,
      expected: Mat,
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    CoreHarness.matrix(name, MatView(actual), MatView(expected), tolerance)

  def matrixMetrics(
      name: String,
      actual: Mat,
      expected: Mat,
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

  private final case class MatView(value: Mat) extends ScenarioMatrix:
    def rows: Int = value.rows
    def cols: Int = value.cols
    def apply(row: Int, col: Int): Double = value(row, col)
