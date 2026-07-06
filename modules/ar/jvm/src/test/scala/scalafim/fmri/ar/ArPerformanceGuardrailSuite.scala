package scalafim.fmri.ar

import scalafim.linalg.DoubleMatrix

class ArPerformanceGuardrailSuite extends munit.FunSuite:

  // JVM-only coarse guardrails. They are sized to catch accidental algorithmic
  // regressions while keeping the ordinary AR test task fast.
  private val WhiteningRows = 640
  private val WhiteningCols = 96
  private val EstimationRows = 720
  private val EstimationCols = 80

  test("whitening representative two-run matrix stays within the JVM guardrail") {
    val input = ar2ResidualMatrix(WhiteningRows, WhiteningCols)
    val segments = TimeSegments.fromRunLengths(Vector(WhiteningRows / 2, WhiteningRows / 2))
    val plan = WhiteningPlan.global(
      ArmaCoefficients.ar(0.45, -0.15),
      segments,
      exactFirstAr1 = false
    )

    val whitened = timed("ar.whitening", defaultMs = 3000) {
      WhiteningTransform
        .matrix(plan, input)
        .fold(error => fail(error.message), identity)
    }

    assertEquals(whitened.rows, input.rows)
    assertEquals(whitened.cols, input.cols)
    assert(finiteChecksum(whitened).isFinite)
  }

  test("pooled fixed-order AR estimation stays within the JVM guardrail") {
    val residuals = ar2ResidualMatrix(EstimationRows, EstimationCols)
    val segments = TimeSegments.fromRunLengths(Vector(EstimationRows / 2, EstimationRows / 2))

    val plan = timed("ar.estimation", defaultMs = 5000) {
      ArEstimation
        .fitNoise(
          residuals,
          segments,
          ArFitOptions(order = ArOrder.Fixed(2), exactFirstAr1 = false)
        )
        .fold(error => fail(error.message), identity)
    }

    val phi = plan.coefficients.head.phi
    assertEquals(plan.method, WhiteningMethod.Estimated)
    assertEquals(phi.length, 2)
    assert(phi.forall(_.isFinite), clues(phi))
  }

  private def timed[A](name: String, defaultMs: Long)(body: => A): A =
    val budgetMs = guardrailBudgetMs(name, defaultMs)
    val start = System.nanoTime()
    val result = body
    val elapsedMs = (System.nanoTime() - start).toDouble / 1000000.0
    assert(elapsedMs <= budgetMs.toDouble, clues(name, elapsedMs, budgetMs))
    result

  private def guardrailBudgetMs(name: String, defaultMs: Long): Long =
    scala.sys.props
      .get(s"scalafim.perf.$name.ms")
      .flatMap(_.toLongOption)
      .filter(_ > 0L)
      .getOrElse(defaultMs)

  private def ar2ResidualMatrix(rows: Int, cols: Int): DoubleMatrix =
    val data = new Array[Double](rows * cols)
    val lag1 = Array.fill(cols)(0.0)
    val lag2 = Array.fill(cols)(0.0)
    val runBreak = rows / 2

    var row = 0
    while row < rows do
      if row == runBreak then
        var col = 0
        while col < cols do
          lag1(col) = 0.0
          lag2(col) = 0.0
          col += 1

      var col = 0
      while col < cols do
        val innovation = deterministicInnovation(row, col)
        val value = innovation + 0.45 * lag1(col) - 0.15 * lag2(col)
        data(row * cols + col) = value
        lag2(col) = lag1(col)
        lag1(col) = value
        col += 1
      row += 1

    DoubleMatrix.unsafe(rows, cols, data)

  private def deterministicInnovation(row: Int, col: Int): Double =
    val raw = math.sin((row + 1).toDouble * 12.9898 + (col + 1).toDouble * 78.233) * 43758.5453
    (raw - math.floor(raw)) * 2.0 - 1.0

  private def finiteChecksum(matrix: DoubleMatrix): Double =
    val data = matrix.copyData
    var sum = 0.0
    var i = 0
    while i < data.length do
      sum += data(i) * 1.0e-6
      i += 1
    sum
