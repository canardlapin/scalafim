package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import gale.linalg.DMat

class OlsPerformanceGuardrailSuite extends munit.FunSuite:

  // JVM-only coarse guardrail for the default QR dense OLS path. Use
  // -Dscalafim.perf.fit.ols.ms=<millis> to tune the budget on slower machines.
  private val Rows = 640
  private val Voxels = 120
  private val Predictors = 5

  test("default QR dense OLS stays within the JVM guardrail") {
    val designValue = designMatrix(Rows)
    val responseValue = responseMatrix(designValue, Voxels)
    val design = DesignMatrix.unsafe(designValue)
    val response = ResponseBlock.unsafe(responseValue)

    val fit = timed("fit.ols", defaultMs = 4000) {
      Ols.fit(design, response).fold(error => fail(error.message), identity)
    }

    assertEquals(fit.predictors, Predictors)
    assertEquals(fit.voxels, Voxels)
    assertFinite(fit.coefficients.value.copyData)
    assertFinite(fit.residualVariance.copyData)
    assertFinite(fit.normalizedCovariance.copyData)
    assertFinite(fit.standardErrors.value.copyData)
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

  private def assertFinite(values: Array[Double]): Unit =
    assert(values.forall(_.isFinite), clues(values.take(16).toVector))

  private def designMatrix(rows: Int): DMat =
    val data = new Array[Double](rows * Predictors)
    val midpoint = (rows - 1).toDouble / 2.0
    val firstRunRows = rows / 2

    var row = 0
    while row < rows do
      val offset = row * Predictors
      val centered = (row.toDouble - midpoint) / midpoint
      data(offset) = 1.0
      data(offset + 1) = centered
      data(offset + 2) = math.sin(row.toDouble * 0.07)
      data(offset + 3) = math.cos(row.toDouble * 0.05)
      data(offset + 4) = if row >= firstRunRows then 1.0 else 0.0
      row += 1

    scalafim.fmri.fit.GaleTestMatrix.fromArray(rows, Predictors, data)

  private def responseMatrix(design: DMat, voxels: Int): DMat =
    val data = new Array[Double](design.rows * voxels)

    var row = 0
    while row < design.rows do
      var voxel = 0
      while voxel < voxels do
        var value = deterministicInnovation(row, voxel) * 0.03
        var predictor = 0
        while predictor < design.cols do
          value += beta(predictor, voxel) * design(row, predictor)
          predictor += 1
        data(row * voxels + voxel) = value
        voxel += 1
      row += 1

    scalafim.fmri.fit.GaleTestMatrix.fromArray(design.rows, voxels, data)

  private def beta(predictor: Int, voxel: Int): Double =
    val scale = 1.0 + (voxel % 13).toDouble * 0.015
    predictor match
      case 0 => 1.0 + voxel.toDouble * 0.001
      case 1 => 0.6 * scale
      case 2 => -0.25 * scale
      case 3 => 0.15 * scale
      case 4 => -0.1 + voxel.toDouble * 0.0005
      case _ => 0.0

  private def deterministicInnovation(row: Int, voxel: Int): Double =
    val raw = math.sin((row + 1).toDouble * 12.9898 + (voxel + 1).toDouble * 78.233) * 43758.5453
    (raw - math.floor(raw)) * 2.0 - 1.0
