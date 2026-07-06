package scalafim.fmri.fit

import scalafim.fmri.model.{ArOptions, ArStructure}
import scalafim.linalg.DoubleMatrix

class GlsPerformanceGuardrailSuite extends munit.FunSuite:

  // JVM-only coarse guardrail for the dense fixed-AR GLS path. Use
  // -Dscalafim.perf.fit.gls.ms=<millis> to tune the budget on slower machines.
  private val Rows = 480
  private val Voxels = 80
  private val Predictors = 4

  test("fixed AR(1) dense GLS stays within the JVM guardrail") {
    val designValue = designMatrix(Rows)
    val responseValue = responseMatrix(designValue, Voxels)
    val design = DesignMatrix.unsafe(designValue)
    val response = ResponseBlock.unsafe(responseValue)

    val fit = timed("fit.gls", defaultMs = 6000) {
      Gls
        .fit(
          design,
          response,
          runPartitions(Rows),
          ArOptions(structure = ArStructure.Ar(1), rho = Some(0.35))
        )
        .fold(error => fail(error.message), identity)
    }

    assertEquals(fit.predictors, Predictors)
    assertEquals(fit.voxels, Voxels)
    assertEquals(fit.diagnostics.order, 1)
    assertEquals(fit.diagnostics.runs.map(_.method), Vector("fixed", "fixed"))
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

  private def runPartitions(rows: Int): Vector[RunPartition] =
    val firstRunRows = rows / 2
    Vector(
      RunPartition(
        runIndex = 0,
        rowIndices = (0 until firstRunRows).toVector,
        timepoints = (0 until firstRunRows).toVector
      ),
      RunPartition(
        runIndex = 1,
        rowIndices = (firstRunRows until rows).toVector,
        timepoints = (firstRunRows until rows).toVector
      )
    )

  private def designMatrix(rows: Int): DoubleMatrix =
    val data = new Array[Double](rows * Predictors)
    val midpoint = (rows - 1).toDouble / 2.0
    val firstRunRows = rows / 2

    var row = 0
    while row < rows do
      val offset = row * Predictors
      val centered = (row.toDouble - midpoint) / midpoint
      data(offset) = 1.0
      data(offset + 1) = centered
      data(offset + 2) = math.sin(row.toDouble * 0.11)
      data(offset + 3) = if row >= firstRunRows then 1.0 else 0.0
      row += 1

    DoubleMatrix.unsafe(rows, Predictors, data)

  private def responseMatrix(design: DoubleMatrix, voxels: Int): DoubleMatrix =
    val data = new Array[Double](design.rows * voxels)
    val lag1 = Array.fill(voxels)(0.0)
    val firstRunRows = design.rows / 2

    var row = 0
    while row < design.rows do
      if row == firstRunRows then
        var voxel = 0
        while voxel < voxels do
          lag1(voxel) = 0.0
          voxel += 1

      var voxel = 0
      while voxel < voxels do
        val residual = 0.05 * deterministicInnovation(row, voxel) + 0.35 * lag1(voxel)
        val value =
          beta(0, voxel) * design(row, 0) +
            beta(1, voxel) * design(row, 1) +
            beta(2, voxel) * design(row, 2) +
            beta(3, voxel) * design(row, 3) +
            residual
        data(row * voxels + voxel) = value
        lag1(voxel) = residual
        voxel += 1
      row += 1

    DoubleMatrix.unsafe(design.rows, voxels, data)

  private def beta(predictor: Int, voxel: Int): Double =
    val scale = 1.0 + (voxel % 17).toDouble * 0.01
    predictor match
      case 0 => 1.5 + voxel.toDouble * 0.002
      case 1 => 0.8 * scale
      case 2 => -0.35 * scale
      case 3 => 0.25 - voxel.toDouble * 0.001
      case _ => 0.0

  private def deterministicInnovation(row: Int, voxel: Int): Double =
    val raw = math.sin((row + 1).toDouble * 12.9898 + (voxel + 1).toDouble * 78.233) * 43758.5453
    (raw - math.floor(raw)) * 2.0 - 1.0
