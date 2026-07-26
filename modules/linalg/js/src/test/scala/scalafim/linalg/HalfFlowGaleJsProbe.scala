package scalafim.linalg

import gale.linalg.*
import gale.solvers.{IterativeSolvers, SolverConfig, ToleranceMode}
import scala.scalajs.js

object HalfFlowGaleJsProbe:
  import HalfFlowGaleStress.*

  private final case class Measurement(medianMillis: Double, p95Millis: Double, checksum: Double)

  private final class ConstructionCounter:
    var scalar: Int = 0
    var vector: Int = 0
    var workspace: Int = 0
    var scratch: Int = 0

    def reset(): Unit =
      scalar = 0
      vector = 0
      workspace = 0
      scratch = 0

  def main(args: Array[String]): Unit =
    val systems = 32768
    val warmups = 5
    val samples = 11
    val counter = ConstructionCounter()
    val values = Array.tabulate(systems): index =>
      val t = index.toDouble
      fromLower(
        1.0 + 0.2 * math.abs(math.sin(t * 0.011)),
        0.08 * math.sin(t * 0.017),
        0.8 + 0.15 * math.abs(math.cos(t * 0.013)),
        0.06 * math.cos(t * 0.019),
        0.05 * math.sin(t * 0.023),
        0.7 + 0.1 * math.abs(math.sin(t * 0.029)),
        math.sin(t * 0.007),
        math.cos(t * 0.005),
        math.sin(t * 0.003 + 0.4),
        damping = 1e-3
      )
    counter.scalar += 1

    val side = 24
    val alpha = 0.6
    val operator = PeriodicHelmholtz(side, side, side, alpha)
    val input = fourierMixture(side)
    val output = MutableDVec.zeros(input.length)
    val outputView = output.asVec
    val rhs = fourierMixture(side)
    val expected = inverseFourierMixture(side, alpha)
    counter.vector += 4

    operator.applyTo(input, output)
    require(maximumFourierApplyError(side, alpha, outputView) <= 4e-12)
    val initialResult = IterativeSolvers.cg(
      operator,
      rhs,
      SolverConfig(tolerance = 1e-12, maxIterations = 40),
      toleranceMode = ToleranceMode.RelativeToRhs
    )
    require(initialResult.converged)
    require(maximumDifference(initialResult.x, expected) <= 3e-11)
    val scalarOracle = scalarCholesky(values)
    val tinyOracle = galeTinyAdjugate(values)
    require(math.abs(scalarOracle - tinyOracle) <= 1e-8 * math.max(1.0, math.abs(scalarOracle)))

    counter.reset()
    val scalar = measure(warmups, samples)(scalarCholesky(values))
    val tiny = measure(warmups, samples)(galeTinyAdjugate(values))
    val applyInto = measure(warmups, samples):
      operator.applyTo(input, output)
      outputView(input.length / 2)
    val galeCg = measure(warmups, samples):
      val result = IterativeSolvers.cg(
        operator,
        rhs,
        SolverConfig(tolerance = 1e-12, maxIterations = 40),
        toleranceMode = ToleranceMode.RelativeToRhs
      )
      result.x(rhs.length / 2) + result.iterations.toDouble + result.residual

    println("HALF_FLOW_GALE_JS_PROBE_JSON_BEGIN")
    println("{")
    println("  \"schema\": \"scalafim-half-flow-gale-js-probe-v1\",")
    println(s"  \"systems3x3\": $systems,")
    println(s"  \"helmholtzSide\": $side,")
    println(s"  \"warmups\": $warmups,")
    println(s"  \"samples\": $samples,")
    println("  \"timedFullVolumeConstructions\": {")
    println(s"    \"scalar\": ${counter.scalar},")
    println(s"    \"vector\": ${counter.vector},")
    println(s"    \"workspace\": ${counter.workspace},")
    println(s"    \"scratch\": ${counter.scratch}")
    println("  },")
    printMeasurement("scalarCholeskyBatch", scalar, trailingComma = true)
    printMeasurement("galeMat3AdjugateBatch", tiny, trailingComma = true)
    printMeasurement("helmholtzApplyInto", applyInto, trailingComma = true)
    printMeasurement("galeCg", galeCg, trailingComma = false)
    println("}")
    println("HALF_FLOW_GALE_JS_PROBE_JSON_END")

  private def scalarCholesky(systems: Array[System3]): Double =
    var checksum = 0.0
    var index = 0
    while index < systems.length do
      val system = systems(index)
      val solution = solveCholesky(system, 1e-16) match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error)
      checksum += solution.x0 + 0.5 * solution.x1 + 0.25 * solution.x2
      index += 1
    checksum

  private def galeTinyAdjugate(systems: Array[System3]): Double =
    var checksum = 0.0
    var index = 0
    while index < systems.length do
      val system = systems(index)
      val matrix = Mat3(
        system.a00, system.a01, system.a02,
        system.a01, system.a11, system.a12,
        system.a02, system.a12, system.a22
      )
      val c00 = matrix.a11 * matrix.a22 - matrix.a12 * matrix.a21
      val c01 = matrix.a02 * matrix.a21 - matrix.a01 * matrix.a22
      val c02 = matrix.a01 * matrix.a12 - matrix.a02 * matrix.a11
      val c11 = matrix.a00 * matrix.a22 - matrix.a02 * matrix.a20
      val c12 = matrix.a01 * matrix.a20 - matrix.a00 * matrix.a21
      val c22 = matrix.a00 * matrix.a11 - matrix.a01 * matrix.a10
      val inverseDeterminant = 1.0 / matrix.det
      val x0 = (c00 * system.b0 + c01 * system.b1 + c02 * system.b2) * inverseDeterminant
      val x1 = (c01 * system.b0 + c11 * system.b1 + c12 * system.b2) * inverseDeterminant
      val x2 = (c02 * system.b0 + c12 * system.b1 + c22 * system.b2) * inverseDeterminant
      checksum += x0 + 0.5 * x1 + 0.25 * x2
      index += 1
    checksum

  private def measure(warmups: Int, samples: Int)(operation: => Double): Measurement =
    var checksum = 0.0
    var iteration = 0
    while iteration < warmups do
      checksum = operation
      iteration += 1
    val elapsed = new Array[Double](samples)
    iteration = 0
    while iteration < samples do
      val start = js.Date.now()
      checksum = operation
      elapsed(iteration) = js.Date.now() - start
      iteration += 1
    val sorted = elapsed.sorted
    Measurement(percentile(sorted, 0.5), percentile(sorted, 0.95), checksum)

  private def percentile(sorted: Array[Double], probability: Double): Double =
    val index = math.min(sorted.length - 1, math.ceil(probability * sorted.length.toDouble).toInt - 1)
    sorted(math.max(0, index))

  private def printMeasurement(name: String, measurement: Measurement, trailingComma: Boolean): Unit =
    println(s"  \"$name\": {")
    println(s"    \"medianMillis\": ${measurement.medianMillis},")
    println(s"    \"p95Millis\": ${measurement.p95Millis},")
    println(s"    \"checksum\": ${measurement.checksum}")
    println(if trailingComma then "  }," else "  }")
