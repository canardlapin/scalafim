package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.regressor.Regressor

/** Bounded Scala.js timing characterization for the public direct-convolution
  * path. It is deliberately a runMain probe, not a performance admission gate.
  */
object HrfConvolutionTiming:

  private val warmupIterations = 10
  private val measuredIterations = 30
  private val precisionSeconds = 0.1
  // Mirrors one current JVM benchmark point: 300 scans at TR 2 s, one
  // deliberately off-grid event roughly every 12 s, the three-column SPMG3
  // basis, and 0.1 s microtime precision.
  private val scans = 300
  private val trSeconds = 2.0
  private val onsets = Vector.tabulate(scans / 6)(index => index * 12.0 + 0.37)
  private val grid = Vector.tabulate(scans)(index => index * trSeconds)
  private val regressor = Regressor(onsets, Hrfs.SPMG3, span = Some(24.0))

  private def checksum(matrix: Mat): Double =
    var total = 0.0
    var index = 0
    while index < matrix.data.length do
      total += matrix.data(index) * (index + 1).toDouble
      index += 1
    total

  private def evaluateChecksum(): Double =
    checksum(
      Regressor.evaluate(
        regressor,
        grid,
        precision = precisionSeconds,
        method = Regressor.EvalMethod.Conv
      )
    )

  private def equivalent(actual: Double, expected: Double): Boolean =
    math.abs(actual - expected) <= 1e-10 * math.max(1.0, math.abs(expected))

  def main(args: Array[String]): Unit =
    val expectedChecksum = evaluateChecksum()
    if !expectedChecksum.isFinite then
      throw new IllegalStateException(s"non-finite baseline checksum: $expectedChecksum")

    var iteration = 0
    while iteration < warmupIterations do
      val actual = evaluateChecksum()
      if !equivalent(actual, expectedChecksum) then
        throw new IllegalStateException(s"warmup checksum changed: expected=$expectedChecksum actual=$actual")
      iteration += 1

    val started = System.nanoTime()
    iteration = 0
    while iteration < measuredIterations do
      val actual = evaluateChecksum()
      if !equivalent(actual, expectedChecksum) then
        throw new IllegalStateException(s"timed checksum changed: expected=$expectedChecksum actual=$actual")
      iteration += 1
    val elapsedNanos = System.nanoTime() - started
    val nanosPerIteration = elapsedNanos.toDouble / measuredIterations.toDouble

    println(
      s"""{"schema":"scalafim.hrf-convolution-timing.v1","method":"conv","events":${onsets.length},"grid_points":${grid.length},"basis_columns":${Hrfs.SPMG3.nbasis},"precision_seconds":$precisionSeconds,"warmup_iterations":$warmupIterations,"measured_iterations":$measuredIterations,"elapsed_nanoseconds":$elapsedNanos,"nanoseconds_per_iteration":$nanosPerIteration,"checksum":$expectedChecksum}"""
    )
