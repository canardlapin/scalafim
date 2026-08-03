package scalafim.fmri.hrf

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized
import scalafim.fmri.hrf.regressor.{HrfAssignment, Regressor, StimulusEvent}

/** Cost of the three `Regressor.EvalMethod` paths.
  *
  * The three are not interchangeable on accuracy: `Conv` and `FFT` bin onsets
  * onto the microtime grid, while `Loop` evaluates each event at its exact
  * onset. `RegressorMethodAccuracy` measures that gap; this measures the price.
  *
  * Scan counts are realistic fMRI runs at TR = 2 s: 300 ≈ 10 min,
  * 1200 ≈ 40 min, 4800 ≈ a long concatenated session.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class RegressorConvolutionBenchmark:

  @Param(Array("300", "1200", "4800"))
  var nScans: Int = 0

  @Param(Array("1", "3"))
  var nbasis: Int = 0

  @Param(Array("0.33", "0.1"))
  var precision: Double = 0.0

  private val tr = 2.0

  private var grid: Vector[Double] = uninitialized
  private var reg: Regressor = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    grid = Vector.tabulate(nScans)(_ * tr)
    val duration = nScans * tr
    // One event roughly every 12 s, deliberately off the TR grid so the
    // onset-quantization difference between the paths is exercised.
    val nEvents = math.max(1, (duration / 12.0).toInt)
    val onsets = Vector.tabulate(nEvents)(i => i * 12.0 + 0.37)
    val kernel = if nbasis == 1 then Hrfs.SPMG1 else Hrfs.SPMG3
    reg = Regressor(onsets, kernel, span = Some(24.0))

  @Benchmark
  def conv(): Mat =
    Regressor.evaluate(reg, grid, precision = precision, method = Regressor.EvalMethod.Conv)

  @Benchmark
  def fft(): Mat =
    Regressor.evaluate(reg, grid, precision = precision, method = Regressor.EvalMethod.FFT)

  @Benchmark
  def loop(): Mat =
    Regressor.evaluate(reg, grid, precision = precision, method = Regressor.EvalMethod.Loop)

/** Cost of the epoch (box-response) path, exact primitive versus quadrature.
  *
  * The trapezoid needs `O(width / precision)` kernel evaluations per sample;
  * the primitive needs two antiderivative evaluations regardless. This is the
  * measurement behind that claim.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class EpochIntegrationBenchmark:

  @Param(Array("2.0", "12.0"))
  var width: Double = 0.0

  @Param(Array("0.33", "0.05"))
  var precision: Double = 0.0

  private var grid: Vector[Double] = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    grid = Vector.tabulate(600)(_ * 0.5)

  @Benchmark
  def exact(): Mat =
    Evaluate.doubles(
      Hrfs.SPMG1,
      grid,
      duration = width,
      precision = precision,
      integration = Integration.Exact
    )

  @Benchmark
  def trapezoid(): Mat =
    Evaluate.doubles(
      Hrfs.SPMG1,
      grid,
      duration = width,
      precision = precision,
      integration = Integration.Trapezoid
    )
