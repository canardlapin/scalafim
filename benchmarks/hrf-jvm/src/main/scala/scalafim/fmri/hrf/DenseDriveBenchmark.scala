package scalafim.fmri.hrf

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized
import scalafim.fmri.hrf.regressor.Regressor

/** Where, if anywhere, the FFT path earns its keep.
  *
  * `evalConv` skips zero entries of the neural drive, and an impulse design's
  * drive is almost all zeros — a few hundred non-zero bins out of a hundred
  * thousand — so the direct loop is effectively `O(events × kernel)` while the
  * FFT is `O(N log N)` on the full grid regardless. That is why the FFT loses
  * on event-related designs even after its allocation problem was fixed.
  *
  * Long epochs fill the drive in. This benchmark sweeps the duty cycle to find
  * the density at which the FFT overtakes the direct loop, if it does.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class DenseDriveBenchmark:

  /** Fraction of the run covered by stimulation. */
  @Param(Array("0.0", "0.25", "0.9"))
  var dutyCycle: Double = 0.0

  @Param(Array("1200", "4800"))
  var nScans: Int = 0

  private val tr = 2.0
  private val isi = 15.0

  private var grid: Vector[Double] = uninitialized
  private var reg: Regressor = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    grid = Vector.tabulate(nScans)(_ * tr)
    val runLength = nScans * tr
    val nEvents = math.max(1, (runLength / isi).toInt)
    val onsets = Vector.tabulate(nEvents)(i => i * isi + 0.37)
    val duration = dutyCycle * isi
    reg = Regressor(
      onsets,
      Hrfs.SPMG1,
      duration = Seq(duration),
      span = Some(24.0)
    )

  @Benchmark
  def conv(): Mat =
    Regressor.evaluate(reg, grid, precision = 0.1, method = Regressor.EvalMethod.Conv)

  @Benchmark
  def fft(): Mat =
    Regressor.evaluate(reg, grid, precision = 0.1, method = Regressor.EvalMethod.FFT)
