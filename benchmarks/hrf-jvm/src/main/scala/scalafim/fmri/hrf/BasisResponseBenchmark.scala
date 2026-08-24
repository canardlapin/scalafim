package scalafim.fmri.hrf

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

/** Basis-aware response work against the retained direct-coordinate baseline.
  *
  * Setup proves that typed reconstruction and direct contraction agree. The
  * benchmark then keeps semantic compilation, exact integration, explicit
  * quadrature, and coordinate contraction as separate costs.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class BasisResponseBenchmark:

  private val kernel = Hrfs.SPMG3
  private val basis = ResponseBasis.of(kernel)
  private val lag = Lag(6.0)
  private val coefficients =
    basis
      .coefficients(Vector(0.8, -0.2, 0.1))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  @Setup(Level.Trial)
  def validateEquivalentPaths(): Unit =
    val typed = typedReconstruction()
    val direct = directContraction()
    require(math.abs(typed - direct) <= 1e-12, s"typed=$typed direct=$direct")

  @Benchmark
  def pointFunctional(): Vector[Double] =
    basis
      .responseFunctional(ResponseFunctional.At(Seconds(6.0)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .values

  @Benchmark
  def exactWindowFunctional(): Vector[Double] =
    basis
      .responseFunctional(ResponseFunctional.WindowMean(Seconds(4.0), Seconds(8.0)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .values

  @Benchmark
  def trapezoidWindowFunctional(): Vector[Double] =
    basis
      .responseFunctional(
        ResponseFunctional.WindowMean(Seconds(4.0), Seconds(8.0)),
        FunctionalDiscretization.Trapezoid(PositiveSeconds.unsafe(Seconds(0.05)))
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .values

  @Benchmark
  def typedReconstruction(): Double =
    basis.reconstruct(coefficients)(lag).data(0)

  @Benchmark
  def directContraction(): Double =
    val values = kernel(lag).data
    val weights = coefficients.values
    var total = 0.0
    var index = 0
    while index < values.length do
      total += values(index) * weights(index)
      index += 1
    total
