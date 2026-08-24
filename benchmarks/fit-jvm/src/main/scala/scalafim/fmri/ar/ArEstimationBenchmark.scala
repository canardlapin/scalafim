package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

/** AR-estimation timing and allocation at a realistic multiresponse shape. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class ArEstimationBenchmark:

  @Param(Array("360", "720"))
  var timepoints: Int = 0

  @Param(Array("16", "128"))
  var responses: Int = 0

  @Param(Array("1", "4"))
  var order: Int = 0

  private var residuals: DMat = uninitialized
  private var layout: NoiseEstimationLayout = uninitialized
  private var globalFixed: ArFitOptions = uninitialized
  private var runFixed: ArFitOptions = uninitialized
  private var globalAutomatic: ArFitOptions = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    residuals = residualMatrix(timepoints, responses)
    val runLengths = Vector(timepoints / 2, timepoints - timepoints / 2)
    val excluded = Set(timepoints / 4, timepoints / 2 + timepoints / 5)
    val segments = TimeSegments.withCensorResets(
      TimeSegments.fromRunLengths(runLengths),
      excluded
    )
    layout = NoiseEstimationLayout
      .excludingRows(segments, timepoints, excluded)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    globalFixed = ArFitOptions(
      order = ArOrder.Fixed(order),
      pooling = NoisePooling.Global,
      exactFirstAr1 = false
    )
    runFixed = ArFitOptions(
      order = ArOrder.Fixed(order),
      pooling = NoisePooling.Run,
      exactFirstAr1 = false
    )
    globalAutomatic = ArFitOptions(
      order = ArOrder.Auto(order),
      pooling = NoisePooling.Global,
      exactFirstAr1 = false
    )

  /** Fixed-order estimation with surviving-row-weighted global pooling. */
  @Benchmark
  def fixedOrderGlobal(): WhiteningPlan =
    estimate(globalFixed)

  /** Fixed-order estimation retaining one coefficient vector per run. */
  @Benchmark
  def fixedOrderRun(): WhiteningPlan =
    estimate(runFixed)

  /** BIC order selection through the configured maximum order. */
  @Benchmark
  def automaticGlobal(): WhiteningPlan =
    estimate(globalAutomatic)

  private def estimate(options: ArFitOptions): WhiteningPlan =
    ArEstimation
      .fitNoise(residuals, layout, options)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def residualMatrix(rows: Int, columns: Int): DMat =
    val states = Array.fill(columns, 4)(0.0)
    val out = Matrix.newBuilder(rows, columns)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        if row == rows / 2 then
          var lag = 0
          while lag < 4 do
            states(column)(lag) = 0.0
            lag += 1
        val raw = math.sin((row + 1).toDouble * 12.9898 + (column + 1).toDouble * 78.233) * 43758.5453
        val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
        val value =
          innovation +
            0.42 * states(column)(0) -
            0.18 * states(column)(1) +
            0.08 * states(column)(2) -
            0.03 * states(column)(3)
        states(column)(3) = states(column)(2)
        states(column)(2) = states(column)(1)
        states(column)(1) = states(column)(0)
        states(column)(0) = value
        out(row, column) = value
        column += 1
      row += 1
    out.result()
