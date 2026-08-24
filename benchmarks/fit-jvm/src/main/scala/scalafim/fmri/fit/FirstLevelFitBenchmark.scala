package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, VolumeWeighting}

/** First-level fit costs with planning and response work kept distinct.
  *
  * The default parameter grid spans short and long runs, modest and
  * nuisance-heavy designs, and parcel-sized through volume-block response
  * batches. Admission receipts override this grid with one named realistic
  * shape so every required path is measured in a bounded CI job.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class FirstLevelFitBenchmark:

  @Param(Array("360", "720"))
  var timepoints: Int = 0

  @Param(Array("12", "32"))
  var predictors: Int = 0

  @Param(Array("16", "128"))
  var responses: Int = 0

  private var design: DesignMatrix = uninitialized
  private var response: ResponseBlock = uninitialized
  private var singleResponse: ResponseBlock = uninitialized
  private var olsPrepared: OlsPrepared = uninitialized
  private var partitions: Vector[RunPartition] = uninitialized
  private var glsOptions: ArOptions = uninitialized
  private var glsPrepared: GlsPrepared = uninitialized
  private var blockInput: FitBlockInput = uninitialized
  private var weighting: ResponsePreparationPlan = uninitialized
  private var weightedInput: FitBlockInput = uninitialized
  private var weightedPrepared: OlsPrepared = uninitialized
  private var responseChunks: Vector[(ResponseBlock, Vector[Int])] = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    val designValue = designMatrix(timepoints, predictors)
    val responseValue = responseMatrix(designValue, responses)
    design = DesignMatrix.unsafe(designValue)
    response = ResponseBlock.unsafe(responseValue)
    singleResponse = ResponseBlock.unsafe(responseValue.slice(0, timepoints, 0, 1))
    olsPrepared = Ols.unsafePrepare(design)
    partitions = runPartitions(timepoints)
    glsOptions = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.35))
    glsPrepared =
      Gls
        .prepare(design, response, partitions, glsOptions)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    blockInput = FitBlockInput(
      design = design,
      response = response,
      voxelIndices = (0 until responses).toVector,
      timepoints = (0 until timepoints).toVector,
      partitions = partitions
    )
    val weights = Vector.tabulate(timepoints) { row =>
      0.75 + 0.5 * ((row % 17).toDouble / 16.0)
    }
    weighting = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(weights))
    )
    weightedInput = weighting
      .prepare(blockInput)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .input
    weightedPrepared = Ols.unsafePrepare(weightedInput.design)
    val chunkSize = math.max(1, math.min(32, responses / 4))
    responseChunks =
      (0 until responses).grouped(chunkSize).map { indices =>
        val voxelIndices = indices.toVector
        val from = voxelIndices.head
        val until = voxelIndices.last + 1
        ResponseBlock.unsafe(responseValue.slice(0, timepoints, from, until)) -> voxelIndices
      }.toVector

  /** One-time QR planning plus one multiresponse fit. */
  @Benchmark
  def olsPlanAndFit(): OlsFit =
    Ols.unsafeFit(design, response)

  /** Per-batch response work after reusing the QR factorization. */
  @Benchmark
  def olsPreparedMultiresponse(): OlsFit =
    olsPrepared.unsafeFit(response)

  /** Per-response cost after reusing the same QR factorization. */
  @Benchmark
  def olsPreparedSingleResponse(): OlsFit =
    olsPrepared.unsafeFit(singleResponse)

  /** Prepared response chunks plus the production dense-block merge. */
  @Benchmark
  def olsPreparedChunked(): DenseFitBlockResult =
    val blocks = responseChunks.map { case (chunk, voxelIndices) =>
      val input = blockInput.copy(response = chunk, voxelIndices = voxelIndices)
      DenseFitBlockResult.fromOls(input, olsPrepared.unsafeFit(chunk), scalafim.fmri.model.FitEngine.OrdinaryLeastSquares)
    }
    DenseFitBlockResult
      .merge(blocks)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Resolve fixed WLS weights, transform X/Y, plan, and fit. */
  @Benchmark
  def weightedPlanAndFit(): OlsFit =
    val prepared = weighting
      .prepare(blockInput)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    Ols.unsafeFit(prepared.input.design, prepared.input.response)

  /** Per-batch WLS response work after the weighted design is factorized. */
  @Benchmark
  def weightedPreparedFit(): OlsFit =
    weightedPrepared.unsafeFit(weightedInput.response)

  /** Initial fit, fixed-AR whitening plan, whitening, and final fit. */
  @Benchmark
  def fixedGlsPlanAndFit(): GlsFit =
    Gls
      .fit(design, response, partitions, glsOptions)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Per-batch whitening and fitting with the fixed-AR plan retained. */
  @Benchmark
  def fixedGlsPreparedFit(): GlsFit =
    glsPrepared
      .fit(response)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def runPartitions(rows: Int): Vector[RunPartition] =
    val split = rows / 2
    Vector(
      RunPartition(0, (0 until split).toVector, (0 until split).toVector),
      RunPartition(1, (split until rows).toVector, (split until rows).toVector)
    )

  private def designMatrix(rows: Int, columns: Int): DMat =
    Matrix.tabulate(rows, columns) { (row, column) =>
      if column == 0 then 1.0
      else
        val r = row.toDouble + 1.0
        val c = column.toDouble
        math.sin(r * (0.007 + c * 0.0031)) +
          math.cos(r * (0.011 + c * 0.0023)) +
          0.01 * ((row + column * 7) % 13).toDouble
    }

  private def responseMatrix(x: DMat, columns: Int): DMat =
    Matrix.tabulate(x.rows, columns) { (row, responseColumn) =>
      var value = 0.03 * deterministicInnovation(row, responseColumn)
      var predictor = 0
      while predictor < x.cols do
        value += x(row, predictor) * beta(predictor, responseColumn)
        predictor += 1
      value
    }

  private def beta(predictor: Int, responseColumn: Int): Double =
    math.sin((predictor + 1).toDouble * 0.37 + (responseColumn + 1).toDouble * 0.013) * 0.5

  private def deterministicInnovation(row: Int, responseColumn: Int): Double =
    val raw = math.sin((row + 1).toDouble * 12.9898 + (responseColumn + 1).toDouble * 78.233) * 43758.5453
    (raw - math.floor(raw)) * 2.0 - 1.0
