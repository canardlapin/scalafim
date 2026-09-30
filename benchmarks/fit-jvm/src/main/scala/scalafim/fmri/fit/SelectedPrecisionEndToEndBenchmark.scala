package scalafim.fmri.fit

import gale.linalg.Matrix
import org.openjdk.jmh.annotations.*
import scalafim.dataset.{DataSelection, DatasetBackend, DatasetError, DatasetId, DatasetMetadata, DatasetSeriesReader, DatasetShape, FmriDataset, FmriSeries}
import scalafim.fmri.design.{ColumnRole, DesignAudit, DesignSchema, ModulatorId, RowLayout, RunScope, StructuralColumn, StructuralColumnOrigin}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FitStrategy, FmriModel}
import scalafim.image.{Mask, SampleSpaces}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** Whole public selected path: bounded source reads, run QR coordinates,
  * precision pooling and requested output. The binary source is mapped and
  * accessed by selected row/voxel, rather than copied into a resident matrix.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class SelectedPrecisionEndToEndBenchmark:
  @Param(Array("2", "64"))
  var outputs: Int = 0

  @Param(Array("resident", "mapped-file"))
  var backing: String = ""

  @Param(Array("none", "marginal"))
  var uncertainty: String = ""

  private val rows = 160
  private val voxels = 8192
  private var prepared: FirstLevelFixedEffectsEstimatePlan = uninitialized
  private var reader: DatasetSeriesReader = uninitialized
  private var path: Path = uninitialized
  private var channel: FileChannel = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    val space = SampleSpaces(Vector(voxels, 1, 1))
    val benchmarkShape = DatasetShape.make(space, rows).fold(e => throw IllegalArgumentException(e.message), identity)
    val backend = new DatasetBackend:
      val id = DatasetId(s"selected-$backing-$outputs")
      val shape: DatasetShape = benchmarkShape
      val mask = Mask.all(shape.space)
      val metadata = DatasetMetadata.Empty
      def readEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        Left(DatasetError.StorageFailure("benchmark uses its explicitly selected reader"))
    val frame = SamplingFrame(blockLens = Vector(80, 80), tr = Vector(2.0, 0.8))
    val dataset = FmriDataset.unsafe(backend, frame)
    val names = Vector.tabulate(outputs)(i => s"fir-$i")
    val designData = Array.tabulate(rows * outputs) { index =>
      val row = index / outputs
      val column = index % outputs
      if row % 80 == column then 1.0 else 0.0
    }
    val eventMatrix = Mat.unsafe(rows, outputs, designData)
    val columns = Vector.tabulate(outputs) { column =>
      StructuralColumn.fromOrigin(column + 1,
        StructuralColumnOrigin.Sampled(ModulatorId.unsafe(s"fir-$column"), ColumnRole.Task, RunScope.Global),
        names(column)).fold(e => throw IllegalArgumentException(e.message), identity)
    }
    val schema = DesignSchema.validated(eventMatrix, RowLayout.fromSamplingFrame(frame), columns, DesignAudit())
      .fold(e => throw IllegalArgumentException(e.message), identity)
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = frame,
      designMatrix = eventMatrix,
      columnNames = names,
      termSpans = Vector(0 -> outputs),
      colIndices = Map("fir" -> (0 until outputs).toVector),
      compiledSchema = Some(schema)
    )
    val baseline = BaselineModel.build(samplingFrame = frame,
      basis = BaselineBasis.Constant, intercept = Intercept.Global)
    val model = FmriModel(event, baseline, dataset)
    val plan = FitPlan(model, FitStrategy.SeparateRunsThenFixedEffects())
    val axis = model.designSchema.get.coefficientAxis
    val request = FirstLevelEstimateRequest.make(
      axis.columnIds.take(outputs).map(EstimateOutput.Coefficient.apply),
      if uncertainty == "none" then EstimateUncertaintyRequest.None else EstimateUncertaintyRequest.Marginal
    ).fold(e => throw IllegalArgumentException(e.message), identity)
    prepared = FirstLevelFixedEffectsEstimates.prepare(plan, request, ChunkSize.unsafe(512))
      .fold(e => throw IllegalArgumentException(e.message), identity)
    reader =
      if backing == "resident" then
        val matrix = Matrix.tabulate(rows, voxels)(signal)
        new DatasetSeriesReader:
          val dataset: FmriDataset = model.dataset
          def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
            selectedSeries(dataset, selection, (row, voxel) => matrix(row, voxel))
      else
        path = Files.createTempFile("scalafim-selected-mapped-", ".bin")
        channel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)
        val line = ByteBuffer.allocate(voxels * java.lang.Double.BYTES).order(ByteOrder.LITTLE_ENDIAN)
        var row = 0
        while row < rows do
          line.clear()
          var voxel = 0
          while voxel < voxels do
            line.putDouble(signal(row, voxel))
            voxel += 1
          line.flip()
          while line.hasRemaining do
            val _ = channel.write(line)
          row += 1
        val mapped = channel.map(FileChannel.MapMode.READ_ONLY, 0, rows.toLong * voxels * java.lang.Double.BYTES)
        val _ = mapped.order(ByteOrder.LITTLE_ENDIAN)
        new DatasetSeriesReader:
          val dataset: FmriDataset = model.dataset
          def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
            selectedSeries(dataset, selection, (row, voxel) =>
              mapped.getDouble((row * voxels + voxel) * java.lang.Double.BYTES))

  @TearDown(Level.Trial)
  def teardown(): Unit =
    if channel != null then channel.close()
    if path != null then
      val _ = Files.deleteIfExists(path)

  @Benchmark
  def fullSelectedFit(): Double =
    var checksum = 0.0
    prepared.foreachBlock(reader, block =>
      block.result match
        case FixedEffectsEstimateBlockResult.Selected(result, _) =>
          checksum += result.estimates(0, 0)
        case _ => ()
      Right(())
    ).fold(e => throw IllegalArgumentException(e.message), identity)
    checksum

  private def selectedSeries(dataset: FmriDataset, selection: DataSelection,
      value: (Int, Int) => Double): Either[DatasetError, FmriSeries] =
    for
      resolved <- dataset.resolve(selection)
      series <- FmriSeries.make(
        Matrix.tabulate(resolved.nTimepoints, resolved.nVoxels)((row, voxel) =>
          value(resolved.timepoints(row), resolved.voxels(voxel))),
        resolved.voxelIndexValues, resolved.timepointIndices, dataset.shape, dataset.metadata)
    yield series

  private def signal(row: Int, voxel: Int): Double =
    val bin = row % 80
    val beta = if bin < outputs then math.sin((bin + 1) * 0.13 + (voxel + 1) * 0.017) else 0.0
    1.0 + beta + 0.03 * math.sin((row + 1) * 0.71 + (voxel + 1) * 0.11)

object SelectedPrecisionBenchmarkSmoke:
  def main(args: Array[String]): Unit =
    require(args.length == 3, "usage: outputs backing uncertainty")
    val benchmark = new SelectedPrecisionEndToEndBenchmark
    benchmark.outputs = args(0).toInt
    benchmark.backing = args(1)
    benchmark.uncertainty = args(2)
    benchmark.setup()
    try println(s"selected-checksum=${benchmark.fullSelectedFit()}")
    finally benchmark.teardown()
