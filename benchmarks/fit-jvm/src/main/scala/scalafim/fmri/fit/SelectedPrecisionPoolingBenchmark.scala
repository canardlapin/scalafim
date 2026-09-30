package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import org.openjdk.jmh.annotations.*
import scalafim.fmri.design.{ColumnRole, DesignAudit, DesignSchema, ModulatorId, RowLayout, RunIndex as DesignRunIndex, RunScope, ScanIndex, StructuralColumn, StructuralColumnOrigin}
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** Exact selected multivariate pooling component, including readout and
  * optional uncertainty. The response-gather/QR and IO phases are separate.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class SelectedPrecisionPoolingBenchmark:
  @Param(Array("2", "64"))
  var outputs: Int = 0

  @Param(Array("none", "marginal"))
  var uncertainty: String = ""

  private var statistics: FixedEffectsSufficientStatistics = uninitialized
  private var selection: CompiledEstimateSelection = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    val rows = 128
    val voxels = 8192
    val columns = Vector.tabulate(outputs) { i =>
      StructuralColumn.fromOrigin(i + 1,
        StructuralColumnOrigin.Sampled(ModulatorId.unsafe(s"fir-$i"), ColumnRole.Task, RunScope.Global),
        s"fir-$i").fold(e => throw IllegalArgumentException(e.message), identity)
    }
    val schema = DesignSchema.validated(
      Mat.unsafe(rows, outputs, Array.tabulate(rows * outputs) { index =>
        val row = index / outputs
        val col = index % outputs
        if row % outputs == col then 1.0 else 0.0
      }),
      RowLayout(Vector.tabulate(rows)(i => DesignRunIndex.unsafeOneBased(i / 64 + 1)),
        Vector.tabulate(rows)(i => Seconds((i % 64).toDouble)),
        Vector.tabulate(rows)(i => ScanIndex.unsafeOneBased(i + 1))),
      columns,
      DesignAudit()
    ).fold(e => throw IllegalArgumentException(e.message), identity)
    val axis = schema.coefficientAxis
    val request = FirstLevelEstimateRequest.make(
      axis.columnIds.map(EstimateOutput.Coefficient.apply),
      if uncertainty == "none" then EstimateUncertaintyRequest.None else EstimateUncertaintyRequest.Marginal
    ).fold(e => throw IllegalArgumentException(e.message), identity)
    selection = request.compile(schema).fold(e => throw IllegalArgumentException(e.message), identity)
    val baseA = precision(outputs, 0.15)
    val baseB = precision(outputs, -0.09)
    val runA = contribution(0, 0, baseA, voxels)
    val runB = contribution(1, 64, baseB, voxels)
    statistics = FixedEffectsSufficientStatistics(axis, (0 until voxels).toVector,
      Vector(runA, runB), sourceColumnIndices = (0 until outputs).toVector)

  @Benchmark
  def selectedPooling(): FixedEffectsEstimateResult =
    FixedEffectsEstimates.estimate(statistics, selection)
      .fold(e => throw IllegalArgumentException(e.message), identity)

  @Benchmark
  def precisionAccumulation(): Double =
    val fields = statistics.contributions.map(_.precisionByVoxel)
    var checksum = 0.0
    var voxel = 0
    while voxel < statistics.voxelIndices.length do
      checksum += CoefficientMatrixStorage.sumAt(fields, voxel)(0, 0)
      voxel += 1
    checksum

  private def precision(size: Int, correlation: Double): DMat =
    Matrix.tabulate(size, size) { (row, col) =>
      if row == col then 2.0 + row.toDouble / size
      else correlation / (1.0 + math.abs(row - col))
    }

  private def contribution(run: Int, start: Int, base: DMat, voxels: Int): FixedEffectsRunContribution =
    val scales = Vector.tabulate(voxels)(voxel => 0.75 + (voxel % 17).toDouble / 20.0)
    val field = CoefficientMatrixStorage.scaled(base, scales)
      .fold(e => throw IllegalArgumentException(e.message), identity)
    val weighted = Matrix.tabulate(outputs, voxels) { (row, voxel) =>
      scales(voxel) * (0.2 + math.sin((row + 1) * 0.13 + (voxel + 1) * 0.017))
    }
    FixedEffectsRunContribution(run, 64, (start until start + 64).toVector,
      ResidualDegreesOfFreedom(32).fold(e => throw IllegalArgumentException(e.message), identity),
      field, weighted, (0 until outputs).toVector)
