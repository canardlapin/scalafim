package scalafim.fmri.fit

import gale.backend.Backend
import gale.linalg.DMat
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import scala.compiletime.uninitialized
import scalafim.dataset.*
import scalafim.fmri.design.StructuralColumnOrigin
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.*
import scalafim.image.{DMat as ImageDMat, NeuroSpace}

/** Public dataset-path costs on an already resident synthetic response source.
  * This deliberately does not claim filesystem, decompression, verification,
  * durable-output or whole-application throughput. Trial setup compares every
  * requested value with the existing full-fit route before timing either route.
  * The legacy full shared route may read all voxels during preparation; that
  * observed read size is reported separately, and never allowed in the selected
  * reader. Both routes retain their original scientific and resource behavior.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(2)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class SelectedFirstLevelBenchmark:
  @Param(Array("condition", "contrast", "FIR"))
  var representation: String = ""

  @Param(Array("shared", "pooled"))
  var route: String = ""

  @Param(Array("320"))
  var scansPerRun: Int = 0

  @Param(Array("16"))
  var nuisanceColumns: Int = 0

  @Param(Array("1024", "8192"))
  var voxels: Int = 0

  @Param(Array("512"))
  var blockVoxels: Int = 0

  private var fitPlan: FitPlan = uninitialized
  private var request: FirstLevelEstimateRequest = uninitialized
  private var reader: DatasetSeriesReader = uninitialized
  private var fullReader: DatasetSeriesReader = uninitialized
  private var largestFullRead: Int = 0
  private var prepared: PreparedSelectedEstimates = uninitialized
  private var chunkSize: ChunkSize = uninitialized

  private def checked[E, A](value: Either[E, A]): A =
    value.fold(error => throw IllegalStateException(error.toString), identity)

  @Setup(Level.Trial)
  def setup(): Unit =
    require(Set("condition", "contrast", "FIR").contains(representation))
    require(Set("shared", "pooled").contains(route))
    require(scansPerRun >= 160 && nuisanceColumns >= 0 && voxels > 0)
    val runs = 2
    val frame = SamplingFrame(blockLens = Vector.fill(runs)(scansPerRun), tr = Vector.fill(runs)(1.0))
    val events = DatasetEvents(Vector.tabulate(runs, 32) { (run, event) =>
      Map("onset" -> (5.0 + event * (scansPerRun - 35.0) / 32.0 + ((event * 7 + run * 3) % 5) * 0.25).toString,
        "condition" -> s"C${event % 8}", "run" -> s"run-${run + 1}")
    }.flatten)
    val empty = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("selective-benchmark"),
      ImageDMat.fromRows(Vector.fill(scansPerRun * runs)(Vector.fill(voxels)(0.0))), NeuroSpace(Vector(voxels, 1, 1))), frame, events)
    val nuisance = if nuisanceColumns == 0 then None else
      Some(checked(NuisanceRegressors.fromRuns(Vector.tabulate(runs) { run =>
        checked(SampledRegressorRun.fromColumns(Vector.tabulate(nuisanceColumns) { column =>
          s"nuisance-$column" -> Vector.tabulate(scansPerRun) { row =>
            math.sin((row + 1) * (column + 1) * 0.061 + run * 0.43) +
              0.1 * math.cos((row + 3) * (column + 2) * 0.037 + run * 0.73)
          }
        }*))
      })))
    val basis = if representation == "FIR" then Hrfs.fir(nBasis = 8, span = 16.s) else Hrfs.SPMG1
    val model = FmriModelBuilder.buildModel(empty, ModelBuildSpec("onset ~ hrf(condition)",
      blockColumn = Some("run"), defaultHrf = basis, baselineIntercept = Intercept.Runwise,
      nuisance = nuisance, precision = 0.25.s))
    val axis = model.designSchema.get.coefficientAxis
    val task = axis.columns.filter(_.origin.isInstanceOf[StructuralColumnOrigin.Event])
    require(task.length == (if representation == "FIR" then 64 else 8))
    val signal = Vector.tabulate(model.nTimepoints, 2) { (row, component) =>
      var total = 0.0
      var col = 0
      while col < model.nPredictors do
        total += model.designMatrix(row, col) * math.sin((col + 1) * (component + 1) * 0.23)
        col += 1
      total
    }
    val values = Vector.tabulate(model.nTimepoints, voxels) { (row, voxel) =>
      signal(row)(0) * math.sin((voxel + 1) * 0.017) + signal(row)(1) * math.cos((voxel + 1) * 0.011) +
        0.15 * math.sin((row + 1) * 1.731 + (voxel + 1) * 0.071) * (1.0 + row / scansPerRun)
    }
    val populated = FmriDataset.unsafe(InMemoryDatasetBackend(empty.id, ImageDMat.fromRows(values), empty.shape.space), frame, events)
    fitPlan = FitPlan(model.copy(dataset = populated),
      if route == "pooled" then FitStrategy.SeparateRunsThenFixedEffects() else FitStrategy.OrdinaryLeastSquares())
    reader = new DatasetSeriesReader:
      val dataset: FmriDataset = populated
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        val resolved = checked(selection.resolveEither(dataset.acquisitionDomain))
        require(resolved.voxels.size <= blockVoxels, "Public path exceeded its declared response block limit")
        dataset.seriesEither(selection)
    fullReader = new DatasetSeriesReader:
      val dataset: FmriDataset = populated
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        val resolved = checked(selection.resolveEither(dataset.acquisitionDomain))
        largestFullRead = math.max(largestFullRead, resolved.voxels.size)
        dataset.seriesEither(selection)
    val outputs = if representation == "contrast" then
      Vector(EstimateOutput.Contrast(StructuralTContrast.fromIds(ContrastId.unsafe("C0-minus-C7"), "C0 minus C7",
        Map(task.head.id -> 1.0, task.last.id -> -1.0))))
      else task.map(column => EstimateOutput.Coefficient(column.id))
    request = checked(FirstLevelEstimateRequest.make(outputs))
    chunkSize = checked(ChunkSize(blockVoxels))
    prepared = checked(SelectedEstimates.prepare(fitPlan, request, chunkSize))
    val full = checked(FitPlanExecutor.fitChunked(fullReader, fitPlan, DataSelection.All, FitChunkingStrategy.ByVoxelCount(chunkSize)))
    val coefficients = full match
      case value: DenseFmriFitResult => value.coefficients
      case value: FixedEffectsFmriFitResult => value.coefficients
      case other => throw IllegalStateException(s"Unexpected full benchmark route: ${other.engine}")
    val fullAxis = full.coefficientAxis.get
    val selected = checked(request.compile(model.designSchema.get))
    val positions = fullAxis.columnIds.map(axis.columnIds.indexOf)
    require(positions.forall(_ >= 0))
    val weights = selected.readout.weights
    var maximumError = 0.0
    var seen = 0
    val outcome = checked(prepared.foreachBlock(reader, block => {
      val (estimates, indices) = extract(block)
      indices.zipWithIndex.foreach { (voxel, local) =>
        var output = 0
        while output < estimates.rows do
          var expected = 0.0
          var column = 0
          while column < positions.length do
            expected += weights(output, positions(column)) * coefficients(column, voxel)
            column += 1
          val error = math.abs(estimates(output, local) - expected)
          require(error <= 1e-8 * math.max(1.0, math.abs(expected)), s"Selected/full equivalence failed: $error")
          maximumError = math.max(maximumError, error)
          output += 1
        seen += 1
      }
      Right(())
    }))
    require(outcome == EstimateExecutionOutcome.Completed((voxels + blockVoxels - 1) / blockVoxels, voxels) && seen == voxels)
    println(s"SELECTED_EQUIVALENCE representation=$representation route=$route scans=${model.nTimepoints} columns=${model.nPredictors} outputs=${outputs.length} voxels=$voxels block=$blockVoxels largestFullRead=$largestFullRead backend=${summon[Backend].name} maximumError=$maximumError galeQrImplementationSha256=${SelectedEstimationFixtureExport.galeQrImplementationSha256}")

  /** Export realized inputs separately from timing for an independent solver.
    * Samples retain their source voxel identity; no shared seed or planted beta
    * is used as the numerical oracle.
    */
  def fixtureJson(sampleVoxels: Vector[Int]): String =
    require(sampleVoxels.nonEmpty && sampleVoxels.distinct == sampleVoxels &&
      sampleVoxels.forall(voxel => voxel >= 0 && voxel < voxels))
    val model = fitPlan.model
    val schema = model.designSchema.get
    val compiled = checked(request.compile(schema))
    val response = checked(reader.seriesEither(DataSelection(voxels = VoxelSelection.indices(sampleVoxels*))))
    val selected = Array.fill(compiled.outputs.length, sampleVoxels.length)(0.0)
    val seen = Array.fill(sampleVoxels.length)(false)
    checked(prepared.foreachBlock(reader, block => {
      val (estimates, indices) = extract(block)
      indices.zipWithIndex.foreach { (sourceVoxel, local) =>
        val sample = sampleVoxels.indexOf(sourceVoxel)
        if sample >= 0 then
          require(!seen(sample))
          seen(sample) = true
          var output = 0
          while output < estimates.rows do
            selected(output)(sample) = estimates(output, local)
            output += 1
      }
      Right(())
    }))
    require(seen.forall(identity))
    def numbers(values: Iterable[Double]): String =
      require(values.forall(java.lang.Double.isFinite))
      values.mkString("[", ",", "]")
    def ints(values: Iterable[Int]): String = values.mkString("[", ",", "]")
    def rows(n: Int, m: Int)(value: (Int, Int) => Double): String =
      Vector.tabulate(n)(row => numbers(Vector.tabulate(m)(column => value(row, column)))).mkString("[", ",", "]")
    val design = rows(model.nTimepoints, model.nPredictors)(model.designMatrix.apply)
    val data = rows(model.nTimepoints, sampleVoxels.length)(response.data.apply)
    val weights = rows(compiled.readout.weights.rows, compiled.readout.weights.cols)(compiled.readout.weights.apply)
    val estimates = selected.iterator.map(row => numbers(row.toVector)).mkString("[", ",", "]")
    val runFields = prepared match
      case PreparedSelectedEstimates.Shared(_) => ""
      case PreparedSelectedEstimates.Pooled(plan) =>
        val pooled = plan.pooledCoefficientAxis.columnIds.map(schema.coefficientAxis.columnIds.indexOf)
        val runs = plan.runPreparations.map { run =>
          val slice = checked(schema.runwiseSlice(scalafim.fmri.design.RunIndex.unsafeOneBased(run.partition.runIndex + 1),
            plan.timepoints.map(index => scalafim.fmri.design.ScanIndex.unsafeOneBased(index + 1))))
          s"""{"rows":${ints(slice.sourceRowIndices)},"sourceColumns":${ints(slice.sourceColumnIndices)}}"""
        }.mkString("[", ",", "]")
        s""","pooledSourceColumns":${ints(pooled)},"runs":$runs"""
    s"""{"name":"$representation-$route","route":"$route","sampleVoxelIndices":${ints(sampleVoxels)},"design":$design,"response":$data,"weights":$weights,"estimates":$estimates$runFields}"""

  /** Small realized fixture reused by file-backed qualification. */
  private[fit] def fileQualificationInputs: (FitPlan, FirstLevelEstimateRequest, DatasetSeriesReader) =
    (fitPlan, request, reader)

  @Benchmark
  def selectedPreparation(): PreparedSelectedEstimates =
    checked(SelectedEstimates.prepare(fitPlan, request, chunkSize))

  @Benchmark
  def selectedPreparedDataset(blackhole: Blackhole): EstimateExecutionOutcome =
    checked(prepared.foreachBlock(reader, block => { blackhole.consume(block); Right(()) }))

  @Benchmark
  def fullDatasetBaseline(): FmriFitResult =
    checked(FitPlanExecutor.fitChunked(fullReader, fitPlan, DataSelection.All, FitChunkingStrategy.ByVoxelCount(chunkSize)))

  private def extract(block: SelectedEstimateBlock): (DMat, Vector[Int]) = block match
    case SelectedEstimateBlock.Shared(value) =>
      require(value.result.uncertainty == OlsEstimateUncertainty.NotRequested)
      value.result.estimates -> value.voxelIndices
    case SelectedEstimateBlock.Pooled(value) => value.result match
      case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
        require(exclusions.isEmpty && result.uncertainty == FixedEffectsEstimateUncertainty.NotRequested)
        result.estimates -> result.voxelIndices
      case other => throw IllegalStateException(s"Unexpected benchmark exclusions: $other")


/** JVM-only fixture writer; it does no performance measurement. */
object SelectedEstimationFixtureExport:
  lazy val galeQrImplementationSha256: String =
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    Vector("QR$.class", "DenseDecompositions$.class").foreach { name =>
      val stream = classOf[gale.linalg.QR].getResourceAsStream(name)
      require(stream != null, "The loaded Gale QR implementation must have inspectable bytes")
      val bytes = try stream.readAllBytes() finally stream.close()
      digest.update(name.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      digest.update(bytes)
    }
    digest.digest().map(value => f"${value & 0xff}%02x").mkString

  def main(args: Array[String]): Unit =
    require(args.length == 1, "Provide a new fixture output path")
    val destination = java.nio.file.Path.of(args(0))
    require(!java.nio.file.Files.exists(destination), "Previous numerical evidence is immutable")
    val cases = for
      representation <- Vector("condition", "contrast", "FIR")
      route <- Vector("shared", "pooled")
    yield
      val fixture = new SelectedFirstLevelBenchmark
      fixture.representation = representation
      fixture.route = route
      fixture.scansPerRun = 320
      fixture.nuisanceColumns = 16
      fixture.voxels = 64
      fixture.blockVoxels = 32
      fixture.setup()
      fixture.fixtureJson(Vector(0, 31, 63))
    val json = "{\"schema\":\"scalafim.selected-estimation-fixtures.v1\",\"galeQrImplementationSha256\":\"" + galeQrImplementationSha256 + "\",\"cases\":" + cases.mkString("[", ",", "]") + "}\n"
    java.nio.file.Files.writeString(destination, json, java.nio.file.StandardOpenOption.CREATE_NEW)
    println(s"SELECTED_FIXTURES path=$destination cases=${cases.length} galeQrSource=${classOf[gale.linalg.QR].getProtectionDomain.getCodeSource.getLocation}")
