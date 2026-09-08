package scalafim.fmri.fit

import gale.linalg.DMat
import java.lang.management.ManagementFactory
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption as Open}
import java.security.MessageDigest
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*
import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache, NiftiStagingLimits}
import scalafim.fmri.model.{FitPlan, FitStrategy}

/** File-backed qualification, not a production result format or publication API.
  * Separate JVM invocations expose whole-process RSS for selected/full routes.
  * Payload reads include decoding; compute includes native matrix conversion.
  * First/repeated passes do not imply cold operating-system file caches.
  */
object SelectedFirstLevelFileBenchmark:
  private def checked[E, A](value: Either[E, A]): A =
    value.fold(error => throw IllegalStateException(error.toString), identity)
  private def quote(value: String): String = "\"" + value.flatMap {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case c if c < ' ' => f"\\u${c.toInt}%04x"
    case c => c.toString
  } + "\""
  private def obj(fields: (String, String)*): String =
    fields.map((key, value) => quote(key) + ":" + value).mkString("{", ",", "}")
  private def array(values: Iterable[String]): String = values.mkString("[", ",", "]")
  private def writeJson(path: Path, value: String): Unit =
    val _ = Files.writeString(path, value + "\n", Open.CREATE_NEW)
  private def timed[A](operation: => A): (A, Long) =
    val start = System.nanoTime()
    val value = operation
    value -> (System.nanoTime() - start)
  private def digest(path: Path): String =
    val hash = MessageDigest.getInstance("SHA-256")
    val input = Files.newInputStream(path)
    try
      val bytes = new Array[Byte](1024 * 1024)
      var count = input.read(bytes)
      while count >= 0 do
        hash.update(bytes, 0, count)
        count = input.read(bytes)
    finally input.close()
    hash.digest().map(b => f"${b & 0xff}%02x").mkString

  private def fixture(representation: String, route: String): SelectedFirstLevelBenchmark =
    val result = new SelectedFirstLevelBenchmark
    result.representation = representation
    result.route = route
    result.scansPerRun = 320
    result.nuisanceColumns = 16
    result.voxels = 64
    result.blockVoxels = 32
    result.setup()
    result

  private def writeNifti(path: Path, plan: FitPlan, voxels: Int): Unit =
    val model = plan.model
    val width = math.min(voxels, 256)
    require(voxels > 0 && voxels % width == 0 && voxels / width <= 32767)
    val signal = Array.tabulate(model.nTimepoints, 2) { (row, component) =>
      var total = 0.0
      var column = 0
      while column < model.nPredictors do
        total += model.designMatrix(row, column) * math.sin((column + 1) * (component + 1) * 0.23)
        column += 1
      total
    }
    val spatialA = Array.tabulate(voxels)(voxel => math.sin((voxel + 1) * 0.017))
    val spatialB = Array.tabulate(voxels)(voxel => math.cos((voxel + 1) * 0.011))
    val header = ByteBuffer.allocate(352).order(ByteOrder.LITTLE_ENDIAN)
    header.putInt(0, 348)
    header.putShort(40, 4.toShort)
    header.putShort(42, width.toShort)
    header.putShort(44, (voxels / width).toShort)
    header.putShort(46, 1.toShort)
    header.putShort(48, model.nTimepoints.toShort)
    header.putShort(70, 64.toShort)
    header.putShort(72, 64.toShort)
    header.putFloat(76, 1.0f)
    header.putFloat(80, 1.0f)
    header.putFloat(84, 1.0f)
    header.putFloat(88, 1.0f)
    header.putFloat(92, 1.0f)
    header.putFloat(108, 352.0f)
    header.putFloat(112, 1.0f)
    header.put(344, 'n'.toByte)
    header.put(345, '+'.toByte)
    header.put(346, '1'.toByte)
    val channel = FileChannel.open(path, Open.CREATE_NEW, Open.WRITE)
    try
      while header.hasRemaining do
        val _ = channel.write(header)
      val buffer = ByteBuffer.allocateDirect(1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)
      var row = 0
      while row < model.nTimepoints do
        var voxel = 0
        while voxel < voxels do
          val value = signal(row)(0) * spatialA(voxel) + signal(row)(1) * spatialB(voxel) +
            0.15 * math.sin((row + 1) * 1.731 + (voxel + 1) * 0.071) * (1.0 + row / 320)
          buffer.putDouble(value)
          if !buffer.hasRemaining then
            buffer.flip()
            while buffer.hasRemaining do
              val _ = channel.write(buffer)
            val _ = buffer.clear()
          voxel += 1
        row += 1
      buffer.flip()
      while buffer.hasRemaining do
        val _ = channel.write(buffer)
      channel.force(true)
    finally channel.close()

  private def create(root: Path, representation: String, voxels: Int): Unit =
    require(voxels >= 64)
    require(!Files.exists(root), "Use a new fixture directory")
    Files.createDirectories(root)
    val small = fixture(representation, "shared")
    val (plan, _, reader) = small.fileQualificationInputs
    val expandedBytes = 352L + voxels.toLong * plan.model.nTimepoints * 8L
    require(Files.getFileStore(root).getUsableSpace > expandedBytes * 3L + 256L * 1024 * 1024,
      "Fixture and staged copies must fit with a 256 MiB free-space floor")
    val raw = root.resolve("source.nii")
    val (_, writeNs) = timed(writeNifti(raw, plan, voxels))
    val compressed = root.resolve("source.nii.gz")
    val (_, compressionNs) = timed {
      val input = Files.newInputStream(raw)
      val output = new GZIPOutputStream(Files.newOutputStream(compressed, Open.CREATE_NEW))
      try input.transferTo(output)
      finally
        try input.close() finally output.close()
    }
    val cache = checked(NiftiStagingCache.make(root.resolve("cache"),
      NiftiStagingLimits(expandedBytes, 256L * 1024 * 1024, expandedBytes + 65L)))
    val (staged, coldNs) = timed(checked(NiftiResponseBlockSource.open(compressed, Some(cache))))
    val (reused, warmNs) = timed(checked(NiftiResponseBlockSource.open(compressed, Some(cache))))
    require(staged.wasStaged && reused.dataPath == staged.dataPath)
    require(Files.size(raw) == expandedBytes && digest(raw) == digest(staged.dataPath))
    val samples = Vector(0, 31, 63)
    require(voxels >= 64)
    val selection = DataSelection(voxels = VoxelSelection.indices(samples*))
    val expected = checked(reader.seriesEither(selection)).data
    val actual = checked(staged.readBlock(selection)).data
    for row <- 0 until expected.rows; col <- 0 until expected.cols do
      require(java.lang.Double.doubleToRawLongBits(actual(row, col)) ==
        java.lang.Double.doubleToRawLongBits(expected(row, col)), "NIfTI payload differs from the qualified resident fixture")
    writeJson(root.resolve("fixture.json"), obj(
      "schema" -> quote("scalafim.selected-file-fixture.v1"), "status" -> quote("passed"),
      "representation" -> quote(representation), "voxels" -> voxels.toString,
      "scans" -> plan.model.nTimepoints.toString, "expanded_bytes" -> expandedBytes.toString,
      "compressed_bytes" -> Files.size(compressed).toString, "source_sha256" -> quote(digest(raw)),
      "staged_path" -> quote(staged.dataPath.toString), "fixture_write_ns" -> writeNs.toString,
      "fixture_compression_ns" -> compressionNs.toString,
      "verified_cold_staging_open_ns" -> coldNs.toString, "verified_reused_staging_open_ns" -> warmNs.toString,
      "scope" -> quote("Staging times include integrity checks and header opening. Cold means a missing staging entry, not a cold OS cache. Fixture construction is excluded from estimator comparisons.")))
    println(s"SELECTED_FILE_FIXTURE ${root.resolve("fixture.json")}")

  private final class MeasuredReader(val dataset: FmriDataset, source: NiftiResponseBlockSource, limit: Int)
      extends DatasetSeriesReader:
    var reads = 0
    var largestRead = 0
    var readNs = 0L
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      val resolved = checked(dataset.resolve(selection))
      require(resolved.nVoxels <= limit, "Response read exceeds the declared limit")
      largestRead = math.max(largestRead, resolved.nVoxels)
      reads += 1
      val start = System.nanoTime()
      try source.readBlock(selection)
      finally readNs += System.nanoTime() - start

  /** Test-only raw little-endian output rows; no scientific archive is implied. */
  private final class Sink(path: Path, rows: Int, voxels: Int, block: Int):
    private val channel = FileChannel.open(path, Open.CREATE_NEW, Open.WRITE)
    private val buffer = ByteBuffer.allocateDirect(block * 8).order(ByteOrder.LITTLE_ENDIAN)
    var writeNs = 0L
    var writtenVoxels = 0
    def write(indices: Vector[Int], at: (Int, Int) => Double): Unit =
      val start = System.nanoTime()
      require(indices == Vector.range(writtenVoxels, writtenVoxels + indices.size))
      require(indices.size <= block)
      var output = 0
      while output < rows do
        buffer.clear()
        var local = 0
        while local < indices.size do
          val value = at(output, local)
          require(java.lang.Double.isFinite(value), "Non-finite estimate cannot enter the comparison sink")
          buffer.putDouble(value)
          local += 1
        buffer.flip()
        var position = (output.toLong * voxels + writtenVoxels) * 8L
        while buffer.hasRemaining do position += channel.write(buffer, position)
        output += 1
      writtenVoxels += indices.size
      writeNs += System.nanoTime() - start
    def finish(): Long =
      require(writtenVoxels == voxels)
      val start = System.nanoTime()
      channel.force(true)
      System.nanoTime() - start
    def close(): Unit = channel.close()

  private def extracted(block: SelectedEstimateBlock): (DMat, Vector[Int]) = block match
    case SelectedEstimateBlock.Shared(value) =>
      require(value.result.uncertainty == OlsEstimateUncertainty.NotRequested)
      value.result.estimates -> value.voxelIndices
    case SelectedEstimateBlock.Pooled(value) => value.result match
      case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
        require(exclusions.isEmpty && result.uncertainty == FixedEffectsEstimateUncertainty.NotRequested)
        result.estimates -> result.voxelIndices
      case other => throw IllegalStateException(s"Unexpected exclusions: $other")

  private def writeOracle(path: Path, source: NiftiResponseBlockSource, plan: FitPlan,
      prepared: PreparedSelectedEstimates, selection: CompiledEstimateSelection,
      voxels: Int, output: Path, name: String): Unit =
    val samples = Vector(0, voxels / 2, voxels - 1)
    val response = checked(source.readBlock(DataSelection(voxels = VoxelSelection.indices(samples*)))).data
    val estimates = Array.ofDim[Double](selection.outputs.size, samples.size)
    val channel = FileChannel.open(output, Open.READ)
    try
      val buffer = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
      for row <- estimates.indices; sample <- samples.indices do
        buffer.clear()
        var offset = (row.toLong * voxels + samples(sample)) * 8L
        while buffer.hasRemaining do
          val count = channel.read(buffer, offset)
          require(count > 0, "Timed estimate output is truncated")
          offset += count
        buffer.flip()
        estimates(row)(sample) = buffer.getDouble()
    finally channel.close()
    def ints(values: Iterable[Int]): String = array(values.map(_.toString))
    def rows(n: Int, m: Int)(value: (Int, Int) => Double): String =
      array(Vector.tabulate(n)(row => array(Vector.tabulate(m) { col =>
        val number = value(row, col)
        require(java.lang.Double.isFinite(number))
        number.toString
      })))
    val schema = plan.model.designSchema.get
    val weights = selection.readout.weights
    val runFields = prepared match
      case PreparedSelectedEstimates.Shared(_) => ""
      case PreparedSelectedEstimates.Pooled(pooled) =>
        val columns = pooled.pooledCoefficientAxis.columnIds.map(schema.coefficientAxis.columnIds.indexOf)
        val runs = pooled.runPreparations.map { run =>
          val slice = checked(schema.runwiseSlice(
            scalafim.fmri.design.RunIndex.unsafeOneBased(run.partition.runIndex + 1),
            pooled.timepoints.map(index => scalafim.fmri.design.ScanIndex.unsafeOneBased(index + 1))))
          obj("rows" -> ints(slice.sourceRowIndices), "sourceColumns" -> ints(slice.sourceColumnIndices))
        }
        ",\"pooledSourceColumns\":" + ints(columns) + ",\"runs\":" + array(runs)
    val result = obj("name" -> quote(name), "route" -> quote(if plan.engine == scalafim.fmri.model.FitEngine.FixedEffects then "pooled" else "shared"),
      "sampleVoxelIndices" -> ints(samples), "design" -> rows(plan.model.nTimepoints, plan.model.nPredictors)(plan.model.designMatrix.apply),
      "response" -> rows(response.rows, response.cols)(response.apply),
      "weights" -> rows(weights.rows, weights.cols)(weights.apply),
      "estimates" -> rows(estimates.length, samples.size)((row, col) => estimates(row)(col)))
    val completed = result.dropRight(1) + runFields + "}"
    writeJson(path, obj("schema" -> quote("scalafim.selected-estimation-fixtures.v1"),
      "galeQrImplementationSha256" -> quote(SelectedEstimationFixtureExport.galeQrImplementationSha256),
      "cases" -> array(Vector(completed))))

  private def run(root: Path, representation: String, route: String, variant: String,
      voxels: Int, blockSize: Int, destination: Path): Unit =
    require(Set("selected", "full").contains(variant))
    require(voxels >= 64 && blockSize > 0 && voxels > blockSize)
    require(!Files.exists(destination), "Use a new execution directory")
    Files.createDirectories(destination)
    val small = fixture(representation, route)
    val (original, request, _) = small.fileQualificationInputs
    val raw = root.resolve("source.nii")
    val (beforeHash, verificationNs) = timed(digest(raw))
    val (source, openNs) = timed(checked(NiftiResponseBlockSource.open(raw)))
    require(source.shape.spatialSize == voxels)
    val d = original.model.dataset
    val dataset = checked(FmriDataset.describe(d.id, source.shape, source.voxelDomain, d.metadata,
      d.samplingFrame, d.timeAxis.runIds, d.events))
    val plan = FitPlan(original.model.copy(dataset = dataset),
      if route == "pooled" then FitStrategy.SeparateRunsThenFixedEffects() else FitStrategy.OrdinaryLeastSquares())
    val chunk = checked(ChunkSize(blockSize))
    val (prepared, preparationNs) = timed(checked(SelectedEstimates.prepare(plan, request, chunk)))
    val selection = checked(request.compile(plan.model.designSchema.get))
    val outputs = selection.outputs.size
    val passes = Vector.tabulate(2) { pass =>
      val reader = new MeasuredReader(dataset, source, if variant == "selected" then blockSize else voxels)
      ManagementFactory.getMemoryPoolMXBeans.asScala.foreach(_.resetPeakUsage())
      val target = destination.resolve(s"pass-$pass.f64le")
      val sink = new Sink(target, outputs, voxels, blockSize)
      val started = System.nanoTime()
      var forceNs = 0L
      try
        if variant == "selected" then
          val outcome = checked(prepared.foreachBlock(reader, value => {
            val (matrix, indices) = extracted(value)
            sink.write(indices, matrix.apply)
            Right(())
          }))
          require(outcome == EstimateExecutionOutcome.Completed((voxels + blockSize - 1) / blockSize, voxels))
        else
          val result = checked(FitPlanExecutor.fitChunked(reader, plan, DataSelection.All,
            FitChunkingStrategy.ByVoxelCount(chunk)))
          val coefficients = result match
            case value: DenseFmriFitResult => value.coefficients
            case value: FixedEffectsFmriFitResult => value.coefficients
            case other => throw IllegalStateException(s"Unexpected full engine: ${other.engine}")
          val axis = plan.model.designSchema.get.coefficientAxis
          val columns = result.coefficientAxis.get.columnIds.map(axis.columnIds.indexOf)
          require(columns.forall(_ >= 0))
          val weights = Vector.tabulate(outputs)(output => columns.zipWithIndex.flatMap { (sourceColumn, local) =>
            val weight = selection.readout.weights(output, sourceColumn)
            if weight == 0.0 then None else Some(local -> weight)
          })
          Vector.range(0, voxels).grouped(blockSize).foreach { indices =>
            sink.write(indices, (output, local) => weights(output).foldLeft(0.0) { case (sum, (column, weight)) =>
              sum + weight * coefficients(column, indices(local))
            })
          }
        forceNs = sink.finish()
      finally sink.close()
      val executionNs = System.nanoTime() - started
      require(Files.size(target) == outputs.toLong * voxels * 8L)
      val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.map { pool =>
        obj("name" -> quote(pool.getName), "kind" -> quote(pool.getType.toString),
          "independent_peak_used_bytes" -> Option(pool.getPeakUsage).fold("null")(_.getUsed.toString))
      }.toVector
      obj("pass" -> pass.toString, "output" -> quote(target.toString),
        "execution_ns" -> executionNs.toString, "payload_read_decode_ns" -> reader.readNs.toString,
        "sink_write_ns" -> sink.writeNs.toString, "sink_force_ns" -> forceNs.toString,
        "fit_conversion_and_remaining_ns" -> (executionNs - reader.readNs - sink.writeNs - forceNs).toString,
        "reads" -> reader.reads.toString, "largest_read_voxels" -> reader.largestRead.toString,
        "output_sha256" -> quote(digest(target)), "memory_pools" -> array(pools))
    }
    val cancelReader = new MeasuredReader(dataset, source, blockSize)
    var cancel = false
    val stopped = checked(prepared.foreachBlock(cancelReader, _ => { cancel = true; Right(()) }, () => cancel))
    require(voxels > blockSize && stopped == EstimateExecutionOutcome.Cancelled(1, blockSize) && cancelReader.reads == 1)
    writeOracle(destination.resolve("oracle.json"), source, plan, prepared, selection, voxels,
      destination.resolve("pass-1.f64le"), s"$representation-$route-$variant-file")
    val (afterHash, finalVerificationNs) = timed(digest(raw))
    require(beforeHash == afterHash, "Source bytes changed during qualification")
    writeJson(destination.resolve("receipt.json"), obj(
      "schema" -> quote("scalafim.selected-file-execution.v1"), "status" -> quote("passed"),
      "representation" -> quote(representation), "route" -> quote(route), "variant" -> quote(variant),
      "voxels" -> voxels.toString, "outputs" -> outputs.toString, "block_voxels" -> blockSize.toString,
      "source_sha256" -> quote(beforeHash), "initial_source_verification_ns" -> verificationNs.toString,
      "final_source_verification_ns" -> finalVerificationNs.toString, "header_open_ns" -> openNs.toString,
      "selected_preparation_ns" -> preparationNs.toString, "passes" -> array(passes),
      "output_ids" -> array(selection.outputs.map(output => quote(output.id.toString))),
      "cancellation" -> quote("one delivered block, one read, no later read"),
      "gale_qr_implementation_sha256" -> quote(SelectedEstimationFixtureExport.galeQrImplementationSha256),
      "scope" -> quote("Two first/repeated file-backed passes, not cold OS caches. Selected preparation is separate; full execution includes its own preparation. Read time includes decoding; remaining fit time includes conversion and orchestration. Pool peaks are independent high-water marks and must not be summed as simultaneous peak heap. Whole-process RSS includes JVM startup and a small qualified fixture. Raw sink is test-only, not durable scientific publication.")))
    println(s"SELECTED_FILE_EXECUTION ${destination.resolve("receipt.json")}")

  def main(args: Array[String]): Unit = args.toVector match
    case Vector("create", root, representation, voxels) => create(Path.of(root), representation, voxels.toInt)
    case Vector("run", root, representation, route, variant, voxels, block, destination) =>
      run(Path.of(root), representation, route, variant, voxels.toInt, block.toInt, Path.of(destination))
    case _ => throw IllegalArgumentException("create <new-root> <representation> <voxels> | run <root> <representation> <shared|pooled> <selected|full> <voxels> <block> <new-output>")
