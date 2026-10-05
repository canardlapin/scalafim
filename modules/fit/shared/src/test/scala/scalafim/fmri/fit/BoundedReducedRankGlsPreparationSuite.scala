package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.baseline.{BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArCoefficientSpec, AutocorrelationConfig, FitControls, FitPlan, FitStrategy, FmriModel, MissingDataPolicy, ReducedRankBootstrapConfig, ReducedRankComponentSpec, ReducedRankGlsConfig, ReducedRankInferencePolicy}
import scalafim.image.SampleSpaces
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.fit.fixtures.ReducedRankGlsFmriregFixtures as Fixture
import ReducedRankGlsProjection.*
import scala.concurrent.ExecutionContext.Implicits.global

class BoundedReducedRankGlsPreparationSuite extends munit.FunSuite:
  private val fixed = AutocorrelationConfig.unsafe(order = 1, iterations = 0, coefficients = ArCoefficientSpec.Rho(0.0))
  private val bootstrap = ReducedRankBootstrapConfig.unsafe(replicates = 32, blockSize = 2, seed = 7)

  for inference <- Vector(ReducedRankInferencePolicy.Conditional, ReducedRankInferencePolicy.Bootstrap(bootstrap)) do
    test(s"bounded RRG $inference preserves fmrireg coefficients, covariance and contrasts") {
      val plan = makePlan(fixtureModel, ReducedRankComponentSpec.unsafeFixed(1), inference = inference)
      val expected = dense(FitPlanExecutor.fit(plan))
      val reader = new BoundedReader(plan, 1)
      val actual = dense(ChunkedFitExecutor.fit(reader, plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1)))
      assertClose(actual, expected)
      matrixClose(actual.coefficients.value, Fixture.partitionedRankOneCoefficients, 1e-10)
      if inference == ReducedRankInferencePolicy.Conditional then
        actual.inference.varianceScale.toVector.zip(Fixture.partitionedConditionalVariance)
          .foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
      else
        actual.voxelIndices.indices.foreach { voxel =>
          val covariance = actual.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel)
          val targets = Matrix.tabulate(2, 2)((row, col) => covariance(row, col))
          matrixClose(targets, Fixture.partitionedBootstrapCovariance(voxel), 1e-10)
        }
      val passes = if inference == ReducedRankInferencePolicy.Conditional then 4 else bootstrap.replicates.value + 3
      assertEquals(reader.requests.length, passes * 3)
      assertEquals(FitPreparation.describe(plan).topology, FitPreparationTopology.GlobalReduction)
      assert(FitPreparation.describe(plan).reductions.contains(FitPreparationReduction.SpatialBasis))
      assert(FitPreparation.describe(plan).supportsBoundedExecution(plan.engine))
    }

  for kind <- Vector("zero", "tiny", "rank-deficient", "tied", "nuisance"); components <- Vector(ReducedRankComponentSpec.unsafeFixed(1), ReducedRankComponentSpec.unsafeFixed(2), ReducedRankComponentSpec.Full) do
    test(s"exact base and bootstrap task scores survive $kind spectrum, components=$components") {
      val plan = makePlan(if kind == "nuisance" then populationModel() else spectrumModel(kind), components)
      val selected = DataSelection(voxels = IndexSelection.Indices((0 until 19).reverse.toVector))
      val expected = dense(FitPlanExecutor.fit(plan, selected))
      for width <- Vector(1, 7, 8, 9) do
        val reader = new BoundedReader(plan, width)
        val chunks = FitChunkPlan.fromSelection(plan, selected, FitChunkingStrategy.unsafeByVoxelCount(width)).toOption.get
        val source = BoundedResponseReplay.prepare(reader, plan, chunks).fold(error => fail(error.message), identity)
        val underlying = SynchronousFmriDataset.readerFor(plan.model.dataset).toOption.get
        val whole = underlying.seriesEither(selected).toOption.get
        val input = FitPlanExecutor.fitBlockInput(plan, whole, source.rawPartitions, source.responsePreparation).toOption.get
        val partition = FitInterpreters.reducedRankDesignPartition(plan).toOption.get
        val gls = Gls.prepare(source.design, ResponseBlock.unsafe(Matrix.zeros(source.design.timepoints, 1)), source.partitions,
          fixed.toLegacy, Vector(0)).toOption.get
        val whitening = gls.whitening match
          case GlsWhitening.Shared(value) => value
          case _ => fail("expected shared whitening")
        val white = WhiteningTransform(whitening, input.design.value, input.response.value).toOption.get
        val target = selectColumns(white.design, partition.targetColumns)
        val nuisance = selectColumns(white.design, partition.nuisanceColumns)
        val residualized = residualizeAgainstNuisance(target, white.response, nuisance).toOption.get
        val qr = fullRankQr(residualized.targetDesign).toOption.get
        val geometry = BoundedReducedRankGlsPreparation.Geometry(white.design, target, nuisance,
          residualized.targetDesign, qr, whitening, gls, Ols.prepare(DesignMatrix.unsafe(white.design)).toOption.get, partition)
        val request = ReducedRankGlsPrepared.rankRequest(FitInterpreters.reducedRankGlsConfig(plan).toOption.get,
          partition.targetPredictors, input.voxelIndices.length).toOption.get
        val scores = leadingQtRows(qr, residualized.response, partition.targetPredictors).toOption.get
        exactMatrix(BoundedReducedRankGlsPreparation.taskScores(source, geometry, None).toOption.get, scores)
        val task = fitTaskScores(qr, scores, request).toOption.get
        val fitted = residualized.targetDesign * task.coefficients
        val residuals = subtract(residualized.response, fitted)
        val rng = ParkMillerRng(7)
        for _ <- 0 until 6 do
          val indices = sampleIndices(source.design.timepoints, 3, rng)
          val sampled = resampledResponse(fitted, residuals, indices)
          val expectedScores = leadingQtRows(qr, sampled, partition.targetPredictors).toOption.get
          exactMatrix(BoundedReducedRankGlsPreparation.taskScores(source, geometry, Some(task -> indices)).toOption.get, expectedScores)
        assertClose(dense(ChunkedFitExecutor.fit(reader, plan, selected, FitChunkingStrategy.unsafeByVoxelCount(width))), expected)
    }

  test("bootstrap with tied and null task scores remains one global fit per replicate") {
    for kind <- Vector("zero", "tied") do
      val plan = makePlan(spectrumModel(kind), ReducedRankComponentSpec.unsafeFixed(1),
        inference = ReducedRankInferencePolicy.Bootstrap(ReducedRankBootstrapConfig.unsafe(replicates = 12, blockSize = 3, seed = 7)))
      val selected = DataSelection(voxels = IndexSelection.Indices((0 until 19).reverse.toVector))
      val expected = dense(FitPlanExecutor.fit(plan, selected))
      for width <- Vector(1, 7, 8, 9) do
        assertClose(dense(ChunkedFitExecutor.fit(new BoundedReader(plan, width), plan, selected,
          FitChunkingStrategy.unsafeByVoxelCount(width))), expected)
  }

  test("adaptive rank policies and V smaller than target count use global selected geometry") {
    for components <- Vector(ReducedRankComponentSpec.unsafeEnergyRetained(0.98),
        ReducedRankComponentSpec.unsafeResidualSumsOfSquaresBudget(0.5), ReducedRankComponentSpec.Full) do
      val plan = makePlan(fixtureModel, components)
      for selected <- Vector(DataSelection.All, DataSelection(voxels = IndexSelection.indices(2))) do
        assertClose(dense(ChunkedFitExecutor.fit(new BoundedReader(plan, 1), plan, selected,
          FitChunkingStrategy.unsafeByVoxelCount(1))), dense(FitPlanExecutor.fit(plan, selected)))
  }

  test("pooled estimated whitening, gaps and nuisance projection precede spatial learning") {
    val plan = makePlan(populationModel(), ReducedRankComponentSpec.unsafeFixed(1),
      ar = AutocorrelationConfig.unsafe(order = 1, iterations = 2))
    val selected = DataSelection(IndexSelection.Indices((0 until 24).filterNot(Set(3, 17)).toVector),
      IndexSelection.Indices((0 until 19).reverse.toVector))
    val expected = dense(FitPlanExecutor.fit(plan, selected))
    val reader = new BoundedReader(plan, 3, (0 until 24).filterNot(Set(3, 17)).toVector)
    assertClose(dense(ChunkedFitExecutor.fit(reader, plan, selected, FitChunkingStrategy.unsafeByVoxelCount(3))), expected)
    assert(reader.requests.length >= 5 * 7)
  }

  test("voxelwise full-rank fallback validates rank globally before one-voxel final blocks") {
    val ar = AutocorrelationConfig.unsafe(order = 1, iterations = 2, voxelwise = true)
    val full = makePlan(populationModel(), ReducedRankComponentSpec.unsafeFixed(2), ar = ar)
    assertClose(dense(ChunkedFitExecutor.fit(new BoundedReader(full, 1), full,
      chunking = FitChunkingStrategy.unsafeByVoxelCount(1))), dense(FitPlanExecutor.fit(full)))
    val compressed = makePlan(populationModel(), ReducedRankComponentSpec.unsafeFixed(1), ar = ar)
    val result = ChunkedFitExecutor.fit(new BoundedReader(compressed, 1), compressed,
      chunking = FitChunkingStrategy.unsafeByVoxelCount(1))
    assert(result.left.toOption.exists(_.isInstanceOf[FitError.UnsupportedEngine]))
  }

  test("excluded chunks are absent from learned state and all-excluded precedence is preserved") {
    val plan = makePlan(populationModel(exclude = true), ReducedRankComponentSpec.unsafeFixed(1), exclude = true)
    assertClose(dense(ChunkedFitExecutor.fit(new BoundedReader(plan, 1), plan,
      chunking = FitChunkingStrategy.unsafeByVoxelCount(1))), dense(FitPlanExecutor.fit(plan)))
    val empty = makePlan(populationModel(allExcluded = true), ReducedRankComponentSpec.Full, exclude = true)
    val result = ChunkedFitExecutor.fit(new BoundedReader(empty, 1), empty, chunking = FitChunkingStrategy.unsafeByVoxelCount(1))
    assert(result.left.toOption.exists(_.isInstanceOf[FitError.AllVoxelsExcluded]))
  }

  test("decoded RRG recipe preserves bounded execution and target-only inference") {
    val plan = makePlan(fixtureModel, ReducedRankComponentSpec.unsafeFixed(1))
    val reference = FitWorkReference.unsafe("RRG unit", "immutable plan", "immutable source")
    val descriptor = FitWorkDescriptor.compile(reference, plan, ChunkSize.unsafe(1)).toOption.get
    val decoded = FitWorkDescriptor.decode(descriptor.encode).toOption.get
    val resolver = new FitWorkResolver:
      def resolve(ref: FitWorkReference): Either[FitError, ResolvedFitWork] =
        Right(ResolvedFitWork(reference, plan, new BoundedReader(plan, 1)))
    assertClose(dense(FitWorkExecutor.fit(decoded, resolver)), dense(FitPlanExecutor.fit(plan)))
  }

  test("parallel final RRG blocks share one completed bootstrap preparation") {
    val plan = makePlan(fixtureModel, ReducedRankComponentSpec.unsafeFixed(1), inference = ReducedRankInferencePolicy.Bootstrap(bootstrap))
    val expected = dense(FitPlanExecutor.fit(plan))
    FutureChunkedFitExecutor.fit(new BoundedReader(plan, 1), plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1),
      parallelism = FitParallelism.unsafe(2)).map(result => assertClose(dense(result), expected))
  }

  test("final RRG fitting refuses a changed finite response snapshot") {
    val plan = makePlan(fixtureModel, ReducedRankComponentSpec.unsafeFixed(1))
    val reader = new BoundedReader(plan, 1)
    val chunks = FitChunkPlan.fromSelection(plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1)).toOption.get
    val context = BoundedReducedRankGlsPreparation.prepare(reader, plan, chunks).fold(error => fail(error.message), identity)
    reader.mutate = true
    val result = ChunkedFitExecutor.fitChunk(reader, chunks.indexed.head, context)
    assert(result.left.toOption.exists(_.isInstanceOf[FitError.PreparationReplayMismatch]))
  }

  private def dense(result: Either[FitError, FmriFitResult]): DenseFmriFitResult =
    result.fold(error => fail(error.message), _.asInstanceOf[DenseFmriFitResult])

  private def assertClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.coefficientAxis, expected.coefficientAxis)
    assertEquals(actual.inference.scope, expected.inference.scope)
    assertEquals(actual.inference.method, expected.inference.method)
    assertEquals(actual.coefficientCovariance.scope, expected.coefficientCovariance.scope)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.fitExclusions, expected.fitExclusions)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    matrixClose(actual.coefficients.value, expected.coefficients.value, 1e-10)
    matrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-9)
    actual.voxelIndices.indices.foreach { voxel =>
      assertEqualsDouble(actual.residualVariance(voxel), expected.residualVariance(voxel), 1e-10)
      assertEqualsDouble(actual.inference.varianceScale(voxel), expected.inference.varianceScale(voxel), 1e-10)
      matrixClose(actual.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel),
        expected.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel), 1e-10)
    }
    val t = TContrast("task", Map("task_a" -> 1.0))
    t.evaluate(actual).toOption.get.statistics.toVector.zip(t.evaluate(expected).toOption.get.statistics.toVector)
      .foreach((a, e) => assertEqualsDouble(a, e, 1e-7))
    val f = FContrast("tasks", Vector(Map("task_a" -> 1.0), Map("task_b" -> 1.0)))
    f.evaluate(actual).toOption.get.statistics.toVector.zip(f.evaluate(expected).toOption.get.statistics.toVector)
      .foreach((a, e) => assertEqualsDouble(a, e, 1e-6))
    if actual.columnNames.contains("base_constant") then
      assert(TContrast("nuisance", Map("base_constant" -> 1.0)).evaluate(actual).isLeft)

  private def matrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEqualsDouble(actual(row, col), expected(row, col), tolerance)

  private def exactMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEquals(java.lang.Double.doubleToLongBits(actual(row, col)), java.lang.Double.doubleToLongBits(expected(row, col)), clues(row, col))

  private class BoundedReader(plan: FitPlan, limit: Int, selectedTimes: Vector[Int] = Vector.empty) extends DatasetSeriesReader:
    private val underlying = SynchronousFmriDataset.readerFor(plan.model.dataset).toOption.get
    private val times = if selectedTimes.isEmpty then (0 until plan.model.dataset.shape.timepoints).toVector else selectedTimes
    var requests = Vector.empty[Vector[Int]]
    var mutate = false
    def dataset: FmriDataset = underlying.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
        if resolved.voxels.length > limit then Left(DatasetError.StorageFailure("unbounded spatial read"))
        else if resolved.timepoints != times then Left(DatasetError.StorageFailure("split temporal series"))
        else
          synchronized { requests = requests :+ resolved.voxels }
          underlying.seriesEither(selection).flatMap { series =>
            if !mutate then Right(series)
            else FmriSeries.fromIntIndices(Matrix.tabulate(series.data.rows, series.data.cols)((row, col) => series.data(row, col) + 0.01),
              series.voxelIndices, series.timepoints, series.shape)
          }
      }

  private def makePlan(model: FmriModel, components: ReducedRankComponentSpec,
      ar: AutocorrelationConfig = fixed, inference: ReducedRankInferencePolicy = ReducedRankInferencePolicy.Conditional,
      exclude: Boolean = false): FitPlan =
    FitPlan(model, FitStrategy.ReducedRankGls(ReducedRankGlsConfig.unsafe(components, ar, inference),
      FitControls(missingData = if exclude then MissingDataPolicy.ExcludeVoxel else MissingDataPolicy.Error)))

  private def model(id: String, sampling: SamplingFrame, events: Vector[Vector[Double]], response: Vector[Vector[Double]],
      intercept: Intercept = Intercept.Global): FmriModel =
    val event = EventModel(Vector.empty, sampling, Mat.fromRows(events), Vector("task_a", "task_b"),
      Vector(0 -> 2), Map("task" -> Vector(0, 1)))
    val baseline = BaselineModel.build(sampling, intercept = intercept)
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId(id), GaleTestMatrix.fromRows(response),
      SampleSpaces(Vector(response.head.length, 1, 1))), samplingFrame = sampling)
    FmriModel(event, baseline, dataset)

  private def fixtureModel: FmriModel = model("bounded-rrg-fixture", SamplingFrame(Seq(6), Seq(1.0)),
    Fixture.partitionedDesign.toRows.map(_.take(2)), Fixture.partitionedResponse.toRows)

  private def populationModel(exclude: Boolean = false, allExcluded: Boolean = false): FmriModel =
    val task = Vector.tabulate(24)(row => Vector(math.sin(row * 0.43), math.cos(row * 0.71)))
    val response = Vector.tabulate(24)(row => Vector.tabulate(19)(voxel =>
      if (exclude && voxel == 1 || allExcluded) && row == 8 then Double.NaN
      else 1.0 + voxel + (voxel + 1) * task(row)(0) + (voxel % 3 - 1) * task(row)(1) + 0.2 * math.sin(row * 1.37 + voxel)))
    model("bounded-rrg-population", SamplingFrame(Seq(11, 13), Seq(1.0, 1.0)), task, response)

  private def spectrumModel(kind: String): FmriModel =
    val events = Vector.tabulate(8)(row => Vector(if row == 0 then 1.0 else 0.0, if row == 1 then 1.0 else 0.0))
    val response = Vector.tabulate(8)(row => Vector.tabulate(19)(voxel =>
      if row == 2 then 0.2 + voxel / 7.0
      else if row >= 3 then math.sin(row + voxel) * 0.03
      else kind match
        case "zero" => 0.0
        case "tiny" => if row == 0 && voxel == 0 then 1.0 else if row == 1 && voxel == 1 then 1e-14 else 0.0
        case "rank-deficient" => if voxel == 0 then row + 1.0 else 0.0
        case "tied" => if row == voxel then 1.0 else 0.0
        case _ => fail("unknown spectrum")))
    model(s"bounded-rrg-$kind", SamplingFrame(Seq(8), Seq(1.0)), events, response, Intercept.None)
