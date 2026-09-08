package scalafim.fmri.fit

import gale.backend.{Backend, BackendConfig, BackendThresholds, Capability, DenseDoubleKernel, PureDenseDoubleKernel}
import gale.platform.DoubleArray

import scalafim.dataset.{DataSelection, DatasetBackend, DatasetError, DatasetEvents, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.{ColumnId, EmptyCellPolicy, FactorLevelRegistry}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture as R
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}

class FirstLevelFixedEffectsEstimatesSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private final class Fixture(intercept: Intercept = Intercept.Global, excluded: Boolean = false):
    var reads = 0
    private val values = R.response.map(row => if excluded then Vector(0.0) ++ row else row)
    private val source = InMemoryDatasetBackend(DatasetId("compiled-fixed-effects"),
      ImageDMat.fromRows(values), NeuroSpace(Vector(values.head.size, 1, 1)))
    private val backend = new DatasetBackend:
      export source.{id, shape, mask, metadata}
      def readEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads += 1
        source.readEither(selection)
    val dataset = FmriDataset.unsafe(backend,
      SamplingFrame(blockLens = Seq(8, 12), tr = Seq(2.0, 0.8), startTime = Seq(0.0, 0.0), precision = 0.1),
      DatasetEvents(Vector(
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2"))))
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = intercept,
      factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    val fitPlan = FitPlan(model, FitStrategy.SeparateRunsThenFixedEffects())
    val reader = new DatasetSeriesReader:
      val dataset: FmriDataset = Fixture.this.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)
    def request(uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None,
        retained: Vector[ColumnId] = Vector.empty): FirstLevelEstimateRequest =
      val axis = model.designSchema.get.coefficientAxis
      checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(
        StructuralTContrast.fromIds(ContrastId.unsafe("difference"), "A minus B", Map(axis.columnIds(0) -> 1.0, axis.columnIds(1) -> -1.0)))), uncertainty, retained))

  private def execute(plan: FirstLevelFixedEffectsEstimatePlan, fixture: Fixture)(using Backend): Vector[FirstLevelFixedEffectsEstimateBlock] =
    var blocks = Vector.empty[FirstLevelFixedEffectsEstimateBlock]
    val outcome = checked(plan.foreachBlock(fixture.reader, block => { blocks :+= block; Right(()) }))
    assertEquals(outcome, EstimateExecutionOutcome.Completed(blocks.length, blocks.map(_.inputVoxelIndices.length).sum))
    blocks

  private def selected(block: FirstLevelFixedEffectsEstimateBlock): FixedEffectsEstimateResult = block.result match
    case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
      assert(exclusions.isEmpty)
      result
    case other => fail(s"Expected selected results, got $other")

  test("preparation reads no responses and blocked pooling matches independent mixed-TR R contrasts") {
    val f = new Fixture()
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(EstimateUncertaintyRequest.Marginal), ChunkSize.unsafe(1)))
    assertEquals(f.reads, 0)
    assertEquals(plan.runPreparations.map(_.residualDegreesOfFreedom.value), R.runResidualDf)
    assertEquals(plan.runPreparations.map(_.partition.timepoints), Vector((0 until 8).toVector, (8 until 20).toVector))
    assert(plan.computesResidualVariance)
    val blocks = execute(plan, f)
    assertEquals(f.reads, 2)
    for voxel <- 0 until 2 do
      val result = selected(blocks(voxel))
      assertEquals(result.estimates.rows, 1)
      assertEquals(result.voxelIndices, Vector(voxel))
      assertEqualsDouble(result.estimates(0, 0), R.contrastEstimate(voxel), 1e-7)
      result.uncertainty match
        case FixedEffectsEstimateUncertainty.Marginal(se) => assertEqualsDouble(se(0, 0), R.contrastStandardError(voxel), 1e-7)
        case other => fail(s"Unexpected uncertainty: $other")
  }

  test("run-local nuisance projection preserves joint task pooling and selected voxel order") {
    val f = new Fixture(Intercept.Runwise)
    val selection = DataSelection(voxels = IndexSelection.Indices(Vector(1, 0)))
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(2), selection))
    assertEquals(plan.pooledCoefficientAxis.predictors, 2)
    assert(plan.runPreparations.forall(_.diagnostics.predictors == 3))
    val result = selected(execute(plan, f).head)
    assertEquals(result.voxelIndices, Vector(1, 0))
    assertEquals(result.uncertainty, FixedEffectsEstimateUncertainty.NotRequested)
    // Independent 2x2 inverse calculation using only the R fixture's task
    // covariance blocks, run coefficients and run variances (run-local offsets
    // do not participate in the pooled coefficient vector).
    assertEqualsDouble(result.estimates(0, 0), -1.4023927727840713, 1e-7)
    assertEqualsDouble(result.estimates(0, 1), 2.0939242387249237, 1e-7)
    val narrow = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1), selection))
    val blocks = execute(narrow, f)
    for v <- 0 until 2 do assertEqualsDouble(selected(blocks(v)).estimates(0, 0), result.estimates(0, v), 1e-12)
    val nuisance = checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Coefficient(f.model.designSchema.get.columns.last.id))))
    assert(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, nuisance, ChunkSize.unsafe(1)).isLeft)
  }

  test("entirely excluded blocks preserve identities and do not stop subsequent healthy voxels") {
    val f = new Fixture(excluded = true)
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1)))
    val blocks = execute(plan, f)
    assertEquals(blocks.head.result, FixedEffectsEstimateBlockResult.Excluded(Vector(VoxelInferenceExclusion(0, VoxelFitStatus.AllZero))))
    assertEquals(selected(blocks(1)).voxelIndices, Vector(1))
    assertEquals(selected(blocks(2)).voxelIndices, Vector(2))
    assertEqualsDouble(selected(blocks(2)).estimates(0, 0), R.contrastEstimate(1), 1e-7)
    val wide = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(3)))
    execute(wide, f).head.result match
      case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
        assertEquals(result.voxelIndices, Vector(1, 2))
        assertEquals(exclusions, Vector(VoxelInferenceExclusion(0, VoxelFitStatus.AllZero)))
      case other => fail(s"Unexpected result: $other")
  }

  test("cancellation, sink failure and incompatible engines fail without excess reads") {
    val f = new Fixture()
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1)))
    assertEquals(checked(plan.foreachBlock(f.reader, _ => fail("cancelled"), () => true)), EstimateExecutionOutcome.Cancelled(0, 0))
    assertEquals(f.reads, 0)
    val error = FitError.InvalidFitAxis("test sink", "refused")
    assertEquals(plan.foreachBlock(f.reader, _ => Left(error)), Left(error))
    assertEquals(f.reads, 1)
    assert(FirstLevelFixedEffectsEstimates.prepare(FitPlan(f.model), f.request(), ChunkSize.unsafe(1)).isLeft)
    assertEquals(f.reads, 1)
  }

  test("pooled execution dispatches only prepared run statistics and requested voxel products") {
    var products = Vector.empty[(Int, Int, Int)]
    val kernels = new DenseDoubleKernel:
      export PureDenseDoubleKernel.{dot, nrm2, copy, axpy, scal, gemv, syrk}
      def gemm(
          rows: Int, cols: Int, shared: Int, alpha: Double,
          a: DoubleArray, aOffset: Int, aRowStride: Int, aColStride: Int,
          b: DoubleArray, bOffset: Int, bRowStride: Int, bColStride: Int,
          beta: Double, c: DoubleArray, cOffset: Int, cRowStride: Int, cColStride: Int
      ): Unit =
        assertEquals(aColStride, 1, "prepared readout must admit the row-major vector kernel")
        products :+= ((rows, shared, cols))
        PureDenseDoubleKernel.gemm(rows, cols, shared, alpha, a, aOffset, aRowStride, aColStride,
          b, bOffset, bRowStride, bColStride, beta, c, cOffset, cRowStride, cColStride)
    given Backend = new Backend:
      val name = "observed-portable-kernel"
      val capabilities = Set(Capability.Vectorized)
      val config = BackendConfig.singleThreaded
      val thresholds = new BackendThresholds:
        def nativeGemmMinFlops: Long = 0L
        def nativeGemvMinWork: Long = 0L
        def nativeFactorizationMinSize: Int = Int.MaxValue
      val denseDouble = kernels
    val f = new Fixture(Intercept.Runwise)
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(2)))
    products = Vector.empty
    execute(plan, f)
    assertEquals(products, Vector((2, 3, 2), (2, 3, 2), (1, 2, 1), (1, 2, 1)))
    val ids = f.model.designSchema.get.coefficientAxis.columnIds.takeRight(2)
    val withRunRows = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan,
      f.request(retained = ids), ChunkSize.unsafe(2)))
    products = Vector.empty
    val blocks = execute(withRunRows, f)
    assertEquals(retained(blocks.head).map(_.estimates.rows), Vector(1, 1))
    assertEquals(products, Vector((1, 3, 2), (2, 3, 2), (1, 3, 2), (2, 3, 2), (1, 2, 1), (1, 2, 1)))
    assertEquals(f.reads, 2)
  }

  test("FIR pooling retains physical bins and planted estimates through reversed gapped scan selection") {
    import scalafim.fmri.design.StructuralColumnOrigin
    val frame = SamplingFrame(blockLens = Seq(16, 16), tr = Seq(1.0, 1.0))
    val events = DatasetEvents(Vector(
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2")))
    def dataset(values: Vector[Vector[Double]]) = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("selected-pooled-fir"), ImageDMat.fromRows(values), NeuroSpace(Vector(2, 1, 1))), frame, events)
    val model = FmriModelBuilder.buildModel(dataset(Vector.fill(32)(Vector(0.0, 0.0))),
      ModelBuildSpec("onset ~ hrf(cond)", blockColumn = Some("run"), baselineIntercept = Intercept.Runwise,
        defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s), precision = 0.25.s))
    val schema = model.designSchema.get
    val task = schema.columns.filter(_.origin.isInstanceOf[StructuralColumnOrigin.Event])
    assertEquals(task.size, 2)
    val positions = task.map(column => schema.coefficientAxis.columnIds.indexOf(column.id))
    // Noise is explicitly orthogonal to every design column in both runs:
    // opposite signs at two rows outside the FIR support cancel the intercept.
    for row <- Vector(6, 7, 22, 23); col <- positions do assertEqualsDouble(model.designMatrix(row, col), 0.0, 0.0)
    val beta = Vector.tabulate(model.nPredictors)(col => if col == positions.head then 2.0 else if col == positions.last then 5.0 else 100.0 + col)
    val values = Vector.tabulate(32, 2) { (row, voxel) =>
      val signal = beta.indices.map(col => model.designMatrix(row, col) * beta(col) * (voxel + 1)).sum
      val noise = (if row % 16 == 6 then 0.1 else if row % 16 == 7 then -0.1 else 0.0) * (row / 16 + 1) * (voxel + 1)
      signal + noise
    }
    val populated = model.copy(dataset = dataset(values))
    val retainedIds = task.reverse.map(_.id) ++ schema.columns.filterNot(task.contains).map(_.id)
    val request = checked(FirstLevelEstimateRequest.make(task.reverse.map(column => EstimateOutput.Coefficient(column.id)),
      retainRunCoefficients = retainedIds))
    val scans = (0 until 32).filterNot(i => i % 16 == 5 || i % 16 == 10).reverse.toVector
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(FitPlan(populated, FitStrategy.SeparateRunsThenFixedEffects()), request,
      ChunkSize.unsafe(2), DataSelection(time = IndexSelection.Indices(scans))))
    assertEquals(plan.timepoints, scans)
    assertEquals(plan.runPreparations.map(_.residualDegreesOfFreedom.value), Vector(11, 11))
    val roles = plan.selection.outputs.map {
      case EstimateOutputMetadata.Coefficient(column) => column.origin match
        case StructuralColumnOrigin.Event(_, _, _, _, Some(ref), _, _) => ref.role.get
        case other => fail(s"Unexpected origin: $other")
      case other => fail(s"Unexpected metadata: $other")
    }
    assertEquals(roles, Vector(BasisRole.FirBin(2, 2.s, 4.s), BasisRole.FirBin(1, 0.s, 2.s)))
    val reader = new DatasetSeriesReader:
      val dataset = populated.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)
    checked(plan.foreachBlock(reader, block => {
      val result = selected(block)
      for voxel <- 0 until 2 do
        assertEqualsDouble(result.estimates(0, voxel), 5.0 * (voxel + 1), 1e-10)
        assertEqualsDouble(result.estimates(1, voxel), 2.0 * (voxel + 1), 1e-10)
      val runRows = retained(block)
      assertEquals(runRows.map(_.description.partition.runIndex), Vector(0, 1))
      runRows.foreach { value =>
        val axis = value.description.coefficientAxis
        assertEquals(axis.predictors, 3)
        val runRoles = axis.columns.take(2).map(_.origin match
          case StructuralColumnOrigin.Event(_, _, _, _, Some(ref), _, _) => ref.role.get
          case other => fail(s"Lost run-local FIR coordinate: $other"))
        assertEquals(runRoles, roles)
        for row <- 0 until 3 do
          val source = schema.coefficientAxis.columnIds.indexOf(value.description.sourceColumnIds(row))
          assertEquals(axis.columns(row).hrfScale, schema.columns(source).hrfScale)
          for voxel <- 0 until 2 do
            assertEqualsDouble(value.estimates(row, voxel), beta(source) * (voxel + 1), 1e-10)
      }
      Right(())
    }))
  }


  private def retained(block: FirstLevelFixedEffectsEstimateBlock): Vector[RetainedRunCoefficientBlock] =
    block.runCoefficients match
      case RunCoefficientRetentionResult.Retained(values) => values
      case other => fail(s"Expected explicitly retained run coefficients, got $other")

  private def sameUncertainty(a: FixedEffectsEstimateUncertainty, b: FixedEffectsEstimateUncertainty): Unit =
    def matrix(x: gale.linalg.DMat, y: gale.linalg.DMat): Unit =
      assertEquals((x.rows, x.cols), (y.rows, y.cols))
      for row <- 0 until x.rows; col <- 0 until x.cols do assertEqualsDouble(x(row, col), y(row, col), 0.0)
    (a, b) match
      case (FixedEffectsEstimateUncertainty.NotRequested, FixedEffectsEstimateUncertainty.NotRequested) => ()
      case (FixedEffectsEstimateUncertainty.Marginal(x), FixedEffectsEstimateUncertainty.Marginal(y)) =>
        matrix(x.value, y.value)
      case (FixedEffectsEstimateUncertainty.Joint(x, xc), FixedEffectsEstimateUncertainty.Joint(y, yc)) =>
        matrix(x.value, y.value)
        assertEquals(xc.scope, yc.scope)
        for voxel <- 0 until x.voxels do matrix(checked(xc.matrixForVoxelPosition(voxel)), checked(yc.matrixForVoxelPosition(voxel)))
      case other => fail(s"Primary uncertainty changed with retention: $other")

  test("explicit retained run coefficients match independent R values without changing pooled products") {
    for mode <- Vector(EstimateUncertaintyRequest.None, EstimateUncertaintyRequest.Marginal, EstimateUncertaintyRequest.Joint) do
      val f = new Fixture(Intercept.Runwise)
      val source = f.model.designSchema.get.coefficientAxis.columnIds
      val requested = Vector(source.last, source.head, source(2))
      val voxelOrder = Vector(1, 0)
      val selection = DataSelection(voxels = IndexSelection.Indices(voxelOrder))
      val plain = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(mode), ChunkSize.unsafe(2), selection))
      val kept = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(mode, requested), ChunkSize.unsafe(2), selection))
      assertEquals(f.reads, 0)
      assertEquals(plain.retainedRunCoefficients, Vector.empty)
      assertEquals(kept.retainedRunCoefficients.map(_.sourceColumnIds), Vector(Vector(source.head, source(2)), Vector(source.last, source.head)))
      val first = execute(plain, f).head
      val second = execute(kept, f).head
      assertEquals(f.reads, 2)
      assertEquals(first.runCoefficients, RunCoefficientRetentionResult.NotRequested)
      val a = selected(first)
      val b = selected(second)
      assertEquals(a.voxelIndices, b.voxelIndices)
      assertEquals(a.pooledCoefficientAxis, b.pooledCoefficientAxis)
      for voxel <- 0 until 2 do assertEqualsDouble(a.estimates(0, voxel), b.estimates(0, voxel), 0.0)
      sameUncertainty(a.uncertainty, b.uncertainty)
      val runs = retained(second)
      assertEquals(runs.map(_.description.partition.runIndex), Vector(0, 1))
      for run <- runs.indices do
        val value = runs(run)
        assertEquals(value.voxelIndices, voxelOrder)
        assertEquals(value.statuses.length, 2)
        assertEquals(value.uncertainty, OlsEstimateUncertainty.NotRequested)
        assertEquals(value.description.uncertainty, EstimateUncertaintyRequest.None)
        assertEquals(value.estimates.rows, 2)
        for row <- 0 until 2; voxel <- 0 until 2 do
          val original = value.description.sourceColumnIds(row)
          val referenceRow = if original == source.head then 0 else 2
          assertEqualsDouble(value.estimates(row, voxel), R.runCoefficients(run)(referenceRow)(voxelOrder(voxel)), 1e-7)
      val described = checked(SelectedEstimates.prepare(f.fitPlan, f.request(mode, requested), ChunkSize.unsafe(2)))
      assertEquals(described.description.retainedRunCoefficients.size, 2)
      assertEquals(described.description.products(EstimateProduct.RunCoefficients), EstimateProductDisposition.Retained)
      assert(described.description.computations.contains(EstimateComputation.RunCoefficientReadout))
  }

  test("retained run coefficients preserve excluded input voxels and continue to healthy blocks") {
    val f = new Fixture(Intercept.Runwise, excluded = true)
    val ids = f.model.designSchema.get.coefficientAxis.columnIds.takeRight(2)
    val off = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1)))
    val on = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(retained = ids), ChunkSize.unsafe(1)))
    val plain = execute(off, f)
    val kept = execute(on, f)
    assertEquals(kept.head.result, plain.head.result)
    assertEquals(retained(kept.head).map(_.voxelIndices), Vector(Vector(0), Vector(0)))
    retained(kept.head).foreach { block =>
      assertEquals(block.statuses, Vector(VoxelFitStatus.AllZero))
      assertEqualsDouble(block.estimates(0, 0), 0.0, 0.0)
    }
    for voxel <- Vector(1, 2) do
      assertEqualsDouble(selected(kept(voxel)).estimates(0, 0), selected(plain(voxel)).estimates(0, 0), 0.0)
      for run <- 0 until 2 do
        assertEqualsDouble(retained(kept(voxel))(run).estimates(0, 0), R.runCoefficients(run)(2)(voxel - 1), 1e-7)
  }

  test("unsupported run retention cannot be silently discarded by shared or pooling-only entry points") {
    val f = new Fixture(Intercept.Runwise)
    val ids = f.model.designSchema.get.coefficientAxis.columnIds
    val request = f.request(retained = Vector(ids.last))
    assert(FirstLevelEstimateRequest.make(request.outputs, retainRunCoefficients = Vector(ids.last, ids.last)).isLeft)
    val unknown = f.request(retained = Vector(ColumnId.unsafe("unknown-retained-column")))
    assert(SelectedEstimates.prepare(f.fitPlan, unknown, ChunkSize.unsafe(1)).isLeft)
    assert(SelectedEstimates.prepare(FitPlan(f.model), request, ChunkSize.unsafe(1)).isLeft)
    assert(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, request, ChunkSize.unsafe(1),
      DataSelection(time = IndexSelection.Indices((0 until 8).toVector))).isLeft)
    assertEquals(f.reads, 0)
    val full = checked(FitPlanExecutor.fitChunked(f.reader, FitPlan(f.model, FitStrategy.RunwiseLeastSquares()),
      DataSelection.All, FitChunkingStrategy.ByVoxelCount(ChunkSize.unsafe(2)))) match
      case value: RunwiseFmriFitResult => value
      case other => fail(s"Expected runwise source, got $other")
    val compiled = checked(request.compile(f.model.designSchema.get))
    assert(FixedEffectsEstimates.combine(full, compiled).isLeft)
    val statistics = checked(FixedEffects.prepareStatistics(full, FixedEffects.DefaultPolicy))
    assert(FixedEffectsEstimates.estimate(statistics.statistics, compiled).isLeft)
  }

  test("retention obeys the same cancellation and failed-sink read boundary") {
    val f = new Fixture(Intercept.Runwise)
    val ids = f.model.designSchema.get.coefficientAxis.columnIds.takeRight(2)
    val plan = checked(FirstLevelFixedEffectsEstimates.prepare(f.fitPlan, f.request(retained = ids), ChunkSize.unsafe(1)))
    var stop = false
    val outcome = checked(plan.foreachBlock(f.reader, block => {
      assertEquals(retained(block).size, 2)
      stop = true
      Right(())
    }, () => stop))
    assertEquals(outcome, EstimateExecutionOutcome.Cancelled(1, 1))
    assertEquals(f.reads, 1)
    val refused = FitError.InvalidFitAxis("retention sink", "refused")
    assertEquals(plan.foreachBlock(f.reader, _ => Left(refused)), Left(refused))
    assertEquals(f.reads, 2)
  }
