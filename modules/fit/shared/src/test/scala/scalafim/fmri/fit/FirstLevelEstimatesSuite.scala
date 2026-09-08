package scalafim.fmri.fit

import gale.backend.{Backend, BackendConfig, BackendThresholds, Capability, DenseDoubleKernel, PureDenseDoubleKernel}
import scalafim.dataset.{DataSelection, DatasetBackend, DatasetError, DatasetEvents, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.{ColumnId, StructuralColumnOrigin}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{FitEngine, FitPlan, FmriModel, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}

class FirstLevelEstimatesSuite extends munit.FunSuite:
  private def right[A](value: Either[FitError, A]): A = value.fold(error => fail(error.message), identity)

  private def model: FmriModel = modelWithReadObserver(() => ())

  private def modelWithReadObserver(onRead: () => Unit): FmriModel =
    val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
    val source = InMemoryDatasetBackend(DatasetId("selected-estimates"), ImageDMat.fromRows(Vector(
        Vector(1.0, 2.0), Vector(3.0, 1.0), Vector(5.0, 0.0), Vector(7.0, -1.0)
      )), NeuroSpace(Vector(2, 1, 1)))
    val backend = new DatasetBackend:
      export source.{id, shape, mask, metadata}
      def readEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        onRead()
        source.readEither(selection)
    val dataset = FmriDataset.unsafe(backend, frame)
    val event = EventModel(
      terms = Vector.empty, samplingFrame = frame,
      designMatrix = Mat.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0), Vector(3.0))),
      columnNames = Vector("task"), termSpans = Vector(0 -> 1), colIndices = Map("task" -> Vector(0))
    )
    FmriModel(event, BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)

  private class RecordingReader(val dataset: FmriDataset) extends DatasetSeriesReader:
    var selections = Vector.empty[DataSelection]
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selections :+= selection
      dataset.seriesEither(selection)

  private def request(plan: FitPlan): FirstLevelEstimateRequest =
    val ids = plan.coefficientAxis.get.columnIds
    right(FirstLevelEstimateRequest.make(Vector(
      EstimateOutput.Coefficient(ids(0)),
      EstimateOutput.Contrast(StructuralTContrast.fromIds(ContrastId.unsafe("intercept-minus-task"),
        "intercept minus task coefficient", Map(ids(1) -> 1.0, ids(0) -> -1.0)))
    )))

  test("public preparation performs no reads and streams only selected identified products") {
    var sourceReads = 0
    val fitPlan = FitPlan(modelWithReadObserver(() => sourceReads += 1))
    val reader = new RecordingReader(fitPlan.model.dataset)
    val requested = request(fitPlan)
    val prepared = right(FirstLevelEstimates.prepare(fitPlan, requested, ChunkSize.unsafe(1)))
    assertEquals(reader.selections, Vector.empty)
    assertEquals(sourceReads, 0)
    assertEquals(prepared.outputs.map(_.id), requested.outputs.map(_.id))
    assertEquals(prepared.maxBlockVoxels, 1)
    assert(!prepared.computesResidualVariance)
    var blocks = Vector.empty[FirstLevelEstimateBlock]
    val outcome = right(prepared.foreachBlock(reader, block => { blocks :+= block; Right(()) }))
    assertEquals(outcome, EstimateExecutionOutcome.Completed(2, 2))
    assertEquals(reader.selections.size, 2)
    assertEquals(sourceReads, 2)
    assertEquals(blocks.map(_.voxelIndices), Vector(Vector(0), Vector(1)))
    // Independent closed-form slopes/intercepts for y1=1+2t and y2=2-t.
    assertEqualsDouble(blocks(0).result.estimates(0, 0), 2.0, 1e-12)
    assertEqualsDouble(blocks(1).result.estimates(0, 0), -1.0, 1e-12)
    assertEqualsDouble(blocks(0).result.estimates(1, 0), -1.0, 1e-12)
    assertEqualsDouble(blocks(1).result.estimates(1, 0), 3.0, 1e-12)
    assert(blocks.forall(_.result.uncertainty == OlsEstimateUncertainty.NotRequested))
  }

  test("selected time and voxel ordering are preserved across block sizes") {
    val fitPlan = FitPlan(model)
    val selected = DataSelection(time = IndexSelection.Indices(Vector(0, 2, 3)), voxels = IndexSelection.Indices(Vector(1, 0)))
    val narrow = right(FirstLevelEstimates.prepare(fitPlan, request(fitPlan), ChunkSize.unsafe(1), selected))
    val wide = right(FirstLevelEstimates.prepare(fitPlan, request(fitPlan), ChunkSize.unsafe(2), selected))
    assertEquals(narrow.timepoints, Vector(0, 2, 3))
    var narrowBlocks = Vector.empty[FirstLevelEstimateBlock]
    var wideBlocks = Vector.empty[FirstLevelEstimateBlock]
    right(narrow.foreachBlock(new RecordingReader(fitPlan.model.dataset), b => { narrowBlocks :+= b; Right(()) }))
    right(wide.foreachBlock(new RecordingReader(fitPlan.model.dataset), b => { wideBlocks :+= b; Right(()) }))
    assertEquals(narrowBlocks.flatMap(_.voxelIndices), wideBlocks.flatMap(_.voxelIndices))
    for voxel <- 0 until 2; output <- 0 until 2 do
      assertEqualsDouble(narrowBlocks(voxel).result.estimates(output, 0), wideBlocks.head.result.estimates(output, voxel), 1e-12)
  }

  test("cancellation and sink failure prevent subsequent response reads") {
    val fitPlan = FitPlan(model)
    val prepared = right(FirstLevelEstimates.prepare(fitPlan, request(fitPlan), ChunkSize.unsafe(1)))
    val cancelledReader = new RecordingReader(fitPlan.model.dataset)
    assertEquals(right(prepared.foreachBlock(cancelledReader, _ => fail("cancelled sink must not run"), () => true)),
      EstimateExecutionOutcome.Cancelled(0, 0))
    assertEquals(cancelledReader.selections.size, 0)
    val reader = new RecordingReader(fitPlan.model.dataset)
    var stop = false
    assertEquals(right(prepared.foreachBlock(reader, _ => { stop = true; Right(()) }, () => stop)),
      EstimateExecutionOutcome.Cancelled(1, 1))
    assertEquals(reader.selections.size, 1)
    val failingReader = new RecordingReader(fitPlan.model.dataset)
    val failure = FitError.InvalidFitAxis("sink", "write failed")
    assertEquals(prepared.foreachBlock(failingReader, _ => Left(failure)), Left(failure))
    assertEquals(failingReader.selections.size, 1)
  }

  test("invalid identities and unsupported scientific engines fail before reads") {
    val fitPlan = FitPlan(model)
    assert(FirstLevelEstimateRequest.make(Vector.empty).isLeft)
    val duplicate = request(fitPlan).outputs.head
    assert(FirstLevelEstimateRequest.make(Vector(duplicate, duplicate)).isLeft)
    val unknown = right(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Coefficient(ColumnId.unsafe("unknown")))))
    assert(FirstLevelEstimates.prepare(fitPlan, unknown, ChunkSize.unsafe(1)).isLeft)
    val runwise = FitPlan(fitPlan.model, FitEngine.RunwiseLeastSquares)
    assert(FirstLevelEstimates.prepare(runwise, request(fitPlan), ChunkSize.unsafe(1)).isLeft)
    val prepared = right(FirstLevelEstimates.prepare(fitPlan, request(fitPlan), ChunkSize.unsafe(1)))
    val dishonest = new RecordingReader(fitPlan.model.dataset):
      override def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        super.seriesEither(DataSelection.All)
    assert(prepared.foreachBlock(dishonest, _ => fail("mismatched block must not reach the sink")).isLeft)
  }

  test("the caller's backend receives exactly the selected block products") {
    var calls = 0
    given Backend = new Backend:
      val name = "counted-portable-kernel"
      val capabilities = Set(Capability.Vectorized)
      val config = BackendConfig.singleThreaded
      val thresholds = new BackendThresholds:
        def nativeGemmMinFlops: Long = 0L
        def nativeGemvMinWork: Long = 0L
        def nativeFactorizationMinSize: Int = Int.MaxValue
      def denseDouble: DenseDoubleKernel =
        calls += 1
        PureDenseDoubleKernel
    val fitPlan = FitPlan(model)
    val prepared = right(FirstLevelEstimates.prepare(fitPlan, request(fitPlan), ChunkSize.unsafe(1)))
    calls = 0
    right(prepared.foreachBlock(new RecordingReader(fitPlan.model.dataset), _ => Right(())))
    assertEquals(calls, 2)
  }

  test("native FIR selection preserves physical bin coordinates and requested order") {
    val frame = SamplingFrame(blockLens = Seq(16), tr = Seq(1.0))
    val events = DatasetEvents(Vector(Map("onset" -> "0.0", "cond" -> "A"), Map("onset" -> "8.0", "cond" -> "A")))
    def dataset(values: Vector[Vector[Double]]) = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("selected-fir"), ImageDMat.fromRows(values), NeuroSpace(Vector(1, 1, 1))),
      frame, events)
    val empty = dataset(Vector.fill(16)(Vector(0.0)))
    val built = FmriModelBuilder.buildModel(empty, ModelBuildSpec("onset ~ hrf(cond)", defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s), precision = 0.25.s))
    val axis = built.designSchema.get.coefficientAxis
    val fir = axis.columns.filter { column =>
      column.origin match
        case StructuralColumnOrigin.Event(_, _, _, _, Some(ref), _, _) => ref.role.exists(_.isInstanceOf[BasisRole.FirBin])
        case _ => false
    }
    assertEquals(fir.length, 2)
    val beta = Vector.tabulate(built.nPredictors)(i => if axis.columns(i).id == fir(0).id then 2.0 else if axis.columns(i).id == fir(1).id then 5.0 else 100.0)
    val values = Vector.tabulate(16)(row => Vector(beta.indices.map(col => built.designMatrix(row, col) * beta(col)).sum))
    val fittedModel = built.copy(dataset = dataset(values))
    val selection = right(FirstLevelEstimateRequest.make(fir.reverse.map(c => EstimateOutput.Coefficient(c.id))))
    val prepared = right(FirstLevelEstimates.prepare(FitPlan(fittedModel), selection, ChunkSize.unsafe(1)))
    val roles = prepared.outputs.map {
      case EstimateOutputMetadata.Coefficient(column) => column.origin match
        case StructuralColumnOrigin.Event(_, _, _, _, Some(ref), _, _) => ref.role.get
        case other => fail(s"FIR origin lost: $other")
      case other => fail(s"Expected FIR coefficient, got $other")
    }
    assertEquals(roles, Vector(BasisRole.FirBin(2, 2.s, 4.s), BasisRole.FirBin(1, 0.s, 2.s)))
    right(prepared.foreachBlock(new RecordingReader(fittedModel.dataset), block => {
      assertEqualsDouble(block.result.estimates(0, 0), 5.0, 1e-10)
      assertEqualsDouble(block.result.estimates(1, 0), 2.0, 1e-10)
      Right(())
    }))
  }
