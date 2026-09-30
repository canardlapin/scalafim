package scalafim.fmri.fit.estimates

import gale.linalg.Matrix
import scalafim.archive.ContentDigest
import scalafim.estimates.*
import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.fmri.fit.{ChunkSize, EstimateOutput, EstimateUncertaintyRequest, FirstLevelEstimateRequest, FirstLevelEstimates}
import scalafim.image.SampleSpaces

object ProducerFixture:
  private def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), value => value)
  val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
  val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId("producer-test"),
    Matrix.tabulate(4, 2)((t, v) => (if v == 0 then 1.0 + 2.0 * t else 5.0 - 3.0 * t) + (v + 1) * Vector(1.0, -1.0, -1.0, 1.0)(t)), SampleSpaces(Vector(2, 1, 1))), frame)
  private val event = EventModel(Vector.empty, frame, Mat.fromRows(Vector.tabulate(4)(t => Vector(t.toDouble))),
    Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
  private val model = FmriModel(event, BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)
  private val fit = FitPlan(model)
  val ids = Vector(EstimandId("task"), EstimandId("intercept"))
  val identity = EstimatePublicationIdentity(DatasetId("00000000-0000-4000-8000-000000000001"),
    UnitId("00000000-0000-4000-8000-000000000002"), UnitRevisionId("00000000-0000-4000-8000-000000000003"),
    "01", ObservationId("subject-01"), Vector(AcquisitionId("run-1")), "execution-1", "test", "signal", Vector.empty)
  val catalog = EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000004"), ids.map(id =>
    EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "unit", id.value)))
  def prepared(uncertainty: EstimateUncertaintyRequest, blockSize: Int = 1) =
    val request = checked(FirstLevelEstimateRequest.make(fit.coefficientAxis.get.columnIds.map(EstimateOutput.Coefficient.apply), uncertainty))
    checked(FirstLevelEstimates.prepare(fit, request, ChunkSize.unsafe(blockSize)))
  def producer = checked(FitEstimateProducer.shared(prepared(EstimateUncertaintyRequest.Marginal), identity, catalog, ids, "scanner"))
  def reader = new DatasetSeriesReader:
    val dataset = ProducerFixture.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)

class FitEstimateProducerSuite extends munit.FunSuite:
  private final class Sink(val unit: EstimateUnit) extends EstimateSink:
    val maximumBlockCells = 1
    var aborted = false
    var sealedResult = false
    var cells = Map.empty[(ProductKind, EstimandId, Int), Double]
    def write(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte]) =
      assertEquals(selection.cells, 1L)
      assertEquals(validity.toVector, Vector[Byte](0))
      cells += ((unit.products.find(_.id == product).get.kind, selection.estimands.head, selection.samples.head) -> values.head)
      Right(())
    def seal() =
      sealedResult = true
      Right(PinnedUnit(unit.unit, unit.revision, FileReference("test-only.json", ContentDigest.unsafeSha256("a" * 64), 1)))
    def abort() =
      aborted = true
      Right(())

  test("native selected OLS streams exact identified outputs with operator, scan and df metadata") {
    val producer = ProducerFixture.producer
    val sink = new Sink(producer.unit)
    assert(producer.write(ProducerFixture.reader, sink).isRight)
    assert(sink.sealedResult && !sink.aborted)
    assertEqualsDouble(sink.cells((ProductKind.Effect, ProducerFixture.ids.head, 0)), 2.0, 1e-12)
    assertEqualsDouble(sink.cells((ProductKind.Effect, ProducerFixture.ids.last, 1)), 5.0, 1e-12)
    assertEquals(producer.unit.bindings.head.weights, Vector(1.0, 0.0))
    assertEquals(producer.unit.provenance.scans.head.zeroBasedRows, Vector(0, 1, 2, 3))
    assertEquals(producer.unit.degreesOfFreedom.head.value, DfValue.Scalar(2.0))
    assertEquals(producer.unit.marginalUncertainty.head.origin,
      MarginalVarianceOrigin.Estimated(producer.unit.degreesOfFreedom.head))
    assert(producer.unit.estimability.isInstanceOf[EstimabilityEvidence.FullRank])
  }

  test("multi-run shared OLS identifies joint-run pooling without inventing effective df") {
    def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), result => result)
    val frame = SamplingFrame(blockLens = Seq(2, 2), tr = Seq(1.0, 1.0))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId("pooled-producer-test"),
      Matrix.tabulate(4, 2)((t, v) => (if v == 0 then 1.0 + 2.0 * t else 5.0 - 3.0 * t) +
        (v + 1) * Vector(1.0, -1.0, -1.0, 1.0)(t)), SampleSpaces(Vector(2, 1, 1))), frame)
    val event = EventModel(Vector.empty, frame, Mat.fromRows(Vector.tabulate(4)(t => Vector(t.toDouble))),
      Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
    val model = FmriModel(event,
      BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)
    val fit = FitPlan(model)
    val request = checked(FirstLevelEstimateRequest.make(
      fit.coefficientAxis.get.columnIds.map(EstimateOutput.Coefficient.apply), EstimateUncertaintyRequest.Marginal))
    val prepared = checked(FirstLevelEstimates.prepare(fit, request, ChunkSize.unsafe(1)))
    val identity = ProducerFixture.identity.copy(acquisitions = Vector(AcquisitionId("run-1"), AcquisitionId("run-2")))
    val producer = checked(FitEstimateProducer.shared(prepared, identity, ProducerFixture.catalog, ProducerFixture.ids, "scanner"))
    assert(producer.unit.products.forall(_.pooling == PoolingScope.JointRuns))
    assertEquals(producer.unit.provenance.scans.map(_.zeroBasedRows), Vector(Vector(0, 1), Vector(0, 1)))
    assertEquals(producer.unit.degreesOfFreedom.head,
      DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(2.0), "OLS n - numerical rank", false))
    assert(producer.unit.provenance.runCombination.isInstanceOf[ScientificFact.Known])
  }

  test("cancellation aborts the sink without publishing a partial result") {
    val producer = ProducerFixture.producer
    val sink = new Sink(producer.unit)
    assertEquals(producer.write(ProducerFixture.reader, sink, () => true), Left(EstimateError.Cancelled))
    assert(sink.aborted && !sink.sealedResult && sink.cells.isEmpty)
  }

  test("a scalar-only sink is refused before executing requested joint uncertainty") {
    val fixture = ProducerFixture
    val producer = FitEstimateProducer.shared(fixture.prepared(EstimateUncertaintyRequest.Joint), fixture.identity,
      fixture.catalog, fixture.ids, "scanner").toOption.get
    val sink = new Sink(producer.unit)
    assert(producer.write(fixture.reader, sink).isLeft)
    assert(sink.aborted && !sink.sealedResult && sink.cells.isEmpty)
  }

  private final class JointSink(val unit: EstimateUnit, val maximumBlockCells: Int, compact: Boolean) extends SharedCovarianceSink:
    override val supportsCovariance = !compact
    val sharedCovarianceProducts = if compact then unit.covariance.map(_.product).toSet else Set.empty[ProductId]
    var aborted = false
    var scalar = Map.empty[(ProductId, EstimandId, Int), Double]
    var repeated = Map.empty[(EstimandPair, Int), Double]
    var shared = Map.empty[EstimandPair, Double]
    var sharedWrites = 0
    var rejectShared = false
    var throwShared = false
    def write(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte]) =
      assert(selection.cells <= maximumBlockCells)
      selection.samples.zipWithIndex.foreach((sample, index) => scalar += ((product, selection.estimands.head, sample) -> values(index)))
      Right(())
    override def writeCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte]) =
      assert(!compact)
      selection.samples.zipWithIndex.foreach((sample, index) => repeated += ((selection.pairs.head, sample) -> values(index)))
      Right(())
    def writeSharedCovariance(product: ProductId, selection: SharedCovarianceSelection, values: Array[Double], validity: Array[Byte]) =
      assert(compact)
      assertEquals(selection.cells, 1L)
      assertEquals(validity.toVector, Vector(Validity.Valid.code))
      sharedWrites += 1
      if throwShared then throw new IllegalStateException("injected shared callback exception")
      if rejectShared then Left(EstimateError.Io("injected shared callback failure"))
      else
        assert(!shared.contains(selection.pairs.head))
        shared += selection.pairs.head -> values.head
        Right(())
    def seal() = Right(PinnedUnit(unit.unit, unit.revision, FileReference("test-only.json", ContentDigest.unsafeSha256("a" * 64), 1)))
    def abort() =
      aborted = true
      Right(())

  test("actual prepared OLS delivers U once per named pair independent of voxel block size") {
    val fixture = ProducerFixture
    var baseline = Map.empty[(ProductId, EstimandId, Int), Double]
    for blockSize <- Vector(1, 2) do
      val producer = FitEstimateProducer.shared(fixture.prepared(EstimateUncertaintyRequest.Joint, blockSize),
        fixture.identity, fixture.catalog, fixture.ids, "scanner").toOption.get
      val core1 = new JointSink(producer.unit, blockSize, false)
      val compact = new JointSink(producer.unit, blockSize, true)
      assert(producer.write(fixture.reader, core1).isRight)
      assert(producer.write(fixture.reader, compact).isRight)
      assertEquals(compact.sharedWrites, 3)
      assertEquals(compact.scalar.keySet, core1.scalar.keySet)
      compact.scalar.foreach((key, value) => assertEqualsDouble(value, core1.scalar(key), 1e-12))
      compact.shared.foreach: (pair, value) =>
        for sample <- Vector(0, 1) do assertEqualsDouble(value, core1.repeated(pair -> sample), 1e-12)
      // Analytic inverse of [[14,6],[6,4]], independent of the native QR result.
      assertEqualsDouble(compact.shared(EstimandPair(fixture.ids.head, fixture.ids.head)), 0.2, 1e-12)
      assertEqualsDouble(compact.shared(EstimandPair(fixture.ids.head, fixture.ids.last)), -0.3, 1e-12)
      assertEqualsDouble(compact.shared(EstimandPair(fixture.ids.last, fixture.ids.last)), 0.7, 1e-12)
      if baseline.nonEmpty then compact.scalar.foreach((key, value) => assertEqualsDouble(value, baseline(key), 1e-12))
      baseline = compact.scalar
  }

  test("shared callback failure and cancellation abort owned delivery") {
    val fixture = ProducerFixture
    val producer = FitEstimateProducer.shared(fixture.prepared(EstimateUncertaintyRequest.Joint), fixture.identity,
      fixture.catalog, fixture.ids, "scanner").toOption.get
    val failed = new JointSink(producer.unit, 1, true)
    failed.rejectShared = true
    assert(producer.write(fixture.reader, failed).isLeft)
    assert(failed.aborted && failed.sharedWrites == 1)
    val thrown = new JointSink(producer.unit, 1, true)
    thrown.throwShared = true
    assert(producer.write(fixture.reader, thrown).isLeft)
    assert(thrown.aborted)
    val cancelled = new JointSink(producer.unit, 1, true)
    assertEquals(producer.write(fixture.reader, cancelled, () => true), Left(EstimateError.Cancelled))
    assert(cancelled.aborted && cancelled.sharedWrites == 0 && cancelled.scalar.isEmpty)
    val undersized = new JointSink(producer.unit, 1, true)
    val larger = FitEstimateProducer.shared(fixture.prepared(EstimateUncertaintyRequest.Joint, 2), fixture.identity,
      fixture.catalog, fixture.ids, "scanner").toOption.get
    assert(larger.write(fixture.reader, undersized).isLeft)
    assert(undersized.aborted && undersized.scalar.isEmpty && undersized.sharedWrites == 0)
  }
