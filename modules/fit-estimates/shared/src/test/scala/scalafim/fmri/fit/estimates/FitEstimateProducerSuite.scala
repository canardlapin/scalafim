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
  def prepared(uncertainty: EstimateUncertaintyRequest) =
    val request = checked(FirstLevelEstimateRequest.make(fit.coefficientAxis.get.columnIds.map(EstimateOutput.Coefficient.apply), uncertainty))
    checked(FirstLevelEstimates.prepare(fit, request, ChunkSize.unsafe(1)))
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
    assert(producer.unit.estimability.isInstanceOf[EstimabilityEvidence.FullRank])
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
