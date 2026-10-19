package scalafim.fmri.fit.estimates

import gale.linalg.Matrix
import scalafim.dataset.{DataSelection, DatasetError, DatasetEvents, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.estimates.*
import scalafim.fmri.design.StructuralColumnOrigin
import scalafim.fmri.fit.{ChunkSize, EstimateOutput, EstimateUncertaintyRequest, FirstLevelEstimateRequest, FirstLevelEstimates}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FitPlan, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.SampleSpaces

/** Literal, bounded FIR fixture.  The response and analytic oracle are authored
  * independently of the compiled design, so changing FIR compilation cannot
  * silently rewrite the expected fit.
  */
object FirEstimateProducerFixture:
  private def checked[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.toString), value => value)

  val ids = Vector(EstimandId("fir-bin-1"), EstimandId("fir-bin-2"))
  val responses = Vector(
    Vector(11.0, 9.0, 11.0, 9.0, 10.0, 10.0, 13.0, 11.0, 15.0),
    Vector(22.0, 18.0, 22.0, 18.0, 20.0, 20.0, 19.0, 15.0, 20.0)
  )
  val effects = Vector(Vector(2.0, 5.0), Vector(-3.0, 0.0))
  val sse = Vector(6.0, 24.0)
  val sigmaSquared = Vector(1.0, 4.0)
  val effectVariances = Vector(Vector(2.0 / 3.0, 7.0 / 6.0), Vector(8.0 / 3.0, 14.0 / 3.0))

  val frame = SamplingFrame(blockLens = Seq(9), tr = Seq(1.0), startTime = Seq(0.5), precision = 0.25)
  val dataset = FmriDataset.unsafe(
    InMemoryDatasetBackend(scalafim.dataset.DatasetId("analytic-fir"),
      Matrix.tabulate(9, 2)((time, voxel) => responses(voxel)(time)), SampleSpaces(Vector(2, 1, 1))),
    frame,
    DatasetEvents(Vector(Map("onset" -> "6.0", "cond" -> "impulse")))
  )
  val model = FmriModelBuilder.buildModel(dataset,
    ModelBuildSpec("onset ~ hrf(cond)", defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s), precision = 0.25.s))
  val fit = FitPlan(model)
  val firColumns = fit.coefficientAxis.get.columns.filter:
    _.origin match
      case StructuralColumnOrigin.Event(_, _, _, _, Some(reference), _, _) => reference.role.exists(_.isInstanceOf[BasisRole.FirBin])
      case _ => false

  val identity = EstimatePublicationIdentity(
    DatasetId("00000000-0000-4000-8000-000000000701"),
    UnitId("00000000-0000-4000-8000-000000000702"),
    UnitRevisionId("00000000-0000-4000-8000-000000000703"),
    "01", ObservationId("subject-01"), Vector(AcquisitionId("run-1")), "analytic-fir-1", "test", "signal", Vector.empty
  )
  val catalog = EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000704"), Vector(
    EstimandDefinition(ids(0), "FIR 0-2 seconds", EstimandKind.Coefficient, "signal", "unit height",
      "compiled FIR coefficient bound to [0, 2) seconds", ResponseCoordinate.FirInterval("event onset", 0.0, 2.0)),
    EstimandDefinition(ids(1), "FIR 2-4 seconds", EstimandKind.Coefficient, "signal", "unit height",
      "compiled FIR coefficient bound to [2, 4) seconds", ResponseCoordinate.FirInterval("event onset", 2.0, 4.0))
  ))

  def prepared =
    val request = checked(FirstLevelEstimateRequest.make(firColumns.map(column => EstimateOutput.Coefficient(column.id)), EstimateUncertaintyRequest.Marginal))
    checked(FirstLevelEstimates.prepare(fit, request, ChunkSize.unsafe(1)))

  def producer = checked(FitEstimateProducer.shared(prepared, identity, catalog, ids, "scanner"))

  def reader = new DatasetSeriesReader:
    val dataset = FirEstimateProducerFixture.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)

class FirEstimateProducerSuite extends munit.FunSuite:
  import FirEstimateProducerFixture.*

  private final class Sink(val unit: EstimateUnit) extends EstimateSink:
    val maximumBlockCells = 1
    var sealedResult = false
    var values = Map.empty[(ProductKind, EstimandId, Int), Double]
    def write(product: ProductId, selection: EstimateSelection, cells: Array[Double], validity: Array[Byte]) =
      assertEquals(validity.toVector, Vector(Validity.Valid.code))
      val kind = unit.products.find(_.id == product).get.kind
      values += ((kind, selection.estimands.head, selection.samples.head) -> cells.head)
      Right(())
    def seal() =
      sealedResult = true
      Right(PinnedUnit(unit.unit, unit.revision, scalafim.estimates.FileReference("test.json", scalafim.archive.ContentDigest.unsafeSha256("a" * 64), 1)))
    def abort() = Right(())

  test("bounded analytic FIR producer retains compiled bins, literal OLS effects and marginal uncertainty") {
    assertEquals(frame.acquisitionOnsets().map(_.value), Vector(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5))
    assertEquals(firColumns.map(_.origin).map {
      case StructuralColumnOrigin.Event(_, _, _, _, Some(reference), _, _) => reference.role.get
      case other => fail(s"missing FIR structural role: $other")
    }, Vector(BasisRole.FirBin(1, 0.s, 2.s), BasisRole.FirBin(2, 2.s, 4.s)))
    assertEquals(model.designSchema.get.rankPreview.evidence.get.numericalRank, 3)
    val matrix = model.designSchema.get.matrix
    val rows = Vector.tabulate(9)(row => Vector.tabulate(matrix.cols)(column => matrix(row, column)))
    assertEquals(rows.take(6), Vector.fill(6)(Vector(0.0, 0.0, 1.0)))
    assertEquals(rows.slice(6, 8), Vector.fill(2)(Vector(1.0, 0.0, 1.0)))
    assertEquals(rows.drop(8), Vector(Vector(0.0, 1.0, 1.0)))

    val compiled = producer
    assertEquals(compiled.unit.bindings.map(_.estimand), ids)
    assertEquals(compiled.unit.bindings.map(_.columnIds), Vector.fill(2)(fit.coefficientAxis.get.columnIds.map(id => scalafim.estimates.ColumnId(id.value))))
    assertEquals(compiled.unit.bindings.map(_.weights), Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)))
    assertEquals(compiled.unit.catalog.entries.map(_.response), Vector(
      ResponseCoordinate.FirInterval("event onset", 0.0, 2.0), ResponseCoordinate.FirInterval("event onset", 2.0, 4.0)))
    assertEquals(compiled.unit.degreesOfFreedom, Vector(DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(6.0), "OLS n - numerical rank", false)))
    assertEquals(compiled.unit.marginalUncertainty.head.origin, MarginalVarianceOrigin.Estimated(compiled.unit.degreesOfFreedom.head))

    val sink = new Sink(compiled.unit)
    assert(compiled.write(reader, sink).isRight)
    assert(sink.sealedResult)
    for voxel <- effects.indices; bin <- ids.indices do
      assertEqualsDouble(sink.values((ProductKind.Effect, ids(bin), voxel)), effects(voxel)(bin), 1e-10)
      assertEqualsDouble(sink.values((ProductKind.StandardError, ids(bin), voxel)), math.sqrt(effectVariances(voxel)(bin)), 1e-10)
      assertEqualsDouble(sink.values((ProductKind.ResidualVariance, ids(bin), voxel)), sigmaSquared(voxel), 1e-10)
  }
