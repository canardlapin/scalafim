package scalafim.fmri.fit.estimates

import gale.linalg.Matrix
import scalafim.archive.ContentDigest
import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend}
import scalafim.estimates.*
import scalafim.fmri.design.{ColumnRole, DesignSchema, ModulatorId, RowLayout, RunScope, StructuralColumn, StructuralColumnOrigin}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, BaselineSpec, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FitStrategy, FmriModel}
import scalafim.image.SampleSpaces

/** Native X'X=P1/P2, with residuals on rows orthogonal to both columns.
  * The oracle uses literal 2x2 algebra and never calls native pooling/solvers.
  */
object PooledProducerFixture:
  def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), identity)
  val ids = Vector(EstimandId("z-signed"), EstimandId("α-first"), EstimandId("a-mixed"))
  val weights = Vector(Vector(1.0, -2.0), Vector(1.0, 0.0), Vector(-0.5, 1.5))
  val sampleOrder = Vector(2, 4, 0, 3, 1)
  val publication = EstimatePublicationIdentity(DatasetId("00000000-0000-4000-8000-000000000101"),
    UnitId("00000000-0000-4000-8000-000000000102"), UnitRevisionId("00000000-0000-4000-8000-000000000103"),
    "01", ObservationId("pooled-01"), Vector(AcquisitionId("acq-A"), AcquisitionId("acq-B")),
    "pooled-execution", "test", "signal", Vector.empty)
  def beta(run: Int, sample: Int, shift: Double = 0.0): Vector[Double] =
    if run == 0 then Vector(1.0 + sample + shift, -2.0 + sample - shift)
    else Vector(5.0 - sample + shift, 3.0 + 2 * sample - shift)
  def scale(run: Int, sample: Int): Double = if run == 0 || sample == 0 then 1.0 else if sample == 1 then 4.0 else 2.0
  def oracle(sample: Int, shift: Double = 0.0, runs: Vector[Int] = Vector(0, 1)): (Vector[Double], Vector[Vector[Double]]) =
    val precisions = Vector(Vector(Vector(2.0, 1.0), Vector(1.0, 2.0)), Vector(Vector(3.0, -1.0), Vector(-1.0, 1.0)))
    val q = Vector.tabulate(2, 2)((i, j) => runs.map(r => precisions(r)(i)(j) / scale(r, sample)).sum)
    val determinant = q(0)(0) * q(1)(1) - q(0)(1) * q(1)(0)
    val sigma = Vector(Vector(q(1)(1) / determinant, -q(0)(1) / determinant), Vector(-q(1)(0) / determinant, q(0)(0) / determinant))
    val rhs = Vector.tabulate(2)(i => runs.map(r => (0 until 2).map(j => precisions(r)(i)(j) * beta(r, sample, shift)(j) / scale(r, sample)).sum).sum)
    val pooled = Vector.tabulate(2)(i => (0 until 2).map(j => sigma(i)(j) * rhs(j)).sum)
    val effects = weights.map(w => w.zip(pooled).map(_ * _).sum)
    val covariance = weights.map(a => weights.map(b => (for i <- 0 until 2; j <- 0 until 2 yield a(i) * sigma(i)(j) * b(j)).sum))
    (effects, covariance)

  final class NativePooledFixture(val shift: Double = 0.0, allExcluded: Boolean = false):
    val frame = SamplingFrame(blockLens = Seq(4, 4), tr = Seq(1.0, 1.0))
    val design = Vector(Vector(math.sqrt(2.0), 1.0 / math.sqrt(2.0)), Vector(0.0, math.sqrt(1.5)), Vector(0.0, 0.0), Vector(0.0, 0.0),
      Vector(math.sqrt(3.0), -1.0 / math.sqrt(3.0)), Vector(0.0, math.sqrt(2.0 / 3.0)), Vector(0.0, 0.0), Vector(0.0, 0.0))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId(s"native-pooled-$shift-$allExcluded"),
      Matrix.tabulate(8, 5)((t, v) =>
        if allExcluded || v == 3 then 0.0
        else if v == 4 then 7.0
        else design(t).zip(beta(t / 4, v, shift)).map(_ * _).sum + (if t % 4 == 2 then math.sqrt(2.0 * scale(t / 4, v)) else 0.0)),
      SampleSpaces(Vector(5, 1, 1))), frame)
    private val schema = checked(DesignSchema.validated(Mat.fromRows(design), RowLayout.fromSamplingFrame(frame),
      Vector("first", "second").zipWithIndex.map((name, i) => checked(StructuralColumn.fromOrigin(i + 1,
        StructuralColumnOrigin.Sampled(ModulatorId.unsafe(name), ColumnRole.Task, RunScope.Global), name)))))
    private val event = EventModel(Vector.empty, frame, Mat.fromRows(design), Vector("first", "second"), Vector(0 -> 1, 1 -> 2),
      Map("first" -> Vector(0), "second" -> Vector(1)), compiledSchema = Some(schema))
    private val emptyBaseline = Mat.zeros(8, 0)
    private val baseline = BaselineModel(Vector.empty, BaselineSpec(basis = BaselineBasis.Constant, intercept = Intercept.None), frame,
      emptyBaseline, Vector.empty, Vector.empty, Map.empty,
      compiledSchema = Some(checked(DesignSchema.validated(emptyBaseline, RowLayout.fromSamplingFrame(frame), Vector.empty))))
    val model = FmriModel(event, baseline, dataset)
    require(model.nPredictors == 2, "the precision oracle requires exactly two columns and no hidden baseline")
    val fit = FitPlan(model, FitStrategy.SeparateRunsThenFixedEffects())
    val columns = fit.coefficientAxis.get.columnIds
    def request(mode: EstimateUncertaintyRequest, retain: Boolean = false): FirstLevelEstimateRequest = checked(FirstLevelEstimateRequest.make(
      Vector(EstimateOutput.Contrast(StructuralTContrast.fromIds(ContrastId.unsafe("signed"), "signed L", Map(columns(0) -> 1.0, columns(1) -> -2.0))),
        EstimateOutput.Coefficient(columns(0)),
        EstimateOutput.Contrast(StructuralTContrast.fromIds(ContrastId.unsafe("mixed"), "mixed L", Map(columns(0) -> -0.5, columns(1) -> 1.5)))),
      mode, if retain then Vector(columns.head) else Vector.empty))
    def prepared(mode: EstimateUncertaintyRequest = EstimateUncertaintyRequest.Joint, blockSize: Int = 2,
        runs: Vector[Int] = Vector(0, 1), retain: Boolean = false): FirstLevelFixedEffectsEstimatePlan =
      val scans = runs.flatMap(r => Vector(3, 2, 1, 0).map(_ + 4 * r)).reverse
      checked(FirstLevelFixedEffectsEstimates.prepare(fit, request(mode, retain), ChunkSize.unsafe(blockSize),
        DataSelection(time = IndexSelection.Indices(scans), voxels = IndexSelection.Indices(sampleOrder))))
    def catalog(plan: FirstLevelFixedEffectsEstimatePlan): EstimandCatalog =
      EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000104"), ids.indices.toVector.map(i =>
        EstimandDefinition(ids(i), "repeated label", if i == 1 then EstimandKind.Coefficient else EstimandKind.LinearContrast,
          "signal", PooledFitEstimateProducer.normalization(plan, i), s"explicit selected native row $i")).reverse)
    def producer(mode: EstimateUncertaintyRequest = EstimateUncertaintyRequest.Joint, blockSize: Int = 2,
        identity: EstimatePublicationIdentity = publication, runs: Vector[Int] = Vector(0, 1)): PooledFitEstimateProducer =
      val plan = prepared(mode, blockSize, runs)
      checked(PooledFitEstimateProducer.make(plan, identity, catalog(plan), ids, "scanner"))
    final class Reader extends DatasetSeriesReader:
      val dataset = NativePooledFixture.this.dataset
      var reads = 0
      var closed = false
      var reject = false
      var thrown = false
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        require(!closed)
        reads += 1
        if thrown then throw new IllegalStateException("injected reader exception")
        else if reject then Left(DatasetError.ResponseSelectionFailure("injected reader failure"))
        else dataset.seriesEither(selection)
      def close(): Unit = closed = true
    def reader: Reader = new Reader

class PooledFitEstimateProducerSuite extends munit.FunSuite:
  import PooledProducerFixture.*
  private final class Sink(val unit: EstimateUnit, val maximumBlockCells: Int, pairs: Boolean = true) extends SharedCovarianceSink:
    override val supportsCovariance = pairs
    var compact = false
    def sharedCovarianceProducts = if compact then unit.covariance.map(_.product).toSet else Set.empty[ProductId]
    var aborts = 0
    var seals = 0
    var writes = 0
    var rejectAt = 0
    var throwAt = 0
    var sealFailure = false
    var sealThrow = false
    var callback: () => Unit = () => ()
    var scalars = Map.empty[(ProductKind, EstimandId, Int), (Double, Byte)]
    var covariance = Map.empty[(EstimandPair, Int), (Double, Byte)]
    def step(): Either[EstimateError, Unit] =
      writes += 1
      if writes == throwAt then throw new IllegalStateException("injected sink exception")
      callback()
      if writes == rejectAt then Left(EstimateError.Io("injected sink failure")) else Right(())
    def write(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte]) =
      assert(selection.cells <= maximumBlockCells)
      assertEquals(selection.observations, Vector(unit.observations.head.id))
      step().map: _ =>
        selection.samples.zipWithIndex.foreach: (sample, i) =>
          val key = (unit.products.find(_.id == product).get.kind, selection.estimands.head, sample)
          assert(!scalars.contains(key))
          scalars += key -> (values(i), validity(i))
    override def writeCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte]) =
      assertEquals(product, unit.covariance.head.product)
      assert(selection.cells <= maximumBlockCells)
      step().map: _ =>
        selection.samples.zipWithIndex.foreach: (sample, i) =>
          val key = selection.pairs.head -> sample
          assert(!covariance.contains(key))
          covariance += key -> (values(i), validity(i))
    def writeSharedCovariance(product: ProductId, selection: SharedCovarianceSelection, values: Array[Double], validity: Array[Byte]) =
      fail("samplewise absolute covariance must never use shared delivery")
    def seal() =
      seals += 1
      if sealThrow then throw new IllegalStateException("injected seal exception")
      else if sealFailure then Left(EstimateError.Io("injected seal failure"))
      else Right(PinnedUnit(unit.unit, unit.revision, FileReference("test.json", ContentDigest.unsafeSha256("b" * 64), 1)))
    def abort() =
      aborts += 1
      Right(())

  test("native preparation refuses legacy and rank-deficient selected run designs before response IO") {
    val f = new NativePooledFixture()
    val legacy = f.model.copy(eventModel = f.model.eventModel.copy(compiledSchema = None))
    val legacyPlan = FitPlan(legacy, FitStrategy.SeparateRunsThenFixedEffects())
    val legacyRequest = checked(FirstLevelEstimateRequest.make(legacyPlan.coefficientAxis.get.columnIds.map(EstimateOutput.Coefficient.apply), EstimateUncertaintyRequest.Joint))
    val refused = FirstLevelFixedEffectsEstimates.prepare(legacyPlan, legacyRequest, ChunkSize.unsafe(2))
    assert(refused.left.toOption.exists(_.message.contains("non-legacy")))
    // Retain only one nonzero design row in run 2: its rank cannot be two.
    val deficient = FirstLevelFixedEffectsEstimates.prepare(f.fit, f.request(EstimateUncertaintyRequest.Joint), ChunkSize.unsafe(2),
      DataSelection(time = IndexSelection.Indices(Vector(0,1,2,3,4,6,7))))
    assert(deficient.isLeft)
    assertEquals(f.reader.reads, 0)
  }

  test("native heteroscedastic multivariate pooling matches independent dense L beta and L Sigma L' at blocks 1/2/3") {
    for size <- Vector(1, 2, 3) do
      val f = new NativePooledFixture()
      val producer = f.producer(blockSize = size)
      val reader = f.reader
      assertEquals(reader.reads, 0)
      val sink = new Sink(producer.unit, size)
      assert(producer.write(reader, sink).isRight)
      assertEquals(reader.reads, (5 + size - 1) / size)
      assertEquals(sink.aborts, 0)
      assertEquals(sink.seals, 1)
      assert(!reader.closed)
      assertEquals(producer.unit.domain.support, sampleOrder)
      assertEquals(sink.scalars.size, 2 * 3 * 5)
      assertEquals(sink.covariance.size, 6 * 5)
      for sample <- 0 until 3; row <- ids.indices do
        val (effect, cov) = oracle(sample)
        assertEqualsDouble(sink.scalars((ProductKind.Effect, ids(row), sample))._1, effect(row), 1e-11)
        assertEqualsDouble(sink.scalars((ProductKind.StandardError, ids(row), sample))._1, math.sqrt(cov(row)(row)), 1e-11)
        assertEquals(sink.scalars((ProductKind.Effect, ids(row), sample))._2, Validity.Valid.code)
        for col <- row until ids.size do
          val cell = sink.covariance(EstimandPair(ids(row), ids(col)) -> sample)
          assertEqualsDouble(cell._1, cov(row)(col), 1e-11)
          assertEquals(cell._2, Validity.Valid.code)
      for sample <- Vector(3, 4); row <- ids.indices do
        assertEquals(sink.scalars((ProductKind.Effect, ids(row), sample)), (0.0, Validity.NonEstimable.code))
        assertEquals(sink.scalars((ProductKind.StandardError, ids(row), sample)), (0.0, Validity.NonEstimable.code))
        for col <- row until ids.size do assertEquals(sink.covariance(EstimandPair(ids(row), ids(col)) -> sample), (0.0, Validity.NonEstimable.code))
      // c'Sigma c also checks signed cross terms, independently of a pair diagonal.
      val c = Vector(2.0, -1.0, 0.5)
      val actual = (for i <- c.indices; j <- c.indices yield c(i) * c(j) * sink.covariance(EstimandPair(ids(math.min(i,j)), ids(math.max(i,j))) -> 1)._1).sum
      val expected = (for i <- c.indices; j <- c.indices yield c(i) * c(j) * oracle(1)._2(i)(j)).sum
      assertEqualsDouble(actual, expected, 1e-10)
  }

  test("literal two-voxel covariance cannot be one shared normalized table") {
    val a = oracle(0)._2
    val b = oracle(1)._2
    assertEqualsDouble(a(1)(1), 1.0 / 5.0, 1e-14)
    assertEqualsDouble(b(1)(1), 2.0 / 5.0, 1e-14)
    // Signed readout covariance changes shape as well as magnitude.
    assert(math.abs(b(0)(1) / a(0)(1) - b(1)(1) / a(1)(1)) > 0.1)
  }

  test("metadata records exact native axes, selected scans, policies and honest df") {
    val f = new NativePooledFixture()
    val p = f.producer()
    assert(p.unit.products.forall(_.pooling == PoolingScope.PooledRuns))
    assertEquals(p.unit.products.map(_.kind), Vector(ProductKind.Effect, ProductKind.StandardError, ProductKind.Covariance))
    assertEquals(p.unit.covariance.head.equation, CovarianceEquation.Absolute)
    assert(!p.unit.covariance.head.invariantSamples)
    assertEquals(p.unit.bindings.map(_.weights), weights)
    assertEquals(p.unit.bindings.head.columnIds.map(_.value), p.prepared.pooledCoefficientAxis.columnIds.map(_.value))
    assertEquals(p.unit.provenance.scans.map(_.zeroBasedRows), Vector(Vector(0,1,2,3), Vector(0,1,2,3)))
    assertEquals(p.unit.observations.head.acquisitions, publication.acquisitions)
    assertEquals(p.unit.marginalUncertainty.head.origin, MarginalVarianceOrigin.Unknown(PooledFitEstimateProducer.UnknownPooledDfReason))
    assertEquals(p.unit.degreesOfFreedom.head.value, DfValue.Scalar(4.0))
    assert(p.unit.degreesOfFreedom.head.method.contains("descriptive only"))
    assert(p.unit.statistics.isEmpty)
    p.unit.estimability match
      case EstimabilityEvidence.FullRank(_, n, tolerance, method) =>
        assertEquals(n, 8)
        assertEqualsDouble(tolerance, p.prepared.runPreparations.map(_.diagnostics.rankReport.tolerance).max, 0.0)
        assert(method.contains("run=0") && method.contains("run=1") && method.contains("maximum"))
      case other => fail(other.toString)
    val second = f.producer(runs = Vector(1))
    assertEquals(second.unit.observations.head.acquisitions, Vector(publication.acquisitions(1)))
    assertEquals(second.unit.provenance.scans, Vector(AcquisitionScans(publication.acquisitions(1), Vector(0,1,2,3))))
    assert(second.unit.products.forall(_.pooling == PoolingScope.PooledRuns))
    val reader = f.reader
    val sink = new Sink(second.unit, 2)
    assert(second.write(reader, sink).isRight)
    for sample <- 0 until 3; row <- ids.indices do
      assertEqualsDouble(sink.scalars((ProductKind.Effect, ids(row), sample))._1, oracle(sample, runs = Vector(1))._1(row), 1e-11)
  }

  test("effects-only, marginal and joint preserve native pooling without invented residual products") {
    val f = new NativePooledFixture()
    for mode <- Vector(EstimateUncertaintyRequest.None, EstimateUncertaintyRequest.Marginal, EstimateUncertaintyRequest.Joint) do
      val p = f.producer(mode)
      val sink = new Sink(p.unit, 2)
      assert(p.write(f.reader, sink).isRight)
      assert(!p.unit.products.exists(_.kind == ProductKind.ResidualVariance))
      for sample <- 0 until 3; row <- ids.indices do assertEqualsDouble(sink.scalars((ProductKind.Effect, ids(row), sample))._1, oracle(sample)._1(row), 1e-11)
      assertEquals(sink.covariance.isEmpty, mode != EstimateUncertaintyRequest.Joint)
      assertEquals(sink.scalars.size, (if mode == EstimateUncertaintyRequest.None then 1 else 2) * 15)
  }

  test("all-excluded domains are explicitly covered and sealed; coarse native status mapping is exhaustive") {
    val f = new NativePooledFixture(allExcluded = true)
    val p = f.producer()
    val sink = new Sink(p.unit, 2)
    assert(p.write(f.reader, sink).isRight)
    assertEquals(sink.scalars.size, 30)
    assertEquals(sink.covariance.size, 30)
    assert(sink.scalars.values.forall(_ == (0.0, Validity.NonEstimable.code)))
    assert(sink.covariance.values.forall(_ == (0.0, Validity.NonEstimable.code)))
    assertEquals(sink.seals, 1)
    assertEquals(PooledFitEstimateProducer.validity(VoxelFitStatus.NoObservedResponses), Validity.MissingInput)
    assertEquals(PooledFitEstimateProducer.validity(VoxelFitStatus.NonFinite), Validity.NumericalFailure)
    for status <- VoxelFitStatus.values if status != VoxelFitStatus.Estimable && status != VoxelFitStatus.NoObservedResponses && status != VoxelFitStatus.NonFinite do
      assertEquals(PooledFitEstimateProducer.validity(status), Validity.NonEstimable)
  }

  test("unsupported retention and invalid catalog mapping fail before execution") {
    val f = new NativePooledFixture()
    val retained = f.prepared(retain = true)
    assert(PooledFitEstimateProducer.make(retained, publication, f.catalog(retained), ids, "scanner").left.toOption.get.isInstanceOf[EstimateError.Unsupported])
    val p = f.prepared()
    val catalog = f.catalog(p)
    def refused(c: EstimandCatalog, mapped: Vector[EstimandId] = ids, identity: EstimatePublicationIdentity = publication) =
      assert(PooledFitEstimateProducer.make(p, identity, c, mapped, "scanner").isLeft)
    refused(catalog, ids.reverse)
    refused(catalog, Vector(ids.head, ids.head, ids.last))
    refused(catalog, ids.dropRight(1))
    refused(catalog.copy(entries = catalog.entries.map(_.copy(normalization = "made up"))))
    refused(catalog.copy(entries = catalog.entries.map(_.copy(kind = EstimandKind.Hypothesis))))
    refused(catalog.copy(entries = catalog.entries.updated(0, catalog.entries.head.copy(units = "other"))))
    refused(catalog, identity = publication.copy(acquisitions = publication.acquisitions.take(1)))
    refused(catalog, identity = publication.copy(acquisitions = Vector.fill(2)(publication.acquisitions.head)))
    refused(catalog, identity = publication.copy(responseUnits = ""))
    assertEquals(f.reader.reads, 0)
  }

  test("scalar-only, compact, wrong-unit and undersized sinks are refused before reader IO") {
    val f = new NativePooledFixture()
    val p = f.producer()
    val sinks = Vector(new Sink(p.unit, 2, false), new Sink(p.unit, 1), new Sink(p.unit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000199")), 2), new Sink(p.unit, 2))
    sinks.last.compact = true
    for sink <- sinks do
      val reader = f.reader
      assert(p.write(reader, sink).isLeft)
      assertEquals(reader.reads, 0)
      assertEquals(sink.writes, 0)
      assertEquals(sink.aborts, 1)
      assertEquals(sink.seals, 0)
  }

  test("returned/thrown scalar and covariance failures stop before any next read and preserve caller reader") {
    val f = new NativePooledFixture()
    val p = f.producer(blockSize = 1)
    for index <- Vector(1, 7); thrown <- Vector(false, true) do
      val sink = new Sink(p.unit, 1)
      if thrown then sink.throwAt = index else sink.rejectAt = index
      val reader = f.reader
      assert(p.write(reader, sink).isLeft)
      assertEquals(reader.reads, 1)
      assertEquals(sink.writes, index)
      assertEquals(sink.aborts, 1)
      assertEquals(sink.seals, 0)
      assert(!reader.closed)
      assert(reader.seriesEither(DataSelection.All).isRight)
      reader.close()
  }

  test("cancellation before read, between read/delivery, during callback and after final delivery aborts without seal") {
    val f = new NativePooledFixture()
    val p = f.producer(blockSize = 1)
    for point <- Vector("before", "after-read", "callback", "final") do
      val sink = new Sink(p.unit, 1)
      val reader = f.reader
      var stop = point == "before"
      if point == "callback" then sink.callback = () => stop = true
      val cancelled = () =>
        if point == "after-read" && reader.reads == 1 then true
        else if point == "final" && sink.writes == 60 then true
        else stop
      assertEquals(p.write(reader, sink, cancelled), Left(EstimateError.Cancelled))
      assertEquals(sink.seals, 0)
      assertEquals(sink.aborts, 1)
      assert(!reader.closed)
      assertEquals(reader.reads, if point == "before" then 0 else if point == "final" then 5 else 1)
      assertEquals(sink.writes, if point == "before" || point == "after-read" then 0 else if point == "final" then 60 else 1)
  }

  test("reader failures and final seal Left/throw abort with typed errors") {
    val f = new NativePooledFixture()
    val p = f.producer()
    for thrown <- Vector(false, true) do
      val reader = f.reader
      reader.reject = !thrown
      reader.thrown = thrown
      val sink = new Sink(p.unit, 2)
      assert(p.write(reader, sink).isLeft)
      assertEquals(sink.aborts, 1)
      assertEquals(sink.seals, 0)
      assertEquals(reader.reads, 1)
      assert(!reader.closed)
      val finalSink = new Sink(p.unit, 2)
      finalSink.sealFailure = !thrown
      finalSink.sealThrow = thrown
      assert(p.write(f.reader, finalSink).isLeft)
      assertEquals(finalSink.aborts, 1)
      assertEquals(finalSink.seals, 1)
  }
