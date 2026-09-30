package scalafim.estimates.io

import java.nio.file.{Files, Path}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Synthetic invariant storage probe. Mathematical producer qualification is
  * separately covered by the actual native shared-OLS producer tests.
  */
object CompactCovarianceProbe:
  private final class HeapMonitor:
    private val active = new AtomicBoolean(true)
    private val peak = new AtomicLong(0L)
    private def sample(): Unit =
      val used = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
      peak.accumulateAndGet(used, (a, b) => math.max(a, b))
      ()
    private val worker = new Thread(() =>
      while active.get() do
        sample()
        Thread.sleep(5)
    , "compact-probe-heap-monitor")
    worker.setDaemon(true)
    worker.start()
    def stop(): Long =
      sample()
      active.set(false)
      worker.join()
      peak.get()

  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => throw new IllegalStateException(e.message), identity)
  private val dataset = DatasetId("00000000-0000-4000-8000-000000000101")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000102")
  private val id = UnitId("00000000-0000-4000-8000-000000000103")
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000104")
  private val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run")))
  private val axis = Vector.tabulate(64)(i => EstimandId(s"coefficient-$i"))
  private val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation.id), ProductTargets.Scalar(axis), PoolingScope.Run, "signal")
  private val scale = effect.copy(id = ProductId("scale"), kind = ProductKind.ResidualVariance, units = "signal^2")
  private val cov = effect.copy(id = ProductId("U"), kind = ProductKind.Covariance, targets = ProductTargets.UpperTriangle(axis), units = "unitless")
  private val products = Vector(effect, scale, cov)
  private def component(i: Int): Double = (if i % 2 == 0 then 1.0 else -1.0) * (i % 4 + 1) / 8.0
  private def entry(i: Int, j: Int): Double = component(i) * component(j) + (if i == j then i + 1.0 else 0.0)
  private def variance(sample: Int): Double = if sample % 17 == 0 then 0.0 else 1.0 + (sample % 5) / 4.0
  private def declaration(samples: Int): EstimateUnit =
    val unknown = ScientificFact.Unknown("synthetic resource probe")
    EstimateUnit(dataset, id, revision, EstimandCatalog(model, axis.map(e =>
      EstimandDefinition(e, e.value, EstimandKind.Coefficient, "signal", "unit", e.value))),
      right(EstimateDomain.make(SampleSpaces(Vector(samples, 1, 1)), Vector.range(0, samples), "scanner")),
      Vector(observation), Vector.empty, products, products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      EstimabilityEvidence.Unknown("no fitted-design claim"),
      EstimateProvenance("compact-resource-probe", "1", "synthetic", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
      covariance = Vector(CovarianceDescriptor(cov.id, effect.id, CovarianceEquation.Normalized(scale.id), true, true)))

  def main(args: Array[String]): Unit =
    require(args.length == 4, "write|read root core1|compact sampleCount")
    require(Runtime.getRuntime.maxMemory() <= 64L * 1024 * 1024)
    val heap = new HeapMonitor
    val store = right(LocalEstimateStore.open(Path.of(args(1))))
    val samples = args(3).toInt
    require(samples == 2048 || samples == 8192)
    val compact = args(2) == "compact"
    val start = System.nanoTime()
    if args(0) == "write" then
      val declared = declaration(samples)
      val layout = if compact then CovarianceLayout.SharedNormalizedTable() else CovarianceLayout.PairNifti
      val sink = right(store.newSink(declared, 512, layout))
      var first = 0
      while first < samples do
        val selected = Vector.range(first, first + 512)
        for i <- axis.indices do
          right(sink.write(effect.id, EstimateSelection(Vector(observation.id), Vector(axis(i)), selected),
            Array.tabulate(512)(s => i * 1000.0 + first + s), Array.fill[Byte](512)(0)))
          right(sink.write(scale.id, EstimateSelection(Vector(observation.id), Vector(axis(i)), selected),
            Array.tabulate(512)(s => variance(first + s)), Array.fill[Byte](512)(0)))
        if !compact then
          for i <- axis.indices; j <- i until axis.size do
            right(sink.writeCovariance(cov.id, CovarianceSelection(Vector(observation.id), Vector(EstimandPair(axis(i), axis(j))), selected),
              Array.fill(512)(entry(i, j)), Array.fill[Byte](512)(0)))
        first += 512
      if compact then
        val all = axis.indices.toVector.flatMap(i => (i until axis.size).map(j => (i, j)))
        all.grouped(512).foreach: batch =>
          right(sink.asInstanceOf[SharedCovarianceSink].writeSharedCovariance(cov.id,
            SharedCovarianceSelection(Vector(observation.id), batch.map((i, j) => EstimandPair(axis(i), axis(j)))),
            batch.map((i, j) => entry(i, j)).toArray, Array.fill[Byte](batch.size)(0)))
      val coverage = if compact then sink.asInstanceOf[NiftiEstimateSink].sharedOutputs.map(_.coverage.length).sum.toLong else samples.toLong * 2080
      val pinned = right(sink.seal())
      val collection = right(store.publishCollection(EstimateCollection(dataset,
        CollectionRevisionId("00000000-0000-4000-8000-000000000105"), model, Map(id -> UnitOutcome.Published(pinned)))))
      right(store.discover(collection, None))
      val records = right(EstimateMetadata.allRepresentations(Files.readString(store.root.resolve(pinned.manifest.path))))
      val uBytes = records.filter(_.product == cov.id).map {
        case EstimateRepresentation.Nifti(value) => value.values.bytes + value.validity.bytes
        case EstimateRepresentation.SharedNormalizedUpperTriangle(value) => value.table.bytes
        case EstimateRepresentation.Hdf5(_) => throw new AssertionError("compact NIfTI probe cannot produce HDF5")
      }.sum
      val stages = Files.walk(store.root.resolve(".staging"))
      val stageFiles = try stages.filter(path => Files.isRegularFile(path)).count() finally stages.close()
      require(stageFiles == 0)
      println(ujson.write(ujson.Obj("mode" -> "write", "compact" -> compact, "samples" -> samples,
        "UBytes" -> uBytes.toDouble, "pairCoverageEntries" -> coverage.toDouble, "pairs" -> 2080,
        "scalarCoverageEntries" -> (samples.toLong * 128).toDouble, "scalarPayloadBytes" -> (samples.toLong * 128 * 9 + 4*352).toDouble,
        "supportEntries" -> declared.domain.support.size, "stagingFilesAfter" -> stageFiles.toDouble,
        "maximumBlockCells" -> 512, "sharedPairsBudget" -> 4096, "sharedBytesBudget" -> 1048576,
        "writerDataValidityHandles" -> (if compact then 4 else 6), "writerCoverageHandles" -> (if compact then 2 else 3),
        "wallSeconds" -> ((System.nanoTime() - start) / 1e9),
        "maxHeapBytes" -> Runtime.getRuntime.maxMemory().toDouble,
        "sampledPeakHeapBytes" -> heap.stop().toDouble, "heapSamplingIntervalMillis" -> 5)))
    else
      require(args(0) == "read")
      val absent = try
        Class.forName("scalafim.fmri.fit.SelectedEstimates$")
        false
      catch case _: ClassNotFoundException => true
      require(absent)
      val collection = right(store.openCollection(right(store.current()).get._1))
      val pinned = collection.units(id).asInstanceOf[UnitOutcome.Published].reference
      val source = right(store.open(pinned, ReadLimits(6)))
      try
        val values = new Array[Double](6)
        val codes = new Array[Byte](6)
        val pairs = Vector(EstimandPair(axis.last, axis.last), EstimandPair(axis.head, axis.last), EstimandPair(axis.head, axis.head))
        right(source.readCovariance(cov.id, CovarianceSelection(Vector(observation.id), pairs, Vector(samples-1, 0)), values, codes))
        val expected = Vector(entry(63,63), entry(63,63), entry(0,63), entry(0,63), entry(0,0), entry(0,0))
        require(values.zip(expected).forall((a,b) => math.abs(a-b) <= 1e-12) && codes.forall(_ == 0))
        val matrix = right(CovarianceAccess.matrix(source, cov.id, observation.id, 111, axis.reverse, CovariancePolicy(64, 1e-10, 1e-10)))
        for i <- 0 until 64; j <- 0 until 64 do require(math.abs(matrix.values(i,j) - entry(63-i,63-j) * variance(111)) <= 1e-10)
        println(ujson.write(ujson.Obj("mode" -> "read", "compact" -> compact, "samples" -> samples,
          "fitterPresent" -> false, "selectedCells" -> 6, "matrixOrder" -> 64,
          "readerDataValidityHandles" -> (if compact then 4 else 6), "gzipStagingBytes" -> 0,
          "wallSeconds" -> ((System.nanoTime() - start) / 1e9), "maxHeapBytes" -> Runtime.getRuntime.maxMemory().toDouble,
          "sampledPeakHeapBytes" -> heap.stop().toDouble, "heapSamplingIntervalMillis" -> 5)))
      finally right(source.close())

/** Process interruption at the new table-leaf seam and existing public unit/CAS
  * boundaries. A table-only halt probes orphan reachability, not power loss.
  */
object CompactPublicationProbe:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => throw new IllegalStateException(e.message), identity)
  private val fixture = CompactFixture
  private val newRevision = UnitRevisionId("00000000-0000-4000-8000-000000000095")
  private val baseCollection = CollectionRevisionId("00000000-0000-4000-8000-000000000096")
  private val newCollection = CollectionRevisionId("00000000-0000-4000-8000-000000000097")
  private def write(store: LocalEstimateStore, compact: Boolean): PinnedUnit =
    val unit = if compact then fixture.unit.copy(revision = newRevision) else fixture.unit
    val sink = right(store.newSink(unit, 3, if compact then CovarianceLayout.SharedNormalizedTable() else CovarianceLayout.PairNifti))
    for observation <- fixture.observations; product <- Vector(fixture.effect, fixture.scale); target <- fixture.ids do
      right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(0, 3, 5)), Array(2.0, 0.0, 5.0), Array.fill[Byte](3)(0)))
    for (observation, o) <- fixture.observations.zipWithIndex; (pair, p) <- fixture.pairs.zipWithIndex do
      val value = (if o == 0 then fixture.firstValues else fixture.secondValues)(p)
      if compact then right(sink.asInstanceOf[SharedCovarianceSink].writeSharedCovariance(fixture.covariance.id,
        SharedCovarianceSelection(Vector(observation.id), Vector(pair)), Array(value), Array[Byte](0)))
      else right(sink.writeCovariance(fixture.covariance.id, CovarianceSelection(Vector(observation.id), Vector(pair), Vector(0,3,5)),
        Array.fill(3)(value), Array.fill[Byte](3)(0)))
    right(sink.seal())
  private def collection(store: LocalEstimateStore, pinned: PinnedUnit, revision: CollectionRevisionId): PinnedEstimateSet =
    right(store.publishCollection(EstimateCollection(fixture.dataset, revision, fixture.catalog.model, Map(fixture.unit.unit -> UnitOutcome.Published(pinned)))))
  def main(args: Array[String]): Unit =
    val store = right(LocalEstimateStore.open(Path.of(args(1))))
    args(0) match
      case "seed" =>
        right(store.discover(collection(store, write(store, false), baseCollection), None))
        println("SEED_PASS")
      case "crash-before-table" => Runtime.getRuntime.halt(87)
      case "crash-after-table" =>
        val table = right(SharedCovarianceTable.decode(fixture.literal, 6))
        right(store.writeText(s"units/${newRevision.value}/shared-covariance-0.json", SharedCovarianceTable.encode(table)))
        require(!Files.exists(store.root.resolve(s"units/${newRevision.value}/estimates.json")))
        Runtime.getRuntime.halt(87)
      case "crash-after-unit" =>
        write(store, true)
        Runtime.getRuntime.halt(87)
      case "crash-after-cas" =>
        val previous = right(store.current()).get._2
        val pinned = collection(store, write(store, true), newCollection)
        right(store.discover(pinned, Some(previous)))
        Runtime.getRuntime.halt(87)
      case "read" =>
        val expectedNew = args(2) == "new"
        val current = right(store.current()).get._1
        require(current.revision == (if expectedNew then newCollection else baseCollection))
        val collectionValue = right(store.openCollection(current))
        val pinned = collectionValue.units(fixture.unit.unit).asInstanceOf[UnitOutcome.Published].reference
        val source = right(store.open(pinned, ReadLimits(1)))
        try
          val values = new Array[Double](1); val codes = new Array[Byte](1)
          right(source.readCovariance(fixture.covariance.id, CovarianceSelection(Vector(fixture.observations.head.id), Vector(fixture.pairs(1)), Vector(3)), values, codes))
          require(values(0) == -1.0 && codes(0) == 0)
          require(source.unit.revision == (if expectedNew then newRevision else fixture.unit.revision))
          println(s"READ_PASS complete=true compact=$expectedNew")
        finally right(source.close())
      case other => throw new IllegalArgumentException(other)
