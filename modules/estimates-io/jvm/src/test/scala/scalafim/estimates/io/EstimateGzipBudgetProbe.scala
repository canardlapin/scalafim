package scalafim.estimates.io

import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Direct small-heap probe; run_gzip_budget_probe.py supplies independent bytes. */
object EstimateGzipBudgetProbe:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(error =>
    throw new IllegalStateException(error.message), identity)

  def main(args: Array[String]): Unit =
    require(args.length == 1)
    val root = Path.of(args(0))
    val store = right(LocalEstimateStore.open(root))
    val dataset = DatasetId("00000000-0000-4000-8000-000000000071")
    val model = ModelRevisionId("00000000-0000-4000-8000-000000000072")
    val unitId = UnitId("00000000-0000-4000-8000-000000000073")
    val revision = UnitRevisionId("00000000-0000-4000-8000-000000000074")
    val target = EstimandId("A")
    val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
    val catalog = EstimandCatalog(model, Vector(EstimandDefinition(target, "A", EstimandKind.Coefficient,
      "signal", "unit", "A")))
    val domain = right(EstimateDomain.make(SampleSpaces(Vector(256, 256, 128)), Vector(0), "scanner"))
    val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
      Vector(observation.id), ProductTargets.Scalar(Vector(target)), PoolingScope.Run, "signal")
    val unknown = ScientificFact.Unknown("independent synthetic probe")
    val unit = EstimateUnit(dataset, unitId, revision, catalog, domain, Vector(observation), Vector.empty,
      Vector(product), Map(product.id -> ProductOutcome.Available(product.id)), EstimabilityEvidence.Unknown("probe"),
      EstimateProvenance("python-struct", "1", "probe", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))
    val values = store.reference(right(store.objects.inspect("values.nii.gz").left.map(store.fromStore)))
    val validity = store.reference(right(store.objects.inspect("validity.nii.gz").left.map(store.fromStore)))
    val representation = NiftiRepresentation(product.id, observation.id, values, validity,
      NumericPrecision.Float64, 1.0, 0.0, Vector(target), "scanner-sform",
      storedDatatype = Some(NiftiStoredDatatype.Float64))
    val pinned = right(store.publishUnit(unit, Vector(representation)))
    val budget = 2L * 352L + domain.sampleCount.toLong * 9L
    val under = store.open(pinned, ReadLimits(1, budget - 1L))
    assert(under.left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]), s"under-budget result $under")
    val source = right(store.open(pinned, ReadLimits(1, budget))).asInstanceOf[NiftiEstimateSource]
    val staged = source.stagedFiles
    try
      assert(source.stagedPayloadBytes == budget)
      assert(staged.size == 2 && staged.forall(Files.exists(_)))
      assert(staged.map(Files.size(_)).sum == budget)
      val readValues = new Array[Double](1)
      val codes = new Array[Byte](1)
      right(source.read(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(0)), readValues, codes))
      assert(readValues(0) == 0.0 && codes(0) == Validity.Valid.code)
      right(source.read(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(domain.sampleCount - 1)), readValues, codes))
      assert(codes(0) == Validity.OutsideSupport.code)
      val peakHeap = ManagementFactory.getMemoryPoolMXBeans.asScala.iterator
        .filter(_.getType == MemoryType.HEAP).map(_.getPeakUsage.getUsed).sum
      println(s"PROBE_PASS staged_bytes=$budget staged_files=${staged.size} peak_heap_bytes=$peakHeap max_heap_bytes=${Runtime.getRuntime.maxMemory()}")
    finally right(source.close())
    assert(staged.forall(path => !Files.exists(path)))
