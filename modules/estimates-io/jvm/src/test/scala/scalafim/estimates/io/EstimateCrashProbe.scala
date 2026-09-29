package scalafim.estimates.io

import java.nio.file.Path
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Small separate-JVM publication probe. The crash modes deliberately halt the
  * process at public transaction boundaries; a controller reopens in a new JVM.
  */
object EstimateCrashProbe:
  private def right[A](value: Either[EstimateError, A]): A =
    value.fold(error => throw new IllegalStateException(error.message), identity)

  private val dataset = DatasetId("00000000-0000-4000-8000-000000000061")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000062")
  private val unitId = UnitId("00000000-0000-4000-8000-000000000063")
  private val baseRevision = UnitRevisionId("00000000-0000-4000-8000-000000000064")
  private val newRevision = UnitRevisionId("00000000-0000-4000-8000-000000000065")
  private val baseCollection = CollectionRevisionId("00000000-0000-4000-8000-000000000066")
  private val newCollection = CollectionRevisionId("00000000-0000-4000-8000-000000000067")
  private val estimand = EstimandId("effect")
  private val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
  private val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation.id), ProductTargets.Scalar(Vector(estimand)), PoolingScope.Run, "signal")
  private val catalog = EstimandCatalog(model, Vector(
    EstimandDefinition(estimand, "effect", EstimandKind.Coefficient, "signal", "unit", "synthetic")))
  private val domain = right(EstimateDomain.make(SampleSpaces(Vector(2, 1, 1)), Vector(0, 1), "scanner"))
  private val unknown = ScientificFact.Unknown("synthetic")

  private def unit(revision: UnitRevisionId): EstimateUnit =
    EstimateUnit(dataset, unitId, revision, catalog, domain, Vector(observation), Vector.empty,
      Vector(product), Map(product.id -> ProductOutcome.Available(product.id)),
      EstimabilityEvidence.Unknown("no rank claim"),
      EstimateProvenance("crash-probe", "1", "execution", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))

  private def write(store: LocalEstimateStore, revision: UnitRevisionId,
      values: Array[Double], complete: Boolean): Option[PinnedUnit] =
    val sink = right(store.newSink(unit(revision), 2))
    val samples = if complete then Vector(0, 1) else Vector(0)
    right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(estimand), samples),
      values.take(samples.size), Array.fill[Byte](samples.size)(0)))
    if complete then Some(right(sink.seal())) else None

  private def publish(store: LocalEstimateStore, ref: PinnedUnit,
      revision: CollectionRevisionId): PinnedEstimateSet =
    right(store.publishCollection(EstimateCollection(dataset, revision, model,
      Map(unitId -> UnitOutcome.Published(ref)))))

  def main(args: Array[String]): Unit =
    require(args.length >= 2, "seed|crash-before-seal|crash-after-seal|crash-after-collection|crash-after-cas|read root [base|new]")
    val store = right(LocalEstimateStore.open(Path.of(args(1))))
    args(0) match
      case "seed" =>
        val ref = write(store, baseRevision, Array(1.0, 2.0), true).get
        val collection = publish(store, ref, baseCollection)
        right(store.discover(collection, None))
        println("SEED_PASS")
      case "crash-before-seal" =>
        write(store, newRevision, Array(3.0, 4.0), false)
        Runtime.getRuntime.halt(87)
      case "crash-after-seal" | "crash-after-collection" | "crash-after-cas" =>
        val ref = write(store, newRevision, Array(3.0, 4.0), true).get
        if args(0) == "crash-after-seal" then Runtime.getRuntime.halt(87)
        val collection = publish(store, ref, newCollection)
        if args(0) == "crash-after-collection" then Runtime.getRuntime.halt(87)
        val previous = right(store.current()).get._2
        right(store.discover(collection, Some(previous)))
        Runtime.getRuntime.halt(87)
      case "read" =>
        require(args.length == 3)
        val expectedNew = args(2) == "new"
        val pinned = right(store.current()).get._1
        assert(pinned.revision == (if expectedNew then newCollection else baseCollection))
        val collection = right(store.openCollection(pinned))
        val ref = collection.units(unitId) match
          case UnitOutcome.Published(reference) => reference
          case other => throw new IllegalStateException(s"unexpected unit outcome $other")
        val source = right(store.open(ref, ReadLimits(2)))
        try
          val values = new Array[Double](2)
          val codes = new Array[Byte](2)
          right(source.read(product.id,
            EstimateSelection(Vector(observation.id), Vector(estimand), Vector(0, 1)), values, codes))
          assert(values.toVector == (if expectedNew then Vector(3.0, 4.0) else Vector(1.0, 2.0)))
          assert(codes.toVector == Vector[Byte](0, 0))
          println(s"READ_PASS revision=${pinned.revision.value} values=${values.mkString(",")}")
        finally right(source.close())
      case other => throw new IllegalArgumentException(s"unknown probe mode $other")
