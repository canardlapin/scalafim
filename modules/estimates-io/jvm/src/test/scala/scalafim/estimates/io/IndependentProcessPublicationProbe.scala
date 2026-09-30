package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Duration
import scalafim.estimates.*

/** Direct JVM child for the independent publication-race regression. */
object IndependentProcessPublicationProbe:
  private def right[A](value: Either[EstimateError, A]): A =
    value.fold(error => throw new IllegalStateException(error.message), value => value)

  private val dataset = DatasetId("00000000-0000-4000-8000-000000000801")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000802")
  private val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
  private val estimand = EstimandId("effect")
  private val catalog = EstimandCatalog(model, Vector(EstimandDefinition(estimand, "effect", EstimandKind.Coefficient, "signal", "unit", "independent-process")))
  private val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation.id), ProductTargets.Scalar(Vector(estimand)), PoolingScope.Run, "signal")
  private val domain = right(EstimateDomain.make(scalafim.image.SampleSpaces(Vector(2, 1, 1)), Vector(0, 1), "scanner"))
  private val unknown = ScientificFact.Unknown("independent process fixture")

  private final case class Identity(label: String, unit: UnitId, revision: UnitRevisionId, collection: CollectionRevisionId, values: Array[Double])
  private def identity(label: String): Identity = label match
    case "compatible-a" => Identity(label, UnitId("00000000-0000-4000-8000-000000000811"), UnitRevisionId("00000000-0000-4000-8000-000000000812"), CollectionRevisionId("00000000-0000-4000-8000-000000000813"), Array(11.0, 12.0))
    case "compatible-b" => Identity(label, UnitId("00000000-0000-4000-8000-000000000821"), UnitRevisionId("00000000-0000-4000-8000-000000000822"), CollectionRevisionId("00000000-0000-4000-8000-000000000823"), Array(21.0, 22.0))
    case "conflict-a" => Identity(label, UnitId("00000000-0000-4000-8000-000000000831"), UnitRevisionId("00000000-0000-4000-8000-000000000832"), CollectionRevisionId("00000000-0000-4000-8000-000000000833"), Array(31.0, 32.0))
    case "conflict-b" => Identity(label, UnitId("00000000-0000-4000-8000-000000000831"), UnitRevisionId("00000000-0000-4000-8000-000000000834"), CollectionRevisionId("00000000-0000-4000-8000-000000000835"), Array(41.0, 42.0))
    case other => throw new IllegalArgumentException(s"unknown identity $other")

  private def unit(id: Identity): EstimateUnit =
    EstimateUnit(dataset, id.unit, id.revision, catalog, domain, Vector(observation), Vector.empty,
      Vector(product), Map(product.id -> ProductOutcome.Available(product.id)), EstimabilityEvidence.Unknown("synthetic race fixture"),
      EstimateProvenance("independent-process-probe", "1", id.label, unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))

  private def pinned(store: LocalEstimateStore, id: Identity): PinnedUnit =
    val path = s"units/${id.revision.value}/estimates.json"
    PinnedUnit(id.unit, id.revision, store.reference(right(store.objects.inspect(path).left.map(store.fromStore))))

  private def publishUnit(store: LocalEstimateStore, id: Identity): PinnedUnit =
    val sink = right(store.newSink(unit(id), 2))
    right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(estimand), Vector(0, 1)), id.values, Array[Byte](0, 0)))
    right(sink.seal())

  private def await(path: Path): Unit =
    val deadline = System.nanoTime() + Duration.ofSeconds(20).toNanos
    while !Files.exists(path) && System.nanoTime() < deadline do Thread.sleep(10)
    if !Files.exists(path) then throw new IllegalStateException(s"timed out waiting for $path")

  private def publish(root: Path, label: String, ready: Path, go: Path): Unit =
    val store = right(LocalEstimateStore.open(root))
    val id = identity(label)
    val ref = publishUnit(store, id)
    Files.writeString(ready, s"${ref.unit.value}\n${ref.revision.value}\n${ref.manifest.path}\n${ref.manifest.digest.value}\n", UTF_8)
    await(go)
    val collection = EstimateCollection(dataset, id.collection, model, Map(id.unit -> UnitOutcome.Published(ref)))
    store.discover(right(store.publishCollection(collection)), None) match
      case Right(_) => println(s"PUBLISH_PASS label=$label revision=${id.revision.value}")
      case Left(_: EstimateError.Conflict) => println(s"PUBLISH_CONFLICT label=$label revision=${id.revision.value}")
      case Left(error) => throw new IllegalStateException(error.message)

  private def observe(root: Path, expected: String): Unit =
    val store = right(LocalEstimateStore.open(root))
    val (pointer, digest) = right(store.current()).getOrElse(throw new IllegalStateException("missing current pointer"))
    val collection = right(store.openCollection(pointer))
    val id = identity(expected)
    val members = collection.units.collect { case (unit, UnitOutcome.Published(ref)) => unit -> ref }.toMap
    if pointer.revision != id.collection then throw new IllegalStateException("pointer collection revision does not retain the exact CAS winner")
    if members != Map(id.unit -> pinned(store, id)) then throw new IllegalStateException("pointer does not retain the exact CAS winner")
    println(s"OBSERVE_PASS label=$expected pointer=${pointer.revision.value} digest=${digest.value} member=${id.unit.value}:${id.revision.value}")

  private def merge(root: Path): Unit =
    val store = right(LocalEstimateStore.open(root))
    val (_, observed) = right(store.current()).getOrElse(throw new IllegalStateException("missing pointer before explicit merge"))
    val a = identity("compatible-a")
    val b = identity("compatible-b")
    val refs = Vector(a, b).map(id => id.unit -> UnitOutcome.Published(pinned(store, id))).toMap
    val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000841"), model, refs)
    val merged = right(store.publishCollection(collection))
    right(store.discover(merged, Some(observed)))
    println(s"MERGE_PASS pointer=${merged.revision.value} observed=${observed.value} members=${refs.keys.toVector.map(_.value).sorted.mkString(",")}")

  private def check(root: Path, expected: String): Unit =
    val store = right(LocalEstimateStore.open(root))
    val (pointer, digest) = right(store.current()).getOrElse(throw new IllegalStateException("missing current pointer"))
    val collection = right(store.openCollection(pointer))
    val labels = expected.split(',').toVector.map(identity)
    if collection.units.keySet != labels.map(_.unit).toSet then throw new IllegalStateException("published units differ from expected set")
    labels.foreach: id =>
      val ref = collection.units(id.unit) match
        case UnitOutcome.Published(value) => value
        case value => throw new IllegalStateException(s"not published: $value")
      if ref.revision != id.revision then throw new IllegalStateException("unexpected selected revision")
      val source = right(store.open(ref, ReadLimits(2)))
      try
        if source.unit.catalog != catalog then throw new IllegalStateException("catalog drift")
        val values = new Array[Double](2); val validity = new Array[Byte](2)
        right(source.read(product.id, EstimateSelection(Vector(observation.id), Vector(estimand), Vector(0, 1)), values, validity))
        if !java.util.Arrays.equals(values, id.values) || !java.util.Arrays.equals(validity, Array[Byte](0, 0)) then throw new IllegalStateException("values or validity drift")
      finally right(source.close())
    println(s"CHECK_PASS pointer=${pointer.revision.value} digest=${digest.value} expected=$expected")

  def main(args: Array[String]): Unit = args.toList match
    case "publish" :: root :: label :: ready :: go :: Nil => publish(Path.of(root), label, Path.of(ready), Path.of(go))
    case "observe" :: root :: expected :: Nil => observe(Path.of(root), expected)
    case "merge" :: root :: Nil => merge(Path.of(root))
    case "check" :: root :: expected :: Nil => check(Path.of(root), expected)
    case _ => throw new IllegalArgumentException("publish root label ready go | observe root expected-label | merge root | check root labels")
