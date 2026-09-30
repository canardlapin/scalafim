package scalafim.estimates.io

import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scalafim.estimates.*

class LocalEstimateCollectionsSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)
  private def store(): LocalEstimateStore = right(LocalEstimateStore.open(Files.createTempDirectory("explicit-collection-")))
  private val f = Hdf5MetadataFixture
  private val collectionRevision = CollectionRevisionId("00000000-0000-4000-8000-000000000151")
  private val fakeRef = PinnedUnit(f.unit.unit, f.unit.revision, f.catalogRef.copy(path = "opaque-manifest.json"))
  private def collection(reference: PinnedUnit = fakeRef): EstimateCollection =
    EstimateCollection(f.unit.dataset, collectionRevision, f.unit.catalog.model, Map(reference.unit -> UnitOutcome.Published(reference)))

  test("explicit callback refusal precedes collection writes and leaves an existing pointer unchanged"):
    val local = store()
    var calls = Vector.empty[PinnedUnit]
    var refused = false
    val error = EstimateError.Unsupported("fake inspector refuses this backend")
    val helper = new LocalEstimateCollections(local, ref =>
      calls :+= ref
      if refused then Left(error) else Right(f.unit))
    val pinned = right(helper.publish(collection()))
    right(helper.discover(pinned, None))
    val before = right(helper.current()).get
    val pointer = Files.readAllBytes(local.root.resolve("current.json")).toVector
    refused = true
    assertEquals(helper.publish(collection().copy(revision = CollectionRevisionId("00000000-0000-4000-8000-000000000152"))), Left(error))
    assertEquals(helper.open(pinned), Left(error))
    assertEquals(helper.current(), Left(error))
    assertEquals(helper.discover(pinned, Some(before._2)), Left(error))
    assert(calls.nonEmpty && calls.forall(_ == fakeRef))
    assert(!Files.exists(local.root.resolve("collections/00000000-0000-4000-8000-000000000152")))
    assertEquals(Files.readAllBytes(local.root.resolve("current.json")).toVector, pointer)

  test("helper retains failed/missing membership, immutable catalogs, unit pins and metadata budgets"):
    val local = store()
    val secondId = UnitId("00000000-0000-4000-8000-000000000153")
    val secondRevision = UnitRevisionId("00000000-0000-4000-8000-000000000154")
    val second = fakeRef.copy(unit = secondId, revision = secondRevision)
    var units = Map(fakeRef -> f.unit, second -> f.unit.copy(unit = secondId, revision = secondRevision))
    val helper = new LocalEstimateCollections(local, ref => units.get(ref).toRight(EstimateError.Integrity("missing member")))
    val failedId = UnitId("00000000-0000-4000-8000-000000000155")
    val missingId = UnitId("00000000-0000-4000-8000-000000000156")
    val declared = collection().copy(units = collection().units ++ Map(secondId -> UnitOutcome.Published(second),
      failedId -> UnitOutcome.Failed("execution failed"), missingId -> UnitOutcome.Missing("not run")))
    val pinned = right(helper.publish(declared))
    assertEquals(helper.publish(declared), Right(pinned))
    assertEquals(helper.open(pinned), Right(declared))
    assert(!right(helper.open(pinned)).unitsPublished)
    assert(helper.open(pinned.copy(revision = CollectionRevisionId("00000000-0000-4000-8000-000000000157"))).isLeft)
    assert(helper.open(pinned.copy(manifest = pinned.manifest.copy(bytes = 16L * 1024 * 1024 + 1)))
      .left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    units = units.updated(second, units(second).copy(catalog = f.unit.catalog.copy(entries = f.unit.catalog.entries.map(_.copy(normalization = "different")))))
    assert(helper.publish(declared).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    units = units.updated(second, f.unit) // a callback cannot defeat pinned identity
    assert(helper.publish(declared).left.toOption.exists(_.isInstanceOf[EstimateError.Integrity]))
    units = units.removed(second)
    assert(helper.open(pinned).isLeft)
    val mismatchedDataset = collection().copy(dataset = DatasetId("00000000-0000-4000-8000-000000000158"))
    assert(helper.publish(mismatchedDataset).left.toOption.exists(_.isInstanceOf[EstimateError.Invalid]))
    val mismatchedModel = collection().copy(model = ModelRevisionId("00000000-0000-4000-8000-000000000159"))
    assert(helper.publish(mismatchedModel).left.toOption.exists(_.isInstanceOf[EstimateError.Invalid]))

  test("explicit metadata-only HDF inspector verifies closed container identities and shares CAS lifecycle"):
    val local = store()
    // These are opaque leaves; no native HDF5 dataset is opened or qualified.
    val records = f.records.groupBy(_.product).toVector.flatMap: (_, rows) =>
      val leaf = right(local.objects.write(rows.head.container.path)(_.write(Array[Byte](7, 11, 5))).left.map(local.fromStore))
      rows.map(_.copy(container = local.reference(leaf)))
    val ref = right(local.publishHdf5Unit(f.unit, records))
    def inspect(owner: LocalEstimateStore)(pinned: PinnedUnit): Either[EstimateError, EstimateUnit] =
      owner.inspectWithRepresentations(pinned).flatMap: (unit, inventory, status) =>
        if status.nonEmpty || !inventory.forall(_.isInstanceOf[EstimateRepresentation.Hdf5]) then
          Left(EstimateError.Unsupported("explicit inspector requires evidence-free HDF5 metadata"))
        else inventory.collect { case EstimateRepresentation.Hdf5(record) => record.container }.distinct
          .foldLeft[Either[EstimateError, Unit]](Right(()))((previous, leaf) =>
            previous.flatMap(_ => owner.objects.verify(owner.verified(leaf)).left.map(owner.fromStore))).map(_ => unit)
    val helper = new LocalEstimateCollections(local, inspect(local))
    val pinned = right(helper.publish(collection(ref)))
    val ready = new CountDownLatch(2)
    val start = new CountDownLatch(1)
    val results = new Array[Either[EstimateError, Unit]](2)
    val threads = Vector.tabulate(2): index =>
      new Thread(() =>
        val other = right(LocalEstimateStore.open(local.root))
        val contender = new LocalEstimateCollections(other, inspect(other))
        ready.countDown()
        results(index) = if start.await(60, TimeUnit.SECONDS) then contender.discover(pinned, None)
          else Left(EstimateError.Io("CAS start barrier timed out")))
    threads.foreach(_.start())
    assert(ready.await(60, TimeUnit.SECONDS))
    start.countDown()
    threads.foreach(_.join(60000))
    assert(threads.forall(!_.isAlive))
    assert(results.forall(_ != null))
    assertEquals(results.count(_.isRight), 1)
    assertEquals(results.count(_.isLeft), 1)
    assert(results.find(_.isLeft).get.left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    val current = right(helper.current()).get
    assertEquals(current._1, pinned)
    assert(helper.discover(pinned, None).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    right(helper.discover(pinned, Some(current._2)))
    assert(local.current().left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val pointer = Files.readAllBytes(local.root.resolve("current.json")).toVector
    Files.write(local.root.resolve(records.head.container.path), Array[Byte](0))
    assert(helper.open(pinned).isLeft)
    assert(helper.current().isLeft)
    assert(helper.discover(pinned, Some(current._2)).isLeft)
    assertEquals(Files.readAllBytes(local.root.resolve("current.json")).toVector, pointer)

  test("default helper preserves Core3 status digest verification on publish/open/current/discover"):
    val local = store()
    val prefix = "units/00000000-0000-4000-8000-000000000104"
    val names = Vector("estimands.json", "estimands.tsv", "observations.tsv", "estimates.json", "status.nii")
    names.foreach: name =>
      val path = s"$prefix/$name"
      val input = Option(getClass.getResourceAsStream(s"/estimate-golden/inference-evidence/$path")).getOrElse(fail(path))
      val bytes = try input.readAllBytes() finally input.close()
      right(local.objects.write(path)(_.write(bytes)).left.map(local.fromStore))
    val manifest = local.reference(right(local.objects.inspect(s"$prefix/estimates.json").left.map(local.fromStore)))
    val unit = right(EstimateMetadata.readUnit(Files.readString(local.root.resolve(manifest.path), UTF_8),
      right(EstimateMetadata.readCatalog(Files.readString(local.root.resolve(s"$prefix/estimands.json"), UTF_8)))))
    val pinnedUnit = PinnedUnit(unit.unit, unit.revision, manifest)
    val declared = EstimateCollection(unit.dataset, collectionRevision, unit.catalog.model, Map(unit.unit -> UnitOutcome.Published(pinnedUnit)))
    val pinned = right(local.publishCollection(declared))
    right(local.discover(pinned, None))
    val current = right(local.current()).get
    val pointer = Files.readAllBytes(local.root.resolve("current.json")).toVector
    Files.write(local.root.resolve(s"$prefix/status.nii"), Array[Byte](99))
    assert(local.inspect(pinnedUnit).isRight) // old metadata-only inspection remains unchanged
    assert(local.publishCollection(declared).isLeft)
    assert(local.openCollection(pinned).isLeft)
    assert(local.current().isLeft)
    assert(local.discover(pinned, Some(current._2)).isLeft)
    assertEquals(Files.readAllBytes(local.root.resolve("current.json")).toVector, pointer)
