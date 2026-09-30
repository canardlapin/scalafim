package scalafim.estimates.io.hdf5

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalafim.archive.hdf5.*
import scalafim.estimates.*
import scalafim.estimates.io.*
import scalafim.image.SampleSpaces

/** Delegating fault controls retain real native writes and real resource ownership. */
private[hdf5] final class FaultArchive(delegate: Hdf5Archive) extends Hdf5Archive:
  var refuseInventory = false
  var failValidity = false
  var failSecondDataset = false
  var failClose = false
  var writes = 0
  var maxSelected = 0L
  var inventories = 0
  var closeBeforeInspect = true
  def receipt: Hdf5Receipt = delegate.receipt
  private def wrap(file: Hdf5File, writing: Boolean): Hdf5File = new Hdf5File:
    private def dataset(result: Either[Hdf5Error, Hdf5Dataset]): Either[Hdf5Error, Hdf5Dataset] = result.map: owned =>
      new Hdf5Dataset:
        def info: Hdf5DatasetInfo = owned.info
        def close(): Either[Hdf5Error, Unit] = owned.close()
        def write(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean): Either[Hdf5Error, Unit] =
          maxSelected = math.max(maxSelected, slab.elements)
          writes += 1
          if failValidity && owned.info.name.value == "validity" then Left(Hdf5Error.NativeFailure("injected", "validity after real values write"))
          else owned.write(slab, block, cancelled)
        def readInto(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean): Either[Hdf5Error, Unit] =
          maxSelected = math.max(maxSelected, slab.elements)
          owned.readInto(slab, block, cancelled)
    def create(info: Hdf5DatasetInfo): Either[Hdf5Error, Hdf5Dataset] =
      if failSecondDataset && info.name.value == "validity" then Left(Hdf5Error.NativeFailure("injected", "second dataset acquisition"))
      else dataset(file.create(info))
    def inspect(name: Hdf5DatasetName): Either[Hdf5Error, Hdf5Dataset] = dataset(file.inspect(name))
    override def verifyFlatInventory(inventory: Hdf5FlatInventory): Either[Hdf5Error, Unit] =
      inventories += 1
      if refuseInventory then Left(Hdf5Error.UnsupportedStored("injected foreign inventory")) else file.verifyFlatInventory(inventory)
    def close(): Either[Hdf5Error, Unit] =
      val closed = file.close()
      if failClose && writing then Left(Hdf5Error.NativeFailure("injected", "close after real cleanup")) else closed
  def createExclusive(path: String): Either[Hdf5Error, Hdf5File] = delegate.createExclusive(path).map(wrap(_, true))
  def openReadOnly(path: String): Either[Hdf5Error, Hdf5File] =
    closeBeforeInspect &&= delegate.receipt.openFiles == 0 && delegate.receipt.openDatasets == 0
    delegate.openReadOnly(path).map(wrap(_, false))

class Hdf5EstimateStoreSuite extends munit.FunSuite:
  import PhysicalChecks.*
  private val f = PhysicalFixture
  private def selection = EstimateSelection(Vector(f.observations.head.id), Vector(f.ids.head), Vector(0))
  private def noPublication(backend: Hdf5EstimateStore): Unit =
    assert(!Files.exists(backend.root.resolve(s"units/${f.unit.revision.value}/estimates.json")))
    assert(!Files.exists(backend.root.resolve("current.json")))
    val receipt = backend.archive.receipt
    assertEquals(receipt.openFiles, 0)
    assertEquals(receipt.openDatasets, 0)
    assertEquals(receipt.ownedIds, 0)
    assertEquals(receipt.providerOpenIds, Some(0))
  private def hash(path: Path): String = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path)).map(b => f"${b & 255}%02x").mkString

  test("JVM facade and store require explicit bounded capability before touching local root"):
    val root = directory("missing").resolve("absent")
    assert(Hdf5EstimateBackend.open(root.toString, null).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(!Files.exists(root))
    assert(Hdf5EstimateBackend.open("bad\u0000path", archive).left.toOption.exists(_.isInstanceOf[EstimateError.Invalid]))
    val wrong = native(JvmHdf5JniAdapter.open())
    assert(Hdf5EstimateStore.open(root, wrong).isLeft)
    assert(!Files.exists(root))

  test("caller buffer, sample, numeric, Float32 conversion, coverage cap and pair failures precede native work"):
    val backend = store("prenative")
    val sink = get(backend.newSink(f.unit, Set(f.shared.id)))
    val before = backend.archive.receipt
    assert(sink.write(f.effect.id, selection, Array.emptyDoubleArray, Array(0.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection, null, Array(0.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection.copy(samples = Vector(Int.MaxValue)), Array(1.0), Array(0.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection, Array(0.1), Array(0.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection, Array(Double.NaN), Array(0.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection, Array(1.0), Array(127.toByte)).isLeft)
    assert(sink.write(f.effect.id, selection.copy(samples = Vector(1)), Array(1.0), Array(0.toByte)).isLeft)
    assert(sink.writeCovariance(f.shared.id, CovarianceSelection(selection.observations, Vector(f.pairs.head), Vector(0)), Array(1.0), Array(0.toByte)).isLeft)
    assert(sink.writeCovariance(f.absolute.id, CovarianceSelection(selection.observations, Vector(EstimandPair(f.ids(1), f.ids(0))), Vector(0)), Array(1.0), Array(0.toByte)).isLeft)
    assert(sink.writeSharedCovariance(f.shared.id, SharedCovarianceSelection(selection.observations, Vector(f.pairs.head)), Array(-1.0), Array(0.toByte)).isLeft)
    assert(sink.seal().isLeft)
    assertEquals(backend.archive.receipt.attempts, before.attempts)
    get(sink.abort())
    get(sink.abort())
    noPublication(backend)
    val tiny = Hdf5EstimateLimits(maximumCoverageBytes = 1)
    val capped = get(Hdf5EstimateStore.open(directory("coverage-cap"), native(JvmHdf5JniAdapter.open(tiny.archive)), tiny))
    val attempts = capped.archive.receipt.attempts
    assert(capped.newSink(f.unit, Set(f.shared.id)).isLeft)
    assertEquals(capped.archive.receipt.attempts, attempts)
    noPublication(capped)

  test("one active product, duplicate delivery and late write after abort preserve ownership"):
    val backend = store("active")
    val sink = get(backend.newSink(f.unit, Set(f.shared.id)))
    get(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)))
    val before = backend.archive.receipt.payloadCalls
    assert(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assert(sink.write(f.scale.id, selection, Array(2.0), Array(0.toByte)).isLeft)
    assertEquals(backend.archive.receipt.payloadCalls, before)
    assertEquals(backend.archive.receipt.openFiles, 1)
    assertEquals(backend.archive.receipt.openDatasets, 2)
    get(sink.abort())
    assert(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)).isLeft)
    noPublication(backend)

  test("partial validity write and second acquisition failure close real native resources without publication"):
    for acquisition <- Vector(false, true) do
      val controlled = new FaultArchive(archive)
      controlled.failValidity = !acquisition
      controlled.failSecondDataset = acquisition
      val backend = store("partial", controlled)
      val sink = get(backend.newSink(f.scalarUnit))
      val before = controlled.receipt.payloadCalls
      assert(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)).isLeft)
      if !acquisition then assertEquals(controlled.receipt.payloadCalls, before + 1)
      else assertEquals(controlled.receipt.payloadCalls, before)
      assert(sink.seal().isLeft)
      get(sink.abort())
      noPublication(backend)

  test("read-only inventory refusal and close failure stop before catalog and close before every inspection"):
    for closeFailure <- Vector(false, true) do
      val controlled = new FaultArchive(archive)
      controlled.failClose = closeFailure
      val backend = store("reinspect", controlled)
      val sink = get(backend.newSink(f.scalarUnit))
      if closeFailure then
        intercept[AssertionError](deliver(sink, f.scalarUnit))
      else
        deliver(sink, f.scalarUnit)
        controlled.refuseInventory = true
        val payload = controlled.receipt.payloadCalls
        assert(sink.seal().isLeft)
        assertEquals(controlled.receipt.payloadCalls, payload)
      assert(controlled.closeBeforeInspect)
      noPublication(backend)

  test("cancellation before staging, during slabs, seal and reentrant late callbacks dispose resources"):
    val cancelledBackend = store("cancel-pre")
    val before = cancelledBackend.archive.receipt.attempts
    assertEquals(cancelledBackend.newSink(f.scalarUnit, cancelled = () => true), Left(EstimateError.Cancelled))
    assertEquals(cancelledBackend.archive.receipt.attempts, before)
    noPublication(cancelledBackend)
    for mode <- Vector("slab", "seal", "late", "throw") do
      val backend = store("cancel-" + mode)
      var calls = 0
      var armed = false
      var sink: SharedCovarianceSink = null
      val callback = () =>
        calls += 1
        if armed && mode == "late" then sink.abort()
        if armed && mode == "throw" then throw new IllegalStateException("callback failure")
        armed && (mode == "seal" || (mode == "slab" && calls >= 4))
      sink = get(backend.newSink(f.scalarUnit, cancelled = callback))
      if mode == "seal" then
        deliver(sink, f.scalarUnit)
        armed = true
        assert(sink.seal().isLeft)
      else
        armed = true
        assert(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)).isLeft)
      get(sink.abort())
      noPublication(backend)

  test("seal callback abort returning false prevents every publication and refuses late operations"):
    val backend = store("seal-abort-false")
    var armed = false
    var aborts = 0
    var sink: SharedCovarianceSink = null
    val callback = () =>
      if armed then
        get(sink.abort())
        aborts += 1
        assertEquals(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)), Left(EstimateError.Closed))
        assertEquals(sink.seal(), Left(EstimateError.Closed))
      false
    sink = get(backend.newSink(f.scalarUnit, cancelled = callback))
    deliver(sink, f.scalarUnit)
    armed = true
    assertEquals(sink.seal(), Left(EstimateError.Cancelled))
    assertEquals(aborts, 1)
    noPublication(backend)
    assert(!Files.exists(backend.root.resolve(s"units/${f.scalarUnit.revision.value}/product-0.h5")))
    val staged = Files.list(backend.root.resolve(".staging"))
    try assertEquals(staged.count(), 0L)
    finally staged.close()
    get(sink.abort())
    get(sink.abort())
    assertEquals(sink.write(f.effect.id, selection, Array(-8.0), Array(0.toByte)), Left(EstimateError.Closed))
    assertEquals(sink.seal(), Left(EstimateError.Cancelled))
    assertEquals(aborts, 1)
    noPublication(backend)

  test("same-handle seal retry, duplicate transaction refusal, no-clobber and stale CAS leave generation fixed"):
    val backend = store("immutable")
    def publish(unit: EstimateUnit): PinnedUnit =
      val sink = get(backend.newSink(unit, Set(f.shared.id)))
      deliver(sink, unit)
      val pinned = get(sink.seal())
      get(sink.abort())
      assertEquals(get(sink.seal()), pinned)
      pinned
    val first = publish(f.unit)
    val digest = hash(backend.root.resolve(first.manifest.path))
    val rows = get(backend.metadata(first))._2
    val physical = rows.map(_.container).distinct.map(r => r.path -> hash(backend.root.resolve(r.path))).toMap
    val duplicate = get(backend.newSink(f.unit, Set(f.shared.id)))
    deliver(duplicate)
    // Byte-identical staged containers may be reused; provider object timestamps
    // can make a semantic retry differ. A mismatch MUST remain a conflict.
    duplicate.seal() match
      case Right(ref) => assertEquals(ref, first)
      case Left(error) => assert(error.isInstanceOf[EstimateError.Conflict])
    assertEquals(backend.archive.receipt.providerOpenIds, Some(0))
    assertEquals(hash(backend.root.resolve(first.manifest.path)), digest)
    val changed = get(backend.newSink(f.scalarUnit))
    for observation <- f.effect.observations; target <- f.ids; sample <- 0 until 5 do
      val row = f.effect.observations.indexOf(observation)
      val column = f.ids.indexOf(target)
      get(changed.write(f.effect.id, EstimateSelection(Vector(observation), Vector(target), Vector(sample)),
        Array(f.value(f.effect.id, row, column, sample) + 1), Array(f.code(row, column, sample))))
    assert(changed.seal().left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    physical.foreach((path, expected) => assertEquals(hash(backend.root.resolve(path)), expected))
    val a = EstimateCollection(f.dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000210"), f.catalog.model, Map(f.unit.unit -> UnitOutcome.Published(first)))
    val b = a.copy(revision = CollectionRevisionId("00000000-0000-4000-8000-000000000211"))
    val pa = get(backend.publishCollection(a))
    val pb = get(backend.publishCollection(b))
    get(backend.discover(pa, None))
    val pointer = get(backend.current()).get
    assert(backend.discover(pb, None).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assertEquals(get(backend.current()).get, pointer)
    assertEquals(get(backend.openCollection(pa)), a)
    assertEquals(backend.archive.receipt.providerOpenIds, Some(0))

  test("reader early buffers, cancellation and reentrant close do not publish scratch"):
    val backend = store("reader-control")
    val pinned = external(backend)
    assert(backend.open(pinned, ReadLimits(128, maximumStagingBytes = 1)).isLeft)
    for mode <- Vector("buffer", "cancel", "late", "throw") do
      val source = get(backend.open(pinned, ReadLimits(128)))
      val before = backend.archive.receipt.payloadCalls
      val callback = () =>
        if mode == "late" then source.close()
        if mode == "throw" then throw new IllegalStateException("reader callback")
        mode == "cancel"
      val result = source.read(f.effect.id, selection, if mode == "buffer" then Array.emptyDoubleArray else Array(99.0), Array(99.toByte), callback)
      assert(result.isLeft)
      assertEquals(backend.archive.receipt.payloadCalls, before)
      get(source.close())
      assertEquals(backend.archive.receipt.providerOpenIds, Some(0))

  test("compact declared observation invariance is checked with bounded row slabs"):
    val backend = store("invariance")
    val unit = f.unit.copy(covariance = f.unit.covariance.map(c => if c.product == f.shared.id then c.copy(invariantObservations = true) else c))
    val sink = get(backend.newSink(unit, Set(f.shared.id)))
    deliver(sink, unit)
    assert(sink.seal().left.toOption.exists(_.message.contains("observation invariance")))
    noPublication(backend)

  test("65536-cell slabs plus partial edge stay bounded and every native scope closes"):
    val controlled = new FaultArchive(archive)
    val backend = store("resource", controlled)
    val descriptor = f.effect.copy(observations = Vector(f.observations.head.id), targets = ProductTargets.Scalar(Vector(f.ids.head)), precision = NumericPrecision.Float64)
    val unit = f.scalarUnit.copy(domain = EstimateDomain.make(SampleSpaces(Vector(65539, 1, 1)), (0 until 65539).toVector, "scanner").toOption.get,
      products = Vector(descriptor))
    val sink = get(backend.newSink(unit))
    for first <- Vector(65536, 0) do
      val count = if first == 0 then 65536 else 3
      val samples = (first until first + count).toVector
      val values = samples.map(i => i * 0.125 - 17.25).toArray
      get(sink.write(descriptor.id, EstimateSelection(descriptor.observations, Vector(f.ids.head), samples), values, Array.fill[Byte](count)(0)))
    val pinned = get(sink.seal())
    val source = get(backend.open(pinned, ReadLimits(65536)))
    for first <- Vector(0, 65536) do
      val count = if first == 0 then 65536 else 3
      val selection = EstimateSelection(descriptor.observations, Vector(f.ids.head), (first until first + count).toVector)
      val values = new Array[Double](count)
      val codes = new Array[Byte](count)
      get(source.read(descriptor.id, selection, values, codes))
      for i <- values.indices do assertEqualsDouble(values(i), (first + i) * 0.125 - 17.25, 0.0)
    get(source.close())
    val receipt = controlled.receipt
    assertEquals(controlled.maxSelected, 65536L)
    assert(receipt.peakOwnedIds <= limits.archive.maxNativeIds)
    assert(receipt.metadataObservedPeakBytes <= limits.archive.metadataCacheBytes)
    assertEquals(receipt.providerOpenIds, Some(0))
    assertEquals(receipt.ownedIds, 0)
    assertEquals(receipt.openDatasets, 0)
    println(s"PHYSICAL_BOUNDED_RESOURCE_RECEIPT $receipt maxSelected=${controlled.maxSelected} coverageBytes=${get(Hdf5EstimateLayout.derive(unit, descriptor.id, false, limits)).coverageBytes} heapLimit=67108864 nativePeak=unmeasured")

  test("two stores and repeated reader lifetimes retain zero native IDs"):
    val first = store("lifetime-a")
    val second = store("lifetime-b")
    val one = external(first)
    val two = external(second)
    def fdCount: Long = java.lang.management.ManagementFactory.getOperatingSystemMXBean match
      case unix: com.sun.management.UnixOperatingSystemMXBean => unix.getOpenFileDescriptorCount
      case _ => -1L
    val beforeFd = fdCount
    for i <- 0 until 20 do
      val backend = if i % 2 == 0 then first else second
      val source = get(backend.open(if i % 2 == 0 then one else two, ReadLimits(16)))
      val values = Array(0.0)
      get(source.read(f.effect.id, selection, values, Array(0.toByte)))
      get(source.close())
      assertEquals(backend.archive.receipt.providerOpenIds, Some(0))
      assertEquals(backend.archive.receipt.ownedIds, 0)

    val afterFd = fdCount
    if beforeFd >= 0 then assert(afterFd <= beforeFd + 2, s"descriptor growth: $beforeFd -> $afterFd")
    println(s"PHYSICAL_LIFETIME_PASS loops=20 beforeFD=$beforeFd afterFD=$afterFd finalIDs=${first.archive.receipt.providerOpenIds}")

  test("literal external foreign links and incorrect dtype, shape, chunks and filters fail before payload"):
    for mode <- Vector("write-extra", "write-group", "write-soft", "write-dtype", "write-shape", "write-chunks", "write-filter") do
      val backend = store("malformed-" + mode)
      val pinned = external(backend, mode)
      val before = backend.archive.receipt.payloadCalls
      assert(backend.open(pinned, ReadLimits(128)).isLeft, mode)
      assertEquals(backend.archive.receipt.payloadCalls, before)
      assertEquals(backend.archive.receipt.providerOpenIds, Some(0))
      assert(!Files.exists(backend.root.resolve("current.json")))

  test("literal external unknown validity, support mismatch and NaN valid value reject read destination"):
    for mode <- Vector("write-code", "write-support", "write-nan") do
      val backend = store("payload-" + mode)
      val pinned = external(backend, mode)
      val source = get(backend.open(pinned, ReadLimits(128)))
      val selected = selection.copy(samples = Vector(0,1))
      assert(source.read(f.effect.id, selected, Array(99.0,99.0), Array(99.toByte,99.toByte)).isLeft, mode)
      get(source.close())
      assertEquals(backend.archive.receipt.providerOpenIds, Some(0))

  test("evidence refusal, absent estimability bytes and source buffer cap precede staging"):
    val backend = store("admission")
    val before = backend.archive.receipt.attempts
    val evidence = InferenceEvidence(Vector.empty, Vector(InferenceStatusScope.Fit(f.observations.head.id)))
    assert(backend.newSink(f.unit.copy(inferenceEvidence = Some(evidence))).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val missing = FileReference("missing-design.bin", scalafim.archive.ContentDigest.unsafeSha256("a" * 64), 1)
    val declared = f.unit.copy(estimability = EstimabilityEvidence.Design(Vector(ColumnId("column")), missing))
    assert(backend.newSink(declared).isLeft)
    assertEquals(backend.archive.receipt.attempts, before)
    noPublication(backend)

  test("HDF-only reader and collection publication refuse a real NIfTI unit before native payload"):
    val backend = store("nifti-membership")
    val sink = get(backend.local.newSink(f.scalarUnit, 128))
    for observation <- f.effect.observations; target <- f.ids do
      val row = f.effect.observations.indexOf(observation)
      val column = f.ids.indexOf(target)
      val samples = (0 until 5).toVector
      get(sink.write(f.effect.id, EstimateSelection(Vector(observation), Vector(target), samples),
        samples.map(sample => f.value(f.effect.id, row, column, sample)).toArray,
        samples.map(sample => f.code(row, column, sample)).toArray))
    val pinned = get(sink.seal())
    val attempts = backend.archive.receipt.attempts
    assert(backend.open(pinned, ReadLimits(128)).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val collection = EstimateCollection(f.dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000220"), f.catalog.model,
      Map(f.unit.unit -> UnitOutcome.Published(pinned)))
    assert(backend.publishCollection(collection).isLeft)
    assertEquals(backend.archive.receipt.attempts, attempts)
    assert(!Files.exists(backend.root.resolve("collections")))
    assert(!Files.exists(backend.root.resolve("current.json")))
