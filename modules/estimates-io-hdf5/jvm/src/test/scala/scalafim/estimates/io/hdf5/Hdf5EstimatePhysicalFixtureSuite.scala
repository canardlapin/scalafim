package scalafim.estimates.io.hdf5

import java.nio.file.{Files, Path}
import scala.sys.process.*
import scalafim.archive.hdf5.*
import scalafim.estimates.*
import scalafim.estimates.io.*

private[hdf5] object PhysicalChecks:
  def get[A](value: Either[EstimateError, A]): A = value.fold(e => throw new AssertionError(e.message), identity)
  def native[A](value: Either[Hdf5Error, A]): A = value.fold(e => throw new AssertionError(e.message), identity)
  val fixture = PhysicalFixture
  val limits = Hdf5EstimateLimits()
  def archive: Hdf5Archive = native(JvmHdf5JniAdapter.open(limits.archive))
  def directory(name: String): Path =
    val parent = Option(System.getProperty("scalafim.hdf5.test.dir")).map(Path.of(_)).getOrElse(Path.of(System.getProperty("java.io.tmpdir")))
    Files.createDirectories(parent)
    Files.createTempDirectory(parent, name + "-")
  def store(name: String, capability: Hdf5Archive = archive): Hdf5EstimateStore = get(Hdf5EstimateStore.open(directory(name), capability))
  def deliver(sink: SharedCovarianceSink, declared: EstimateUnit = fixture.unit): Unit =
    // Reverse every named axis and sample order; also deliver split nonzero slabs.
    for product <- declared.products do
      for observation <- product.observations.reverse; target <- (0 until product.targets.width.toInt).reverse do
        val row = product.observations.indexOf(observation)
        if sink.sharedCovarianceProducts.contains(product.id) then
          val pair = fixture.pairs(target)
          get(sink.writeSharedCovariance(product.id, SharedCovarianceSelection(Vector(observation), Vector(pair)),
            Array(fixture.value(product.id, row, target, 0)), Array(if row == 0 && target == 1 then Validity.NonEstimable.code else Validity.Valid.code)))
        else
          for samples <- Vector(Vector(4, 2), Vector(0, 1, 3)) do
            val values = samples.map(s => fixture.value(product.id, row, target, s)).toArray
            val codes = samples.map(s => fixture.code(row, target, s)).toArray
            if product.kind == ProductKind.Covariance then
              get(sink.writeCovariance(product.id, CovarianceSelection(Vector(observation), Vector(fixture.pairs(target)), samples), values, codes))
            else get(sink.write(product.id, EstimateSelection(Vector(observation), Vector(fixture.ids(target)), samples), values, codes))
  def oracle(mode: String, root: Path): Unit =
    val resources = "scalafim/estimates/io/hdf5/"
    val script = Path.of(getClass.getClassLoader.getResource(resources + "h5py_oracle.py").toURI)
    val expected = Path.of(getClass.getClassLoader.getResource(resources + "h5py_oracle_expected.json").toURI)
    val command = Vector("/opt/homebrew/bin/python3.12", script.toString, mode, expected.toString, root.toString)
    val output = new StringBuilder
    val code = Process(command).!(ProcessLogger(line => output.append(line).append('\n'), line => output.append(line).append('\n')))
    Files.writeString(root.resolve("h5py-" + mode + ".log"), output.toString)
    Files.writeString(root.resolve("h5py-" + mode + ".meta.json"), ujson.write(ujson.Obj("argv" -> command, "exit_code" -> code)))
    println(output.toString)
    require(code == 0, s"independent h5py $mode failed: $output")
  def external(store: Hdf5EstimateStore, mode: String = "write"): PinnedUnit =
    oracle(mode, store.root)
    val records = fixture.products.flatMap: product =>
      val file = get(store.local.objects.inspect(product.id.value + ".h5").left.map(store.local.fromStore))
      product.observations.map(o => Hdf5Representation(product.id, o, store.local.reference(file),
        if product.id == fixture.shared.id then Hdf5PayloadLayout.SharedNormalizedUpperTriangle else Hdf5PayloadLayout.PerSample))
    get(store.local.publishHdf5Unit(fixture.unit, records))
  def checkRead(source: EstimateSource)(compare: (Double, Double) => Unit): Unit =
    for product <- fixture.products do
      val observations = product.observations.reverse
      val targets = (0 until product.targets.width.toInt).reverse.toVector
      val samples = Vector(4, 2, 1, 0, 3)
      val values = new Array[Double](observations.size * targets.size * samples.size)
      val codes = new Array[Byte](values.length)
      if product.kind == ProductKind.Covariance then
        get(source.readCovariance(product.id, CovarianceSelection(observations, targets.map(fixture.pairs), samples), values, codes))
      else get(source.read(product.id, EstimateSelection(observations, targets.map(fixture.ids), samples), values, codes))
      var i = 0
      for observation <- observations; target <- targets; sample <- samples do
        val row = product.observations.indexOf(observation)
        val expectedCode = if product.id == fixture.shared.id then
          if sample == 1 then Validity.OutsideSupport.code else if row == 0 && target == 1 then Validity.NonEstimable.code else Validity.Valid.code
        else fixture.code(row, target, sample)
        val expectedValue = fixture.value(product.id, row, target, sample)
        if product.id == fixture.shared.id && sample == 1 then assert(values(i).isNaN)
        else compare(values(i), expectedValue)
        assert(codes(i) == expectedCode)
        i += 1

class Hdf5EstimatePhysicalFixtureSuite extends munit.FunSuite:
  import PhysicalChecks.*
  test("Scala physical files pass full independent literal h5py checks and reopened ordered selection readback"):
    val backend = store("scala-literal")
    val sink = get(backend.newSink(fixture.unit, Set(fixture.shared.id)))
    deliver(sink)
    val pinned = get(sink.seal())
    assertEquals(sink.seal(), Right(pinned))
    val rows = get(backend.metadata(pinned))._2
    val oracleRoot = directory("scala-h5py")
    for product <- fixture.products do
      Files.copy(backend.root.resolve(rows.find(_.product == product.id).get.container.path), oracleRoot.resolve(product.id.value + ".h5"))
    oracle("check", oracleRoot)
    val source = get(backend.open(pinned, ReadLimits(128)))
    checkRead(source)((actual, expected) => assertEqualsDouble(actual, expected, 0.0))
    get(source.close())
    get(source.close())
    assert(source.read(fixture.effect.id, EstimateSelection(Vector(fixture.observations.head.id), Vector(fixture.ids.head), Vector(0)), Array(0.0), Array(0.toByte)).isLeft)
    val receipt = backend.archive.receipt
    assertEquals(receipt.openFiles, 0)
    assertEquals(receipt.openDatasets, 0)
    assertEquals(receipt.ownedIds, 0)
    assertEquals(receipt.providerOpenIds, Some(0))
    println(s"PHYSICAL_LITERAL_RESOURCE_RECEIPT $receipt coverageBytes=${get(Hdf5EstimateLayout.plans(fixture.unit, Set(fixture.shared.id), limits)).map(_.coverageBytes).sum}")

  test("independent external h5py writer files are read by Scala across every literal cell and reverse axis"):
    val backend = store("external-literal")
    val pinned = external(backend)
    val source = get(backend.open(pinned, ReadLimits(128)))
    checkRead(source)((actual, expected) => assertEqualsDouble(actual, expected, 0.0))
    get(source.close())
    assert(LocalEstimateStore.open(backend.root).toOption.get.open(pinned, ReadLimits(128)).isLeft)
    println("PHYSICAL_EXTERNAL_LITERAL_PASS products=4 storedCells=76 logicalReadCells=100")
