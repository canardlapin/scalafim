package scalafim.estimates.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.lang.management.ManagementFactory
import scalafim.archive.ContentDigest
import scalafim.estimates.*

class IndependentSharedCovarianceSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)
  private val fixture = CompactFixture
  private def bundle(): (LocalEstimateStore, PinnedUnit) =
    val source = Path.of(getClass.getClassLoader.getResource("estimate-golden/compact-shared/expected.json").toURI).getParent
    val root = Files.createTempDirectory("compact-literal-")
    val files = Files.walk(source)
    try files.forEach: path =>
      val target = root.resolve(source.relativize(path))
      if Files.isDirectory(path) then Files.createDirectories(target)
      else Files.copy(path, target)
    finally files.close()
    val expected = ujson.read(Files.readString(root.resolve("expected.json")))("manifest")
    val ref = PinnedUnit(fixture.unit.unit, fixture.unit.revision,
      FileReference(expected("Path").str, ContentDigest.unsafeSha256(expected("SHA256").str), expected("Bytes").num.toLong))
    (right(LocalEstimateStore.open(root)), ref)

  private def altered(store: LocalEstimateStore, ref: PinnedUnit, name: String,
      mutate: ujson.Value => Unit): PinnedUnit =
    val encoded = ujson.read(Files.readString(store.root.resolve(ref.manifest.path)))
    mutate(encoded)
    ref.copy(manifest = right(store.writeText(s"variants/$name.json", ujson.write(encoded))))

  private def sharedRecord(encoded: ujson.Value, observation: Int = 0): ujson.Value =
    encoded("Content")("Representations")(4 + observation)("Content")

  test("independent physical bundle preserves raw U, axes, support and two distinct scale operations") {
    val (store, ref) = bundle()
    val source = right(store.open(ref, ReadLimits(48)))
    try
      assertEquals(source.unit.catalog.entries.map(_.label).distinct, Vector("repeated label"))
      val observations = fixture.observations.map(_.id).reverse
      val pairs = fixture.pairs.reverse
      val samples = Vector(5, 1, 0, 3)
      val values = new Array[Double](48)
      val codes = new Array[Byte](48)
      right(source.readCovariance(fixture.covariance.id, CovarianceSelection(observations, pairs, samples), values, codes))
      var offset = 0
      for observation <- Vector(1, 0); pair <- Vector(5, 4, 3, 2, 1, 0); sample <- samples do
        assertEqualsDouble(values(offset), (if observation == 0 then fixture.firstValues else fixture.secondValues)(pair), 0.0)
        assertEquals(codes(offset), if sample == 1 then Validity.OutsideSupport.code else Validity.Valid.code)
        offset += 1
      assertEquals(source.readCovariance(fixture.covariance.id, CovarianceSelection(observations, pairs, samples), values, codes, () => true), Left(EstimateError.Cancelled))
    finally right(source.close())
    val bounded = right(store.open(ref, ReadLimits(1)))
    try
      val expected = ujson.read(Files.readString(store.root.resolve("expected.json")))
      for o <- 0 until 2; s <- 0 until 3 do
        val matrix = right(CovarianceAccess.matrix(bounded, fixture.covariance.id, fixture.observations(o).id,
          Vector(0, 3, 5)(s), fixture.ids.reverse, CovariancePolicy(3, 1e-12, 1e-12)))
        assertEqualsDouble(matrix.varianceScale, expected("scales")(o)(s).num, 0.0)
        val upper = expected("SigmaUpper")(o)(s).arr.map(_.num).toVector
        val dense = Vector(Vector(upper(0), upper(1), upper(2)), Vector(upper(1), upper(3), upper(4)), Vector(upper(2), upper(4), upper(5)))
        for i <- 0 until 3; j <- 0 until 3 do assertEqualsDouble(matrix.values(i, j), dense(2-i)(2-j), 1e-12)
      assert(CovarianceAccess.matrix(bounded, fixture.covariance.id, fixture.observations.head.id, 1, fixture.ids, CovariancePolicy(3, 1e-12, 1e-12)).isLeft)
      assert(bounded.readCovariance(fixture.covariance.id, CovarianceSelection(Vector(fixture.observations.head.id),
        Vector(fixture.pairs.head), Vector(0, 3)), new Array[Double](2), new Array[Byte](2)).isLeft)
    finally right(bounded.close())
  }

  test("cumulative pair and byte caps are independent of expanded cells and precede payload access") {
    val (store, ref) = bundle()
    val records = right(EstimateMetadata.allRepresentations(Files.readString(store.root.resolve(ref.manifest.path))))
    val bytes = records.collect { case EstimateRepresentation.SharedNormalizedUpperTriangle(value) => value.table.bytes }.sum
    val source = right(store.open(ref, ReadLimits(1, maximumSharedPairs = 12, maximumSharedTableBytes = bytes)))
    right(source.close())
    assert(store.open(ref, ReadLimits(1, maximumSharedPairs = 11)).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.open(ref, ReadLimits(1, maximumSharedTableBytes = bytes - 1)).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val missing = altered(store, ref, "missing-over-budget", encoded => sharedRecord(encoded)("table")("Path") = "missing.json")
    assert(store.open(missing, ReadLimits(1, maximumSharedPairs = 11)).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.open(missing, ReadLimits(1)).isLeft)
  }

  test("inventory, table authority, declared invariance and leaf integrity refuse repinned conflicts") {
    val (store, ref) = bundle()
    val changes: Vector[(String, ujson.Value => Unit)] = Vector(
      "duplicate" -> (v => v("Content")("Representations").arr.addOne(v("Content")("Representations")(4))),
      "wrong-observation" -> (v => sharedRecord(v)("observation") = "missing"),
      "wrong-product" -> (v => sharedRecord(v)("product") = "effect"),
      "reordered-axis" -> (v => sharedRecord(v)("estimands") = ujson.Arr("M", "a/β", "z")),
      "bad-hash" -> (v => sharedRecord(v)("table")("SHA256") = "0" * 64),
      "bad-length" -> (v => sharedRecord(v)("table")("Bytes") = 1),
      "invariant-observations" -> (v => v("Content")("covariance")(0)("invariantObservations") = true),
      "non-invariant-samples" -> (v => v("Content")("covariance")(0)("invariantSamples") = false),
      "absolute" -> (v => v("Content")("covariance")(0)("equation") = "Absolute"))
    changes.foreach: (name, mutate) =>
      assert(store.open(altered(store, ref, name, mutate), ReadLimits(1)).isLeft, name)
  }

  test("zero scale does not hide indefinite U and pair-invalid validity is never synthesized as zero") {
    val (store, ref) = bundle()
    def tableVariant(name: String, mutate: ujson.Value => Unit): PinnedUnit =
      val original = ujson.read(Files.readString(store.root.resolve(ref.manifest.path)))
      val path = sharedRecord(original)("table")("Path").str
      val table = ujson.read(Files.readString(store.root.resolve(path)))
      mutate(table)
      val leaf = right(store.writeText(s"variants/$name-table.json", ujson.write(table)))
      altered(store, ref, name, encoded => sharedRecord(encoded)("table") = ujson.Obj("Path" -> leaf.path, "SHA256" -> leaf.digest.value, "Bytes" -> leaf.bytes.toDouble))
    for (name, mutate) <- Vector[(String, ujson.Value => Unit)](
        "indefinite" -> (v => v("Pairs")(1)("Value") = 20),
        "non-estimable" -> (v => v("Pairs")(1)("Validity") = 3)) do
      val source = right(store.open(tableVariant(name, mutate), ReadLimits(1)))
      try assert(CovarianceAccess.matrix(source, fixture.covariance.id, fixture.observations.head.id,
        3, fixture.ids, CovariancePolicy(3, 1e-12, 1e-12)).isLeft, name)
      finally right(source.close())
  }

  test("invalid or inconsistent physical variance scale refuses reconstruction") {
    val (store, ref) = bundle()
    val manifest = ujson.read(Files.readString(store.root.resolve(ref.manifest.path)))
    val record = manifest("Content")("Representations")(2)("Content")
    for (name, byteOffset, replacement) <- Vector(("negative", 352, -1.0), ("nonfinite", 352, Double.NaN), ("inconsistent", 352 + 6*8, 2.5)) do
      val bytes = Files.readAllBytes(store.root.resolve(record("values")("Path").str))
      ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).putDouble(byteOffset, replacement)
      val file = right(store.objects.write(s"variants/$name.nii")(_.write(bytes)).left.map(store.fromStore))
      val pinned = altered(store, ref, name, encoded => encoded("Content")("Representations")(2)("Content")("values") =
        ujson.Obj("Path" -> file.path, "SHA256" -> file.digest.value, "Bytes" -> file.bytes.toDouble))
      val source = right(store.open(pinned, ReadLimits(1)))
      try assert(CovarianceAccess.matrix(source, fixture.covariance.id, fixture.observations.head.id,
        0, fixture.ids, CovariancePolicy(3, 1e-12, 1e-12)).isLeft, name)
      finally right(source.close())
    val codes = Files.readAllBytes(store.root.resolve(record("validity")("Path").str))
    codes(352) = Validity.MissingInput.code
    val file = right(store.objects.write("variants/missing-scale.nii")(_.write(codes)).left.map(store.fromStore))
    val pinned = altered(store, ref, "missing-scale", encoded => encoded("Content")("Representations")(2)("Content")("validity") =
      ujson.Obj("Path" -> file.path, "SHA256" -> file.digest.value, "Bytes" -> file.bytes.toDouble))
    val source = right(store.open(pinned, ReadLimits(1)))
    try assert(CovarianceAccess.matrix(source, fixture.covariance.id, fixture.observations.head.id, 0,
      fixture.ids, CovariancePolicy(3, 1e-12, 1e-12)).isLeft)
    finally right(source.close())
  }

  test("a malformed later shared leaf closes earlier NIfTI channels on every failed open") {
    val (store, ref) = bundle()
    val bad = right(store.writeText("variants/bad-table.json", "{}"))
    val original = ujson.read(Files.readString(store.root.resolve(ref.manifest.path)))
    val first = original("Content")("Representations")(0)("Content")
    val compressed = Vector("values", "validity").map: field =>
      val bytes = Files.readAllBytes(store.root.resolve(first(field)("Path").str))
      val output = new java.io.ByteArrayOutputStream()
      val gzip = new java.util.zip.GZIPOutputStream(output)
      gzip.write(bytes)
      gzip.close()
      val file = right(store.objects.write(s"variants/earlier-$field.nii.gz")(_.write(output.toByteArray)).left.map(store.fromStore))
      field -> ujson.Obj("Path" -> file.path, "SHA256" -> file.digest.value, "Bytes" -> file.bytes.toDouble)
    val pinned = altered(store, ref, "later-leaf", encoded =>
      sharedRecord(encoded, 1)("table") = ujson.Obj("Path" -> bad.path, "SHA256" -> bad.digest.value, "Bytes" -> bad.bytes.toDouble)
      compressed.foreach((field, reference) => encoded("Content")("Representations")(0)("Content")(field) = reference))
    def stages(): Set[Path] =
      val files = Files.list(Path.of(System.getProperty("java.io.tmpdir")))
      try
        import scala.jdk.CollectionConverters.*
        files.iterator().asScala.filter(_.getFileName.toString.startsWith("scalafim-estimate-")).toSet
      finally files.close()
    val beforeStages = stages()
    assert(store.open(pinned, ReadLimits(1)).isLeft)
    ManagementFactory.getOperatingSystemMXBean match
      case bean: com.sun.management.UnixOperatingSystemMXBean =>
        val before = bean.getOpenFileDescriptorCount
        for _ <- 0 until 20 do assert(store.open(pinned, ReadLimits(1)).isLeft)
        assertEquals(bean.getOpenFileDescriptorCount, before)
      case _ => fail("descriptor evidence requires the Unix JVM used by this backend")
    assertEquals(stages(), beforeStages)
    val valid = right(store.open(ref, ReadLimits(1)))
    right(valid.close())
  }

  test("32 actual NIfTI pairs coexist with separately bounded shared observations") {
    val (store, ref) = bundle()
    val original = Files.readString(store.root.resolve(ref.manifest.path))
    val records = right(EstimateMetadata.allRepresentations(original))
    val observations = Vector.tabulate(16)(i => fixture.observations.head.copy(id = ObservationId(s"row-$i"),
      participant = ParticipantId(fixture.dataset, s"participant-$i")))
    val products = fixture.products.map(_.copy(observations = observations.map(_.id)))
    val declared = fixture.unit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000098"),
      observations = observations, products = products,
      covariance = fixture.unit.covariance.map(_.copy(invariantObservations = true)))
    val physical = Vector(fixture.effect.id, fixture.scale.id).flatMap: product =>
      val first = records.collectFirst { case EstimateRepresentation.Nifti(value) if value.product == product => value }.get
      observations.map(o => EstimateRepresentation.Nifti(first.copy(observation = o.id)))
    val shared = observations.map: observation =>
      val table = ujson.read(fixture.literal)
      table("Observation") = observation.id.value
      val leaf = right(store.writeText(s"variants/${observation.id.value}-U.json", ujson.write(table)))
      EstimateRepresentation.SharedNormalizedUpperTriangle(SharedCovarianceRepresentation(fixture.covariance.id, observation.id, leaf, fixture.ids))
    val pinned = right(store.publishMixedUnit(declared, physical ++ shared, true))
    val source = right(store.open(pinned, ReadLimits(1, maximumSharedPairs = 96)))
    try
      val values = new Array[Double](1)
      val codes = new Array[Byte](1)
      right(source.readCovariance(fixture.covariance.id, CovarianceSelection(Vector(observations.last.id),
        Vector(fixture.pairs(1)), Vector(5)), values, codes))
      assertEqualsDouble(values(0), -1.0, 0.0)
    finally right(source.close())
    assert(store.open(pinned, ReadLimits(1, maximumSharedPairs = 95)).isLeft)
    val sink = right(store.newSink(declared, 1, CovarianceLayout.SharedNormalizedTable()))
    right(sink.abort())
  }

  test("observation invariance is checked across shared and NIfTI covariance arms") {
    val (store, _) = bundle()
    val coreUnit = fixture.unit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000099"))
    val sink = right(store.newSink(coreUnit, 3))
    for observation <- fixture.observations; product <- Vector(fixture.effect, fixture.scale); target <- fixture.ids do
      right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(0, 3, 5)),
        Array(2.0, 0.0, 5.0), Array.fill[Byte](3)(0)))
    for (observation, o) <- fixture.observations.zipWithIndex; (pair, p) <- fixture.pairs.zipWithIndex do
      val value = (if o == 0 then fixture.firstValues else fixture.secondValues)(p)
      right(sink.writeCovariance(fixture.covariance.id, CovarianceSelection(Vector(observation.id), Vector(pair), Vector(0, 3, 5)),
        Array.fill(3)(value), Array.fill[Byte](3)(0)))
    val coreRef = right(sink.seal())
    val nifti = right(EstimateMetadata.allRepresentations(Files.readString(store.root.resolve(coreRef.manifest.path))))
    val table = right(SharedCovarianceTable.decode(fixture.literal, 6))
    val leaf = right(store.writeText("variants/first-U.json", SharedCovarianceTable.encode(table)))
    val mixed = nifti.filterNot(r => r.product == fixture.covariance.id && r.observation == fixture.observations.head.id) :+
      EstimateRepresentation.SharedNormalizedUpperTriangle(fixture.representation.copy(table = leaf))
    val declared = coreUnit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000100"),
      covariance = coreUnit.covariance.map(_.copy(invariantObservations = true)))
    val pinned = right(store.publishMixedUnit(declared, mixed, true))
    assert(store.open(pinned, ReadLimits(1)).left.toOption.exists(_.message.contains("observation invariance")))
  }
