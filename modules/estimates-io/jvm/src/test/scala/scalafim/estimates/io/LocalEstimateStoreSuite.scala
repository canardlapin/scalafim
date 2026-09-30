package scalafim.estimates.io

import java.nio.file.Files
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scalafim.estimates.*
import scalafim.image.SampleSpaces

class LocalEstimateStoreSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)
  private val dataset = DatasetId("00000000-0000-4000-8000-000000000001")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000002")
  private val id = UnitId("00000000-0000-4000-8000-000000000003")
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000004")
  private val a = EstimandId("A")
  private val b = EstimandId("B")
  private val obs = Observation(ObservationId("participant-01"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
  private val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(obs.id), ProductTargets.Scalar(Vector(a, b)), PoolingScope.Run, "signal")
  private val domain = right(EstimateDomain.make(SampleSpaces(Vector(2, 3, 4)), Vector(0, 3, 7, 23), "scanner"))
  private def unit = EstimateUnit(dataset, id, revision, EstimandCatalog(model, Vector(a, b).map(e =>
    EstimandDefinition(e, "same display label", EstimandKind.Coefficient, "signal", "unit", e.value))),
    domain, Vector(obs), Vector.empty, Vector(product), Map(product.id -> ProductOutcome.Available(product.id)),
    EstimabilityEvidence.Unknown("imported test data"), EstimateProvenance("fixture", "1", "execution-1",
      ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"),
      ScientificFact.Unknown("imported"), Vector.empty, Vector.empty))

  private def write(store: LocalEstimateStore): PinnedUnit =
    val sink = right(store.newSink(unit, 8))
    assert(sink.seal().isLeft)
    val selection = EstimateSelection(Vector(obs.id), Vector(b, a), Vector(23, 0, 7, 3))
    val values = Array(123.0, 100.0, 107.0, 103.0, 23.0, 0.0, 7.0, 3.0)
    val validity = Array[Byte](0, 0, 3, 0, 0, 0, 0, 0)
    right(sink.write(product.id, selection, values, validity))
    assert(sink.write(product.id, selection, values, validity).isLeft)
    val ref = right(sink.seal())
    assertEquals(sink.seal(), Right(ref))
    ref

  test("opt-in mixed sink has pair-only coverage, refuses duplicates and preserves exact retry semantics") {
    val fixture = CompactFixture
    val root = Files.createTempDirectory("compact-sink-")
    val store = right(LocalEstimateStore.open(root))
    def deliver(change: Boolean = false): Either[EstimateError, PinnedUnit] =
      val sink = right(store.newSink(fixture.unit, 3, CovarianceLayout.SharedNormalizedTable()))
      val shared = sink.asInstanceOf[SharedCovarianceSink]
      assert(sink.seal().isLeft)
      assertEquals(sink.asInstanceOf[NiftiEstimateSink].sharedOutputs.map(_.coverage.length).sum, 12)
      for observation <- fixture.observations; product <- Vector(fixture.effect, fixture.scale); target <- fixture.ids do
        right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(0, 3, 5)),
          Array(2.0, 0.0, 5.0), Array.fill[Byte](3)(0)))
      for observation <- fixture.observations.zipWithIndex; pair <- fixture.pairs.zipWithIndex do
        val values = if observation._2 == 0 then fixture.firstValues else fixture.secondValues
        val selection = SharedCovarianceSelection(Vector(observation._1.id), Vector(pair._1))
        val value = values(pair._2) + (if change && observation._2 == 0 && pair._2 == 0 then 1.0 else 0.0)
        right(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(value), Array[Byte](0)))
        assert(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(value), Array[Byte](0)).isLeft)
      assert(sink.writeCovariance(fixture.covariance.id, CovarianceSelection(Vector(fixture.observations.head.id),
        Vector(fixture.pairs.head), Vector(0)), Array(4.0), Array[Byte](0)).isLeft)
      sink.seal()
    val first = right(deliver())
    assertEquals(right(deliver()), first)
    assert(deliver(true).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assertEquals(right(EstimateMetadata.schema(Files.readString(root.resolve(first.manifest.path)), "unit")), EstimateMetadata.compactSchema)
    val stages = Files.walk(root.resolve(".staging"))
    try assert(!stages.anyMatch(path => Files.isRegularFile(path)))
    finally stages.close()
    val reader = right(store.open(first, ReadLimits(1)))
    right(reader.close())
  }

  test("compact caps and descriptors refuse before allocation and partial abort removes owned stages") {
    val fixture = CompactFixture
    val root = Files.createTempDirectory("compact-preallocation-")
    val store = right(LocalEstimateStore.open(root))
    assert(store.newSink(fixture.unit, 1, CovarianceLayout.SharedNormalizedTable(SharedCovarianceLimits(maximumPairs = 11))).isLeft)
    assert(store.newSink(fixture.unit, 1, CovarianceLayout.SharedNormalizedTable(SharedCovarianceLimits(maximumTableBytes = 1))).isLeft)
    assert(store.newSink(fixture.unit.copy(covariance = fixture.unit.covariance.map(_.copy(invariantSamples = false))),
      1, CovarianceLayout.SharedNormalizedTable()).isLeft)
    assert(!Files.exists(root.resolve(".staging")))
    val sink = right(store.newSink(fixture.unit, 1, CovarianceLayout.SharedNormalizedTable()))
    val shared = sink.asInstanceOf[SharedCovarianceSink]
    val selection = SharedCovarianceSelection(Vector(fixture.observations.head.id), Vector(fixture.pairs.head))
    assert(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(-1.0), Array[Byte](0)).isLeft)
    assert(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(Double.NaN), Array[Byte](0)).isLeft)
    assert(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(4.0), Array[Byte](1)).isLeft)
    right(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(4.0), Array[Byte](0)))
    assert(sink.seal().isLeft)
    right(sink.abort())
    assert(!Files.exists(root.resolve(s"units/${fixture.unit.revision.value}/estimates.json")))
    val stages = Files.walk(root.resolve(".staging"))
    try assert(!stages.anyMatch(path => Files.isRegularFile(path)))
    finally stages.close()
    assert(shared.writeSharedCovariance(fixture.covariance.id, selection, Array(4.0), Array[Byte](0)).isLeft)
  }

  test("sparse reordered blocks reopen through fresh handles with exact values and per-estimand validity") {
    val root = Files.createTempDirectory("scalafim-estimates-")
    val reference = write(right(LocalEstimateStore.open(root)))
    val reopened = right(LocalEstimateStore.open(root))
    val source = right(reopened.open(reference, ReadLimits(12)))
    try
      val selection = EstimateSelection(Vector(obs.id), Vector(a, b), Vector(0, 1, 3, 7, 23))
      val data = Array.fill(10)(-999.0)
      val validity = new Array[Byte](10)
      right(source.read(product.id, selection, data, validity))
      assertEquals(data.toVector, Vector(0.0, 0.0, 3.0, 7.0, 23.0, 100.0, 0.0, 103.0, 107.0, 123.0))
      assertEquals(validity.toVector, Vector[Byte](0, 1, 0, 0, 0, 0, 1, 0, 3, 0))
      assert(source.read(product.id, selection, data, validity, () => true) == Left(EstimateError.Cancelled))
    finally right(source.close())
    assert(source.read(product.id, EstimateSelection(Vector(obs.id), Vector(a), Vector(0)), new Array[Double](1), new Array[Byte](1)).isLeft)
    // Independent byte oracle: .nii x-fastest samples, then declared A/B volume.
    val manifest = Files.readString(root.resolve(reference.manifest.path))
    val representation = right(EstimateMetadata.representations(manifest)).head
    val bytes = ByteBuffer.wrap(Files.readAllBytes(root.resolve(representation.values.path))).order(ByteOrder.LITTLE_ENDIAN)
    val offset = bytes.getFloat(108).toInt
    assertEquals(bytes.getDouble(offset + 8 * 23), 23.0)
    assertEquals(bytes.getDouble(offset + 8 * (24 + 23)), 123.0)
    assertEquals(bytes.getShort(254).toInt, 1)
  }

  test("digest-pinned metadata and payload corruption are rejected before analysis reuse") {
    val root = Files.createTempDirectory("scalafim-estimate-corrupt-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val encoded = Files.readString(root.resolve(ref.manifest.path))
    val representation = right(EstimateMetadata.representations(encoded)).head
    val file = new java.io.RandomAccessFile(root.resolve(representation.values.path).toFile, "rw")
    try
      file.seek(file.length() - 1)
      file.writeByte(99)
    finally file.close()
    assert(store.open(ref, ReadLimits(8)).isLeft)
    assert(store.inspect(ref).isRight) // metadata-only inspection advertises no numerical verification
    Files.writeString(root.resolve(ref.manifest.path), encoded + " ")
    assert(store.inspect(ref).isLeft)
  }

  test("estimate NIfTI axis refuses time sampling even when spatial geometry agrees") {
    val root = Files.createTempDirectory("scalafim-estimate-axis-")
    val ref = write(right(LocalEstimateStore.open(root)))
    val representation = right(EstimateMetadata.representations(Files.readString(root.resolve(ref.manifest.path)))).head
    val path = root.resolve(representation.values.path)
    val original = Files.readAllBytes(path)
    val encoded = original.clone()
    val bytes = ByteBuffer.wrap(encoded).order(ByteOrder.LITTLE_ENDIAN)
    bytes.put(123, 10.toByte) // millimetres plus seconds; this is not a time axis
    bytes.putFloat(136, 2.0f)
    val altered = root.resolve("altered-axis.nii")
    Files.write(altered, encoded)
    val header = scalafim.image.io.Nifti.readHeader(altered).toOption.get
    assert(NiftiEstimateSource.validateHeader(unit, product, header, false).isLeft)
    assertEquals(ByteBuffer.wrap(original).order(ByteOrder.LITTLE_ENDIAN).getFloat(136), 0.0f)
  }

  test("coded qform handedness and explicitly named alternative frame are checked against sform") {
    val root = Files.createTempDirectory("scalafim-estimate-qform-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val manifest = Files.readString(root.resolve(ref.manifest.path))
    val representation = right(EstimateMetadata.representations(manifest)).head
    val original = Files.readAllBytes(root.resolve(representation.values.path))
    def header(name: String, qformCode: Int, qfac: Float, xOffset: Float) =
      val altered = original.clone()
      val bytes = ByteBuffer.wrap(altered).order(ByteOrder.LITTLE_ENDIAN)
      bytes.putShort(252, qformCode.toShort)
      bytes.putFloat(76, qfac)
      bytes.putFloat(268, xOffset)
      val path = root.resolve(name)
      Files.write(path, altered)
      scalafim.image.io.Nifti.readHeader(path).toOption.get
    val unset = header("qform-unset.nii", 0, 1.0f, 0.0f)
    assert(NiftiEstimateSource.validateHeader(unit, product, unset, false).isRight)
    assert(NiftiEstimateSource.validateHeader(unit, product, unset, false, Some("aligned-anatomical")).isLeft)
    val agreeing = header("qform-agree.nii", 1, 1.0f, 0.0f)
    assert(NiftiEstimateSource.validateHeader(unit, product, agreeing, false).isRight)
    val reversed = header("qform-reversed.nii", 1, -1.0f, 0.0f)
    assert(NiftiEstimateSource.validateHeader(unit, product, reversed, false).left.toOption.exists(_.message.contains("handedness")))
    val altered = right(store.objects.write(s"units/${revision.value}/opposite-handed.nii")(
      _.write(Files.readAllBytes(root.resolve("qform-reversed.nii")))).left.map(store.fromStore))
    val alternateManifest = EstimateMetadata.unit(unit, right(EstimateMetadata.catalogReference(manifest)),
      Vector(representation.copy(values = store.reference(altered))), right(EstimateMetadata.indexTables(manifest)))
    val alternateRef = ref.copy(manifest = right(store.writeText(s"units/${revision.value}/opposite-handed.json", alternateManifest)))
    assert(store.open(alternateRef, ReadLimits(8)).left.toOption.exists(_.message.contains("handedness")))
    val alternate = header("qform-alternate.nii", 2, 1.0f, 10.0f)
    assert(NiftiEstimateSource.validateHeader(unit, product, alternate, false).isLeft)
    assert(NiftiEstimateSource.validateHeader(unit, product, alternate, false, Some("aligned-anatomical")).isRight)
    assert(NiftiEstimateSource.validateHeader(unit, product, alternate, false, Some("mni-152")).isLeft)
  }

  test("reader caps product observation pairs before accessing missing payloads and closes boundary handles") {
    val root = Files.createTempDirectory("scalafim-estimate-handle-budget-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val representation = right(EstimateMetadata.representations(Files.readString(root.resolve(ref.manifest.path)))).head
    def candidate(size: Int, missing: Boolean): (EstimateUnit, Vector[NiftiRepresentation]) =
      val observations = Vector.tabulate(size): index =>
        obs.copy(id = ObservationId(f"participant-$index%02d"))
      val declaredProduct = product.copy(observations = observations.map(_.id))
      val declared = unit.copy(observations = observations, products = Vector(declaredProduct))
      val reps = observations.zipWithIndex.map: (observation, index) =>
        val values = if missing then representation.values.copy(path = s"missing/values-$index.nii") else representation.values
        val validity = if missing then representation.validity.copy(path = s"missing/validity-$index.nii") else representation.validity
        representation.copy(observation = observation.id, values = values, validity = validity)
      (declared, reps)
    val (tooMany, missing) = candidate(33, true)
    val rejected = NiftiEstimateSource.open(store, tooMany, missing, ReadLimits(8))
    assert(rejected.left.toOption.exists(e => e.isInstanceOf[EstimateError.Unsupported] && e.message.contains("32")))
    val (boundary, available) = candidate(32, false)
    val source = right(NiftiEstimateSource.open(store, boundary, available, ReadLimits(8)))
    val values = new Array[Double](1)
    val validity = new Array[Byte](1)
    right(source.read(product.id, EstimateSelection(Vector(boundary.observations.last.id), Vector(a), Vector(0)), values, validity))
    assertEqualsDouble(values(0), 0.0, 0.0)
    right(source.close())
    assert(source.read(product.id, EstimateSelection(Vector(boundary.observations.head.id), Vector(a), Vector(0)), values, validity).isLeft)
  }

  test("digest-pinned TSV projections cannot redefine ordered JSON estimand axes") {
    val root = Files.createTempDirectory("scalafim-estimate-tables-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val manifest = Files.readString(root.resolve(ref.manifest.path))
    val tables = right(EstimateMetadata.indexTables(manifest)).get
    assertEquals(Files.readString(root.resolve(tables.estimands.path)), EstimateMetadata.estimandsTsv(unit.catalog))
    assertEquals(Files.readString(root.resolve(tables.observations.path)), EstimateMetadata.observationsTsv(unit))
    val conflicting = right(store.writeText(s"units/${revision.value}/reordered-estimands.tsv",
      "index\testimand_id\n0\tB\n1\tA\n"))
    val catalog = right(EstimateMetadata.catalogReference(manifest))
    val representations = right(EstimateMetadata.representations(manifest))
    val changed = EstimateMetadata.unit(unit, catalog, representations, Some(tables.copy(estimands = conflicting)))
    val changedRef = right(store.writeText(s"units/${revision.value}/reordered-manifest.json", changed))
    assert(store.inspect(ref.copy(manifest = changedRef)).isLeft)
  }

  test("validly pinned but oversized payloads and conflicting volume axes refuse") {
    val root = Files.createTempDirectory("scalafim-estimate-invalid-bundle-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val original = Files.readString(root.resolve(ref.manifest.path))
    val catalog = right(EstimateMetadata.catalogReference(original))
    val tables = right(EstimateMetadata.indexTables(original))
    val representations = right(EstimateMetadata.representations(original))
    val first = representations.head
    def candidate(name: String, rep: NiftiRepresentation): PinnedUnit =
      val manifest = EstimateMetadata.unit(unit, catalog, Vector(rep), tables)
      ref.copy(manifest = right(store.writeText(s"units/${revision.value}/$name.json", manifest)))

    val raw = Files.readAllBytes(root.resolve(first.values.path))
    val oversizedObject = store.objects.write(s"units/${revision.value}/oversized-values.nii"):
      output =>
        output.write(raw)
        output.write(0)
    val oversized = right(oversizedObject.left.map(store.fromStore))
    val extra = candidate("oversized", first.copy(values = store.reference(oversized)))
    assert(store.open(extra, ReadLimits(8)).isLeft)

    val reordered = candidate("reordered-axis", first.copy(volumeOrder = Vector(b, a)))
    assert(store.open(reordered, ReadLimits(8)).isLeft)
  }

  test("deficient-rank subspace evidence is pinned and required on fresh reopen") {
    val root = Files.createTempDirectory("scalafim-deficient-evidence-")
    val store = right(LocalEstimateStore.open(root))
    val basis = right(store.writeText("evidence/estimable-subspace.tsv", "column\tcomponent\nA\t1\nB\t0\n"))
    val deficient = unit.copy(estimability = EstimabilityEvidence.Subspace(
      Vector(ColumnId("A"), ColumnId("B")), basis, 1, 1e-8, "synthetic SVD"))
    val sink = right(store.newSink(deficient, 8))
    right(sink.write(product.id, EstimateSelection(Vector(obs.id), Vector(a, b), domain.support),
      Array.fill(8)(2.0), Array.fill[Byte](8)(0)))
    val ref = right(sink.seal())
    val reopened = right(LocalEstimateStore.open(root))
    val source = right(reopened.open(ref, ReadLimits(8)))
    try assertEquals(source.unit.estimability, deficient.estimability)
    finally right(source.close())
    Files.writeString(root.resolve(basis.path), "altered\n")
    assert(reopened.open(ref, ReadLimits(8)).isLeft)
  }

  test("partial abort never publishes a unit and stale pointer updates cannot lose a revision") {
    val root = Files.createTempDirectory("scalafim-estimate-collection-")
    val store = right(LocalEstimateStore.open(root))
    val sink = right(store.newSink(unit, 8))
    right(sink.abort())
    assert(!Files.exists(root.resolve(s"units/${revision.value}/estimates.json")))
    val ref = write(store)
    val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000005"), model,
      Map(id -> UnitOutcome.Published(ref)))
    val pinned = right(store.publishCollection(collection))
    right(store.discover(pinned, None))
    assert(store.discover(pinned, None).isLeft)
    assertEquals(right(store.current()).get._1, pinned)
  }

  test("concurrent pointer contenders admit one CAS winner without clobber") {
    val root = Files.createTempDirectory("scalafim-estimate-cas-")
    val store = right(LocalEstimateStore.open(root))
    val ref = write(store)
    val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000041"), model,
      Map(id -> UnitOutcome.Published(ref)))
    val pinned = right(store.publishCollection(collection))
    val ready = new CountDownLatch(2)
    val start = new CountDownLatch(1)
    val results = new Array[Either[EstimateError, Unit]](2)
    val threads = Vector.tabulate(2): index =>
      new Thread(() =>
        ready.countDown()
        if start.await(60, TimeUnit.SECONDS) then
          results(index) = LocalEstimateStore.open(root).flatMap(_.discover(pinned, None))
        else results(index) = Left(EstimateError.Io("CAS contender start barrier timed out"))
      )
    threads.foreach(_.start())
    assert(ready.await(60, TimeUnit.SECONDS))
    start.countDown()
    threads.foreach(_.join(60000))
    assert(threads.forall(thread => !thread.isAlive))
    assert(results.forall(_ != null))
    assertEquals(results.count(_.isRight), 1)
    assertEquals(results.count(_.isLeft), 1)
    assertEquals(right(store.current()).get._1, pinned)
  }

  test("same-revision retry reuses identical bytes and refuses altered payload") {
    val root = Files.createTempDirectory("scalafim-estimate-retry-")
    val store = right(LocalEstimateStore.open(root))
    val original = write(store)
    assertEquals(write(right(LocalEstimateStore.open(root))), original)
    val before = Files.readAllBytes(root.resolve(original.manifest.path)).toVector
    val sink = right(store.newSink(unit, 8))
    right(sink.write(product.id,
      EstimateSelection(Vector(obs.id), Vector(a, b), domain.support),
      Array.fill(8)(99.0), Array.fill[Byte](8)(0)))
    assert(sink.seal().left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assertEquals(Files.readAllBytes(root.resolve(original.manifest.path)).toVector, before)
    assert(store.open(original, ReadLimits(8)).isRight)
  }

  test("statistic-only products persist without beta or SE links") {
    val statistic = product.copy(id = ProductId("z-statistic"), kind = ProductKind.Statistic(StatisticKind.Z), units = "dimensionless")
    val declared = unit.copy(catalog = unit.catalog.copy(entries = unit.catalog.entries.map(_.copy(kind = EstimandKind.Hypothesis))),
      products = Vector(statistic), outcomes = Map(statistic.id -> ProductOutcome.Available(statistic.id)),
      statistics = Vector(StatisticSemantics(statistic.id, ReferenceDistribution.Normal, Some(TestTail.TwoSided), Some(0.0), None, None)))
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("scalafim-statistic-only-")))
    val sink = right(store.newSink(declared, 8))
    val selected = EstimateSelection(Vector(obs.id), Vector(a, b), domain.support)
    right(sink.write(statistic.id, selected, Array.tabulate(8)(_.toDouble), Array.fill[Byte](8)(0)))
    val ref = right(sink.seal())
    val source = right(store.open(ref, ReadLimits(8)))
    try
      val values = new Array[Double](8)
      val validity = new Array[Byte](8)
      right(source.read(statistic.id, selected, values, validity))
      assertEquals(values.toVector, Vector.tabulate(8)(_.toDouble))
      assertEquals(source.unit.statistics.head.effect, None)
      assertEquals(source.unit.statistics.head.standardError, None)
    finally right(source.close())
  }

  test("collections join a shared catalog and refuse conflicting definitions under one model revision") {
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("scalafim-shared-catalog-")))
    val first = write(store)
    val otherId = UnitId("00000000-0000-4000-8000-000000000031")
    val otherRevision = UnitRevisionId("00000000-0000-4000-8000-000000000032")
    val secondProduct = product.copy(id = ProductId("other-effect"))
    def publish(declared: EstimateUnit): PinnedUnit =
      val sink = right(store.newSink(declared, 8))
      right(sink.write(secondProduct.id, EstimateSelection(Vector(obs.id), Vector(a, b), domain.support),
        Array.fill(8)(1.0), Array.fill[Byte](8)(0)))
      right(sink.seal())
    val secondUnit = unit.copy(unit = otherId, revision = otherRevision,
      products = Vector(secondProduct), outcomes = Map(secondProduct.id -> ProductOutcome.Available(secondProduct.id)))
    val second = publish(secondUnit)
    val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000033"), model,
      Map(id -> UnitOutcome.Published(first), otherId -> UnitOutcome.Published(second)))
    assert(store.publishCollection(collection).isRight)
    val changed = publish(secondUnit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000034"),
      catalog = secondUnit.catalog.copy(entries = secondUnit.catalog.entries.map(_.copy(normalization = "different normalization")))))
    assert(store.publishCollection(collection.copy(revision = CollectionRevisionId("00000000-0000-4000-8000-000000000035"),
      units = collection.units.updated(otherId, UnitOutcome.Published(changed)))).isLeft)
  }

  test("voxelwise absolute covariance has a named pair axis, independent validity and exact physical order") {
    val cov = product.copy(id = ProductId("absolute-covariance"), kind = ProductKind.Covariance,
      targets = ProductTargets.UpperTriangle(Vector(a, b)), units = "signal^2")
    val products = Vector(product, cov)
    val declared = unit.copy(products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      covariance = Vector(CovarianceDescriptor(cov.id, product.id, CovarianceEquation.Absolute, true, false)))
    val root = Files.createTempDirectory("scalafim-voxelwise-covariance-")
    val store = right(LocalEstimateStore.open(root))
    val sink = right(store.newSink(declared, 12))
    right(sink.write(product.id, EstimateSelection(Vector(obs.id), Vector(a, b), domain.support), Array.fill(8)(0.0), Array.fill[Byte](8)(0)))
    val pairs = Vector(EstimandPair(b, b), EstimandPair(a, b), EstimandPair(a, a))
    val selection = CovarianceSelection(Vector(obs.id), pairs, domain.support.reverse)
    val data = pairs.flatMap: pair =>
      domain.support.reverse.map(sample => (if pair.first != pair.second then -0.5 else if pair.first == a then 2.0 else 3.0) * (sample + 1))
    val validity = Array.fill[Byte](12)(0)
    validity(5) = Validity.NonEstimable.code
    assert(sink.writeCovariance(cov.id, selection.copy(pairs = Vector(EstimandPair(b, a))), new Array[Double](4), new Array[Byte](4)).isLeft)
    right(sink.writeCovariance(cov.id, selection, data.toArray, validity))
    assert(sink.writeCovariance(cov.id, selection, data.toArray, validity).isLeft)
    val ref = right(sink.seal())
    val source = right(store.open(ref, ReadLimits(12)))
    try
      val values = new Array[Double](12)
      val codes = new Array[Byte](12)
      right(source.readCovariance(cov.id, selection, values, codes))
      assertEquals(values.toVector, data)
      assertEquals(codes.toVector, validity.toVector)
      val matrix = right(CovarianceAccess.matrix(source, cov.id, obs.id, 23, Vector(b, a), CovariancePolicy(2, 1e-12, 1e-12)))
      assertEqualsDouble(matrix.values(0, 0), 72.0, 1e-12)
      assertEqualsDouble(matrix.values(0, 1), -12.0, 1e-12)
      assert(CovarianceAccess.matrix(source, cov.id, obs.id, 7, Vector(a, b), CovariancePolicy(2, 1e-12, 1e-12)).isLeft)
      val reps = right(EstimateMetadata.representations(Files.readString(root.resolve(ref.manifest.path))))
      val rep = reps.find(_.product == cov.id).get
      assertEquals(rep.pairOrder, Vector(EstimandPair(a, a), EstimandPair(a, b), EstimandPair(b, b)))
      val bytes = ByteBuffer.wrap(Files.readAllBytes(root.resolve(rep.values.path))).order(ByteOrder.LITTLE_ENDIAN)
      val offset = bytes.getFloat(108).toInt
      assertEqualsDouble(bytes.getDouble(offset + 8 * (24 + 23)), -12.0, 0.0)
      assertEqualsDouble(bytes.getDouble(offset + 8 * (48 + 23)), 72.0, 0.0)
    finally right(source.close())
  }

  // Opaque .h5 leaves exercise metadata closure only, never HDF5 conformance.
  private def opaqueHdf5(store: LocalEstimateStore): Vector[Hdf5Representation] =
    Hdf5MetadataFixture.records.groupBy(_.product).toVector.flatMap: (_, rows) =>
      val container = right(store.objects.write(rows.head.container.path)(_.write(Array[Byte](7, 11, 5))).left.map(store.fromStore))
      rows.map(_.copy(container = store.reference(container)))

  test("HDF5 hook publishes identical catalog/TSV projections and exact immutable metadata retries"):
    val f = Hdf5MetadataFixture
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("hdf5-metadata-only-")))
    val records = opaqueHdf5(store).sortBy(r => f.records.indexWhere(x => x.product == r.product && x.observation == r.observation))
    val pinned = right(store.publishHdf5Unit(f.unit, records))
    assertEquals(store.publishHdf5Unit(f.unit, records), Right(pinned))
    val (decoded, reps, status) = right(store.inspectWithRepresentations(pinned))
    assertEquals(decoded.copy(domain = f.unit.domain), f.unit)
    assertEquals(reps, records.map(EstimateRepresentation.Hdf5.apply))
    assertEquals(status, None)
    val manifest = Files.readString(store.root.resolve(pinned.manifest.path))
    val catalog = right(EstimateMetadata.catalogReference(manifest))
    val tables = right(EstimateMetadata.indexTables(manifest)).get
    assertEquals(Files.readString(store.root.resolve(catalog.path)), EstimateMetadata.catalog(f.unit.catalog))
    assertEquals(Files.readString(store.root.resolve(tables.estimands.path)), EstimateMetadata.estimandsTsv(f.unit.catalog))
    assertEquals(Files.readString(store.root.resolve(tables.observations.path)), EstimateMetadata.observationsTsv(f.unit))
    val before = Files.readString(store.root.resolve(pinned.manifest.path))
    val changed = f.unit.copy(provenance = f.unit.provenance.copy(executionId = "different-execution"))
    assert(store.publishHdf5Unit(changed, records).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assertEquals(Files.readString(store.root.resolve(pinned.manifest.path)), before)
    val paths = Files.walk(store.root.resolve(".staging"))
    try assert(!paths.anyMatch(Files.isRegularFile(_)))
    finally paths.close()

  test("invalid HDF5 closure, evidence and old publication routing refuse before metadata writes"):
    val f = Hdf5MetadataFixture
    for mode <- Vector("inventory", "evidence", "estimability", "missing", "corrupt", "old-default", "old-compact", "byte-count") do
      val store = right(LocalEstimateStore.open(Files.createTempDirectory(s"hdf5-refusal-$mode-")))
      val records = opaqueHdf5(store)
      val evidence = InferenceEvidence(Vector.empty, Vector(InferenceStatusScope.Fit(f.unit.observations.head.id)))
      val result = mode match
        case "inventory" => store.publishHdf5Unit(f.unit, records.drop(1))
        case "evidence" => store.publishHdf5Unit(f.unit.copy(inferenceEvidence = Some(evidence)), records)
        case "estimability" => store.publishHdf5Unit(f.unit.copy(estimability = EstimabilityEvidence.Design(Vector(ColumnId("z")),
          f.catalogRef.copy(path = "absent-design.tsv"))), records)
        case "missing" =>
          Files.delete(store.root.resolve(records.head.container.path))
          store.publishHdf5Unit(f.unit, records)
        case "corrupt" =>
          Files.write(store.root.resolve(records.head.container.path), Array[Byte](1, 2, 3))
          store.publishHdf5Unit(f.unit, records)
        case "byte-count" => store.publishHdf5Unit(f.unit, records.map(_.copy(container = records.head.container.copy(bytes = Long.MaxValue))))
        case _ => store.publishMixedUnit(f.unit, records.map(EstimateRepresentation.Hdf5.apply), mode == "old-compact")
      assert(result.isLeft, mode)
      assert(!Files.exists(store.root.resolve(s"units/${f.unit.revision.value}")), mode)
      assert(!Files.exists(store.root.resolve("current.json")), mode)
      val stages = Files.walk(store.root.resolve(".staging"))
      try assert(!stages.anyMatch(Files.isRegularFile(_)), mode)
      finally stages.close()

  test("default inspection, collections and NIfTI opening refuse HDF5 before absent container access"):
    val f = Hdf5MetadataFixture
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("hdf5-default-refusal-")))
    val records = opaqueHdf5(store)
    val pinned = right(store.publishHdf5Unit(f.unit, records))
    records.map(_.container.path).distinct.foreach(path => Files.delete(store.root.resolve(path)))
    def unsupported[A](result: Either[EstimateError, A]): Unit =
      assert(result.left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.inspectWithRepresentations(pinned).isRight) // metadata, not closed-container verification
    unsupported(store.inspect(pinned))
    unsupported(store.open(pinned, ReadLimits(1)))
    unsupported(NiftiEstimateSource.preflight(store, f.unit, records.map(EstimateRepresentation.Hdf5.apply), ReadLimits(1)))
    unsupported(NiftiEstimateSource.openMixed(store, f.unit, records.map(EstimateRepresentation.Hdf5.apply), ReadLimits(1)))
    val collection = EstimateCollection(f.unit.dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000150"),
      f.unit.catalog.model, Map(f.unit.unit -> UnitOutcome.Published(pinned)))
    unsupported(store.publishCollection(collection))
    assert(!Files.exists(store.root.resolve("collections")))
    assert(!Files.exists(store.root.resolve("current.json")))
