package scalafim.estimates.io

import java.nio.file.Files
import java.nio.{ByteBuffer, ByteOrder}
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
