package scalafim.estimates

import scalafim.archive.ContentDigest
import scalafim.image.SampleSpaces

class EstimateContractSuite extends munit.FunSuite:
  private val dataset = DatasetId("00000000-0000-4000-8000-000000000001")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000002")
  private val unitId = UnitId("00000000-0000-4000-8000-000000000003")
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000004")
  private val a = EstimandId("condition-a")
  private val b = EstimandId("condition-b")
  private val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
  private val catalog = EstimandCatalog(model, Vector(a, b).map(id =>
    EstimandDefinition(id, "repeated label", EstimandKind.LinearContrast, "percent", "unscaled", id.value)))
  private val domain = EstimateDomain.make(SampleSpaces(Vector(2, 3, 4)), Vector(0, 3, 7, 23), "scanner").toOption.get
  private val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation.id), ProductTargets.Scalar(Vector(a, b)), PoolingScope.Run, "percent")
  private def unit = EstimateUnit(dataset, unitId, revision, catalog, domain, Vector(observation), Vector.empty,
    Vector(effect), Map(effect.id -> ProductOutcome.Available(effect.id)), EstimabilityEvidence.Unknown("imported effect"),
    EstimateProvenance("fixture", "1", "import-1", ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"),
      ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"), Vector.empty, Vector.empty))

  test("shared catalog joins scientific IDs despite repeated labels and unit-local column order") {
    val first = EstimandBinding(a, Vector(ColumnId("task"), ColumnId("nuisance")), 1, Vector(1.0, 0.0))
    val second = EstimandBinding(a, Vector(ColumnId("nuisance"), ColumnId("task")), 1, Vector(0.0, 1.0))
    assertEquals(unit.copy(bindings = Vector(first)).catalog, unit.copy(bindings = Vector(second)).catalog)
    assertNotEquals(first.weights, second.weights)
    intercept[IllegalArgumentException](catalog.copy(entries = Vector(catalog.entries.head, catalog.entries.head)))
  }

  test("statistic-only units remain inspectable without invented effects or uncertainty") {
    val p = effect.copy(id = ProductId("t"), kind = ProductKind.Statistic(StatisticKind.T), units = "dimensionless")
    val t = unit.copy(catalog = catalog.copy(entries = catalog.entries.map(_.copy(kind = EstimandKind.Hypothesis))), products = Vector(p), outcomes = Map(p.id -> ProductOutcome.Available(p.id)),
      statistics = Vector(StatisticSemantics(p.id, ReferenceDistribution.Unknown("df not retained"), None, None, None, None)))
    assertEquals(t.statistics.head.effect, None)
    assert(!t.products.exists(_.kind == ProductKind.Effect))
    intercept[IllegalArgumentException](t.copy(statistics = Vector.empty))
  }

  test("logical selection preserves reversed estimand and sparse sample order with checked capacities") {
    val request = EstimateSelection(Vector(observation.id), Vector(b, a), Vector(23, 0, 3))
    val checked = EstimateReadValidation.check(unit, effect.id, request, 6, 6, ReadLimits(6))
    assert(checked.isRight)
    assertEquals(request.cells, 6L)
    assertEquals(request.samples, Vector(23, 0, 3))
    assert(EstimateReadValidation.check(unit, effect.id, request, 5, 6, ReadLimits(6)).isLeft)
    assert(EstimateReadValidation.check(unit, effect.id, request, 6, 6, ReadLimits(5)).isLeft)
    assert(EstimateReadValidation.check(unit, effect.id, request.copy(estimands = Vector(EstimandId("unknown"))), 6, 6, ReadLimits(6)).isLeft)
    intercept[IllegalArgumentException](request.copy(samples = Vector(0, 0)))
  }

  test("outside support differs from unknown samples and valid zero remains data") {
    val request = EstimateSelection(Vector(observation.id), Vector(a), Vector(1))
    assert(EstimateReadValidation.check(unit, effect.id, request, 1, 1, ReadLimits(1)).isRight)
    assert(!domain.contains(1))
    assert(effect.kind.accepts(0.0))
    assert(!effect.kind.accepts(Double.NaN))
    assert(!ProductKind.Variance.accepts(-1.0))
    assert(!ProductKind.Statistic(StatisticKind.P).accepts(1.01))
    assertEquals(Validity.fromCode(1), Right(Validity.OutsideSupport))
    assert(Validity.fromCode(9).isLeft)
  }

  test("covariance names both axes in deterministic upper-triangle order") {
    val targets = ProductTargets.UpperTriangle(Vector(b, a))
    assertEquals(targets.pairs, Vector(b -> b, b -> a, a -> a))
    assertEquals(targets.width, 3L)
    val covariance = effect.copy(id = ProductId("cov"), kind = ProductKind.Covariance, targets = targets)
    intercept[IllegalArgumentException](covariance.copy(targets = ProductTargets.Scalar(Vector(a, b))))
    intercept[IllegalArgumentException](unit.copy(products = Vector(effect, covariance),
      outcomes = Map(effect.id -> ProductOutcome.Available(effect.id), covariance.id -> ProductOutcome.Available(covariance.id))))
  }

  test("normalization requires an explicit retained variance-scale product") {
    val cov = effect.copy(id = ProductId("cov"), kind = ProductKind.Covariance, targets = ProductTargets.UpperTriangle(Vector(a, b)))
    val scale = effect.copy(id = ProductId("scale"), kind = ProductKind.ResidualVariance)
    val desc = CovarianceDescriptor(cov.id, effect.id, CovarianceEquation.Normalized(scale.id), true, true)
    val products = Vector(effect, cov, scale)
    val withCov = unit.copy(products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap, covariance = Vector(desc))
    assertEquals(withCov.covariance.head.equation, CovarianceEquation.Normalized(scale.id))
    intercept[IllegalArgumentException](withCov.copy(products = Vector(effect, cov), outcomes = withCov.outcomes - scale.id))
  }

  test("df domain, physical FIR coordinates and artifact references validate at construction") {
    intercept[IllegalArgumentException](DegreesOfFreedom(DfRole.Reference, DfValue.Scalar(0), "t", false))
    DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(0), "OLS", false)
    intercept[IllegalArgumentException](catalog.entries.head.copy(response = ResponseCoordinate.FirInterval("event", 2, 1)))
    val hash = ContentDigest.unsafeSha256("a" * 64)
    for path <- Vector("../payload", "/payload", "a//payload", "a/./payload", "a\\payload") do
      intercept[IllegalArgumentException](FileReference(path, hash, 1))
    intercept[IllegalArgumentException](FileReference("payload", hash, -1))
  }

  test("partial collection coverage preserves missing intended units") {
    val other = UnitId("00000000-0000-4000-8000-000000000005")
    val ref = PinnedUnit(unitId, revision, FileReference("unit.json", ContentDigest.unsafeSha256("a" * 64), 5))
    val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000006"), model,
      Map(unitId -> UnitOutcome.Published(ref), other -> UnitOutcome.Missing("required run failed")))
    assert(!collection.unitsPublished)
    assertEquals(collection.units.size, 2)
  }

  test("pair requests preserve order and reject reversed, duplicate and oversized axes") {
    val cov = effect.copy(id = ProductId("cov"), kind = ProductKind.Covariance, targets = ProductTargets.UpperTriangle(Vector(a, b)))
    val products = Vector(effect, cov)
    val declared = unit.copy(products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      covariance = Vector(CovarianceDescriptor(cov.id, effect.id, CovarianceEquation.Absolute, true, false)))
    val request = CovarianceSelection(Vector(observation.id), Vector(EstimandPair(b, b), EstimandPair(a, b)), Vector(23, 0))
    assert(CovarianceReadValidation.check(declared, cov.id, request, 4, 4, ReadLimits(4)).isRight)
    assertEquals(request.pairs.map(CovarianceReadValidation.volume(cov, _)), Vector(2, 1))
    assert(CovarianceReadValidation.check(declared, cov.id, request.copy(pairs = Vector(EstimandPair(b, a))), 4, 4, ReadLimits(4)).isLeft)
    assert(CovarianceReadValidation.check(declared, cov.id, request, 3, 4, ReadLimits(4)).isLeft)
    assert(CovarianceReadValidation.check(declared, cov.id, request, 4, 4, ReadLimits(3)).isLeft)
    intercept[IllegalArgumentException](request.copy(pairs = Vector(EstimandPair(a, a), EstimandPair(a, a))))
  }

  test("consumer covariance verification rejects indefiniteness even with zero variance scale") {
    val cov = effect.copy(id = ProductId("cov"), kind = ProductKind.Covariance, targets = ProductTargets.UpperTriangle(Vector(a, b)))
    val scale = effect.copy(id = ProductId("scale"), kind = ProductKind.ResidualVariance)
    val products = Vector(effect, cov, scale)
    val declared = unit.copy(products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      covariance = Vector(CovarianceDescriptor(cov.id, effect.id, CovarianceEquation.Normalized(scale.id), true, false)))
    val source = new EstimateSource:
      val unit = declared
      val limits = ReadLimits(1)
      def read(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte], cancelled: () => Boolean) =
        values(0) = if selection.samples.head == 0 then 4.0 else 0.0
        validity(0) = 0
        Right(EstimateReadReceipt(product, selection, 1))
      override def readCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte], cancelled: () => Boolean) =
        val pair = selection.pairs.head
        // Sample 0: [[2,1],[1,3]]. Sample 3: [[1,2],[2,1]], eigenvalues -1,3.
        values(0) = if selection.samples.head == 0 then
          if pair.first != pair.second then 1.0 else if pair.first == a then 2.0 else 3.0
        else if pair.first == pair.second then 1.0 else 2.0
        validity(0) = 0
        Right(CovarianceReadReceipt(product, selection, 1))
      def close() = Right(())
    val policy = CovariancePolicy(2, 1e-12, 1e-12)
    val checked = CovarianceAccess.matrix(source, cov.id, observation.id, 0, Vector(b, a), policy).toOption.get
    assertEqualsDouble(checked.values(0, 0), 12.0, 1e-12)
    assertEqualsDouble(checked.values(0, 1), 4.0, 1e-12)
    assertEqualsDouble(checked.values(1, 1), 8.0, 1e-12)
    assert(CovarianceAccess.matrix(source, cov.id, observation.id, 3, Vector(a, b), policy).isLeft)
    assert(CovarianceAccess.matrix(source, cov.id, observation.id, 0, Vector(a, b), policy.copy(maximumOrder = 1)).isLeft)
    assertEquals(CovarianceAccess.matrix(source, cov.id, observation.id, 0, Vector(a, b), policy, () => true), Left(EstimateError.Cancelled))
  }
