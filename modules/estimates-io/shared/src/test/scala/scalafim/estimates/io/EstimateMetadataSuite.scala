package scalafim.estimates.io

import scalafim.estimates.*
import scalafim.archive.ContentDigest
import scalafim.image.SampleSpaces

class EstimateMetadataSuite extends munit.FunSuite:
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000001")
  private val catalog = EstimandCatalog(model, Vector(
    EstimandDefinition(EstimandId("a/β"), "same label", EstimandKind.BasisReadout, "signal", "unit area", "first",
      ResponseCoordinate.FirInterval("event onset", -1.25, 0.75)),
    EstimandDefinition(EstimandId("b"), "same label", EstimandKind.Hypothesis, "dimensionless", "none", "second")
  ))

  test("catalog roundtrip preserves physical response intervals, IDs and repeated labels") {
    val text = EstimateMetadata.catalog(catalog)
    assertEquals(EstimateMetadata.readCatalog(text), Right(catalog))
    assertEquals(EstimateMetadata.estimandsTsv(catalog), "index\testimand_id\n0\ta/β\n1\tb\n")
    assert(text.contains("0.2.0"))
    assert(!text.contains("scalafim.fmri.fit"))
  }

  test("decoding rejects unknown versions and invalid catalog invariants") {
    assert(EstimateMetadata.readCatalog(EstimateMetadata.catalog(catalog).replace("0.2.0", "9.0.0")).isLeft)
    val duplicated = EstimateMetadata.catalog(catalog).replace("\"b\"", "\"a/β\"")
    assert(EstimateMetadata.readCatalog(duplicated).isLeft)
    assert(EstimateMetadata.readCatalog("{}").isLeft)
  }

  test("pinned pointers preserve exact root-relative Path SHA256 Bytes references") {
    val pinned = PinnedEstimateSet(CollectionRevisionId("00000000-0000-4000-8000-000000000002"),
      FileReference("collections/revision/estimateset.json", ContentDigest.unsafeSha256("a" * 64), 123456789L))
    val text = EstimateMetadata.pointer(pinned)
    assertEquals(EstimateMetadata.readPointer(text), Right(pinned))
    assert(text.contains("\"Path\""))
    assert(text.contains("\"SHA256\""))
    assert(text.contains("\"Bytes\""))
    assert(EstimateMetadata.readPointer(text.replace("123456789", "1.5")).isLeft)
    assert(EstimateMetadata.readPointer(text.replace("collections/revision/", "../")).isLeft)
  }

  test("unit metadata roundtrips uncertainty semantics and reads pre-extension documents as unspecified") {
    val dataset = DatasetId("00000000-0000-4000-8000-000000000010")
    val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
    val target = catalog.entries.head.id
    val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
      Vector(observation.id), ProductTargets.Scalar(Vector(target)), PoolingScope.Run, "signal")
    val se = effect.copy(id = ProductId("se"), kind = ProductKind.StandardError)
    val df = DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(18.0), "OLS residual df", false)
    val products = Vector(effect, se)
    val unit = EstimateUnit(dataset,
      UnitId("00000000-0000-4000-8000-000000000011"),
      UnitRevisionId("00000000-0000-4000-8000-000000000012"), catalog,
      EstimateDomain.make(SampleSpaces(Vector(1, 1, 1)), Vector(0), "scanner").toOption.get,
      Vector(observation), Vector.empty, products,
      products.map(product => product.id -> ProductOutcome.Available(product.id)).toMap,
      EstimabilityEvidence.Unknown("fixture"),
      EstimateProvenance("fixture", "1", "run", ScientificFact.Known("OLS"),
        ScientificFact.Known("independent"), ScientificFact.Unknown("not retained"),
        ScientificFact.Known("single run"), Vector.empty, Vector.empty),
      degreesOfFreedom = Vector(df),
      marginalUncertainty = Vector(MarginalUncertaintyDescriptor(se.id, effect.id,
        MarginalVarianceOrigin.Estimated(df))))
    val catalogRef = FileReference("catalog.json", ContentDigest.unsafeSha256("b" * 64), 1)
    val text = EstimateMetadata.unit(unit, catalogRef)
    assert(text.contains(EstimateMetadata.developmentSchema))
    assertEquals(EstimateMetadata.observationsTsv(unit), "index\tobservation_id\n0\trow\n")
    val decoded = EstimateMetadata.readUnit(text, catalog).toOption.get
    assertEquals(decoded.products, unit.products)
    assertEquals(decoded.degreesOfFreedom, unit.degreesOfFreedom)
    assertEquals(decoded.marginalUncertainty, unit.marginalUncertainty)
    assertEquals(decoded.domain.dimensions, unit.domain.dimensions)
    assertEquals(decoded.domain.worldFrame, unit.domain.worldFrame)

    val previous = ujson.read(text)
    previous("Content").obj.remove("marginalUncertainty")
    assertEquals(EstimateMetadata.readUnit(ujson.write(previous), catalog).map(_.marginalUncertainty), Right(Vector.empty))

    val tableRef = FileReference("estimands.tsv", ContentDigest.unsafeSha256("c" * 64), 10)
    val stable = EstimateMetadata.unit(unit, catalogRef, tables = Some(EstimateIndexTables(
      tableRef, tableRef.copy(path = "observations.tsv"))))
    assert(stable.contains(EstimateMetadata.coreSchema))
    assert(stable.contains(EstimateMetadata.wireVersion))
    assert(EstimateMetadata.readUnit(stable, catalog).isRight)
    val withoutTables = ujson.read(stable)
    withoutTables("Content").obj.remove("Tables")
    assert(EstimateMetadata.readUnit(ujson.write(withoutTables), catalog).isLeft)
    val withoutStatistics = ujson.read(stable)
    withoutStatistics("Content").obj.remove("statistics")
    assert(EstimateMetadata.readUnit(ujson.write(withoutStatistics), catalog).isLeft)
    val wrongWire = ujson.read(stable)
    wrongWire("WireVersion") = "2.0.0"
    assert(EstimateMetadata.readUnit(ujson.write(wrongWire), catalog).isLeft)
    val extraEnvelope = ujson.read(stable)
    extraEnvelope("Unexpected") = true
    assert(EstimateMetadata.readUnit(ujson.write(extraEnvelope), catalog).isLeft)
  }

  test("development bare statistic links migrate to unknown correspondence; Core requires structured mapping") {
    val dataset = DatasetId("00000000-0000-4000-8000-000000000020")
    val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
    val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
      Vector(observation.id), ProductTargets.Scalar(Vector(catalog.entries.head.id)), PoolingScope.Run, "signal")
    val statistic = effect.copy(id = ProductId("t"), kind = ProductKind.Statistic(StatisticKind.T),
      targets = ProductTargets.Scalar(Vector(catalog.entries.last.id)), units = "dimensionless")
    val products = Vector(effect, statistic)
    val unknown = ScientificFact.Unknown("imported")
    val semantics = StatisticSemantics(statistic.id, ReferenceDistribution.StudentT(
      DegreesOfFreedom(DfRole.Reference, DfValue.Scalar(9.0), "known reference", false)),
      Some(TestTail.TwoSided), Some(0.0), Some(StatisticProductLink(effect.id,
        StatisticCorrespondence.Known(Vector(HypothesisTarget(catalog.entries.last.id, Vector(catalog.entries.head.id)))))), None)
    val unit = EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000021"),
      UnitRevisionId("00000000-0000-4000-8000-000000000022"), catalog,
      EstimateDomain.make(SampleSpaces(Vector(1, 1, 1)), Vector(0), "scanner").toOption.get,
      Vector(observation), Vector.empty, products,
      products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      EstimabilityEvidence.Unknown("imported"),
      EstimateProvenance("fixture", "1", "run", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
      statistics = Vector(semantics))
    val catalogRef = FileReference("catalog.json", ContentDigest.unsafeSha256("d" * 64), 1)
    val old = ujson.read(EstimateMetadata.unit(unit, catalogRef))
    old("Content")("statistics")(0)("effect") = effect.id.value
    old("Content")("products")(0)("pooling") = "JointRuns"
    val decoded = EstimateMetadata.readUnit(ujson.write(old), catalog).toOption.get
    assertEquals(decoded.statistics.head.effect.map(_.correspondence),
      Some(StatisticCorrespondence.Unknown("development-1 link lacks hypothesis correspondence")))
    assertEquals(decoded.products.head.pooling, PoolingScope.JointRuns)
    val tableRef = FileReference("estimands.tsv", ContentDigest.unsafeSha256("e" * 64), 10)
    val stable = ujson.read(EstimateMetadata.unit(unit, catalogRef,
      tables = Some(EstimateIndexTables(tableRef, tableRef.copy(path = "observations.tsv")))))
    assertEquals(EstimateMetadata.readUnit(ujson.write(stable), catalog).map(_.statistics.head.effect), Right(semantics.effect))
    stable("Content")("statistics")(0)("effect") = effect.id.value
    assert(EstimateMetadata.readUnit(ujson.write(stable), catalog).isLeft)
  }
