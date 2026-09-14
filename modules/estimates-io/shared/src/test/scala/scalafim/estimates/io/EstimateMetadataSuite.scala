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
    val decoded = EstimateMetadata.readUnit(text, catalog).toOption.get
    assertEquals(decoded.products, unit.products)
    assertEquals(decoded.degreesOfFreedom, unit.degreesOfFreedom)
    assertEquals(decoded.marginalUncertainty, unit.marginalUncertainty)
    assertEquals(decoded.domain.dimensions, unit.domain.dimensions)
    assertEquals(decoded.domain.worldFrame, unit.domain.worldFrame)

    val previous = ujson.read(text)
    previous("Content").obj.remove("marginalUncertainty")
    assertEquals(EstimateMetadata.readUnit(ujson.write(previous), catalog).map(_.marginalUncertainty), Right(Vector.empty))
  }
