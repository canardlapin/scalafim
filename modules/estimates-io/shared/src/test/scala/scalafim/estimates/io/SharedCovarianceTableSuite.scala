package scalafim.estimates.io

import scalafim.estimates.*
import scalafim.archive.ContentDigest
import scalafim.image.SampleSpaces

object CompactFixture:
  val ids = Vector(EstimandId("z"), EstimandId("a/β"), EstimandId("M"))
  val dataset = DatasetId("00000000-0000-4000-8000-000000000091")
  val observations = Vector("row-z", "row-a").map(id =>
    Observation(ObservationId(id), ParticipantId(dataset, id), Vector(AcquisitionId("run"))))
  val catalog = EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000092"), ids.map(id =>
    EstimandDefinition(id, "repeated label", EstimandKind.Coefficient, "signal", "unit", id.value)))
  val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    observations.map(_.id), ProductTargets.Scalar(ids), PoolingScope.Run, "signal")
  val scale = effect.copy(id = ProductId("scale"), kind = ProductKind.ResidualVariance, units = "signal^2")
  val covariance = effect.copy(id = ProductId("U"), kind = ProductKind.Covariance, targets = ProductTargets.UpperTriangle(ids), units = "unitless")
  val products = Vector(effect, scale, covariance)
  val unknown = ScientificFact.Unknown("independent fixture")
  def unit: EstimateUnit = EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000093"),
    UnitRevisionId("00000000-0000-4000-8000-000000000094"), catalog,
    EstimateDomain.make(SampleSpaces(Vector(2, 3, 1)), Vector(0, 3, 5), "scanner").toOption.get,
    observations, Vector.empty, products, products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
    EstimabilityEvidence.Unknown("imported independent fixture"),
    EstimateProvenance("independent fixture", "1", "literal", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
    covariance = Vector(CovarianceDescriptor(covariance.id, effect.id, CovarianceEquation.Normalized(scale.id), false, true)))
  val firstValues = Vector(4.0, -1.0, 0.5, 9.0, 2.0, 16.0)
  val secondValues = Vector(1.0, 0.25, -0.5, 2.0, 0.75, 3.0)
  val pairs = Vector(EstimandPair(ids(0), ids(0)), EstimandPair(ids(0), ids(1)), EstimandPair(ids(0), ids(2)),
    EstimandPair(ids(1), ids(1)), EstimandPair(ids(1), ids(2)), EstimandPair(ids(2), ids(2)))
  val reference = FileReference("U.json", ContentDigest.unsafeSha256("a" * 64), 1000)
  def representation = SharedCovarianceRepresentation(covariance.id, observations.head.id, reference, ids)

  // Hand-authored schema and physical U, independent of the production encoder.
  val literal = """{
    "Schema":"scalafim-estimates-shared-normalized-upper-triangle-1","WireVersion":"1.0.0",
    "Product":"U","Observation":"row-z","Estimands":["z","a/β","M"],
    "Precision":"Float64","ValidityBroadcast":"SupportedSamples","Pairs":[
      {"First":0,"Second":0,"Value":4,"Validity":0},
      {"First":0,"Second":1,"Value":-1,"Validity":0},
      {"First":0,"Second":2,"Value":0.5,"Validity":0},
      {"First":1,"Second":1,"Value":9,"Validity":0},
      {"First":1,"Second":2,"Value":2,"Validity":0},
      {"First":2,"Second":2,"Value":16,"Validity":0}]}
  """

class SharedCovarianceTableSuite extends munit.FunSuite:
  private val fixture = CompactFixture

  test("literal normalized table preserves nonlexical named axes and signed entries") {
    val table = SharedCovarianceTable.decode(fixture.literal, 6).toOption.get
    assertEquals(table.estimands, fixture.ids)
    table.entries.map(_.value).zip(fixture.firstValues).foreach((actual, expected) => assertEqualsDouble(actual, expected, 0.0))
    assert(table.validate(fixture.representation).isRight)
    assert(fixture.representation.validate(fixture.unit).isRight)
    assert(table.validate(fixture.representation.copy(estimands = fixture.ids.reverse)).isLeft)
    assert(table.validate(fixture.representation.copy(observation = fixture.observations.last.id)).isLeft)
    assert(SharedCovarianceTable.decode(fixture.literal, 5).isLeft)
  }

  test("strict table dispatch rejects unknown fields, tags, precision, broadcast and numeric codes") {
    val mutations: Vector[ujson.Value => Unit] = Vector(
      v => v("Schema") = "unknown", v => v("WireVersion") = "2.0.0", v => v("Precision") = "Float32",
      v => v("ValidityBroadcast") = "AllSamples", v => v("Unexpected") = true,
      v => v("Pairs")(0)("Unexpected") = true,
      v => v("Pairs")(0)("Validity") = 1, v => v("Pairs")(0)("Validity") = 256,
      v => v("Pairs")(0)("Validity") = 2.5, v => v("Pairs")(0)("First") = -1,
      v => v("Pairs")(0)("Value") = -1, v => v("Pairs")(1)("Value") = ujson.Null,
      v => v("Pairs")(0)("Second") = 1,
      v => v("Pairs").arr.remove(5), v => v("Pairs")(1) = v("Pairs")(0))
    mutations.zipWithIndex.foreach: (mutate, index) =>
      val value = ujson.read(fixture.literal)
      mutate(value)
      assert(SharedCovarianceTable.decode(ujson.write(value), 6).isLeft, s"mutation $index")
    val invalid = ujson.read(fixture.literal)
    invalid("Pairs")(1)("Validity") = 3
    assertEquals(SharedCovarianceTable.decode(ujson.write(invalid), 6).toOption.get.entries(1).validity, Validity.NonEstimable)
    intercept[IllegalArgumentException](SharedCovarianceEntry(0, 1, Double.NaN, Validity.Valid))
  }

  test("shared layout refuses absolute, sample-dependent, wrong identity or precision descriptors") {
    val declared = fixture.unit
    assert(fixture.representation.validate(declared.copy(covariance = declared.covariance.map(_.copy(invariantSamples = false)))).isLeft)
    assert(fixture.representation.validate(declared.copy(covariance = declared.covariance.map(_.copy(equation = CovarianceEquation.Absolute)))).isLeft)
    assert(fixture.representation.validate(declared.copy(products = declared.products.map(p =>
      if p.id == fixture.covariance.id then p.copy(precision = NumericPrecision.Float32) else p))).isLeft)
    assert(fixture.representation.copy(product = fixture.effect.id).validate(declared).isLeft)
    assert(fixture.representation.copy(observation = ObservationId("missing")).validate(declared).isLeft)
    intercept[IllegalArgumentException](fixture.representation.copy(precision = NumericPrecision.Float32))
  }

  test("Core-2 is unit-only, strictly tagged, and preserves the old representation helper") {
    val declared = fixture.unit
    val tableRef = fixture.reference.copy(path = "estimands.tsv")
    val tables = EstimateIndexTables(tableRef, tableRef.copy(path = "observations.tsv"))
    val record = EstimateRepresentation.SharedNormalizedUpperTriangle(fixture.representation)
    val encoded = EstimateMetadata.compactUnit(declared, fixture.reference.copy(path = "catalog.json"), Vector(record), tables)
    assertEquals(EstimateMetadata.schema(encoded, "unit"), Right(EstimateMetadata.compactSchema))
    assert(EstimateMetadata.readUnit(encoded, declared.catalog).isRight)
    assertEquals(EstimateMetadata.allRepresentations(encoded), Right(Vector(record)))
    assert(EstimateMetadata.representations(encoded).isLeft)
    val mutations: Vector[ujson.Value => Unit] = Vector(
      v => v("WireVersion") = "1.0.0", v => v("Schema") = EstimateMetadata.coreSchema,
      v => v("DocumentKind") = "catalog", v => v("Content")("Representations")(0)("Tag") = "Other",
      v => v("Content")("Representations")(0)("Unexpected") = true,
      v => v("Content")("Representations")(0)("Content")("Unexpected") = true,
      v => v("Content")("Representations")(0)("Content").obj.remove("precision"))
    mutations.zipWithIndex.foreach: (mutate, index) =>
      val value = ujson.read(encoded)
      mutate(value)
      assert(EstimateMetadata.readUnit(ujson.write(value), declared.catalog).isLeft, s"mutation $index")
    val old = EstimateMetadata.unit(declared, fixture.reference, tables = Some(tables))
    assertEquals(EstimateMetadata.representations(old), Right(Vector.empty))
    assertEquals(EstimateMetadata.allRepresentations(old), Right(Vector.empty))
  }
