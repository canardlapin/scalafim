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

  test("Core-3 is explicit and strict; old encoders refuse recorded evidence"):
    val dataset = DatasetId("00000000-0000-4000-8000-000000000120")
    val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run")))
    val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
      Vector(observation.id), ProductTargets.Scalar(Vector(catalog.entries.head.id)), PoolingScope.Run, "signal")
    val unknown = ScientificFact.Unknown("fixture")
    val evidence = InferenceEvidence(Vector.empty, Vector(InferenceStatusScope.Fit(observation.id)))
    val unit = EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000121"),
      UnitRevisionId("00000000-0000-4000-8000-000000000122"), catalog,
      EstimateDomain.make(SampleSpaces(Vector(2, 1, 1)), Vector(1), "scanner").toOption.get,
      Vector(observation), Vector.empty, Vector(effect), Map(effect.id -> ProductOutcome.Available(effect.id)),
      EstimabilityEvidence.Unknown("fixture"),
      EstimateProvenance("fixture", "1", "run", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
      inferenceEvidence = Some(evidence))
    val ref = FileReference("catalog.json", ContentDigest.unsafeSha256("f" * 64), 12)
    val tables = EstimateIndexTables(ref.copy(path = "estimands.tsv"), ref.copy(path = "observations.tsv"))
    val status = InferenceStatusRepresentation(ref.copy(path = "status.nii"), evidence.planes)
    intercept[IllegalArgumentException](EstimateMetadata.unit(unit, ref))
    intercept[IllegalArgumentException](EstimateMetadata.unit(unit, ref, tables = Some(tables)))
    intercept[IllegalArgumentException](EstimateMetadata.compactUnit(unit, ref, Vector.empty, tables))
    val text = EstimateMetadata.inferenceUnit(unit, ref, Vector.empty, tables, status)
    assertEquals(EstimateMetadata.schema(text, "unit"), Right(EstimateMetadata.inferenceSchema))
    assertEquals(EstimateMetadata.readUnit(text, catalog).map(_.inferenceEvidence), Right(Some(evidence)))
    assertEquals(EstimateMetadata.inferenceStatus(text), Right(Some(status)))
    assert(EstimateMetadata.representations(text).isLeft)
    def mutate(action: ujson.Value => Unit): Unit =
      val json = ujson.read(text)
      action(json)
      assert(EstimateMetadata.readUnit(ujson.write(json), catalog).isLeft)
    mutate(_("WireVersion") = "1.0.0")
    mutate(_("Content").obj.remove("InferenceStatus"))
    mutate(_("Content")("inferenceEvidence") = ujson.Null)
    mutate(_("Content")("InferenceStatus")("Content")("unknown") = true)
    mutate(_("Content")("InferenceStatus")("Content")("planes") = ujson.Arr())
    mutate(_("Content")("inferenceEvidence")("unknown") = true)
    mutate(_("Content")("inferenceEvidence")("planes")(0)("unknown") = true)
    mutate: json =>
      json("Schema") = EstimateMetadata.coreSchema
      json("WireVersion") = EstimateMetadata.wireVersion
    val old = EstimateMetadata.unit(unit.copy(inferenceEvidence = None), ref, tables = Some(tables))
    assert(!old.contains("inferenceEvidence"))
    assertEquals(EstimateMetadata.readUnit(old, catalog).map(_.inferenceEvidence), Right(None))

  test("frozen old Core encoder bytes"):
    val fixture = CompactFixture
    val ref = fixture.reference.copy(path = "catalog.json")
    val tables = EstimateIndexTables(ref.copy(path = "estimands.tsv"), ref.copy(path = "observations.tsv"))
    val evidence = InferenceEvidence(Vector.empty, fixture.observations.map(o => InferenceStatusScope.Fit(o.id)))
    val evidenceUnit = fixture.unit.copy(inferenceEvidence = Some(evidence))
    val status = InferenceStatusRepresentation(ref.copy(path = "status.nii"), evidence.planes)
    val old1 = EstimateMetadata.unit(fixture.unit, ref, tables = Some(tables))
    val old2 = EstimateMetadata.compactUnit(fixture.unit, ref,
      Vector(EstimateRepresentation.SharedNormalizedUpperTriangle(fixture.representation)), tables)
    val old3 = EstimateMetadata.inferenceUnit(evidenceUnit, ref, Vector.empty, tables, status)
    assertEquals(old1, "{\n  \"ProfileVersion\": \"0.2.0\",\n  \"Schema\": \"scalafim-estimates-core-nifti-1\",\n  \"DocumentKind\": \"unit\",\n  \"Content\": {\n    \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n    \"unit\": \"00000000-0000-4000-8000-000000000093\",\n    \"revision\": \"00000000-0000-4000-8000-000000000094\",\n    \"domain\": {\n      \"Dimensions\": [\n        2,\n        3,\n        1\n      ],\n      \"VoxelToWorldRASMillimetres\": [\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1\n      ],\n      \"WorldFrame\": \"scanner\",\n      \"Support\": [\n        0,\n        3,\n        5\n      ]\n    },\n    \"observations\": [\n      {\n        \"id\": \"row-z\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-z\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      },\n      {\n        \"id\": \"row-a\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-a\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      }\n    ],\n    \"bindings\": [],\n    \"products\": [\n      {\n        \"id\": \"effect\",\n        \"kind\": \"Effect\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal\"\n      },\n      {\n        \"id\": \"scale\",\n        \"kind\": \"ResidualVariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal^2\"\n      },\n      {\n        \"id\": \"U\",\n        \"kind\": \"Covariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"UpperTriangle\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"unitless\"\n      }\n    ],\n    \"outcomes\": [\n      [\n        \"effect\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"effect\"\n        }\n      ],\n      [\n        \"scale\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"scale\"\n        }\n      ],\n      [\n        \"U\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"U\"\n        }\n      ]\n    ],\n    \"estimability\": {\n      \"$type\": \"Unknown\",\n      \"reason\": \"imported independent fixture\"\n    },\n    \"provenance\": {\n      \"producer\": \"independent fixture\",\n      \"version\": \"1\",\n      \"executionId\": \"literal\",\n      \"estimator\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"noise\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"nuisance\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"runCombination\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"scans\": [],\n      \"inputs\": []\n    },\n    \"covariance\": [\n      {\n        \"product\": \"U\",\n        \"effects\": \"effect\",\n        \"equation\": {\n          \"$type\": \"Normalized\",\n          \"varianceScale\": \"scale\"\n        },\n        \"invariantObservations\": false,\n        \"invariantSamples\": true\n      }\n    ],\n    \"Catalog\": {\n      \"Path\": \"catalog.json\",\n      \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n      \"Bytes\": 1000\n    },\n    \"Representations\": [],\n    \"Tables\": {\n      \"estimands\": {\n        \"Path\": \"estimands.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      },\n      \"observations\": {\n        \"Path\": \"observations.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      }\n    },\n    \"ModelRevisionId\": \"00000000-0000-4000-8000-000000000092\",\n    \"statistics\": [],\n    \"degreesOfFreedom\": [],\n    \"marginalUncertainty\": []\n  },\n  \"WireVersion\": \"1.0.0\"\n}\n")
    assertEquals(old2, "{\n  \"ProfileVersion\": \"0.2.0\",\n  \"Schema\": \"scalafim-estimates-core-nifti-2\",\n  \"DocumentKind\": \"unit\",\n  \"Content\": {\n    \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n    \"unit\": \"00000000-0000-4000-8000-000000000093\",\n    \"revision\": \"00000000-0000-4000-8000-000000000094\",\n    \"domain\": {\n      \"Dimensions\": [\n        2,\n        3,\n        1\n      ],\n      \"VoxelToWorldRASMillimetres\": [\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1\n      ],\n      \"WorldFrame\": \"scanner\",\n      \"Support\": [\n        0,\n        3,\n        5\n      ]\n    },\n    \"observations\": [\n      {\n        \"id\": \"row-z\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-z\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      },\n      {\n        \"id\": \"row-a\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-a\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      }\n    ],\n    \"bindings\": [],\n    \"products\": [\n      {\n        \"id\": \"effect\",\n        \"kind\": \"Effect\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal\"\n      },\n      {\n        \"id\": \"scale\",\n        \"kind\": \"ResidualVariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal^2\"\n      },\n      {\n        \"id\": \"U\",\n        \"kind\": \"Covariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"UpperTriangle\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"unitless\"\n      }\n    ],\n    \"outcomes\": [\n      [\n        \"effect\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"effect\"\n        }\n      ],\n      [\n        \"scale\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"scale\"\n        }\n      ],\n      [\n        \"U\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"U\"\n        }\n      ]\n    ],\n    \"estimability\": {\n      \"$type\": \"Unknown\",\n      \"reason\": \"imported independent fixture\"\n    },\n    \"provenance\": {\n      \"producer\": \"independent fixture\",\n      \"version\": \"1\",\n      \"executionId\": \"literal\",\n      \"estimator\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"noise\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"nuisance\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"runCombination\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"scans\": [],\n      \"inputs\": []\n    },\n    \"covariance\": [\n      {\n        \"product\": \"U\",\n        \"effects\": \"effect\",\n        \"equation\": {\n          \"$type\": \"Normalized\",\n          \"varianceScale\": \"scale\"\n        },\n        \"invariantObservations\": false,\n        \"invariantSamples\": true\n      }\n    ],\n    \"Catalog\": {\n      \"Path\": \"catalog.json\",\n      \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n      \"Bytes\": 1000\n    },\n    \"Representations\": [\n      {\n        \"Tag\": \"SharedNormalizedUpperTriangle\",\n        \"Content\": {\n          \"product\": \"U\",\n          \"observation\": \"row-z\",\n          \"table\": {\n            \"Path\": \"U.json\",\n            \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n            \"Bytes\": 1000\n          },\n          \"estimands\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ],\n          \"precision\": \"Float64\",\n          \"validityBroadcast\": \"SupportedSamples\"\n        }\n      }\n    ],\n    \"Tables\": {\n      \"estimands\": {\n        \"Path\": \"estimands.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      },\n      \"observations\": {\n        \"Path\": \"observations.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      }\n    },\n    \"ModelRevisionId\": \"00000000-0000-4000-8000-000000000092\",\n    \"statistics\": [],\n    \"degreesOfFreedom\": [],\n    \"marginalUncertainty\": []\n  },\n  \"WireVersion\": \"2.0.0\"\n}\n")
    assertEquals(old3, "{\n  \"ProfileVersion\": \"0.2.0\",\n  \"Schema\": \"scalafim-estimates-core-nifti-3\",\n  \"DocumentKind\": \"unit\",\n  \"Content\": {\n    \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n    \"unit\": \"00000000-0000-4000-8000-000000000093\",\n    \"revision\": \"00000000-0000-4000-8000-000000000094\",\n    \"domain\": {\n      \"Dimensions\": [\n        2,\n        3,\n        1\n      ],\n      \"VoxelToWorldRASMillimetres\": [\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1,\n        0,\n        0,\n        0,\n        0,\n        1\n      ],\n      \"WorldFrame\": \"scanner\",\n      \"Support\": [\n        0,\n        3,\n        5\n      ]\n    },\n    \"observations\": [\n      {\n        \"id\": \"row-z\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-z\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      },\n      {\n        \"id\": \"row-a\",\n        \"participant\": {\n          \"dataset\": \"00000000-0000-4000-8000-000000000091\",\n          \"label\": \"row-a\"\n        },\n        \"acquisitions\": [\n          \"run\"\n        ]\n      }\n    ],\n    \"bindings\": [],\n    \"products\": [\n      {\n        \"id\": \"effect\",\n        \"kind\": \"Effect\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal\"\n      },\n      {\n        \"id\": \"scale\",\n        \"kind\": \"ResidualVariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"Scalar\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"signal^2\"\n      },\n      {\n        \"id\": \"U\",\n        \"kind\": \"Covariance\",\n        \"precision\": \"Float64\",\n        \"observations\": [\n          \"row-z\",\n          \"row-a\"\n        ],\n        \"targets\": {\n          \"$type\": \"UpperTriangle\",\n          \"ids\": [\n            \"z\",\n            \"a/β\",\n            \"M\"\n          ]\n        },\n        \"pooling\": \"Run\",\n        \"units\": \"unitless\"\n      }\n    ],\n    \"outcomes\": [\n      [\n        \"effect\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"effect\"\n        }\n      ],\n      [\n        \"scale\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"scale\"\n        }\n      ],\n      [\n        \"U\",\n        {\n          \"$type\": \"Available\",\n          \"product\": \"U\"\n        }\n      ]\n    ],\n    \"estimability\": {\n      \"$type\": \"Unknown\",\n      \"reason\": \"imported independent fixture\"\n    },\n    \"provenance\": {\n      \"producer\": \"independent fixture\",\n      \"version\": \"1\",\n      \"executionId\": \"literal\",\n      \"estimator\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"noise\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"nuisance\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"runCombination\": {\n        \"$type\": \"Unknown\",\n        \"reason\": \"independent fixture\"\n      },\n      \"scans\": [],\n      \"inputs\": []\n    },\n    \"covariance\": [\n      {\n        \"product\": \"U\",\n        \"effects\": \"effect\",\n        \"equation\": {\n          \"$type\": \"Normalized\",\n          \"varianceScale\": \"scale\"\n        },\n        \"invariantObservations\": false,\n        \"invariantSamples\": true\n      }\n    ],\n    \"Catalog\": {\n      \"Path\": \"catalog.json\",\n      \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n      \"Bytes\": 1000\n    },\n    \"Representations\": [],\n    \"Tables\": {\n      \"estimands\": {\n        \"Path\": \"estimands.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      },\n      \"observations\": {\n        \"Path\": \"observations.tsv\",\n        \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n        \"Bytes\": 1000\n      }\n    },\n    \"ModelRevisionId\": \"00000000-0000-4000-8000-000000000092\",\n    \"statistics\": [],\n    \"degreesOfFreedom\": [],\n    \"marginalUncertainty\": [],\n    \"inferenceEvidence\": {\n      \"coefficients\": [],\n      \"planes\": [\n        {\n          \"$type\": \"Fit\",\n          \"observation\": \"row-z\"\n        },\n        {\n          \"$type\": \"Fit\",\n          \"observation\": \"row-a\"\n        }\n      ]\n    },\n    \"InferenceStatus\": {\n      \"Tag\": \"UInt8Nifti\",\n      \"Content\": {\n        \"file\": {\n          \"Path\": \"status.nii\",\n          \"SHA256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\n          \"Bytes\": 1000\n        },\n        \"planes\": [\n          {\n            \"$type\": \"Fit\",\n            \"observation\": \"row-z\"\n          },\n          {\n            \"$type\": \"Fit\",\n            \"observation\": \"row-a\"\n          }\n        ]\n      }\n    }\n  },\n  \"WireVersion\": \"3.0.0\"\n}\n")

  test("independent HDF5 literal preserves scientific unit and product-local reordered axes"):
    val f = Hdf5MetadataFixture
    val decoded = EstimateMetadata.readUnit(f.literal, f.unit.catalog).fold(e => fail(e.message), identity)
    assertEquals(decoded.copy(domain = f.unit.domain), f.unit)
    assertEquals(decoded.domain.dimensions, Vector(2, 3, 1))
    assertEquals(decoded.domain.support, Vector(0, 3, 5))
    assertEquals(decoded.domain.worldFrame, "scanner")
    assertEquals(EstimateMetadata.allRepresentations(f.literal), Right(f.records.map(EstimateRepresentation.Hdf5.apply)))
    assertEquals(EstimateMetadata.catalogReference(f.literal), Right(f.catalogRef))
    assertEquals(EstimateMetadata.indexTables(f.literal), Right(Some(f.tables)))
    assertEquals(EstimateMetadata.inferenceStatus(f.literal), Right(None))
    assert(EstimateMetadata.representations(f.literal).isLeft)
    val encoded = EstimateMetadata.hdf5Unit(f.unit, f.catalogRef, f.records, f.tables).fold(e => fail(e.message), identity)
    def canonical(json: ujson.Value): String = json match
      case obj: ujson.Obj => obj.obj.toVector.sortBy(_._1).map((key, value) => ujson.write(key) + ":" + canonical(value)).mkString("{", ",", "}")
      case arr: ujson.Arr => arr.arr.map(canonical).mkString("[", ",", "]")
      case value => ujson.write(value)
    assertEquals(canonical(ujson.read(encoded)), canonical(ujson.read(f.literal)))
    assertEquals(EstimateMetadata.readUnit(encoded, f.unit.catalog).map(_.copy(domain = f.unit.domain)), Right(f.unit))
    assertEquals(f.unit.products.head.observations, Vector(ObservationId("row-a"), ObservationId("row-z")))
    assertEquals(f.unit.observations.map(_.id), Vector(ObservationId("row-z"), ObservationId("row-a"), ObservationId("unused")))
    assertEquals(f.unit.products.find(_.id == ProductId("A")).get.targets.pairs, CompactFixture.pairs.map(p => p.first -> p.second))

  test("HDF5 inventory enforces exact pairs, one container/layout per product and separate products"):
    val f = Hdf5MetadataFixture
    def refused(records: Vector[Hdf5Representation], unit: EstimateUnit = f.unit): Unit =
      assert(Hdf5Representation.validateInventory(unit, records).isLeft)
      assert(EstimateMetadata.hdf5Unit(unit, f.catalogRef, records, f.tables).isLeft)
    refused(f.records.drop(1))
    refused(f.records :+ f.records.head)
    refused(f.records.updated(0, f.records.head.copy(product = ProductId("unknown"))))
    refused(f.records.updated(0, f.records.head.copy(observation = ObservationId("unused"))))
    refused(f.records.updated(0, f.records.head.copy(container = f.records.head.container.copy(path = "different.h5"))))
    refused(f.records.updated(0, f.records.head.copy(container = f.records.head.container.copy(bytes = 4))))
    refused(f.records.updated(0, f.records.head.copy(container = f.records.head.container.copy(digest = ContentDigest.unsafeSha256("b" * 64)))))
    refused(f.records.map(r => if r.product == ProductId("scale") then r.copy(container = f.records.head.container) else r))
    refused(f.records.updated(4, f.records(4).copy(layout = Hdf5PayloadLayout.PerSample)))
    refused(f.records.map(r => if r.product == ProductId("A") then r.copy(layout = Hdf5PayloadLayout.SharedNormalizedUpperTriangle) else r))
    refused(f.records.map(r => if r.product == ProductId("effect") then r.copy(layout = Hdf5PayloadLayout.SharedNormalizedUpperTriangle) else r))
    refused(f.records, f.unit.copy(covariance = f.unit.covariance.map(c => if c.product == ProductId("U") then c.copy(invariantSamples = false) else c)))
    refused(f.records, f.unit.copy(products = f.unit.products.map(p => if p.id == ProductId("U") then p.copy(precision = NumericPrecision.Float32) else p)))
    refused(f.records.map(r => if r.product == ProductId("effect") then r.copy(container = r.container.copy(bytes = Long.MaxValue)) else r))
    assert(Hdf5Representation.validateInventory(f.unit, f.records.map(_.copy(layout = Hdf5PayloadLayout.PerSample))).isRight)
    intercept[IllegalArgumentException](f.records.head.copy(container = f.records.head.container.copy(path = "effect.nii")))
    val evidence = InferenceEvidence(Vector.empty, Vector(InferenceStatusScope.Fit(f.unit.observations.head.id)))
    assert(EstimateMetadata.hdf5Unit(f.unit.copy(inferenceEvidence = Some(evidence)), f.catalogRef, f.records, f.tables)
      .left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    intercept[IllegalArgumentException](EstimateMetadata.compactUnit(f.unit, f.catalogRef, f.records.map(EstimateRepresentation.Hdf5.apply), f.tables))

  test("HDF5 literal rejects malformed envelope, fields, layout, references, coverage and status disguises"):
    val f = Hdf5MetadataFixture
    val mutations: Vector[ujson.Value => Unit] = Vector(
      v => v("ProfileVersion") = "0.3.0", v => v("WireVersion") = "2.0.0",
      v => v("DocumentKind") = "catalog", v => v("Unexpected") = true,
      v => v("Content")("Unexpected") = true,
      v => v("Content").obj.remove("statistics"), v => v("Content").obj.remove("Tables"),
      v => v("Content")("Catalog")("extra") = true,
      v => v("Content")("Tables")("extra") = true,
      v => v("Content")("Tables")("observations")("extra") = true,
      v => v("Content")("Representations")(0)("extra") = true,
      v => v("Content")("Representations")(0)("Tag") = "Nifti",
      v => v("Content")("Representations")(0)("Tag") = "SharedNormalizedUpperTriangle",
      v => v("Content")("Representations")(0)("Content")("extra") = true,
      v => v("Content")("Representations")(0)("Content")("container")("extra") = true,
      v => v("Content")("Representations")(0)("Content")("container")("Path") = "../effect.h5",
      v => v("Content")("Representations")(0)("Content")("container")("Path") = "effect.nii",
      v => v("Content")("Representations")(0)("Content")("container")("SHA256") = "A" * 64,
      v => v("Content")("Representations")(0)("Content")("container")("SHA256") = "123",
      v => v("Content")("Representations")(0)("Content")("container")("Bytes") = -1,
      v => v("Content")("Representations")(0)("Content")("container")("Bytes") = 1.5,
      v => v("Content")("Representations")(0)("Content")("container")("Bytes") = 9007199254740992.0,
      v => v("Content")("Representations")(0)("Content")("layout") = "AbsolutePerSample",
      v => v("Content")("Representations").arr.remove(0),
      v => v("Content")("Representations")(1) = v("Content")("Representations")(0),
      v => v("Content")("Representations")(0)("Content")("observation") = "unused",
      v => v("Content")("Representations")(4)("Content")("layout") = "PerSample",
      v => v("Content")("products")(2)("precision") = "Float32",
      v => v("Content")("covariance")(0)("invariantSamples") = false,
      v => v("Content")("covariance")(0)("equation") = ujson.Obj("$type" -> "Absolute"))
    mutations.zipWithIndex.foreach: (mutate, index) =>
      val json = ujson.read(f.literal)
      mutate(json)
      val text = ujson.write(json)
      assert(EstimateMetadata.readUnit(text, f.unit.catalog).isLeft, s"HDF5 mutation $index")
      assert(EstimateMetadata.inferenceStatus(text).isLeft, s"HDF5 status helper mutation $index")
    for field <- Vector("inferenceEvidence", "InferenceStatus"); disguise <- Vector(ujson.Null, ujson.Arr(), ujson.Obj()) do
      val json = ujson.read(f.literal)
      json("Content")(field) = disguise
      val text = ujson.write(json)
      assert(EstimateMetadata.readUnit(text, f.unit.catalog).isLeft)
      assert(EstimateMetadata.inferenceStatus(text).isLeft)
      assert(EstimateMetadata.allRepresentations(text).isLeft)
      assert(EstimateMetadata.catalogReference(text).isLeft)
      assert(EstimateMetadata.indexTables(text).isLeft)
    for (schema, wire) <- Vector(EstimateMetadata.coreSchema -> "1.0.0", EstimateMetadata.compactSchema -> "2.0.0", EstimateMetadata.inferenceSchema -> "3.0.0") do
      val json = ujson.read(f.literal)
      json("Schema") = schema
      json("WireVersion") = wire
      assert(EstimateMetadata.readUnit(ujson.write(json), f.unit.catalog).isLeft)
      assert(EstimateMetadata.allRepresentations(ujson.write(json)).isLeft)

/** Independently authored logical JSON. The .h5 leaves are declarations only;
  * this fixture contains no HDF5 bytes or physical dataset evidence.
  */
object Hdf5MetadataFixture:
  val catalogRef = CompactFixture.reference.copy(path = "catalog.json")
  val tables = EstimateIndexTables(catalogRef.copy(path = "estimands.tsv"), catalogRef.copy(path = "observations.tsv"))
  private val axis = CompactFixture.observations.reverse.map(_.id)
  private val effect = CompactFixture.effect.copy(observations = axis, precision = NumericPrecision.Float32)
  private val scale = CompactFixture.scale.copy(observations = axis)
  private val normalized = CompactFixture.covariance.copy(observations = axis)
  private val absolute = normalized.copy(id = ProductId("A"), units = "signal^2")
  private val products = Vector(effect, scale, normalized, absolute)
  val unit = CompactFixture.unit.copy(
    observations = CompactFixture.observations :+ Observation(ObservationId("unused"), ParticipantId(CompactFixture.dataset, "unused"), Vector(AcquisitionId("run"))),
    products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
    covariance = Vector(CovarianceDescriptor(normalized.id, effect.id, CovarianceEquation.Normalized(scale.id), false, true),
      CovarianceDescriptor(absolute.id, effect.id, CovarianceEquation.Absolute, true, false)))
  val records = products.flatMap: product =>
    product.observations.map: observation =>
      Hdf5Representation(product.id, observation, FileReference(s"${product.id.value}.h5", ContentDigest.unsafeSha256("a" * 64), 3),
        if product.id == normalized.id then Hdf5PayloadLayout.SharedNormalizedUpperTriangle else Hdf5PayloadLayout.PerSample)
  val literal = """{
    "ProfileVersion":"0.2.0","Schema":"scalafim-estimates-core-hdf5-1","DocumentKind":"unit","WireVersion":"1.0.0",
    "Content":{
      "dataset":"00000000-0000-4000-8000-000000000091","unit":"00000000-0000-4000-8000-000000000093",
      "revision":"00000000-0000-4000-8000-000000000094",
      "domain":{"Dimensions":[2,3,1],"VoxelToWorldRASMillimetres":[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],"WorldFrame":"scanner","Support":[0,3,5]},
      "observations":[
        {"id":"row-z","participant":{"dataset":"00000000-0000-4000-8000-000000000091","label":"row-z"},"acquisitions":["run"]},
        {"id":"row-a","participant":{"dataset":"00000000-0000-4000-8000-000000000091","label":"row-a"},"acquisitions":["run"]},
        {"id":"unused","participant":{"dataset":"00000000-0000-4000-8000-000000000091","label":"unused"},"acquisitions":["run"]}],
      "bindings":[],
      "products":[
        {"id":"effect","kind":"Effect","precision":"Float32","observations":["row-a","row-z"],"targets":{"$type":"Scalar","ids":["z","a/β","M"]},"pooling":"Run","units":"signal"},
        {"id":"scale","kind":"ResidualVariance","precision":"Float64","observations":["row-a","row-z"],"targets":{"$type":"Scalar","ids":["z","a/β","M"]},"pooling":"Run","units":"signal^2"},
        {"id":"U","kind":"Covariance","precision":"Float64","observations":["row-a","row-z"],"targets":{"$type":"UpperTriangle","ids":["z","a/β","M"]},"pooling":"Run","units":"unitless"},
        {"id":"A","kind":"Covariance","precision":"Float64","observations":["row-a","row-z"],"targets":{"$type":"UpperTriangle","ids":["z","a/β","M"]},"pooling":"Run","units":"signal^2"}],
      "outcomes":[["effect",{"$type":"Available","product":"effect"}],["scale",{"$type":"Available","product":"scale"}],["U",{"$type":"Available","product":"U"}],["A",{"$type":"Available","product":"A"}]],
      "estimability":{"$type":"Unknown","reason":"imported independent fixture"},
      "provenance":{"producer":"independent fixture","version":"1","executionId":"literal",
        "estimator":{"$type":"Unknown","reason":"independent fixture"},"noise":{"$type":"Unknown","reason":"independent fixture"},
        "nuisance":{"$type":"Unknown","reason":"independent fixture"},"runCombination":{"$type":"Unknown","reason":"independent fixture"},"scans":[],"inputs":[]},
      "covariance":[{"product":"U","effects":"effect","equation":{"$type":"Normalized","varianceScale":"scale"},"invariantObservations":false,"invariantSamples":true},
        {"product":"A","effects":"effect","equation":"Absolute","invariantObservations":true,"invariantSamples":false}],
      "statistics":[],"degreesOfFreedom":[],"marginalUncertainty":[],
      "Catalog":{"Path":"catalog.json","SHA256":"HASH","Bytes":1000},
      "Tables":{"estimands":{"Path":"estimands.tsv","SHA256":"HASH","Bytes":1000},"observations":{"Path":"observations.tsv","SHA256":"HASH","Bytes":1000}},
      "ModelRevisionId":"00000000-0000-4000-8000-000000000092",
      "Representations":[
        {"Tag":"Hdf5","Content":{"product":"effect","observation":"row-a","container":{"Path":"effect.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}},
        {"Tag":"Hdf5","Content":{"product":"effect","observation":"row-z","container":{"Path":"effect.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}},
        {"Tag":"Hdf5","Content":{"product":"scale","observation":"row-a","container":{"Path":"scale.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}},
        {"Tag":"Hdf5","Content":{"product":"scale","observation":"row-z","container":{"Path":"scale.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}},
        {"Tag":"Hdf5","Content":{"product":"U","observation":"row-a","container":{"Path":"U.h5","SHA256":"HASH","Bytes":3},"layout":"SharedNormalizedUpperTriangle"}},
        {"Tag":"Hdf5","Content":{"product":"U","observation":"row-z","container":{"Path":"U.h5","SHA256":"HASH","Bytes":3},"layout":"SharedNormalizedUpperTriangle"}},
        {"Tag":"Hdf5","Content":{"product":"A","observation":"row-a","container":{"Path":"A.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}},
        {"Tag":"Hdf5","Content":{"product":"A","observation":"row-z","container":{"Path":"A.h5","SHA256":"HASH","Bytes":3},"layout":"PerSample"}}]}}
  """.replace("HASH", "a" * 64)
