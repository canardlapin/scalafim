package scalafim.estimates.io

import scalafim.estimates.*
import scalafim.archive.ContentDigest

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
