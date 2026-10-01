package scalafim.fmri.mvpa.artifacts

class PatternArchiveProfileSuite extends munit.FunSuite:
  test("scientific declaration refuses incomplete or reordered committed unit coverage"):
    assert(PatternScientificDeclaration.coverage(Vector("u1", "u2"), Vector("u1")).isLeft)
    assert(PatternScientificDeclaration.coverage(Vector("u1", "u2"), Vector("u2", "u1")).isLeft)
    assertEquals(PatternScientificDeclaration.coverage(Vector("u1", "u2"), Vector("u1", "u2")), Right(()))

  test("payload shape arithmetic rejects overflow and requires exact Float64 bytes"):
    assertEquals(ProfilePayload.cells(Int.MaxValue, Int.MaxValue), Some(4611686014132420609L))
    intercept[IllegalArgumentException](ProfilePayload("A", 1, 1, scalafim.estimates.FileReference("a", scalafim.archive.ContentDigest.unsafeSha256("a" * 64), 7)))

  test("profile limits are positive and bounded"):
    intercept[IllegalArgumentException](PatternArchiveLimits(0, 1))
    intercept[IllegalArgumentException](PatternArchiveLimits(1, 0))

  test("shared v1 metadata envelope round-trips and rejects an unknown schema"):
    val encoded = PatternProfileMetadata.document(ujson.Obj("Kind" -> "categorical", "Seed" -> Long.MaxValue.toString))
    assertEquals(PatternProfileMetadata.content(encoded, 1024).map(_("Seed").str), Right(Long.MaxValue.toString))
    assert(PatternProfileMetadata.content("{\"Schema\":\"other\",\"Content\":{}}", 1024).isLeft)
