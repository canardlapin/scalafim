package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class DesignFingerprintMigrationSuite extends munit.FunSuite:
  private val rows = RowLayout(
    blockIds = Vector.fill(2)(RunIndex.unsafeOneBased(1)),
    acquisitionTimes = Vector(Seconds(0.0), Seconds(1.0)),
    selectedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2))
  )
  private val column = StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Intercept(RunScope.Global), "intercept").toOption.get
  private def schema(audit: DesignAudit = DesignAudit(), matrix: Mat = Mat.unsafe(2, 1, Array(1.0, 0.0)), layout: RowLayout = rows) =
    DesignSchema.validated(matrix, layout, Vector(column), audit).toOption.get
  // Pinned before migration; the new value was independently FNV-checked in Python.
  private val legacy = "design-schema/v2:a2612fa13f6d46eb"

  test("new v3 identity and exact legacy-v2 projection retain independent literal goldens"):
    val current = schema()
    assertEquals(current.fingerprint.value, "design-schema/v3:77ad21c1e89ad8b6")
    assertEquals(DesignFingerprint.legacyV2Value(current.matrix, current.rows, current.columns, current.audit), legacy)
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(current, legacy), Right(SavedDesignFingerprintMatch.LegacyV2DigestMatch))
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(current, current.fingerprint.value), Right(SavedDesignFingerprintMatch.CurrentV3DigestMatch))

  test("a legacy receipt cannot identify ordinary source rows even when matrix and old semantics match"):
    def source(row: Int) = SourceEventRow(TermId.unsafe("task"), 0, row, 0, Seconds(1), Seconds(0))
    val first = schema(DesignAudit(sourceEvents = Vector(source(2))))
    val second = schema(DesignAudit(sourceEvents = Vector(source(9))))
    val expectedCanonical =
      "design-schema/v3|matrix=2x1|rows=rows=2;blocks=1,1;times=0,4607182418800017408;selected=1,2|columns=1|intercept|global|ordinal=1|intercept|global|hrf-scale=as-convolved:4607182418800017408|audit=seen=0;used=0;excluded=;empty=;empty-audits=;factors=;missing=;provenance=;centering=;degenerate-modulators=;orthogonalization=;policies=;rank=PivotedQr:2:1:1:ScaleAware:4382002437431492608:pivots=intercept|global|ordinal=1:independent=intercept|global|ordinal=1:aliased=:diagonal=4607182418800017408:condition=4607182418800017408;diagnostics=;source-events=term=task|event=0|source=2|block=0|onset=4607182418800017408|duration=0|values=4607182418800017408,0"
    assertEquals(first.audit.sourceEvents.head.canonical,
      "term=task|event=0|source=2|block=0|onset=4607182418800017408|duration=0")
    assertEquals(first.fingerprint.canonicalEncoding, expectedCanonical)
    assertEquals(first.fingerprint.value, "design-schema/v3:4c19d08e41c2d4d8")
    assertNotEquals(first.fingerprint, second.fingerprint)
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(first, legacy), Right(SavedDesignFingerprintMatch.LegacyV2DigestMatch))
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(second, legacy), Right(SavedDesignFingerprintMatch.LegacyV2DigestMatch))
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(second, first.fingerprint.value), Left(SavedDesignFingerprintError.Mismatch))

  test("legacy receipt checking still distinguishes matrix bits, old audit fields and row layout"):
    val oneUlp = Mat.unsafe(2, 1, Array(1.0, java.lang.Double.longBitsToDouble(1L)))
    val changed = Vector(schema(matrix = oneUlp), schema(DesignAudit(eventsSeen = 1, eventsUsed = 1)),
      schema(layout = rows.copy(acquisitionTimes = Vector(Seconds(0.0), Seconds(2.0)))))
    changed.foreach(value => assertEquals(DesignFingerprintCompatibility.compareSavedValue(value, legacy), Left(SavedDesignFingerprintError.Mismatch)))

  test("receipt parser rejects malformed, unsupported and mismatched identities explicitly"):
    val current = schema()
    Vector("", "design-schema/v3:", "design-schema/v3:ABCD", "design-schema/v03:abc", "design-schema/v3:12345678901234567").foreach: value =>
      assertEquals(DesignFingerprintCompatibility.compareSavedValue(current, value), Left(SavedDesignFingerprintError.MalformedValue))
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(current, "design-schema/v1:abc"),
      Left(SavedDesignFingerprintError.UnsupportedVersion("design-schema/v1")))
    assertEquals(DesignFingerprintCompatibility.compareSavedValue(current, "design-schema/v3:abc"), Left(SavedDesignFingerprintError.Mismatch))

  test("strict schema validation and the type boundary do not manufacture identities from receipts"):
    val current = schema()
    def checked(fingerprint: DesignFingerprint) =
      DesignSchema.validate(current.matrix, current.rows, current.columns, current.audit, current.rankPreview, fingerprint)
    assert(checked(current.fingerprint).isRight)
    assert(checked(schema(DesignAudit(eventsSeen = 1)).fingerprint).isLeft)
    assert(!scala.compiletime.testing.typeChecks("""def forge(value: scalafim.fmri.design.DesignFingerprint) = value.copy(value = "design-schema/v2:abc")"""))
    assert(!scala.compiletime.testing.typeChecks("""def forge(value: scalafim.fmri.design.DesignFingerprint) = value.copy(canonicalEncoding = "forged")"""))
