package scalafim.fmri.mvpa

class AxisRefSuite extends munit.FunSuite:

  private val basis =
    CoordinateBasis.unsafe("trial-table", "ordering" -> "acquisition")

  private val provenance =
    CoordinateProvenance.unsafe("bids://study", "revision-1")

  private val sampleIds =
    Vector("sample-101", "sample-205", "sample-309").map(SampleId.unsafe)

  private def samples(
      keys: Vector[SampleId] = sampleIds,
      basisValue: CoordinateBasis = basis
  ): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe("trials"),
        AxisPurpose.Samples,
        keys,
        basisValue,
        None,
        AxisScale.nominal,
        provenance
      )
      .toOption
      .get

  test("semantic keys are unique, ordered, and recoverable without becoming ordinals"):
    val axis = samples()

    assertEquals(axis.keys.map(_.value), Vector("sample-101", "sample-205", "sample-309"))
    assertEquals(axis.keyAt(0).map(_.value), Right("sample-101"))
    assertEquals(axis.keyAt(2).map(_.value), Right("sample-309"))
    assertEquals(axis.positionOf(SampleId.unsafe("sample-205")), Some(1))
    assertEquals(axis.positionOf(SampleId.unsafe("missing")), None)
    assert(axis.keyAt(-1).isLeft)
    assert(axis.keyAt(3).isLeft)

  test("typed keys must exactly agree with the canonical identity"):
    val axis = samples()

    assert(AxisRef(axis.identity, sampleIds.dropRight(1)).left.exists:
      case AxisRefError.KeyCountMismatch(3, 2) => true
      case _                                   => false)
    assert(AxisRef(axis.identity, sampleIds.updated(1, SampleId.unsafe("sample-999"))).left.exists:
      case AxisRefError.KeyMismatch(1, expected, actual) =>
        expected.value == "sample-205" && actual.value == "sample-999"
      case _ => false)
    assert(
      AxisRef
        .create(
          AxisId.unsafe("duplicates"),
          AxisPurpose.Samples,
          Vector(SampleId.unsafe("same"), SampleId.unsafe("same")),
          basis,
          None,
          AxisScale.nominal,
          provenance
        )
        .left
        .exists:
          case AxisRefError.InvalidIdentity(AxisIdentityError.DuplicateKey("same")) => true
          case _                                                                    => false
    )

  test("Multivar evidence uses the complete identity fingerprint, not a label or dimension"):
    val original = samples()
    val foreign = samples(basisValue = CoordinateBasis.unsafe("contrast-table"))

    assertEquals(original.evidence.dimension, 3)
    assertEquals(original.evidence.id.value, original.identity.fingerprint.value)
    assertEquals(foreign.evidence.dimension, original.evidence.dimension)
    assertNotEquals(foreign.evidence.id, original.evidence.id)
    assert(!foreign.sameIdentity(original))

  test("runtime identity binds only to the existing fully validated nominal witness"):
    val expected = samples()
    val bound = expected.bind(expected.identity.toRecord)

    assert(bound.isRight)
    assert(bound.toOption.get eq expected.evidence)

    val sameShapeForeign = samples(basisValue = CoordinateBasis.unsafe("contrast-table"))
    assert(
      expected
        .bind(sameShapeForeign.identity)
        .left
        .exists:
          case AxisRefError.RuntimeIdentityMismatch(wanted, actual) =>
            wanted == expected.identity.fingerprint && actual == sameShapeForeign.identity.fingerprint
          case _ => false
    )

    val corrupted = expected.identity.toRecord.copy(fingerprint = "scalafim-mvpa-axis-v1-" + "0" * 64)
    assert(
      expected
        .bind(corrupted)
        .left
        .exists:
          case AxisRefError.InvalidIdentity(AxisIdentityError.FingerprintMismatch(_, _)) => true
          case _                                                                         => false
    )

  test("a nontrivial reorder derives a distinct child axis and explicit parent relation"):
    val parent = samples()
    val relation = parent.reorder(Vector(2, 0, 1)).toOption.get

    assertEquals(relation.parentIdentity, parent.identity)
    assert(relation.parentEvidence eq parent.evidence)
    assertEquals(relation.reindexing.toVector, Vector(2, 0, 1))
    assertEquals(relation.child.keys.map(_.value), Vector("sample-309", "sample-101", "sample-205"))
    assertEquals(relation.child.identity.orderedKeys.map(_.value), relation.child.keys.map(_.value))
    assertNotEquals(relation.child.identity.fingerprint, parent.identity.fingerprint)
    assertNotEquals(relation.child.evidence.id, parent.evidence.id)

  test("invalid total reorders fail closed"):
    val axis = samples()

    assert(axis.reorder(Vector(0, 1)).isLeft)
    assert(axis.reorder(Vector(0, 1, 3)).isLeft)
    assert(axis.reorder(Vector(0, 1, 1)).isLeft)

  test("sample IDs, feature IDs, and implementation positions are distinct at compile time"):
    val sampleVsFeature = compileErrors("""
      val feature = scalafim.fmri.mvpa.FeatureId.unsafe("feature-1")
      val sample: scalafim.fmri.mvpa.SampleId = feature
    """)
    val ordinalVsSample = compileErrors("""
      val position = 1
      val sample: scalafim.fmri.mvpa.SampleId = position
    """)

    assert(sampleVsFeature.nonEmpty)
    assert(ordinalVsSample.nonEmpty)
