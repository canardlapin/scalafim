package scalafim.fmri.mvpa

class AxisIdentitySuite extends munit.FunSuite:

  private val GoldenEncoding =
    "150000007363616c6166696d2d6d7670612d617869732f76310b000000626f6c642d747269616c730700000073616d706c657303000000170000007375622d30312f72756e2d30312f747269616c2d303031170000007375622d30312f72756e2d30312f747269616c2d303032170000007375622d30312f72756e2d30322f747269616c2d3030310b000000747269616c2d7461626c6502000000080000006f72646572696e670900000072756e2d747269616c070000007375626a656374060000007375622d3031010c00000061637469766174696f6e2d7a01000000000000f4bf000000000000e03f0f000000626964733a2f2f73747564792d30310d0000007368613235363a61626331323302000000140000006576656e74732e74737620726f77206f7264657214000000747269616c7769736520726561646f7574207632"

  private val GoldenFingerprint =
    "scalafim-mvpa-axis-v1-23862b9cf316ab647062f688002f0b4b296c4c3f65a051f401a372220ded27b3"

  private val GoldenKeys =
    Vector(
      "sub-01/run-01/trial-001",
      "sub-01/run-01/trial-002",
      "sub-01/run-02/trial-001"
    ).map(AxisKey.unsafe)

  private val GoldenBasis =
    CoordinateBasis.unsafe(
      "trial-table",
      "subject" -> "sub-01",
      "ordering" -> "run-trial"
    )

  private val GoldenProvenance =
    CoordinateProvenance.unsafe(
      "bids://study-01",
      "sha256:abc123",
      "events.tsv row order",
      "trialwise readout v2"
    )

  private val GoldenScale =
    AxisScale.affine(-1.25, 0.5).toOption.get

  private def identity(
      id: String = "bold-trials",
      purpose: AxisPurpose = AxisPurpose.Samples,
      keys: Vector[AxisKey] = GoldenKeys,
      basis: CoordinateBasis = GoldenBasis,
      units: Option[AxisUnits] = Some(AxisUnits.unsafe("activation-z")),
      scale: AxisScale = GoldenScale,
      provenance: CoordinateProvenance = GoldenProvenance
  ): AxisIdentity =
    AxisIdentity(
      AxisId.unsafe(id),
      purpose,
      keys,
      basis,
      units,
      scale,
      provenance
    ).toOption.get

  test("canonical v1 encoding and SHA-256 fingerprint match an independent oracle"):
    val value = identity()

    assertEquals(value.size, 3)
    assertEquals(value.canonicalHex, GoldenEncoding)
    assertEquals(value.fingerprint.value, GoldenFingerprint)

  test("descriptor field input order is canonical"):
    val reversedBasis = CoordinateBasis.unsafe(
      "trial-table",
      "ordering" -> "run-trial",
      "subject" -> "sub-01"
    )

    assertEquals(identity(basis = reversedBasis), identity())
    assertEquals(identity(basis = reversedBasis).canonicalHex, GoldenEncoding)

  test("the trust-boundary record round-trips only with its canonical fingerprint"):
    val original = identity()
    val decoded = AxisIdentity.decode(original.toRecord)

    assertEquals(decoded, Right(original))
    assertEquals(decoded.toOption.map(_.canonicalHex), Some(GoldenEncoding))

  test("every scientific coordinate component participates in identity"):
    val original = identity()
    val mutations = Vector(
      identity(id = "bold-trials-copy"),
      identity(purpose = AxisPurpose.Effects),
      identity(keys = GoldenKeys.updated(2, AxisKey.unsafe("sub-01/run-02/trial-099"))),
      identity(keys = GoldenKeys.reverse),
      identity(basis = CoordinateBasis.unsafe("contrast-table", "ordering" -> "run-trial")),
      identity(units = Some(AxisUnits.unsafe("percent-signal-change"))),
      identity(scale = AxisScale.affine(-1.25, 0.25).toOption.get),
      identity(
        provenance = CoordinateProvenance.unsafe(
          "bids://study-01",
          "sha256:different",
          "events.tsv row order",
          "trialwise readout v2"
        )
      )
    )

    mutations.foreach: mutation =>
      assertEquals(mutation.size, original.size)
      assertNotEquals(mutation.fingerprint, original.fingerprint)
      assertNotEquals(mutation, original)

  test("value provenance and descriptive tags do not participate in axis identity"):
    val scientificAxis = identity()
    val first = AxisMetadata(
      valueProvenance = Vector("standardized in training fold 1"),
      tags = Set("bold", "prepared")
    )
    val second = AxisMetadata(
      valueProvenance = Vector("raw scanner values"),
      tags = Set("unprepared")
    )

    assertNotEquals(first, second)
    assertEquals(scientificAxis.fingerprint, identity().fingerprint)

  test("malformed identities fail closed at construction and decoding"):
    assert(AxisId("bad id with spaces").isLeft)
    assert(AxisId(" padded").isLeft)
    assert(AxisPurpose.named("Samples").isLeft)
    assert(AxisKey(" leading").isLeft)
    assert(AxisUnits("seconds ").isLeft)
    assert(CoordinateProvenance("source ", "revision").isLeft)
    assert(
      AxisIdentity(
        AxisId.unsafe("empty"),
        AxisPurpose.Samples,
        Vector.empty,
        GoldenBasis,
        None,
        AxisScale.nominal,
        GoldenProvenance
      ).left.exists(_ == AxisIdentityError.EmptyAxis)
    )
    assert(
      AxisIdentity(
        AxisId.unsafe("duplicate"),
        AxisPurpose.Samples,
        Vector(AxisKey.unsafe("trial-1"), AxisKey.unsafe("trial-1")),
        GoldenBasis,
        None,
        AxisScale.nominal,
        GoldenProvenance
      ).left.exists(_ == AxisIdentityError.DuplicateKey("trial-1"))
    )
    assert(
      CoordinateBasis(
        "table",
        Vector("order" -> "first", "order" -> "second")
      ).left.exists(_ == AxisIdentityError.DuplicateDescriptorField("order"))
    )
    assert(AxisScale.affine(Double.NaN, 1.0).isLeft)
    assert(AxisScale.affine(0.0, Double.PositiveInfinity).isLeft)
    assert(AxisScale.affine(0.0, 0.0).isLeft)

    val valid = identity().toRecord
    assert(
      AxisIdentity
        .decode(valid.copy(protocol = "scalafim-mvpa-axis/v2"))
        .left
        .exists:
          case AxisIdentityError.InvalidProtocol(_) => true
          case _                                    => false
    )
    assert(
      AxisIdentity
        .decode(valid.copy(fingerprint = "sha256:nope"))
        .left
        .exists:
          case AxisIdentityError.InvalidFingerprint(_) => true
          case _                                       => false
    )
    assert(
      AxisIdentity
        .decode(valid.copy(fingerprint = GoldenFingerprint.toUpperCase))
        .left
        .exists:
          case AxisIdentityError.InvalidFingerprint(_) => true
          case _                                       => false
    )
    assert(
      AxisIdentity
        .decode(valid.copy(fingerprint = "scalafim-mvpa-axis-v1-" + "0" * 64))
        .left
        .exists:
          case AxisIdentityError.FingerprintMismatch(_, _) => true
          case _                                           => false
    )
    assert(
      AxisIdentity
        .decode(valid.copy(orderedKeys = Vector("trial-1", "trial-1")))
        .left
        .exists:
          case AxisIdentityError.DuplicateKey("trial-1") => true
          case _                                         => false
    )
