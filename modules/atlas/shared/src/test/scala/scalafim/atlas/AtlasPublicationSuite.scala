package scalafim.atlas

import locus4s.DomainRegistry
import scalafim.image.NeuroSpace
import scalafim.surface.Hemisphere as SurfaceHemisphere
import scalafim.surface.HemispherePair
import scalafim.surface.LabelInfo
import scalafim.surface.LabeledSurface
import scalafim.surface.SurfaceGeometry
import scalafim.surface.SurfaceKind
import scalafim.surface.TriangleMesh
import scalafim.surface.VertexId

class AtlasPublicationSuite extends munit.FunSuite:
  private val regions =
    RegionIndex(
      Vector(
        AtlasRegionMetadata(
          RegionId(10),
          "Left",
          hemisphere = Some(Hemisphere.Left),
          network = Some(NetworkId("NetA"))
        ),
        AtlasRegionMetadata(
          RegionId(20),
          "Right",
          hemisphere = Some(Hemisphere.Right),
          network = Some(NetworkId("NetB"))
        )
      )
    )

  private val volumeRef =
    AtlasRef.volume(
      family = "publication",
      model = "Tiny",
      templateSpace = SpaceId.MNI152,
      coordSpace = SpaceId.MNI152,
      confidence = Confidence.Exact,
      parcelVariant = Some("two-parcel-v1")
    )

  private val surfaceRef =
    AtlasRef.surface(
      family = "publication",
      model = "Tiny",
      templateSpace = SpaceId.FsAverage6,
      coordSpace = SpaceId.FsAverage6,
      confidence = Confidence.Exact,
      parcelVariant = Some("two-parcel-v1")
    )

  test("Schaefer variants define exact ordered parcel identity"):
    val sevenVolumeRef =
      Schaefer2018(
        SchaeferParcels.P100,
        YeoNetworks.Seven
      ).atlasRef(Confidence.Exact)
    val sevenSurfaceRef =
      Schaefer2018Surface(
        SchaeferParcels.P100,
        YeoNetworks.Seven
      ).atlasRef(Confidence.Exact)
    val seventeenVolumeRef =
      Schaefer2018(
        SchaeferParcels.P100,
        YeoNetworks.Seventeen
      ).atlasRef(Confidence.Exact)
    val sevenVolume = volumeAtlas(sevenVolumeRef).realization
    val sevenSurface = surfaceAtlas(sevenSurfaceRef, triangleMesh()).realization
    val seventeenVolume = volumeAtlas(seventeenVolumeRef).realization

    assertEquals(sevenVolumeRef.parcelVariant, Some("100Parcels_7Networks"))
    assert(
      sevenVolume.parcelDomain.samePersistentIdentityAs(
        sevenSurface.parcelDomain
      )
    )
    assert(
      !sevenVolume.parcelDomain.samePersistentIdentityAs(
        seventeenVolume.parcelDomain
      )
    )

  test("volume support identity includes the exact grid"):
    val original = volumeAtlas(volumeRef).realization
    val shifted =
      volumeAtlas(
        volumeRef,
        NeuroSpace(
          dims = Vector(2, 2, 1),
          origin = Some(Vector(10.0, 0.0, 0.0))
        )
      ).realization

    assert(original.parcelDomain.samePersistentIdentityAs(shifted.parcelDomain))
    assertNotEquals(
      original.volumeSupportDomain.identity.structuralFingerprint,
      shifted.volumeSupportDomain.identity.structuralFingerprint
    )
    assertEquals(
      original.validateNeuropublishProjection(shifted.neuropublishProjection),
      Left(
        AtlasPublicationError.SupportDomainMismatch(
          "exact spatial identity differs"
        )
      )
    )

  test("surface support identity includes ordered topology but not coordinates"):
    val first =
      surfaceAtlas(surfaceRef, quadMesh(swappedFaces = false)).realization
    val reordered =
      surfaceAtlas(surfaceRef, quadMesh(swappedFaces = true)).realization
    val moved =
      surfaceAtlas(
        surfaceRef,
        quadMesh(swappedFaces = false, coordinateScale = 2.0)
      ).realization

    assertNotEquals(
      first.leftSupportDomain.identity.structuralFingerprint,
      reordered.leftSupportDomain.identity.structuralFingerprint
    )
    assertEquals(
      first.leftSupportDomain.identity.structuralFingerprint,
      moved.leftSupportDomain.identity.structuralFingerprint
    )
    assertEquals(
      first.validateNeuropublishProjection(reordered.neuropublishProjection),
      Left(
        AtlasPublicationError.SupportDomainMismatch(
          "exact spatial identity differs"
        )
      )
    )

  test("bilateral publication retains one quotient and explicit empty fibers"):
    val realization = surfaceAtlas(surfaceRef, triangleMesh()).realization
    val projection = realization.neuropublishProjection
    val leftEmpty = projection.parcelDomain.elementKeys(1)
    val rightEmpty = projection.parcelDomain.elementKeys(0)

    assertEquals(realization.parcelAssignment.to.size, 2)
    assertEquals(
      projection.assignments.map(_.coverage),
      Vector(
        NeuropublishTargetCoverageV1.AllowEmpty(Vector(leftEmpty)),
        NeuropublishTargetCoverageV1.AllowEmpty(Vector(rightEmpty))
      )
    )

  test("foreign-producer v1 fixture matches fixed domain and assignment bytes"):
    val realization = volumeAtlas(volumeRef).realization
    val foreign = ForeignProducerFixture.projection

    assertEquals(
      AtlasPublicationSha256.hex(Vector.empty),
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      foreign.parcelDomain.identity.structuralFingerprint,
      "8d84890c1f25d45032b02b8027cf0975d9b509571fb01dcb3b08be6d65083065"
    )
    assertEquals(
      foreign.supportDomains.head.identity.structuralFingerprint,
      "173cf1dba9ba03fe8de2baad9613fd9b6ac49f157aa671248fd4e8717e62bcad"
    )
    assertEquals(
      hex(foreign.assignments.head.assetBytes),
      "00000000010000000000000001000000"
    )
    assertEquals(
      foreign.assignments.head.assetSha256,
      "7605713c8dae56537c6f3beb182bfc4efa2ae399d80004a20a45425fd1b25762"
    )
    assert(realization.validateNeuropublishProjection(foreign).isRight)

  test("publication admission recomputes fingerprints and identity preimages"):
    val realization = volumeAtlas(volumeRef).realization
    val projection = realization.neuropublishProjection
    val identity = projection.parcelDomain.identity
    val wrongFingerprint =
      projection.copy(
        parcelDomain = projection.parcelDomain.copy(
          identity = identity.copy(structuralFingerprint = "0" * 64)
        )
      )

    assertEquals(
      realization.validateNeuropublishProjection(wrongFingerprint),
      Left(
        AtlasPublicationError.ParcelDomainMismatch(
          "structural fingerprint does not match identity preimage"
        )
      )
    )

    val changedPreimage =
      identity.identityPreimage.updated(
        identity.identityPreimage.length - 1,
        (identity.identityPreimage.last ^ 1).toByte
      )
    val internallyHashedButStructurallyFalse =
      projection.copy(
        parcelDomain = projection.parcelDomain.copy(
          identity = identity.copy(
            structuralFingerprint =
              AtlasPublicationSha256.hex(changedPreimage),
            identityPreimage = changedPreimage
          )
        )
      )

    assertEquals(
      realization.validateNeuropublishProjection(
        internallyHashedButStructurallyFalse
      ),
      Left(
        AtlasPublicationError.ParcelDomainMismatch(
          "identity preimage does not match structural fields"
        )
      )
    )

  test("publication admission rejects malformed support and assignments"):
    val realization = volumeAtlas(volumeRef).realization
    val projection = realization.neuropublishProjection
    val support =
      projection.supportDomains.head match
        case value: NeuropublishVolumeGridDomainV1 => value
        case other => fail(s"expected volume support, found $other")

    val falseGrid =
      projection.copy(
        supportDomains = Vector(support.copy(shape = Vector(1, 4, 1)))
      )
    assertEquals(
      realization.validateNeuropublishProjection(falseGrid),
      Left(
        AtlasPublicationError.SupportDomainMismatch(
          "identity preimage does not match structural fields"
        )
      )
    )

    val outOfRange =
      projection.copy(
        assignments = Vector(
          projection.assignments.head.copy(
            targetOrdinals = Vector(0, 2, 0, 1)
          )
        )
      )
    assertEquals(
      realization.validateNeuropublishProjection(outOfRange),
      Left(
        AtlasPublicationError.AssignmentMismatch(
          "payload contains an out-of-range target ordinal"
        )
      )
    )

    val falseCoverage =
      projection.copy(
        assignments = Vector(
          projection.assignments.head.copy(
            targetOrdinals = Vector(0, 0, 0, 0)
          )
        )
      )
    assertEquals(
      realization.validateNeuropublishProjection(falseCoverage),
      Left(
        AtlasPublicationError.AssignmentMismatch(
          "declared target coverage differs from ordinal payload"
        )
      )
    )

    val falseSourceTable =
      projection.copy(
        assignments = Vector(
          projection.assignments.head.copy(
            provenance = projection.assignments.head.provenance.copy(
              sourceLabelToParcelKeys =
                projection.assignments.head.provenance
                  .sourceLabelToParcelKeys
                  .reverse
            )
          )
        )
      )
    assertEquals(
      realization.validateNeuropublishProjection(falseSourceTable),
      Left(
        AtlasPublicationError.AssignmentMismatch(
          "source-label-to-parcel-key table differs from parcel metadata"
        )
      )
    )

  test("checked constructors preserve typed invalid-payload failures"):
    val valid = volumeAtlas(volumeRef)
    val falseBackground =
      valid.provenance.copy(
        labels = valid.provenance.labels.copy(background = Some(99))
      )
    assertEquals(
      AtlasRealization.volumeFromLabelsIn(
        DomainRegistry.empty,
        volumeRef,
        regions,
        valid.labelVolume,
        falseBackground
      ),
      Left(
        AtlasRealizationError.Publication(
          AtlasPublicationError.AtlasProvenanceMismatch(
            "hard-label realizations require source background label 0"
          )
        )
      )
    )

    val foreignLabels =
      AtlasTestImages.labelVolume(
        NeuroSpace(Vector(2, 2, 1)),
        Array(10, 10, 30, 30)
      )
    AtlasRealization.volumeFromLabelsIn(
      DomainRegistry.empty,
      volumeRef,
      regions,
      foreignLabels,
      AtlasProvenance.fromRef(volumeRef, regions)
    ) match
      case Left(
            AtlasRealizationError.InvalidAtlas(
              AtlasError.MissingRegionId(id)
            )
          ) => assertEquals(id, RegionId(30))
      case other => fail(s"expected typed missing-region failure, found $other")

    val incomplete =
      AtlasTestImages.labelVolume(
        NeuroSpace(Vector(2, 2, 1)),
        Array(10, 10, 10, 10)
      )
    AtlasRealization.volumeFromLabelsIn(
      DomainRegistry.empty,
      volumeRef,
      regions,
      incomplete,
      AtlasProvenance.fromRef(volumeRef, regions)
    ) match
      case Left(
            AtlasRealizationError.InvalidAtlas(
              AtlasError.MissingPayloadRegionIds(ids)
            )
          ) => assertEquals(ids, Vector(RegionId(20)))
      case other => fail(s"expected typed coverage failure, found $other")

  private def volumeAtlas(
      ref: AtlasRef,
      space: NeuroSpace = NeuroSpace(Vector(2, 2, 1))
  ): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      regions,
      AtlasTestImages.labelVolume(
        space,
        Array(10, 20, 10, 20)
      )
    )

  private def surfaceAtlas(
      ref: AtlasRef,
      mesh: TriangleMesh
  ): SurfaceAtlas =
    val vertices = Vector.tabulate(mesh.vertexCount)(VertexId.apply)
    val left =
      LabeledSurface.fromIndexed(
        SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Pial),
        vertices,
        Vector.fill(mesh.vertexCount)(10),
        Vector(LabelInfo(10, "Left"))
      )
    val right =
      LabeledSurface.fromIndexed(
        SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Pial),
        vertices,
        Vector.fill(mesh.vertexCount)(20),
        Vector(LabelInfo(20, "Right"))
      )
    SurfaceAtlas.fromLabeledSurfaces(ref, regions, left, right)

  private def triangleMesh(): TriangleMesh =
    TriangleMesh.fromRows(
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0)
      ),
      Vector((0, 1, 2))
    )

  private def quadMesh(
      swappedFaces: Boolean,
      coordinateScale: Double = 1.0
  ): TriangleMesh =
    val faces =
      if swappedFaces then Vector((0, 2, 3), (0, 1, 2))
      else Vector((0, 1, 2), (0, 2, 3))
    TriangleMesh.fromRows(
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(coordinateScale, 0.0, 0.0),
        Vector(coordinateScale, coordinateScale, 0.0),
        Vector(0.0, coordinateScale, 0.0)
      ),
      faces
    )

  private def fromHex(value: String): Vector[Byte] =
    value.grouped(2).map(Integer.parseInt(_, 16).toByte).toVector

  private def hex(values: Vector[Byte]): String =
    values.map(value => f"${value & 0xff}%02x").mkString

  private object ForeignProducerFixture:
    // These bytes and digests were generated independently from the
    // Neuropublish v1 byte profile, not by ScalaFIM's encoder.
    private val firstKey =
      "org.scalafim.atlas/parcel/v1:91:38:org.scalafim.atlas/parcel-namespace/v111:publication4:Tiny13:two-parcel-v111:unspecified:10"
    private val secondKey =
      "org.scalafim.atlas/parcel/v1:91:38:org.scalafim.atlas/parcel-namespace/v111:publication4:Tiny13:two-parcel-v111:unspecified:20"
    private val finitePreimage = fromHex(
      "4e5055444f4d3100260000006f72672e6e6575726f7075626c6973682e646f6d61696e2f66696e6974652d696e6465786564010000003102000000000000007e0000006f72672e7363616c6166696d2e61746c61732f70617263656c2f76313a39313a33383a6f72672e7363616c6166696d2e61746c61732f70617263656c2d6e616d6573706163652f763131313a7075626c69636174696f6e343a54696e7931333a74776f2d70617263656c2d763131313a756e7370656369666965643a31307e0000006f72672e7363616c6166696d2e61746c61732f70617263656c2f76313a39313a33383a6f72672e7363616c6166696d2e61746c61732f70617263656c2d6e616d6573706163652f763131313a7075626c69636174696f6e343a54696e7931333a74776f2d70617263656c2d763131313a756e7370656369666965643a3230"
    )
    private val volumePreimage = fromHex(
      "4e5055444f4d3100230000006f72672e6e6575726f7075626c6973682e646f6d61696e2f766f6c756d652d677269640100000031060000004d4e49313532030000005241530a0000006d696c6c696d657465721e000000726f772d6d616a6f722d6c6173742d617869732d666173746573742f7631020000000200000001000000000000000000f03f0000000000000000000000000000000000000000000000000000000000000000000000000000f03f0000000000000000000000000000000000000000000000000000000000000000000000000000f03f0000000000000000000000000000000000000000000000000000000000000000000000000000f03f"
    )
    private val parcelIdentity =
      NeuropublishDomainIdentityV1(
        "org.neuropublish.domain/finite-indexed",
        "1",
        2,
        "8d84890c1f25d45032b02b8027cf0975d9b509571fb01dcb3b08be6d65083065",
        finitePreimage
      )
    private val volumeIdentity =
      NeuropublishDomainIdentityV1(
        "org.neuropublish.domain/volume-grid",
        "1",
        4,
        "173cf1dba9ba03fe8de2baad9613fd9b6ac49f157aa671248fd4e8717e62bcad",
        volumePreimage
      )
    private val affine =
      Vector(
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
      )
    private val atlasProvenance =
      NeuropublishAtlasProvenanceV1(
        "atlas-provenance",
        NeuropublishAtlasIdentityV1(
          "publication",
          "Tiny",
          None,
          Some("two-parcel-v1"),
          None
        ),
        NeuropublishDeclaredSupportV1.Volume(
          "MNI152",
          "MNI152",
          None,
          None
        ),
        NeuropublishAtlasLabelSchemaV1(
          "volume-integer-labels",
          Vector(10, 20),
          Some(0),
          None,
          Vector.empty
        ),
        Vector(
          NeuropublishSourceArtifactV1(
            "descriptor_publication_tiny",
            "descriptor",
            "publication",
            "Tiny",
            None,
            None,
            NeuropublishLicenseV1.Unspecified(
              "descriptor-only provenance; consult upstream source terms"
            ),
            None,
            None
          )
        ),
        Vector(
          NeuropublishAtlasDerivationV1.DeclaredDescriptor(
            "publication:Tiny"
          )
        ),
        Vector.empty,
        "exact"
      )
    private val assignmentProvenance =
      NeuropublishAssignmentProvenanceV1(
        "atlas-provenance",
        "org.scalafim.atlas/hard-label-realization",
        "1",
        Some(0),
        Vector(10 -> firstKey, 20 -> secondKey)
      )

    val projection: NeuropublishAtlasProjectionV1 =
      val parcelDomain =
        NeuropublishFiniteIndexedDomainV1(
          "parcel-domain",
          parcelIdentity,
          Vector(firstKey, secondKey)
        )
      val support =
        NeuropublishVolumeGridDomainV1(
          "volume-support",
          volumeIdentity,
          "MNI152",
          "RAS",
          "millimeter",
          "row-major-last-axis-fastest/v1",
          Vector(2, 2, 1),
          affine
        )
      NeuropublishAtlasProjectionV1(
        atlasProvenance,
        parcelDomain,
        Vector(
          NeuropublishParcelMetadataV1(
            firstKey,
            10,
            "Left",
            "Left",
            Some("left"),
            Some("NetA"),
            None,
            Vector.empty
          ),
          NeuropublishParcelMetadataV1(
            secondKey,
            20,
            "Right",
            "Right",
            Some("right"),
            Some("NetB"),
            None,
            Vector.empty
          )
        ),
        Vector(firstKey, secondKey),
        Some(
          NeuropublishParcelHierarchyV1(
            Vector("NetA", "NetB"),
            Vector(0, 1)
          )
        ),
        Vector(support),
        Vector(
          NeuropublishHardAssignmentV1(
            "volume-hard-assignment",
            volumeIdentity,
            parcelIdentity,
            Vector(0, 1, 0, 1),
            NeuropublishTargetCoverageV1.Complete,
            assignmentProvenance
          )
        )
      )
