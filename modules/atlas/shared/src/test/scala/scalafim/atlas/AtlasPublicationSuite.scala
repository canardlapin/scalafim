package scalafim.atlas

import locus4s.{Bijection, CertifiedMapError, DomainRegistry, TotalMap}
import scalafim.image.*
import scalafim.surface.{
  Hemisphere as SurfaceHemisphere,
  HemispherePair,
  LabelInfo,
  LabeledSurface,
  SurfaceGeometry,
  SurfaceKind,
  TriangleMesh,
  VertexId
}

class AtlasPublicationSuite extends munit.FunSuite:
  private val regions =
    RegionIndex(
      Vector(
        Region(
          RegionId(10),
          "Left",
          hemisphere = Some(Hemisphere.Left),
          network = Some(NetworkId("NetA"))
        ),
        Region(
          RegionId(20),
          "Right",
          hemisphere = Some(Hemisphere.Right),
          network = Some(NetworkId("NetB"))
        )
      )
    )

  private val reversedRegions =
    RegionIndex(regions.regions.reverse)

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

  test("realizations pair exact parcel identity, metadata, hierarchy, and provenance"):
    val atlas = volumeAtlas(volumeRef, regions)
    val realization = atlas.realization
    val projection = realization.neuropublishProjection

    assertEquals(realization.ref, atlas.ref)
    assertEquals(realization.provenance, atlas.provenance)
    assertEquals(realization.parcelDomain.size, 2)
    assertEquals(
      realization.parcelKeys.space.indices.map(realization.parcelKeys.apply).toVector,
      projection.parcelDomain.elementKeys
    )
    assertEquals(projection.parcelMetadata.map(_.id), Vector(10, 20))
    assertEquals(projection.displayOrder, projection.parcelDomain.elementKeys)
    assertEquals(
      projection.hierarchy.map(_.networkKeys),
      Some(Vector("NetA", "NetB"))
    )
    assertEquals(
      projection.hierarchy.map(_.parcelToNetworkOrdinals),
      Some(Vector(0, 1))
    )
    assertEquals(projection.atlasProvenance.identity.family, "publication")
    assertEquals(projection.atlasProvenance.identity.model, "Tiny")
    assertEquals(
      projection.atlasProvenance.identity.parcelVariant,
      Some("two-parcel-v1")
    )
    assertEquals(
      projection.atlasProvenance.labels.regionIds,
      Vector(10, 20)
    )
    assertEquals(
      projection.assignments.head.targetOrdinals,
      Vector(0, 1, 0, 1)
    )
    assertEquals(
      projection.assignments.head.coverage,
      NeuropublishTargetCoverageV1.Complete
    )
    assertEquals(projection.parcelDomain.identity.structuralFingerprint.length, 64)
    assert(realization.validateNeuropublishProjection(projection).isRight)

  test("same-sized foreign and reordered parcel domains fail exact alignment"):
    val base = volumeAtlas(volumeRef, regions).realization
    val reordered = volumeAtlas(volumeRef, reversedRegions).realization
    val foreign =
      volumeAtlas(
        volumeRef.copy(family = "foreign-producer"),
        regions
      ).realization

    assert(!base.parcelDomain.samePersistentIdentityAs(reordered.parcelDomain))
    assert(!base.parcelDomain.samePersistentIdentityAs(foreign.parcelDomain))
    assert(base.assignmentAlignedTo(reordered.parcelDomain).isLeft)
    assert(base.assignmentAlignedTo(foreign.parcelDomain).isLeft)
    assertEquals(
      base.validateNeuropublishProjection(reordered.neuropublishProjection),
      Left(
        AtlasPublicationError.ParcelDomainMismatch(
          "exact ordered finite-indexed identity differs"
        )
      )
    )

  test("metadata changes do not redefine ordered parcel identity"):
    val renamed =
      RegionIndex(
        Vector(
          regions.regions(0).copy(label = "Renamed left"),
          regions.regions(1).copy(label = "Renamed right")
        )
      )
    val original = volumeAtlas(volumeRef, regions).realization
    val changed = volumeAtlas(volumeRef, renamed).realization

    assert(original.parcelDomain.samePersistentIdentityAs(changed.parcelDomain))
    assertEquals(
      original.validateNeuropublishProjection(changed.neuropublishProjection),
      Left(AtlasPublicationError.ParcelMetadataMismatch("indexed rows differ"))
    )

  test("volume and surface share parcels by exact identity or an explicit bijection"):
    val volume = volumeAtlas(volumeRef, regions).realization
    val surface = surfaceAtlas(surfaceRef, regions, triangleMesh()).realization
    val reordered =
      surfaceAtlas(surfaceRef, reversedRegions, triangleMesh()).realization

    assert(volume.parcelDomain.samePersistentIdentityAs(surface.parcelDomain))
    assert(volume.assignmentAlignedTo(surface.parcelDomain).isRight)
    assert(volume.assignmentAlignedTo(reordered.parcelDomain).isLeft)

    val explicitMap =
      TotalMap
        .fromTargetOrdinals(
          volume.parcelDomain,
          reordered.parcelDomain,
          Vector(1, 0)
        )
        .fold(error => fail(error.toString), identity)
    val explicitBijection =
      Bijection
        .fromTotalMap(explicitMap)
        .fold(error => fail(error.message), identity)
    val retargeted =
      volume
        .assignmentRetargeted(explicitBijection)
        .fold(error => fail(error.message), identity)
    val reorderedTen = reordered.parcelPoint(RegionId(10)).get

    assertEquals(
      retargeted.fiber(reorderedTen).ordinalsInDomainOrder.toVector,
      volume.region(RegionId(10)).get.ordinalsInDomainOrder.toVector
    )

  test("Schaefer network variants differ while matching volume and surface variants align"):
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
    val sevenVolume = volumeAtlas(sevenVolumeRef, regions).realization
    val sevenSurface =
      surfaceAtlas(sevenSurfaceRef, regions, triangleMesh()).realization
    val seventeenVolume =
      volumeAtlas(seventeenVolumeRef, regions).realization

    assertEquals(
      sevenVolumeRef.parcelVariant,
      Some("100Parcels_7Networks")
    )
    assert(sevenVolume.parcelDomain.samePersistentIdentityAs(sevenSurface.parcelDomain))
    assert(!sevenVolume.parcelDomain.samePersistentIdentityAs(seventeenVolume.parcelDomain))

  test("volume support identity includes the exact grid"):
    val original = volumeAtlas(volumeRef, regions).realization
    val shifted =
      volumeAtlas(
        volumeRef,
        regions,
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

  test("surface support identity includes every ordered face but excludes coordinates"):
    val first =
      surfaceAtlas(surfaceRef, regions, quadMesh(swappedFaces = false)).realization
    val reorderedTopology =
      surfaceAtlas(surfaceRef, regions, quadMesh(swappedFaces = true)).realization
    val movedCoordinates =
      surfaceAtlas(
        surfaceRef,
        regions,
        quadMesh(swappedFaces = false, coordinateScale = 2.0)
      ).realization

    assertNotEquals(
      first.leftSupportDomain.identity.structuralFingerprint,
      reorderedTopology.leftSupportDomain.identity.structuralFingerprint
    )
    assertEquals(
      first.leftSupportDomain.identity.structuralFingerprint,
      movedCoordinates.leftSupportDomain.identity.structuralFingerprint
    )
    assertEquals(
      first.validateNeuropublishProjection(reorderedTopology.neuropublishProjection),
      Left(
        AtlasPublicationError.SupportDomainMismatch(
          "exact spatial identity differs"
        )
      )
    )

  test("bilateral surface projection retains complete quotient and per-hemisphere empty parcels"):
    val realization =
      surfaceAtlas(surfaceRef, regions, triangleMesh()).realization
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
    val realization = volumeAtlas(volumeRef, regions).realization
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
    assertEquals(
      foreign.assignments.head.provenance.sourceLabelToParcelKeys,
      Vector(10 -> foreign.parcelDomain.elementKeys(0), 20 -> foreign.parcelDomain.elementKeys(1))
    )
    assert(realization.validateNeuropublishProjection(foreign).isRight)

  test("publication admission recomputes parcel fingerprints and identity preimages"):
    val realization = volumeAtlas(volumeRef, regions).realization
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
            structuralFingerprint = AtlasPublicationSha256.hex(changedPreimage),
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

  test("publication admission rejects malformed support and assignment records"):
    val realization = volumeAtlas(volumeRef, regions).realization
    val projection = realization.neuropublishProjection
    val falseProvenance =
      projection.copy(
        atlasProvenance = projection.atlasProvenance.copy(
          confidence = "certain"
        )
      )
    assertEquals(
      realization.validateNeuropublishProjection(falseProvenance),
      Left(
        AtlasPublicationError.AtlasProvenanceMismatch(
          "confidence is unsupported"
        )
      )
    )

    val support =
      projection.supportDomains.head match
        case value: NeuropublishVolumeGridDomainV1 => value
        case other => fail(s"expected volume support, found $other")
    val falseGrid =
      projection.copy(
        supportDomains = Vector(
          support.copy(shape = Vector(1, 4, 1))
        )
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
                projection.assignments.head.provenance.sourceLabelToParcelKeys.reverse
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

  test("checked realization constructors preserve typed failures for invalid payloads"):
    val space = NeuroSpace(Vector(2, 2, 1))
    val validAtlas = volumeAtlas(volumeRef, regions, space)
    val falseBackground =
      validAtlas.provenance.copy(
        labels = validAtlas.provenance.labels.copy(background = Some(99))
      )
    AtlasQuotient.volumeIn(
      DomainRegistry.empty,
      volumeRef,
      regions,
      validAtlas.volume,
      falseBackground
    ) match
      case Left(
            AtlasQuotientError.Publication(
              AtlasPublicationError.AtlasProvenanceMismatch(detail)
            )
          ) =>
        assertEquals(
          detail,
          "hard-label realizations require source background label 0"
        )
      case other => fail(s"expected typed provenance failure, found $other")

    val mask =
      NeuroVol.fromLinear(
        PrimitiveBuffers.fillConst[Boolean](4, true),
        space
      )
    val foreignVolume =
      ClusteredNeuroVol(
        mask,
        PrimitiveBuffers.fromArray(Array(10, 10, 30, 30))
      )
    AtlasQuotient.volumeIn(
      DomainRegistry.empty,
      volumeRef,
      regions,
      foreignVolume,
      volumeRef.toProvenance(regions)
    ) match
      case Left(
            AtlasQuotientError.InvalidAtlas(
              AtlasError.MissingRegionId(id)
            )
          ) => assertEquals(id, RegionId(30))
      case other => fail(s"expected typed missing-region failure, found $other")

    val incompleteVolume =
      ClusteredNeuroVol(
        mask,
        PrimitiveBuffers.fromArray(Array(10, 10, 10, 10))
      )
    AtlasQuotient.volumeIn(
      DomainRegistry.empty,
      volumeRef,
      regions,
      incompleteVolume,
      volumeRef.toProvenance(regions)
    ) match
      case Left(
            AtlasQuotientError.CertifiedMap(
              CertifiedMapError.NotSurjective(ordinal)
            )
          ) => assertEquals(ordinal, 1)
      case other => fail(s"expected typed non-surjective failure, found $other")

    val mesh = triangleMesh()
    val vertices = Vector.tabulate(mesh.vertexCount)(VertexId.apply)
    val left =
      LabeledSurface.fromIndexed(
        SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Pial),
        vertices,
        Vector.fill(mesh.vertexCount)(30),
        Vector(LabelInfo(30, "Foreign"))
      )
    val right =
      LabeledSurface.fromIndexed(
        SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Pial),
        vertices,
        Vector.fill(mesh.vertexCount)(20),
        Vector(LabelInfo(20, "Right"))
      )
    AtlasQuotient.surfaceIn(
      DomainRegistry.empty,
      surfaceRef,
      regions,
      SurfaceAtlasPayload(HemispherePair(left, right)),
      surfaceRef.toProvenance(regions)
    ) match
      case Left(
            AtlasQuotientError.InvalidAtlas(
              AtlasError.MissingRegionId(id)
            )
          ) => assertEquals(id, RegionId(30))
      case other => fail(s"expected typed missing-region failure, found $other")

  private def volumeAtlas(
      ref: AtlasRef,
      index: RegionIndex,
      space: NeuroSpace = NeuroSpace(Vector(2, 2, 1))
  ): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      index,
      NeuroVol.fromLinear(
        PrimitiveBuffers.fromArray(Array(10, 10, 20, 20)),
        space
      )
    )

  private def surfaceAtlas(
      ref: AtlasRef,
      index: RegionIndex,
      mesh: TriangleMesh
  ): SurfaceAtlas =
    val leftGeometry =
      SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Pial)
    val rightGeometry =
      SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Pial)
    val vertices =
      Vector.tabulate(mesh.vertexCount)(VertexId.apply)
    val left =
      LabeledSurface.fromIndexed(
        leftGeometry,
        vertices,
        Vector.fill(mesh.vertexCount)(10),
        Vector(LabelInfo(10, "Left"))
      )
    val right =
      LabeledSurface.fromIndexed(
        rightGeometry,
        vertices,
        Vector.fill(mesh.vertexCount)(20),
        Vector(LabelInfo(20, "Right"))
      )
    SurfaceAtlas.fromLabeledSurfaces(ref, index, left, right)

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
    // These preimages and SHA-256 values were generated independently from the
    // Neuropublish v1 byte profile, not by AtlasPublicationBinaryV1.
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
        descriptorId = "org.neuropublish.domain/finite-indexed",
        descriptorVersion = "1",
        size = 2,
        structuralFingerprint =
          "8d84890c1f25d45032b02b8027cf0975d9b509571fb01dcb3b08be6d65083065",
        identityPreimage = finitePreimage
      )
    private val volumeIdentity =
      NeuropublishDomainIdentityV1(
        descriptorId = "org.neuropublish.domain/volume-grid",
        descriptorVersion = "1",
        size = 4,
        structuralFingerprint =
          "173cf1dba9ba03fe8de2baad9613fd9b6ac49f157aa671248fd4e8717e62bcad",
        identityPreimage = volumePreimage
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
        localId = "atlas-provenance",
        identity = NeuropublishAtlasIdentityV1(
          family = "publication",
          model = "Tiny",
          variant = None,
          parcelVariant = Some("two-parcel-v1"),
          release = None
        ),
        declaredSupport = NeuropublishDeclaredSupportV1.Volume(
          templateSpaceId = "MNI152",
          coordinateSpaceId = "MNI152",
          voxelSizeMillimeters = None,
          dimensions = None
        ),
        labels = NeuropublishAtlasLabelSchemaV1(
          encoding = "volume-integer-labels",
          regionIds = Vector(10, 20),
          backgroundSourceLabel = Some(0),
          labelTableArtifactId = None,
          attributes = Vector.empty
        ),
        sourceArtifacts = Vector(
          NeuropublishSourceArtifactV1(
            id = "descriptor_publication_tiny",
            role = "descriptor",
            sourceName = "publication",
            sourceRef = "Tiny",
            sourceUri = None,
            citationDoi = None,
            license = NeuropublishLicenseV1.Unspecified(
              "descriptor-only provenance; consult upstream source terms"
            ),
            digest = None,
            notes = None
          )
        ),
        derivation = Vector(
          NeuropublishAtlasDerivationV1.DeclaredDescriptor(
            "publication:Tiny"
          )
        ),
        citations = Vector.empty,
        confidence = "exact"
      )
    private val assignmentProvenance =
      NeuropublishAssignmentProvenanceV1(
        atlasProvenanceId = "atlas-provenance",
        converterId = "org.scalafim.atlas/hard-label-realization",
        converterVersion = "1",
        sourceBackgroundLabel = Some(0),
        sourceLabelToParcelKeys = Vector(10 -> firstKey, 20 -> secondKey)
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
          localId = "volume-support",
          identity = volumeIdentity,
          coordinateSpaceId = "MNI152",
          coordinateConvention = "RAS",
          spatialUnit = "millimeter",
          ordinalLayout = "row-major-last-axis-fastest/v1",
          shape = Vector(2, 2, 1),
          affineRowMajor = affine
        )
      NeuropublishAtlasProjectionV1(
        atlasProvenance = atlasProvenance,
        parcelDomain = parcelDomain,
        parcelMetadata = Vector(
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
        displayOrder = Vector(firstKey, secondKey),
        hierarchy = Some(
          NeuropublishParcelHierarchyV1(
            Vector("NetA", "NetB"),
            Vector(0, 1)
          )
        ),
        supportDomains = Vector(support),
        assignments = Vector(
          NeuropublishHardAssignmentV1(
            localId = "volume-hard-assignment",
            source = volumeIdentity,
            target = parcelIdentity,
            targetOrdinals = Vector(0, 1, 0, 1),
            coverage = NeuropublishTargetCoverageV1.Complete,
            provenance = assignmentProvenance
          )
        )
      )
