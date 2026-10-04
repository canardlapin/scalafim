package scalafim.atlas

import locus4s.DomainRegistry
import locus4s.data.VectorField
import ravel.DType.given
import scalafim.image.{NeuroVolume, SampleSpaces, SomeNeuroVolume}
import scalafim.surface.{Hemisphere as SurfaceHemisphere, HemispherePair, LabelInfo, LabeledSurface, SurfaceGeometry, SurfaceKind, TriangleMesh, VertexId}

class ParcelIdentitySuite extends munit.FunSuite:
  // Independent edge fixture from neuroatlas d65ff97:
  // tests/testthat/test-glasser-parcel-identity.R, volume hcp_R_first and
  // surface surfatlas_L_first. Keep expected anatomy/values literal.
  private val volumeRegions = RegionIndex(Vector(
    region(1, "R_V1_ROI", Hemisphere.Right),
    region(180, "R_p24_ROI", Hemisphere.Right),
    region(181, "L_V1_ROI", Hemisphere.Left),
    region(360, "L_p24_ROI", Hemisphere.Left)
  ))
  private val surfaceRegions = RegionIndex(Vector(
    region(1, "L_V1_ROI", Hemisphere.Left),
    region(180, "L_p24_ROI", Hemisphere.Left),
    region(181, "R_V1_ROI", Hemisphere.Right),
    region(360, "R_p24_ROI", Hemisphere.Right)
  ))
  private val volumeRef = GlasserHcpMmp1(GlasserSource.Mni2009c).atlasRef()
  private val surfaceRef = GlasserHcpMmp1Surface().atlasRef()

  test("Glasser source IDs with opposite hemispheres map to the same canonical anatomy"):
    val volume = volumeAtlas(volumeRef, volumeRegions, Array(1, 180, 181, 360))
    val surface = surfaceAtlas(surfaceRef, surfaceRegions)
    val v = volume.realization
    val s = surface.realization
    assert(v.parcelDomain.samePersistentIdentityAs(s.parcelDomain))
    assertNotEquals(v.parcelKeys(v.parcelPoint(RegionId(1)).get), s.parcelKeys(s.parcelPoint(RegionId(1)).get))
    assertEquals(v.parcelKeys(v.parcelPoint(RegionId(1)).get), s.parcelKeys(s.parcelPoint(RegionId(181)).get))

    val source = right(VectorField.fromValues(v.parcelDomain, Vector(181, 360, 1, 180)))
    val transferred = source.rebind(right(v.parcelDomain.align(s.parcelDomain)))
    assertEquals(s.displayOrder.indices.map(transferred.apply).toVector, Vector(181, 360, 1, 180))
    assertEquals(v.neuropublishAssignments.head.targetOrdinals, Vector(2, 3, 0, 1))
    assertEquals(s.neuropublishAssignments.map(_.targetOrdinals), Vector(Vector(0, 1, 1), Vector(2, 3, 3)))
    assertEquals(volume.regions, volumeRegions)
    assertEquals(surface.regions, surfaceRegions)
    assertEquals(volume.labelVolume.copyToCanonicalArray.toVector, Vector(1, 180, 181, 360))
    assertEquals(surface.left.labels.toVector, Vector(1, 180, 180))
    assertEquals(surface.right.labels.toVector, Vector(181, 360, 360))

  test("canonical order survives source renumbering and metadata reordering"):
    val original = volumeAtlas(volumeRef, volumeRegions, Array(1, 180, 181, 360)).realization
    val renumbered = RegionIndex(volumeRegions.regions.reverse.zip(Vector(7, 8, 9, 10)).map:
      (metadata, id) => metadata.copy(id = RegionId(id))
    )
    val atlas = volumeAtlas(volumeRef, renumbered, Array(10, 9, 8, 7))
    val other = atlas.realization
    assert(original.parcelDomain.samePersistentIdentityAs(other.parcelDomain))
    assert(original.assignmentAlignedTo(other.parcelDomain).isRight)
    assertEquals(other.neuropublishAssignments.head.targetOrdinals, Vector(2, 3, 0, 1))
    assertEquals(atlas.regions, renumbered)
    assertEquals(other.neuropublishProjection.displayOrder, original.neuropublishProjection.displayOrder.reverse)
    val map = other.neuropublishAssignments.head.provenance.sourceLabelToParcelKeys.toMap
    assertEquals(map(10), original.parcelKeys(original.parcelPoint(RegionId(1)).get))
    assertEquals(map(7), original.parcelKeys(original.parcelPoint(RegionId(360)).get))

  test("source labels remain distinct when no canonical encoding is declared"):
    val vref = volumeRef.withDetails(details => details.copy(parcelIdentity = ParcelIdentity.SourceLabels))
    val sref = surfaceRef.withDetails(details => details.copy(parcelIdentity = ParcelIdentity.SourceLabels))
    val v = volumeAtlas(vref, volumeRegions, Array(1, 180, 181, 360)).realization
    val s = surfaceAtlas(sref, surfaceRegions).realization
    assert(!v.parcelDomain.samePersistentIdentityAs(s.parcelDomain))
    assert(v.assignmentAlignedTo(s.parcelDomain).isLeft)
    val changedSource = volumeAtlas(vref.withDetails(details => details.copy(source = Some("unknown-encoding"))), volumeRegions, Array(1, 180, 181, 360)).realization
    assert(v.assignmentAlignedTo(changedSource.parcelDomain).isLeft)
    val swapped = volumeAtlas(vref, surfaceRegions, Array(1, 180, 181, 360)).realization
    assert(v.assignmentAlignedTo(swapped.parcelDomain).isLeft)

  test("canonical anatomy and explicitly shared numeric labels are distinct identity schemes"):
    val canonical = volumeAtlas(volumeRef, volumeRegions, Array(1, 180, 181, 360)).realization
    val numeric = volumeAtlas(volumeRef.withDetails(details => details.copy(parcelIdentity = ParcelIdentity.SharedRegionIds)), volumeRegions, Array(1, 180, 181, 360)).realization
    assert(numeric.parcelDomainRecord.elementKeys.forall(_.startsWith("org.scalafim.atlas/parcel/v2:")))
    assert(canonical.parcelDomainRecord.elementKeys.forall(_.startsWith("org.scalafim.atlas/parcel/v2:")))
    assert(canonical.assignmentAlignedTo(numeric.parcelDomain).isLeft)

  test("duplicate full keys fail before an assignment can be published"):
    val duplicate = RegionIndex(Vector(region(1, "L_V1_ROI", Hemisphere.Left), region(2, "L_V1_ROI", Hemisphere.Left)))
    assertEquals(identityError(volumeRef, duplicate), ParcelIdentityError.DuplicateGlasserKeys(Vector(right(GlasserParcelKey.from("L_V1_ROI")))))

  test("missing, malformed, and conflicting hemisphere keys are typed failures"):
    val missing = RegionIndex(Vector(AtlasRegionMetadata.fromStrings(RegionId(1), "V1", hemisphere = Some(Hemisphere.Left))))
    assertEquals(identityError(volumeRef, missing), ParcelIdentityError.MissingGlasserKey(RegionId(1)))
    val malformed = RegionIndex(Vector(region(1, "V1", Hemisphere.Left)))
    assertEquals(identityError(volumeRef, malformed), ParcelIdentityError.InvalidGlasserKey("V1"))
    val conflicting = RegionIndex(Vector(region(1, "R_V1_ROI", Hemisphere.Left)))
    assertEquals(identityError(volumeRef, conflicting), ParcelIdentityError.ConflictingHemisphere(RegionId(1), right(GlasserParcelKey.from("R_V1_ROI")), Hemisphere.Left))
    assert(GlasserParcelKey.from("L__ROI").isLeft)
    assert(GlasserParcelKey.from(" L_V1_ROI").isLeft)
    assert(GlasserParcelKey.from("B_V1_ROI").isLeft)

  test("a canonical Glasser policy cannot be applied to a different atlas family"):
    assertEquals(identityError(volumeRef.withDetails(details => details.copy(family = "other")), volumeRegions), ParcelIdentityError.IncompatibleGlasserAtlas("other", "HCP-MMP1.0"))

  test("canonical domains restore in one registry with exact source-label converters"):
    val first = right(AtlasParcelDomain.restore(DomainRegistry.empty, volumeRef, volumeRef.toProvenance(volumeRegions), volumeRegions))
    val second = right(AtlasParcelDomain.restore(first.registry, surfaceRef, surfaceRef.toProvenance(surfaceRegions), surfaceRegions))
    assert(first.space.sameRuntimeOwnerAs(second.space))
    val v = volumeAtlas(volumeRef, volumeRegions, Array(1, 180, 181, 360)).realization
    val keys = v.neuropublishProjection.parcelDomain.elementKeys
    assertEquals(v.neuropublishAssignments.head.provenance.sourceLabelToParcelKeys, Vector(181 -> keys(0), 360 -> keys(1), 1 -> keys(2), 180 -> keys(3)))
    assert(v.validateNeuropublishProjection(v.neuropublishProjection).isRight)
    val tampered = v.neuropublishProjection.copy(assignments = v.neuropublishAssignments.map:
      assignment =>
        val table = assignment.provenance.sourceLabelToParcelKeys
        assignment.copy(provenance = assignment.provenance.copy(
          sourceLabelToParcelKeys = table.map(_._1).zip(table.map(_._2).reverse)
        ))
    )
    assert(v.validateNeuropublishProjection(tampered).isLeft)

  test("canonical surface keys check payload hemisphere even when metadata omits it"):
    val regions = RegionIndex(surfaceRegions.regions.map(_.copy(hemisphere = None)))
    val payload = surfacePayload(regions, Vector(181, 360, 360), Vector(1, 180, 180))
    val result = AtlasRealization.surfaceIn(DomainRegistry.empty, surfaceRef, regions, payload, surfaceRef.toProvenance(regions))
    assertEquals(result.left.toOption, Some(AtlasRealizationError.Publication(
      AtlasPublicationError.InvalidParcelIdentity(ParcelIdentityError.ConflictingSurfaceHemisphere(
        RegionId(181), right(GlasserParcelKey.from("R_V1_ROI")), Hemisphere.Left
      ))
    )))

  test("network membership and scalar summaries follow canonical parcel ordinals"):
    val regions = RegionIndex(volumeRegions.regions.map:
      metadata => metadata.copy(network = Some(NetworkId(if metadata.hemisphere.contains(Hemisphere.Right) then "RightNet" else "LeftNet")))
    )
    val atlas = volumeAtlas(volumeRef, regions, Array(1, 180, 181, 360))
    val realization = atlas.realization
    assertEquals(realization.networkRegion(NetworkId("RightNet")).get.ordinalsInDomainOrder.toVector, Vector(0, 1))
    assertEquals(realization.networkRegion(NetworkId("LeftNet")).get.ordinalsInDomainOrder.toVector, Vector(2, 3))
    val data = right(NeuroVolume.copyContinuousFromCanonicalArray[Double](atlas.space, Array(10.0, 20.0, 30.0, 40.0)).map(SomeNeuroVolume.eraseSpace))
    val summary = AtlasReduce.summarizeVolume(atlas, data)
    val records = right(ParcelFields.records(atlas.realization)(summary))
    assertEquals(summary.toVector, Vector(30.0, 40.0, 10.0, 20.0))
    records.zip(Vector(10.0, 20.0, 30.0, 40.0)).foreach:
      (actual, expected) => assertEqualsDouble(actual.value, expected, 1e-12)
    assertEquals(records.map(_.region.id.value), Vector(1, 180, 181, 360))

  private def identityError(ref: AtlasRef, regions: RegionIndex): ParcelIdentityError =
    AtlasParcelDomain.restore(DomainRegistry.empty, ref, ref.toProvenance(regions), regions) match
      case Left(AtlasPublicationError.InvalidParcelIdentity(error)) => error
      case other => fail(s"expected parcel identity error, got $other")

  private def region(id: Int, fullLabel: String, hemisphere: Hemisphere): AtlasRegionMetadata =
    AtlasRegionMetadata.fromStrings(RegionId(id), fullLabel.stripPrefix("L_").stripPrefix("R_").stripSuffix("_ROI"), Some(fullLabel), Some(hemisphere))

  private def volumeAtlas(ref: AtlasRef, regions: RegionIndex, data: Array[Int]): VolumeAtlas =
    val space = right(SampleSpaces.requireVolumeD3(SampleSpaces(Vector(data.length, 1, 1))))
    val labels = right(NeuroVolume.copyCategoricalFromCanonicalArray[Int](space, data).map(SomeNeuroVolume.eraseSpace))
    VolumeAtlas.fromLabelVolume(ref, regions, labels)

  private def surfaceAtlas(ref: AtlasRef, regions: RegionIndex): SurfaceAtlas =
    val payload = surfacePayload(regions, Vector(1, 180, 180), Vector(181, 360, 360))
    SurfaceAtlas.fromLabeledSurfaces(ref, regions, payload.left, payload.right)

  private def surfacePayload(regions: RegionIndex, left: Vector[Int], right: Vector[Int]): SurfaceAtlasPayload =
    val mesh = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2)))
    def side(hemisphere: SurfaceHemisphere, ids: Vector[Int]): LabeledSurface =
      LabeledSurface.fromIndexed(SurfaceGeometry(mesh, hemisphere, SurfaceKind.Pial), Vector(VertexId(0), VertexId(1), VertexId(2)), ids, ids.distinct.map(id => LabelInfo(id, regions.requireRegion(RegionId(id)).fullLabel.value)))
    SurfaceAtlasPayload(HemispherePair(side(SurfaceHemisphere.Left, left), side(SurfaceHemisphere.Right, right)))

  private def right[A](result: Either[?, A]): A =
    result.fold(error => fail(error.toString), identity)
