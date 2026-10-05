package scalafim.atlas

import image4s.geometry.{Affine, D3}
import locus4s.PartialMap
import locus4s.data.VectorField
import scalafim.image.{SampleSpaces, SomeSampleSpace}
import scalafim.image.world.WorldSpace
import scalafim.atlas.fixtures.AtlasCompositionFixture

class AtlasComposeSuite extends munit.FunSuite:
  private val world = right(WorldSpace.declare("atlas composition fixture"))
  private def space(size: Int, origin: Double = 0.0, spacing: Double = 1.0): SomeSampleSpace =
    right(SampleSpaces.inWorld(SampleSpaces(Vector(size, 1, 1),
      spacing = Some(Vector(spacing, 2.0, 3.0)), origin = Some(Vector(origin, 0.0, 0.0))), world))
  private val shared = space(6)
  private def region(id: Int, label: String, network: Option[String] = None): AtlasRegionMetadata =
    AtlasRegionMetadata.fromStrings(RegionId(id), label, hemisphere = Some(Hemisphere.Left),
      network = network.map(NetworkId.apply), attributes = Map("annotation" -> label))
  private def atlas(model: String, labels: Vector[Int], regions: Vector[AtlasRegionMetadata],
      sampling: SomeSampleSpace = shared, coordinate: VolumeOrUnknownSpaceId = SpaceId.Custom,
      template: VolumeOrUnknownSpaceId = SpaceId.Custom): VolumeAtlas =
    val ref = AtlasRef.volume("compose-test", model, template, coordinate, confidence = Confidence.High)
    VolumeAtlas.fromLabelVolume(ref, RegionIndex(regions), AtlasTestImages.labelVolume(sampling, labels.toArray))
  private def parents(): (VolumeAtlas, VolumeAtlas) =
    (atlas("first", Vector(7, 7, 19, 0, 0, 0), Vector(region(7, "A", Some("1")), region(19, "B", Some("1")))),
      atlas("second", Vector(0, 7, 0, 7, 19, 0), Vector(region(7, "C", Some("1")), region(19, "D", Some("1")))))
  private def labels(atlas: VolumeAtlas): Vector[Int] =
    val rendered = atlas.labelVolume
    Vector.tabulate(atlas.realization.domain.space.size)(AtlasTestImages.labelAtCanonicalOrdinal(rendered, _))
  private def right[E, A](result: Either[E, A]): A = result.fold(e => fail(e.toString), identity)

  test("explicit overlap policies compose assignments and preserve the exact first owner"):
    val (a, b) = parents()
    val first = a.realization
    val second = b.realization
    val preferred = right(AtlasCompose.volume(first, second, AtlasCompositionOverlap.PreferFirst, AtlasCompositionOccluded.Reject))
    val exact: VolumeAtlasRealization { type F = first.F; type X = first.X; type P = preferred.P } = preferred.realization
    val firstMap: PartialMap[first.P, preferred.P] = preferred.firstRemap
    val secondMap: PartialMap[second.P, preferred.P] = preferred.secondRemap
    assert(exact.domain eq first.domain)
    assert(exact.domain.grid eq first.domain.grid)
    assert(firstMap.from.sameRuntimeOwnerAs(first.parcelDomain))
    assert(secondMap.from.sameRuntimeOwnerAs(second.parcelDomain))
    assert(firstMap.to.sameRuntimeOwnerAs(exact.parcelDomain))
    assertEquals(labels(preferred.atlas), Vector(1, 1, 2, 3, 4, 0))
    assertEquals(firstMap.optionalTargetOrdinals, Vector(Some(0), Some(1)))
    assertEquals(secondMap.optionalTargetOrdinals, Vector(Some(2), Some(3)))
    assertEquals(exact.metadata.toVector.map(_.label.value), Vector("A", "B", "C", "D"))
    assertEquals(exact.metadata.toVector.map(_.attributes.toMap), first.metadata.toVector.map(_.attributes.toMap) ++ second.metadata.toVector.map(_.attributes.toMap))
    val preferredSecond = right(AtlasCompose.volume(first, second, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    assertEquals(labels(preferredSecond.atlas), Vector(1, 3, 2, 3, 4, 0))
    val rejected = AtlasCompose.volume(first, second, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Drop)
    assertEquals(rejected.swap.toOption.get, AtlasCompositionError.Overlap(1, first.parcelDomainRecord.elementKeys(0), second.parcelDomainRecord.elementKeys(0)))
    assertEquals(labels(a), Vector(7, 7, 19, 0, 0, 0))
    assertEquals(labels(b), Vector(0, 7, 0, 7, 19, 0))
    // Correspondence is not the resulting spatial assignment after partial occlusion.
    val voxel = first.domain.space.indexAtValidatedOrdinal(1)
    assertNotEquals(first.parcelAssignment(voxel).flatMap(preferredSecond.firstRemap.apply), preferredSecond.realization.parcelAssignment(voxel))

  test("equal network names are scoped by parent instead of silently grouped"):
    val (a, b) = parents()
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    assertEquals(c.realization.metadata.toVector.map(_.network.map(_.value)), Vector(Some("first:1:1"), Some("first:1:1"), Some("second:1:1"), Some("second:1:1")))
    val network = c.realization.networkAssignment.get
    assertEquals(network.networkIds.toVector.map(_.value).toSet, Set("first:1:1", "second:1:1"))
    val subset = right(c.atlas.subsetEither(_.label.value != "D"))
    assertEquals(subset.realization.networkAssignment.get.networkIds.toVector.map(_.value).toSet, Set("first:1:1", "second:1:1"))

  test("fully occluded parcels require a policy and compact ids cannot collapse distinct ancestry"):
    val s = space(2)
    val a = atlas("duplicates", Vector(1, 2), Vector(region(1, "X"), region(2, "X")), s)
    val b = atlas("cover", Vector(0, 9), Vector(region(9, "Y")), s)
    val other = atlas("cover", Vector(9, 0), Vector(region(9, "Y")), s)
    val first = a.realization
    val reject = AtlasCompose.volume(first, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject)
    assertEquals(reject.swap.toOption.get, AtlasCompositionError.Occluded(Vector(first.parcelDomainRecord.elementKeys(1)), Vector.empty))
    val c = right(AtlasCompose.volume(first, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Drop))
    val d = right(AtlasCompose.volume(first, other.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Drop))
    assertEquals(c.realization.metadata.toVector.map(_.label.value), d.realization.metadata.toVector.map(_.label.value))
    assertNotEquals(c.realization.parcelDomainRecord.identity, d.realization.parcelDomainRecord.identity)
    assertEquals(c.firstRemap.optionalTargetOrdinals, Vector(Some(0), None))
    assertEquals(d.firstRemap.optionalTargetOrdinals, Vector(None, Some(0)))
    assertEquals(labels(c.atlas), Vector(1, 2))
    assertEquals(labels(d.atlas), Vector(2, 1))
    val same = right(AtlasCompose.volume(first, first, AtlasCompositionOverlap.PreferFirst, AtlasCompositionOccluded.Drop))
    assertEquals(same.secondRemap.optionalTargetOrdinals, Vector(None, None))
    assertEquals(labels(same.atlas), Vector(1, 2))

  test("disjoint composition accepts separately admitted exact grids and large overlapping source ids"):
    val a = atlas("a", Vector(Int.MaxValue, 0), Vector(region(Int.MaxValue, "same")), space(2))
    val b = atlas("b", Vector(0, Int.MaxValue), Vector(region(Int.MaxValue, "same")), space(2))
    assert(!(a.realization.domain.grid eq b.realization.domain.grid))
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject))
    assertEquals(labels(c.atlas), Vector(1, 2))
    assertEquals(c.atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    val reverse = right(AtlasCompose.volume(b.realization, a.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject))
    assertNotEquals(c.realization.parcelDomainRecord.identity, reverse.realization.parcelDomainRecord.identity)

  test("composition rejects shifted, anisotropic, reshaped and foreign-frame grids and conflicting declarations"):
    val a = atlas("a", Vector(1, 0), Vector(region(1, "A")), space(2))
    val geometries = Vector(space(2, origin = 0.1), space(2, spacing = 1.1),
      right(SampleSpaces.inWorld(SampleSpaces(Vector(1, 2, 1)), world)), SampleSpaces(Vector(2, 1, 1)))
    geometries.foreach: s =>
      val b = atlas("b", Vector(0, 2), Vector(region(2, "B")), s)
      assert(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject)
        .swap.toOption.get.isInstanceOf[AtlasCompositionError.Geometry])
    val b = atlas("b", Vector(0, 2), Vector(region(2, "B")), space(2), coordinate = SpaceId.MNI305)
    assert(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject)
      .swap.toOption.get.isInstanceOf[AtlasCompositionError.ConflictingSpaces])
    val differentTemplate = atlas("b", Vector(0, 2), Vector(region(2, "B")), space(2), template = SpaceId.MNI152)
    assert(AtlasCompose.volume(a.realization, differentTemplate.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject).isLeft)

  test("equal cube dimensions do not admit permuted axes"):
    val diagonal = right(SampleSpaces.inWorld(SampleSpaces(Vector(2, 2, 2)), world))
    val permutation = right(Affine.fromRowMajor[D3](Vector(
      0.0, 1.0, 0.0, 0.0,
      1.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    )))
    val permuted = right(SampleSpaces.inWorld(SampleSpaces(Vector(2, 2, 2), affine = Some(permutation)), world))
    val a = atlas("a", Vector(1, 0, 0, 0, 0, 0, 0, 0), Vector(region(1, "A")), diagonal)
    val b = atlas("b", Vector(0, 0, 0, 0, 0, 0, 0, 2), Vector(region(2, "B")), permuted)
    assert(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject)
      .swap.toOption.get.isInstanceOf[AtlasCompositionError.Geometry])

  test("both scoped parent provenances survive publication, nested composition, selection and metric persistence"):
    val (a, b) = parents()
    def withArtifact(parent: VolumeAtlas, uri: String): VolumeAtlas =
      val source = SourceArtifact("same-id", ArtifactRole.LabelTable, "synthetic", "same-ref", sourceUri = Some(uri),
        licenseInfo = LicenseInfo.Known("CC0"), digest = Some(Digest.sha256("a" * 64)))
      VolumeAtlas.fromLabelVolume(parent.ref, parent.regions, parent.labelVolume,
        parent.provenance.withSourceArtifacts(Vector(source)).copy(labels = parent.provenance.labels.copy(labelTableArtifactId = Some("same-id"))))
    val aa = withArtifact(a, "https://example.test/a")
    val bb = withArtifact(b, "https://example.test/b")
    val c = right(AtlasCompose.volume(aa.realization, bb.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    val projection = c.realization.neuropublishProjection
    right(c.realization.validateNeuropublishProjection(projection))
    val step = projection.atlasProvenance.derivation.last.asInstanceOf[NeuropublishAtlasDerivationV1.ComposedParcels]
    assertEquals(step.first.provenance, aa.realization.neuropublishProjection.atlasProvenance)
    assertEquals(step.second.provenance, bb.realization.neuropublishProjection.atlasProvenance)
    assertEquals(step.first.parcelDomain, aa.realization.parcelDomainRecord)
    assertEquals(step.second.assignmentDigests.map(_.value), bb.realization.identity.assignmentDigests.map(_.value))
    assertEquals(step.first.provenance.sourceArtifacts.head.id, step.second.provenance.sourceArtifacts.head.id)
    assertNotEquals(step.first.provenance.sourceArtifacts.head.sourceUri, step.second.provenance.sourceArtifacts.head.sourceUri)
    val r = c.realization
    val values = VectorField.tabulate(r.parcelDomain)(p => p.ordinal + 0.5)
    val schema = right(ParcelMetricSchema.from("composition statistic"))
    val document = right(ParcelMetricJson.encode(r)(values, schema))
    val restored = right(ParcelMetricJson.decode(r)(document))
    assertEquals(restored.origin.provenance, projection.atlasProvenance)
    assertEquals(restored.values.toVector, values.toVector)
    val selected = right(c.atlas.subsetEither(_.label.value == "A"))
    right(selected.realization.validateNeuropublishProjection(selected.realization.neuropublishProjection))
    val sr = selected.realization
    right(ParcelMetricJson.encode(sr)(VectorField.tabulate(sr.parcelDomain)(_ => 3.0), schema))
    val nested = right(AtlasCompose.volume(r, aa.realization, AtlasCompositionOverlap.PreferFirst, AtlasCompositionOccluded.Drop))
    val nr = nested.realization
    val nestedDocument = right(ParcelMetricJson.encode(nr)(VectorField.tabulate(nr.parcelDomain)(_ => 1.0), schema))
    assertEquals(right(ParcelMetricJson.decode(nr)(nestedDocument)).origin.provenance, nr.neuropublishProjection.atlasProvenance)

  test("origin validation rejects forged parent ancestry and correspondence with recomputed envelope digest"):
    val (a, b) = parents()
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    val r = c.realization
    val document = right(ParcelMetricJson.encode(r)(VectorField.tabulate(r.parcelDomain)(_ => 1.0), right(ParcelMetricSchema.from("metric"))))
    val mutations: Vector[ujson.Value => Unit] = Vector(
      step => step("first")("provenance")("identity")("model") = "foreign",
      step => step("first")("correspondence")(0) = ujson.Null,
      step => step("first")("correspondence")(0) = step("second")("correspondence")(0),
      step => step("first")("assignmentDigests")(0)("value") = "invalid",
      step => step("first")("provenance")("identity")("release") = ujson.Obj("label" -> "bad", "year" -> 1),
      step => step("occluded") = "Drop"
    )
    mutations.foreach: mutate =>
      val envelope = ujson.read(document)
      val payload = ujson.read(envelope("payload").str)
      mutate(payload("origin")("provenance")("derivation").arr.last)
      val body = ujson.write(payload)
      envelope("payload") = body
      envelope("sha256") = ParcelMetricJson.digest(body)
      assert(ParcelMetricJson.decode(r)(ujson.write(envelope)).isLeft)

  test("actual neuroatlas second-parent precedence matches coordinate-keyed ancestry fixtures"):
    val sampling = right(SampleSpaces.inWorld(SampleSpaces(Vector(2, 2, 2),
      spacing = Some(Vector(2.0, 3.0, 4.0)), origin = Some(Vector(-10.0, 5.0, 12.0))), world))
    AtlasCompositionFixture.cases.foreach: fixture =>
      val a = atlas("first", fixture.first, Vector(region(7, "first-A"), region(19, "first-B")), sampling)
      val b = atlas("second", fixture.second, Vector(region(7, "second-A"), region(19, "second-B")), sampling)
      val first = a.realization
      val second = b.realization
      val c = right(AtlasCompose.volume(first, second, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Drop))
      val ancestry = Array.fill(c.realization.parcelDomain.size)(0)
      first.parcelDomain.indices.foreach(p => c.firstRemap(p).foreach(out => ancestry(out.ordinal) = p.ordinal + 1))
      second.parcelDomain.indices.foreach(p => c.secondRemap(p).foreach(out => ancestry(out.ordinal) = p.ordinal + 3))
      val actual = c.realization.domain.space.indices.map(voxel =>
        c.realization.parcelAssignment(voxel).fold(0)(p => ancestry(p.ordinal))).toVector
      assertEquals(actual, fixture.ancestry, fixture.name)
      assert(c.realization.domain eq first.domain)

  test("canonical Glasser ancestry is independent of source numbering and presentation order"):
    val sampling = space(4)
    def glasser(names: Vector[String], ids: Vector[Int], labels: Vector[Int]): VolumeAtlas =
      val regions = names.zip(ids).map: (name, id) =>
        val key = right(GlasserParcelKey.from(name))
        AtlasRegionMetadata.fromStrings(RegionId(id), key.area, Some(name), Some(key.hemisphere))
      VolumeAtlas.fromLabelVolume(GlasserHcpMmp1().atlasRef(), RegionIndex(regions), AtlasTestImages.labelVolume(sampling, labels.toArray))
    val a = glasser(Vector("R_V1_ROI", "L_V1_ROI"), Vector(1, 181), Vector(181, 1, 0, 0))
    val aa = glasser(Vector("L_V1_ROI", "R_V1_ROI"), Vector(7, 19), Vector(7, 19, 0, 0))
    val b = glasser(Vector("R_p24_ROI", "L_p24_ROI"), Vector(360, 180), Vector(0, 0, 180, 360))
    val bb = glasser(Vector("L_p24_ROI", "R_p24_ROI"), Vector(42, 43), Vector(0, 0, 42, 43))
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject))
    val d = right(AtlasCompose.volume(aa.realization, bb.realization, AtlasCompositionOverlap.Reject, AtlasCompositionOccluded.Reject))
    assertEquals(c.realization.parcelDomainRecord, d.realization.parcelDomainRecord)
    assertEquals(labels(c.atlas), labels(d.atlas))
    assertEquals(c.atlas.regions.ids, Vector(RegionId(2), RegionId(1), RegionId(4), RegionId(3)))
    assertEquals(d.atlas.regions.ids, Vector(RegionId(1), RegionId(2), RegionId(3), RegionId(4)))
    val cr = c.realization
    val dr = d.realization
    val values = VectorField.tabulate(cr.parcelDomain)(p => p.ordinal.toDouble)
    val document = right(ParcelMetricJson.encode(cr)(values, right(ParcelMetricSchema.from("canonical metric"))))
    assertEquals(right(ParcelMetricJson.decode(dr)(document)).values.toVector, values.toVector)

  test("parent remaps reject foreign parcel indices at compile time"):
    assert(compileErrors("""
      import scalafim.atlas.*
      import locus4s.PartialMap
      def wrong(first: VolumeAtlasRealization, second: VolumeAtlasRealization)
          (result: VolumeAtlasComposition[first.F, first.X, first.P, second.P]) =
        result.firstRemap(second.parcelDomain.indexAtValidatedOrdinal(0))
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      import locus4s.PartialMap
      def wrong(first: VolumeAtlasRealization, second: VolumeAtlasRealization)
          (result: VolumeAtlasComposition[first.F, first.X, first.P, second.P]): PartialMap[second.P, result.P] =
        result.firstRemap
    """).nonEmpty)

  test("review regressions reject nested nonzero background, surface encoding and foreign support"):
    import ParcelMetricWire.given
    val (a, b) = parents()
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    val r = c.realization
    val document = right(ParcelMetricJson.encode(r)(VectorField.tabulate(r.parcelDomain)(_ => 1.0), right(ParcelMetricSchema.from("metric"))))
    val foreign = atlas("foreign", Vector(1, 1, 1, 1, 1, 1), Vector(region(1, "foreign")), space(6, origin = 5.0))
    val foreignIdentity = ujson.read(upickle.default.write(foreign.realization.volumeSupportDomain))
    val mutations: Vector[ujson.Value => Unit] = Vector(
      step => step("first")("provenance")("labels")("backgroundSourceLabel") = 7,
      step => step("first")("provenance")("labels")("encoding") = "surface-integer-labels",
      step =>
        step("first")("supportDomains")(0) = foreignIdentity
        step("second")("supportDomains")(0) = foreignIdentity
    )
    val accepted = mutations.map: mutate =>
      val envelope = ujson.read(document)
      val payload = ujson.read(envelope("payload").str)
      mutate(payload("origin")("provenance")("derivation").arr.last)
      val body = ujson.write(payload)
      envelope("payload") = body
      envelope("sha256") = ParcelMetricJson.digest(body)
      ParcelMetricJson.decode(r)(ujson.write(envelope)).isRight
    assertEquals(accepted, Vector(false, false, false))

  test("composition selection history and recursive support binding cannot be removed or substituted"):
    import ParcelMetricWire.given
    val (a, b) = parents()
    val c = right(AtlasCompose.volume(a.realization, b.realization, AtlasCompositionOverlap.PreferSecond, AtlasCompositionOccluded.Reject))
    val schema = right(ParcelMetricSchema.from("metric"))
    def encoded(r: AtlasRealization): String = right(ParcelMetricJson.encode(r)(VectorField.tabulate(r.parcelDomain)(_ => 1.0), schema))
    def rewrite(document: String)(f: ujson.Value => Unit): String =
      val envelope = ujson.read(document)
      val payload = ujson.read(envelope("payload").str)
      f(payload)
      val body = ujson.write(payload)
      envelope("payload") = body
      envelope("sha256") = ParcelMetricJson.digest(body)
      ujson.write(envelope)
    val selected = right(c.atlas.subsetEither(_.label.value == "A"))
    val sr = selected.realization
    val document = encoded(sr)
    right(ParcelMetricJson.decode(sr)(document))
    val removed = rewrite(document)(p => p("origin")("provenance")("derivation").arr.remove(2))
    assert(ParcelMetricJson.decode(sr)(removed).isLeft)
    val foreignPartition = rewrite(document)(p => p("origin")("provenance")("derivation").arr.last("kept")(0) = "foreign")
    assert(ParcelMetricJson.decode(sr)(foreignPartition).isLeft)
    val twiceSelected = right(selected.subsetEither(_ => true))
    val tr = twiceSelected.realization
    right(ParcelMetricJson.decode(tr)(encoded(tr)))
    val nested = right(AtlasCompose.volume(c.realization, a.realization, AtlasCompositionOverlap.PreferFirst, AtlasCompositionOccluded.Drop))
    val nr = nested.realization
    val foreign = atlas("foreign", Vector(1, 1, 1, 1, 1, 1), Vector(region(1, "foreign")), space(6, origin = 5.0))
    val grid = ujson.read(upickle.default.write(foreign.realization.volumeSupportDomain))
    val changed = rewrite(encoded(nr)): p =>
      val inner = p("origin")("provenance")("derivation").arr.last("first")("provenance")("derivation").arr.last
      inner("first")("supportDomains")(0) = grid
      inner("second")("supportDomains")(0) = grid
    assert(ParcelMetricJson.decode(nr)(changed).isLeft)
