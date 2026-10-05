package scalafim.atlas

import scalafim.atlas.syntax.*
import scalafim.image.SampleSpaces

class AtlasInvariantSuite extends munit.FunSuite:
  private val ref = AtlasRef.volume("invariants", "Tiny", SpaceId.Custom, SpaceId.MNI305)
  private val a = AtlasRegionMetadata.fromStrings(RegionId(1), "A", hemisphere = Some(Hemisphere.Left))
  private val b = AtlasRegionMetadata.fromStrings(RegionId(2), "B", hemisphere = Some(Hemisphere.Right))

  private def atlas(labels: Array[Int] = Array(1, 2, 2, 0)): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      RegionIndex(Vector(b, a)),
      AtlasTestImages.labelVolume(SampleSpaces(dims = Vector(4, 1, 1)), labels)
    )

  test("reference constructors and updates reject contradictory types at compile time"):
    assert(compileErrors("""
      import scalafim.atlas.*
      val ref: VolumeAtlasRef = AtlasRef.surface("x", "y", SpaceId.FsLR32k, SpaceId.FsLR32k)
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      AtlasRef.volume("x", "y", SpaceId.FsLR32k, SpaceId.MNI152)
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      GlasserHcpMmp1().atlasRef().copy(representation = AtlasRepresentation.Surface)
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      new AtlasRef.Volume(AtlasDetails("x", "y"), SpaceId.MNI152, SpaceId.MNI152)
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      new SpaceId.Volume("fsLR_32k")
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      GlasserHcpMmp1().atlasRef().withDetails(_.copy(coordSpace = SpaceId.FsLR32k))
    """).nonEmpty)

  test("realizations cannot be implemented outside the checked construction boundary"):
    assert(compileErrors("""
      import scalafim.atlas.*
      trait Forged extends AtlasRealization
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      trait ForgedVolume extends VolumeAtlasRealization
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      trait ForgedSurface extends SurfaceAtlasRealization
    """).nonEmpty)

  test("metadata updates preserve the concrete reference type and fixed spaces"):
    val updated: VolumeAtlasRef = ref.withDetails(_.copy(notes = Some("checked"), confidence = Confidence.Exact))
    val coord: VolumeOrUnknownSpaceId = updated.coordSpace
    assertEquals(updated.representation, AtlasRepresentation.Volume)
    assertEquals(coord, ref.coordSpace)
    assertEquals(updated.templateSpace, ref.templateSpace)
    assertEquals(updated.notes, Some("checked"))
    intercept[IllegalArgumentException](ref.withDetails(_.copy(family = " ")))

  test("checked space parsing canonicalizes known aliases and refuses opposite kinds"):
    assertEquals(SpaceId.surfaceFrom("FS-LR_32k"), Right(SpaceId.FsLR32k))
    assertEquals(SpaceId.volumeFrom("mni305"), Right(SpaceId.MNI305))
    assertEquals(SpaceId.volumeFrom("fsLR_32k"), Left(AtlasError.SpaceKindMismatch(SpaceId.FsLR32k, SpaceKindTag.Volume, SpaceKindTag.Surface)))
    assertEquals(SpaceId.surfaceFrom("MNI305"), Left(AtlasError.SpaceKindMismatch(SpaceId.MNI305, SpaceKindTag.Surface, SpaceKindTag.Volume)))
    assert(SpaceId.unknownFrom("MNI152").isLeft)
    assert(SpaceId.from(" ").isLeft)
    intercept[IllegalArgumentException](SpaceId.volume("fsaverage"))
    val custom = SpaceId.volume("subject-native")
    assertEquals(SpaceId.normalize(custom), custom)
    assertEquals(SpaceId.kind(custom), SpaceKindTag.Volume)
    assertEquals(SpaceId.asVolume(custom), Right(custom))
    assertEquals(SpaceId.kind(SpaceId("subject-native")), SpaceKindTag.Unknown)
    val explicitlyClassified = SpaceId.volume("custom")
    assertEquals(SpaceId.normalize(explicitlyClassified), explicitlyClassified)

  test("dynamic reference admission checks both coordinate and template kinds"):
    val details = AtlasDetails("parsed", "custom")
    assert(AtlasRef.checked(details, AtlasRepresentation.Volume, SpaceId.FsAverage, SpaceId.MNI152).isLeft)
    assert(AtlasRef.checked(details, AtlasRepresentation.Surface, SpaceId.FsAverage, SpaceId.MNI152).isLeft)
    assert(AtlasRef.checked(details, AtlasRepresentation.Volume, SpaceId.Custom, SpaceId.MNI152).isRight)
    assert(AtlasRef.checked(details, AtlasRepresentation.Derived, SpaceId.FsAverage, SpaceId.MNI152).isRight)

  test("metadata stores checked values and rejects invalid strings at construction"):
    val metadata = AtlasRegionMetadata.checked(RegionId(1), "A", Some("Full A"), attributes = Map("source" -> "fixture")).toOption.get
    assertEquals(metadata.label, RegionLabel.unsafe("A"))
    assertEquals(metadata.fullLabel, RegionLabel.unsafe("Full A"))
    assertEquals(metadata.attributes.toMap, Map("source" -> "fixture"))
    assert(AtlasRegionMetadata.checked(RegionId(1), " ").isLeft)
    assert(AtlasRegionMetadata.checked(RegionId(1), "A", Some(" ")).isLeft)
    assert(AtlasRegionMetadata.checked(RegionId(1), "A", attributes = Map(" " -> "x")).isLeft)
    intercept[IllegalArgumentException](AtlasRegionMetadata.fromStrings(RegionId(1), "A", Some(" ")))
    assert(compileErrors("""
      import scalafim.atlas.*
      RegionLabel.unsafe("valid").copy(value = " ")
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      RegionAttributeKey.unsafe("valid").copy(value = " ")
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      AtlasRegionMetadata(RegionId(1), "unchecked")
    """).nonEmpty)

  test("parcel imports reject duplicate, missing and unknown source IDs"):
    val source = atlas()
    val r = source.realization
    val va = a.id -> 10.0
    val vb = b.id -> 20.0
    assertEquals(ParcelFields.fromSourceIds(r)(r, Vector(va, a.id -> 999.0, vb)), Left(ParcelFieldError.DuplicateSourceIds(Vector(a.id))))
    assert(ParcelFields.fromSourceIds(r)(r, Vector(va)).swap.toOption.get.isInstanceOf[ParcelFieldError.MissingKeys])
    assertEquals(ParcelFields.fromSourceIds(r)(r, Vector(va, vb, RegionId(3) -> 30.0)), Left(ParcelFieldError.UnknownSourceIds(Vector(RegionId(3)))))
    val checked = ParcelFields.fromSourceIds(r)(r, Vector(va, vb)).toOption.get
    val records = ParcelFields.records(r)(checked).toOption.get
    assertEquals(records.map(_.region), Vector(b, a))
    assertEqualsDouble(checked(r.parcelPoint(a.id).get), 10.0, 0.0)

  test("direct and extension queries use the atlas native coordinates by default"):
    val source = atlas()
    val point = Point3D.Origin
    val native = source.query(point).head
    assertEquals(AtlasQuery.exact(source, point), native)
    assertEquals(AtlasQuery.exactEither(source, point), Right(native))
    assertEquals(AtlasQuery.query(source, Vector(point)), Vector(native))
    assertEquals(AtlasQuery.queryEither(source, Vector(point)), Right(Vector(native)))
    assertEquals(native.atlasPoint, point)
    assertEquals(AtlasQuery.query(source, Vector(point), radiusMm = 1.1), source.query(point, radiusMm = 1.1))
    val transformed = AtlasQuery.exact(source, point, SpaceId.MNI152)
    assertEquals(transformed, source.query(point, fromSpace = SpaceId.MNI152).head)
    assert(transformed.atlasPoint != point)
    assertEquals(AtlasQuery.queryEither(source, Vector(point), fromSpace = SpaceId.MNI152), source.queryEither(point, fromSpace = SpaceId.MNI152))

  test("selection preserves exact spatial ownership and records canonical parent evidence"):
    val source = atlas()
    val selected = source.subsetEither(_.id == b.id).toOption.get
    assertEquals(selected.ref, source.ref)
    assertEquals(selected.provenance.identity, source.provenance.identity)
    assert(selected.realization.domain.space.sameRuntimeOwnerAs(source.realization.domain.space))
    assertEquals(selected.realization.neuropublishSupportDomains, source.realization.neuropublishSupportDomains)
    assertEquals(selected.regions.ids, Vector(b.id))
    val childLabels = selected.labelVolume
    assertEquals((0 until 4).map(AtlasTestImages.labelAtCanonicalOrdinal(childLabels, _)).toVector, Vector(0, 2, 2, 0))
    selected.provenance.derivation.last match
      case DerivationStep.SelectedParcels(parent, kept, dropped) =>
        assertEquals(parent, source.realization.identity)
        assertEquals(kept, source.realization.displayOrder.indices.filter(p => source.realization.metadata(p).id == b.id).map(source.realization.parcelKeys.apply).toVector)
        assertEquals(kept, selected.realization.neuropublishProjection.displayOrder)
        assertEquals(dropped.size, 1)
      case other => fail(s"expected selection derivation, got $other")
    assert(selected.realization.neuropublishProjection.atlasProvenance.derivation.last.isInstanceOf[NeuropublishAtlasDerivationV1.SelectedParcels])
    val data = AtlasTestImages.scalarVolume(source, Array(10.0, 20.0, 30.0, 1000.0))
    assertEqualsDouble(selected.reduceEither(data).toOption.get(selected.realization.parcelPoint(b.id).get), 25.0, 0.0)
    assert(source.reduceEither(data).isRight)
    val originalGrid = source.realization.domain.grid
    val independentGrid = image4s.geometry.Grid.in[image4s.geometry.D3](originalGrid.frame)(
      originalGrid.shape, originalGrid.indexToFrame
    ).toOption.get
    val independentSpace = image4s.SampleSpace.create(independentGrid, image4s.NonSpatialAxes.empty)
    val foreignData = scalafim.image.NeuroVolume.copyContinuousFromCanonicalArray(
      independentSpace, Array(10.0, 20.0, 30.0, 1000.0), image4s.ImageMetadata.empty
    ).map(scalafim.image.SomeNeuroVolume.eraseSpace).toOption.get
    assertEquals(independentGrid.shape, originalGrid.shape)
    assertEquals(independentGrid.indexToFrame, originalGrid.indexToFrame)
    assert(source.reduceEither(foreignData).swap.toOption.get.isInstanceOf[AtlasError.ExactGridRequired])
    assert(selected.reduceEither(foreignData).swap.toOption.get.isInstanceOf[AtlasError.ExactGridRequired])
    val reindexed = source.subset(_.id == a.id)
    assertEquals(reindexed.realization.parcelDomain.size, 1)
    assertEquals((0 until 4).map(AtlasTestImages.labelAtCanonicalOrdinal(reindexed.labelVolume, _)).toVector, Vector(1, 0, 0, 0))

  test("empty selection is a typed error and selection identity distinguishes actual assignments"):
    val source = atlas()
    assertEquals(source.subsetEither(_ => false), Left(AtlasRealizationError.InvalidAtlas(AtlasError.EmptyAtlas)))
    intercept[IllegalArgumentException](source.subset(_ => false))
    val changed = atlas(Array(2, 1, 2, 0))
    assertEquals(source.realization.identity.parcelDomain, changed.realization.identity.parcelDomain)
    assertEquals(source.realization.identity.supportDomains, changed.realization.identity.supportDomains)
    assert(source.realization.identity.assignmentDigests != changed.realization.identity.assignmentDigests)
    val all = source.subsetEither(_ => true).toOption.get
    assertEquals(all.realization.identity, source.realization.identity)
    assertEquals(all.provenance.derivation.size, source.provenance.derivation.size + 1)
