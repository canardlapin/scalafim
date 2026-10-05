package scalafim.atlas

import scalafim.image.SampleSpaces

class JulichVisualSuite extends munit.FunSuite:
  test("anatomical visual selection retains exact assignments and parent evidence"):
    val regions = RegionIndex(Vector(
      AtlasRegionMetadata.fromStrings(RegionId(81), "GM Visual cortex V1 BA17 L", hemisphere = Some(Hemisphere.Left)),
      AtlasRegionMetadata.fromStrings(RegionId(85), "GM Visual cortex V3V L", hemisphere = Some(Hemisphere.Left)),
      AtlasRegionMetadata.fromStrings(RegionId(1), "GM unrelated L")))
    val parent = VolumeAtlas.fromLabelVolume(JulichVisualRequest.source.ref, regions,
      AtlasTestImages.labelVolume(SampleSpaces(Vector(4, 1, 1)), Array(81, 1, 85, 0)))
    val selected = JulichVisualRequest.select(parent).fold(e => fail(e.message), identity)
    assert(selected.realization.domain eq parent.realization.domain)
    assertEquals(selected.regions.ids.toSet, Set(RegionId(81), RegionId(85)))
    assertEquals(selected.regions.regions.flatMap(JulichVisualRequest.area), Vector("V1", "V3V"))
    assertEquals(selected.realization.support.ordinalsInDomainOrder.toVector, Vector(0, 2))
    assertEquals(selected.provenance.derivation.last.asInstanceOf[DerivationStep.SelectedParcels].parent, parent.realization.identity)
    assert(JulichVisualRequest.select(VolumeAtlas.fromLabelVolume(
      AtlasRef.volume("foreign", "foreign", SpaceId.MNI152, SpaceId.MNI152), regions, parent.labelVolume)).isLeft)
