package scalafim.atlas

import locus4s.data.Field
import locus4s.data.VectorField
import scalafim.image.SampleSpaces

class AtlasGroupingSuite extends munit.FunSuite:
  private val ref = AtlasRef.volume("grouping", "Unequal", SpaceId.Custom, SpaceId.Custom)
  private val visualA = AtlasRegionMetadata.fromStrings(
    RegionId(1), "Visual A", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("visual"))
  )
  private val visualB = AtlasRegionMetadata.fromStrings(
    RegionId(2), "Visual B", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("visual"))
  )
  private val unannotated = AtlasRegionMetadata.fromStrings(
    RegionId(3), "Unknown network", hemisphere = Some(Hemisphere.Right)
  )
  private val default = AtlasRegionMetadata.fromStrings(
    RegionId(4), "Default", hemisphere = Some(Hemisphere.Right), network = Some(NetworkId("default"))
  )

  private def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      RegionIndex(Vector(visualB, default, visualA, unannotated)),
      AtlasTestImages.labelVolume(
        SampleSpaces(Vector(7, 1, 1)),
        Array(1, 1, 1, 2, 3, 3, 4)
      )
    )

  private def values(realization: AtlasRealization): Field[realization.P, Double] =
    ParcelFields.fromSourceIds(realization)(
      realization,
      Vector(
        RegionId(1) -> 0.0,
        RegionId(2) -> 10.0,
        RegionId(3) -> 20.0,
        RegionId(4) -> 100.0
      )
    ).fold(error => fail(error.message), identity)

  private def at[G](groups: Field[G, AtlasGroupId], output: Field[G, Double], id: String): Double =
    val point = groups.space.indices.find(index => groups(index).value == id)
      .getOrElse(fail(s"missing group $id"))
    output(point)

  private def groupFor[P, G](grouping: AtlasGrouping[P], id: RegionId): Option[String] =
    grouping.parent.parcelPoint(id)
      .flatMap(grouping.parcelToGroup.apply)
      .map(grouping.groupIds.apply)
      .map(_.value)

  test("hemisphere grouping retains the parent parcel owner and distinguishes parcel from spatial weighting"):
    val source = atlas().realization
    val grouping = AtlasGrouping.hemisphere(source).fold(error => fail(error.message), identity)
    val input: Field[source.P, Double] = values(source)
    assert(grouping.parent.parcelDomain.sameRuntimeOwnerAs(source.parcelDomain))
    assertEquals(grouping.groupIds.toVector.map(_.value).toSet, Set("hemisphere:left", "hemisphere:right"))
    assertEquals(
      ParcelFields.records(source)(grouping.spatialSupportCounts).toOption.get.map(record => record.region.id -> record.value).toMap,
      Map(RegionId(1) -> 3L, RegionId(2) -> 1L, RegionId(3) -> 2L, RegionId(4) -> 1L)
    )
    assertEquals(groupFor(grouping, RegionId(1)), Some("hemisphere:left"))
    assertEquals(groupFor(grouping, RegionId(2)), Some("hemisphere:left"))
    assertEquals(groupFor(grouping, RegionId(3)), Some("hemisphere:right"))
    assertEquals(groupFor(grouping, RegionId(4)), Some("hemisphere:right"))

    val equal = AtlasGrouping.mean(grouping)(input, AtlasGroupingWeight.EqualParcels).fold(error => fail(error.message), identity)
    val spatial = AtlasGrouping.mean(grouping)(input, AtlasGroupingWeight.SpatialSupport).fold(error => fail(error.message), identity)
    assertEqualsDouble(at(grouping.groupIds, equal.values, "hemisphere:left"), 5.0, 0.0)
    assertEqualsDouble(at(grouping.groupIds, equal.values, "hemisphere:right"), 60.0, 0.0)
    // Independent support-weighted expectations: (3 * 0 + 1 * 10) / 4 and (2 * 20 + 1 * 100) / 3.
    assertEqualsDouble(at(grouping.groupIds, spatial.values, "hemisphere:left"), 2.5, 0.0)
    assertEqualsDouble(at(grouping.groupIds, spatial.values, "hemisphere:right"), 140.0 / 3.0, 1e-14)
    assertEquals(spatial.provenance.parent, source.identity)
    assertEquals(spatial.provenance.weighting, AtlasGroupingWeight.SpatialSupport)
    assertEquals(grouping.atlasProvenance(AtlasGroupingWeight.SpatialSupport).derivation.last,
      DerivationStep.GroupedParcels(source.identity, "hemisphere", "reject", "spatial-support"))

  test("network grouping reports absent annotations or retains only known partial assignments"):
    val source = atlas().realization
    AtlasGrouping.network(source) match
      case Left(AtlasGroupingError.MissingAnnotations(AtlasGroupingKind.Network, parcels)) =>
        assertEquals(parcels.map(_.value), Vector(source.parcelKeys(source.parcelPoint(RegionId(3)).get)))
      case other => fail(s"expected missing network annotation, got $other")

    val grouping = AtlasGrouping.network(source, MissingGroupingAnnotationPolicy.Drop)
      .fold(error => fail(error.message), identity)
    val input: Field[source.P, Double] = values(source)
    assertEquals(grouping.groupIds.toVector.map(_.value).toSet, Set("network:visual", "network:default"))
    assertEquals(groupFor(grouping, RegionId(1)), Some("network:visual"))
    assertEquals(groupFor(grouping, RegionId(2)), Some("network:visual"))
    assertEquals(groupFor(grouping, RegionId(3)), None)
    assertEquals(groupFor(grouping, RegionId(4)), Some("network:default"))
    val equal = AtlasGrouping.mean(grouping)(input, AtlasGroupingWeight.EqualParcels).fold(error => fail(error.message), identity)
    val spatial = AtlasGrouping.mean(grouping)(input, AtlasGroupingWeight.SpatialSupport).fold(error => fail(error.message), identity)
    assertEqualsDouble(at(grouping.groupIds, equal.values, "network:visual"), 5.0, 0.0)
    assertEqualsDouble(at(grouping.groupIds, spatial.values, "network:visual"), 2.5, 0.0)
    assertEqualsDouble(at(grouping.groupIds, equal.values, "network:default"), 100.0, 0.0)
    assertEquals(spatial.provenance.missingAnnotations, MissingGroupingAnnotationPolicy.Drop)

  test("grouping rejects a foreign live parcel owner and non-finite parcel values"):
    val source = atlas().realization
    val grouping = AtlasGrouping.hemisphere(source).fold(error => fail(error.message), identity)
    val foreign = atlas().realization
    val wrong = VectorField.tabulate(foreign.parcelDomain)(_ => 1.0).asInstanceOf[Field[source.P, Double]]
    assert(AtlasGrouping.mean(grouping)(wrong, AtlasGroupingWeight.EqualParcels).swap.toOption.get
      .isInstanceOf[AtlasGroupingError.ParcelOwner])
    val nonFinite = values(source).map(value => if value == 10.0 then Double.NaN else value)
    AtlasGrouping.mean(grouping)(nonFinite, AtlasGroupingWeight.EqualParcels) match
      case Left(AtlasGroupingError.NonFiniteParcelValue(key, value)) =>
        assertEquals(key.value, source.parcelKeys(source.parcelPoint(RegionId(2)).get))
        assert(value.isNaN)
      case other => fail(s"expected non-finite-value rejection, got $other")
