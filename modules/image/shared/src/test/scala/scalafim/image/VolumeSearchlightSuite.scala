package scalafim.image

import scalafim.locus.{Relation, SpaceKey}
import spire.std.int.given

class VolumeSearchlightSuite extends munit.FunSuite:
  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(3, 3, 1)))
  private val packedDomain =
    VolumeDomain.semantic(SpaceKey.unsafe("searchlight:volume"), volumeSpace)
  private type S = packedDomain.S
  private val domain: VolumeDomain[S] = packedDomain.value

  test("radius zero is the identity relation"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get

    assertEquals(
      searchlight.searchlight.neighborhoods,
      Relation.identity(domain.finiteSpace).toOption.get
    )

  test("metric balls are symmetric, monotone, and obey triangle composition"):
    val radiusOne = SearchlightRadius.make(1.0).toOption.get
    val radiusTwo = SearchlightRadius.make(2.0).toOption.get
    val one = VolumeSearchlight.metricBalls(domain, radiusOne).toOption.get.searchlight.neighborhoods
    val two = VolumeSearchlight.metricBalls(domain, radiusTwo).toOption.get.searchlight.neighborhoods

    assertEquals(one, one.converse.toOption.get)
    assert(one.subsetOf(two))
    assert(one.andThen(one).subsetOf(two))

  test("closed metric balls include points exactly on the radius boundary"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val relation =
      VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight.neighborhoods
    val center = domain.finiteSpace.indexOption(4).get

    assertEquals(
      relation.row(center).ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )

  test("relation rows materialize the same full window as the legacy spherical constructor"):
    val values = Array.ofDim[Int](volumeSpace.nVoxels)
    var i = 0
    while i < values.length do
      values(i) = i + 1
      i += 1
    val volume = NeuroVol.fromLinear[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight
    val center = domain.finiteSpace.indexOption(4).get
    val materialized =
      VolumeSearchlight.materialize[S, Int](domain, searchlight, center, field).toOption.get
    val legacy =
      Searchlight.sphericalRoi(volume, Vector(1, 1, 0), radius = 1.0)

    assertEquals(materialized.region, legacy.region)
    assertEquals(
      materialized.selection.voxelCoords.zip(intValues(materialized.values)).toMap,
      legacy.selection.voxelCoords.zip(intValues(legacy.values)).toMap
    )

  test("value support restricts materialization without changing metric geometry"):
    val values = PrimitiveBuffers.fillConst[Int](volumeSpace.nVoxels, 1)
    values(1) = 0
    val volume = NeuroVol.fromLinear[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight
    val center = domain.finiteSpace.indexOption(4).get
    val geometry = searchlight.neighborhoods.row(center)
    val nonZero = domain.supportWhere(field)(_ != 0).toOption.get
    val materialized =
      VolumeSearchlight
        .materialize[S, Int](domain, searchlight, center, field, support = Some(nonZero))
        .toOption
        .get

    assertEquals(geometry.ordinalsInDomainOrder.toVector, Vector(1, 3, 4, 5, 7))
    assertEquals(intValues(materialized.region.linearIndices), Vector(3, 4, 5, 7))
    assertEquals(searchlight.neighborhoods.row(center), geometry)

  private def intValues(values: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))
