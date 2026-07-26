package scalafim.image

import narr.NArray
import scalafim.locus.{Relation, SpaceKey, SymmetricRelation}
import spire.std.int.given

class VolumeSearchlightSuite extends munit.FunSuite:
  private sealed trait S

  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(3, 3, 1)))
  private val domain =
    VolumeDomain.semantic[S](SpaceKey.unsafe("searchlight:volume"), volumeSpace)

  test("radius zero is the identity relation"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get

    assertEquals(searchlight.searchlight.neighborhoods, Relation.identity(domain.finiteSpace))

  test("metric balls are symmetric, monotone, and obey triangle composition"):
    val radiusOne = SearchlightRadius.make(1.0).toOption.get
    val radiusTwo = SearchlightRadius.make(2.0).toOption.get
    val one = VolumeSearchlight.metricBalls(domain, radiusOne).toOption.get.searchlight.neighborhoods
    val two = VolumeSearchlight.metricBalls(domain, radiusTwo).toOption.get.searchlight.neighborhoods

    assert(SymmetricRelation.validate(one).isRight)
    assert(one.subsetOf(two).toOption.get)
    assert(one.andThen(one).toOption.get.subsetOf(two).toOption.get)

  test("closed metric balls include points exactly on the radius boundary"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val relation =
      VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight.neighborhoods
    val center = domain.finiteSpace.point(4).get

    assertEquals(
      relation.row(center).ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )

  test("relation rows materialize the same full window as the legacy spherical constructor"):
    val values = NArray.ofSize[Int](volumeSpace.nVoxels)
    var i = 0
    while i < values.length do
      values(i) = i + 1
      i += 1
    val volume = NeuroVol.fromLinear[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight
    val center = domain.finiteSpace.point(4).get
    val materialized =
      VolumeSearchlight.materialize[S, Int](domain, searchlight, center, field).toOption.get
    val legacy =
      Searchlight.sphericalRoi(volume, Vector(1, 1, 0), radius = 1.0)

    assertEquals(materialized.region, legacy.region)
    assertEquals(
      materialized.selection.voxelCoords.zip(intValues(materialized.toNArray)).toMap,
      legacy.selection.voxelCoords.zip(intValues(legacy.toNArray)).toMap
    )

  test("value support restricts materialization without changing metric geometry"):
    val values = NArrayUtil.fillConst[Int](volumeSpace.nVoxels, 1)
    values(1) = 0
    val volume = NeuroVol.fromLinear[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.indexedField(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get.searchlight
    val center = domain.finiteSpace.point(4).get
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

  private def intValues(values: NArray[Int]): Vector[Int] =
    Vector.tabulate(values.length)(values.apply)
