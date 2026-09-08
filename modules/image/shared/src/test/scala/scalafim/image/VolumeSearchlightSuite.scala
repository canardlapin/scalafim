package scalafim.image

import scalafim.locus.{Region, Relation, Selection, SpaceKey}
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

    assertEquals(searchlight.membership, Relation.identity(domain.finiteSpace))

  test("metric balls are symmetric, monotone, and obey triangle composition"):
    val radiusOne = SearchlightRadius.make(1.0).toOption.get
    val radiusTwo = SearchlightRadius.make(2.0).toOption.get
    val one = VolumeSearchlight.metricBalls(domain, radiusOne).toOption.get.membership
    val two = VolumeSearchlight.metricBalls(domain, radiusTwo).toOption.get.membership

    assertEquals(one, one.converse)
    assert(one.subsetOf(two))
    assert(one.andThen(one).subsetOf(two))

  test("closed metric balls include points exactly on the radius boundary"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val relation =
      VolumeSearchlight.metricBalls(domain, radius).toOption.get.membership
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
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.finiteSpace.indexOption(4).get
    val materialized =
      VolumeSearchlight.materialize(domain, searchlight, center, field).toOption.get
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
    val searchlight = VolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.finiteSpace.indexOption(4).get
    val geometry = searchlight.neighborhood(center)
    val nonZero = domain.supportWhere(field)(_ != 0).toOption.get
    val materialized =
      VolumeSearchlight
        .materialize(domain, searchlight, center, field, support = Some(nonZero))
        .toOption
        .get

    assertEquals(geometry.ordinalsInDomainOrder.toVector, Vector(1, 3, 4, 5, 7))
    assertEquals(intValues(materialized.region.linearIndices), Vector(3, 4, 5, 7))
    assertEquals(searchlight.neighborhood(center), geometry)

  test("sparse, empty, and singleton center selections stay compact and preserve order"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val sparse = Selection
      .fromOrdinals(domain.finiteSpace, Vector(7, 1))
      .toOption
      .get
    val sparseSystem = VolumeSearchlight.metricBalls(domain, radius, sparse).toOption.get

    assertEquals(sparseSystem.centers.size, 2)
    assertEquals(sparseSystem.membership.from.size, 2)
    assertEquals(sparseSystem.center(sparse.positions.indexOption(0).get).value, 7)
    assertEquals(sparseSystem.center(sparse.positions.indexOption(1).get).value, 1)
    assertEquals(
      sparseSystem.neighborhood(sparse.positions.indexOption(0).get).ordinalsInDomainOrder.toVector,
      Vector(7)
    )

    val singleton = Selection
      .fromOrdinals(domain.finiteSpace, Vector(4))
      .toOption
      .get
    val singletonSystem = VolumeSearchlight.metricBalls(domain, radius, singleton).toOption.get
    assertEquals(singletonSystem.membership.pairCount, 1)

    val empty = Selection.empty(domain.finiteSpace).toOption.get
    val emptySystem = VolumeSearchlight.metricBalls(domain, radius, empty).toOption.get
    assertEquals(emptySystem.centers.size, 0)
    assertEquals(emptySystem.membership.pairCount, 0)

  test("equal-size foreign center owners are rejected"):
    val foreignPacked =
      VolumeDomain.semantic(SpaceKey.unsafe("searchlight:foreign"), volumeSpace)
    val foreign = foreignPacked.value
    val selection = Selection
      .fromOrdinals(foreign.finiteSpace, Vector(0))
      .toOption
      .get
      .asInstanceOf[Selection[S]]
    val radius = SearchlightRadius.make(0.0).toOption.get

    assert(VolumeSearchlight.metricBalls(domain, radius, selection).isLeft)

  private def intValues(values: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))
