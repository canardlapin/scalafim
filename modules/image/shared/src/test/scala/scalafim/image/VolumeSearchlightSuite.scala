package scalafim.image

import VolumeDomain.*
import locus4s.DomainRegistry
import locus4s.Relation
import spire.std.int.given

class VolumeSearchlightSuite extends munit.FunSuite:
  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(3, 3, 1)))
  private val packedDomain =
    right(
      VolumeDomain.register(
        volumeSpace,
        "searchlight voxels",
        DomainRegistry.empty
      )
    )
  private type S = packedDomain.S
  private val domain: VolumeDomain[S] = packedDomain.value

  test("radius zero is the identity relation"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get

    assertEquals(
      searchlight.relation,
      Relation.identity(domain.space)
    )

  test("metric balls are symmetric, monotone, and obey triangle composition"):
    val radiusOne = SearchlightRadius.make(1.0).toOption.get
    val radiusTwo = SearchlightRadius.make(2.0).toOption.get
    val one = ExactVolumeSearchlight.metricBalls(domain, radiusOne).toOption.get.relation
    val two = ExactVolumeSearchlight.metricBalls(domain, radiusTwo).toOption.get.relation

    assertEquals(one, one.converse)
    assert(one.subsetOf(two))
    assert(one.andThen(one).subsetOf(two))

  test("closed metric balls include points exactly on the radius boundary"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val relation =
      ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get.relation
    val center = domain.space.indexOption(4).get

    assertEquals(
      relation.row(center).ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )

  test("relation rows materialize an exact selected window"):
    val values = Array.ofDim[Int](volumeSpace.nVoxels)
    var i = 0
    while i < values.length do
      values(i) = i + 1
      i += 1
    val volume = NeuroVol.copyFromCanonicalArray[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.fieldOf(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.space.indexOption(4).get
    val materialized =
      ExactVolumeSearchlight
        .materializeCategorical(domain, searchlight, center, field)
        .toOption
        .get

    assertEquals(
      materialized.values.selection.region.ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )
    assertEquals(
      materialized.values.data.iterator.toVector,
      Vector(2, 4, 5, 6, 8)
    )
    assertEquals(materialized.centerPosition, 2)

  test("value support restricts materialization without changing metric geometry"):
    val values = PrimitiveBuffers.fillConst[Int](volumeSpace.nVoxels, 1)
    values(1) = 0
    val volume = NeuroVol.copyFromCanonicalArray[Int](values, volumeSpace.toNeuroSpace)
    val field = domain.fieldOf(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.space.indexOption(4).get
    val geometry = searchlight.relation.row(center)
    val nonZero = domain.supportWhere(field)(_ != 0).toOption.get
    val materialized =
      ExactVolumeSearchlight
        .materializeCategorical(
          domain,
          searchlight,
          center,
          field,
          support = Some(nonZero)
        )
        .toOption
        .get

    assertEquals(geometry.ordinalsInDomainOrder.toVector, Vector(1, 3, 4, 5, 7))
    assertEquals(
      materialized.values.selection.region.ordinalsInDomainOrder.toVector,
      Vector(3, 4, 5, 7)
    )
    assertEquals(searchlight.relation.row(center), geometry)

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
