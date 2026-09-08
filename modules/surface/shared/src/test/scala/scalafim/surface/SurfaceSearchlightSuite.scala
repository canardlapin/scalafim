package scalafim.surface

import locus4s.data.VectorField
import scalafim.locus.{Relation, Selection, SpaceKey}
import scalafim.surface.fixtures.SurfaceTestFixtures

class SurfaceSearchlightSuite extends munit.FunSuite:

  private val packedDomain =
    SurfaceLocusDomain
      .semantic(
        SpaceKey.unsafe("subject-01:left-cortex"),
        SurfaceTestFixtures.tetraGeometry
      )
      .toOption
      .get
  private type Vertex = packedDomain.S
  private val domain: SurfaceLocusDomain[Vertex] = packedDomain.value
  private val topology = SurfaceTestFixtures.tetraTopology

  test("zero-radius metric balls are the identity relation"):
    val searchlight =
      SurfaceSearchlight.metricBalls(domain, topology, 0.0).toOption.get

    assertEquals(
      searchlight.membership,
      Relation.identity(domain.finiteSpace)
    )

  test("geodesic metric balls are symmetric and monotone in radius"):
    val small =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val large =
      SurfaceSearchlight.metricBalls(domain, topology, math.sqrt(2.0)).toOption.get

    assertEquals(
      small.membership,
      small.membership.converse
    )
    assert(
      small.membership
        .subsetOf(large.membership)
    )

  test("metric-ball composition is contained by the summed radius"):
    val radiusOne =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val radiusTwo =
      SurfaceSearchlight.metricBalls(domain, topology, 2.0).toOption.get
    val composed =
      radiusOne.membership
        .andThen(radiusOne.membership)

    assert(
      composed
        .subsetOf(radiusTwo.membership)
    )

  test("metric balls use a closed radius boundary"):
    val searchlight =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val center = domain.finiteSpace.indexOption(0).get

    assertEquals(
      searchlight.neighborhood(center).ordinalsInDomainOrder.toVector,
      Vector(0, 1, 2, 3)
    )

  test("sparse center support is compact and preserves explicit center order"):
    val centers =
      Selection.fromOrdinals(domain.finiteSpace, Vector(3, 1)).toOption.get
    val searchlight =
      SurfaceSearchlight
        .metricBalls(domain, topology, 1.0, centers)
        .toOption
        .get

    assertEquals(searchlight.centers.size, 2)
    assertEquals(searchlight.membership.from.size, 2)
    assertEquals(searchlight.center(centers.positions.indexOption(0).get).value, 3)
    assertEquals(searchlight.center(centers.positions.indexOption(1).get).value, 1)
    assert(searchlight.neighborhood(centers.positions.indexOption(0).get).contains(domain.finiteSpace.indexOption(3).get))
    assert(searchlight.neighborhood(centers.positions.indexOption(1).get).contains(domain.finiteSpace.indexOption(1).get))

  test("equal vertex counts do not excuse a topology mismatch"):
    assert(
      SurfaceSearchlight
        .metricBalls(
          domain,
          SurfaceTestFixtures.sheetTopology,
          1.0
        )
        .isLeft
    )

  test("local extraction is ordinary indexed-field restriction"):
    val searchlight =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val field =
      VectorField
        .fromValues(domain.finiteSpace, Vector(10, 20, 30, 40))
        .toOption
        .get
    val center = domain.finiteSpace.indexOption(1).get
    val section =
      SurfaceSearchlight
        .sectionAt(searchlight, center, field)

    assertEquals(section.support.ordinalsInDomainOrder.toVector, Vector(0, 1))
    assertEquals(section.valuesInDomainOrder.toVector, Vector(10, 20))

  test("invalid radii are rejected before geometry work"):
    assert(SurfaceSearchlight.metricBalls(domain, topology, -0.1).isLeft)
    assert(SurfaceSearchlight.metricBalls(domain, topology, Double.NaN).isLeft)

  test("empty and singleton center selections retain centered evidence"):
    val empty = Selection.empty(domain.finiteSpace).toOption.get
    val emptySystem =
      SurfaceSearchlight.metricBalls(domain, topology, 0.0, empty).toOption.get
    assertEquals(emptySystem.centers.size, 0)
    assertEquals(emptySystem.membership.pairCount, 0)

    val singleton = Selection
      .fromOrdinals(domain.finiteSpace, Vector(2))
      .toOption
      .get
    val singletonSystem =
      SurfaceSearchlight.metricBalls(domain, topology, 0.0, singleton).toOption.get
    val center = singleton.positions.indexOption(0).get
    assertEquals(singletonSystem.center(center).value, 2)
    assertEquals(
      singletonSystem.neighborhood(center).ordinalsInDomainOrder.toVector,
      Vector(2)
    )

  test("equal-size foreign center owners are rejected"):
    val foreignPacked =
      SurfaceLocusDomain
        .semantic(
          SpaceKey.unsafe("subject-02:left-cortex"),
          SurfaceTestFixtures.tetraGeometry
        )
        .toOption
        .get
    val foreign = foreignPacked.value
    val centers = Selection
      .fromOrdinals(foreign.finiteSpace, Vector(0))
      .toOption
      .get
      .asInstanceOf[Selection[Vertex]]

    assert(SurfaceSearchlight.metricBalls(domain, topology, 0.0, centers).isLeft)
