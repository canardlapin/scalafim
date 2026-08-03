package scalafim.surface

import scalafim.locus.{IndexedField, Region, Relation, SpaceKey}
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
      searchlight.searchlight.neighborhoods,
      Relation.identity(domain.finiteSpace)
    )

  test("geodesic metric balls are symmetric and monotone in radius"):
    val small =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val large =
      SurfaceSearchlight.metricBalls(domain, topology, math.sqrt(2.0)).toOption.get

    assertEquals(
      small.searchlight.neighborhoods,
      small.searchlight.neighborhoods.converse
    )
    assert(
      small.searchlight.neighborhoods
        .subsetOf(large.searchlight.neighborhoods)
    )

  test("metric-ball composition is contained by the summed radius"):
    val radiusOne =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val radiusTwo =
      SurfaceSearchlight.metricBalls(domain, topology, 2.0).toOption.get
    val composed =
      radiusOne.searchlight.neighborhoods
        .andThen(radiusOne.searchlight.neighborhoods)

    assert(
      composed
        .subsetOf(radiusTwo.searchlight.neighborhoods)
    )

  test("metric balls use a closed radius boundary"):
    val searchlight =
      SurfaceSearchlight.metricBalls(domain, topology, 1.0).toOption.get
    val center = domain.finiteSpace.pointOption(0).get

    assertEquals(
      searchlight.searchlight.regionAt(center).get.ordinalsInDomainOrder.toVector,
      Vector(0, 1, 2, 3)
    )

  test("partial center support leaves every non-center relation row empty"):
    val centers =
      Region.fromOrdinals(domain.finiteSpace, Vector(1, 3)).toOption.get
    val searchlight =
      SurfaceSearchlight
        .metricBalls(domain, topology, 1.0, centers)
        .toOption
        .get

    assert(
      searchlight.searchlight.neighborhoods
        .row(domain.finiteSpace.pointOption(0).get)
        .isEmpty
    )
    assert(searchlight.searchlight.regionAt(domain.finiteSpace.pointOption(0).get).isEmpty)
    assert(searchlight.searchlight.regionAt(domain.finiteSpace.pointOption(1).get).nonEmpty)

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
      IndexedField
        .fromValues(domain.finiteSpace, Vector(10, 20, 30, 40))
        .toOption
        .get
    val center = domain.finiteSpace.pointOption(1).get
    val section =
      SurfaceSearchlight
        .sectionAt(searchlight.searchlight, center, field)
        .get

    assertEquals(section.support.ordinalsInDomainOrder.toVector, Vector(0, 1))
    assertEquals(section.valuesInDomainOrder.toVector, Vector(10, 20))

  test("invalid radii are rejected before geometry work"):
    assert(SurfaceSearchlight.metricBalls(domain, topology, -0.1).isLeft)
    assert(SurfaceSearchlight.metricBalls(domain, topology, Double.NaN).isLeft)
