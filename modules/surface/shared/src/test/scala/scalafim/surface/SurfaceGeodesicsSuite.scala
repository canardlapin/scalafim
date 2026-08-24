package scalafim.surface

import scalafim.surface.fixtures.SurfaceTestFixtures
import scala.compiletime.testing.typeCheckErrors

class SurfaceGeodesicsSuite extends munit.FunSuite:

  private val tetraTopology = SurfaceTestFixtures.tetraTopology

  private def legacyWeights(values: Seq[Double]): SurfaceEdgeWeights =
    SurfaceEdgeWeights
      .fromLegacyLexicographic(tetraTopology, values)
      .fold(error => fail(error.message), identity)

  test("edge-graph distance matrix matches tetrahedron edge distances"):
    val matrix = SurfaceGeodesics.allPairsDistanceMatrix(tetraTopology)

    assertEquals(matrix.rows, 4)
    assertEquals(matrix.cols, 4)
    assertEqualsDouble(matrix(0, 0), 0.0, 1e-12)
    assertEqualsDouble(matrix(0, 1), 1.0, 1e-12)
    assertEqualsDouble(matrix(1, 2), math.sqrt(2.0), 1e-12)
    assertEqualsDouble(matrix(1, 2), matrix(2, 1), 1e-12)

  test("distance matrix supports source and target subsets"):
    val matrix =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(0), VertexId(1)),
        targets = Vector(VertexId(2), VertexId(3)),
        chunkSize = 1
      )

    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
    assertEqualsDouble(matrix(0, 0), 1.0, 1e-12)
    assertEqualsDouble(matrix(1, 0), math.sqrt(2.0), 1e-12)

  test("neighborsWithin includes self and vertices inside radius"):
    val hits =
      SurfaceGeodesics.neighborsWithin(
        tetraTopology,
        radius = 1.01,
        sources = Vector(VertexId(0)),
        metric = DistanceMetric.EdgeGraphShortestPath
      )

    assertEquals(hits.map(_.target).toSet, Set(VertexId(0), VertexId(1), VertexId(2), VertexId(3)))
    assert(hits.exists(hit => hit.source == VertexId(0) && hit.target == VertexId(0) && hit.distance == 0.0))

  test("neighborsWithin uses a closed radius boundary"):
    val hits =
      SurfaceGeodesics.neighborsWithin(
        tetraTopology,
        radius = 1.0,
        sources = Vector(VertexId(0))
      )

    assertEquals(hits.map(_.target).toSet, Set(VertexId(0), VertexId(1), VertexId(2), VertexId(3)))

  test("euclidean metric ignores mesh paths"):
    val matrix =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(1)),
        targets = Vector(VertexId(2)),
        metric = DistanceMetric.Euclidean
      )

    assertEqualsDouble(matrix(0, 0), math.sqrt(2.0), 1e-12)

  test("spherical metric clamps cosine and returns finite distances"):
    val topology = SurfaceTestFixtures.sphericalTopology

    val matrix =
      SurfaceGeodesics.distanceMatrix(
        topology,
        sources = Vector(VertexId(0)),
        targets = Vector(VertexId(0), VertexId(1), VertexId(3)),
        metric = DistanceMetric.Spherical
      )

    assertEqualsDouble(matrix(0, 0), 0.0, 1e-12)
    assertEquals(matrix.colVertices(2), VertexId(3))
    assert(matrix(0, 1).isFinite)
    assert(matrix(0, 2).isFinite)

  test("unreachable vertices remain Infinity for edge-graph metric"):
    val topology = SurfaceTestFixtures.disconnectedTopology

    val matrix =
      SurfaceGeodesics.distanceMatrix(
        topology,
        sources = Vector(VertexId(0)),
        targets = Vector(VertexId(3))
      )

    assert(matrix(0, 0).isPosInfinity)

  test("cache policy stores and reuses distance matrices"):
    val cache = GeodesicCache()
    val policy = CachePolicy.Use(cache)

    val first =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(0), VertexId(1)),
        targets = Vector(VertexId(2), VertexId(3)),
        cachePolicy = policy
      )
    val second =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(0), VertexId(1)),
        targets = Vector(VertexId(2), VertexId(3)),
        cachePolicy = policy
      )

    assertEquals(cache.size, 1)
    assertEquals(second, first)

  test("custom edge weights drive edge-graph distances and cache keys"):
    val cache = GeodesicCache()
    val weightsA = legacyWeights(Vector.fill(tetraTopology.edgeCount)(1.0))
    val weightsB = legacyWeights(Vector.tabulate(tetraTopology.edgeCount)(i => if i == 0 then 5.0 else 1.0))

    val directA =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(0)),
        targets = Vector(VertexId(1)),
        edgeWeights = Some(weightsA),
        cachePolicy = CachePolicy.Use(cache)
      )
    val directB =
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        sources = Vector(VertexId(0)),
        targets = Vector(VertexId(1)),
        edgeWeights = Some(weightsB),
        cachePolicy = CachePolicy.Use(cache)
      )

    assertEqualsDouble(directA(0, 0), 1.0, 1e-12)
    assertEqualsDouble(directB(0, 0), 2.0, 1e-12)
    assertEquals(cache.size, 2)

  test("edge-graph input validation catches bad radius, vertices, and weights"):
    assertEquals(
      SurfaceGeodesics
        .neighborsWithin(tetraTopology, 0.0, Vector(VertexId(0)))
        .map(_.target),
      Vector(VertexId(0))
    )

    interceptMessage[IllegalArgumentException]("requirement failed: radius must be non-negative and finite"):
      SurfaceGeodesics.neighborsWithin(tetraTopology, -0.1, Vector(VertexId(0)))

    interceptMessage[IllegalArgumentException]("requirement failed: target vertex id out of range"):
      SurfaceGeodesics.distanceMatrix(tetraTopology, Vector(VertexId(0)), Vector(VertexId(99)))

    assertEquals(
      SurfaceEdgeWeights.fromTopologyOrder(tetraTopology, Vector(1.0)).left.map(_.message),
      Left("surface edge weights require 6 values, found 1")
    )
    assertEquals(
      SurfaceEdgeWeights
        .fromTopologyOrder(tetraTopology, Vector.fill(tetraTopology.edgeCount)(Double.NaN))
        .left
        .map(_.message),
      Left("surface edge weight 0 must be finite, found NaN")
    )
    val zeroWeights = legacyWeights(Vector.fill(tetraTopology.edgeCount)(0.0))
    interceptMessage[IllegalArgumentException]("requirement failed: edge-graph weights must contain at least one positive value"):
      SurfaceGeodesics.distanceMatrix(
        tetraTopology,
        Vector(VertexId(0)),
        Vector(VertexId(1)),
        edgeWeights = Some(zeroWeights)
      )

  test("legacy lexicographic weights are checked and permuted into mesh4s edge order"):
    val legacyValues = Vector.tabulate(tetraTopology.edgeCount)(_.toDouble + 1.0)
    val weights = legacyWeights(legacyValues)
    val expected = tetraTopology.edges.zip(legacyValues).toMap
    val topologyPairs =
      weights.topology.edges.indices.map: edge =>
        val endpoints = weights.topology.endpointsOf(edge)
        val compatibility =
          Edge.between(
            VertexId(endpoints.first.ordinal),
            VertexId(endpoints.second.ordinal)
          )
        assertEqualsDouble(weights(edge), expected(compatibility), 1e-12)
        compatibility
      .toVector

    assertNotEquals(topologyPairs, tetraTopology.edges)
    assert(weights.belongsTo(tetraTopology))

  test("edge fields reject a foreign edge index at compile time"):
    val errors = typeCheckErrors("""
      import scalafim.surface.SurfaceEdgeWeights
      import locus4s.Index

      def exact(weights: SurfaceEdgeWeights, edge: Index[weights.Edge]) =
        weights(edge)

      def foreign(left: SurfaceEdgeWeights, right: SurfaceEdgeWeights) =
        val edge = left.edges.indexAtValidatedOrdinal(0)
        right(edge)
    """)

    assert(errors.nonEmpty)

  test("edge weights reject a structurally equal but foreign topology owner"):
    val geometry = SurfaceTestFixtures.tetraGeometry
    val duplicate =
      MeshTopology.from:
        TriangleMesh.fromRows(
          geometry.mesh.vertices.map(point => Vector(point.x, point.y, point.z)),
          geometry.mesh.faces.map(face => (face.a.index, face.b.index, face.c.index))
        )
    val foreign =
      SurfaceEdgeWeights
        .euclidean(duplicate)
        .fold(error => fail(error.message), identity)

    assertEquals(
      duplicate.mesh.connectivityFingerprint,
      tetraTopology.mesh.connectivityFingerprint
    )
    assert(!foreign.belongsTo(tetraTopology))
    val error =
      intercept[IllegalArgumentException]:
        SurfaceGeodesics.distanceMatrix(
          tetraTopology,
          Vector(VertexId(0)),
          Vector(VertexId(1)),
          edgeWeights = Some(foreign)
        )
    assert(error.getMessage.contains("belong to another topology owner"))

  test("distance caches distinguish exact topology owners"):
    val geometry = SurfaceTestFixtures.tetraGeometry
    val duplicate =
      MeshTopology.from:
        TriangleMesh.fromRows(
          geometry.mesh.vertices.map(point => Vector(point.x, point.y, point.z)),
          geometry.mesh.faces.map(face => (face.a.index, face.b.index, face.c.index))
        )
    val cache = GeodesicCache()
    val policy = CachePolicy.Use(cache)

    SurfaceGeodesics.distanceMatrix(
      tetraTopology,
      Vector(VertexId(0)),
      Vector(VertexId(1)),
      cachePolicy = policy
    )
    SurfaceGeodesics.distanceMatrix(
      duplicate,
      Vector(VertexId(0)),
      Vector(VertexId(1)),
      cachePolicy = policy
    )

    assertEquals(cache.size, 2)
