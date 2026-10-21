package scalafim.surface

import scalafim.surface.fixtures.SurfaceTestFixtures

class SurfaceGeodesicsSuite extends munit.FunSuite:

  private val tetraTopology = SurfaceTestFixtures.tetraTopology

  test("endpoint-keyed custom costs survive face-order changes while positional reuse is unsafe"):
    val reordered = MeshTopology.from(TriangleMesh.fromRows(
      SurfaceTestFixtures.tetraVertices, SurfaceTestFixtures.tetraFaces.reverse))
    val costs = Map((0, 1) -> 2.0, (0, 2) -> 7.0, (0, 3) -> 20.0,
      (1, 2) -> 3.0, (1, 3) -> 9.0, (2, 3) -> 4.0)
    def weights(topology: MeshTopology): Vector[Double] =
      val keys = topology.edges.map(edge => (edge.a.index, edge.b.index))
      assertEquals(costs.keySet, keys.toSet)
      keys.map(costs)
    assertNotEquals(tetraTopology.edges, reordered.edges)
    val all = Vector.tabulate(4)(VertexId.unsafe)
    // Independent hand calculation: 0->2 uses 0-1-2, and 0->3 uses 0-1-2-3.
    val expected = Vector(0.0, 2.0, 5.0, 9.0, 2.0, 0.0, 3.0, 7.0,
      5.0, 3.0, 0.0, 4.0, 9.0, 7.0, 4.0, 0.0)
    for topology <- Vector(tetraTopology, reordered) do
      val matrix = SurfaceGeodesics.distanceMatrix(topology, all, all, edgeWeights = Some(weights(topology)))
      matrix.data.zip(expected).foreach((actual, reference) => assertEqualsDouble(actual, reference, 1e-12))
      val subset = SurfaceGeodesics.distanceMatrix(topology,
        Vector(VertexId(0), VertexId(2), VertexId(0)), Vector(VertexId(3), VertexId(1), VertexId(3)),
        edgeWeights = Some(weights(topology)), chunkSize = 1)
      subset.data.zip(Vector(9.0, 2.0, 9.0, 4.0, 3.0, 4.0, 9.0, 2.0, 9.0)).foreach:
        (actual, reference) => assertEqualsDouble(actual, reference, 1e-12)
    val wrong = SurfaceGeodesics.distanceMatrix(reordered, all, all, edgeWeights = Some(weights(tetraTopology)))
    assertEqualsDouble(wrong(0, 1), 4.0, 1e-12)

  test("geodesic distance matrix matches tetrahedron edge distances"):
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
        metric = DistanceMetric.Geodesic
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

  test("spherical policy retains unequal radii and repeated target order"):
    val topology = MeshTopology.from(TriangleMesh.fromArrays(
      Array(2.0, 0, 0, 0, 4.0, 0, -6.0, 0, 0), Array(0, 1, 2)
    ))
    val matrix = SurfaceGeodesics.distanceMatrix(
      topology, Vector(VertexId(0), VertexId(0)),
      Vector(VertexId(2), VertexId(1), VertexId(2), VertexId(0)), DistanceMetric.Spherical
    )
    val expected = Vector(4 * math.Pi, 2 * math.Pi, 4 * math.Pi, 0.0)
    assertEquals(matrix.rows, 2)
    matrix.data.grouped(4).foreach: row =>
      row.zip(expected).foreach((actual, value) => assertEqualsDouble(actual, value, 1e-12))

  test("spherical policy preserves zero, underflowed endpoint and overflowed radius refusals"):
    Vector(0.0, 1e-200, 1e200).foreach: magnitude =>
      val topology = MeshTopology.from(TriangleMesh.fromArrays(
        Array(magnitude, 0, 0, 0, 1.0, 0, 0, 0, 1.0), Array(0, 1, 2)
      ))
      intercept[IllegalArgumentException]:
        SurfaceGeodesics.distanceMatrix(
          topology, Vector(VertexId(0)), Vector(VertexId(1)), DistanceMetric.Spherical
        )

  test("normalized provider angle corrects the admitted subnormal cosine collapse"):
    val topology = MeshTopology.from(TriangleMesh.fromArrays(
      Array(2e-162, 2e-162, 0, 2e-162, 0, 0, 0, 1.0, 0), Array(0, 1, 2)
    ))
    val distance = SurfaceGeodesics.distances(
      topology, VertexId(0), Vector(VertexId(1)), DistanceMetric.Spherical
    ).head
    // The unit-radius third vertex makes the policy mean 1/3. Both queried
    // tiny endpoints passed the legacy admission, whose cosine rounded to 1.
    assertEqualsDouble(distance, math.Pi / 12, 1e-12)

  test("unreachable vertices remain Infinity for geodesic metric"):
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

  test("custom edge weights drive geodesic distances and cache keys"):
    val cache = GeodesicCache()
    val weightsA = Vector.fill(tetraTopology.edgeCount)(1.0)
    val weightsB = Vector.tabulate(tetraTopology.edgeCount)(i => if i == 0 then 5.0 else 1.0)

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

  test("geodesic input validation catches bad radius, vertices, and weights"):
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

    interceptMessage[IllegalArgumentException]("requirement failed: edgeWeights length must equal topology edge count"):
      SurfaceGeodesics.distanceMatrix(tetraTopology, Vector(VertexId(0)), Vector(VertexId(1)), edgeWeights = Some(Vector(1.0)))

    interceptMessage[IllegalArgumentException]("requirement failed: edgeWeights must be finite and non-negative"):
      SurfaceGeodesics.distanceMatrix(tetraTopology, Vector(VertexId(0)), Vector(VertexId(1)), edgeWeights = Some(Vector.fill(tetraTopology.edgeCount)(Double.NaN)))

    interceptMessage[IllegalArgumentException]("requirement failed: geodesic edgeWeights must contain at least one positive value"):
      SurfaceGeodesics.distanceMatrix(tetraTopology, Vector(VertexId(0)), Vector(VertexId(1)), edgeWeights = Some(Vector.fill(tetraTopology.edgeCount)(0.0)))

  test("distance matrices retain repeated sources and target columns in request order"):
    val topology = SurfaceTestFixtures.sphericalTopology
    val sources = Vector(VertexId(1), VertexId(0), VertexId(1))
    val targets = Vector(VertexId(2), VertexId(0), VertexId(2), VertexId(3))
    DistanceMetric.values.foreach: metric =>
      val matrix = SurfaceGeodesics.distanceMatrix(topology, sources, targets, metric, chunkSize = 1)
      assertEquals(matrix.rowVertices, sources)
      assertEquals(matrix.colVertices, targets)
      sources.indices.foreach: row =>
        targets.indices.foreach: col =>
          val single = SurfaceGeodesics.distances(topology, sources(row), Vector(targets(col)), metric).head
          assertEqualsDouble(matrix(row, col), single, 1e-12)

  test("cache distinguishes same-sized meshes with different connectivity"):
    val first = SurfaceTestFixtures.sheetTopology
    val second = MeshTopology.from(TriangleMesh.fromRows(
      first.mesh.coordinates.toVector.grouped(3).toVector,
      Vector((0, 1, 3), (0, 3, 2))
    ))
    assertEquals(first.edgeCount, second.edgeCount)
    val cache = GeodesicCache()
    def distance(topology: MeshTopology): Double =
      SurfaceGeodesics.distanceMatrix(topology, Vector(VertexId(0)), Vector(VertexId(3)),
        edgeWeights = Some(Vector.fill(topology.edgeCount)(1.0)), cachePolicy = CachePolicy.Use(cache))(0, 0)
    assertEqualsDouble(distance(first), 2.0, 1e-12)
    assertEqualsDouble(distance(second), 1.0, 1e-12)
    assertEquals(cache.size, 2)

  test("cache distinguishes coordinate changes for direct and spherical metrics"):
    val first = SurfaceTestFixtures.sphericalTopology
    val coordinates = first.mesh.coordinates.toArray
    coordinates(3) = math.sqrt(0.5)
    coordinates(4) = math.sqrt(0.5)
    val second = MeshTopology.from(first.mesh.withCoordinates(coordinates))
    Vector(DistanceMetric.Euclidean, DistanceMetric.Spherical).foreach: metric =>
      val cache = GeodesicCache()
      def distance(topology: MeshTopology): Double =
        SurfaceGeodesics.distanceMatrix(topology, Vector(VertexId(0)), Vector(VertexId(1)), metric,
          edgeWeights = Some(Vector.fill(topology.edgeCount)(1.0)), cachePolicy = CachePolicy.Use(cache))(0, 0)
      val expectedFirst = if metric == DistanceMetric.Spherical then math.Pi / 2 else math.sqrt(2)
      val expectedSecond = if metric == DistanceMetric.Spherical then math.Pi / 4 else math.sqrt(2 - math.sqrt(2))
      assertEqualsDouble(distance(first), expectedFirst, 1e-12)
      assertEqualsDouble(distance(second), expectedSecond, 1e-12)
      assertEquals(cache.size, 2)
      // Equal immutable mesh values still reuse the same entry, even if restored
      // into a fresh runtime topology/frame owner.
      val equal = MeshTopology.from(TriangleMesh.fromArrays(second.mesh.coordinates.toArray, second.mesh.faceIndices.toArray))
      assertEqualsDouble(distance(equal), expectedSecond, 1e-12)
      assertEquals(cache.size, 2)

  test("cache verifies full weights even when their 32-bit hashes collide"):
    val cache = GeodesicCache()
    val a = 1.0
    val b = java.lang.Double.longBitsToDouble(java.lang.Double.doubleToLongBits(a) ^ 0x0000000100000001L)
    assert(b > a && b < 1.01)
    // Reproduce the legacy cache's explicit bit fold. Boxed Double.hashCode
    // differs across JVM and JS and was never the cache's hash contract.
    def legacyHash(value: Double): Int =
      val bits = java.lang.Double.doubleToLongBits(value)
      (bits ^ (bits >>> 32)).toInt
    assertEquals(legacyHash(a), legacyHash(b))
    val edge = tetraTopology.edges.head
    def distance(weight: Double): Double =
      val weights = Vector.tabulate(tetraTopology.edgeCount)(i => if i == 0 then weight else 2.0)
      SurfaceGeodesics.distanceMatrix(tetraTopology, Vector(edge.a), Vector(edge.b), edgeWeights = Some(weights),
        cachePolicy = CachePolicy.Use(cache))(0, 0)
    assertEqualsDouble(distance(a), a, 1e-12)
    assertEqualsDouble(distance(b), b, 1e-12)
    assertEquals(cache.size, 2)

  test("nonnegative custom costs match Floyd-Warshall including zero paths and disconnected repeated targets"):
    Vector(tetraTopology, SurfaceTestFixtures.sheetTopology, SurfaceTestFixtures.disconnectedTopology).foreach: topology =>
      (0 until 12).foreach: seed =>
        val weights = Vector.tabulate(topology.edgeCount): edge =>
          if (edge + seed) % 4 == 0 then 0.0 else 0.25 * (1 + (7 * edge + 3 * seed) % 9)
        val count = topology.mesh.vertexCount
        // Independent all-pairs dynamic program; no priority queue or visited set.
        val expected = Array.fill(count, count)(Double.PositiveInfinity)
        (0 until count).foreach(i => expected(i)(i) = 0.0)
        topology.edges.zip(weights).foreach: (edge, weight) =>
          expected(edge.a.index)(edge.b.index) = weight
          expected(edge.b.index)(edge.a.index) = weight
        (0 until count).foreach: k =>
          (0 until count).foreach: i =>
            (0 until count).foreach: j =>
              expected(i)(j) = math.min(expected(i)(j), expected(i)(k) + expected(k)(j))
        val sources = (0 until count).reverse.map(VertexId.apply).toVector :+ VertexId(0)
        val targets = Vector(VertexId(0), VertexId(2), VertexId(0), VertexId(count - 1), VertexId(count - 1))
        val actual = SurfaceGeodesics.distanceMatrix(topology, sources, targets, edgeWeights = Some(weights), chunkSize = 2)
        sources.indices.foreach: row =>
          targets.indices.foreach: col =>
            val wanted = expected(sources(row).index)(targets(col).index)
            if wanted.isPosInfinity then assert(actual(row, col).isPosInfinity)
            else assertEqualsDouble(actual(row, col), wanted, 1e-12)

  test("graph-cost overflow preserves the matrix infinity convention and duplicate columns"):
    val topology = SurfaceTestFixtures.sheetTopology
    val matrix = SurfaceGeodesics.distanceMatrix(
      topology, Vector(VertexId(0)), Vector(VertexId(3), VertexId(1), VertexId(3)),
      edgeWeights = Some(Vector.fill(topology.edgeCount)(Double.MaxValue))
    )
    assert(matrix(0, 0).isPosInfinity)
    assertEqualsDouble(matrix(0, 1), Double.MaxValue, 0.0)
    assert(matrix(0, 2).isPosInfinity)
