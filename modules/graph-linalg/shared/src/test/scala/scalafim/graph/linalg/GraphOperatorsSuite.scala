package scalafim.graph.linalg

import scalafim.graph.Graph
import scalafim.graph.VertexBasis
import scalafim.graph.connectedComponents
import scalafim.linalg.CsrMatrix
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.LinalgSolvers

class GraphOperatorsSuite extends munit.FunSuite:
  private val tolerance = 1e-10

  private val basis =
    VertexBasis.from(Vector("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")).toOption.get

  test("affinity wrappers validate their distinct numerical domains"):
    assert(SignedAffinity.from(-0.5).isRight)
    assert(SignedAffinity.from(Double.NaN).isLeft)
    assert(NonNegativeAffinity.from(0.0).isRight)
    assert(NonNegativeAffinity.from(-0.01).isLeft)
    assert(PositiveAffinity.from(0.0).isLeft)
    assert(PositiveAffinity.from(0.01).isRight)

  test("topology adjacency preserves explicit zero edges while weighted CSR drops numerical zeros"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(2.0)),
        ("b", "c", NonNegativeAffinity.unsafe(0.0))
      )
    ).toOption.get

    val topology = dense(graph.topologyAdjacency.toOption.get.matrix)
    val weighted = dense(graph.weightedAdjacency.toOption.get.matrix)

    assertMatrixEquals(
      topology,
      Vector(
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertMatrixEquals(
      weighted,
      Vector(
        Vector(0.0, 2.0, 0.0, 0.0),
        Vector(2.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertEquals(graph.degree.values.toVector, Vector(1.0, 2.0, 1.0, 0.0))
    assertEquals(graph.strength.toOption.get.values.toVector, Vector(2.0, 2.0, 0.0, 0.0))

  test("signed weighted adjacency is symmetric without claiming non-negative spectral evidence"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", SignedAffinity.unsafe(-0.4)),
        ("a", "d", SignedAffinity.unsafe(0.7))
      )
    ).toOption.get
    val adjacency = dense(graph.weightedAdjacency.toOption.get.matrix)

    assertEqualsDouble(adjacency(0)(1), -0.4, tolerance)
    assertEqualsDouble(adjacency(1)(0), -0.4, tolerance)
    assertEqualsDouble(adjacency(0)(3), 0.7, tolerance)
    assertEqualsDouble(adjacency(3)(0), 0.7, tolerance)

  test("directed degree and strength retain direction"):
    val graph = Graph.directed(
      basis,
      Vector(
        ("a", "b", SignedAffinity.unsafe(2.0)),
        ("a", "c", SignedAffinity.unsafe(-1.0)),
        ("d", "a", SignedAffinity.unsafe(4.0))
      )
    ).toOption.get

    assertEquals(graph.outDegree.values.toVector, Vector(2.0, 0.0, 0.0, 1.0))
    assertEquals(graph.inDegree.values.toVector, Vector(1.0, 1.0, 1.0, 0.0))
    assertEquals(graph.outStrength.toOption.get.values.toVector, Vector(1.0, 0.0, 0.0, 4.0))
    assertEquals(graph.inStrength.toOption.get.values.toVector, Vector(4.0, 2.0, -1.0, 0.0))

  test("incidence columns use canonical graph order and carry an endpoint basis"):
    val graph = Graph.undirected(
      basis,
      Vector(("d", "b", 3), ("c", "a", 2), ("b", "a", 1))
    ).toOption.get
    val incidence = graph.incidence.toOption.get

    assertEquals(
      incidence.edgeBasis.edges.map(edge => edge.from -> edge.to),
      Vector("a" -> "b", "a" -> "c", "b" -> "d")
    )
    assertMatrixEquals(
      dense(incidence.matrix),
      Vector(
        Vector(-1.0, -1.0, 0.0),
        Vector(1.0, 0.0, -1.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )

  test("combinatorial Laplacian equals D minus A and is positive semidefinite"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(2.0)),
        ("a", "c", NonNegativeAffinity.unsafe(1.0)),
        ("b", "d", NonNegativeAffinity.unsafe(3.0))
      )
    ).toOption.get
    val laplacian = dense(graph.combinatorialLaplacian.toOption.get.matrix)
    val expected = Vector(
      Vector(3.0, -2.0, -1.0, 0.0),
      Vector(-2.0, 5.0, 0.0, -3.0),
      Vector(-1.0, 0.0, 1.0, 0.0),
      Vector(0.0, -3.0, 0.0, 3.0)
    )

    assertMatrixEquals(laplacian, expected)
    laplacian.foreach(row => assertEqualsDouble(row.sum, 0.0, tolerance))
    Vector(
      Vector(1.0, -2.0, 0.5, 3.0),
      Vector(-4.0, 1.0, 2.0, -0.25),
      Vector(0.0, 0.0, 0.0, 0.0)
    ).foreach: vector =>
      assert(quadraticForm(laplacian, vector) >= -tolerance)

  test("oriented incidence satisfies L equals B W B transpose"):
    val weights = Vector(2.0, 1.0, 3.0)
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(weights(0))),
        ("a", "c", NonNegativeAffinity.unsafe(weights(1))),
        ("b", "d", NonNegativeAffinity.unsafe(weights(2)))
      )
    ).toOption.get
    val b = dense(graph.incidence.toOption.get.matrix)
    val reconstructed = multiplyWeightedIncidence(b, weights)
    val laplacian = dense(graph.combinatorialLaplacian.toOption.get.matrix)

    assertMatrixEquals(reconstructed, laplacian)

  test("weighted Laplacian nullity follows positive-weight support rather than explicit topology"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(1.0)),
        ("b", "c", NonNegativeAffinity.unsafe(0.0)),
        ("c", "d", NonNegativeAffinity.unsafe(2.0))
      )
    ).toOption.get
    val support = Graph.undirected(
      basis,
      Vector(("a", "b", ()), ("c", "d", ()))
    ).toOption.get
    val eigen = LinalgSolvers.symmetricEigen.decompose(toDoubleMatrix(dense(graph.combinatorialLaplacian.toOption.get.matrix))).toOption.get
    val nullity = eigen.values.toVector.count(value => Math.abs(value) < 1e-9)

    assertEquals(graph.connectedComponents.size, 1)
    assertEquals(support.connectedComponents.size, 2)
    assertEquals(nullity, 2)

  test("normalized Laplacians make zero-strength behavior explicit"):
    val graph = Graph.undirected(
      basis,
      Vector(("a", "b", NonNegativeAffinity.unsafe(2.0)))
    ).toOption.get
    val symmetricZero = dense(
      graph.normalizedLaplacian(NormalizedLaplacian.Symmetric, ZeroStrengthPolicy.KeepZeroRow).toOption.get.matrix
    )
    val symmetricIdentity = dense(
      graph.normalizedLaplacian(
        NormalizedLaplacian.Symmetric,
        ZeroStrengthPolicy.IdentityOnZeroStrength
      ).toOption.get.matrix
    )
    val randomWalk = dense(
      graph.normalizedLaplacian(NormalizedLaplacian.RandomWalk, ZeroStrengthPolicy.KeepZeroRow).toOption.get.matrix
    )

    assertMatrixEquals(
      symmetricZero,
      Vector(
        Vector(1.0, -1.0, 0.0, 0.0),
        Vector(-1.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertEqualsDouble(symmetricIdentity(2)(2), 1.0, tolerance)
    assertEqualsDouble(symmetricIdentity(3)(3), 1.0, tolerance)
    assertMatrixEquals(randomWalk, symmetricZero)
    graph.normalizedLaplacian(NormalizedLaplacian.Symmetric, ZeroStrengthPolicy.Error) match
      case Left(GraphLinalgError.ZeroStrengthVertex("c")) => ()
      case other => fail(s"expected zero-strength error for c, got $other")

  test("normalized random-walk Laplacian uses source strengths"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(1.0)),
        ("b", "c", NonNegativeAffinity.unsafe(3.0))
      )
    ).toOption.get
    val laplacian = dense(
      graph.normalizedLaplacian(NormalizedLaplacian.RandomWalk, ZeroStrengthPolicy.KeepZeroRow).toOption.get.matrix
    )

    assertEqualsDouble(laplacian(0)(1), -1.0, tolerance)
    assertEqualsDouble(laplacian(1)(0), -0.25, tolerance)
    assertEqualsDouble(laplacian(1)(2), -0.75, tolerance)
    assertEqualsDouble(laplacian(2)(1), -1.0, tolerance)

  test("invalid capability implementations are checked at the graph boundary"):
    given NonNegativeAdjacencyWeight[Double] with
      def weight(edge: Double): Double = edge

    val graph = Graph.undirected(basis, Vector(("a", "b", -1.0))).toOption.get
    graph.combinatorialLaplacian match
      case Left(GraphLinalgError.InvalidWeight(0, "a", "b", -1.0, WeightRequirement.NonNegative)) => ()
      case other => fail(s"expected invalid non-negative weight, got $other")

  test("reindexing gives permutation-similar adjacency and Laplacian operators"):
    val graph = Graph.undirected(
      basis,
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(2.0)),
        ("a", "d", NonNegativeAffinity.unsafe(4.0)),
        ("c", "d", NonNegativeAffinity.unsafe(1.0))
      )
    ).toOption.get
    val reordered = graph.reindex(Vector("d", "b", "a", "c")).toOption.get.graph
    val originalAdjacency = dense(graph.weightedAdjacency.toOption.get.matrix)
    val reorderedAdjacency = dense(reordered.weightedAdjacency.toOption.get.matrix)
    val originalLaplacian = dense(graph.combinatorialLaplacian.toOption.get.matrix)
    val reorderedLaplacian = dense(reordered.combinatorialLaplacian.toOption.get.matrix)

    assertKeyPermutation(originalAdjacency, graph.basis.keys, reorderedAdjacency, reordered.basis.keys)
    assertKeyPermutation(originalLaplacian, graph.basis.keys, reorderedLaplacian, reordered.basis.keys)

  private def dense(matrix: CsrMatrix): Vector[Vector[Double]] =
    val out = Array.fill(matrix.rows, matrix.cols)(0.0)
    val triplets = matrix.toTriplets
    val rows = triplets.rowIndices
    val cols = triplets.colIndices
    val values = triplets.values
    var index = 0
    while index < values.length do
      out(rows(index))(cols(index)) = values(index)
      index += 1
    out.iterator.map(_.toVector).toVector

  private def toDoubleMatrix(matrix: Vector[Vector[Double]]): DoubleMatrix =
    DoubleMatrix.fromRows(matrix)

  private def assertMatrixEquals(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]]
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual.indices.foreach: row =>
      assertEquals(actual(row).length, expected(row).length)
      actual(row).indices.foreach: col =>
        assertEqualsDouble(actual(row)(col), expected(row)(col), tolerance)

  private def quadraticForm(matrix: Vector[Vector[Double]], vector: Vector[Double]): Double =
    vector.indices.map: row =>
      vector.indices.map(col => vector(row) * matrix(row)(col) * vector(col)).sum
    .sum

  private def multiplyWeightedIncidence(
      incidence: Vector[Vector[Double]],
      weights: Vector[Double]
  ): Vector[Vector[Double]] =
    Vector.tabulate(incidence.length, incidence.length): (row, col) =>
      weights.indices.map(edge => incidence(row)(edge) * weights(edge) * incidence(col)(edge)).sum

  private def assertKeyPermutation[K](
      original: Vector[Vector[Double]],
      originalKeys: Vector[K],
      reordered: Vector[Vector[Double]],
      reorderedKeys: Vector[K]
  ): Unit =
    reorderedKeys.indices.foreach: row =>
      reorderedKeys.indices.foreach: col =>
        val sourceRow = originalKeys.indexOf(reorderedKeys(row))
        val sourceCol = originalKeys.indexOf(reorderedKeys(col))
        assertEqualsDouble(reordered(row)(col), original(sourceRow)(sourceCol), tolerance)
