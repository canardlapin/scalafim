package scalafim.graph.linalg

import scalafim.graph.Graph
import scalafim.graph.VertexBasis

class GraphSpectralSuite extends munit.FunSuite:
  private val basis = VertexBasis.from(Vector("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")).toOption.get
  private val tolerance = 1e-8

  test("path graph spectrum matches analytic combinatorial eigenvalues and carries its basis"):
    val graph = weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum = graph.vertexSpectrum(4).toOption.get
    val expected = Vector(
      0.0,
      2.0 - Math.sqrt(2.0),
      2.0,
      2.0 + Math.sqrt(2.0)
    )

    assertEquals(spectrum.basis, graph.basis)
    assertVectorClose(spectrum.result.eigenvalues.toSeq.toVector, expected, tolerance)
    assert(spectrum.result.diagnostics.residuals.toSeq.forall(_ < tolerance))
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 1)
    assertEquals(spectrum.support.components.map(_.keys), Vector(Vector("a", "b", "c", "d")))

  test("zero-weight support, rather than explicit topology, determines combinatorial nullity"):
    val graph = weightedGraph(
      Vector(
        ("a", "b", 1.0),
        ("b", "c", 0.0),
        ("c", "d", 2.0)
      )
    )
    val spectrum = graph.vertexSpectrum(4).toOption.get
    val nullity = spectrum.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance)

    assertEquals(spectrum.support.zeroWeightEdgeCount, 1)
    assertEquals(spectrum.support.components.map(_.keys), Vector(Vector("a", "b"), Vector("c", "d")))
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 2)
    assertEquals(nullity, 2)

  test("normalized isolate policy changes expected nullity explicitly"):
    val graph = weightedGraph(Vector(("a", "b", 2.0)))
    val keep = graph.vertexSpectrum(
      4,
      SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.KeepZeroRow)
    ).toOption.get
    val identity = graph.vertexSpectrum(
      4,
      SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.IdentityOnZeroStrength)
    ).toOption.get

    assertEquals(keep.support.zeroStrengthVertices, Vector("c", "d"))
    assertEquals(keep.expectedZeroEigenvalueMultiplicity, 3)
    assertEquals(identity.expectedZeroEigenvalueMultiplicity, 1)
    assertEquals(keep.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance), 3)
    assertEquals(identity.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance), 1)
    graph.vertexSpectrum(
      2,
      SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.Error)
    ) match
      case Left(GraphLinalgError.ZeroStrengthVertex("c")) => ()
      case other => fail(s"expected zero-strength vertex c, got $other")

  test("spectral embedding drops the expected nullspace and retains numerical diagnostics"):
    val graph = weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val embedding = graph.spectralEmbedding(2).toOption.get

    assertEquals(embedding.basis, graph.basis)
    assertEquals(embedding.coordinates.rows, 4)
    assertEquals(embedding.coordinates.cols, 2)
    assertEquals(embedding.droppedEigenvectors, 1)
    assertVectorClose(
      embedding.eigenvalues.toSeq.toVector,
      Vector(2.0 - Math.sqrt(2.0), 2.0),
      tolerance
    )
    assert(embedding.residualNorms.toSeq.forall(_ < tolerance))

  test("cycle embedding is invariant under reindexing by repeated-eigenspace geometry"):
    val graph = weightedGraph(
      Vector(
        ("a", "b", 1.0),
        ("b", "c", 1.0),
        ("c", "d", 1.0),
        ("d", "a", 1.0)
      )
    )
    val reordered = graph.reindex(Vector("d", "b", "a", "c")).toOption.get.graph
    val first = graph.spectralEmbedding(2).toOption.get
    val second = reordered.spectralEmbedding(2).toOption.get

    assertVectorClose(first.eigenvalues.toSeq.toVector, Vector(2.0, 2.0), tolerance)
    assertVectorClose(second.eigenvalues.toSeq.toVector, first.eigenvalues.toSeq.toVector, tolerance)
    basis.keys.foreach: left =>
      basis.keys.foreach: right =>
        assertEqualsDouble(
          squaredDistance(first, left, right),
          squaredDistance(second, left, right),
          tolerance
        )

  test("largest spectrum remains available without changing support metadata"):
    val graph = weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum = graph.vertexSpectrum(
      1,
      spectrum = SpectralEnd.Largest
    ).toOption.get

    assertEqualsDouble(spectrum.result.eigenvalues(0), 2.0 + Math.sqrt(2.0), tolerance)
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 1)

  test("largest selection still uses Gale's ascending-algebraic result layout"):
    val graph = weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum = graph.vertexSpectrum(2, spectrum = SpectralEnd.Largest).toOption.get

    assertVectorClose(
      spectrum.result.eigenvalues.toSeq.toVector,
      Vector(2.0, 2.0 + Math.sqrt(2.0)),
      tolerance
    )

  test("spectrum rank is validated at the graph boundary"):
    val graph = weightedGraph(Vector(("a", "b", 1.0)))

    assertEquals(graph.vertexSpectrum(0), Left(GraphLinalgError.InvalidSpectrumRank(0, 4)))
    assertEquals(graph.vertexSpectrum(5), Left(GraphLinalgError.InvalidSpectrumRank(5, 4)))

  test("embedding dimensions fail when the requested geometry cannot fit after nullspace removal"):
    val edgeless = weightedGraph(Vector.empty)

    assert(edgeless.spectralEmbedding(0).isLeft)
    edgeless.spectralEmbedding(1) match
      case Left(GraphLinalgError.InvalidEmbeddingDimensions(1, 4, 4)) => ()
      case other => fail(s"expected embedding dimension error, got $other")

  private def weightedGraph(edges: Vector[(String, String, Double)]) =
    Graph.undirected(
      basis,
      edges.map: (from, to, value) =>
        (from, to, NonNegativeAffinity.unsafe(value))
    ).toOption.get

  private def squaredDistance(
      embedding: SpectralEmbedding[String, String],
      left: String,
      right: String
  ): Double =
    val leftRow = embedding.basis.indexOf(left).get.toInt
    val rightRow = embedding.basis.indexOf(right).get.toInt
    var total = 0.0
    var col = 0
    while col < embedding.coordinates.cols do
      val difference = embedding.coordinates(leftRow, col) - embedding.coordinates(rightRow, col)
      total += difference * difference
      col += 1
    total

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach: (left, right) =>
      assertEqualsDouble(left, right, tolerance)
