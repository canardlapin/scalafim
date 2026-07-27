package scalafim.graph.linalg

import gale.linalg.DMat
import gale.linalg.Matrix
import gale.spectral.Eigen
import gale.spectral.EigenSelection
import scalafim.graph.Graph
import scalafim.graph.VertexBasis
import multivar.family.kernel.Kernel
import multivar.core.MatrixView

class GraphSimilaritySuite extends munit.FunSuite:
  private val basis = VertexBasis.from(Vector("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")).toOption.get
  private val tolerance = 1e-8

  test("heat and diffusion diagonal features match the analytic two-vertex graph"):
    val smallBasis = VertexBasis.from(Vector("a" -> "A", "b" -> "B")).toOption.get
    val graph = Graph.undirected(
      smallBasis,
      Vector(("a", "b", NonNegativeAffinity.unsafe(1.0)))
    ).toOption.get
    val spectrum = graph.vertexSpectrum(2).toOption.get
    val heat = SpectralDiagonalFeature.from(Vector(0.0, 1.0)).toOption.get(spectrum).toOption.get
    val diffusion = SpectralDiagonalFeature
      .from(Vector(1.0), SpectralDiagonalKind.DiffusionEnergy)
      .toOption
      .get(spectrum)
      .toOption
      .get

    assertEqualsDouble(heat.values(0, 0), 1.0, tolerance)
    assertEqualsDouble(heat.values(1, 0), 1.0, tolerance)
    assertEqualsDouble(heat.values(0, 1), 0.5 * (1.0 + Math.exp(-2.0)), tolerance)
    assertEqualsDouble(heat.values(1, 1), heat.values(0, 1), tolerance)
    assertEqualsDouble(diffusion.values(0, 0), 0.5 * (1.0 + Math.exp(-4.0)), tolerance)

  test("feature similarities require ordered bases and support explicit key alignment"):
    val graph = weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 2.0)))
    val reordered = graph.reindex(Vector("d", "b", "a", "c")).toOption.get.graph
    val featurePlan = SpectralDiagonalFeature.from(Vector(0.25, 1.0)).toOption.get
    val left = featurePlan(graph.vertexSpectrum(4).toOption.get).toOption.get
    val right = featurePlan(reordered.vertexSpectrum(4).toOption.get).toOption.get
    val similarity = LinearFeatureSimilarity[String, String]()

    assert(similarity(left, right).isLeft)
    val aligned = right.alignTo(left.basis).toOption.get
    assertMatrixClose(aligned.values, left.values, tolerance)
    assertEqualsDouble(similarity(left, aligned).toOption.get, similarity(left, left).toOption.get, tolerance)

  test("linear and RBF feature similarities declare PSD constructions and form PSD Gram matrices"):
    val featurePlan = SpectralDiagonalFeature.from(Vector(0.1, 0.5, 1.5)).toOption.get
    val graphs = Vector(
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0))),
      weightedGraph(Vector(("a", "b", 2.0), ("b", "c", 0.5), ("c", "d", 1.5))),
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0), ("d", "a", 1.0)))
    )
    val features = graphs.map: graph =>
      featurePlan(graph.vertexSpectrum(4).toOption.get).toOption.get
    val linear = LinearFeatureSimilarity[String, String]()
    val rbf = RbfFeatureSimilarity.from[String, String](0.75).toOption.get

    assert(linear.psdStatus.isInstanceOf[PsdStatus.KnownPsd])
    assert(rbf.psdStatus.isInstanceOf[PsdStatus.KnownPsd])
    assertPsd(gram(features, linear))
    assertPsd(gram(features, rbf))

  test("known-PSD linear features adapt exactly to the existing multivar linear kernel"):
    val featurePlan = SpectralDiagonalFeature.from(Vector(0.2, 1.0)).toOption.get
    val graphs = Vector(
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0))),
      weightedGraph(Vector(("a", "b", 2.0), ("b", "c", 1.0), ("c", "d", 0.5))),
      weightedGraph(Vector(("a", "b", 1.0), ("a", "d", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    )
    val features = graphs.map: graph =>
      featurePlan(graph.vertexSpectrum(4).toOption.get).toOption.get
    val rows = Matrix.dense(
      features.length,
      features.head.toVector.length,
      features.flatMap(_.toVector.toSeq)
    )
    val multivarGram = Kernel.linear.compute(MatrixView.dense(rows), MatrixView.dense(rows)).toOption.get
    val directGram = gram(features, LinearFeatureSimilarity[String, String]())

    assertMatrixClose(multivarGram, directGram, tolerance)

  test("unjustified similarities remain explicitly outside the PSD adapter path"):
    val status = NegativeEuclideanFeatureSimilarity[String, String]().psdStatus

    status match
      case PsdStatus.NotEstablished(reason) => assert(reason.contains("not a declared PSD kernel"))
      case other                            => fail(s"expected unestablished PSD status, got $other")

  test("feature and similarity parameters fail at construction boundaries"):
    assert(SpectralDiagonalFeature.from(Vector.empty).isLeft)
    assert(SpectralDiagonalFeature.from(Vector(-1.0)).isLeft)
    assert(SpectralDiagonalFeature.from(Vector(Double.NaN)).isLeft)
    assert(RbfFeatureSimilarity.from[String, String](0.0).isLeft)

  private def weightedGraph(edges: Vector[(String, String, Double)]) =
    Graph.undirected(
      basis,
      edges.map: (from, to, value) =>
        (from, to, NonNegativeAffinity.unsafe(value))
    ).toOption.get

  private def gram(
      features: Vector[VertexFeatureSet[String, String]],
      similarity: GraphSimilarity[VertexFeatureSet[String, String]]
  ): DMat =
    Matrix.dense(
      features.length,
      features.length,
      features.flatMap(left => features.map(right => similarity(left, right).toOption.get))
    )

  private def assertPsd(matrix: DMat): Unit =
    val eigen = Eigen.eigSymmetric(matrix, EigenSelection.All).toOption.get
    assert(eigen.eigenvalues.toSeq.forall(_ >= -tolerance), clue = eigen.eigenvalues.toSeq)

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
