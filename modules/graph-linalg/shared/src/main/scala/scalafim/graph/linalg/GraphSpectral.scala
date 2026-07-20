package scalafim.graph.linalg

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.Matrix
import gale.spectral.Eigen
import gale.spectral.EigenDecomposition
import gale.spectral.EigenOrder
import gale.spectral.EigenSelection
import scalafim.graph.Graph
import scalafim.graph.UndirectedGraph
import scalafim.graph.connectedComponents

object GraphSpectral:
  def spectrum[K, V, E](
      graph: UndirectedGraph[K, V, E],
      rank: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: SpectralEnd = SpectralEnd.Smallest
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSpectrum[K, V]] =
    prepare(graph, operator).flatMap: prepared =>
      decompose(prepared.operator, rank, spectrum).map: result =>
        new VertexSpectrum(
          graph.basis,
          result,
          operator,
          prepared.support,
          prepared.expectedNullity
        )

  def embedding[K, V, E](
      graph: UndirectedGraph[K, V, E],
      dimensions: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      eigenvectors: EmbeddingEigenvectors = EmbeddingEigenvectors.DropExpectedNullspace
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], SpectralEmbedding[K, V]] =
    if dimensions <= 0 then
      Left(GraphLinalgError.InvalidEmbeddingDimensions(dimensions, graph.order, 0))
    else prepare(graph, operator).flatMap: prepared =>
      val dropped =
        eigenvectors match
          case EmbeddingEigenvectors.IncludeSmallest       => 0
          case EmbeddingEigenvectors.DropExpectedNullspace => prepared.expectedNullity
      val requested = dimensions.toLong + dropped.toLong
      if requested > graph.order.toLong then
        Left(GraphLinalgError.InvalidEmbeddingDimensions(dimensions, graph.order, dropped))
      else
        decompose(prepared.operator, requested.toInt, SpectralEnd.Smallest).map: result =>
          new SpectralEmbedding(
            graph.basis,
            sliceVector(result.eigenvalues, dropped, dimensions),
            sliceColumns(result.eigenvectors, dropped, dimensions),
            sliceVector(result.diagnostics.residuals, dropped, dimensions),
            operator,
            prepared.support,
            dropped
          )

  private final case class Prepared[K, V](
      operator: VertexOperator[K, V],
      support: WeightedSupport[K, V],
      expectedNullity: Int
  )

  private def prepare[K, V, E](
      graph: UndirectedGraph[K, V, E],
      kind: SpectralLaplacian
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], Prepared[K, V]] =
    operatorFor(graph, kind).flatMap: operator =>
      supportFor(graph).map: support =>
        val expectedNullity =
          kind match
            case SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.IdentityOnZeroStrength) =>
              support.components.length - support.zeroStrengthVertices.length
            case _ => support.components.length
        Prepared(operator, support, expectedNullity)

  private def operatorFor[K, V, E](
      graph: UndirectedGraph[K, V, E],
      kind: SpectralLaplacian
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    kind match
      case SpectralLaplacian.Combinatorial =>
        GraphOperators.combinatorialLaplacian(graph)
      case SpectralLaplacian.SymmetricNormalized(policy) =>
        GraphOperators.normalizedLaplacian(graph, NormalizedLaplacian.Symmetric, policy)

  private def supportFor[K, V, E](
      graph: UndirectedGraph[K, V, E]
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], WeightedSupport[K, V]] =
    val positiveEdges = Vector.newBuilder[(K, K, Unit)]
    val positiveDegree = Array.fill(graph.order)(0)
    var zeroEdges = 0
    var edgeIndex = 0
    while edgeIndex < graph.size do
      val edge = graph.edge(edgeIndex)
      val value = weight.weight(edge.value)
      if value == 0.0 then zeroEdges += 1
      else
        val first = edge.endpoints.first
        val second = edge.endpoints.second
        positiveDegree(first.toInt) += 1
        positiveDegree(second.toInt) += 1
        positiveEdges += ((graph.basis.keyAt(first), graph.basis.keyAt(second), ()))
      edgeIndex += 1

    Graph.undirected(graph.basis, positiveEdges.result()) match
      case Left(errors) => Left(GraphLinalgError.TopologyFailure(errors.message))
      case Right(supportGraph) =>
        val zeroStrength = graph.basis.indices.collect:
          case index if positiveDegree(index.toInt) == 0 => graph.basis.keyAt(index)
        Right(
          new WeightedSupport(
            graph.basis,
            supportGraph.connectedComponents,
            zeroEdges,
            zeroStrength
          )
        )

  private def decompose[K, V](
      operator: VertexOperator[K, V],
      rank: Int,
      spectrum: SpectralEnd
  ): Either[GraphLinalgError[Nothing], EigenDecomposition] =
    if rank <= 0 || rank > operator.basis.size then
      Left(GraphLinalgError.InvalidSpectrumRank(rank, operator.basis.size))
    else
      val selection =
        if rank == operator.basis.size then EigenSelection.All
        else
          val order =
            spectrum match
              case SpectralEnd.Smallest => EigenOrder.SmallestAlgebraic
              case SpectralEnd.Largest  => EigenOrder.LargestAlgebraic
          EigenSelection.Count(rank, order)
      // Gale's current single-vector Lanczos path does not recover repeated
      // eigenvalue multiplicity. Graph embeddings need the complete repeated
      // eigenspace, so select from Gale's dense symmetric decomposition.
      val decomposed = Eigen.eigSymmetric(operator.matrix.toDense(), selection)
      decomposed
        .left
        .map(GraphLinalgError.EigenFailure.apply)
        .flatMap(_.requireConverged.left.map(GraphLinalgError.EigenFailure.apply))

  private def sliceVector(values: DVec, start: Int, count: Int): DVec =
    values.slice(start, start + count).copy

  private def sliceColumns(matrix: DMat, start: Int, count: Int): DMat =
    val out = Matrix.newBuilder(matrix.rows, count)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < count do
        out(row, col) = matrix(row, start + col)
        col += 1
      row += 1
    out.result()

extension [K, V, E](graph: UndirectedGraph[K, V, E])
  def vertexSpectrum(
      rank: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: SpectralEnd = SpectralEnd.Smallest
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSpectrum[K, V]] =
    GraphSpectral.spectrum(graph, rank, operator, spectrum)

  def spectralEmbedding(
      dimensions: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      eigenvectors: EmbeddingEigenvectors = EmbeddingEigenvectors.DropExpectedNullspace
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], SpectralEmbedding[K, V]] =
    GraphSpectral.embedding(graph, dimensions, operator, eigenvectors)
