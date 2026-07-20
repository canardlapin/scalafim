package scalafim.graph.linalg

import scalafim.graph.Graph
import scalafim.graph.UndirectedGraph
import scalafim.graph.connectedComponents
import scalafim.linalg.DecompositionRank
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.linalg.PartialSpectrum
import scalafim.linalg.PartialSymmetricEigenResult
import scalafim.linalg.PartialSymmetricEigenSolver
import scalafim.linalg.SymmetricOperator

object GraphSpectral:
  def spectrum[K, V, E](
      graph: UndirectedGraph[K, V, E],
      rank: DecompositionRank,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: PartialSpectrum = PartialSpectrum.Smallest
  )(using
      weight: NonNegativeAdjacencyWeight[E],
      solver: PartialSymmetricEigenSolver
  ): Either[GraphLinalgError[K], VertexSpectrum[K, V]] =
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
  )(using
      weight: NonNegativeAdjacencyWeight[E],
      solver: PartialSymmetricEigenSolver
  ): Either[GraphLinalgError[K], SpectralEmbedding[K, V]] =
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
      else DecompositionRank.bounded(requested.toInt, graph.order) match
        case Left(error) => Left(GraphLinalgError.EigenFailure(error))
        case Right(rank) =>
          decompose(prepared.operator, rank, PartialSpectrum.Smallest).map: result =>
            new SpectralEmbedding(
              graph.basis,
              sliceVector(result.values, dropped, dimensions),
              sliceColumns(result.vectors, dropped, dimensions),
              sliceVector(result.residualNorms, dropped, dimensions),
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
      rank: DecompositionRank,
      spectrum: PartialSpectrum
  )(using solver: PartialSymmetricEigenSolver): Either[GraphLinalgError[Nothing], PartialSymmetricEigenResult] =
    SymmetricOperator
      .declared(operator.map)
      .left
      .map(GraphLinalgError.EigenFailure.apply)
      .flatMap(symmetric => solver.decompose(symmetric, rank, spectrum).left.map(GraphLinalgError.EigenFailure.apply))

  private def sliceVector(values: DoubleVector, start: Int, count: Int): DoubleVector =
    DoubleVector.fromSeq(Vector.tabulate(count)(offset => values(start + offset)))

  private def sliceColumns(matrix: DoubleMatrix, start: Int, count: Int): DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector.tabulate(matrix.rows): row =>
        Vector.tabulate(count)(offset => matrix(row, start + offset))
    )

extension [K, V, E](graph: UndirectedGraph[K, V, E])
  def vertexSpectrum(
      rank: DecompositionRank,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: PartialSpectrum = PartialSpectrum.Smallest
  )(using
      weight: NonNegativeAdjacencyWeight[E],
      solver: PartialSymmetricEigenSolver
  ): Either[GraphLinalgError[K], VertexSpectrum[K, V]] =
    GraphSpectral.spectrum(graph, rank, operator, spectrum)

  def spectralEmbedding(
      dimensions: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      eigenvectors: EmbeddingEigenvectors = EmbeddingEigenvectors.DropExpectedNullspace
  )(using
      weight: NonNegativeAdjacencyWeight[E],
      solver: PartialSymmetricEigenSolver
  ): Either[GraphLinalgError[K], SpectralEmbedding[K, V]] =
    GraphSpectral.embedding(graph, dimensions, operator, eigenvectors)
