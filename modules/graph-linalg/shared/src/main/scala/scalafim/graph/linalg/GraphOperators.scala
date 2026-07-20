package scalafim.graph.linalg

import gale.linalg.Vec
import gale.sparse.COOBuilder
import gale.sparse.Sparse
import scalafim.graph.Direction
import scalafim.graph.Graph
import scalafim.graph.UndirectedGraph
import scalafim.graph.DirectedGraph
import scalafim.graph.VertexIx

object GraphOperators:
  def topologyAdjacency[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E]
  ): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    val entries = Sparse.coo(graph.order, graph.order)
    var edgeIndex = 0
    while edgeIndex < graph.size do
      val edge = graph.edge(edgeIndex)
      entries.add(edge.endpoints.first.toInt, edge.endpoints.second.toInt, 1.0)
      if graph.direction == Direction.Undirected then
        entries.add(edge.endpoints.second.toInt, edge.endpoints.first.toInt, 1.0)
      edgeIndex += 1
    Right(vertexOperator(graph, entries))

  def weightedAdjacency[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    validatedWeights(graph, WeightRequirement.Finite).flatMap: weights =>
      val entries = Sparse.coo(graph.order, graph.order)
      var edgeIndex = 0
      while edgeIndex < graph.size do
        val edge = graph.edge(edgeIndex)
        val value = weights(edgeIndex)
        entries.add(edge.endpoints.first.toInt, edge.endpoints.second.toInt, value)
        if graph.direction == Direction.Undirected then
          entries.add(edge.endpoints.second.toInt, edge.endpoints.first.toInt, value)
        edgeIndex += 1
      Right(vertexOperator(graph, entries))

  def degree[K, V, E](graph: UndirectedGraph[K, V, E]): VertexSignal[K, V] =
    val values = Array.ofDim[Double](graph.order)
    var vertex = 0
    while vertex < graph.order do
      values(vertex) = graph.outgoingEdges(VertexIx.unsafe(vertex)).length.toDouble
      vertex += 1
    new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def outDegree[K, V, E](graph: DirectedGraph[K, V, E]): VertexSignal[K, V] =
    val values = Array.ofDim[Double](graph.order)
    var vertex = 0
    while vertex < graph.order do
      values(vertex) = graph.outgoingEdges(VertexIx.unsafe(vertex)).length.toDouble
      vertex += 1
    new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def inDegree[K, V, E](graph: DirectedGraph[K, V, E]): VertexSignal[K, V] =
    val values = Array.ofDim[Double](graph.order)
    var vertex = 0
    while vertex < graph.order do
      values(vertex) = graph.incomingEdges(VertexIx.unsafe(vertex)).length.toDouble
      vertex += 1
    new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def strength[K, V, E](
      graph: UndirectedGraph[K, V, E]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    validatedWeights(graph, WeightRequirement.Finite).map: weights =>
      val values = undirectedStrength(graph, weights)
      new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def outStrength[K, V, E](
      graph: DirectedGraph[K, V, E]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    validatedWeights(graph, WeightRequirement.Finite).map: weights =>
      val values = Array.ofDim[Double](graph.order)
      var edgeIndex = 0
      while edgeIndex < graph.size do
        values(graph.edge(edgeIndex).endpoints.first.toInt) += weights(edgeIndex)
        edgeIndex += 1
      new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def inStrength[K, V, E](
      graph: DirectedGraph[K, V, E]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    validatedWeights(graph, WeightRequirement.Finite).map: weights =>
      val values = Array.ofDim[Double](graph.order)
      var edgeIndex = 0
      while edgeIndex < graph.size do
        values(graph.edge(edgeIndex).endpoints.second.toInt) += weights(edgeIndex)
        edgeIndex += 1
      new VertexSignal(graph.basis, Vec.tabulate(values.length)(values.apply))

  def incidence[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E]
  ): Either[GraphLinalgError[K], IncidenceOperator[D, K, V]] =
    val entries = Sparse.coo(graph.order, graph.size)
    var edgeIndex = 0
    while edgeIndex < graph.size do
      val edge = graph.edge(edgeIndex)
      entries.add(edge.endpoints.first.toInt, edgeIndex, -1.0)
      entries.add(edge.endpoints.second.toInt, edgeIndex, 1.0)
      edgeIndex += 1
    Right(new IncidenceOperator(graph.basis, EdgeBasis.fromGraph(graph), entries.pruneZeros.toCSR()))

  def combinatorialLaplacian[K, V, E](
      graph: UndirectedGraph[K, V, E]
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    validatedWeights(graph, WeightRequirement.NonNegative).flatMap: weights =>
      val strengths = undirectedStrength(graph, weights)
      val entries = Sparse.coo(graph.order, graph.order)
      var vertex = 0
      while vertex < graph.order do
        entries.add(vertex, vertex, strengths(vertex))
        vertex += 1
      var edgeIndex = 0
      while edgeIndex < graph.size do
        val edge = graph.edge(edgeIndex)
        val value = -weights(edgeIndex)
        entries.add(edge.endpoints.first.toInt, edge.endpoints.second.toInt, value)
        entries.add(edge.endpoints.second.toInt, edge.endpoints.first.toInt, value)
        edgeIndex += 1
      Right(vertexOperator(graph, entries))

  def normalizedLaplacian[K, V, E](
      graph: UndirectedGraph[K, V, E],
      kind: NormalizedLaplacian,
      zeroStrengthPolicy: ZeroStrengthPolicy
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    validatedWeights(graph, WeightRequirement.NonNegative).flatMap: weights =>
      val strengths = undirectedStrength(graph, weights)
      firstZeroStrength(graph, strengths, zeroStrengthPolicy) match
        case Some(error) => Left(error)
        case None =>
          val entries = Sparse.coo(graph.order, graph.order)
          var vertex = 0
          while vertex < graph.order do
            if strengths(vertex) > 0.0 || zeroStrengthPolicy == ZeroStrengthPolicy.IdentityOnZeroStrength then
              entries.add(vertex, vertex, 1.0)
            vertex += 1

          var edgeIndex = 0
          while edgeIndex < graph.size do
            val edge = graph.edge(edgeIndex)
            val left = edge.endpoints.first.toInt
            val right = edge.endpoints.second.toInt
            val edgeWeight = weights(edgeIndex)
            if edgeWeight != 0.0 then
              kind match
                case NormalizedLaplacian.Symmetric =>
                  val value = -edgeWeight / Math.sqrt(strengths(left) * strengths(right))
                  entries.add(left, right, value)
                  entries.add(right, left, value)
                case NormalizedLaplacian.RandomWalk =>
                  entries.add(left, right, -edgeWeight / strengths(left))
                  entries.add(right, left, -edgeWeight / strengths(right))
            edgeIndex += 1
          Right(vertexOperator(graph, entries))

  private def validatedWeights[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      requirement: WeightRequirement
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], Array[Double]] =
    val values = new Array[Double](graph.size)
    var edgeIndex = 0
    var error = Option.empty[GraphLinalgError[K]]
    while edgeIndex < graph.size && error.isEmpty do
      val edge = graph.edge(edgeIndex)
      val value = weight.weight(edge.value)
      val invalid =
        !value.isFinite ||
          (requirement == WeightRequirement.NonNegative && value < 0.0) ||
          (requirement == WeightRequirement.StrictlyPositive && value <= 0.0)
      if invalid then
        error = Some(
          GraphLinalgError.InvalidWeight(
            edgeIndex,
            graph.basis.keyAt(edge.endpoints.first),
            graph.basis.keyAt(edge.endpoints.second),
            value,
            requirement
          )
        )
      else values(edgeIndex) = value
      edgeIndex += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(values)

  private def undirectedStrength[K, V, E](
      graph: UndirectedGraph[K, V, E],
      weights: Array[Double]
  ): Array[Double] =
    val strengths = Array.ofDim[Double](graph.order)
    var edgeIndex = 0
    while edgeIndex < graph.size do
      val edge = graph.edge(edgeIndex)
      val value = weights(edgeIndex)
      strengths(edge.endpoints.first.toInt) += value
      strengths(edge.endpoints.second.toInt) += value
      edgeIndex += 1
    strengths

  private def firstZeroStrength[K, V, E](
      graph: UndirectedGraph[K, V, E],
      strengths: Array[Double],
      policy: ZeroStrengthPolicy
  ): Option[GraphLinalgError[K]] =
    if policy != ZeroStrengthPolicy.Error then None
    else
      var vertex = 0
      var error = Option.empty[GraphLinalgError[K]]
      while vertex < graph.order && error.isEmpty do
        if strengths(vertex) == 0.0 then
          error = Some(GraphLinalgError.ZeroStrengthVertex(graph.basis.keyAt(VertexIx.unsafe(vertex))))
        vertex += 1
      error

  private def vertexOperator[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      entries: COOBuilder
  ): VertexOperator[K, V] =
    new VertexOperator(graph.basis, entries.pruneZeros.toCSR())

extension [D <: Direction, K, V, E](graph: Graph[D, K, V, E])
  def topologyAdjacency: Either[GraphLinalgError[K], VertexOperator[K, V]] =
    GraphOperators.topologyAdjacency(graph)

  def weightedAdjacency(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    GraphOperators.weightedAdjacency(graph)

  def incidence: Either[GraphLinalgError[K], IncidenceOperator[D, K, V]] =
    GraphOperators.incidence(graph)

extension [K, V, E](graph: UndirectedGraph[K, V, E])
  def degree: VertexSignal[K, V] =
    GraphOperators.degree(graph)

  def strength(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    GraphOperators.strength(graph)

  def combinatorialLaplacian(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    GraphOperators.combinatorialLaplacian(graph)

  def normalizedLaplacian(
      kind: NormalizedLaplacian,
      zeroStrengthPolicy: ZeroStrengthPolicy
  )(using weight: NonNegativeAdjacencyWeight[E]): Either[GraphLinalgError[K], VertexOperator[K, V]] =
    GraphOperators.normalizedLaplacian(graph, kind, zeroStrengthPolicy)

extension [K, V, E](graph: DirectedGraph[K, V, E])
  def outDegree: VertexSignal[K, V] =
    GraphOperators.outDegree(graph)

  def inDegree: VertexSignal[K, V] =
    GraphOperators.inDegree(graph)

  def outStrength(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    GraphOperators.outStrength(graph)

  def inStrength(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[K], VertexSignal[K, V]] =
    GraphOperators.inStrength(graph)
