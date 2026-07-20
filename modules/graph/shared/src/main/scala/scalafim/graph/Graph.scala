package scalafim.graph

import scala.collection.mutable

enum Direction:
  case Directed
  case Undirected

sealed trait Endpoints[D <: Direction]:
  def first: VertexIx
  def second: VertexIx

  final def vertices: (VertexIx, VertexIx) =
    (first, second)

  private[graph] def mapIndices(f: VertexIx => VertexIx): Endpoints[D]

object Endpoints:
  final case class Arc private[graph] (from: VertexIx, to: VertexIx) extends Endpoints[Direction.Directed.type]:
    def first: VertexIx = from
    def second: VertexIx = to
    private[graph] def mapIndices(f: VertexIx => VertexIx): Endpoints[Direction.Directed.type] =
      Arc(f(from), f(to))

  final case class Link private[graph] (a: VertexIx, b: VertexIx) extends Endpoints[Direction.Undirected.type]:
    require(a.toInt < b.toInt, "undirected endpoints must be canonical and distinct")
    def first: VertexIx = a
    def second: VertexIx = b
    private[graph] def mapIndices(f: VertexIx => VertexIx): Endpoints[Direction.Undirected.type] =
      val left = f(a)
      val right = f(b)
      if left.toInt < right.toInt then Link(left, right) else Link(right, left)

final class Edge[D <: Direction, +E] private[graph] (
    val endpoints: Endpoints[D],
    val value: E
):
  override def equals(other: Any): Boolean =
    other match
      case that: Edge[?, ?] => endpoints == that.endpoints && value == that.value
      case _                => false

  override def hashCode(): Int =
    31 * endpoints.hashCode + value.##

  override def toString: String =
    s"Edge(${endpoints.vertices},$value)"

type DirectedGraph[K, V, E] = Graph[Direction.Directed.type, K, V, E]
type UndirectedGraph[K, V, E] = Graph[Direction.Undirected.type, K, V, E]

final class Graph[D <: Direction, K, V, E] private[graph] (
    val direction: D,
    val basis: VertexBasis[K, V],
    val edges: Vector[Edge[D, E]],
    private val outgoing: Vector[Vector[Int]],
    private val incoming: Vector[Vector[Int]],
    private val edgeIndexByEndpoints: Map[(Int, Int), Int]
):
  require(outgoing.length == basis.size, "outgoing adjacency rows must match basis size")
  require(incoming.length == basis.size, "incoming adjacency rows must match basis size")

  def order: Int =
    basis.size

  def size: Int =
    edges.length

  def vertices: Vector[VertexIx] =
    basis.indices

  def edge(index: Int): Edge[D, E] =
    edges(index)

  def outgoingEdges(vertex: VertexIx): Vector[Edge[D, E]] =
    requireVertex(vertex)
    outgoing(vertex.toInt).map(edges)

  def incomingEdges(vertex: VertexIx): Vector[Edge[D, E]] =
    requireVertex(vertex)
    incoming(vertex.toInt).map(edges)

  private[graph] def outgoingEdgeIndices(vertex: VertexIx): Vector[Int] =
    requireVertex(vertex)
    outgoing(vertex.toInt)

  private[graph] def incomingEdgeIndices(vertex: VertexIx): Vector[Int] =
    requireVertex(vertex)
    incoming(vertex.toInt)

  def edgeBetween(first: K, second: K): Option[Edge[D, E]] =
    for
      left <- basis.indexOf(first)
      right <- basis.indexOf(second)
      key = endpointKey(left, right)
      edgeIndex <- edgeIndexByEndpoints.get(key)
    yield edges(edgeIndex)

  def containsEdge(first: K, second: K): Boolean =
    edgeBetween(first, second).nonEmpty

  def mapVertexMetadata[V2](f: (K, V) => V2): Graph[D, K, V2, E] =
    new Graph(direction, basis.mapValues(f), edges, outgoing, incoming, edgeIndexByEndpoints)

  def mapEdgeValues[E2](f: E => E2): Graph[D, K, V, E2] =
    val mapped = edges.map(edge => new Edge(edge.endpoints, f(edge.value)))
    new Graph(direction, basis, mapped, outgoing, incoming, edgeIndexByEndpoints)

  def inducedByKeys(keys: Iterable[K]): Either[BasisError[K], InducedGraph[D, K, V, E]] =
    basis.inducedByKeys(keys).map: inducedBasis =>
      val retained = edges.flatMap: edge =>
        for
          first <- inducedBasis.inducedOf(edge.endpoints.first)
          second <- inducedBasis.inducedOf(edge.endpoints.second)
        yield new Edge(edge.endpoints.mapIndices(index => inducedBasis.inducedOf(index).get), edge.value)
      val graph = Graph.fromCanonical(direction, inducedBasis.basis, retained)
      new InducedGraph(graph, inducedBasis)

  def reindex(orderedKeys: Iterable[K]): Either[BasisError[K], ReindexedGraph[D, K, V, E]] =
    basis.reindex(orderedKeys).map: reindexedBasis =>
      val remapped = edges.map: edge =>
        new Edge(edge.endpoints.mapIndices(reindexedBasis.permutation.targetOf), edge.value)
      val graph = Graph.fromCanonical(direction, reindexedBasis.basis, remapped)
      new ReindexedGraph(graph, reindexedBasis.permutation)

  private def endpointKey(first: VertexIx, second: VertexIx): (Int, Int) =
    direction match
      case Direction.Directed =>
        (first.toInt, second.toInt)
      case Direction.Undirected =>
        if first.toInt <= second.toInt then (first.toInt, second.toInt)
        else (second.toInt, first.toInt)

  private def requireVertex(vertex: VertexIx): Unit =
    require(basis.contains(vertex), s"vertex index ${vertex.toInt} out of bounds for graph order $order")

  override def equals(other: Any): Boolean =
    other match
      case that: Graph[?, ?, ?, ?] =>
        direction == that.direction && basis == that.basis && edges == that.edges
      case _ => false

  override def hashCode(): Int =
    var result = direction.hashCode
    result = 31 * result + basis.hashCode
    31 * result + edges.hashCode

  override def toString: String =
    s"Graph($direction,$basis,${edges.mkString("[", ",", "]")})"

object Graph:
  def directed[K, V, E](
      basis: VertexBasis[K, V],
      edges: Iterable[(K, K, E)]
  ): Either[GraphBuildErrors[K], DirectedGraph[K, V, E]] =
    build(Direction.Directed, basis, edges, (from, to) => Endpoints.Arc(from, to), canonicalUndirected = false)

  def undirected[K, V, E](
      basis: VertexBasis[K, V],
      edges: Iterable[(K, K, E)]
  ): Either[GraphBuildErrors[K], UndirectedGraph[K, V, E]] =
    build(
      Direction.Undirected,
      basis,
      edges,
      (first, second) =>
        if first.toInt < second.toInt then Endpoints.Link(first, second)
        else Endpoints.Link(second, first),
      canonicalUndirected = true
    )

  private def build[D <: Direction, K, V, E](
      direction: D,
      basis: VertexBasis[K, V],
      inputEdges: Iterable[(K, K, E)],
      endpoints: (VertexIx, VertexIx) => Endpoints[D],
      canonicalUndirected: Boolean
  ): Either[GraphBuildErrors[K], Graph[D, K, V, E]] =
    val errors = Vector.newBuilder[GraphBuildError[K]]
    val built = Vector.newBuilder[Edge[D, E]]
    val seen = mutable.HashSet.empty[(Int, Int)]
    val values = inputEdges.toVector
    var edgeIndex = 0
    while edgeIndex < values.length do
      val (firstKey, secondKey, value) = values(edgeIndex)
      val first = basis.indexOf(firstKey)
      val second = basis.indexOf(secondKey)
      if first.isEmpty then errors += GraphBuildError.UnknownVertex(edgeIndex, EndpointRole.Source, firstKey)
      if second.isEmpty then errors += GraphBuildError.UnknownVertex(edgeIndex, EndpointRole.Target, secondKey)
      (first, second) match
        case (Some(left), Some(right)) if left == right =>
          errors += GraphBuildError.SelfEdge(edgeIndex, firstKey)
        case (Some(left), Some(right)) =>
          val key =
            if canonicalUndirected && left.toInt > right.toInt then (right.toInt, left.toInt)
            else (left.toInt, right.toInt)
          if !seen.add(key) then
            errors += GraphBuildError.DuplicateEdge(edgeIndex, basis.keyAt(VertexIx.unsafe(key._1)), basis.keyAt(VertexIx.unsafe(key._2)))
          else
            built += new Edge(endpoints(VertexIx.unsafe(key._1), VertexIx.unsafe(key._2)), value)
        case _ =>
          ()
      edgeIndex += 1

    val errorValues = errors.result()
    if errorValues.nonEmpty then Left(GraphBuildErrors.from(errorValues))
    else Right(fromCanonical(direction, basis, built.result()))

  private[graph] def fromCanonical[D <: Direction, K, V, E](
      direction: D,
      basis: VertexBasis[K, V],
      inputEdges: Vector[Edge[D, E]]
  ): Graph[D, K, V, E] =
    val sorted = inputEdges.sortBy(edge => (edge.endpoints.first.toInt, edge.endpoints.second.toInt))
    val outgoingBuilders = Array.fill(basis.size)(Vector.newBuilder[Int])
    val incomingBuilders = Array.fill(basis.size)(Vector.newBuilder[Int])
    val endpointIndices = mutable.HashMap.empty[(Int, Int), Int]
    var index = 0
    while index < sorted.length do
      val edge = sorted(index)
      val first = edge.endpoints.first.toInt
      val second = edge.endpoints.second.toInt
      endpointIndices((first, second)) = index
      direction match
        case Direction.Directed =>
          outgoingBuilders(first) += index
          incomingBuilders(second) += index
        case Direction.Undirected =>
          outgoingBuilders(first) += index
          outgoingBuilders(second) += index
          incomingBuilders(first) += index
          incomingBuilders(second) += index
      index += 1

    new Graph(
      direction,
      basis,
      sorted,
      outgoingBuilders.toVector.map(_.result()),
      incomingBuilders.toVector.map(_.result()),
      endpointIndices.toMap
    )

final class InducedGraph[D <: Direction, K, V, E] private[graph] (
    val graph: Graph[D, K, V, E],
    val basisMapping: InducedVertexBasis[K, V]
):
  require(graph.basis == basisMapping.basis, "induced graph basis must match its basis mapping")

  override def equals(other: Any): Boolean =
    other match
      case that: InducedGraph[?, ?, ?, ?] => graph == that.graph && basisMapping == that.basisMapping
      case _                              => false

  override def hashCode(): Int =
    31 * graph.hashCode + basisMapping.hashCode

final class ReindexedGraph[D <: Direction, K, V, E] private[graph] (
    val graph: Graph[D, K, V, E],
    val permutation: BasisPermutation[K]
):
  require(graph.basis.keys == permutation.targetKeys, "reindexed graph basis must match permutation target keys")

  override def equals(other: Any): Boolean =
    other match
      case that: ReindexedGraph[?, ?, ?, ?] => graph == that.graph && permutation == that.permutation
      case _                                => false

  override def hashCode(): Int =
    31 * graph.hashCode + permutation.hashCode
