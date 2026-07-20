package scalafim.graph

import scala.collection.mutable

trait EdgeCost[-E]:
  def cost(edge: E): Double

opaque type Distance = Double

enum DistanceError:
  case InvalidValue(value: Double)

  def message: String =
    this match
      case InvalidValue(value) => s"distance must be finite and non-negative, got $value"

object Distance:
  def from(value: Double): Either[DistanceError, Distance] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(DistanceError.InvalidValue(value))

  def unsafe(value: Double): Distance =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (distance: Distance)
    inline def value: Double = distance

  given EdgeCost[Distance] with
    def cost(edge: Distance): Double = edge.value

final case class Component[K](keys: Vector[K]):
  require(keys.nonEmpty, "component must contain at least one vertex")

final case class PathEdge[K, E](from: K, to: K, value: E, cost: Double):
  require(cost.isFinite && cost >= 0.0, "path edge cost must be finite and non-negative")

final case class ShortestPath[K, E](
    vertices: Vector[K],
    edges: Vector[PathEdge[K, E]],
    totalCost: Double
):
  require(vertices.nonEmpty, "shortest path must contain at least one vertex")
  require(edges.length + 1 == vertices.length, "shortest path edge count must be one less than vertex count")
  require(totalCost.isFinite && totalCost >= 0.0, "shortest path cost must be finite and non-negative")

sealed trait PathError[+K]:
  def message: String

object PathError:
  final case class UnknownVertex[K](key: K) extends PathError[K]:
    def message: String =
      s"unknown path vertex '$key'"

  final case class NoPath[K](source: K, target: K) extends PathError[K]:
    def message: String =
      s"no path exists from '$source' to '$target'"

  final case class InvalidEdgeCost[K](from: K, to: K, value: Double) extends PathError[K]:
    def message: String =
      s"edge ('$from','$to') has invalid cost $value; costs must be finite and non-negative"

final case class Cycle[K](keys: Vector[K]):
  require(keys.length >= 2, "cycle witness must contain at least one edge")
  require(keys.head == keys.last, "cycle witness must end at its starting key")

final class Dag[K, V, E] private (val graph: DirectedGraph[K, V, E]):
  def topologicalOrder: Vector[K] =
    DirectedAlgorithms.topologicalLayers(graph).toOption.get.flatten

  def topologicalLayers: Vector[Vector[K]] =
    DirectedAlgorithms.topologicalLayers(graph).toOption.get

object Dag:
  def from[K, V, E](graph: DirectedGraph[K, V, E]): Either[Cycle[K], Dag[K, V, E]] =
    graph.findCycle match
      case Some(cycle) => Left(cycle)
      case None        => Right(new Dag(graph))

extension [K, V, E](graph: UndirectedGraph[K, V, E])
  def connectedComponents: Vector[Component[K]] =
    GraphAlgorithmOps.components(graph, GraphAlgorithmOps.undirectedNeighbors(graph))

  def isConnected: Boolean =
    graph.order > 0 && graph.connectedComponents.length == 1

extension [K, V, E](graph: DirectedGraph[K, V, E])
  def weaklyConnectedComponents: Vector[Component[K]] =
    GraphAlgorithmOps.components(graph, GraphAlgorithmOps.weakNeighbors(graph))

  def stronglyConnectedComponents: Vector[Component[K]] =
    DirectedAlgorithms.stronglyConnectedComponents(graph)

  def findCycle: Option[Cycle[K]] =
    DirectedAlgorithms.findCycle(graph)

  def topologicalOrder: Either[Cycle[K], Vector[K]] =
    DirectedAlgorithms.topologicalLayers(graph).map(_.flatten)

  def topologicalLayers: Either[Cycle[K], Vector[Vector[K]]] =
    DirectedAlgorithms.topologicalLayers(graph)

extension [D <: Direction, K, V, E](graph: Graph[D, K, V, E])
  def shortestPath(source: K, target: K)(using edgeCost: EdgeCost[E]): Either[PathError[K], ShortestPath[K, E]] =
    GraphAlgorithmOps.shortestPath(graph, source, target)

private[graph] object GraphAlgorithmOps:
  def components[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      neighbors: Vector[Vector[Int]]
  ): Vector[Component[K]] =
    val visited = Array.fill(graph.order)(false)
    val result = Vector.newBuilder[Component[K]]
    var start = 0
    while start < graph.order do
      if !visited(start) then
        val queue = mutable.Queue.empty[Int]
        val vertices = Vector.newBuilder[Int]
        visited(start) = true
        queue.enqueue(start)
        while queue.nonEmpty do
          val vertex = queue.dequeue()
          vertices += vertex
          val row = neighbors(vertex)
          var i = 0
          while i < row.length do
            val next = row(i)
            if !visited(next) then
              visited(next) = true
              queue.enqueue(next)
            i += 1
        val keys = vertices.result().sorted.map(index => graph.basis.keyAt(VertexIx.unsafe(index)))
        result += Component(keys)
      start += 1
    result.result()

  def undirectedNeighbors[K, V, E](graph: UndirectedGraph[K, V, E]): Vector[Vector[Int]] =
    Vector.tabulate(graph.order): vertex =>
      val index = VertexIx.unsafe(vertex)
      graph.outgoingEdgeIndices(index).map: edgeIndex =>
        val edge = graph.edge(edgeIndex)
        if edge.endpoints.first == index then edge.endpoints.second.toInt else edge.endpoints.first.toInt

  def weakNeighbors[K, V, E](graph: DirectedGraph[K, V, E]): Vector[Vector[Int]] =
    Vector.tabulate(graph.order): vertex =>
      val index = VertexIx.unsafe(vertex)
      val outgoing = graph.outgoingEdgeIndices(index).map(edgeIndex => graph.edge(edgeIndex).endpoints.second.toInt)
      val incoming = graph.incomingEdgeIndices(index).map(edgeIndex => graph.edge(edgeIndex).endpoints.first.toInt)
      (outgoing ++ incoming).distinct.sorted

  def directedNeighbors[K, V, E](graph: DirectedGraph[K, V, E]): Vector[Vector[Int]] =
    Vector.tabulate(graph.order): vertex =>
      graph.outgoingEdgeIndices(VertexIx.unsafe(vertex)).map(edgeIndex => graph.edge(edgeIndex).endpoints.second.toInt)

  def reverseDirectedNeighbors[K, V, E](graph: DirectedGraph[K, V, E]): Vector[Vector[Int]] =
    Vector.tabulate(graph.order): vertex =>
      graph.incomingEdgeIndices(VertexIx.unsafe(vertex)).map(edgeIndex => graph.edge(edgeIndex).endpoints.first.toInt)

  def shortestPath[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      source: K,
      target: K
  )(using edgeCost: EdgeCost[E]): Either[PathError[K], ShortestPath[K, E]] =
    val sourceIndex = graph.basis.indexOf(source).toRight(PathError.UnknownVertex(source))
    val targetIndex = graph.basis.indexOf(target).toRight(PathError.UnknownVertex(target))
    for
      start <- sourceIndex
      end <- targetIndex
      costs <- validatedCosts(graph)
      path <- dijkstra(graph, start, end, costs, source, target)
    yield path

  private def validatedCosts[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E]
  )(using edgeCost: EdgeCost[E]): Either[PathError[K], Array[Double]] =
    val costs = new Array[Double](graph.size)
    var index = 0
    var error = Option.empty[PathError[K]]
    while index < graph.size && error.isEmpty do
      val edge = graph.edge(index)
      val cost = edgeCost.cost(edge.value)
      if !cost.isFinite || cost < 0.0 then
        error = Some(
          PathError.InvalidEdgeCost(
            graph.basis.keyAt(edge.endpoints.first),
            graph.basis.keyAt(edge.endpoints.second),
            cost
          )
        )
      else costs(index) = cost
      index += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(costs)

  private def dijkstra[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      source: VertexIx,
      target: VertexIx,
      costs: Array[Double],
      sourceKey: K,
      targetKey: K
  ): Either[PathError[K], ShortestPath[K, E]] =
    if source == target then
      Right(ShortestPath(Vector(sourceKey), Vector.empty, 0.0))
    else
      val distances = Array.fill(graph.order)(Double.PositiveInfinity)
      val previousEdge = Array.fill(graph.order)(-1)
      val visited = Array.fill(graph.order)(false)
      given Ordering[(Double, Int)] with
        def compare(left: (Double, Int), right: (Double, Int)): Int =
          val byDistance = java.lang.Double.compare(right._1, left._1)
          if byDistance != 0 then byDistance
          else java.lang.Integer.compare(right._2, left._2)
      val queue = mutable.PriorityQueue.empty[(Double, Int)]
      distances(source.toInt) = 0.0
      queue.enqueue((0.0, source.toInt))

      while queue.nonEmpty && !visited(target.toInt) do
        val (distance, vertex) = queue.dequeue()
        if !visited(vertex) then
          visited(vertex) = true
          val incident = graph.outgoingEdgeIndices(VertexIx.unsafe(vertex))
          var i = 0
          while i < incident.length do
            val edgeIndex = incident(i)
            val edge = graph.edge(edgeIndex)
            val next =
              graph.direction match
                case Direction.Directed => edge.endpoints.second.toInt
                case Direction.Undirected =>
                  if edge.endpoints.first.toInt == vertex then edge.endpoints.second.toInt
                  else edge.endpoints.first.toInt
            val candidate = distance + costs(edgeIndex)
            if candidate < distances(next) then
              distances(next) = candidate
              previousEdge(next) = edgeIndex
              queue.enqueue((candidate, next))
            i += 1

      if !distances(target.toInt).isFinite then Left(PathError.NoPath(sourceKey, targetKey))
      else Right(reconstructPath(graph, source, target, previousEdge, costs, distances(target.toInt)))

  private def reconstructPath[D <: Direction, K, V, E](
      graph: Graph[D, K, V, E],
      source: VertexIx,
      target: VertexIx,
      previousEdge: Array[Int],
      costs: Array[Double],
      totalCost: Double
  ): ShortestPath[K, E] =
    val reversedVertices = Vector.newBuilder[K]
    val reversedEdges = Vector.newBuilder[PathEdge[K, E]]
    var current = target
    reversedVertices += graph.basis.keyAt(current)
    while current != source do
      val edgeIndex = previousEdge(current.toInt)
      require(edgeIndex >= 0, "reachable path vertex must have a predecessor edge")
      val edge = graph.edge(edgeIndex)
      val previous =
        graph.direction match
          case Direction.Directed => edge.endpoints.first
          case Direction.Undirected =>
            if edge.endpoints.first == current then edge.endpoints.second else edge.endpoints.first
      reversedEdges += PathEdge(
        graph.basis.keyAt(previous),
        graph.basis.keyAt(current),
        edge.value,
        costs(edgeIndex)
      )
      current = previous
      reversedVertices += graph.basis.keyAt(current)

    ShortestPath(reversedVertices.result().reverse, reversedEdges.result().reverse, totalCost)

private[graph] object DirectedAlgorithms:
  def stronglyConnectedComponents[K, V, E](graph: DirectedGraph[K, V, E]): Vector[Component[K]] =
    val outgoing = GraphAlgorithmOps.directedNeighbors(graph)
    val incoming = GraphAlgorithmOps.reverseDirectedNeighbors(graph)
    val visited = Array.fill(graph.order)(false)
    val finishOrder = Vector.newBuilder[Int]
    var start = 0
    while start < graph.order do
      if !visited(start) then appendFinishOrder(start, outgoing, visited, finishOrder)
      start += 1

    java.util.Arrays.fill(visited, false)
    val components = Vector.newBuilder[Vector[Int]]
    val order = finishOrder.result().reverse
    var i = 0
    while i < order.length do
      val vertex = order(i)
      if !visited(vertex) then
        val members = collectReachable(vertex, incoming, visited).sorted
        components += members
      i += 1

    components.result().sortBy(_.headOption.getOrElse(Int.MaxValue)).map: members =>
      Component(members.map(index => graph.basis.keyAt(VertexIx.unsafe(index))))

  def findCycle[K, V, E](graph: DirectedGraph[K, V, E]): Option[Cycle[K]] =
    val neighbors = GraphAlgorithmOps.directedNeighbors(graph)
    val color = Array.fill(graph.order)(0)
    val parent = Array.fill(graph.order)(-1)
    var start = 0
    while start < graph.order do
      if color(start) == 0 then
        findCycleFrom(start, neighbors, color, parent) match
          case Some(indices) =>
            return Some(Cycle(indices.map(index => graph.basis.keyAt(VertexIx.unsafe(index)))))
          case None =>
            ()
      start += 1
    None

  def topologicalLayers[K, V, E](graph: DirectedGraph[K, V, E]): Either[Cycle[K], Vector[Vector[K]]] =
    val indegree = Array.fill(graph.order)(0)
    var edgeIndex = 0
    while edgeIndex < graph.size do
      indegree(graph.edge(edgeIndex).endpoints.second.toInt) += 1
      edgeIndex += 1

    var ready = Vector.tabulate(graph.order)(identity).filter(vertex => indegree(vertex) == 0)
    val layers = Vector.newBuilder[Vector[K]]
    var visited = 0
    while ready.nonEmpty do
      layers += ready.map(vertex => graph.basis.keyAt(VertexIx.unsafe(vertex)))
      visited += ready.length
      val next = mutable.TreeSet.empty[Int]
      var i = 0
      while i < ready.length do
        val vertex = ready(i)
        val outgoing = graph.outgoingEdgeIndices(VertexIx.unsafe(vertex))
        var j = 0
        while j < outgoing.length do
          val target = graph.edge(outgoing(j)).endpoints.second.toInt
          indegree(target) -= 1
          if indegree(target) == 0 then next += target
          j += 1
        i += 1
      ready = next.toVector

    if visited == graph.order then Right(layers.result())
    else Left(findCycle(graph).get)

  private def appendFinishOrder(
      start: Int,
      neighbors: Vector[Vector[Int]],
      visited: Array[Boolean],
      result: mutable.Builder[Int, Vector[Int]]
  ): Unit =
    val vertices = mutable.ArrayBuffer(start)
    val nextNeighbor = mutable.ArrayBuffer(0)
    visited(start) = true
    while vertices.nonEmpty do
      val frame = vertices.length - 1
      val vertex = vertices(frame)
      val nextIndex = nextNeighbor(frame)
      if nextIndex < neighbors(vertex).length then
        val next = neighbors(vertex)(nextIndex)
        nextNeighbor(frame) = nextIndex + 1
        if !visited(next) then
          visited(next) = true
          vertices += next
          nextNeighbor += 0
      else
        result += vertex
        vertices.remove(frame)
        nextNeighbor.remove(frame)

  private def collectReachable(
      start: Int,
      neighbors: Vector[Vector[Int]],
      visited: Array[Boolean]
  ): Vector[Int] =
    val result = Vector.newBuilder[Int]
    val stack = mutable.ArrayDeque(start)
    visited(start) = true
    while stack.nonEmpty do
      val vertex = stack.removeLast()
      result += vertex
      val row = neighbors(vertex)
      var i = row.length - 1
      while i >= 0 do
        val next = row(i)
        if !visited(next) then
          visited(next) = true
          stack.append(next)
        i -= 1
    result.result()

  private def findCycleFrom(
      start: Int,
      neighbors: Vector[Vector[Int]],
      color: Array[Int],
      parent: Array[Int]
  ): Option[Vector[Int]] =
    val vertices = mutable.ArrayBuffer(start)
    val nextNeighbor = mutable.ArrayBuffer(0)
    color(start) = 1
    while vertices.nonEmpty do
      val frame = vertices.length - 1
      val vertex = vertices(frame)
      val nextIndex = nextNeighbor(frame)
      if nextIndex < neighbors(vertex).length then
        val next = neighbors(vertex)(nextIndex)
        nextNeighbor(frame) = nextIndex + 1
        if color(next) == 0 then
          parent(next) = vertex
          color(next) = 1
          vertices += next
          nextNeighbor += 0
        else if color(next) == 1 then
          val reverse = Vector.newBuilder[Int]
          var current = vertex
          reverse += current
          while current != next do
            current = parent(current)
            reverse += current
          return Some(reverse.result().reverse :+ next)
      else
        color(vertex) = 2
        vertices.remove(frame)
        nextNeighbor.remove(frame)
    None
