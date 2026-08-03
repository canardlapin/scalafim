package scalafim.surface

import cats.{Hash, Order}
import graph4s.{Graph, Link}
import graph4s.algorithms.GraphAlgorithms
import graph4s.data.{EdgeField, WeightedGraph}
import graph4s.indexed.{IndexedGraph, VertexOrder}
import scalafim.surface.fixtures.SurfaceTestFixtures

class GraphDifferentialSuite extends munit.FunSuite:
  private given Hash[VertexId] =
    Hash.by(_.index)

  private given Order[VertexId] =
    Order.by(_.index)

  test("thresholded surface components match induced generic graph components"):
    val geometry = SurfaceTestFixtures.disconnectedGeometry
    val topology = SurfaceTestFixtures.disconnectedTopology
    val values = Vector(2.0, 2.2, 0.0, -2.0, -2.1, -2.2)
    val threshold = SurfaceThreshold(-1.0, 1.0)
    val field = SurfaceField.full(geometry, values, "differential")
    val domain = SurfaceComponents.connectedComponents(field, threshold)
    val active = values.indices.collect { case index if threshold.keep(values(index)) => VertexId(index) }.toVector
    val graph = topologyGraph(topology, topology.edgeLengths)
      .topology
      .induced(active)
      .graph

    val genericComponents =
      GraphAlgorithms
        .connectedComponents(graph)
        .components
        .map(_.iterator.map(_.index).toSet)
        .toSet
    val domainComponents = active
      .groupBy(vertex => domain.index.valueAt(vertex).get)
      .values
      .map(_.map(_.index).toSet)
      .toSet

    assertEquals(genericComponents, domainComponents)

  test("surface geodesics match graph shortest paths for default and custom weights"):
    val topology = SurfaceTestFixtures.tetraTopology
    val weightSets = Vector(
      topology.edgeLengths,
      Vector.tabulate(topology.edgeCount)(index => if index == 0 then 5.0 else 1.0 + index.toDouble / 10.0)
    )

    weightSets.foreach: weights =>
      val graph =
        if weights == topology.edgeLengths then topology.toGraph
        else topologyGraph(topology, weights)
      val vertices = Vector.tabulate(topology.mesh.vertexCount)(VertexId.apply)
      val domain = SurfaceGeodesics.distanceMatrix(
        topology,
        vertices,
        vertices,
        edgeWeights = Some(weights)
      )

      vertices.indices.foreach: row =>
        vertices.indices.foreach: col =>
          val generic = shortestDistance(graph, vertices(row), vertices(col)).get
          assertEqualsDouble(generic, domain(row, col), 1e-12)

  test("MeshTopology graph interop preserves explicit indexing, edges, and distances"):
    val topology = SurfaceTestFixtures.tetraTopology
    val graph = topology.toGraph
    val vertices = Vector.tabulate(topology.mesh.vertexCount)(VertexId.apply)
    val indexed =
      IndexedGraph
        .from(graph.topology, VertexOrder.explicit(vertices))
        .toOption
        .get
    val indexedEdges =
      indexed.edges.map: edge =>
        val (left, right) = indexed.endpoints(edge)
        indexed.ordinal(left) -> indexed.ordinal(right)
    .toVector

    assertEquals(indexed.vertices.map(indexed.label).map(_.index).toVector, Vector(0, 1, 2, 3))
    assertEquals(
      indexedEdges,
      topology.edges.map(edge => (edge.a.index, edge.b.index))
    )
    assertEquals(
      topology.edges.map(edge => graph.weights.get(edge.a, edge.b).get),
      topology.edgeLengths
    )

  test("unreachable surface geodesics correspond to graph NoPath"):
    val topology = SurfaceTestFixtures.disconnectedTopology
    val graph = topologyGraph(topology, topology.edgeLengths)
    val domain = SurfaceGeodesics.distanceMatrix(
      topology,
      Vector(VertexId(0)),
      Vector(VertexId(3))
    )

    assert(domain(0, 0).isPosInfinity)
    assertEquals(shortestDistance(graph, VertexId(0), VertexId(3)), None)

  private def topologyGraph(
      topology: MeshTopology,
      weights: Vector[Double]
  ) =
    val vertices = Vector.tabulate(topology.mesh.vertexCount)(VertexId.apply)
    val graph =
      Graph
        .of(vertices, topology.edges.map(edge => Link(edge.a, edge.b)))
        .toOption
        .get
    val byEndpoints =
      topology.edges.zip(weights).map: (edge, weight) =>
        (edge.a.index, edge.b.index) -> weight
      .toMap
    WeightedGraph.from:
      EdgeField.total(graph): edge =>
        val key =
          if edge.first.index < edge.second.index then
            edge.first.index -> edge.second.index
          else
            edge.second.index -> edge.first.index
        byEndpoints(key)

  private def shortestDistance(
      graph: WeightedGraph[VertexId, Double],
      source: VertexId,
      target: VertexId
  ): Option[Double] =
    if !graph.topology.containsVertex(source) || !graph.topology.containsVertex(target) then
      None
    else
      val distances =
        scala.collection.mutable.Map.empty[VertexId, Double].withDefaultValue(Double.PositiveInfinity)
      val remaining =
        scala.collection.mutable.Set.from(graph.topology.vertices.iterator)
      distances(source) = 0.0

      while remaining.nonEmpty do
        val current =
          remaining.minBy(vertex => (distances(vertex), vertex.index))
        val currentDistance = distances(current)
        remaining -= current
        if current == target then
          return Option.when(currentDistance.isFinite)(currentDistance)
        if currentDistance.isFinite then
          graph.topology.neighborsOf(current).foreach: neighbors =>
            neighbors.iterator.foreach: neighbor =>
              if remaining(neighbor) then
                val candidate =
                  currentDistance + graph.weights.get(current, neighbor).get
                if candidate < distances(neighbor) then
                  distances(neighbor) = candidate

      None
