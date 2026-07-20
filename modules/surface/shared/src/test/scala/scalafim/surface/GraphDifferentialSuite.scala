package scalafim.surface

import scalafim.graph.Distance as GraphDistance
import scalafim.graph.Graph
import scalafim.graph.VertexBasis
import scalafim.graph.connectedComponents
import scalafim.graph.shortestPath
import scalafim.surface.fixtures.SurfaceTestFixtures

class GraphDifferentialSuite extends munit.FunSuite:
  test("thresholded surface components match induced generic graph components"):
    val geometry = SurfaceTestFixtures.disconnectedGeometry
    val topology = SurfaceTestFixtures.disconnectedTopology
    val values = Vector(2.0, 2.2, 0.0, -2.0, -2.1, -2.2)
    val threshold = SurfaceThreshold(-1.0, 1.0)
    val field = SurfaceField.full(geometry, values, "differential")
    val domain = SurfaceComponents.connectedComponents(field, threshold)
    val active = values.indices.collect { case index if threshold.keep(values(index)) => VertexId(index) }.toVector
    val graph = topologyGraph(topology, topology.edgeLengths)
      .inducedByKeys(active)
      .toOption
      .get
      .graph

    val genericComponents = graph.connectedComponents.map(_.keys.map(_.index).toSet).toSet
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
          val generic = graph.shortestPath(vertices(row), vertices(col)).toOption.get.totalCost
          assertEqualsDouble(generic, domain(row, col), 1e-12)

  test("MeshTopology graph interop preserves basis order, points, edges, and distances"):
    val topology = SurfaceTestFixtures.tetraTopology
    val graph = topology.toGraph

    assertEquals(graph.basis.keys.map(_.index), Vector(0, 1, 2, 3))
    assertEquals(graph.basis.values, topology.mesh.vertices)
    assertEquals(
      graph.edges.map(edge => (edge.endpoints.first.toInt, edge.endpoints.second.toInt)),
      topology.edges.map(edge => (edge.a.index, edge.b.index))
    )
    assertEquals(graph.edges.map(_.value.value), topology.edgeLengths)

  test("unreachable surface geodesics correspond to graph NoPath"):
    val topology = SurfaceTestFixtures.disconnectedTopology
    val graph = topologyGraph(topology, topology.edgeLengths)
    val domain = SurfaceGeodesics.distanceMatrix(
      topology,
      Vector(VertexId(0)),
      Vector(VertexId(3))
    )

    assert(domain(0, 0).isPosInfinity)
    assert(graph.shortestPath(VertexId(0), VertexId(3)).isLeft)

  private def topologyGraph(
      topology: MeshTopology,
      weights: Vector[Double]
  ) =
    val vertices = Vector.tabulate(topology.mesh.vertexCount)(VertexId.apply)
    val basis = VertexBasis.from(vertices.map(vertex => vertex -> topology.mesh.vertex(vertex))).toOption.get
    val edges = topology.edges.zip(weights).map: (edge, weight) =>
      (edge.a, edge.b, GraphDistance.unsafe(weight))
    Graph.undirected(basis, edges).toOption.get
