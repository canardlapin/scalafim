package scalafim.surface

import cats.{Hash, Order}
import graph4s.indexed.{IndexedGraph, VertexOrder}

import scala.util.Random

class Mesh4sIntegrationLawSuite extends munit.FunSuite:
  private given Hash[VertexId] = Hash.by(_.index)
  private given Order[VertexId] = Order.by(_.index)

  test("generated coordinate realizations retain one exact mesh4s topology owner"):
    val random = new Random(0x4d455348L)
    var trial = 0
    while trial < 24 do
      val grid = gridFixture(2 + random.nextInt(7), 2 + random.nextInt(7), random)
      val canonical = TriangleMesh.fromArrays(grid.coordinates, grid.faces)
      val movedCoordinates = grid.coordinates.clone()
      var offset = 0
      while offset < movedCoordinates.length do
        movedCoordinates(offset) = movedCoordinates(offset) * (0.5 + random.nextDouble()) + random.nextDouble()
        movedCoordinates(offset + 1) += random.nextDouble() - 0.5
        movedCoordinates(offset + 2) += random.nextDouble() - 0.5
        offset += 3
      val moved = canonical.withCoordinatesEither(movedCoordinates).toOption.get
      assert(moved.topology eq canonical.topology)
      assert(moved.realization.topology eq canonical.topology)
      assertEquals(moved.topologyIdentity, canonical.topologyIdentity)
      assert(moved.faceIndices.sameElements(canonical.faceIndices))

      val independent = TriangleMesh.fromArrays(movedCoordinates, grid.faces)
      assert(independent.hasSameTopology(canonical))
      assert(!(independent.topology eq canonical.topology))
      val rebound = independent.shareTopologyFrom(canonical).toOption.get
      assert(rebound.topology eq canonical.topology)
      assert(rebound.realization.topology eq canonical.topology)
      trial += 1

  test("equal-connectivity meshes still reject foreign edge-domain owners"):
    val random = new Random(0x4f574e4552L)
    var trial = 0
    while trial < 16 do
      val grid = gridFixture(3 + random.nextInt(5), 3 + random.nextInt(5), random)
      val left = MeshTopology.from(TriangleMesh.fromArrays(grid.coordinates, grid.faces))
      val right = MeshTopology.from(TriangleMesh.fromArrays(grid.coordinates, grid.faces))
      val weights = SurfaceEdgeWeights
        .fromTopologyOrder(left, Vector.fill(left.edgeCount)(1.0))
        .toOption
        .get
      assertEquals(left.mesh.connectivityFingerprint, right.mesh.connectivityFingerprint)
      assert(weights.belongsTo(left))
      assert(!weights.belongsTo(right))
      SurfaceEdgeWeights.validateOwner(right, weights) match
        case Left(SurfaceEdgeWeightError.WrongTopologyOwner(_, _)) => ()
        case other => fail(s"expected exact-owner rejection, found $other")
      trial += 1

  test("legacy ordered-connectivity weights are permuted by endpoints, never position"):
    val random = new Random(0x4544474553L)
    var trial = 0
    while trial < 20 do
      val grid = gridFixture(2 + random.nextInt(8), 2 + random.nextInt(8), random)
      val topology = MeshTopology.from(TriangleMesh.fromArrays(grid.coordinates, grid.faces))
      def endpointWeight(first: Int, second: Int): Double = first.toDouble * 10000.0 + second.toDouble
      val legacy = topology.edges.map(edge => endpointWeight(edge.a.index, edge.b.index))
      val weights = SurfaceEdgeWeights.fromLegacyLexicographic(topology, legacy).toOption.get
      var ordinal = 0
      while ordinal < topology.mesh.topology.edges.size do
        val edge = topology.mesh.topology.edges.indexAtValidatedOrdinal(ordinal)
        val endpoints = topology.mesh.topology.endpointsOf(edge)
        assertEqualsDouble(
          weights.valueAtOrdinal(ordinal),
          endpointWeight(endpoints.first.ordinal, endpoints.second.ordinal),
          0.0
        )
        ordinal += 1
      trial += 1

  test("generated mesh4s primal projections preserve every vertex, edge, and Euclidean weight"):
    val random = new Random(0x4752415048L)
    var trial = 0
    while trial < 16 do
      val grid = gridFixture(2 + random.nextInt(7), 2 + random.nextInt(7), random)
      val topology = MeshTopology.from(TriangleMesh.fromArrays(grid.coordinates, grid.faces))
      val graph = topology.toGraph
      val vertices = Vector.tabulate(topology.mesh.vertexCount)(VertexId.apply)
      val indexed = IndexedGraph.from(graph.topology, VertexOrder.explicit(vertices)).toOption.get
      val projectedEdges = indexed.edges.iterator.map: edge =>
        val (first, second) = indexed.endpoints(edge)
        val a = indexed.ordinal(first)
        val b = indexed.ordinal(second)
        if a < b then a -> b else b -> a
      .toSet
      val meshEdges = topology.edges.iterator.map(edge => edge.a.index -> edge.b.index).toSet
      assertEquals(indexed.vertices.map(indexed.label).map(_.index).toVector, vertices.map(_.index))
      assertEquals(projectedEdges, meshEdges)
      topology.edges.foreach: edge =>
        assertEqualsDouble(graph.weights.get(edge.a, edge.b).get, topology.edgeLength(edge), 1e-12)
      trial += 1

  private final case class GridFixture(coordinates: Array[Double], faces: Array[Int])

  private def gridFixture(columns: Int, rows: Int, random: Random): GridFixture =
    val coordinates = new Array[Double](columns * rows * 3)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        val vertex = row * columns + column
        val offset = vertex * 3
        coordinates(offset) = column.toDouble
        coordinates(offset + 1) = row.toDouble
        coordinates(offset + 2) = random.nextDouble() * 0.25
        column += 1
      row += 1
    val faces = new Array[Int]((columns - 1) * (rows - 1) * 6)
    var offset = 0
    row = 0
    while row < rows - 1 do
      var column = 0
      while column < columns - 1 do
        val lowerLeft = row * columns + column
        val lowerRight = lowerLeft + 1
        val upperLeft = lowerLeft + columns
        val upperRight = upperLeft + 1
        faces(offset) = lowerLeft
        faces(offset + 1) = lowerRight
        faces(offset + 2) = upperLeft
        faces(offset + 3) = lowerRight
        faces(offset + 4) = upperRight
        faces(offset + 5) = upperLeft
        offset += 6
        column += 1
      row += 1
    GridFixture(coordinates, faces)
