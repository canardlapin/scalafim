package scalafim.surface

import cats.Hash
import graph4s.{Graph, Link}
import graph4s.data.{EdgeField, WeightedGraph}

final case class Edge private (a: VertexId, b: VertexId):
  def vertices: (VertexId, VertexId) =
    (a, b)

object Edge:
  def between(a: VertexId, b: VertexId): Edge =
    require(a != b, "edge endpoints must be distinct")
    if a.index < b.index then Edge(a, b) else Edge(b, a)

/** ScalaFIM compatibility view over the mesh4s topology owned by `mesh`.
  *
  * Edges, neighbor rows, and lengths are derived on demand. The only retained
  * topology representation is `mesh.topology`.
  */
final case class MeshTopology private (
  mesh: TriangleMesh,
  domainOption: Option[SurfaceDomain] = None
):
  domainOption.foreach: domain =>
    require(
      domain.vertexCount == mesh.vertexCount,
      "topology domain vertex count must match mesh"
    )

  def edgeCount: Int =
    mesh.topology.edges.size

  /** Legacy lexicographic edge view. mesh4s remains the edge-domain owner; S5
    * will replace positional weight consumers with a typed edge field.
    */
  def edges: Vector[Edge] =
    val result = Vector.newBuilder[Edge]
    mesh.topology.edges.foreachIndex: meshEdge =>
      val endpoints = mesh.topology.endpointsOf(meshEdge)
      result += Edge.between(
        VertexId.unsafe(endpoints.first.ordinal),
        VertexId.unsafe(endpoints.second.ordinal)
      )
    result.result().sortBy(edge => (edge.a.index, edge.b.index))

  def edgeLengths: Vector[Double] =
    edges.map(edgeLength)

  /** Materialize a reusable graph value for interop and downstream graph
    * algorithms. The result is deliberately not retained by `MeshTopology`,
    * so the mesh never owns two persistent full topology representations.
    */
  def toGraph: WeightedGraph[VertexId, Double] =
    given Hash[VertexId] =
      Hash.by(_.index)
    val compatibilityEdges = edges
    val vertices = Vector.tabulate(mesh.vertexCount)(VertexId.apply)
    val links = compatibilityEdges.map(edge => Link(edge.a, edge.b))
    Graph.of(vertices, links).toEither match
      case Right(topology) =>
        val lengthsByEndpoints =
          compatibilityEdges.iterator.map: edge =>
            (edge.a.index, edge.b.index) -> edgeLength(edge)
        .toMap
        val weights =
          EdgeField.total(topology): edge =>
            val key =
              if edge.first.index < edge.second.index then
                edge.first.index -> edge.second.index
              else
                edge.second.index -> edge.first.index
            lengthsByEndpoints(key)
        WeightedGraph.from(weights)
      case Left(errors) =>
        throw new IllegalStateException(
          s"validated mesh topology violated graph4s invariants: " +
            errors.toNonEmptyList.toList.mkString(", ")
        )

  def neighborsOf(vertex: VertexId): Vector[VertexId] =
    val ordinal = vertex.index
    require(ordinal < mesh.vertexCount, "vertex id out of range")
    val meshVertex = mesh.topology.vertices.indexAtValidatedOrdinal(ordinal)
    val result = Vector.newBuilder[VertexId]
    mesh.topology.foreachNeighbor(meshVertex): neighbor =>
      result += VertexId.unsafe(neighbor.ordinal)
    result.result().sortBy(_.index)

  def vertexDegree(vertex: VertexId): Int =
    val ordinal = vertex.index
    require(ordinal < mesh.vertexCount, "vertex id out of range")
    val meshVertex = mesh.topology.vertices.indexAtValidatedOrdinal(ordinal)
    var degree = 0
    mesh.topology.foreachNeighbor(meshVertex)(_ => degree += 1)
    degree

  def boundaryHalfedgeCount: Int =
    mesh.topology.boundaryHalfedgeCount

  def boundaryLoopCount: Int =
    mesh.topology.boundaryLoops.length

  def componentCount: Int =
    mesh.topology.componentCount

  def isClosed: Boolean =
    mesh.topology.isClosed

  def isConnected: Boolean =
    mesh.topology.isConnected

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    domainOption.toRight(SurfaceError.MissingSurfaceDomain("mesh topology"))

  def withDomain(domain: SurfaceDomain): MeshTopology =
    MeshTopology(mesh, Some(domain))

  def edgeLength(edge: Edge): Double =
    val a = mesh.vertex(edge.a)
    val b = mesh.vertex(edge.b)
    (a - b).norm

  def faceArea(face: FaceId): Double =
    val triangle = mesh.face(face)
    val a = mesh.vertex(triangle.a)
    val b = mesh.vertex(triangle.b)
    val c = mesh.vertex(triangle.c)
    ((b - a).cross(c - a)).norm / 2.0

  def faceAreas: Vector[Double] =
    Vector.tabulate(mesh.faceCount)(i => faceArea(FaceId.unsafe(i)))

  def surfaceArea: Double =
    faceAreas.sum

  def eulerCharacteristic: Int =
    mesh.topology.eulerCharacteristic

  def vertexNormals: Vector[Point3D] =
    val sums = Array.fill(mesh.vertexCount)(Point3D.Zero)

    var i = 0
    while i < mesh.faceCount do
      val triangle = mesh.face(FaceId.unsafe(i))
      val a = mesh.vertex(triangle.a)
      val b = mesh.vertex(triangle.b)
      val c = mesh.vertex(triangle.c)
      val normal = (b - a).cross(c - a)
      val ia = triangle.a.index
      val ib = triangle.b.index
      val ic = triangle.c.index
      sums(ia) = sums(ia) + normal
      sums(ib) = sums(ib) + normal
      sums(ic) = sums(ic) + normal
      i += 1

    sums.toVector.map(_.normalized)

object MeshTopology:

  def from(mesh: TriangleMesh): MeshTopology =
    MeshTopology(mesh)

  def from(geometry: SurfaceGeometry): MeshTopology =
    MeshTopology(geometry.mesh, geometry.domainEither.toOption)

  def fromEither(mesh: TriangleMesh): Either[SurfaceError, MeshTopology] =
    Right(from(mesh))

  def fromEither(geometry: SurfaceGeometry): Either[SurfaceError, MeshTopology] =
    Right(from(geometry))
