package scalafim.surface

import cats.Hash
import graph4s.{Graph, Link}
import graph4s.data.{EdgeField, WeightedGraph}
import scala.util.control.NonFatal

final case class Edge private (a: VertexId, b: VertexId):
  def vertices: (VertexId, VertexId) =
    (a, b)

object Edge:
  def between(a: VertexId, b: VertexId): Edge =
    require(a != b, "edge endpoints must be distinct")
    if a.index < b.index then Edge(a, b) else Edge(b, a)

final case class MeshTopology private (
  mesh: TriangleMesh,
  domainOption: Option[SurfaceDomain] = None
):
  domainOption.foreach(domain => require(domain.vertexCount == mesh.vertexCount, "topology domain vertex count must match mesh"))

  /** Edge ordinals are mesh4s edge ordinals, derived without another adjacency owner. */
  lazy val edges: Vector[Edge] =
    val owner = mesh.topology
    owner.edges.indices.map: index =>
      val endpoints = owner.endpointsOf(index)
      Edge.between(VertexId.unsafe(endpoints.first.ordinal), VertexId.unsafe(endpoints.second.ordinal))
    .toVector

  lazy val edgeLengths: Vector[Double] = edges.map(edgeLength)

  def edgeCount: Int =
    edges.length

  /** Materialize a reusable graph value for interop and downstream graph
    * algorithms. The result is deliberately not retained by `MeshTopology`,
    * so the mesh never owns two persistent full topology representations.
    */
  def toGraph: WeightedGraph[VertexId, Double] =
    given Hash[VertexId] =
      Hash.by(_.index)
    val vertices = Vector.tabulate(mesh.vertexCount)(VertexId.apply)
    val links = edges.map(edge => Link(edge.a, edge.b))
    Graph.of(vertices, links).toEither match
      case Right(topology) =>
        val lengthsByEndpoints =
          edges.zip(edgeLengths).map: (edge, length) =>
            (edge.a.index, edge.b.index) -> length
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
    val i = vertex.index
    require(i < mesh.vertexCount, "vertex id out of range")
    val owner = mesh.topology
    owner.neighbors(owner.vertices.indexAtValidatedOrdinal(i)).ordinalsInDomainOrder.toVector.map(VertexId.unsafe)

  def vertexDegree(vertex: VertexId): Int =
    neighborsOf(vertex).length

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    domainOption.toRight(SurfaceError.MissingSurfaceDomain("mesh topology"))

  def withDomain(domain: SurfaceDomain): MeshTopology =
    MeshTopology(mesh, Some(domain))

  def edgeLength(edge: Edge): Double =
    val a = mesh.vertex(edge.a)
    val b = mesh.vertex(edge.b)
    (a - b).norm

  def faceArea(face: FaceId): Double =
    val tri = mesh.face(face)
    val a = mesh.vertex(tri.a)
    val b = mesh.vertex(tri.b)
    val c = mesh.vertex(tri.c)
    ((b - a).cross(c - a)).norm / 2.0

  def faceAreas: Vector[Double] =
    Vector.tabulate(mesh.faceCount)(i => faceArea(FaceId.unsafe(i)))

  def surfaceArea: Double =
    faceAreas.sum

  def eulerCharacteristic: Int =
    mesh.vertexCount - edgeCount + mesh.faceCount

  def vertexNormals: Vector[Point3D] =
    val sums = Array.fill(mesh.vertexCount)(Point3D.Zero)

    var i = 0
    while i < mesh.faceCount do
      val tri = mesh.face(FaceId.unsafe(i))
      val a = mesh.vertex(tri.a)
      val b = mesh.vertex(tri.b)
      val c = mesh.vertex(tri.c)
      val normal = (b - a).cross(c - a)
      val ia = tri.a.index
      val ib = tri.b.index
      val ic = tri.c.index
      sums(ia) = sums(ia) + normal
      sums(ib) = sums(ib) + normal
      sums(ic) = sums(ic) + normal
      i += 1

    sums.toVector.map(_.normalized)

object MeshTopology:

  def from(mesh: TriangleMesh): MeshTopology =
    from(mesh, None)

  def from(geometry: SurfaceGeometry): MeshTopology =
    from(geometry.mesh, geometry.domainEither.toOption)

  def fromEither(mesh: TriangleMesh): Either[SurfaceError, MeshTopology] =
    try scala.util.Right(from(mesh))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidTopology(SurfaceError.reason(error)))

  def fromEither(geometry: SurfaceGeometry): Either[SurfaceError, MeshTopology] =
    try scala.util.Right(from(geometry))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidTopology(SurfaceError.reason(error)))

  private def from(mesh: TriangleMesh, domain: Option[SurfaceDomain]): MeshTopology =
    MeshTopology(mesh, domain)
