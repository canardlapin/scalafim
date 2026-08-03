package scalafim.surface.view.connectivity

import scalafim.connectivity.*
import scalafim.surface.*
import scalafim.surface.view.*

enum ConnectivitySurfaceNetworkError:
  case Connectivity(cause: ConnectivityError)
  case MissingPlacement(node: NodeId)
  case InvalidPlacement(node: NodeId, cause: SurfaceViewError)
  case InvalidNetwork(cause: SurfaceViewError)

  def message: String =
    this match
      case Connectivity(cause) => cause.message
      case MissingPlacement(node) => s"connectivity node '${node.value}' has no surface placement"
      case InvalidPlacement(node, cause) => s"connectivity node '${node.value}' has invalid surface placement: ${cause.message}"
      case InvalidNetwork(cause) => cause.message

final case class ConnectivityNodePlacement(
  surface: SurfaceId,
  vertex: VertexId,
  geometry: SurfaceGeometry,
  region: Option[String] = None
)

object ConnectivitySurfaceNetwork:
  def fromStatic(
    connectivity: StaticConnectivity,
    placements: Map[NodeId, ConnectivityNodePlacement]
  ): Either[ConnectivitySurfaceNetworkError, SurfaceNetwork] =
    connectivity.matrix.edgeVector
      .left.map(ConnectivitySurfaceNetworkError.Connectivity.apply)
      .flatMap(fromEdgeVector(_, placements))

  def fromEdgeVector(
    vector: EdgeVector,
    placements: Map[NodeId, ConnectivityNodePlacement]
  ): Either[ConnectivitySurfaceNetworkError, SurfaceNetwork] =
    val orderedIds = vector.space.edges.iterator
      .flatMap(edge => Iterator(edge.source, edge.target))
      .toVector
      .distinct
    val nodes = Vector.newBuilder[SurfaceNetworkNode]
    var index = 0
    while index < orderedIds.length do
      val id = orderedIds(index)
      placements.get(id) match
        case None => return Left(ConnectivitySurfaceNetworkError.MissingPlacement(id))
        case Some(placement) =>
          SurfaceNetworkNode.onSurface(
            SurfaceNetworkNodeId.unsafe(id.value),
            placement.surface,
            placement.vertex,
            placement.geometry,
            placement.region
          ) match
            case Left(error) => return Left(ConnectivitySurfaceNetworkError.InvalidPlacement(id, error))
            case Right(node) => nodes += node
      index += 1
    val edges = Vector.newBuilder[SurfaceNetworkEdge]
    index = 0
    while index < vector.length do
      val edge = vector.space.edge(index)
      SurfaceNetworkEdge.make(
        SurfaceNetworkNodeId.unsafe(edge.source.value),
        SurfaceNetworkNodeId.unsafe(edge.target.value),
        vector(index)
      ) match
        case Left(error) => return Left(ConnectivitySurfaceNetworkError.InvalidNetwork(error))
        case Right(displayEdge) => edges += displayEdge
      index += 1
    SurfaceNetwork.make(nodes.result(), edges.result())
      .left.map(ConnectivitySurfaceNetworkError.InvalidNetwork.apply)
