package scalafim.surface.view.connectivity

import scalafim.connectivity.*
import scalafim.surface.*
import scalafim.surface.view.*

class ConnectivitySurfaceNetworkSuite extends munit.FunSuite:
  private val axis = NodeAxis.generated(3, "parcel").toOption.get
  private val edgeSpace = EdgeSpace.unsafeUndirected(axis)
  private val values = EdgeVector.from(edgeSpace, Vector(0.8, -0.9, 0.2)).toOption.get
  private val surfaceId = SurfaceId.unsafe("left")
  private val geometry = SurfaceGeometry(
    TriangleMesh.fromRows(
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0)
      ),
      Vector((0, 1, 2))
    ),
    Hemisphere.Left,
    SurfaceKind.Inflated
  )
  private val placements = axis.ids.zipWithIndex.map: (id, index) =>
    id -> ConnectivityNodePlacement(surfaceId, VertexId(index), geometry, Some(if index < 2 then "front" else "back"))
  .toMap

  test("EdgeVector adapter preserves ordered NodeId identity and signed weights"):
    val network = ConnectivitySurfaceNetwork.fromEdgeVector(values, placements).toOption.get
    assertEquals(network.nodes.map(_.id.value), axis.ids.map(_.value))
    assertEquals(network.edges.map(_.weight), Vector(0.8, -0.9, 0.2))
    assertEquals(network.edges.map(edge => (edge.source.value, edge.target.value)), edgeSpace.edges.map: edge =>
      (edge.source.value, edge.target.value)
    )
    val negative = SurfaceNetworkDisplay.compile(
      network,
      SurfaceNetworkFilter.make(sign = SurfaceNetworkSign.Negative).toOption.get,
      SurfaceNetworkStyle.line(SurfaceNetworkRadius.unsafe(0.01))
    ).toOption.get
    assertEquals(negative.edges.map(_.weight), Vector(-0.9))
    assertEquals(negative.edges.head.source.id.value, edgeSpace.edges(1).source.value)
    assertEquals(negative.edges.head.target.id.value, edgeSpace.edges(1).target.value)

  test("missing connectivity placements fail before display planning"):
    assertEquals(
      ConnectivitySurfaceNetwork.fromEdgeVector(values, placements.removed(axis.ids.last)).left.toOption,
      Some(ConnectivitySurfaceNetworkError.MissingPlacement(axis.ids.last))
    )
