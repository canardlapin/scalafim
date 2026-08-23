package scalafim.surface.view

import intaglio.*
import scalafim.image.*
import scalafim.surface.*

/** Small asymmetric integration fixture shared by backend conformance tests.
  * It combines a CPU volume projection with signed network tubes while keeping
  * estimator logic outside the renderer.
  */
final case class SurfaceProjectionFixtureCase(
  morphism: VolToSurfMorphism,
  volume: NeuroVol[Double],
  policy: SurfaceProjectionPolicy
)

object SurfaceFeatureFixture:
  val Surface: SurfaceId = SurfaceId.unsafe("advanced-left")

  def projectionCase: SurfaceProjectionFixtureCase =
    val white = geometry(0.0, SurfaceKind.White)
    val pial = geometry(2.0, SurfaceKind.Pial)
    val space = NeuroSpace(Vector(5, 5, 5))
    val volume = NeuroVol.copyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](125): index =>
        val grid = space.indexToGrid3D(index)
        grid(0).toDouble + 10.0 * grid(1).toDouble + 100.0 * grid(2).toDouble,
      space,
      "advanced-volume"
    )
    SurfaceProjectionFixtureCase(
      VolToSurfMorphism(
        SpatialDomainId("advanced-volume"),
        SpatialDomainId("advanced-surface"),
        VolumeSurfaceSamplingPlan(
          SurfaceGeometryPair(white, pial),
          SurfaceSamplingPath.Midpoint,
          SurfaceSampleAggregation.Nearest
        )
      ),
      volume,
      SurfaceProjectionPolicy()
    )

  def plan: SurfaceRenderPlan =
    val projectionFixture = projectionCase
    val display = geometry(0.0, SurfaceKind.Inflated)
    val projection = SurfaceVolumeProjection.materialize(
      projectionFixture.morphism,
      projectionFixture.volume,
      projectionFixture.policy
    )
    val overlay = SurfaceVolumeProjection.scalarLayer(
      projection,
      SurfaceLayerId.unsafe("volume-overlay"),
      Surface,
      display,
      ScalarColorizer(DisplayWindow.unsafe(0.0, 500.0), ColorRamp.Heat)
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(Surface, display).toOption.get),
      Vector(overlay)
    ).toOption.get
    val base = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    val nodes = Vector(
      node("a", 0, "front", display),
      node("b", 1, "front", display),
      node("c", 2, "back", display),
      node("d", 3, "back", display)
    )
    val network = SurfaceNetwork.make(
      nodes,
      Vector(
        SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 0.8),
        SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(2).id, -0.9)
      )
    ).toOption.get
    val networkDisplay = SurfaceNetworkDisplay.compile(
      network,
      SurfaceNetworkFilter.All,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.025), sides = 6).toOption.get
    ).toOption.get
    SurfaceNetworkCompiler.attach(
      base,
      Surface,
      SurfaceLayerId.unsafe("network"),
      networkDisplay
    ).toOption.get.plan

  private def geometry(z: Double, kind: SurfaceKind): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(1.0, 1.0, z),
          Vector(2.0, 1.0, z),
          Vector(1.0, 2.0, z),
          Vector(1.0, 1.0, z + 0.5)
        ),
        Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
      ),
      Hemisphere.Left,
      kind
    )

  private def node(id: String, vertex: Int, region: String, geometry: SurfaceGeometry): SurfaceNetworkNode =
    SurfaceNetworkNode.onSurface(
      SurfaceNetworkNodeId.unsafe(id),
      Surface,
      VertexId(vertex),
      geometry,
      Some(region)
    ).toOption.get
