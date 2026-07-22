package scalafim.surface.view

import scalafim.graphics.*
import scalafim.image.*
import scalafim.surface.*

class SurfaceProjectionNetworkSuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")

  private def geometry(
    z: Double,
    kind: SurfaceKind,
    transform: DMat = DMat.eye(4)
  ): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, z),
          Vector(1.0, 0.0, z),
          Vector(0.0, 1.0, z),
          Vector(0.0, 0.0, z + 0.5)
        ),
        Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
      ),
      Hemisphere.Left,
      kind,
      transform
    )

  private val translation = DMat.fromRows(Vector(
    Vector(1.0, 0.0, 0.0, 1.0),
    Vector(0.0, 1.0, 0.0, 0.0),
    Vector(0.0, 0.0, 1.0, 0.0),
    Vector(0.0, 0.0, 0.0, 1.0)
  ))

  private val white = geometry(0.0, SurfaceKind.White, translation)
  private val pial = geometry(2.0, SurfaceKind.Pial, translation)
  private val pair = SurfaceGeometryPair(white, pial)
  private val volumeSpace = NeuroSpace(Vector(5, 5, 5))
  private val volume = NeuroVol.fromLinear(
    NArrayUtil.tabulate[Double](125): index =>
      val grid = volumeSpace.indexToGrid3D(index)
      grid(0).toDouble + 10.0 * grid(1).toDouble + 100.0 * grid(2).toDouble,
    volumeSpace,
    "asymmetric-volume"
  )

  private def morphism(path: SurfaceSamplingPath, reducer: SurfaceSampleAggregation): VolToSurfMorphism =
    VolToSurfMorphism(
      SpatialDomainId("volume"),
      SpatialDomainId("left-surface"),
      VolumeSurfaceSamplingPlan(pair, path, reducer)
    )

  test("CPU ribbon projection is the typed oracle and records exact work"):
    val projection = SurfaceVolumeProjection.materialize(
      morphism(
        SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.5, 1.0)),
        SurfaceSampleAggregation.Average
      ),
      volume,
      SurfaceProjectionPolicy(SurfaceMinimumSamples.unsafe(3))
    )
    assertEquals(projection.receipt.vertices, 4)
    assertEquals(projection.receipt.requestedSamples, 12L)
    assertEquals(projection.receipt.acceptedSamples, 12L)
    assertEquals(projection.receipt.rejectedSamples, 0L)
    assertEquals(projection.receipt.qualifiedVertices, 4)
    assertEquals(projection.receipt.sourceVolumeValues, 125L)
    assert(projection.receipt.elapsedNanos >= 0L)
    assertEqualsDouble(projection.values.valueAt(VertexId(0)).get, 101.0, 1e-12)
    assertEqualsDouble(projection.values.valueAt(VertexId(1)).get, 102.0, 1e-12)
    assertEqualsDouble(projection.values.valueAt(VertexId(2)).get, 111.0, 1e-12)

  test("projection quality and fill policy preserve rejected samples explicitly"):
    val emptyMask = NeuroVol.fromLinear(
      NArrayUtil.fillConst[Boolean](125, false),
      volumeSpace,
      "empty"
    )
    val projection = SurfaceVolumeProjection.materialize(
      morphism(SurfaceSamplingPath.Midpoint, SurfaceSampleAggregation.Nearest),
      volume,
      SurfaceProjectionPolicy(
        SurfaceMinimumSamples.unsafe(1),
        SurfaceProjectionFill.constant(-99.0).toOption.get
      ),
      Some(emptyMask)
    )
    assertEquals(projection.receipt.acceptedSamples, 0L)
    assertEquals(projection.receipt.rejectedSamples, 4L)
    assertEquals(projection.receipt.qualifiedVertices, 0)
    assertEquals(projection.values.valueAt(VertexId(0)), Some(-99.0))
    assertEquals(projection.quality.valueAt(VertexId(0)), Some(false))

  test("materialized projection is an ordinary layer on topology-compatible display geometry"):
    val projection = SurfaceVolumeProjection.materialize(
      morphism(SurfaceSamplingPath.Midpoint, SurfaceSampleAggregation.Nearest),
      volume
    )
    val inflated = geometry(0.0, SurfaceKind.Inflated, translation)
    val layer = SurfaceVolumeProjection.scalarLayer(
      projection,
      SurfaceLayerId.unsafe("volume-overlay"),
      surfaceId,
      inflated,
      ScalarColorizer(DisplayWindow.unsafe(0.0, 500.0))
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, inflated).toOption.get),
      Vector(layer)
    ).toOption.get
    val plan = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    assertEquals(plan.layers.length, 1)
    assertEquals(plan.layers.head.colors.length, inflated.vertexCount)
    val rewound = SurfaceGeometry(
      TriangleMesh.fromRows(inflated.mesh.vertices.map(p => Vector(p.x, p.y, p.z)), Vector(
        (0, 2, 1), (0, 1, 3), (0, 2, 3), (1, 2, 3)
      )),
      Hemisphere.Left,
      SurfaceKind.Inflated,
      translation
    )
    assert(SurfaceVolumeProjection.scalarLayer(
      projection,
      SurfaceLayerId.unsafe("bad"),
      surfaceId,
      rewound,
      ScalarColorizer(DisplayWindow.unsafe(0.0, 1.0))
    ).isLeft)

  private def node(id: String, vertex: Int, region: String): SurfaceNetworkNode =
    SurfaceNetworkNode.onSurface(
      SurfaceNetworkNodeId.unsafe(id),
      surfaceId,
      VertexId(vertex),
      white,
      Some(region)
    ).toOption.get

  private val nodes = Vector(
    node("a", 0, "frontal"),
    node("b", 1, "frontal"),
    node("c", 2, "temporal"),
    node("d", 3, "parietal")
  )

  private val network = SurfaceNetwork.make(
    nodes,
    Vector(
      SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 0.8),
      SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(2).id, -0.9),
      SurfaceNetworkEdge.unsafe(nodes(1).id, nodes(3).id, 0.2),
      SurfaceNetworkEdge.unsafe(nodes(2).id, nodes(3).id, -0.4)
    )
  ).toOption.get

  test("network filters preserve node identity, negative weights, regions, and stable top-N order"):
    val filter = SurfaceNetworkFilter.make(
      threshold = SurfaceNetworkThreshold.unsafe(0.3),
      topN = Some(SurfaceNetworkTopN.unsafe(2)),
      regions = Set("frontal", "temporal"),
      regionMatch = SurfaceRegionMatch.AnyEndpoint
    ).toOption.get
    val display = SurfaceNetworkDisplay.compile(
      network,
      filter,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.025), sides = 6).toOption.get
    ).toOption.get
    assertEquals(display.edges.map(edge => (edge.source.id.value, edge.target.id.value, edge.weight)), Vector(
      ("a", "c", -0.9),
      ("a", "b", 0.8)
    ))
    assertEquals(display.edges.head.color, SurfaceNetworkPalette().negative)
    assertEquals(display.receipt.negativeEdges, 1)
    assertEquals(display.receipt.positiveEdges, 1)
    assertEquals(display.receipt.generatedVertices, 24)
    assertEquals(display.receipt.generatedTriangles, 24)

    val negativeOnly = SurfaceNetworkDisplay.compile(
      network,
      SurfaceNetworkFilter.make(sign = SurfaceNetworkSign.Negative).toOption.get,
      SurfaceNetworkStyle.line(SurfaceNetworkRadius.unsafe(0.01))
    ).toOption.get
    assertEquals(negativeOnly.edges.map(_.weight), Vector(-0.9, -0.4))

  test("network tubes attach as ordinary mesh and color resources for every backend"):
    val underlay = SurfaceLayer.packedRgba(
      SurfaceLayerId.unsafe("underlay"),
      surfaceId,
      white,
      Vector.fill(white.vertexCount)(Rgba32.unsafe(160, 160, 160))
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, white).toOption.get),
      Vector(underlay)
    ).toOption.get
    val base = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    val display = SurfaceNetworkDisplay.compile(
      network,
      SurfaceNetworkFilter.make(topN = Some(SurfaceNetworkTopN.unsafe(2))).toOption.get,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.02), sides = 8).toOption.get
    ).toOption.get
    val attached = SurfaceNetworkCompiler.attach(
      base,
      surfaceId,
      SurfaceLayerId.unsafe("connectivity"),
      display
    ).toOption.get
    assertEquals(attached.plan.meshes.length, 1)
    assertEquals(attached.plan.layers.length, 2)
    assertEquals(attached.plan.drawPasses.length, 2)
    assertEquals(attached.plan.meshes.head.positions.length / 3, white.vertexCount + 32)
    assertEquals(attached.plan.meshes.head.indices.length / 3, white.faceCount + 32)
    assert(attached.plan.layers.forall(_.colors.length == white.vertexCount + 32))
    assertNotEquals(attached.plan.receipt.meshKeys, base.receipt.meshKeys)
    assert(attached.plan.profile.primitiveBytes > base.profile.primitiveBytes)

  test("large signed network compilation reports bounded primitive work and memory"):
    val nodeCount = 256
    val edgeCount = 20000
    val manyNodes = Vector.tabulate(nodeCount): index =>
      node(s"large-$index", index % white.vertexCount, if index % 2 == 0 then "even" else "odd")
    val manyEdges = Vector.tabulate(edgeCount): index =>
      val source = index % nodeCount
      val target = (source + 1) % nodeCount
      val magnitude = 0.1 + (index % 100).toDouble / 100.0
      SurfaceNetworkEdge.unsafe(
        manyNodes(source).id,
        manyNodes(target).id,
        if index % 2 == 0 then magnitude else -magnitude
      )
    val large = SurfaceNetwork.make(manyNodes, manyEdges).toOption.get
    val style = SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.01), sides = 8).toOption.get
    val compiled = SurfaceNetworkDisplay.compile(large, SurfaceNetworkFilter.All, style).toOption.get
    assertEquals(compiled.receipt.inputNodes, nodeCount)
    assertEquals(compiled.receipt.inputEdges, edgeCount)
    assertEquals(compiled.receipt.retainedEdges, edgeCount)
    assertEquals(compiled.receipt.positiveEdges, edgeCount / 2)
    assertEquals(compiled.receipt.negativeEdges, edgeCount / 2)
    assertEquals(compiled.receipt.generatedVertices, edgeCount * 16)
    assertEquals(compiled.receipt.generatedTriangles, edgeCount * 16)
    assertEquals(compiled.receipt.primitiveBytes, edgeCount.toLong * 16L * 40L)
    assert(compiled.receipt.elapsedNanos >= 0L)
