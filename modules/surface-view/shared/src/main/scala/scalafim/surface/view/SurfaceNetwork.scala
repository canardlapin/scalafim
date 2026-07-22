package scalafim.surface.view

import scala.util.hashing.MurmurHash3

import scalafim.graphics.*
import scalafim.image.WorldPoint
import scalafim.surface.*

opaque type SurfaceNetworkNodeId = String

object SurfaceNetworkNodeId:
  def make(value: String): Either[SurfaceViewError, SurfaceNetworkNodeId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SurfaceViewError.InvalidNetwork("node id must be non-empty"))
    else Right(normalized)

  def unsafe(value: String): SurfaceNetworkNodeId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SurfaceNetworkNodeId)
    def value: String = id

final case class SurfaceNetworkNode private (
  id: SurfaceNetworkNodeId,
  surface: SurfaceId,
  vertex: VertexId,
  world: WorldPoint,
  region: Option[String]
)

object SurfaceNetworkNode:
  def onSurface(
    id: SurfaceNetworkNodeId,
    surface: SurfaceId,
    vertex: VertexId,
    geometry: SurfaceGeometry,
    region: Option[String] = None
  ): Either[SurfaceViewError, SurfaceNetworkNode] =
    val normalizedRegion = region.map(_.trim).filter(_.nonEmpty)
    SurfaceWorldLink.worldPoint(geometry, vertex).map: world =>
      new SurfaceNetworkNode(id, surface, vertex, world, normalizedRegion)

final case class SurfaceNetworkEdge private (
  source: SurfaceNetworkNodeId,
  target: SurfaceNetworkNodeId,
  weight: Double
)

object SurfaceNetworkEdge:
  def make(
    source: SurfaceNetworkNodeId,
    target: SurfaceNetworkNodeId,
    weight: Double
  ): Either[SurfaceViewError, SurfaceNetworkEdge] =
    if source == target then Left(SurfaceViewError.InvalidNetwork("self edges are not displayable"))
    else if !weight.isFinite then Left(SurfaceViewError.InvalidNetwork(s"edge weight must be finite; got $weight"))
    else Right(new SurfaceNetworkEdge(source, target, weight))

  def unsafe(source: SurfaceNetworkNodeId, target: SurfaceNetworkNodeId, weight: Double): SurfaceNetworkEdge =
    make(source, target, weight).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SurfaceNetwork private (
  nodes: Vector[SurfaceNetworkNode],
  edges: Vector[SurfaceNetworkEdge]
):
  private lazy val byId: Map[SurfaceNetworkNodeId, SurfaceNetworkNode] = nodes.map(node => node.id -> node).toMap
  def node(id: SurfaceNetworkNodeId): Option[SurfaceNetworkNode] = byId.get(id)

object SurfaceNetwork:
  def make(
    nodes: Vector[SurfaceNetworkNode],
    edges: Vector[SurfaceNetworkEdge]
  ): Either[SurfaceViewError, SurfaceNetwork] =
    if nodes.isEmpty then Left(SurfaceViewError.InvalidNetwork("at least one node is required"))
    else
      val ids = nodes.map(_.id)
      if ids.distinct.length != ids.length then Left(SurfaceViewError.InvalidNetwork("node ids must be unique"))
      else
        val known = ids.toSet
        edges.find(edge => !known(edge.source) || !known(edge.target)) match
          case Some(edge) =>
            Left(SurfaceViewError.InvalidNetwork(
              s"edge '${edge.source.value}->${edge.target.value}' references an unknown node"
            ))
          case None => Right(new SurfaceNetwork(nodes, edges))

opaque type SurfaceNetworkThreshold = Double

object SurfaceNetworkThreshold:
  def make(value: Double): Either[SurfaceViewError, SurfaceNetworkThreshold] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidNetwork(s"absolute edge threshold must be finite and non-negative; got $value"))

  def unsafe(value: Double): SurfaceNetworkThreshold =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (threshold: SurfaceNetworkThreshold)
    def value: Double = threshold

opaque type SurfaceNetworkTopN = Int

object SurfaceNetworkTopN:
  def make(value: Int): Either[SurfaceViewError, SurfaceNetworkTopN] =
    if value > 0 then Right(value)
    else Left(SurfaceViewError.InvalidNetwork(s"top-N edge count must be positive; got $value"))

  def unsafe(value: Int): SurfaceNetworkTopN =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (top: SurfaceNetworkTopN)
    def value: Int = top

opaque type SurfaceNetworkRadius = Double

object SurfaceNetworkRadius:
  def make(value: Double): Either[SurfaceViewError, SurfaceNetworkRadius] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidNetwork(s"edge radius must be finite and positive; got $value"))

  def unsafe(value: Double): SurfaceNetworkRadius =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (radius: SurfaceNetworkRadius)
    def value: Double = radius

enum SurfaceNetworkSign:
  case Both, Positive, Negative

enum SurfaceRegionMatch:
  case AnyEndpoint, BothEndpoints

enum SurfaceNetworkShape:
  case Line, Tube

final case class SurfaceNetworkStyle private (
  shape: SurfaceNetworkShape,
  radius: SurfaceNetworkRadius,
  sides: Int
)

object SurfaceNetworkStyle:
  def line(width: SurfaceNetworkRadius): SurfaceNetworkStyle =
    new SurfaceNetworkStyle(SurfaceNetworkShape.Line, width, 4)

  def tube(radius: SurfaceNetworkRadius, sides: Int = 8): Either[SurfaceViewError, SurfaceNetworkStyle] =
    if sides >= 3 then Right(new SurfaceNetworkStyle(SurfaceNetworkShape.Tube, radius, sides))
    else Left(SurfaceViewError.InvalidNetwork(s"tube side count must be at least 3; got $sides"))

final case class SurfaceNetworkPalette(
  positive: Rgba32 = Rgba32.unsafe(230, 72, 62),
  negative: Rgba32 = Rgba32.unsafe(54, 116, 217),
  zero: Rgba32 = Rgba32.unsafe(150, 150, 150)
):
  def color(weight: Double): Rgba32 =
    if weight > 0.0 then positive else if weight < 0.0 then negative else zero

final case class SurfaceNetworkFilter private (
  threshold: SurfaceNetworkThreshold,
  topN: Option[SurfaceNetworkTopN],
  sign: SurfaceNetworkSign,
  regions: Set[String],
  regionMatch: SurfaceRegionMatch
)

object SurfaceNetworkFilter:
  val All: SurfaceNetworkFilter = new SurfaceNetworkFilter(
    SurfaceNetworkThreshold.unsafe(0.0),
    None,
    SurfaceNetworkSign.Both,
    Set.empty,
    SurfaceRegionMatch.AnyEndpoint
  )

  def make(
    threshold: SurfaceNetworkThreshold = SurfaceNetworkThreshold.unsafe(0.0),
    topN: Option[SurfaceNetworkTopN] = None,
    sign: SurfaceNetworkSign = SurfaceNetworkSign.Both,
    regions: Set[String] = Set.empty,
    regionMatch: SurfaceRegionMatch = SurfaceRegionMatch.AnyEndpoint
  ): Either[SurfaceViewError, SurfaceNetworkFilter] =
    val normalized = regions.map(_.trim)
    if normalized.exists(_.isEmpty) then Left(SurfaceViewError.InvalidNetwork("region filters must be non-empty"))
    else Right(new SurfaceNetworkFilter(threshold, topN, sign, normalized, regionMatch))

final case class SurfaceNetworkDisplayEdge(
  source: SurfaceNetworkNode,
  target: SurfaceNetworkNode,
  weight: Double,
  color: Rgba32
)

final case class SurfaceNetworkDisplayReceipt(
  inputNodes: Int,
  inputEdges: Int,
  retainedEdges: Int,
  positiveEdges: Int,
  negativeEdges: Int,
  zeroEdges: Int,
  generatedVertices: Int,
  generatedTriangles: Int,
  primitiveBytes: Long,
  elapsedNanos: Long
)

final case class SurfaceNetworkDisplay(
  edges: Vector[SurfaceNetworkDisplayEdge],
  style: SurfaceNetworkStyle,
  receipt: SurfaceNetworkDisplayReceipt
)

object SurfaceNetworkDisplay:
  def compile(
    network: SurfaceNetwork,
    filter: SurfaceNetworkFilter,
    style: SurfaceNetworkStyle,
    palette: SurfaceNetworkPalette = SurfaceNetworkPalette()
  ): Either[SurfaceViewError, SurfaceNetworkDisplay] =
    val started = System.nanoTime()
    val selected = network.edges.zipWithIndex.flatMap: (edge, inputIndex) =>
      val source = network.node(edge.source).get
      val target = network.node(edge.target).get
      val signAccepted = filter.sign match
        case SurfaceNetworkSign.Both => true
        case SurfaceNetworkSign.Positive => edge.weight > 0.0
        case SurfaceNetworkSign.Negative => edge.weight < 0.0
      val sourceRegion = source.region.exists(filter.regions)
      val targetRegion = target.region.exists(filter.regions)
      val regionAccepted =
        if filter.regions.isEmpty then true
        else filter.regionMatch match
          case SurfaceRegionMatch.AnyEndpoint => sourceRegion || targetRegion
          case SurfaceRegionMatch.BothEndpoints => sourceRegion && targetRegion
      val dx = target.world.x - source.world.x
      val dy = target.world.y - source.world.y
      val dz = target.world.z - source.world.z
      val nonDegenerate = dx * dx + dy * dy + dz * dz > 1e-24
      if math.abs(edge.weight) >= filter.threshold.value && signAccepted && regionAccepted && nonDegenerate then
        Some((edge, inputIndex, source, target))
      else None
    val ordered = selected.sortBy: (edge, inputIndex, _, _) =>
      (-math.abs(edge.weight), edge.source.value, edge.target.value, inputIndex)
    val retained = filter.topN.map(value => ordered.take(value.value)).getOrElse(ordered)
    val displayEdges = retained.map: (edge, _, source, target) =>
      SurfaceNetworkDisplayEdge(source, target, edge.weight, palette.color(edge.weight))
    val positives = displayEdges.count(_.weight > 0.0)
    val negatives = displayEdges.count(_.weight < 0.0)
    val zeros = displayEdges.length - positives - negatives
    val vertices = displayEdges.length * style.sides * 2
    val triangles = displayEdges.length * style.sides * 2
    val bytes = vertices.toLong * (3L * 4L * 2L + 4L) + triangles.toLong * 3L * 4L
    Right(SurfaceNetworkDisplay(
      displayEdges,
      style,
      SurfaceNetworkDisplayReceipt(
        network.nodes.length,
        network.edges.length,
        displayEdges.length,
        positives,
        negatives,
        zeros,
        vertices,
        triangles,
        bytes,
        System.nanoTime() - started
      )
    ))

final case class SurfaceNetworkAttachment(plan: SurfaceRenderPlan, receipt: SurfaceNetworkDisplayReceipt)

object SurfaceNetworkCompiler:
  def attach(
    plan: SurfaceRenderPlan,
    targetSurface: SurfaceId,
    layerId: SurfaceLayerId,
    display: SurfaceNetworkDisplay
  ): Either[SurfaceViewError, SurfaceNetworkAttachment] =
    val meshIndex = plan.meshes.indexWhere(_.surface == targetSurface)
    val slotIndex = plan.slots.indexWhere(_.surface == targetSurface)
    if meshIndex < 0 || slotIndex < 0 then Left(SurfaceViewError.UnknownSurface(targetSurface))
    else
      val generated = tubeGeometry(display)
      val base = plan.meshes(meshIndex)
      val baseVertices = base.positions.length / 3
      val addedVertices = generated.positions.length / 3
      val positions = new Array[Float](base.positions.length + generated.positions.length)
      val normals = new Array[Float](base.normals.length + generated.normals.length)
      val indices = new Array[Int](base.indices.length + generated.indices.length)
      copyFloats(base.positions, positions, 0)
      copyFloats(generated.positions, positions, base.positions.length)
      copyFloats(base.normals, normals, 0)
      copyFloats(generated.normals, normals, base.normals.length)
      var index = 0
      while index < base.indices.length do
        indices(index) = base.indices(index)
        index += 1
      index = 0
      while index < generated.indices.length do
        indices(base.indices.length + index) = generated.indices(index) + baseVertices
        index += 1

      val hash = networkHash(display)
      val meshKey = SurfaceResourceKey(s"${base.resourceKey.value}:network:$hash")
      val mergedMesh = SurfaceMeshPacket(
        targetSurface,
        meshKey,
        new FloatBufferView(positions),
        new FloatBufferView(normals),
        new IntBufferView(indices)
      )
      val layerKeyChanges = scala.collection.mutable.Map.empty[SurfaceResourceKey, SurfaceResourceKey]
      val extendedLayers = plan.layers.map: layer =>
        if layer.surface != targetSurface then layer
        else
          val colors = new Array[Int](baseVertices + addedVertices)
          var vertex = 0
          while vertex < baseVertices do
            colors(vertex) = layer.colors(vertex)
            vertex += 1
          while vertex < colors.length do
            colors(vertex) = Rgba32.unsafe(0, 0, 0, 0).packedInt
            vertex += 1
          val key = SurfaceResourceKey(s"${layer.resourceKey.value}:network:$hash")
          layerKeyChanges(layer.resourceKey) = key
          layer.copy(resourceKey = key, colors = new IntBufferView(colors))
      val networkColors = new Array[Int](baseVertices + addedVertices)
      index = 0
      while index < generated.colors.length do
        networkColors(baseVertices + index) = generated.colors(index)
        index += 1
      val networkKey = SurfaceResourceKey(s"network-layer:${layerId.value}:$hash")
      val networkLayer = SurfaceLayerPacket(
        layerId,
        targetSurface,
        networkKey,
        new IntBufferView(networkColors),
        DisplayOpacity.Opaque,
        DisplayBlendMode.Normal
      )
      val meshes = plan.meshes.updated(meshIndex, mergedMesh)
      val layers = extendedLayers :+ networkLayer
      val passes = plan.drawPasses.map: pass =>
        val nextMesh = if pass.mesh == base.resourceKey then meshKey else pass.mesh
        val nextLayer = layerKeyChanges.getOrElse(pass.layer, pass.layer)
        pass.copy(mesh = nextMesh, layer = nextLayer)
      val drawPasses = passes :+ SurfaceDrawPass(slotIndex, meshKey, networkKey, DisplayBlendMode.Normal)
      val profile = plan.profile.copy(
        verticesPacked = plan.profile.verticesPacked + addedVertices,
        facesPacked = plan.profile.facesPacked + generated.indices.length / 3,
        layersColored = plan.profile.layersColored + 1,
        colorValuesWritten = plan.profile.colorValuesWritten + baseVertices + addedVertices,
        primitiveBytes = plan.profile.primitiveBytes + display.receipt.primitiveBytes + baseVertices.toLong * 4L
      )
      val receipt = plan.receipt.copy(
        meshKeys = meshes.map(_.resourceKey),
        layerKeys = layers.map(_.resourceKey),
        drawPassCount = drawPasses.length
      )
      Right(SurfaceNetworkAttachment(
        plan.copy(meshes = meshes, layers = layers, drawPasses = drawPasses, profile = profile, receipt = receipt),
        display.receipt
      ))

  private final case class GeneratedNetwork(
    positions: FloatBufferView,
    normals: FloatBufferView,
    indices: IntBufferView,
    colors: Array[Int]
  )

  private def tubeGeometry(display: SurfaceNetworkDisplay): GeneratedNetwork =
    val sides = display.style.sides
    val vertexCount = display.edges.length * sides * 2
    val positions = new Array[Float](vertexCount * 3)
    val normals = new Array[Float](vertexCount * 3)
    val indices = new Array[Int](display.edges.length * sides * 6)
    val colors = new Array[Int](vertexCount)
    var edgeIndex = 0
    while edgeIndex < display.edges.length do
      val edge = display.edges(edgeIndex)
      val dx0 = edge.target.world.x - edge.source.world.x
      val dy0 = edge.target.world.y - edge.source.world.y
      val dz0 = edge.target.world.z - edge.source.world.z
      val length = math.sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0)
      val dx = dx0 / length
      val dy = dy0 / length
      val dz = dz0 / length
      val (rx, ry, rz) = if math.abs(dx) < 0.9 then (1.0, 0.0, 0.0) else (0.0, 1.0, 0.0)
      val ux0 = dy * rz - dz * ry
      val uy0 = dz * rx - dx * rz
      val uz0 = dx * ry - dy * rx
      val un = math.sqrt(ux0 * ux0 + uy0 * uy0 + uz0 * uz0)
      val ux = ux0 / un
      val uy = uy0 / un
      val uz = uz0 / un
      val vx = dy * uz - dz * uy
      val vy = dz * ux - dx * uz
      val vz = dx * uy - dy * ux
      var side = 0
      while side < sides do
        val angle = 2.0 * math.Pi * side.toDouble / sides.toDouble
        val nx = math.cos(angle) * ux + math.sin(angle) * vx
        val ny = math.cos(angle) * uy + math.sin(angle) * vy
        val nz = math.cos(angle) * uz + math.sin(angle) * vz
        val startVertex = edgeIndex * sides * 2 + side
        val endVertex = startVertex + sides
        writePoint(positions, startVertex, edge.source.world, nx, ny, nz, display.style.radius.value)
        writePoint(positions, endVertex, edge.target.world, nx, ny, nz, display.style.radius.value)
        writeNormal(normals, startVertex, nx, ny, nz)
        writeNormal(normals, endVertex, nx, ny, nz)
        colors(startVertex) = edge.color.packedInt
        colors(endVertex) = edge.color.packedInt
        val next = (side + 1) % sides
        val nextStart = edgeIndex * sides * 2 + next
        val nextEnd = nextStart + sides
        val offset = (edgeIndex * sides + side) * 6
        indices(offset) = startVertex
        indices(offset + 1) = endVertex
        indices(offset + 2) = nextStart
        indices(offset + 3) = nextStart
        indices(offset + 4) = endVertex
        indices(offset + 5) = nextEnd
        side += 1
      edgeIndex += 1
    GeneratedNetwork(
      new FloatBufferView(positions),
      new FloatBufferView(normals),
      new IntBufferView(indices),
      colors
    )

  private def writePoint(
    target: Array[Float],
    vertex: Int,
    center: WorldPoint,
    nx: Double,
    ny: Double,
    nz: Double,
    radius: Double
  ): Unit =
    val offset = vertex * 3
    target(offset) = (center.x + radius * nx).toFloat
    target(offset + 1) = (center.y + radius * ny).toFloat
    target(offset + 2) = (center.z + radius * nz).toFloat

  private def writeNormal(target: Array[Float], vertex: Int, x: Double, y: Double, z: Double): Unit =
    val offset = vertex * 3
    target(offset) = x.toFloat
    target(offset + 1) = y.toFloat
    target(offset + 2) = z.toFloat

  private def copyFloats(source: FloatBufferView, target: Array[Float], offset: Int): Unit =
    var index = 0
    while index < source.length do
      target(offset + index) = source(index)
      index += 1

  private def networkHash(display: SurfaceNetworkDisplay): String =
    var hash = MurmurHash3.mix(0x4a31d24f, display.style.sides)
    var index = 0
    while index < display.edges.length do
      val edge = display.edges(index)
      hash = MurmurHash3.mix(hash, MurmurHash3.stringHash(edge.source.id.value))
      hash = MurmurHash3.mix(hash, MurmurHash3.stringHash(edge.target.id.value))
      hash = MurmurHash3.mix(hash, java.lang.Double.hashCode(edge.weight))
      index += 1
    java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, display.edges.length * 3 + 1))
