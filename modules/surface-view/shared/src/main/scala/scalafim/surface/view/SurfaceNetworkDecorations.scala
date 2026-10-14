package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.VertexId

/** The one display occurrence used for each scientific surface in a network.
  * A map is required even when a surface currently has one occurrence: a later
  * repeated copy must never silently redirect the network.
  */
final case class SurfaceNetworkDecorationMapping(
  view: SurfaceViewId,
  instances: Map[SurfaceId, SurfaceInstanceId]
)

/** Edge radius policy is deliberately separate from filtering: display
  * selection is decided by SurfaceNetworkDisplay before this lowering step. */
sealed trait SurfaceNetworkDecorationRadius:
  private[view] def radiusFor(weight: Double): Double

object SurfaceNetworkDecorationRadius:
  final case class Fixed(value: SurfaceNetworkRadius) extends SurfaceNetworkDecorationRadius:
    private[view] def radiusFor(weight: Double): Double = value.value

  final class ByAbsoluteWeight private[SurfaceNetworkDecorationRadius] (
    val base: SurfaceNetworkRadius,
    val minimum: SurfaceNetworkRadius,
    val maximum: SurfaceNetworkRadius
  ) extends SurfaceNetworkDecorationRadius:
    private[view] def radiusFor(weight: Double): Double =
      math.max(minimum.value, math.min(maximum.value, base.value * math.abs(weight)))

  def byAbsoluteWeight(base: SurfaceNetworkRadius, minimum: SurfaceNetworkRadius,
      maximum: SurfaceNetworkRadius): Either[SurfaceDecorationError, SurfaceNetworkDecorationRadius] =
    if minimum.value > maximum.value then
      Left(SurfaceDecorationError.NetworkMapping("network radius minimum exceeds maximum"))
    else Right(new ByAbsoluteWeight(base, minimum, maximum))

/** Lowers retained network display edges to display-only decoration meshes.
  * Scientific network nodes are only anchors; their cached world coordinates
  * are intentionally never used here.
  */
object SurfaceNetworkDecorations:
  def lower(
    display: SurfaceNetworkDisplay,
    plan: SurfaceCompositionPlan,
    mapping: SurfaceNetworkDecorationMapping,
    nodeShape: SurfaceMarkerShape,
    nodeSize: SurfaceDecorationSize,
    canvasWidth: Int,
    canvasHeight: Int,
    width: Option[SurfaceNetworkDecorationRadius] = None,
    nodeColor: Rgba32 = Rgba32.unsafe(245, 245, 245)
  ): Either[SurfaceDecorationError, SurfaceDecorationGeometry] =
    for
      view <- plan.views.find(_.id == mapping.view).toRight(SurfaceDecorationError.NetworkMapping("network view is absent"))
      _ <- validateMapping(display, view, mapping)
      packets <- nodePackets(display, plan, mapping, nodeShape, nodeSize, nodeColor)
      nodes <- SurfaceDecorationGeometry.lower(packets, plan, canvasWidth, canvasHeight)
      edges <- edgeMeshes(display, plan, mapping, width.getOrElse(SurfaceNetworkDecorationRadius.Fixed(display.style.radius)),
        display.style.shape, display.style.sides)
    yield SurfaceDecorationGeometry(nodes.meshes ++ edges, nodes.labels, canvasWidth, canvasHeight,
      SurfaceDecorationBinding.key(plan, canvasWidth, canvasHeight))

  private def validateMapping(display: SurfaceNetworkDisplay, view: SurfaceCompositionView,
      mapping: SurfaceNetworkDecorationMapping): Either[SurfaceDecorationError, Unit] =
    val surfaces = display.edges.flatMap(edge => Vector(edge.source.surface, edge.target.surface)).distinct
    surfaces.find(surface => !mapping.instances.contains(surface)) match
      case Some(surface) => Left(SurfaceDecorationError.NetworkMapping(s"network mapping is missing '${surface.value}'"))
      case None =>
        surfaces.find: surface =>
          val id = mapping.instances(surface)
          !view.instances.exists(instance => instance.id == id && instance.surface == surface)
        match
          case Some(surface) => Left(SurfaceDecorationError.NetworkMapping(
            s"network mapping for '${surface.value}' does not name an instance in the selected view"))
          case None => Right(())

  private def nodePackets(display: SurfaceNetworkDisplay, plan: SurfaceCompositionPlan,
      mapping: SurfaceNetworkDecorationMapping, shape: SurfaceMarkerShape, size: SurfaceDecorationSize,
      color: Rgba32): Either[SurfaceDecorationError, SurfaceDecorationPackets] =
    val nodes = scala.collection.mutable.LinkedHashMap.empty[SurfaceNetworkNodeId, SurfaceNetworkNode]
    display.edges.foreach: edge =>
      nodes.getOrElseUpdate(edge.source.id, edge.source)
      nodes.getOrElseUpdate(edge.target.id, edge.target)
    val resolved = Vector.newBuilder[SurfaceMarkerPacket]
    var failure: Option[SurfaceDecorationError] = None
    val iterator = nodes.valuesIterator
    while iterator.hasNext && failure.isEmpty do
      val node = iterator.next()
      endpoint(node, plan, mapping) match
        case Left(value) => failure = Some(value)
        case Right(point) =>
          resolved += SurfaceMarkerPacket(
            SurfaceDecorationElementId(decorationId("node", node.id.value), "glyph"),
            SurfaceDisplayOrigin(mapping.view, mapping.instances(node.surface)),
            SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(node.surface, node.vertex)), point,
            shape, size, color, SurfaceDecorationDepthPolicy.DepthTest)
    failure.fold[Either[SurfaceDecorationError, SurfaceDecorationPackets]](
      Right(SurfaceDecorationPackets(resolved.result(), Vector.empty, Vector.empty,
        SurfaceDecorationBinding.key(plan, 1, 1))))(Left(_))

  private def edgeMeshes(display: SurfaceNetworkDisplay, plan: SurfaceCompositionPlan,
      mapping: SurfaceNetworkDecorationMapping, width: SurfaceNetworkDecorationRadius,
      shape: SurfaceNetworkShape, sides: Int): Either[SurfaceDecorationError, Vector[SurfaceDecorationMesh]] =
    val output = Vector.newBuilder[SurfaceDecorationMesh]
    var error: Option[SurfaceDecorationError] = None
    var index = 0
    while index < display.edges.length && error.isEmpty do
      val edge = display.edges(index)
      (endpoint(edge.source, plan, mapping), endpoint(edge.target, plan, mapping)) match
        case (Right(start), Right(end)) =>
          val occurrence = display.edges.take(index).count(prior => prior.source.id == edge.source.id && prior.target.id == edge.target.id)
          if occurrence > 0 then
            error = Some(SurfaceDecorationError.NetworkMapping(
              s"duplicate network edge '${edge.source.id.value}->${edge.target.id.value}' requires a stable edge id"))
          else
            val mesh = shape match
              case SurfaceNetworkShape.Line => ribbon(decorationId("edge", edge.source.id.value, edge.target.id.value),
                occurrence, edge, start, end, width, plan, mapping)
              case SurfaceNetworkShape.Tube => tube(decorationId("edge", edge.source.id.value, edge.target.id.value),
                occurrence, edge, start, end, width, sides, mapping)
            mesh match
              case Left(value) => error = Some(value)
              case Right(mesh) => output += mesh
        case (Left(value), _) => error = Some(value)
        case (_, Left(value)) => error = Some(value)
      index += 1
    error.fold[Either[SurfaceDecorationError, Vector[SurfaceDecorationMesh]]](Right(output.result()))(Left(_))

  private def endpoint(node: SurfaceNetworkNode, plan: SurfaceCompositionPlan,
      mapping: SurfaceNetworkDecorationMapping): Either[SurfaceDecorationError, SurfaceDecorationWorldPoint] =
    val instance = mapping.instances.get(node.surface).toRight(SurfaceDecorationError.NetworkMapping(
      s"network mapping is missing '${node.surface.value}'"))
    instance.flatMap: id =>
      plan.views.find(_.id == mapping.view).flatMap(_.instances.find(_.id == id)) match
        case None => Left(SurfaceDecorationError.NetworkMapping("network endpoint is not mapped in the selected view"))
        case Some(value) =>
          plan.resources.meshes.get(value.mesh).toRight(SurfaceDecorationError.NetworkMapping("network instance mesh is absent")).flatMap: mesh =>
            SurfaceDecorationCompiler.resolve(SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(node.surface, node.vertex)), mesh).map: point =>
              SurfaceDecorationWorldPoint(point.x + value.translationX, point.y + value.translationY, point.z + value.translationZ)

  private def tube(id: SurfaceDecorationId, occurrence: Int, edge: SurfaceNetworkDisplayEdge,
      start: SurfaceDecorationWorldPoint, end: SurfaceDecorationWorldPoint, radiusPolicy: SurfaceNetworkDecorationRadius,
      sides: Int, mapping: SurfaceNetworkDecorationMapping): Either[SurfaceDecorationError, SurfaceDecorationMesh] =
    val r = radiusPolicy.radiusFor(edge.weight)
    val dx = end.x - start.x
    val dy = end.y - start.y
    val dz = end.z - start.z
    val length = math.sqrt(dx * dx + dy * dy + dz * dz)
    if length <= 1e-12 then Left(SurfaceDecorationError.NetworkMapping("network edge endpoints coincide in the selected composition"))
    else
      val axis = (dx / length, dy / length, dz / length)
      val reference = if math.abs(axis._1) < .9 then (1.0, 0.0, 0.0) else (0.0, 1.0, 0.0)
      val u0 = cross(axis, reference)
      val un = norm(u0)
      val u = (u0._1 / un, u0._2 / un, u0._3 / un)
      val v = cross(axis, u)
      val positions = new Array[Float]((sides * 2 + 2) * 3)
      val normals = new Array[Float](positions.length)
      val indices = new Array[Int](sides * 12)
      var side = 0
      while side < sides do
        val angle = 2.0 * math.Pi * side / sides
        val n = (u._1 * math.cos(angle) + v._1 * math.sin(angle), u._2 * math.cos(angle) + v._2 * math.sin(angle), u._3 * math.cos(angle) + v._3 * math.sin(angle))
        put(positions, side, start.x + r * n._1, start.y + r * n._2, start.z + r * n._3)
        put(positions, sides + side, end.x + r * n._1, end.y + r * n._2, end.z + r * n._3)
        put(normals, side, n._1, n._2, n._3)
        put(normals, sides + side, n._1, n._2, n._3)
        val next = (side + 1) % sides
        val offset = side * 6
        indices(offset) = side
        indices(offset + 1) = next
        indices(offset + 2) = sides + side
        indices(offset + 3) = next
        indices(offset + 4) = sides + next
        indices(offset + 5) = sides + side
        val cap = sides * 6 + side * 6
        indices(cap) = sides * 2
        indices(cap + 1) = next
        indices(cap + 2) = side
        indices(cap + 3) = sides * 2 + 1
        indices(cap + 4) = sides + side
        indices(cap + 5) = sides + next
        side += 1
      put(positions, sides * 2, start.x, start.y, start.z)
      put(positions, sides * 2 + 1, end.x, end.y, end.z)
      put(normals, sides * 2, -axis._1, -axis._2, -axis._3)
      put(normals, sides * 2 + 1, axis._1, axis._2, axis._3)
      Right(SurfaceDecorationMesh(SurfaceDecorationElementId(id, s"occurrence-$occurrence"),
        SurfaceDisplayOrigin(mapping.view, mapping.instances(edge.source.surface)),
        SurfaceDecorationTarget.NetworkEdge(SurfaceVertexAnchor.make(edge.source.surface, edge.source.vertex),
          SurfaceVertexAnchor.make(edge.target.surface, edge.target.vertex)),
        new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(indices), edge.color,
        SurfaceDecorationDepthPolicy.DepthTest))

  /** A line is a camera-facing world ribbon. It remains independent of the
    * scientific mesh, unlike the legacy network compiler's appended faces. */
  private def ribbon(id: SurfaceDecorationId, occurrence: Int, edge: SurfaceNetworkDisplayEdge,
      start: SurfaceDecorationWorldPoint, end: SurfaceDecorationWorldPoint, radiusPolicy: SurfaceNetworkDecorationRadius,
      plan: SurfaceCompositionPlan, mapping: SurfaceNetworkDecorationMapping): Either[SurfaceDecorationError, SurfaceDecorationMesh] =
    val dx = end.x - start.x
    val dy = end.y - start.y
    val dz = end.z - start.z
    val length = math.sqrt(dx * dx + dy * dy + dz * dz)
    if length <= 1e-12 then Left(SurfaceDecorationError.NetworkMapping("network edge endpoints coincide in the selected composition"))
    else
      val axis = (dx / length, dy / length, dz / length)
      val camera = plan.views.find(_.id == mapping.view).get.camera
      val normal0 = (camera.directionX, camera.directionY, camera.directionZ)
      val normalLength = norm(normal0)
      val normal = if normalLength > 1e-12 then
        (normal0._1 / normalLength, normal0._2 / normalLength, normal0._3 / normalLength)
      else (0.0, 0.0, 1.0)
      val side0 = cross(normal, axis)
      val sideLength = norm(side0)
      val side = if sideLength > 1e-12 then
        (side0._1 / sideLength, side0._2 / sideLength, side0._3 / sideLength)
      else
        val fallback = cross(axis, if math.abs(axis._1) < .9 then (1.0, 0.0, 0.0) else (0.0, 1.0, 0.0))
        val fallbackLength = norm(fallback)
        (fallback._1 / fallbackLength, fallback._2 / fallbackLength, fallback._3 / fallbackLength)
      val r = radiusPolicy.radiusFor(edge.weight)
      val positions = new Array[Float](12)
      val normals = new Array[Float](12)
      put(positions, 0, start.x + r * side._1, start.y + r * side._2, start.z + r * side._3)
      put(positions, 1, end.x + r * side._1, end.y + r * side._2, end.z + r * side._3)
      put(positions, 2, end.x - r * side._1, end.y - r * side._2, end.z - r * side._3)
      put(positions, 3, start.x - r * side._1, start.y - r * side._2, start.z - r * side._3)
      var vertex = 0
      while vertex < 4 do
        put(normals, vertex, normal._1, normal._2, normal._3)
        vertex += 1
      Right(SurfaceDecorationMesh(SurfaceDecorationElementId(id, s"occurrence-$occurrence"),
        SurfaceDisplayOrigin(mapping.view, mapping.instances(edge.source.surface)),
        SurfaceDecorationTarget.NetworkEdge(SurfaceVertexAnchor.make(edge.source.surface, edge.source.vertex),
          SurfaceVertexAnchor.make(edge.target.surface, edge.target.vertex)),
        new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(Array(0, 1, 2, 0, 2, 3)), edge.color,
        SurfaceDecorationDepthPolicy.DepthTest))

  private def decorationId(kind: String, values: String*): SurfaceDecorationId =
    SurfaceDecorationId.make(s"network-$kind-${values.map(value => s"${value.length}:$value").mkString("-")}").toOption.get

  private def cross(a: (Double, Double, Double), b: (Double, Double, Double)): (Double, Double, Double) =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)

  private def norm(value: (Double, Double, Double)): Double = math.sqrt(value._1 * value._1 + value._2 * value._2 + value._3 * value._3)

  private def put(values: Array[Float], index: Int, x: Double, y: Double, z: Double): Unit =
    values(index * 3) = x.toFloat
    values(index * 3 + 1) = y.toFloat
    values(index * 3 + 2) = z.toFloat
