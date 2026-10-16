package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.VertexId

enum SurfaceDecorationTarget:
  case Anchor(value: SurfaceDecorationAnchor)
  case Edge(surface: SurfaceId, from: VertexId, to: VertexId)
  /** A display-only network edge whose endpoints retain their separate
    * scientific surface anchors. */
  case NetworkEdge(source: SurfaceVertexAnchor, target: SurfaceVertexAnchor)

final case class SurfaceDecorationMesh(
  id: SurfaceDecorationElementId,
  origin: SurfaceDisplayOrigin,
  target: SurfaceDecorationTarget,
  positions: FloatBufferView,
  normals: FloatBufferView,
  indices: IntBufferView,
  color: Rgba32,
  depthPolicy: SurfaceDecorationDepthPolicy,
  texture: Option[SurfaceDecorationTexture] = None,
  pickable: Boolean = true,
  roundedStroke: Option[SurfaceDecorationGeometry.RoundedStroke] = None
):
  require(positions.length >= 9 && positions.length % 3 == 0, "decoration positions must contain triangle triples")
  require(normals.length == positions.length, "decoration normals must cover every position")
  require(indices.length >= 3 && indices.length % 3 == 0, "decoration indices must contain triangles")
  require((0 until positions.length).forall(i => positions(i).isFinite && normals(i).isFinite),
    "decoration positions and normals must be finite")
  require((0 until indices.length).forall(i => indices(i) >= 0 && indices(i) < positions.length / 3),
    "decoration indices must address positions")

/** Text remains an explicit renderer-owned asset until label layout supplies
  * camera-facing textured quads. */
final case class SurfaceDecorationGeometry(
  meshes: Vector[SurfaceDecorationMesh],
  labels: Vector[SurfaceLabelPacket],
  canvasWidth: Int,
  canvasHeight: Int,
  binding: String = ""
):
  require(canvasWidth > 0 && canvasHeight > 0, "decoration canvas dimensions must be positive")

object SurfaceDecorationGeometry:
  /** Proof of the builder's nonoverlapping capsule triangulation in this view.
    * A copied mesh cannot reuse this proof with different geometry or origin.
    * Only this builder can construct it. Buffers are publicly read-only;
    * trusted internal consumers must not mutate their unsafe arrays. */
  final class RoundedStroke private[SurfaceDecorationGeometry] (
      positions: FloatBufferView, normals: FloatBufferView, indices: IntBufferView,
      origin: SurfaceDisplayOrigin, target: SurfaceDecorationTarget, binding: String):
    private[view] def accepts(mesh: SurfaceDecorationMesh, currentBinding: String): Boolean =
      (mesh.positions eq positions) && (mesh.normals eq normals) && (mesh.indices eq indices) &&
        mesh.origin == origin && mesh.target == target && binding == currentBinding

  def lower(packets: SurfaceDecorationPackets, plan: SurfaceCompositionPlan,
      canvasWidth: Int, canvasHeight: Int): Either[SurfaceDecorationError, SurfaceDecorationGeometry] =
    if packets.binding != SurfaceDecorationBinding.key(plan, 1, 1) then
      Left(SurfaceDecorationError.StalePackets)
    else if canvasWidth <= 0 || canvasHeight <= 0 then
      Left(SurfaceDecorationError.InvalidSize(math.min(canvasWidth, canvasHeight)))
    else
      val meshes = Vector.newBuilder[SurfaceDecorationMesh]
      val halos = Vector.newBuilder[SurfaceDecorationMesh]
      val binding = SurfaceDecorationBinding.key(plan, canvasWidth, canvasHeight)
      var failure: Option[SurfaceDecorationError] = None
      var marker = 0
      while marker < packets.markers.length && failure.isEmpty do
        markerMesh(packets.markers(marker), plan, canvasWidth, canvasHeight) match
          case Right(mesh) => meshes += mesh
          case Left(error) => failure = Some(error)
        marker += 1
      var contour = 0
      while contour < packets.contours.length && failure.isEmpty do
        contourMeshes(packets.contours(contour), plan, canvasWidth, canvasHeight, binding) match
          case Right(values) =>
            if packets.contours(contour).style.mode == SurfaceContourMode.RoundedStrokes then
              meshes += values.head
              halos ++= values.tail
            else meshes ++= values
          case Left(error) => failure = Some(error)
        contour += 1
      failure.fold[Either[SurfaceDecorationError, SurfaceDecorationGeometry]](
        Right(SurfaceDecorationGeometry(meshes.result() ++ halos.result(), packets.labels, canvasWidth, canvasHeight,
          binding)))(Left(_))

  private def markerMesh(packet: SurfaceMarkerPacket, plan: SurfaceCompositionPlan,
      width: Int, height: Int): Either[SurfaceDecorationError, SurfaceDecorationMesh] =
    view(packet.origin, plan).flatMap: target =>
      val (right, up, normal) = basis(target.camera)
      val radii = packet.size.unit match
        case SurfaceDecorationUnit.World => Right((packet.size.value, packet.size.value))
        case SurfaceDecorationUnit.Pixels => pixelAxes(packet.point, plan, target, width, height).map: axes =>
          (packet.size.value * axes._1, packet.size.value * axes._2)
      radii.flatMap: values =>
        if packet.shape == SurfaceMarkerShape.Sphere && packet.size.unit == SurfaceDecorationUnit.Pixels &&
            !close(values._1, values._2) then Left(SurfaceDecorationError.AnisotropicPixelSphere)
        else Right(packet.shape match
          case SurfaceMarkerShape.Disk => disk(packet, values._1, values._2, right, up, normal)
          case SurfaceMarkerShape.Crosshair => crosshair(packet, values._1, values._2, right, up, normal)
          case SurfaceMarkerShape.Sphere => sphere(packet, values._1))

  private def contourMeshes(packet: SurfaceContourPacket, plan: SurfaceCompositionPlan,
      width: Int, height: Int, binding: String): Either[SurfaceDecorationError, Vector[SurfaceDecorationMesh]] =
    view(packet.origin, plan).flatMap: target =>
      if packet.style.mode == SurfaceContourMode.RoundedStrokes then
        for
          core <- capsule(packet, plan, target, width, height, packet.id, packet.style.width.value, packet.style.color, binding)
          halo <- packet.style.halo match
            case None => Right(Vector.empty)
            case Some(value) => capsule(packet, plan, target, width, height,
              SurfaceDecorationElementId(packet.id.decoration, s"${packet.id.suffix}-halo"),
              packet.style.width.value + value.width.value, value.color, binding).map(Vector(_))
        yield Vector(core) ++ halo
      else contourOffsets(packet, plan, target, width, height).map: offsets =>
        val edge = SurfaceDecorationTarget.Edge(packet.surface, packet.sourceEdge._1, packet.sourceEdge._2)
        val core = ribbon(packet.id, packet.origin, edge, offsets.coreLeftStart, offsets.coreLeftEnd,
          offsets.coreRightStart, offsets.coreRightEnd, packet.style.color, packet.style.depthPolicy)
        packet.style.halo match
          case None => Vector(core)
          case Some(halo) =>
            Vector(
              ribbon(SurfaceDecorationElementId(packet.id.decoration, s"${packet.id.suffix}-halo-left"), packet.origin, edge,
                offsets.coreLeftStart, offsets.coreLeftEnd, offsets.outerLeftStart, offsets.outerLeftEnd,
                halo.color, packet.style.depthPolicy),
              ribbon(SurfaceDecorationElementId(packet.id.decoration, s"${packet.id.suffix}-halo-right"), packet.origin, edge,
                offsets.outerRightStart, offsets.outerRightEnd, offsets.coreRightStart, offsets.coreRightEnd,
                halo.color, packet.style.depthPolicy),
              core
            )

  /** The body and both semicircles share one mesh, so one edge contributes
    * once per pixel while separate edges retain their overlapping alpha. */
  private def capsule(packet: SurfaceContourPacket, plan: SurfaceCompositionPlan, view: SurfaceCompositionView, width: Int, height: Int,
      id: SurfaceDecorationElementId, strokeWidth: Double, color: Rgba32, binding: String): Either[SurfaceDecorationError, SurfaceDecorationMesh] =
    for
      _ <- roundedProjection(packet.start, packet.end, view.camera)
      start <- project(packet.start, plan, view, width, height)
      end <- project(packet.end, plan, view, width, height)
      startAxes <- pixelAxes(packet.start, plan, view, width, height)
      endAxes <- pixelAxes(packet.end, plan, view, width, height)
    yield
      val (right, up, normal) = basis(view.camera)
      val dx = end._1 - start._1
      val dy = end._2 - start._2
      val length = math.hypot(dx, dy)
      val tangent = if length <= 1e-9 then (1.0, 0.0) else (dx / length, dy / length)
      val radius = strokeWidth / 2.0
      // Fixed geometric approximation, independent of captured candidate pixels.
      val segments = 32
      val capVertices = segments + 2
      val positions = new Array[Float]((4 + 2 * capVertices) * 3)
      val normals = new Array[Float](positions.length)
      val indices = new Array[Int](6 + 6 * segments)
      def offset(point: P, axes: (Double, Double), along: Double, across: Double): P =
        val x = (tangent._1 * along - tangent._2 * across) * radius * axes._1
        val y = (tangent._2 * along + tangent._1 * across) * radius * axes._2
        add(point, add(scale(right, x), scale(up, y)))
      put(positions, 0, offset(packet.start, startAxes, 0, 1))
      put(positions, 1, offset(packet.end, endAxes, 0, 1))
      put(positions, 2, offset(packet.start, startAxes, 0, -1))
      put(positions, 3, offset(packet.end, endAxes, 0, -1))
      Array(0, 1, 2, 2, 1, 3).copyToArray(indices)
      var cap = 0
      while cap < 2 do
        val centre = 4 + cap * capVertices
        val point = if cap == 0 then packet.start else packet.end
        val axes = if cap == 0 then startAxes else endAxes
        put(positions, centre, point)
        var vertex = 0
        while vertex <= segments do
          val angle = (if cap == 0 then math.Pi / 2 else -math.Pi / 2) + math.Pi * vertex / segments
          put(positions, centre + 1 + vertex, offset(point, axes, math.cos(angle), math.sin(angle)))
          if vertex < segments then
            val index = 6 + (cap * segments + vertex) * 3
            indices(index) = centre
            indices(index + 1) = centre + 1 + vertex
            indices(index + 2) = centre + 2 + vertex
          vertex += 1
        cap += 1
      var vertex = 0
      while vertex < positions.length / 3 do
        put(normals, vertex, normal)
        vertex += 1
      val mesh = make(id, packet.origin, SurfaceDecorationTarget.Edge(packet.surface, packet.sourceEdge._1, packet.sourceEdge._2),
        positions, normals, indices, color, SurfaceDecorationDepthPolicy.OrderedDepthTest)
      mesh.copy(roundedStroke = Some(new RoundedStroke(mesh.positions, mesh.normals, mesh.indices, mesh.origin, mesh.target, binding)))

  private def roundedProjection(start: P, end: P, camera: SurfaceCameraPacket): Either[SurfaceDecorationError, Unit] =
    val inFront = Vector(start, end).forall: point =>
      val eye = mul(camera.viewMatrix, point, 1.0)
      val p = camera.projectionMatrix
      val w = p(12) * eye._1 + p(13) * eye._2 + p(14) * eye._3 + p(15) * eye._4
      w.isFinite && w > 1e-12
    if inFront then Right(()) else Left(SurfaceDecorationError.InvalidProjection)

  private final case class ContourOffsets(
    coreLeftStart: P, coreLeftEnd: P, coreRightStart: P, coreRightEnd: P,
    outerLeftStart: P, outerLeftEnd: P, outerRightStart: P, outerRightEnd: P
  )

  /** Pixel ribbons are perpendicular in projected pixel space at both endpoints.
    * World ribbons are camera-facing and use their declared world-unit width. */
  private def contourOffsets(packet: SurfaceContourPacket, plan: SurfaceCompositionPlan, view: SurfaceCompositionView,
      width: Int, height: Int): Either[SurfaceDecorationError, ContourOffsets] =
    val haloWidth = packet.style.halo.fold(0.0)(_.width.value / 2.0)
    packet.style.width.unit match
      case SurfaceDecorationUnit.Pixels =>
        for
          perpendicular <- projectedPerpendicular(packet.start, packet.end, plan, view, width, height)
          startCore <- screenOffset(packet.start, perpendicular, packet.style.width.value / 2.0, plan, view, width, height)
          endCore <- screenOffset(packet.end, perpendicular, packet.style.width.value / 2.0, plan, view, width, height)
          startOuter <- screenOffset(packet.start, perpendicular, packet.style.width.value / 2.0 + haloWidth, plan, view, width, height)
          endOuter <- screenOffset(packet.end, perpendicular, packet.style.width.value / 2.0 + haloWidth, plan, view, width, height)
        yield offsets(packet.start, packet.end, startCore, endCore, startOuter, endOuter)
      case SurfaceDecorationUnit.World =>
        val (_, _, cameraNormal) = basis(view.camera)
        val tangent = unit(sub(packet.end, packet.start), cameraNormal)
        val side = unit(cross(cameraNormal, tangent), fallbackPerpendicular(cameraNormal))
        val core = scale(side, packet.style.width.value / 2.0)
        val outer = scale(side, packet.style.width.value / 2.0 + haloWidth)
        Right(offsets(packet.start, packet.end, core, core, outer, outer))

  private def offsets(start: P, end: P, startCore: P, endCore: P,
      startOuter: P, endOuter: P): ContourOffsets =
    ContourOffsets(add(start, startCore), add(end, endCore), sub(start, startCore), sub(end, endCore),
      add(start, startOuter), add(end, endOuter), sub(start, startOuter), sub(end, endOuter))

  private def projectedPerpendicular(start: P, end: P, plan: SurfaceCompositionPlan, view: SurfaceCompositionView,
      width: Int, height: Int): Either[SurfaceDecorationError, (Double, Double)] =
    for
      a <- project(start, plan, view, width, height)
      b <- project(end, plan, view, width, height)
    yield
      val dx = b._1 - a._1
      val dy = b._2 - a._2
      val length = math.hypot(dx, dy)
      if length <= 1e-9 then (0.0, 1.0) else (-dy / length, dx / length)

  private def screenOffset(point: P, direction: (Double, Double), pixels: Double,
      plan: SurfaceCompositionPlan, view: SurfaceCompositionView, width: Int, height: Int): Either[SurfaceDecorationError, P] =
    pixelAxes(point, plan, view, width, height).map: axes =>
      val (right, up, _) = basis(view.camera)
      add(scale(right, direction._1 * pixels * axes._1), scale(up, direction._2 * pixels * axes._2))

  private def view(origin: SurfaceDisplayOrigin, plan: SurfaceCompositionPlan): Either[SurfaceDecorationError, SurfaceCompositionView] =
    plan.views.find(v => v.id == origin.view && v.instances.exists(_.id == origin.instance)) match
      case Some(value) => Right(value)
      case None => Left(SurfaceDecorationError.UnknownDisplay(origin))

  private def pixelAxes(point: P, plan: SurfaceCompositionPlan, view: SurfaceCompositionView, width: Int,
      height: Int): Either[SurfaceDecorationError, (Double, Double)] =
    val eye = mul(view.camera.viewMatrix, point, 1.0)
    val projection = view.camera.projectionMatrix
    val clipW = projection(12) * eye._1 + projection(13) * eye._2 + projection(14) * eye._3 + projection(15) * eye._4
    val viewport = fittedViewport(plan, view, width, height)
    val horizontal = math.abs(projection(0).toDouble) * viewport.width * width
    val vertical = math.abs(projection(5).toDouble) * viewport.height * height
    if !clipW.isFinite || math.abs(clipW) <= 1e-12 || !horizontal.isFinite || !vertical.isFinite ||
        horizontal <= 1e-12 || vertical <= 1e-12 then Left(SurfaceDecorationError.InvalidProjection)
    else Right((2.0 * math.abs(clipW) / horizontal, 2.0 * math.abs(clipW) / vertical))

  private def project(point: P, plan: SurfaceCompositionPlan, view: SurfaceCompositionView, width: Int,
      height: Int): Either[SurfaceDecorationError, (Double, Double)] =
    val eye = mul(view.camera.viewMatrix, point, 1.0)
    val clip = mul(view.camera.projectionMatrix, P(eye._1, eye._2, eye._3), eye._4)
    val viewport = fittedViewport(plan, view, width, height)
    if !clip._4.isFinite || math.abs(clip._4) <= 1e-12 then Left(SurfaceDecorationError.InvalidProjection)
    else
      val x = (viewport.x + (clip._1 / clip._4 + 1.0) * viewport.width / 2.0) * width
      val y = (viewport.y + (clip._2 / clip._4 + 1.0) * viewport.height / 2.0) * height
      if x.isFinite && y.isFinite then Right((x, y)) else Left(SurfaceDecorationError.InvalidProjection)

  private def fittedViewport(plan: SurfaceCompositionPlan, view: SurfaceCompositionView, width: Int, height: Int): SurfaceViewport =
    plan.fittedViewports(width, height).find(_._1.id == view.id).get._2

  private def disk(packet: SurfaceMarkerPacket, radiusX: Double, radiusY: Double, right: P, up: P,
      normal: P): SurfaceDecorationMesh =
    val sides = 16
    val positions = new Array[Float]((sides + 1) * 3)
    val normals = new Array[Float](positions.length)
    val indices = new Array[Int](sides * 3)
    put(positions, 0, packet.point)
    put(normals, 0, normal)
    var index = 0
    while index < sides do
      val angle = 2.0 * math.Pi * index / sides
      put(positions, index + 1, add(packet.point, add(scale(right, radiusX * math.cos(angle)), scale(up, radiusY * math.sin(angle)))))
      put(normals, index + 1, normal)
      indices(index * 3) = 0
      indices(index * 3 + 1) = index + 1
      indices(index * 3 + 2) = (index + 1) % sides + 1
      index += 1
    make(packet.id, packet.origin, SurfaceDecorationTarget.Anchor(packet.anchor), positions, normals, indices, packet.color, packet.depthPolicy)

  private def crosshair(packet: SurfaceMarkerPacket, spanX: Double, spanY: Double, right: P, up: P,
      normal: P): SurfaceDecorationMesh =
    val strokeX = spanX / 5.0
    val strokeY = spanY / 5.0
    val points = Vector((-spanX, -strokeY), (spanX, -strokeY), (spanX, strokeY), (-spanX, strokeY),
      (-strokeX, -spanY), (strokeX, -spanY), (strokeX, spanY), (-strokeX, spanY))
    val positions = new Array[Float](24)
    val normals = new Array[Float](24)
    var index = 0
    while index < points.length do
      put(positions, index, add(packet.point, add(scale(right, points(index)._1), scale(up, points(index)._2))))
      put(normals, index, normal)
      index += 1
    make(packet.id, packet.origin, SurfaceDecorationTarget.Anchor(packet.anchor), positions, normals,
      Array(0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7), packet.color, packet.depthPolicy)

  private def sphere(packet: SurfaceMarkerPacket, radius: Double): SurfaceDecorationMesh =
    // A Y-polar 12-by-12 sphere matches the public reference marker primitive.
    // Omit the degenerate pole triangles, keeping winding outward throughout.
    val latitude = 12
    val longitude = 12
    val positions = new Array[Float]((latitude + 1) * (longitude + 1) * 3)
    val normals = new Array[Float](positions.length)
    val indices = new Array[Int]((latitude - 1) * longitude * 6)
    var lat = 0
    while lat <= latitude do
      var lon = 0
      while lon <= longitude do
        val theta = lat.toDouble / latitude * math.Pi
        val phi = lon.toDouble / longitude * (2.0 * math.Pi)
        val ring = radius * math.sin(theta)
        val offset = P(-ring * math.cos(phi), radius * math.cos(theta), ring * math.sin(phi))
        val normal = P(-math.sin(theta) * math.cos(phi), math.cos(theta), math.sin(theta) * math.sin(phi))
        val vertex = lat * (longitude + 1) + lon
        put(positions, vertex, add(packet.point, offset))
        put(normals, vertex, normal)
        lon += 1
      lat += 1
    var offset = 0
    lat = 0
    while lat < latitude do
      var lon = 0
      while lon < longitude do
        val vertex = lat * (longitude + 1) + lon
        if lat > 0 then
          indices(offset) = vertex + 1
          indices(offset + 1) = vertex
          indices(offset + 2) = vertex + longitude + 2
          offset += 3
        if lat < latitude - 1 then
          indices(offset) = vertex
          indices(offset + 1) = vertex + longitude + 1
          indices(offset + 2) = vertex + longitude + 2
          offset += 3
        lon += 1
      lat += 1
    make(packet.id, packet.origin, SurfaceDecorationTarget.Anchor(packet.anchor), positions, normals, indices, packet.color, packet.depthPolicy)

  private def ribbon(id: SurfaceDecorationElementId, origin: SurfaceDisplayOrigin, target: SurfaceDecorationTarget,
      leftStart: P, leftEnd: P, rightStart: P, rightEnd: P, color: Rgba32,
      depth: SurfaceDecorationDepthPolicy): SurfaceDecorationMesh =
    val positions = new Array[Float](12)
    val normal = unit(cross(sub(leftEnd, leftStart), sub(rightStart, leftStart)), P(0.0, 0.0, 1.0))
    val normals = new Array[Float](12)
    put(positions, 0, leftStart)
    put(positions, 1, leftEnd)
    put(positions, 2, rightStart)
    put(positions, 3, rightEnd)
    var index = 0
    while index < 4 do
      put(normals, index, normal)
      index += 1
    make(id, origin, target, positions, normals, Array(0, 1, 2, 2, 1, 3), color, depth)

  private def make(id: SurfaceDecorationElementId, origin: SurfaceDisplayOrigin, target: SurfaceDecorationTarget,
      positions: Array[Float], normals: Array[Float], indices: Array[Int], color: Rgba32,
      depth: SurfaceDecorationDepthPolicy): SurfaceDecorationMesh =
    SurfaceDecorationMesh(id, origin, target, new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(indices), color, depth)

  private type P = SurfaceDecorationWorldPoint
  private object P:
    def apply(x: Double, y: Double, z: Double): P = SurfaceDecorationWorldPoint(x, y, z)
  private def add(a: P, b: P): P = P(a.x + b.x, a.y + b.y, a.z + b.z)
  private def sub(a: P, b: P): P = P(a.x - b.x, a.y - b.y, a.z - b.z)
  private def scale(point: P, amount: Double): P = P(point.x * amount, point.y * amount, point.z * amount)
  private def cross(a: P, b: P): P = P(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
  private def unit(point: P, fallback: P): P =
    val length = math.sqrt(point.x * point.x + point.y * point.y + point.z * point.z)
    if !length.isFinite || length <= 1e-12 then fallback else scale(point, 1.0 / length)
  private def fallbackPerpendicular(normal: P): P =
    if math.abs(normal.x) < 0.9 then unit(cross(normal, P(1.0, 0.0, 0.0)), P(0.0, 1.0, 0.0))
    else unit(cross(normal, P(0.0, 1.0, 0.0)), P(0.0, 0.0, 1.0))
  private def close(a: Double, b: Double): Boolean =
    math.abs(a - b) <= 1e-9 * math.max(1.0, math.max(math.abs(a), math.abs(b)))
  private def basis(camera: SurfaceCameraPacket): (P, P, P) =
    val right = unit(P(camera.viewMatrix(0), camera.viewMatrix(1), camera.viewMatrix(2)), P(1.0, 0.0, 0.0))
    val up = unit(P(camera.viewMatrix(4), camera.viewMatrix(5), camera.viewMatrix(6)), P(0.0, 1.0, 0.0))
    (right, up, unit(cross(right, up), P(camera.directionX, camera.directionY, camera.directionZ)))
  private def put(buffer: Array[Float], index: Int, point: P): Unit =
    val offset = index * 3
    buffer(offset) = point.x.toFloat
    buffer(offset + 1) = point.y.toFloat
    buffer(offset + 2) = point.z.toFloat
  private def mul(matrix: FloatBufferView, point: P, w: Double): (Double, Double, Double, Double) =
    (matrix(0) * point.x + matrix(1) * point.y + matrix(2) * point.z + matrix(3) * w,
      matrix(4) * point.x + matrix(5) * point.y + matrix(6) * point.z + matrix(7) * w,
      matrix(8) * point.x + matrix(9) * point.y + matrix(10) * point.z + matrix(11) * w,
      matrix(12) * point.x + matrix(13) * point.y + matrix(14) * point.z + matrix(15) * w)
