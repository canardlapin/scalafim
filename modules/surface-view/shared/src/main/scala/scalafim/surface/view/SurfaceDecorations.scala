package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.{FaceId, VertexId}

opaque type SurfaceDecorationId = String
object SurfaceDecorationId:
  def make(value: String): Either[SurfaceDecorationError, SurfaceDecorationId] =
    if value.trim.nonEmpty then Right(value) else Left(SurfaceDecorationError.BlankId)
  extension (value: SurfaceDecorationId) def value: String = value

enum SurfaceDecorationError:
  case BlankId
  case NetworkMapping(reason: String)
  case StalePackets
  case DuplicateId(id: SurfaceDecorationId)
  case UnknownSurface(surface: SurfaceId)
  case UnknownDisplay(origin: SurfaceDisplayOrigin)
  case VertexOutOfRange(surface: SurfaceId, vertex: VertexId)
  case FaceOutOfRange(surface: SurfaceId, face: FaceId)
  case InvalidBarycentric
  case InvalidSize(value: Double)
  case InvalidProjection
  case AnisotropicPixelSphere
  case EmptyText
  case LabelCount(surface: SurfaceId, expected: Int, actual: Int)

final case class SurfaceVertexAnchor private (surface: SurfaceId, vertex: VertexId)
object SurfaceVertexAnchor:
  def make(surface: SurfaceId, vertex: VertexId): SurfaceVertexAnchor = SurfaceVertexAnchor(surface, vertex)

final case class SurfaceFaceAnchor private (surface: SurfaceId, face: FaceId, a: Double, b: Double, c: Double):
  require(a.isFinite && b.isFinite && c.isFinite && a >= 0.0 && b >= 0.0 && c >= 0.0 &&
    math.abs(a + b + c - 1.0) <= 1e-9, "face anchor weights must be finite non-negative barycentric coordinates")
object SurfaceFaceAnchor:
  def make(surface: SurfaceId, face: FaceId, a: Double, b: Double, c: Double): Either[SurfaceDecorationError, SurfaceFaceAnchor] =
    if !a.isFinite || !b.isFinite || !c.isFinite || a < 0.0 || b < 0.0 || c < 0.0 || math.abs(a + b + c - 1.0) > 1e-9 then
      Left(SurfaceDecorationError.InvalidBarycentric)
    else Right(SurfaceFaceAnchor(surface, face, a, b, c))

enum SurfaceDecorationAnchor:
  case Vertex(value: SurfaceVertexAnchor)
  case Face(value: SurfaceFaceAnchor)

object SurfaceDecorationAnchor:
  def surface(anchor: SurfaceDecorationAnchor): SurfaceId = anchor match
    case Vertex(value) => value.surface
    case Face(value) => value.surface

enum SurfaceMarkerShape:
  case Disk, Sphere, Crosshair

enum SurfaceDecorationUnit:
  case Pixels, World

enum SurfaceDecorationDepthPolicy:
  case DepthTest, Overlay

final case class SurfaceDecorationSize private (value: Double, unit: SurfaceDecorationUnit):
  require(value.isFinite && value > 0.0, "decoration size must be finite and positive")
object SurfaceDecorationSize:
  def pixels(value: Double): Either[SurfaceDecorationError, SurfaceDecorationSize] = make(value, SurfaceDecorationUnit.Pixels)
  def world(value: Double): Either[SurfaceDecorationError, SurfaceDecorationSize] = make(value, SurfaceDecorationUnit.World)
  private def make(value: Double, unit: SurfaceDecorationUnit): Either[SurfaceDecorationError, SurfaceDecorationSize] =
    if !value.isFinite || value <= 0.0 then Left(SurfaceDecorationError.InvalidSize(value))
    else Right(SurfaceDecorationSize(value, unit))

final case class SurfaceContourHalo(color: Rgba32, width: SurfaceDecorationSize)
/** Widths are full visible stroke widths. A halo is an equally wide band split
  * around the core, so it must share the core's unit. */
final case class SurfaceContourStyle(width: SurfaceDecorationSize, color: Rgba32, halo: Option[SurfaceContourHalo] = None,
    depthPolicy: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest):
  require(halo.forall(_.width.unit == width.unit), "contour and halo widths must use the same unit")

/** Label values are scientific vertex labels. `Ignore` has no inferred medial-wall
  * meaning: callers must name every ignored label explicitly.
  */
enum SurfaceContourPolicy:
  case Transitions
  case RoiExterior(selected: Set[Int])
  case Ignore(labels: Set[Int], underlying: SurfaceContourPolicy)

sealed trait SurfaceDecoration:
  def id: SurfaceDecorationId
  def surface: SurfaceId

final case class SurfaceMarkerDecoration(
  id: SurfaceDecorationId,
  anchor: SurfaceDecorationAnchor,
  shape: SurfaceMarkerShape,
  size: SurfaceDecorationSize,
  color: Rgba32,
  depthPolicy: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest
) extends SurfaceDecoration:
  def surface: SurfaceId = SurfaceDecorationAnchor.surface(anchor)

final case class SurfaceLabelDecoration(
  id: SurfaceDecorationId,
  anchor: SurfaceDecorationAnchor,
  text: String,
  size: SurfaceDecorationSize,
  color: Rgba32,
  depthPolicy: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest
) extends SurfaceDecoration:
  def surface: SurfaceId = SurfaceDecorationAnchor.surface(anchor)

object SurfaceLabelDecoration:
  def make(id: SurfaceDecorationId, anchor: SurfaceDecorationAnchor, text: String,
      size: SurfaceDecorationSize, color: Rgba32,
      depthPolicy: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest): Either[SurfaceDecorationError, SurfaceLabelDecoration] =
    if text.trim.isEmpty then Left(SurfaceDecorationError.EmptyText)
    else Right(SurfaceLabelDecoration(id, anchor, text, size, color, depthPolicy))

final case class SurfaceContourDecoration(
  id: SurfaceDecorationId,
  surface: SurfaceId,
  vertexLabels: Vector[Int],
  policy: SurfaceContourPolicy,
  style: SurfaceContourStyle
) extends SurfaceDecoration

final case class SurfaceDecorationWorldPoint(x: Double, y: Double, z: Double):
  require(x.isFinite && y.isFinite && z.isFinite, "decoration point coordinates must be finite")

final case class SurfaceDecorationElementId(decoration: SurfaceDecorationId, suffix: String):
  require(suffix.trim.nonEmpty, "decoration element suffix must not be blank")

final case class SurfaceMarkerPacket(
  id: SurfaceDecorationElementId,
  origin: SurfaceDisplayOrigin,
  anchor: SurfaceDecorationAnchor,
  point: SurfaceDecorationWorldPoint,
  shape: SurfaceMarkerShape,
  size: SurfaceDecorationSize,
  color: Rgba32,
  depthPolicy: SurfaceDecorationDepthPolicy
)

final case class SurfaceLabelPacket(
  id: SurfaceDecorationElementId,
  origin: SurfaceDisplayOrigin,
  anchor: SurfaceDecorationAnchor,
  point: SurfaceDecorationWorldPoint,
  text: String,
  size: SurfaceDecorationSize,
  color: Rgba32,
  depthPolicy: SurfaceDecorationDepthPolicy = SurfaceDecorationDepthPolicy.DepthTest
):
  require(text.trim.nonEmpty, "label packet text must not be blank")

final case class SurfaceContourPacket(
  id: SurfaceDecorationElementId,
  origin: SurfaceDisplayOrigin,
  surface: SurfaceId,
  sourceEdge: (VertexId, VertexId),
  start: SurfaceDecorationWorldPoint,
  end: SurfaceDecorationWorldPoint,
  style: SurfaceContourStyle
)

final case class SurfaceDecorationPackets(
  markers: Vector[SurfaceMarkerPacket],
  labels: Vector[SurfaceLabelPacket],
  contours: Vector[SurfaceContourPacket],
  binding: String = ""
)

/** Lowers scientific anchors onto every display occurrence in the current
  * composition. Coordinates always come from the current render mesh; source
  * vertex and face provenance only select their scientific owner.
  */
object SurfaceDecorationCompiler:
  def compile(decorations: Vector[SurfaceDecoration], plan: SurfaceCompositionPlan): Either[SurfaceDecorationError, SurfaceDecorationPackets] =
    duplicate(decorations.map(_.id)) match
      case Some(id) => Left(SurfaceDecorationError.DuplicateId(id))
      case None =>
        val markers = Vector.newBuilder[SurfaceMarkerPacket]
        val labels = Vector.newBuilder[SurfaceLabelPacket]
        val contours = Vector.newBuilder[SurfaceContourPacket]
        var error: Option[SurfaceDecorationError] = None
        var index = 0
        while index < decorations.length && error.isEmpty do
          process(decorations(index), plan, markers, labels, contours) match
            case Left(value) => error = Some(value)
            case Right(_) => ()
          index += 1
        error.fold[Either[SurfaceDecorationError, SurfaceDecorationPackets]](
          Right(SurfaceDecorationPackets(markers.result(), labels.result(), contours.result(), SurfaceDecorationBinding.key(plan, 1, 1))))(Left(_))

  private def process(decoration: SurfaceDecoration, plan: SurfaceCompositionPlan,
      markers: scala.collection.mutable.Builder[SurfaceMarkerPacket, Vector[SurfaceMarkerPacket]],
      labels: scala.collection.mutable.Builder[SurfaceLabelPacket, Vector[SurfaceLabelPacket]],
      contours: scala.collection.mutable.Builder[SurfaceContourPacket, Vector[SurfaceContourPacket]]): Either[SurfaceDecorationError, Unit] =
    decoration match
      case label: SurfaceLabelDecoration if label.text.trim.isEmpty => Left(SurfaceDecorationError.EmptyText)
      case _ =>
        val occurrences = plan.views.flatMap(view => view.instances.collect:
          case instance if instance.surface == decoration.surface => view -> instance)
        if occurrences.isEmpty then Left(SurfaceDecorationError.UnknownSurface(decoration.surface))
        else
          var error: Option[SurfaceDecorationError] = None
          var index = 0
          while index < occurrences.length && error.isEmpty do
            val (view, instance) = occurrences(index)
            val mesh = plan.resources.meshes(instance.mesh)
            decoration match
              case marker: SurfaceMarkerDecoration =>
                resolve(marker.anchor, mesh) match
                  case Left(value) => error = Some(value)
                  case Right(point) => markers += SurfaceMarkerPacket(SurfaceDecorationElementId(marker.id, "marker"),
                    SurfaceDisplayOrigin(view.id, instance.id), marker.anchor, translated(point, instance), marker.shape, marker.size, marker.color, marker.depthPolicy)
              case label: SurfaceLabelDecoration =>
                resolve(label.anchor, mesh) match
                  case Left(value) => error = Some(value)
                  case Right(point) => labels += SurfaceLabelPacket(SurfaceDecorationElementId(label.id, "label"),
                    SurfaceDisplayOrigin(view.id, instance.id), label.anchor, translated(point, instance), label.text, label.size, label.color, label.depthPolicy)
              case contour: SurfaceContourDecoration =>
                contourEdges(contour, mesh) match
                  case Left(value) => error = Some(value)
                  case Right(edges) =>
                    var edge = 0
                    while edge < edges.length && error.isEmpty do
                      val (a, b) = edges(edge)
                      (vertexPoint(mesh, contour.surface, a), vertexPoint(mesh, contour.surface, b)) match
                        case (Right(start), Right(end)) => contours += SurfaceContourPacket(
                          SurfaceDecorationElementId(contour.id, s"edge-${a.index}-${b.index}"),
                          SurfaceDisplayOrigin(view.id, instance.id), contour.surface, a -> b,
                          translated(start, instance), translated(end, instance), contour.style)
                        case (Left(value), _) => error = Some(value)
                        case (_, Left(value)) => error = Some(value)
                      edge += 1
            index += 1
          error.fold[Either[SurfaceDecorationError, Unit]](Right(()))(Left(_))

  private[view] def resolve(anchor: SurfaceDecorationAnchor, mesh: SurfaceMeshPacket): Either[SurfaceDecorationError, SurfaceDecorationWorldPoint] = anchor match
    case SurfaceDecorationAnchor.Vertex(value) => vertexPoint(mesh, value.surface, value.vertex)
    case SurfaceDecorationAnchor.Face(value) =>
      val faces = mesh.indices.length / 3
      val renderFace = (0 until faces).find(mesh.sourceFace(_) == value.face.index)
      renderFace match
        case None => Left(SurfaceDecorationError.FaceOutOfRange(value.surface, value.face))
        case Some(face) =>
          val (a, b, c) = mesh.sourceFaceVertices(face)
          for
            pa <- vertexPoint(mesh, value.surface, VertexId(a))
            pb <- vertexPoint(mesh, value.surface, VertexId(b))
            pc <- vertexPoint(mesh, value.surface, VertexId(c))
          yield SurfaceDecorationWorldPoint(
            value.a * pa.x + value.b * pb.x + value.c * pc.x,
            value.a * pa.y + value.b * pb.y + value.c * pc.y,
            value.a * pa.z + value.b * pb.z + value.c * pc.z)

  private def contourEdges(decoration: SurfaceContourDecoration, mesh: SurfaceMeshPacket): Either[SurfaceDecorationError, Vector[(VertexId, VertexId)]] =
    val sourceCount = mesh.samplePositions.map(_.length / 3)
      .orElse(mesh.sampleNormals.map(_.length / 3))
      .getOrElse(mesh.sourceVertices.fold(mesh.positions.length / 3)(values =>
        (0 until values.length).map(values(_)).maxOption.fold(0)(_ + 1)))
    if decoration.vertexLabels.length != sourceCount then
      Left(SurfaceDecorationError.LabelCount(decoration.surface, sourceCount, decoration.vertexLabels.length))
    else
      val edges = scala.collection.mutable.LinkedHashSet.empty[(VertexId, VertexId)]
      var face = 0
      while face < mesh.indices.length / 3 do
        val (x, y, z) = mesh.sourceFaceVertices(face)
        addEdge(VertexId(x), VertexId(y), decoration.vertexLabels, decoration.policy, edges)
        addEdge(VertexId(y), VertexId(z), decoration.vertexLabels, decoration.policy, edges)
        addEdge(VertexId(z), VertexId(x), decoration.vertexLabels, decoration.policy, edges)
        face += 1
      Right(edges.toVector)

  private def addEdge(a: VertexId, b: VertexId, labels: Vector[Int], policy: SurfaceContourPolicy,
      target: scala.collection.mutable.LinkedHashSet[(VertexId, VertexId)]): Unit =
    val left = labels(a.index)
    val right = labels(b.index)
    if include(left, right, policy) then
      target += (if a.index <= b.index then a -> b else b -> a)

  private def include(left: Int, right: Int, policy: SurfaceContourPolicy): Boolean = policy match
    case SurfaceContourPolicy.Transitions => left != right
    case SurfaceContourPolicy.RoiExterior(selected) => selected(left) != selected(right)
    case SurfaceContourPolicy.Ignore(values, underlying) => !values(left) && !values(right) && include(left, right, underlying)

  private def vertexPoint(mesh: SurfaceMeshPacket, surface: SurfaceId, vertex: VertexId): Either[SurfaceDecorationError, SurfaceDecorationWorldPoint] =
    mesh.samplePositions match
      case Some(samples) if mesh.sourceVertices.nonEmpty && vertex.index * 3 + 2 < samples.length =>
        Right(SurfaceDecorationWorldPoint(samples(vertex.index * 3), samples(vertex.index * 3 + 1), samples(vertex.index * 3 + 2)))
      case _ =>
        val render = sourceRenderVertex(mesh, vertex)
        render match
          case None => Left(SurfaceDecorationError.VertexOutOfRange(surface, vertex))
          case Some(index) => Right(SurfaceDecorationWorldPoint(mesh.positions(index * 3), mesh.positions(index * 3 + 1), mesh.positions(index * 3 + 2)))

  /** Nearest-partition output contains generated points attributed to a source
    * vertex. Prefer its exact corner, never the first generated subtriangle
    * point carrying that source id.
    */
  private def sourceRenderVertex(mesh: SurfaceMeshPacket, vertex: VertexId): Option[Int] =
    var first: Option[Int] = None
    var exact: Option[Int] = None
    var index = 0
    while index < mesh.positions.length / 3 && exact.isEmpty do
      if mesh.sourceVertex(index) == vertex.index then
        if first.isEmpty then first = Some(index)
        mesh.nearestPartition match
          case Some(partition) =>
            val (a, b, c) = partition.weights(index)
            if a == 1.0 || b == 1.0 || c == 1.0 then exact = Some(index)
          case None => exact = Some(index)
      index += 1
    exact.orElse(first)

  private def translated(point: SurfaceDecorationWorldPoint, instance: SurfaceCompositionInstance): SurfaceDecorationWorldPoint =
    SurfaceDecorationWorldPoint(point.x + instance.translationX, point.y + instance.translationY, point.z + instance.translationZ)

  private def duplicate[A](values: Vector[A]): Option[A] = values.find(value => values.count(_ == value) > 1)
