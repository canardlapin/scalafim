package scalafim.surface.view

import scalafim.surface.VertexId

enum SurfaceFlatMapError:
  case InvalidGeometry(reason: String)
  case InvalidViewport(reason: String)
  case VertexOutside(vertex: VertexId, count: Int)
  case InvalidPolygon(reason: String)
  case InvalidRoi(reason: String)

final case class SurfaceFlatMapPoint(x: Double, y: Double):
  require(x.isFinite && y.isFinite, "flat-map points must be finite")

enum SurfaceFlatMapFit:
  case Contain, Stretch

final case class SurfaceFlatMapViewport private (width: Double, height: Double, padding: Double, fit: SurfaceFlatMapFit):
  require(width.isFinite && height.isFinite && padding.isFinite && padding >= 0 && width > 2 * padding && height > 2 * padding)

object SurfaceFlatMapViewport:
  def make(width: Double, height: Double, padding: Double = 0.0,
      fit: SurfaceFlatMapFit = SurfaceFlatMapFit.Contain): Either[SurfaceFlatMapError, SurfaceFlatMapViewport] =
    if !width.isFinite || !height.isFinite || !padding.isFinite || padding < 0.0 || width <= 2 * padding || height <= 2 * padding then
      Left(SurfaceFlatMapError.InvalidViewport("viewport dimensions must be finite with positive drawable extent"))
    else Right(SurfaceFlatMapViewport(width, height, padding, fit))

final case class SurfaceFlatMapRoi private (id: String, surface: SurfaceId, vertices: Vector[VertexId], outline: Vector[SurfaceFlatMapPoint]):
  require(id.trim.nonEmpty && vertices.nonEmpty)

object SurfaceFlatMapRoi:
  def make(id: String, map: SurfaceFlatMap, vertices: Vector[VertexId],
      outline: Vector[SurfaceFlatMapPoint]): Either[SurfaceFlatMapError, SurfaceFlatMapRoi] =
    if id.trim.isEmpty then Left(SurfaceFlatMapError.InvalidRoi("ROI id must not be blank"))
    else if vertices.isEmpty then Left(SurfaceFlatMapError.InvalidRoi("ROI must contain a vertex"))
    else vertices.find(vertex => vertex.index < 0 || vertex.index >= map.vertexCount) match
      case Some(vertex) => Left(SurfaceFlatMapError.VertexOutside(vertex, map.vertexCount))
      case None => Right(SurfaceFlatMapRoi(id.trim, map.surface, vertices.distinct.sortBy(_.index), outline))

enum SurfaceFlatMapPick:
  case Vertex(value: VertexId, distancePixels: Double)
  case Triangle(face: Int, a: Double, b: Double, c: Double)

/** Immutable 2D presentation with the original scientific vertex domain. */
final class SurfaceFlatMap private (
  val surface: SurfaceId,
  private val coordinates: Vector[SurfaceFlatMapPoint],
  val triangles: Vector[(VertexId, VertexId, VertexId)]
):
  private val minX = coordinates.map(_.x).min
  private val maxX = coordinates.map(_.x).max
  private val minY = coordinates.map(_.y).min
  private val maxY = coordinates.map(_.y).max
  val vertexCount: Int = coordinates.length

  def point(vertex: VertexId): Either[SurfaceFlatMapError, SurfaceFlatMapPoint] =
    if vertex.index < 0 || vertex.index >= vertexCount then Left(SurfaceFlatMapError.VertexOutside(vertex, vertexCount))
    else Right(coordinates(vertex.index))

  def project(vertex: VertexId, viewport: SurfaceFlatMapViewport): Either[SurfaceFlatMapError, SurfaceFlatMapPoint] =
    point(vertex).map(projectPoint(_, viewport))

  /** Projects a flat-map-domain point to the viewport. This is deliberately
    * separate from [[project]] so ROI outlines retain map coordinates and stay
    * geometrically stable when the Canvas is resized. */
  def projectMapPoint(point: SurfaceFlatMapPoint, viewport: SurfaceFlatMapViewport): SurfaceFlatMapPoint =
    projectPoint(point, viewport)

  def unproject(point: SurfaceFlatMapPoint, viewport: SurfaceFlatMapViewport): SurfaceFlatMapPoint =
    val (sx, sy, ox, oy) = transform(viewport)
    SurfaceFlatMapPoint((point.x - ox) / sx + minX, maxY - (point.y - oy) / sy)

  def closestVertex(point: SurfaceFlatMapPoint, viewport: SurfaceFlatMapViewport,
      radiusPixels: Double): Either[SurfaceFlatMapError, Option[SurfaceFlatMapPick.Vertex]] =
    if !radiusPixels.isFinite || radiusPixels <= 0.0 then Left(SurfaceFlatMapError.InvalidViewport("pick radius must be positive and finite"))
    else
      var best = -1
      var distance = Double.PositiveInfinity
      var index = 0
      while index < vertexCount do
        val p = projectPoint(coordinates(index), viewport)
        val d = math.hypot(point.x - p.x, point.y - p.y)
        if d <= distance then
          best = index
          distance = d
        index += 1
      if distance <= radiusPixels then Right(Some(SurfaceFlatMapPick.Vertex(VertexId(best), distance))) else Right(None)

  def triangleAt(point: SurfaceFlatMapPoint, viewport: SurfaceFlatMapViewport): Option[SurfaceFlatMapPick.Triangle] =
    var face = 0
    while face < triangles.length do
      val (ia, ib, ic) = triangles(face)
      val a = projectPoint(coordinates(ia.index), viewport)
      val b = projectPoint(coordinates(ib.index), viewport)
      val c = projectPoint(coordinates(ic.index), viewport)
      val area = cross(a, b, c)
      if math.abs(area) > 1e-12 then
        val wa = cross(point, b, c) / area
        val wb = cross(point, c, a) / area
        val wc = 1.0 - wa - wb
        if wa >= -1e-12 && wb >= -1e-12 && wc >= -1e-12 then return Some(SurfaceFlatMapPick.Triangle(face, wa, wb, wc))
      face += 1
    None

  /** Closed boundaries count as selected, matching the reference ROI tool. */
  def verticesInPolygon(polygon: Vector[SurfaceFlatMapPoint], viewport: SurfaceFlatMapViewport): Either[SurfaceFlatMapError, Vector[VertexId]] =
    if polygon.length < 3 then Left(SurfaceFlatMapError.InvalidPolygon("polygon requires at least three points"))
    else if polygon.exists(point => !point.x.isFinite || !point.y.isFinite) then Left(SurfaceFlatMapError.InvalidPolygon("polygon points must be finite"))
    else Right(Vector.tabulate(vertexCount)(VertexId(_)).filter(vertex => contains(projectPoint(coordinates(vertex.index), viewport), polygon)))

  private def projectPoint(point: SurfaceFlatMapPoint, viewport: SurfaceFlatMapViewport): SurfaceFlatMapPoint =
    val (sx, sy, ox, oy) = transform(viewport)
    SurfaceFlatMapPoint(ox + (point.x - minX) * sx, oy + (maxY - point.y) * sy)

  private def transform(viewport: SurfaceFlatMapViewport): (Double, Double, Double, Double) =
    val sourceWidth = math.max(maxX - minX, 1e-6)
    val sourceHeight = math.max(maxY - minY, 1e-6)
    val drawableWidth = viewport.width - 2 * viewport.padding
    val drawableHeight = viewport.height - 2 * viewport.padding
    val (sx, sy) = viewport.fit match
      case SurfaceFlatMapFit.Stretch => drawableWidth / sourceWidth -> drawableHeight / sourceHeight
      case SurfaceFlatMapFit.Contain =>
        val scale = math.min(drawableWidth / sourceWidth, drawableHeight / sourceHeight)
        scale -> scale
    val ox = viewport.padding + (drawableWidth - sourceWidth * sx) / 2
    val oy = viewport.padding + (drawableHeight - sourceHeight * sy) / 2
    (sx, sy, ox, oy)

  private def contains(point: SurfaceFlatMapPoint, polygon: Vector[SurfaceFlatMapPoint]): Boolean =
    var inside = false
    var previous = polygon.length - 1
    var index = 0
    while index < polygon.length do
      val a = polygon(previous)
      val b = polygon(index)
      if onSegment(point, a, b) then return true
      if (a.y > point.y) != (b.y > point.y) && point.x < (b.x - a.x) * (point.y - a.y) / (b.y - a.y) + a.x then inside = !inside
      previous = index
      index += 1
    inside

  private def onSegment(p: SurfaceFlatMapPoint, a: SurfaceFlatMapPoint, b: SurfaceFlatMapPoint): Boolean =
    math.abs(cross(a, b, p)) <= 1e-9 && p.x >= math.min(a.x, b.x) - 1e-9 && p.x <= math.max(a.x, b.x) + 1e-9 &&
      p.y >= math.min(a.y, b.y) - 1e-9 && p.y <= math.max(a.y, b.y) + 1e-9

  private def cross(a: SurfaceFlatMapPoint, b: SurfaceFlatMapPoint, c: SurfaceFlatMapPoint): Double =
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

object SurfaceFlatMap:
  def make(surface: SurfaceId, coordinates: Vector[SurfaceFlatMapPoint], faces: Vector[(Int, Int, Int)]): Either[SurfaceFlatMapError, SurfaceFlatMap] =
    if coordinates.isEmpty then Left(SurfaceFlatMapError.InvalidGeometry("flat map needs at least one vertex"))
    else faces.find(face => face._1 < 0 || face._2 < 0 || face._3 < 0 || face._1 >= coordinates.length || face._2 >= coordinates.length || face._3 >= coordinates.length) match
      case Some(_) => Left(SurfaceFlatMapError.InvalidGeometry("triangle index is outside the scientific vertex domain"))
      case None => Right(new SurfaceFlatMap(surface, coordinates, faces.map((a, b, c) => (VertexId(a), VertexId(b), VertexId(c)))))
