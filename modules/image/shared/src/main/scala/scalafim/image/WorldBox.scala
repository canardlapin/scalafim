package scalafim.image

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Point

/** An axis-aligned box in one world frame, bounded by two frame-owned corners with `min <= max` on every axis.
  *
  * This is the single world-space bounding box: grid extents, reslicing bounds and viewer camera framing all use it.
  * Voxel-index boxes (integer lattice extents such as a threshold region's) are a different quantity and are not
  * represented here.
  */
final class WorldBox[F <: Frame[D3]] private (val min: Point[F, D3], val max: Point[F, D3]):
  def frame: F = min.frame

  def center: Point[F, D3] =
    WorldBox.corner(min, (min.x + max.x) / 2.0, (min.y + max.y) / 2.0, (min.z + max.z) / 2.0)

  /** Length of the box diagonal in millimetres. */
  def diagonal: Double =
    val dx = max.x - min.x
    val dy = max.y - min.y
    val dz = max.z - min.z
    math.sqrt(dx * dx + dy * dy + dz * dz)

  def contains(point: Point[F, D3]): Boolean =
    point.x >= min.x && point.x <= max.x &&
      point.y >= min.y && point.y <= max.y &&
      point.z >= min.z && point.z <= max.z

  /** The smallest box enclosing both boxes; both are already in frame `F`. */
  def union(other: WorldBox[F]): WorldBox[F] =
    new WorldBox(
      WorldBox.corner(min, math.min(min.x, other.min.x), math.min(min.y, other.min.y), math.min(min.z, other.min.z)),
      WorldBox.corner(max, math.max(max.x, other.max.x), math.max(max.y, other.max.y), math.max(max.z, other.max.z))
    )

  override def equals(other: Any): Boolean =
    other match
      case that: WorldBox[?] =>
        frame.sameRuntimeOwnerAs(that.frame) && min.coordinates == that.min.coordinates &&
          max.coordinates == that.max.coordinates
      case _ => false

  override def hashCode(): Int =
    (min.coordinates, max.coordinates).hashCode()

  override def toString: String =
    s"WorldBox(${min.coordinates.mkString("(", ", ", ")")}, ${max.coordinates.mkString("(", ", ", ")")})"

object WorldBox:
  /** The box spanned by two corners of one frame, in either order. */
  def spanning[F <: Frame[D3]](first: Point[F, D3], second: Point[F, D3]): WorldBox[F] =
    enclosingNonEmpty(first, Iterator.single(second))

  /** The smallest box enclosing a non-empty collection of points of one frame. */
  def enclosing[F <: Frame[D3]](points: Iterable[Point[F, D3]]): Option[WorldBox[F]] =
    points.headOption.map(head => enclosingNonEmpty(head, points.iterator.drop(1)))

  /** A box from coordinate extrema in `frame`; rejects non-finite or inverted extrema. */
  def fromExtrema[F <: Frame[D3]](
      frame: F,
      minimum: WorldPoint,
      maximum: WorldPoint
  ): Either[GeometryError, WorldBox[F]] =
    if minimum.x > maximum.x || minimum.y > maximum.y || minimum.z > maximum.z then
      Left(GeometryError.InvalidGridGeometry(s"box minimum $minimum exceeds maximum $maximum"))
    else
      val corners =
        for
          low <- GridSpec.pointIn(frame, minimum.toVector)
          high <- GridSpec.pointIn(frame, maximum.toVector)
        yield new WorldBox(low, high)
      corners.left.map(error => GeometryError.InvalidGridGeometry(error.message))

  private def enclosingNonEmpty[F <: Frame[D3]](head: Point[F, D3], rest: Iterator[Point[F, D3]]): WorldBox[F] =
    var minX = head.x
    var minY = head.y
    var minZ = head.z
    var maxX = minX
    var maxY = minY
    var maxZ = minZ
    rest.foreach: point =>
      val values = point.coordinates
      minX = math.min(minX, values(0))
      minY = math.min(minY, values(1))
      minZ = math.min(minZ, values(2))
      maxX = math.max(maxX, values(0))
      maxY = math.max(maxY, values(1))
      maxZ = math.max(maxZ, values(2))
    new WorldBox(corner(head, minX, minY, minZ), corner(head, maxX, maxY, maxZ))

  /** A point in `like`'s frame; coordinates come from finite points, so construction cannot fail. */
  private[image] def corner[F <: Frame[D3]](like: Point[F, D3], x: Double, y: Double, z: Double): Point[F, D3] =
    GridSpec
      .pointIn(like.frame, Vector(x, y, z))
      .fold(error => throw new IllegalStateException(error.message), identity)
