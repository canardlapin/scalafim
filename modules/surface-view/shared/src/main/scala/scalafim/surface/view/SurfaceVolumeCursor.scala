package scalafim.surface.view

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{WorldLink, WorldLinkError}
import scalafim.surface.{FramedSurface, SurfaceFrameError, VertexId}

/** Failures of linking a surface vertex cursor with a volume cursor. */
enum SurfaceCursorError derives CanEqual:
  /** The typed map between the surface's and the volume's frames failed, or lacks the direction needed. */
  case Link(error: WorldLinkError)
  case Surface(error: SurfaceFrameError)

  /** A selection on another surface than the one this cursor links. */
  case WrongSurface(expected: SurfaceId, actual: SurfaceId)

  /** The linked point is farther than the permitted radius from every vertex. */
  case BeyondRadius(distance: Double, maximum: Double)

  def message: String =
    this match
      case Link(error)                   => s"linked cursor: ${error.message}"
      case Surface(error)                => s"linked cursor: ${error.message}"
      case WrongSurface(expected, actual) => s"linked cursor belongs to surface ${expected.value}, not ${actual.value}"
      case BeyondRadius(distance, maximum) =>
        s"linked point is $distance from the nearest vertex, beyond the permitted $maximum"

/** The surface vertex nearest a linked volume point: the selection, its placed position, and its distance from the
  * linked point (in the surface frame's units).
  */
final case class SurfaceCursorHit[S <: Frame[D3]](selection: SurfaceSelection, vertex: Point[S, D3], distance: Double)

/** A cursor linking one displayed surface, in frame `S`, with a volume in frame `V`.
  *
  * Every crossing goes through the typed [[scalafim.image.world.WorldLink]]:
  *   - when the surface and the volume are one world, a `WorldLink.Shared` alignment carries coordinates over unchanged;
  *   - when they are different worlds (a surface in subject tkRAS or scanner space and a volume in MNI, for example),
  *     a `WorldLink.Mapped` carries them, typically a `WorldTransform`'s `link` from the `transform` module. A direction
  *     the transform does not supply (the forward map of a dense warp read without its inverse) is
  *     [[SurfaceCursorError.Link]] with `DirectionUnavailable`, never an approximation.
  *
  * Volume points find their vertex by exhaustive nearest-vertex search over the placed coordinates, which are copied
  * once when the cursor is made.
  */
final class SurfaceVolumeCursor[S <: Frame[D3], V <: Frame[D3]] private (
    val surfaceId: SurfaceId,
    val surface: FramedSurface[S],
    val link: WorldLink[S, V]
):
  private val placed: IArray[Double] = surface.coordinates

  /** The volume point linked to a selected vertex of this cursor's surface. */
  def toVolume(selection: SurfaceSelection): Either[SurfaceCursorError, Point[V, D3]] =
    for
      _ <- ownSelection(selection)
      point <- surface.vertex(selection.vertex).left.map(SurfaceCursorError.Surface.apply)
      linked <- link.toRight(point).left.map(SurfaceCursorError.Link.apply)
    yield linked

  /** The vertex nearest the surface point linked to a volume point, if it lies within `maximumDistance`. Ties go to the
    * lower vertex index.
    */
  def toSurface(point: Point[V, D3], maximumDistance: SurfaceLinkRadius): Either[SurfaceCursorError, SurfaceCursorHit[S]] =
    for
      linked <- link.toLeft(point).left.map(SurfaceCursorError.Link.apply)
      (vertex, distance) = nearest(linked.coordinates)
      _ <-
        if distance <= maximumDistance.value then Right(())
        else Left(SurfaceCursorError.BeyondRadius(distance, maximumDistance.value))
      id = VertexId.unsafe(vertex)
      position <- surface.vertex(id).left.map(SurfaceCursorError.Surface.apply)
    yield SurfaceCursorHit(SurfaceSelection(surfaceId, id), position, distance)

  private def ownSelection(selection: SurfaceSelection): Either[SurfaceCursorError, Unit] =
    if selection.surface == surfaceId then Right(()) else Left(SurfaceCursorError.WrongSurface(surfaceId, selection.surface))

  private def nearest(query: Vector[Double]): (Int, Double) =
    val (x, y, z) = (query(0), query(1), query(2))
    var best = 0
    var bestSquared = Double.PositiveInfinity
    var vertex = 0
    val count = placed.length / 3
    while vertex < count do
      val dx = placed(3 * vertex) - x
      val dy = placed(3 * vertex + 1) - y
      val dz = placed(3 * vertex + 2) - z
      val squared = dx * dx + dy * dy + dz * dz
      if squared < bestSquared then
        bestSquared = squared
        best = vertex
      vertex += 1
    (best, math.sqrt(bestSquared))

  override def toString: String = s"SurfaceVolumeCursor(${surfaceId.value}, $link)"

object SurfaceVolumeCursor:
  /** Link `surface`, shown as `surfaceId`, to a volume through `link`, whose left frame must be the surface's frame (one
    * runtime owner or one persistent key).
    */
  def make[S <: Frame[D3], V <: Frame[D3]](
      surfaceId: SurfaceId,
      surface: FramedSurface[S],
      link: WorldLink[S, V]
  ): Either[SurfaceCursorError, SurfaceVolumeCursor[S, V]] =
    Frame
      .alignOwners[D3, S, S](surface.frame, link.left)
      .left
      .map(error => SurfaceCursorError.Link(WorldLinkError.FrameMismatch(error)))
      .map(_ => new SurfaceVolumeCursor(surfaceId, surface, link))

  /** Link the surface a viewer displays, typed by its [[SurfaceDisplayFrame]], so hits can also focus a
    * [[SurfaceCameraPose]] in the same frame.
    */
  def forDisplay[S <: Frame[D3], V <: Frame[D3]](
      display: SurfaceDisplayFrame[S],
      link: WorldLink[S, V]
  ): Either[SurfaceCursorError, SurfaceVolumeCursor[S, V]] =
    make(display.surfaceId, display.surface, link)
