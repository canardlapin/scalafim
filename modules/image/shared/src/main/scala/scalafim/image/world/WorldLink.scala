package scalafim.image.world

import image4s.geometry.{D3, Frame, FrameAlignment, GeometryError, Point}
import reframe4s.core.{MapError, SpatialMap}

/** Which way a point crosses a [[WorldLink]]. */
enum LinkDirection derives CanEqual:
  /** From the link's left frame to its right frame. */
  case LeftToRight

  /** From the link's right frame to its left frame. */
  case RightToLeft

  def reverse: LinkDirection =
    this match
      case LeftToRight => RightToLeft
      case RightToLeft => LeftToRight

/** Failures of carrying a point across a [[WorldLink]]. */
enum WorldLinkError derives CanEqual:
  /** The two frames are different worlds and nothing supplied the map in this direction, for example a dense warp read
    * without its inverse. The point is not approximated.
    */
  case DirectionUnavailable(direction: LinkDirection, link: String)

  /** A link's maps were constructed with neither direction. */
  case NoDirection(link: String)

  /** The map exists but failed at this point (outside a field's support, a rejected boundary). */
  case Map(direction: LinkDirection, error: MapError)

  /** A point or map does not belong to the frame the link expects. */
  case FrameMismatch(error: GeometryError)

  def message: String =
    this match
      case DirectionUnavailable(direction, link) =>
        s"$link has no ${WorldLinkError.describe(direction)} map; it is not approximated"
      case NoDirection(link)       => s"$link supplies no map in either direction"
      case Map(direction, error)   => s"${WorldLinkError.describe(direction)} map failed: ${error.message}"
      case FrameMismatch(error)    => s"point is not in the link's frame: ${error.message}"

object WorldLinkError:
  private def describe(direction: LinkDirection): String =
    direction match
      case LinkDirection.LeftToRight => "left-to-right"
      case LinkDirection.RightToLeft => "right-to-left"

/** How points of the world frame `L` correspond to points of the world frame `R`: the typed map behind a linked cursor.
  *
  * A link is either [[WorldLink.Shared]], when both frames are one world and coordinates carry over unchanged under a
  * checked alignment, or [[WorldLink.Mapped]], when the frames are different worlds joined by typed maps (for example a
  * `WorldTransform` from the `transform` module). A mapped link always knows which directions exist: a direction nothing
  * supplied is [[WorldLinkError.DirectionUnavailable]], never an approximation.
  *
  * Points may belong to any runtime owner of a link frame's persistent key; they are rebound to the link's own frames
  * before a map runs, and results are returned as points of the link's frames.
  */
sealed trait WorldLink[L <: Frame[D3], R <: Frame[D3]]:
  def left: L
  def right: R

  /** Whether a point can cross in `direction` at all (it can still fail at a particular point). */
  def supports(direction: LinkDirection): Boolean

  def toRight(point: Point[L, D3]): Either[WorldLinkError, Point[R, D3]]

  def toLeft(point: Point[R, D3]): Either[WorldLinkError, Point[L, D3]]

  /** The same correspondence read the other way. */
  def swap: WorldLink[R, L]

object WorldLink:
  /** Both frames are one world (one runtime owner or one persistent key): coordinates carry over unchanged. */
  final class Shared[L <: Frame[D3], R <: Frame[D3]] private[WorldLink] (val alignment: FrameAlignment[D3, L, R])
      extends WorldLink[L, R]:
    def left: L = alignment.left
    def right: R = alignment.right
    def supports(direction: LinkDirection): Boolean = true

    def toRight(point: Point[L, D3]): Either[WorldLinkError, Point[R, D3]] =
      for
        owned <- own(point, left)
        moved <- alignment.pointToRight(owned).left.map(WorldLinkError.FrameMismatch.apply)
      yield moved

    def toLeft(point: Point[R, D3]): Either[WorldLinkError, Point[L, D3]] =
      for
        owned <- own(point, right)
        moved <- alignment.pointToLeft(owned).left.map(WorldLinkError.FrameMismatch.apply)
      yield moved

    def swap: Shared[R, L] =
      new Shared(
        Frame
          .alignOwners[D3, R, L](alignment.right, alignment.left)
          .fold(error => throw new IllegalStateException(s"an alignment failed to reverse: ${error.message}"), identity)
      )

    override def toString: String = s"WorldLink.Shared($left, $right)"

  /** Different worlds joined by typed maps; at least one direction exists. */
  final class Mapped[L <: Frame[D3], R <: Frame[D3]] private[WorldLink] (
      val left: L,
      val right: R,
      private val rightward: Option[SpatialMap[L, R, D3]],
      private val leftward: Option[SpatialMap[R, L, D3]],
      val description: String
  ) extends WorldLink[L, R]:
    def supports(direction: LinkDirection): Boolean =
      direction match
        case LinkDirection.LeftToRight => rightward.nonEmpty
        case LinkDirection.RightToLeft => leftward.nonEmpty

    def toRight(point: Point[L, D3]): Either[WorldLinkError, Point[R, D3]] =
      cross(point, rightward, LinkDirection.LeftToRight, right)

    def toLeft(point: Point[R, D3]): Either[WorldLinkError, Point[L, D3]] =
      cross(point, leftward, LinkDirection.RightToLeft, left)

    def swap: Mapped[R, L] = new Mapped(right, left, leftward, rightward, description)

    private def cross[A <: Frame[D3], B <: Frame[D3]](
        point: Point[A, D3],
        map: Option[SpatialMap[A, B, D3]],
        direction: LinkDirection,
        endpoint: B
    ): Either[WorldLinkError, Point[B, D3]] =
      map match
        case None => Left(WorldLinkError.DirectionUnavailable(direction, description))
        case Some(f) =>
          for
            owned <- own(point, f.source)
            mapped <- f(owned).left.map(WorldLinkError.Map(direction, _))
            result <- own(mapped, endpoint)
          yield result

    override def toString: String = s"WorldLink.Mapped($left, $right, $description)"

  /** Link two frames of one world; fails when they name different worlds. */
  def shared[L <: Frame[D3], R <: Frame[D3]](left: L, right: R): Either[WorldLinkError, Shared[L, R]] =
    Frame.alignOwners[D3, L, R](left, right).left.map(WorldLinkError.FrameMismatch.apply).map(aligned)

  /** Link two frames through an alignment already checked. */
  def aligned[L <: Frame[D3], R <: Frame[D3]](alignment: FrameAlignment[D3, L, R]): Shared[L, R] =
    new Shared(alignment)

  /** Link through a pullback `R -> L`, which always exists, and a forward map `L -> R` when one is known.
    *
    * This is the shape of a transform from `L` (source) to `R` (target): the pullback carries target points to source
    * points, and the forward direction exists only when something supplied it.
    */
  def pullback[L <: Frame[D3], R <: Frame[D3]](
      pull: SpatialMap[R, L, D3],
      push: Option[SpatialMap[L, R, D3]],
      description: String
  ): Either[WorldLinkError, Mapped[L, R]] =
    mapped(pull.target, pull.source, push, Some(pull), description)

  /** Link `left` and `right` through whichever maps exist; at least one must, and each must join the two frames. */
  def mapped[L <: Frame[D3], R <: Frame[D3]](
      left: L,
      right: R,
      toRight: Option[SpatialMap[L, R, D3]],
      toLeft: Option[SpatialMap[R, L, D3]],
      description: String
  ): Either[WorldLinkError, Mapped[L, R]] =
    val label = if description.trim.isEmpty then s"link $left -> $right" else description.trim
    def joins[A <: Frame[D3], B <: Frame[D3]](map: SpatialMap[A, B, D3], from: A, to: B): Either[WorldLinkError, Unit] =
      for
        _ <- Frame.alignOwners[D3, A, A](map.source, from).left.map(WorldLinkError.FrameMismatch.apply)
        _ <- Frame.alignOwners[D3, B, B](map.target, to).left.map(WorldLinkError.FrameMismatch.apply)
      yield ()
    if toRight.isEmpty && toLeft.isEmpty then Left(WorldLinkError.NoDirection(label))
    else
      for
        _ <- toRight.fold(Right(()))(joins(_, left, right))
        _ <- toLeft.fold(Right(()))(joins(_, right, left))
      yield new Mapped(left, right, toRight, toLeft, label)

  /** Rebind a point to `owner` when it belongs to any runtime owner of `owner`'s persistent key. */
  private def own[F <: Frame[D3]](point: Point[F, D3], owner: F): Either[WorldLinkError, Point[F, D3]] =
    if point.belongsTo(owner) then Right(point)
    else
      Frame
        .alignOwners[D3, F, F](point.frame, owner)
        .flatMap(_.pointToRight(point))
        .left
        .map(WorldLinkError.FrameMismatch.apply)
