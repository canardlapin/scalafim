package scalafim.image.world

import image4s.geometry.{D3, Frame, FrameAlignment, Point, Vec}

/** A frame-indexed value whose frame is known only at runtime: a dependent pair.
  *
  * The value's type mentions the package's own frame (`V[frame.type]`), so a value cannot be packaged with a frame it
  * does not belong to: the only constructor demands a value already typed at exactly the supplied frame. Code without a
  * static target works inside `frame.type`; [[bindTo]] retypes the value to a statically known frame after image4s
  * checks that the two frames share a persistent key.
  *
  * @tparam V
  *   a frame-indexed family, e.g. `[F <: Frame[D3]] =>> Point[F, D3]`
  */
sealed trait Placed[V[_ <: Frame[D3]]]:
  val frame: Frame[D3]
  val world: WorldSpace
  def value: V[frame.type]

object Placed:
  /** Package a value typed at exactly `frame`; fails unless `frame` is a world frame from [[FrameCatalog]]. */
  def apply[V[_ <: Frame[D3]]](frame: Frame[D3])(value: V[frame.type]): Either[SpaceError, Placed[V]] =
    FrameCatalog.worldOf(frame).map(world => pack(frame, world, value))

  private def pack[V[_ <: Frame[D3]]](f: Frame[D3], w: WorldSpace, v: V[f.type]): Placed[V] =
    new Placed[V]:
      val frame: Frame[D3] = f
      val world: WorldSpace = w
      // `frame` is `f`; the singleton types differ only syntactically.
      def value: V[frame.type] = v.asInstanceOf[V[frame.type]]

  extension [V[_ <: Frame[D3]]](placed: Placed[V])
    /** Retype the value to `target` when `target` shares the package frame's persistent key. */
    def bindTo(target: Frame[D3])(using rebind: Rebind[V]): Either[SpaceError, V[target.type]] =
      Frame
        .alignOwners[D3, placed.frame.type, target.type](placed.frame, target)
        .left
        .map(error => SpaceError.FrameBinding(error.message))
        .flatMap(alignment => rebind.toRight(placed.value, alignment))

/** Moves a frame-indexed value across a checked frame alignment. */
trait Rebind[V[_ <: Frame[D3]]]:
  def toRight[A <: Frame[D3], B <: Frame[D3]](value: V[A], alignment: FrameAlignment[D3, A, B]): Either[SpaceError, V[B]]

object Rebind:
  type PointIn[F <: Frame[D3]] = Point[F, D3]
  type VecIn[F <: Frame[D3]] = Vec[F, D3]

  given points: Rebind[PointIn] with
    def toRight[A <: Frame[D3], B <: Frame[D3]](value: Point[A, D3], alignment: FrameAlignment[D3, A, B]): Either[SpaceError, Point[B, D3]] =
      alignment.pointToRight(value).left.map(error => SpaceError.FrameBinding(error.message))

  given vectors: Rebind[VecIn] with
    def toRight[A <: Frame[D3], B <: Frame[D3]](value: Vec[A, D3], alignment: FrameAlignment[D3, A, B]): Either[SpaceError, Vec[B, D3]] =
      alignment.vectorToRight(value).left.map(error => SpaceError.FrameBinding(error.message))
