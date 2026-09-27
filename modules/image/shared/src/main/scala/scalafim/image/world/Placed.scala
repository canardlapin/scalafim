package scalafim.image.world

import image4s.geometry.{D3, Frame, FrameAlignment, Point, Vec}

/** A frame-indexed value whose frame is known only at runtime: a dependent pair.
  *
  * The frame is a type member `F`, and the value is typed at exactly that member (`V[F]`), so a value cannot be
  * packaged with a frame it does not belong to. Values from runtime decoders, whose frame type is an existential
  * (`GridSpec[?]`), package through [[Placed.of]] by capture. Code without a static target works inside `placed.F`;
  * [[Placed.bindTo]] retypes the value to a statically known frame after image4s checks that the two frames share a
  * persistent key.
  *
  * @tparam V
  *   a frame-indexed family, e.g. `[F <: Frame[D3]] =>> Point[F, D3]`
  */
sealed trait Placed[V[_ <: Frame[D3]]]:
  type F <: Frame[D3]
  val frame: F
  val world: WorldSpace
  def value: V[F]

object Placed:
  /** Package a value typed at exactly `frame`; fails unless `frame` is a world frame from [[FrameCatalog]]. */
  def apply[V[_ <: Frame[D3]]](frame: Frame[D3])(value: V[frame.type]): Either[SpaceError, Placed[V]] =
    FrameCatalog.worldOf(frame).map(world => pack[V, frame.type](frame, world, value))

  /** Package a value typed at the frame type `F`, including an existential one captured from a runtime decoder:
    * {{{
    * val decoded: GridSpec[?] = ...
    * Placed.of(decoded.frame)(decoded)
    * }}}
    * Because `F` may be a wide type such as `Frame[D3]`, the static types alone do not prove that `value` belongs to
    * `frame`; the value's own runtime frame owner is therefore checked against `frame` as well.
    */
  def of[F0 <: Frame[D3], V[_ <: Frame[D3]]](frame: F0)(value: V[F0])(using owned: FrameOwned[V]): Either[SpaceError, Placed[V]] =
    if !owned.frameOf(value).sameRuntimeOwnerAs(frame) then
      Left(SpaceError.FrameBinding("the value's frame owner is not the frame it is being packaged with"))
    else FrameCatalog.worldOf(frame).map(world => pack[V, F0](frame, world, value))

  private def pack[V[_ <: Frame[D3]], F0 <: Frame[D3]](f: F0, w: WorldSpace, v: V[F0]): Placed[V] =
    new Placed[V]:
      type F = F0
      val frame: F0 = f
      val world: WorldSpace = w
      def value: V[F0] = v

  extension [V[_ <: Frame[D3]]](placed: Placed[V])
    /** Retype the value to `target` when `target` shares the package frame's persistent key. */
    def bindTo(target: Frame[D3])(using rebind: Rebind[V]): Either[SpaceError, V[target.type]] =
      Frame
        .alignOwners[D3, placed.F, target.type](placed.frame, target)
        .left
        .map(error => SpaceError.FrameBinding(error.message))
        .flatMap(alignment => rebind.toRight(placed.value, alignment))

/** Reads the runtime frame owner of a frame-indexed value. */
trait FrameOwned[V[_ <: Frame[D3]]]:
  def frameOf[F <: Frame[D3]](value: V[F]): F

object FrameOwned:
  given points: FrameOwned[Rebind.PointIn] with
    def frameOf[F <: Frame[D3]](value: Point[F, D3]): F = value.frame

  given vectors: FrameOwned[Rebind.VecIn] with
    def frameOf[F <: Frame[D3]](value: Vec[F, D3]): F = value.frame

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
