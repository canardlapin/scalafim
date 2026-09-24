package scalafim.transform

import image4s.geometry.{D3, Frame, Point}
import reframe4s.core.{SmoothIso, SpatialMap}
import reframe4s.lie.FramedAffine

/** A spatial transform from world space `S` (source, moving) to world space `T` (target, fixed), whatever toolkit it
  * came from.
  *
  * Every transform carries its pullback [[pull]], `T -> S`: resampling data from `S` onto a grid in `T` evaluates the
  * pullback at target points. Whether the forward direction `S -> T` exists is part of the type: affines and analytic
  * isomorphisms always have it; a dense warp has it only when an inverse asset (or, later, a numerical estimate) was
  * supplied. No caller ever passes a direction flag.
  */
sealed trait WorldTransform[S <: Frame[D3], T <: Frame[D3]]:
  def source: S
  def target: T

  /** Target point -> source point. */
  def pull: SpatialMap[T, S, D3]

  /** Source point -> target point, when known. */
  def push: Option[SpatialMap[S, T, D3]]

  def provenance: TransformProvenance

  final def pullPoint(point: Point[T, D3]): Either[TransformError, Point[S, D3]] =
    pull(point).left.map(TransformError.Map(_))

  final def mapPoint(point: Point[S, D3]): Either[TransformError, Point[T, D3]] =
    push
      .toRight(TransformError.NoForwardMap(provenance.describe))
      .flatMap(forward => forward(point).left.map(TransformError.Map(_)))

  /** Lazy composition `S -> T -> U`. Two affines are better fused with [[WorldTransform.Linear.andThen]]. */
  def andThen[U <: Frame[D3]](next: WorldTransform[T, U]): WorldTransform[S, U] =
    val pushed =
      for
        first  <- push
        second <- next.push
      yield first.andThen(second)
    WorldTransform.Mapped(
      next.pull.andThen(pull),
      pushed.fold(PushAvailability.Unavailable[S, U]())(PushAvailability.Composed(_)),
      provenance.andThen(next.provenance)
    )

object WorldTransform:
  /** An affine transform; `framed` is its pullback `T -> S`. The inverse is total. */
  final case class Linear[S <: Frame[D3], T <: Frame[D3]](framed: FramedAffine[T, S, D3], provenance: TransformProvenance)
      extends WorldTransform[S, T]:
    def source: S = framed.target
    def target: T = framed.source
    def pull: SpatialMap[T, S, D3] = framed
    def push: Option[SpatialMap[S, T, D3]] = Some(framed.inverse)

    def inverse: Linear[T, S] =
      Linear(framed.inverse, provenance.andThen(TransformProvenance(Vector(TransformProvenance.Step.Derived("exact affine inverse")))))

    /** Fused composition of two affines into one, through the provider's conditioned affine composition. */
    def andThen[U <: Frame[D3]](next: Linear[T, U]): Either[TransformError, Linear[S, U]] =
      // pullback of S -> U is U -> T (next) followed by T -> S (this)
      next.framed.operator
        .andThen(framed.operator)
        .left
        .map(TransformError.Geometry(_))
        .map: operator =>
          Linear(
            FramedAffine.betweenFrames[U, S, D3](next.target, source)(operator),
            provenance.andThen(next.provenance).andThen(TransformProvenance(Vector(TransformProvenance.Step.Derived("fused affine composition"))))
          )

  /** A smooth analytic isomorphism; `iso` is its pullback. The forward map is `iso.inverse`, never stored separately. */
  final case class Smooth[S <: Frame[D3], T <: Frame[D3]](iso: SmoothIso[T, S, D3], provenance: TransformProvenance)
      extends WorldTransform[S, T]:
    def source: S = iso.target
    def target: T = iso.source
    def pull: SpatialMap[T, S, D3] = iso
    def push: Option[SpatialMap[S, T, D3]] = Some(iso.inverse)

    def inverse: Smooth[T, S] =
      Smooth(iso.inverse, provenance.andThen(TransformProvenance(Vector(TransformProvenance.Step.Derived("analytic inverse")))))

  /** A dense or composite map. The forward direction exists only if something supplied it. */
  final case class Mapped[S <: Frame[D3], T <: Frame[D3]](
      pull: SpatialMap[T, S, D3],
      availability: PushAvailability[S, T],
      provenance: TransformProvenance
  ) extends WorldTransform[S, T]:
    def source: S = pull.target
    def target: T = pull.source
    def push: Option[SpatialMap[S, T, D3]] = availability.map

    /** Swap directions; possible only when the forward map exists. */
    def invert: Either[TransformError, Mapped[T, S]] =
      push
        .toRight(TransformError.NoForwardMap(provenance.describe))
        .map(forward => Mapped(forward, PushAvailability.Composed(pull), provenance.andThen(TransformProvenance(Vector(TransformProvenance.Step.Derived("swapped with supplied inverse"))))))

  def affine[S <: Frame[D3], T <: Frame[D3]](pullback: FramedAffine[T, S, D3], provenance: TransformProvenance): Linear[S, T] =
    Linear(pullback, provenance)

/** Whether, and how, the forward direction of a [[WorldTransform.Mapped]] is known. */
enum PushAvailability[S <: Frame[D3], T <: Frame[D3]]:
  /** Read from an inverse asset, e.g. ANTs `InverseWarp` or FSL `invwarp` output. */
  case FromAsset(push: SpatialMap[S, T, D3], asset: AssetRef)

  /** Composed from forward maps that were themselves available. */
  case Composed(push: SpatialMap[S, T, D3])

  case Unavailable()

  def map: Option[SpatialMap[S, T, D3]] =
    this match
      case FromAsset(push, _) => Some(push)
      case Composed(push)     => Some(push)
      case Unavailable()      => None
