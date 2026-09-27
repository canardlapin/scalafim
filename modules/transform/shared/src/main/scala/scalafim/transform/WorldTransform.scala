package scalafim.transform

import image4s.{BoundaryPolicy, Continuous, ContinuousImage, SampleSpace}
import image4s.geometry.{D3, Frame, Grid, Point}
import ravel.AnyRank
import reframe4s.core.{SmoothIso, SpatialMap}
import reframe4s.field.{CoordinateBoundaryPolicy, CoverageReportingMap, DenseMap, DeterminantDirection, LogDeterminantField}
import reframe4s.lie.FramedAffine
import reframe4s.resample.{Interpolation, ResamplingResult, VolumeModulation}
import scalafim.image.world.WorldLink

/** A spatial transform from world space `S` (source, moving) to world space `T` (target, fixed), whatever toolkit it
  * came from.
  *
  * Every transform carries its pullback [[pull]], `T -> S`: resampling data from `S` onto a grid in `T` evaluates the
  * pullback at target points. Whether the forward direction `S -> T` exists is part of the type: affines and analytic
  * isomorphisms always have it; a dense warp has it only when an inverse asset was supplied or a numerical estimate
  * passed its qualification gates ([[WorldTransform.Mapped.invertNumerically]]). No caller ever passes a direction flag.
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

  /** This transform as a typed point link from its source frame (left) to its target frame (right), the map behind a
    * linked cursor between data in the two worlds. `toLeft` is the pullback and always exists; `toRight` exists exactly
    * when [[push]] does, and is otherwise `WorldLinkError.DirectionUnavailable`, never an approximation. Read the other
    * way with `swap`.
    */
  final def link: Either[TransformError, WorldLink.Mapped[S, T]] =
    WorldLink.pullback(pull, push, provenance.describe).left.map(error => TransformError.Invalid(error.message))

  /** Lazy composition `S -> T -> U`. Two affines are better fused with [[WorldTransform.Linear.andThen]].
    *
    * The composed pullback reports, per evaluation, the first stage whose boundary policy supplied the value, so a
    * later [[materialize]] attributes coverage to the stage that left its support.
    */
  def andThen[U <: Frame[D3]](next: WorldTransform[T, U]): WorldTransform.Mapped[S, U] =
    val pushed =
      for
        first  <- push
        second <- next.push
      yield first.andThen(second)
    WorldTransform.Mapped(
      CoverageReportingMap.compose(next.pull, pull),
      pushed.fold(PushAvailability.Unavailable[S, U]())(PushAvailability.Composed(_)),
      provenance.andThen(next.provenance)
    )

  /** Sample the pullback at every point of `on` as an absolute-coordinate field (the `convertwarp` operation).
    *
    * Points where a stage leaves its sampled support are counted in the coverage report and filled only by an explicit
    * `fill` policy; the default `Reject` fails with [[TransformError.Composition]] carrying the report.
    */
  final def materialize(
      on: Grid[T, D3],
      fill: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject,
      interpolation: Interpolation[Continuous] = Interpolation.Linear
  ): Either[TransformError, MaterializedField[S, T]] =
    WarpAlgebra.materialize(this, on, fill, interpolation)

  /** Physical Jacobian determinant of the pullback, `det D pull(y)` (source volume per unit target volume), at every
    * point of `on`; `Push` reports its reciprocal. Folds (`det <= 0`) are masked and counted, never `NaN`. Every
    * lattice point must be evaluated by the transform itself (its stages' own boundary policies included): a point
    * that leaves a rejecting stage fails with [[TransformError.Composition]] rather than being differentiated as fill.
    */
  final def jacobianDeterminant(
      on: Grid[T, D3],
      direction: DeterminantDirection = DeterminantDirection.Pull
  ): Either[TransformError, JacobianDeterminant[S, T]] =
    WarpAlgebra.jacobianDeterminant(this, on, direction)

  /** Log-Jacobian of the pullback on `on`; folded points are a typed `NonPositive` status, not `-Inf`. */
  final def logJacobian(
      on: Grid[T, D3],
      direction: DeterminantDirection = DeterminantDirection.Pull
  ): Either[TransformError, LogDeterminantField[T, D3]] =
    jacobianDeterminant(on, direction).map(_.logJacobian)

  /** Resample a source image onto `onto` through the pullback, as one reframe4s `ResamplingPlan` (affine pullbacks keep
    * the affine kernel). A target point whose pullback leaves the source image fails the plan unless `boundary` fills it:
    * there is no silent identity outside a field.
    */
  final def resample[Space <: SampleSpace[S, D3], R <: AnyRank](
      image: ContinuousImage[Space, Double, R],
      onto: Grid[T, D3],
      interpolation: Interpolation[Continuous] = Interpolation.Linear,
      boundary: BoundaryPolicy[Double] = BoundaryPolicy.Reject
  ): Either[TransformError, ResamplingResult[T, D3, Continuous]] =
    WarpAlgebra.resample(this, image, onto, interpolation, boundary)

  /** Resample a source image onto `onto` through the pullback, scaling each target sample by the volume change:
    * `Jacobian` preserves a density's integral, `SqrtJacobian` the squared L2 norm of an amplitude.
    *
    * Determinants are central differences of the pullback on `onto` (exact for affines). Orientation-reversing points
    * are modulated by `|det|` and singular points by zero; both are counted in the result's diagnostics, which callers
    * that must not resample through folds check. A target point the pullback rejects fails the whole plan.
    */
  final def resampleModulated[Space <: SampleSpace[S, D3], R <: AnyRank](
      image: ContinuousImage[Space, Double, R],
      onto: Grid[T, D3],
      modulation: VolumeModulation,
      interpolation: Interpolation[Continuous] = Interpolation.Linear,
      boundary: BoundaryPolicy[Double] = BoundaryPolicy.Reject
  ): Either[TransformError, ModulatedResample[T]] =
    WarpAlgebra.resampleModulated(this, image, onto, modulation, interpolation, boundary)

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
    availability match
      case PushAvailability.Estimated(inverse) =>
        require(inverse.qualifiedPull eq pull, "an estimated forward map belongs to the pullback it was qualified against")
      case _ => ()

    def source: S = pull.target
    def target: T = pull.source
    def push: Option[SpatialMap[S, T, D3]] = availability.map

    /** Estimate the forward map numerically on the source lattice `on` (which must be persistent: the estimate
      * records its evaluation domain). The result carries [[PushAvailability.Estimated]] only when every gate of
      * `policy` passes; otherwise it is [[TransformError.Inversion]] with the complete evidence. The pullback must be
      * dense: materialize composites first. A forward map read from an inverse asset is never silently replaced.
      *
      * Points the pullback's own boundary policy supplies (`PreserveSource`, `Constant`) are part of the map being
      * inverted and count towards coverage like any other.
      */
    def invertNumerically(on: Grid[S, D3], policy: InversionPolicy): Either[TransformError, Mapped[S, T]] =
      (pull, availability) match
        case (_, PushAvailability.FromAsset(_, asset)) =>
          Left(TransformError.Invalid(s"${provenance.describe} already has a forward map from ${asset.label}"))
        // DenseMap is final and invariant, and `pull` is statically SpatialMap[T, S, D3]: a class test suffices.
        case (dense: DenseMap[T, S, D3, ?] @unchecked, _) => WarpAlgebra.invertNumerically(this, dense, on, policy)
        case _                                            => Left(TransformError.NeedsMaterialization(provenance.describe))

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

  /** A numerical inverse that passed its qualification gates, bound to the pullback it was qualified against. It
    * carries the reframe4s `InverseEstimate` and the full residual evidence.
    */
  case Estimated(inverse: QualifiedInverse[S, T])

  case Unavailable()

  def map: Option[SpatialMap[S, T, D3]] =
    this match
      case FromAsset(push, _) => Some(push)
      case Composed(push)     => Some(push)
      case Estimated(inverse) => Some(inverse.estimate.value)
      case Unavailable()      => None
