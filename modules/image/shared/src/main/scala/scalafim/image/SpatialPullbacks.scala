package scalafim.image

import SampleSpaces.*

import image4s.Axis
import image4s.AxisKind
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import ravel.NDArray
import ravel.Rank
import reframe4s.core.MapError
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.field.DenseMap
import reframe4s.field.FieldError
import reframe4s.lie.FramedAffine

enum SpatialPullbackError:
  case Image(error: ImageError)
  case Field(error: FieldError)
  case Geometry(error: GeometryError)
  case Map(error: MapError)
  case ShapeMismatch(expected: Vector[Int], actual: Vector[Int])
  case InvalidCoordinates(reason: String)

  def message: String =
    this match
      case Image(error) => error.message
      case Field(error) => error.message
      case Geometry(error) => error.message
      case Map(error) => error.message
      case ShapeMismatch(expected, actual) =>
        s"coordinate field shape mismatch: expected $expected, got $actual"
      case InvalidCoordinates(reason) => reason

/** Neuroimaging renditions that produce provider maps directly.
  *
  * These constructors own only the conversion from a D3 image/grid boundary
  * into reframe4s. The returned value is a `SpatialMap`; composition,
  * evaluation, interpolation, and resampling remain provider operations.
  */
object SpatialPullbacks:
  /** Apply a provider pullback at the neuroimaging value boundary without
    * weakening its live frame-owner checks.
    */
  def transform[T <: Frame[D3], S <: Frame[D3]](
      pullback: SpatialPullback[T, S],
      point: SpatialPoint
  ): Either[SpatialPullbackError, SpatialPoint] =
    for
      framed <- GridSpec
        .pointIn(pullback.source, point.toVector)
        .left
        .map(SpatialPullbackError.Geometry.apply)
      result <- pullback(framed).left.map(SpatialPullbackError.Map.apply)
      value <- SpatialPoint
        .fromVector(result.coordinates, "provider pullback result")
        .left
        .map(error => SpatialPullbackError.InvalidCoordinates(error.message))
    yield value

  /** The affine pullback from `target` grid points to `source` grid points. */
  def affine[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T],
      pull: Affine[D3]
  ): SpatialPullback[T, S] =
    affineBetween(source.frame, target.frame, pull)

  /** Bind an affine to explicit live output/input frame owners. */
  private[scalafim] def affineBetween[S <: Frame[D3], T <: Frame[D3]](
      outputFrame: S,
      inputFrame: T,
      pull: Affine[D3]
  ): SpatialPullback[T, S] =
    FramedAffine.betweenFrames[T, S, D3](
      inputFrame,
      outputFrame
    )(pull)

  /** Identity on world coordinates between two grids whose worlds are taken to coincide. */
  def worldAligned[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T]
  ): SpatialPullback[T, S] =
    affine(source, target, Affine.identity[D3])

  def coordinates[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback[T, S]] =
    coordinatesBetween(
      source.frame,
      target.frame,
      target,
      values,
      method,
      boundary
    )

  /** Bind field geometry to the authoritative target-domain frame owner. */
  def coordinatesOn[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T],
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback[T, S]] =
    coordinatesBetween(
      source.frame,
      target.frame,
      fieldGrid,
      values,
      method,
      boundary
    )

  /** Bind field geometry to the authoritative target-domain frame owner. */
  def coordinatesOn[S <: Frame[D3], Q <: Frame[D3]](
      source: GridSpec[S],
      queryFrame: Q,
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback[Q, S]] =
    coordinatesBetween(
      source.frame,
      queryFrame,
      fieldGrid,
      values,
      method,
      boundary
    )

  private[scalafim] def coordinatesBetween[S <: Frame[D3], Q <: Frame[D3]](
      outputFrame: S,
      queryFrame: Q,
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback[Q, S]] =
    val expected = fieldGrid.dims :+ 3
    val actual = Vector.tabulate(values.shape.rank)(values.shape.apply)
    if actual != expected then
      Left(SpatialPullbackError.ShapeMismatch(expected, actual))
    else
      for
        direction <- Axis
          .create("direction", 3, AxisKind.Direction)
          .left
          .map(SpatialPullbackError.Image.apply)
        axes <- NonSpatialAxes
          .from(Vector(direction))
          .left
          .map(SpatialPullbackError.Image.apply)
        reboundGrid <- Grid
          .forFrame[D3, Q](queryFrame)(fieldGrid.dims, fieldGrid.affine)
          .left
          .map(SpatialPullbackError.Geometry.apply)
        space = SampleSpace.create(reboundGrid, axes)
        sampled <- Sampled
          .continuous(space, values)
          .left
          .map(SpatialPullbackError.Image.apply)
        map <- DenseMap
          .fromCoordinates(
            sampled,
            outputFrame,
            method.interpolation,
            boundary
          )
          .left
          .map(SpatialPullbackError.Field.apply)
      yield map

  def displacement[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback[T, S]] =
    displacementBetween(
      source.frame,
      target.frame,
      target,
      values,
      method,
      boundary
    )

  def displacementOn[S <: Frame[D3], T <: Frame[D3]](
      source: GridSpec[S],
      target: GridSpec[T],
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback[T, S]] =
    displacementBetween(
      source.frame,
      target.frame,
      fieldGrid,
      values,
      method,
      boundary
    )

  def displacementOn[S <: Frame[D3], Q <: Frame[D3]](
      source: GridSpec[S],
      queryFrame: Q,
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback[Q, S]] =
    displacementBetween(
      source.frame,
      queryFrame,
      fieldGrid,
      values,
      method,
      boundary
    )

  private[scalafim] def displacementBetween[S <: Frame[D3], Q <: Frame[D3]](
      outputFrame: S,
      queryFrame: Q,
      fieldGrid: GridSpec[?],
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback[Q, S]] =
    val expected = fieldGrid.dims :+ 3
    val actual = Vector.tabulate(values.shape.rank)(values.shape.apply)
    if actual != expected then
      Left(SpatialPullbackError.ShapeMismatch(expected, actual))
    else
      val shape = fieldGrid.shape
      val absolute =
        NDArray.tabulate[Double](shape.x, shape.y, shape.z, 3):
          (x, y, z, component) =>
            val world =
              fieldGrid.voxelToWorld(
                SpatialPoint(x.toDouble, y.toDouble, z.toDouble)
              )
            world.toVector(component) + values(x, y, z, component)
      coordinatesBetween(
        outputFrame,
        queryFrame,
        fieldGrid,
        absolute,
        method,
        boundary
      )
