package scalafim.image

import SampleSpaces.*

import image4s.Axis
import image4s.AxisKind
import image4s.Continuous
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Point
import ravel.NDArray
import ravel.Rank
import reframe4s.core.MapError
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.field.DenseMap
import reframe4s.field.FieldError
import reframe4s.core.SpatialMap
import reframe4s.lie.FramedAffine
import reframe4s.resample.Interpolation

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
  def transform(
      pullback: SpatialPullback,
      point: SpatialPoint
  ): Either[SpatialPullbackError, SpatialPoint] =
    val sourceFrame: Frame[D3] = pullback.source
    for
      raw <- Point
        .fromVector(sourceFrame, point.toVector)
        .left
        .map(SpatialPullbackError.Geometry.apply)
      alignment <- Frame
        .alignOwners[D3, sourceFrame.type, Frame[D3]](
          sourceFrame,
          sourceFrame
        )
        .left
        .map(SpatialPullbackError.Geometry.apply)
      framed <- alignment
        .pointToRight(raw)
        .left
        .map(SpatialPullbackError.Geometry.apply)
      result <- pullback(framed).left.map(SpatialPullbackError.Map.apply)
      value <- SpatialPoint
        .fromVector(result.coordinates, "provider pullback result")
        .left
        .map(error => SpatialPullbackError.InvalidCoordinates(error.message))
    yield value

  def affine(
      source: GridSpec,
      target: GridSpec,
      pull: Affine[D3]
  ): SpatialPullback =
    affineBetween(source.providerFrame, target.providerFrame, pull)

  /** Bind an affine to explicit live output/input frame owners. */
  private[scalafim] def affineBetween(
      outputFrame: Frame[D3],
      inputFrame: Frame[D3],
      pull: Affine[D3]
  ): SpatialPullback =
    FramedAffine.betweenFrames[Frame[D3], Frame[D3], D3](
      inputFrame,
      outputFrame
    )(pull)

  def worldAligned(
      source: GridSpec,
      target: GridSpec
  ): SpatialPullback =
    affine(source, target, Affine.identity[D3])

  def coordinates(
      source: GridSpec,
      target: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback] =
    coordinatesOn(
      source,
      target.nativeGrid.frame,
      target,
      values,
      method,
      boundary
    )

  /** Bind field geometry to the authoritative target-domain frame owner. */
  def coordinatesOn(
      source: GridSpec,
      target: GridSpec,
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback] =
    coordinatesBetween(
      source.providerFrame,
      target.nativeGrid.frame,
      fieldGrid,
      values,
      method,
      boundary
    )

  /** Bind field geometry to the authoritative target-domain frame owner. */
  def coordinatesOn(
      source: GridSpec,
      queryFrame: Frame[D3],
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback] =
    coordinatesBetween(
      source.providerFrame,
      queryFrame,
      fieldGrid,
      values,
      method,
      boundary
    )

  private[scalafim] def coordinatesBetween(
      outputFrame: Frame[D3],
      queryFrame: Frame[D3],
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback] =
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
          .in(queryFrame)(fieldGrid.dims, fieldGrid.affine)
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
            interpolation(method),
            boundary
          )
          .left
          .map(SpatialPullbackError.Field.apply)
      yield SpatialMap.eraseFrameRefinements(map)

  def displacement(
      source: GridSpec,
      target: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback] =
    displacementOn(
      source,
      target.nativeGrid.frame,
      target,
      values,
      method,
      boundary
    )

  def displacementOn(
      source: GridSpec,
      target: GridSpec,
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback] =
    displacementBetween(
      source.providerFrame,
      target.nativeGrid.frame,
      fieldGrid,
      values,
      method,
      boundary
    )

  def displacementOn(
      source: GridSpec,
      queryFrame: Frame[D3],
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method = Resample.Method.Linear,
      boundary: CoordinateBoundaryPolicy =
        CoordinateBoundaryPolicy.PreserveSource
  ): Either[SpatialPullbackError, SpatialPullback] =
    displacementBetween(
      source.providerFrame,
      queryFrame,
      fieldGrid,
      values,
      method,
      boundary
    )

  private[scalafim] def displacementBetween(
      outputFrame: Frame[D3],
      queryFrame: Frame[D3],
      fieldGrid: GridSpec,
      values: NDArray[Double, Rank[4]],
      method: Resample.Method,
      boundary: CoordinateBoundaryPolicy
  ): Either[SpatialPullbackError, SpatialPullback] =
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

  private def interpolation(
      method: Resample.Method
  ): Interpolation[Continuous] =
    method match
      case Resample.Method.Nearest => Interpolation.Nearest
      case Resample.Method.Linear  => Interpolation.Linear
      case Resample.Method.Cubic   => Interpolation.Cubic
