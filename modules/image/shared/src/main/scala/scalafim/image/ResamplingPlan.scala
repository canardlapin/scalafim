package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.BoundaryPolicy
import image4s.Continuous
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LatticeIndex
import image4s.geometry.Point
import ravel.AnyRank
import ravel.ArrayBuilder
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import reframe4s.core.AffineMap
import reframe4s.core.MapError
import reframe4s.core.SmoothMap
import reframe4s.core.SpatialDifferential
import reframe4s.core.SpatialMap
import reframe4s.resample.Interpolation
import reframe4s.resample.ResamplingError
import reframe4s.resample.ResamplingPlan as ReframeResamplingPlan
import reframe4s.resample.ResamplingSink

/** Dynamic D3 boundary used by ScalaFIM's owner-erased image API.
  *
  * The value is the provider `SpatialMap` itself. Endpoints are widened only
  * after runtime owner checks against the source and target grids.
  */
type SpatialPullback = SpatialMap[Frame[D3], Frame[D3], D3]

enum ResamplingPlanError:
  case Geometry(error: GeometryError)
  case Map(error: MapError)
  case Provider(error: ResamplingError)

  def message: String =
    this match
      case Geometry(error) => error.message
      case Map(error)      => error.message
      case Provider(error) => error.message

enum JacobianModulation:
  case None, Jacobian, SqrtJacobian

enum ResamplingExecutionModel:
  /** Provider affine scanline kernel; no coordinate collection is retained. */
  case ProviderAffine

  /** Provider arbitrary-map kernel; one coordinate is prepared per target voxel. */
  case ProviderMapped

private sealed trait ModulationFactors:
  def atVoxel(ordinal: Int): Double

private object ModulationFactors:
  final case class Constant(value: Double) extends ModulationFactors:
    def atVoxel(ordinal: Int): Double = value

  final case class PerVoxel(values: Vector[Double]) extends ModulationFactors:
    def atVoxel(ordinal: Int): Double = values(ordinal)

/** Neuroimaging facade over reframe4s mapped resampling.
  *
  * This class owns only ScalaFIM policy: admitted D3 grids, output image
  * construction, and optional Jacobian modulation. Map evaluation,
  * interpolation, boundary handling, and prepared coordinates belong to
  * reframe4s.
  */
final class ResamplingPlan private (
    val source: GridSpec,
    val target: GridSpec,
    val pullback: SpatialPullback,
    val method: Resample.Method
):
  private def timeAxis(extent: Int): image4s.Axis =
    image4s.Axis
      .ordinal("time", image4s.AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def executionModel: ResamplingExecutionModel =
    pullback match
      case _: AffineMap[?, ?, ?] => ResamplingExecutionModel.ProviderAffine
      case _                     => ResamplingExecutionModel.ProviderMapped

  def materializedCoordinateCount: Int =
    executionModel match
      case ResamplingExecutionModel.ProviderAffine => 0
      case ResamplingExecutionModel.ProviderMapped => target.nVoxels

  def apply(
      volume: SomeScalarVolume[Double]
  ): Either[ResamplingPlanError, SomeScalarVolume[Double]] =
    apply(volume, outside = 0.0)

  def apply(
      volume: SomeScalarVolume[Double],
      outside: Double
  ): Either[ResamplingPlanError, SomeScalarVolume[Double]] =
    apply(volume, outside, JacobianModulation.None)

  def apply(
      volume: SomeScalarVolume[Double],
      outside: Double,
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, SomeScalarVolume[Double]] =
    Grid
      .exactCongruence(source.nativeGrid, volume.grid)
      .left
      .map(ResamplingPlanError.Geometry.apply)
      .flatMap: _ =>
        modulationFactors(modulation).flatMap: factors =>
          sampleVolume(volume, outside, factors)

  @scala.annotation.targetName("applyNeuroSeries")
  def apply(
      series: SomeScalarSeries[Double]
  ): Either[ResamplingPlanError, SomeScalarSeries[Double]] =
    apply(series, outside = 0.0)

  @scala.annotation.targetName("applyNeuroSeriesOutside")
  def apply(
      series: SomeScalarSeries[Double],
      outside: Double
  ): Either[ResamplingPlanError, SomeScalarSeries[Double]] =
    apply(series, outside, JacobianModulation.None)

  @scala.annotation.targetName("applyNeuroSeriesModulated")
  def apply(
      series: SomeScalarSeries[Double],
      outside: Double,
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, SomeScalarSeries[Double]] =
    Grid
      .exactCongruence(source.nativeGrid, series.grid)
      .left
      .map(ResamplingPlanError.Geometry.apply)
      .flatMap: _ =>
        modulationFactors(modulation).flatMap: factors =>
          sampleSeries(series, outside, factors)

  private def sampleVolume(
      volume: SomeScalarVolume[Double],
      outside: Double,
      factors: ModulationFactors
  ): Either[ResamplingPlanError, SomeScalarVolume[Double]] =
    given DType[Double] = volume.values.dtype
    val targetShape = target.shape
    var failure = Option.empty[ResamplingPlanError]
    val output =
      RavelArray.build[Double, Rank[3]](
        Shape(targetShape.x, targetShape.y, targetShape.z)
      ): builder =>
        failure =
          scanProvider(
            volume.sampled,
            builder,
            trailingSize = 1,
            outside,
            factors
          ).left.toOption
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          SomeNeuroVolume.unsafeFromRavel(
            output,
            target.toSampleSpace,
            volume.metadata
          )
        )

  private def sampleSeries(
      series: SomeScalarSeries[Double],
      outside: Double,
      factors: ModulationFactors
  ): Either[ResamplingPlanError, SomeScalarSeries[Double]] =
    given DType[Double] = series.values.dtype
    val targetShape = target.shape
    val volumes = series.nVolumes
    var failure = Option.empty[ResamplingPlanError]
    val output =
      RavelArray.build[Double, Rank[4]](
        Shape(targetShape.x, targetShape.y, targetShape.z, volumes)
      ): builder =>
        failure =
          scanProvider(
            series.sampled,
            builder,
            trailingSize = volumes,
            outside,
            factors
          ).left.toOption
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          SomeNeuroSeries.unsafeFromRavel(
            output,
            target.toSampleSpace.addDim(timeAxis(volumes)),
            series.metadata
          )
        )

  private def scanProvider[R <: AnyRank](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        Double,
        Continuous,
        R
      ],
      output: ArrayBuilder[Double],
      trailingSize: Int,
      outside: Double,
      factors: ModulationFactors
  ): Either[ResamplingPlanError, Unit] =
    val captured = sampled.asInstanceOf[
      Sampled[SampleSpace[Frame[D3], D3], Double, Continuous, R]
    ]
    ReframeResamplingPlan
      .mapped(
        captured,
        target.nativeGrid,
        pullback,
        ResamplingPlan.interpolation(method),
        BoundaryPolicy.Constant(outside)
      )
      .left
      .map(ResamplingPlanError.Provider.apply)
      .flatMap: plan =>
        plan
          .scan(
            plan.newWorkspace(),
            new ResamplingSink:
              def accept(
                  outputLinearIndex: Int,
                  value: Double,
                  validityWeight: Double
              ): Unit =
                val voxelOrdinal = outputLinearIndex / trailingSize
                output.writeLinear(
                  outputLinearIndex,
                  value * factors.atVoxel(voxelOrdinal)
                )
          )
          .left
          .map(ResamplingPlanError.Provider.apply)

  private def modulationFactors(
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, ModulationFactors] =
    modulation match
      case JacobianModulation.None =>
        Right(ModulationFactors.Constant(1.0))
      case JacobianModulation.Jacobian | JacobianModulation.SqrtJacobian =>
        targetPoints.flatMap: points =>
          val determinants = Vector.newBuilder[Double]
          var index = 0
          var failure = Option.empty[MapError]
          while index < points.length && failure.isEmpty do
            differentialAt(points(index)) match
              case Left(error) => failure = Some(error)
              case Right(matrix) =>
                determinants += math.abs(ResamplingPlan.determinant3(matrix))
            index += 1
          failure match
            case Some(error) => Left(ResamplingPlanError.Map(error))
            case None =>
              val values = determinants.result()
              Right(
                ModulationFactors.PerVoxel(
                  if modulation == JacobianModulation.SqrtJacobian then
                    values.map(math.sqrt)
                  else values
                )
              )

  private def differentialAt(
      point: Point[Frame[D3], D3]
  ): Either[MapError, DMat] =
    pullback match
      case smooth: SmoothMap[?, ?, ?] =>
        smooth
          .asInstanceOf[SmoothMap[Frame[D3], Frame[D3], D3]]
          .jet1At(point)
          .map(_.differential)
      case _ =>
        SpatialDifferential
          .centralDifference(pullback, point, step = 1e-3)
          .map(_.differential)

  private def targetPoints: Either[ResamplingPlanError, Vector[Point[Frame[D3], D3]]] =
    val grid = target.nativeGrid
    val points = Vector.newBuilder[Point[Frame[D3], D3]]
    var linear = 0
    var failure = Option.empty[GeometryError]
    while linear < target.nVoxels && failure.isEmpty do
      val coordinate = Indexing.indexToGrid3D(target.shape, linear)
      val point =
        for
          index <- LatticeIndex.fromVector[D3](coordinate.toVector)
          value <- grid.pointAt(index)
        yield value
      point match
        case Left(error)  => failure = Some(error)
        case Right(value) => points += value
      linear += 1
    failure
      .map(error => Left(ResamplingPlanError.Geometry(error)))
      .getOrElse(Right(points.result()))

object ResamplingPlan:
  def make(
      source: GridSpec,
      target: GridSpec,
      pullback: SpatialPullback,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    for
      _ <- SpatialMap
        .validateSourceFrame(pullback.source, target.nativeGrid.frame)
        .left
        .map(ResamplingPlanError.Map.apply)
      _ <- SpatialMap
        .validateResultFrame(source.nativeGrid.frame, pullback.target)
        .left
        .map(ResamplingPlanError.Map.apply)
    yield new ResamplingPlan(source, target, pullback, method)

  def identity(
      source: GridSpec,
      method: Resample.Method = Resample.Method.Linear
  ): Either[ResamplingPlanError, ResamplingPlan] =
    val frame = source.nativeGrid.frame
    val pullback = AffineMap.identity[D3, Frame[D3]](frame)
    make(source, source, pullback, method)

  def fromSpaces(
      source: SomeSampleSpace,
      target: SomeSampleSpace,
      pullback: SpatialPullback,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    make(
      GridSpec.fromSpace(source),
      GridSpec.fromSpace(target),
      pullback,
      method
    )

  private def interpolation(
      method: Resample.Method
  ): Interpolation[Continuous] =
    method match
      case Resample.Method.Nearest => Interpolation.Nearest
      case Resample.Method.Linear  => Interpolation.Linear
      case Resample.Method.Cubic   => Interpolation.Cubic

  private def determinant3(matrix: DMat): Double =
    require(matrix.rows == 3 && matrix.cols == 3, "D3 differential must be 3x3")
    val a = matrix(0, 0)
    val b = matrix(0, 1)
    val c = matrix(0, 2)
    val d = matrix(1, 0)
    val e = matrix(1, 1)
    val f = matrix(1, 2)
    val g = matrix(2, 0)
    val h = matrix(2, 1)
    val i = matrix(2, 2)
    a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
