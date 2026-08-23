package scalafim.image

import image4s.BoundaryPolicy
import image4s.Continuous
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine as GeometryAffine
import image4s.geometry.D3
import image4s.geometry.Frame
import ravel.AnyRank
import ravel.ArrayBuilder
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import reframe4s.lie.FramedAffine
import reframe4s.resample.Interpolation
import reframe4s.resample.ResamplingPlan as ReframeResamplingPlan
import reframe4s.resample.ResamplingSink

enum ResamplingPlanError:
  case SingularSourceAffine(reason: String)
  case SourceSpaceMismatch(expected: GridSpec, actual: GridSpec)
  case UnsupportedMethod(method: Resample.Method)
  case MorphismEvaluationFailed(reason: String)
  case ProviderFailure(reason: String)

  def message: String =
    this match
      case SingularSourceAffine(reason) =>
        s"source affine is singular: $reason"
      case SourceSpaceMismatch(expected, actual) =>
        s"source volume space does not match plan source grid: expected ${expected.dims}, actual ${actual.dims}"
      case UnsupportedMethod(method) =>
        s"resampling plan does not support method $method"
      case MorphismEvaluationFailed(reason) =>
        s"morphism evaluation failed: $reason"
      case ProviderFailure(reason) =>
        s"reframe4s resampling failed: $reason"

enum JacobianModulation:
  case None, Jacobian, SqrtJacobian

enum ResamplingExecutionModel:
  /** Affine coordinates are fused by reframe4s; no coordinate collection is retained. */
  case AffineProvider

  /** Non-affine or cubic execution retains one prepared source-voxel coordinate per target voxel. */
  case WorkloadPrepared

private sealed trait ResamplingBackend:
  def executionModel: ResamplingExecutionModel
  def materializedCoordinateCount: Int

private object ResamplingBackend:
  final case class AffineProvider(
      pull: GeometryAffine[D3],
      interpolation: Interpolation[Continuous]
  ) extends ResamplingBackend:
    val executionModel: ResamplingExecutionModel =
      ResamplingExecutionModel.AffineProvider
    val materializedCoordinateCount: Int = 0

  final case class WorkloadPrepared(
      sourceVoxelPoints: Vector[VoxelPoint]
  ) extends ResamplingBackend:
    val executionModel: ResamplingExecutionModel =
      ResamplingExecutionModel.WorkloadPrepared
    val materializedCoordinateCount: Int = sourceVoxelPoints.length

private sealed trait ModulationFactors:
  def atVoxel(ordinal: Int): Double

private object ModulationFactors:
  final case class Constant(value: Double) extends ModulationFactors:
    def atVoxel(ordinal: Int): Double = value

  final case class PerVoxel(values: Vector[Double]) extends ModulationFactors:
    def atVoxel(ordinal: Int): Double = values(ordinal)

final class ResamplingPlan private (
    val source: GridSpec,
    val target: GridSpec,
    val morphism: SpatialMorphism,
    val method: Resample.Method,
    private val backend: ResamplingBackend
):
  def executionModel: ResamplingExecutionModel =
    backend.executionModel

  /** Number of coordinate triples retained by the immutable plan. */
  def materializedCoordinateCount: Int =
    backend.materializedCoordinateCount

  def apply(volume: NeuroVol[Double]): Either[ResamplingPlanError, NeuroVol[Double]] =
    apply(volume, outside = 0.0)

  def apply(volume: NeuroVol[Double], outside: Double): Either[ResamplingPlanError, NeuroVol[Double]] =
    apply(volume, outside, JacobianModulation.None)

  def apply(
      volume: NeuroVol[Double],
      outside: Double,
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, NeuroVol[Double]] =
    val actual = GridSpec.fromSpace(volume.space)
    if GridCompatibility
        .exact(source.toNeuroSpace, actual.toNeuroSpace)
        .isLeft
    then Left(ResamplingPlanError.SourceSpaceMismatch(source, actual))
    else
      modulationFactors(modulation).flatMap: factors =>
        backend match
          case affine: ResamplingBackend.AffineProvider =>
            sampleVolumeWithProvider(volume, outside, factors, affine)
          case prepared: ResamplingBackend.WorkloadPrepared =>
            Right(samplePreparedVolume(volume, outside, factors, prepared))

  @scala.annotation.targetName("applyNeuroVec")
  def apply(vec: NeuroVec[Double]): Either[ResamplingPlanError, NeuroVec[Double]] =
    apply(vec, outside = 0.0)

  @scala.annotation.targetName("applyNeuroVecOutside")
  def apply(vec: NeuroVec[Double], outside: Double): Either[ResamplingPlanError, NeuroVec[Double]] =
    apply(vec, outside, JacobianModulation.None)

  @scala.annotation.targetName("applyNeuroVecModulated")
  def apply(
      vec: NeuroVec[Double],
      outside: Double,
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, NeuroVec[Double]] =
    val actual = GridSpec.fromSpace(vec.space)
    if GridCompatibility
        .exact(source.toNeuroSpace, actual.toNeuroSpace)
        .isLeft
    then Left(ResamplingPlanError.SourceSpaceMismatch(source, actual))
    else
      modulationFactors(modulation).flatMap: factors =>
        backend match
          case affine: ResamplingBackend.AffineProvider =>
            sampleSeriesWithProvider(vec, outside, factors, affine)
          case prepared: ResamplingBackend.WorkloadPrepared =>
            Right(samplePreparedSeries(vec, outside, factors, prepared))

  private def sampleVolumeWithProvider(
      volume: NeuroVol[Double],
      outside: Double,
      factors: ModulationFactors,
      backend: ResamplingBackend.AffineProvider
  ): Either[ResamplingPlanError, NeuroVol[Double]] =
    given DType[Double] = volume.values.dtype
    val targetShape = target.shape
    var failure = Option.empty[ResamplingPlanError]
    val out =
      RavelArray.build[Double, Rank[3]](
        Shape(targetShape.x, targetShape.y, targetShape.z)
      ): output =>
        failure =
          scanProvider(
            ContinuousImageRefinement.volume(volume),
            output,
            trailingSize = 1,
            outside,
            factors,
            backend
          ).left.toOption
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(NeuroVol.fromRavel(out, target.toNeuroSpace, volume.label))

  private def sampleSeriesWithProvider(
      vec: NeuroVec[Double],
      outside: Double,
      factors: ModulationFactors,
      backend: ResamplingBackend.AffineProvider
  ): Either[ResamplingPlanError, NeuroVec[Double]] =
    given DType[Double] = vec.values.dtype
    val targetShape = target.shape
    val tLen = vec.nVolumes
    var failure = Option.empty[ResamplingPlanError]
    val out =
      RavelArray.build[Double, Rank[4]](
        Shape(targetShape.x, targetShape.y, targetShape.z, tLen)
      ): output =>
        failure =
          scanProvider(
            ContinuousImageRefinement.series(vec),
            output,
            trailingSize = tLen,
            outside,
            factors,
            backend
          ).left.toOption
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          NeuroVec.fromRavel(
            out,
            target.toNeuroSpace.addDim(tLen, Some(Axis.Time)),
            vec.label
          )
        )

  /** View-safe execution into one whole-canonical destination. Input volumes
    * may be arbitrary immutable Ravel views; coordinate access honors their
    * strides and no hidden canonicalization occurs.
    */
  private def samplePreparedVolume(
      volume: NeuroVol[Double],
      outside: Double,
      factors: ModulationFactors,
      backend: ResamplingBackend.WorkloadPrepared
  ): NeuroVol[Double] =
    given DType[Double] = volume.values.dtype
    val dims = source.shape
    val targetShape = target.shape
    val workspace = new CubicWorkspace
    val out =
      RavelArray.build[Double, Rank[3]](
        Shape(targetShape.x, targetShape.y, targetShape.z)
      ): output =>
        var ordinal = 0
        while ordinal < backend.sourceVoxelPoints.length do
          val coord = backend.sourceVoxelPoints(ordinal)
          val sampled =
            sampleAt(volume, dims, coord, outside, workspace)
          output.writeLinear(
            ordinal,
            sampled * factors.atVoxel(ordinal)
          )
          ordinal += 1
    NeuroVol.fromRavel(out, target.toNeuroSpace, volume.label)

  /** Series execution is fused across time so it retains one destination and
    * never materializes a temporary volume for each frame.
    */
  private def samplePreparedSeries(
      vec: NeuroVec[Double],
      outside: Double,
      factors: ModulationFactors,
      backend: ResamplingBackend.WorkloadPrepared
  ): NeuroVec[Double] =
    given DType[Double] = vec.values.dtype
    val tLen = vec.nVolumes
    val volumes = Vector.tabulate(tLen)(vec.volume)
    val dims = source.shape
    val targetShape = target.shape
    val workspace = new CubicWorkspace
    val out =
      RavelArray.build[Double, Rank[4]](
        Shape(targetShape.x, targetShape.y, targetShape.z, tLen)
      ): output =>
        var voxelOrdinal = 0
        while voxelOrdinal < backend.sourceVoxelPoints.length do
          val coord = backend.sourceVoxelPoints(voxelOrdinal)
          val factor = factors.atVoxel(voxelOrdinal)
          var time = 0
          while time < tLen do
            val sampled =
              sampleAt(
                volumes(time),
                dims,
                coord,
                outside,
                workspace
              )
            output.writeLinear(
              voxelOrdinal * tLen + time,
              sampled * factor
            )
            time += 1
          voxelOrdinal += 1
    NeuroVec.fromRavel(
      out,
      target.toNeuroSpace.addDim(tLen, Some(Axis.Time)),
      vec.label
    )

  private def scanProvider[R <: AnyRank](
      sampled: Sampled[
        SampleSpace[Frame[D3], D3],
        Double,
        Continuous,
        R
      ],
      output: ArrayBuilder[Double],
      trailingSize: Int,
      outside: Double,
      factors: ModulationFactors,
      backend: ResamplingBackend.AffineProvider
  ): Either[ResamplingPlanError, Unit] =
    val targetGrid = target.nativeGrid
    val pull =
      FramedAffine.betweenFrames[Frame[D3], Frame[D3], D3](
        targetGrid.frame,
        sampled.frame
      )(backend.pull)
    ReframeResamplingPlan
      .affine(
        sampled,
        targetGrid,
        pull,
        backend.interpolation,
        BoundaryPolicy.Constant(outside)
      )
      .left
      .map(error => ResamplingPlanError.ProviderFailure(error.message))
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
          .map(error => ResamplingPlanError.ProviderFailure(error.message))

  private inline def sampleAt(
      volume: NeuroVol[Double],
      dims: SpatialDims,
      coord: VoxelPoint,
      outside: Double,
      workspace: CubicWorkspace
  ): Double =
    method match
      case Resample.Method.Nearest =>
        VoxelSamplingKernel.nearest(
          volume.toNative,
          dims,
          coord.x,
          coord.y,
          coord.z,
          outside
        )
      case Resample.Method.Linear =>
        VoxelSamplingKernel.linear(
          volume.toNative,
          dims,
          coord.x,
          coord.y,
          coord.z,
          outside
        )
      case Resample.Method.Cubic =>
        VoxelSamplingKernel.cubic(
          volume.toNative,
          dims,
          coord.x,
          coord.y,
          coord.z,
          outside,
          workspace
        )

  private def modulationFactors(
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, ModulationFactors] =
    modulation match
      case JacobianModulation.None =>
        Right(ModulationFactors.Constant(1.0))
      case JacobianModulation.Jacobian | JacobianModulation.SqrtJacobian =>
        constantJacobianMagnitude match
          case Some(magnitude) =>
            Right(
              ModulationFactors.Constant(
                if modulation == JacobianModulation.SqrtJacobian then
                  math.sqrt(magnitude)
                else magnitude
              )
            )
          case None =>
            val targetWorldPoints =
              target.worldPoints.map(WorldPoint.fromSpatialPoint)
            morphism
              .jacobianDetAtWorld(
                targetWorldPoints,
                log = false,
                mode = JacobianMode.Pullback
              )
              .left
              .map(error =>
                ResamplingPlanError.MorphismEvaluationFailed(error.message)
              )
              .map: determinants =>
                ModulationFactors.PerVoxel(
                  determinants.map: determinant =>
                    val magnitude = math.abs(determinant)
                    if modulation == JacobianModulation.SqrtJacobian then
                      math.sqrt(magnitude)
                    else magnitude
                )

  private def constantJacobianMagnitude: Option[Double] =
    morphism match
      case _: IdentityMorphism => Some(1.0)
      case affine: Affine3DMorphism =>
        Some(math.abs(ResamplingPlan.linearDeterminant(affine.matrix)))
      case _ => None

object ResamplingPlan:
  def make(
      source: GridSpec,
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    providerBackend(morphism, method) match
      case Left(error) => Left(error)
      case Right(Some(backend)) =>
        Right(new ResamplingPlan(source, target, morphism, method, backend))
      case Right(None) =>
        val targetWorldPoints =
          target.worldPoints.map(WorldPoint.fromSpatialPoint)
        val sourceWorldPoints =
          morphism.transformWorldPoints(targetWorldPoints)
        source.worldPointsToVoxel(sourceWorldPoints) match
          case Left(error) =>
            Left(ResamplingPlanError.SingularSourceAffine(error.message))
          case Right(sourceVoxelPoints) =>
            Right(
              new ResamplingPlan(
                source,
                target,
                morphism,
                method,
                ResamplingBackend.WorkloadPrepared(sourceVoxelPoints)
              )
            )

  def identity(
      source: GridSpec,
      method: Resample.Method = Resample.Method.Linear
  ): Either[ResamplingPlanError, ResamplingPlan] =
    val domain = SpatialDomainId("identity")
    make(source, source, IdentityMorphism(domain), method)

  def fromSpaces(
      source: NeuroSpace,
      target: NeuroSpace,
      morphism: SpatialMorphism,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    make(GridSpec.fromSpace(source), GridSpec.fromSpace(target), morphism, method)

  private def providerBackend(
      morphism: SpatialMorphism,
      method: Resample.Method
  ): Either[ResamplingPlanError, Option[ResamplingBackend.AffineProvider]] =
    val interpolation =
      method match
        case Resample.Method.Nearest =>
          Some(Interpolation.Nearest: Interpolation[Continuous])
        case Resample.Method.Linear =>
          Some(Interpolation.Linear)
        case Resample.Method.Cubic => None

    interpolation match
      case None => Right(None)
      case Some(kernel) =>
        morphism match
          case _: IdentityMorphism =>
            Right(
              Some(
                ResamplingBackend.AffineProvider(
                  GeometryAffine.identity[D3],
                  kernel
                )
              )
            )
          case affine: Affine3DMorphism =>
            GeometryAffine
              .fromRowMajor[D3](
                Vector.tabulate(16)(index => affine.matrix.data(index))
              )
              .left
              .map(error =>
                ResamplingPlanError.MorphismEvaluationFailed(error.message)
              )
              .map(operator =>
                Some(
                  ResamplingBackend.AffineProvider(operator, kernel)
                )
              )
          case _ => Right(None)

  private def linearDeterminant(matrix: DMat): Double =
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
