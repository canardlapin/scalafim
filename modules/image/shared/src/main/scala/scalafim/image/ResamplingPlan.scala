package scalafim.image


enum ResamplingPlanError:
  case SingularSourceAffine(reason: String)
  case SourceSpaceMismatch(expected: GridSpec, actual: GridSpec)
  case UnsupportedMethod(method: Resample.Method)
  case MorphismEvaluationFailed(reason: String)

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

enum JacobianModulation:
  case None, Jacobian, SqrtJacobian

final case class ResamplingPlan private (
    source: GridSpec,
    target: GridSpec,
    morphism: SpatialMorphism,
    method: Resample.Method,
    targetWorldPoints: Vector[WorldPoint],
    sourceWorldPoints: Vector[WorldPoint],
    sourceVoxelPoints: Vector[VoxelPoint]
):
  require(targetWorldPoints.length == target.nVoxels, "target coordinate count must match target grid")
  require(sourceWorldPoints.length == targetWorldPoints.length, "source world coordinate count mismatch")
  require(sourceVoxelPoints.length == targetWorldPoints.length, "source voxel coordinate count mismatch")

  def targetWorldCoords: Vector[Vector[Double]] =
    targetWorldPoints.map(_.toVector)

  def sourceWorldCoords: Vector[Vector[Double]] =
    sourceWorldPoints.map(_.toVector)

  def sourceVoxelCoords: Vector[Vector[Double]] =
    sourceVoxelPoints.map(_.toVector)

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
      val sampled =
        method match
          case Resample.Method.Nearest =>
            Right(sampleNearest(volume, outside))
          case Resample.Method.Linear =>
            Right(sampleLinear(volume, outside))
          case Resample.Method.Cubic =>
            Right(sampleCubic(volume, outside))
      sampled.flatMap(vol => modulate(vol, modulation))

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
      method match
        case Resample.Method.Nearest =>
          sampleVolumes(vec, volume => modulate(sampleNearest(volume, outside), modulation))
        case Resample.Method.Linear =>
          sampleVolumes(vec, volume => modulate(sampleLinear(volume, outside), modulation))
        case Resample.Method.Cubic =>
          sampleVolumes(vec, volume => modulate(sampleCubic(volume, outside), modulation))

  private def sampleVolumes(
      vec: NeuroVec[Double],
      sample: NeuroVol[Double] => Either[ResamplingPlanError, NeuroVol[Double]]
  ): Either[ResamplingPlanError, NeuroVec[Double]] =
    val tLen = vec.nVolumes
    val spatialNels = target.nVoxels
    val out = PrimitiveBuffers.ofSize[Double](spatialNels * tLen)
    var t = 0
    var error = Option.empty[ResamplingPlanError]
    while t < tLen && error.isEmpty do
      sample(vec.volume(t)) match
        case Left(err) =>
          error = Some(err)
        case Right(sampled) =>
          var index = 0
          while index < spatialNels do
            out(index * tLen + t) =
              sampled.valueAtCanonicalOrdinal(index)
            index += 1
      t += 1
    error match
      case Some(err) => Left(err)
      case None => Right(NeuroVec.copyFromCanonicalArray(out, target.toNeuroSpace.addDim(tLen, Some(Axis.Time)), vec.label))

  private def sampleNearest(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = PrimitiveBuffers.ofSize[Double](target.nVoxels)
    var i = 0
    while i < sourceVoxelPoints.length do
      val coord = sourceVoxelPoints(i)
      out(i) = VoxelSamplingKernel.nearest(volume, dims, coord.x, coord.y, coord.z, outside)
      i += 1
    NeuroVol.copyFromCanonicalArray(out, target.toNeuroSpace, volume.label)

  private def sampleLinear(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = Array.ofDim[Double](target.nVoxels)
    var i = 0
    while i < sourceVoxelPoints.length do
      val coord = sourceVoxelPoints(i)
      out(i) = VoxelSamplingKernel.linear(volume, dims, coord.x, coord.y, coord.z, outside)
      i += 1

    NeuroVol.copyFromCanonicalArray(out, target.toNeuroSpace, volume.label)

  private def sampleCubic(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = PrimitiveBuffers.ofSize[Double](target.nVoxels)
    val workspace = new CubicWorkspace
    var i = 0
    while i < sourceVoxelPoints.length do
      val coord = sourceVoxelPoints(i)
      out(i) = VoxelSamplingKernel.cubic(volume, dims, coord.x, coord.y, coord.z, outside, workspace)
      i += 1

    NeuroVol.copyFromCanonicalArray(out, target.toNeuroSpace, volume.label)

  private def modulate(
      volume: NeuroVol[Double],
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, NeuroVol[Double]] =
    modulation match
      case JacobianModulation.None =>
        Right(volume)
      case JacobianModulation.Jacobian | JacobianModulation.SqrtJacobian =>
        morphism.jacobianDetAtWorld(targetWorldPoints, log = false, mode = JacobianMode.Pullback) match
          case Left(err) =>
            Left(ResamplingPlanError.MorphismEvaluationFailed(err.message))
          case Right(dets) =>
            val out = PrimitiveBuffers.ofSize[Double](target.nVoxels)
            var i = 0
            while i < target.nVoxels do
              val base = math.abs(dets(i))
              val factor =
                modulation match
                  case JacobianModulation.SqrtJacobian => math.sqrt(base)
                  case _ => base
              out(i) = volume.valueAtCanonicalOrdinal(i) * factor
              i += 1
            Right(NeuroVol.copyFromCanonicalArray(out, target.toNeuroSpace, volume.label))

object ResamplingPlan:
  def make(
      source: GridSpec,
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    val targetWorldPoints = target.worldPoints.map(WorldPoint.fromSpatialPoint)
    val sourceWorldPoints = morphism.transformWorldPoints(targetWorldPoints)
    source.worldPointsToVoxel(sourceWorldPoints) match
      case Left(err) =>
        Left(ResamplingPlanError.SingularSourceAffine(err.message))
      case Right(sourceVoxelPoints) =>
        Right(
          new ResamplingPlan(
            source = source,
            target = target,
            morphism = morphism,
            method = method,
            targetWorldPoints = targetWorldPoints,
            sourceWorldPoints = sourceWorldPoints,
            sourceVoxelPoints = sourceVoxelPoints
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
