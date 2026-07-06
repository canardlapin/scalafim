package scalafim.image

import narr.NArray

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
    targetWorldCoords: Vector[Vector[Double]],
    sourceWorldCoords: Vector[Vector[Double]],
    sourceVoxelCoords: Vector[Vector[Double]]
):
  require(targetWorldCoords.length == target.nVoxels, "target coordinate count must match target grid")
  require(sourceWorldCoords.length == targetWorldCoords.length, "source world coordinate count mismatch")
  require(sourceVoxelCoords.length == targetWorldCoords.length, "source voxel coordinate count mismatch")

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
    if actual != source then Left(ResamplingPlanError.SourceSpaceMismatch(source, actual))
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

  def apply(vec: NeuroVec[Double]): Either[ResamplingPlanError, NeuroVec[Double]] =
    apply(vec, outside = 0.0)

  def apply(vec: NeuroVec[Double], outside: Double): Either[ResamplingPlanError, NeuroVec[Double]] =
    apply(vec, outside, JacobianModulation.None)

  def apply(
      vec: NeuroVec[Double],
      outside: Double,
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, NeuroVec[Double]] =
    val actual = GridSpec.fromSpace(vec.space)
    if actual != source then Left(ResamplingPlanError.SourceSpaceMismatch(source, actual))
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
    val out = NArrayUtil.ofSize[Double](spatialNels * tLen)
    var t = 0
    var error = Option.empty[ResamplingPlanError]
    while t < tLen && error.isEmpty do
      sample(vec.volume(t)) match
        case Left(err) =>
          error = Some(err)
        case Right(sampled) =>
          NArrayUtil.copyInto(sampled.values.data, 0, out, t * spatialNels, spatialNels)
      t += 1
    error match
      case Some(err) => Left(err)
      case None => Right(NeuroVec.fromLinear(out, target.toNeuroSpace.addDim(tLen, Some(Axis.Time)), vec.label))

  private def sampleNearest(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = NArrayUtil.ofSize[Double](target.nVoxels)
    var i = 0
    while i < sourceVoxelCoords.length do
      val coord = sourceVoxelCoords(i)
      val x = math.round(coord(0)).toInt
      val y = math.round(coord(1)).toInt
      val z = math.round(coord(2)).toInt
      out(i) =
        if inBounds(dims, x, y, z) then volume(x, y, z)
        else outside
      i += 1
    NeuroVol.fromLinear(out, target.toNeuroSpace, volume.label)

  private def sampleLinear(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = NArray.ofSize[Double](target.nVoxels)

    inline def sample(x: Int, y: Int, z: Int): Double =
      if inBounds(dims, x, y, z) then volume(x, y, z) else outside

    var i = 0
    while i < sourceVoxelCoords.length do
      val coord = sourceVoxelCoords(i)
      val sx = coord(0)
      val sy = coord(1)
      val sz = coord(2)

      val x0 = math.floor(sx).toInt
      val y0 = math.floor(sy).toInt
      val z0 = math.floor(sz).toInt
      val x1 = x0 + 1
      val y1 = y0 + 1
      val z1 = z0 + 1

      val xd = sx - x0
      val yd = sy - y0
      val zd = sz - z0

      val c000 = sample(x0, y0, z0)
      val c100 = sample(x1, y0, z0)
      val c010 = sample(x0, y1, z0)
      val c110 = sample(x1, y1, z0)
      val c001 = sample(x0, y0, z1)
      val c101 = sample(x1, y0, z1)
      val c011 = sample(x0, y1, z1)
      val c111 = sample(x1, y1, z1)

      val c00 = c000 * (1 - xd) + c100 * xd
      val c10 = c010 * (1 - xd) + c110 * xd
      val c01 = c001 * (1 - xd) + c101 * xd
      val c11 = c011 * (1 - xd) + c111 * xd

      val c0 = c00 * (1 - yd) + c10 * yd
      val c1 = c01 * (1 - yd) + c11 * yd

      out(i) = c0 * (1 - zd) + c1 * zd
      i += 1

    NeuroVol.fromLinear(out, target.toNeuroSpace, volume.label)

  private def sampleCubic(volume: NeuroVol[Double], outside: Double): NeuroVol[Double] =
    val dims = source.shape
    val out = NArrayUtil.ofSize[Double](target.nVoxels)

    inline def sample(x: Int, y: Int, z: Int): Double =
      if inBounds(dims, x, y, z) then volume(x, y, z) else outside

    inline def cubic(p0: Double, p1: Double, p2: Double, p3: Double, t: Double): Double =
      val t2 = t * t
      val t3 = t2 * t
      0.5 * (
        (2.0 * p1) +
          (-p0 + p2) * t +
          (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
          (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3
      )

    val tmpY = Array.ofDim[Double](4)
    val tmpZ = Array.ofDim[Double](4)
    var i = 0
    while i < sourceVoxelCoords.length do
      val coord = sourceVoxelCoords(i)
      val sx = coord(0)
      val sy = coord(1)
      val sz = coord(2)
      val x1 = math.floor(sx).toInt
      val y1 = math.floor(sy).toInt
      val z1 = math.floor(sz).toInt
      val tx = sx - x1.toDouble
      val ty = sy - y1.toDouble
      val tz = sz - z1.toDouble

      var kk = 0
      while kk < 4 do
        val z = z1 + (kk - 1)
        var jj = 0
        while jj < 4 do
          val y = y1 + (jj - 1)
          val p0 = sample(x1 - 1, y, z)
          val p1 = sample(x1, y, z)
          val p2 = sample(x1 + 1, y, z)
          val p3 = sample(x1 + 2, y, z)
          tmpY(jj) = cubic(p0, p1, p2, p3, tx)
          jj += 1
        tmpZ(kk) = cubic(tmpY(0), tmpY(1), tmpY(2), tmpY(3), ty)
        kk += 1

      out(i) = cubic(tmpZ(0), tmpZ(1), tmpZ(2), tmpZ(3), tz)
      i += 1

    NeuroVol.fromLinear(out, target.toNeuroSpace, volume.label)

  private def modulate(
      volume: NeuroVol[Double],
      modulation: JacobianModulation
  ): Either[ResamplingPlanError, NeuroVol[Double]] =
    modulation match
      case JacobianModulation.None =>
        Right(volume)
      case JacobianModulation.Jacobian | JacobianModulation.SqrtJacobian =>
        morphism.jacobianDet(targetWorldCoords, log = false, mode = JacobianMode.Pullback) match
          case Left(err) =>
            Left(ResamplingPlanError.MorphismEvaluationFailed(err.message))
          case Right(dets) =>
            val out = NArrayUtil.ofSize[Double](target.nVoxels)
            var i = 0
            while i < target.nVoxels do
              val base = math.abs(dets(i))
              val factor =
                modulation match
                  case JacobianModulation.SqrtJacobian => math.sqrt(base)
                  case _ => base
              out(i) = volume.values.data(i) * factor
              i += 1
            Right(NeuroVol.fromLinear(out, target.toNeuroSpace, volume.label))

  private inline def inBounds(dims: SpatialDims, x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < dims.x &&
      y >= 0 && y < dims.y &&
      z >= 0 && z < dims.z

object ResamplingPlan:
  def make(
      source: GridSpec,
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Resample.Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    val targetWorld = target.worldCoords
    val sourceWorld = morphism.transform(targetWorld)
    source.worldsToVoxel(sourceWorld) match
      case Left(err) =>
        Left(ResamplingPlanError.SingularSourceAffine(err.message))
      case Right(sourceVoxelCoords) =>
        Right(
          new ResamplingPlan(
            source = source,
            target = target,
            morphism = morphism,
            method = method,
            targetWorldCoords = targetWorld,
            sourceWorldCoords = sourceWorld,
            sourceVoxelCoords = sourceVoxelCoords
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
