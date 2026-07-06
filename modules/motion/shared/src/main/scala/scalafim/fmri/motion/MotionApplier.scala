package scalafim.fmri.motion

import scalafim.image.{Axis, NeuroVec, NArrayUtil}

object MotionApplier:
  def apply(
      run: NeuroVec[Double],
      trace: MotionTrace,
      control: ApplyControl = ApplyControl.linear
  ): Either[MotionError, NeuroVec[Double]] =
    if trace.length != run.nVolumes then Left(MotionError.TraceLengthMismatch(trace.length, run.nVolumes))
    else
      control.interpolation match
        case Interpolation.Linear =>
          Right(applyLinear(run, trace, control))

  private def applyLinear(
      run: NeuroVec[Double],
      trace: MotionTrace,
      control: ApplyControl
  ): NeuroVec[Double] =
    val dims = run.space.spatialDims
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nt = run.nVolumes
    val nxyz = nx * ny * nz
    val out = NArrayUtil.ofSize[Double](nxyz * nt)
    val px = run.space.spacing(0)
    val py = run.space.spacing(1)
    val pz = run.space.spacing(2)
    val zeroPad = control.padMode == PadMode.Zero || control.zpad > 0

    var t = 0
    while t < nt do
      val pose = trace.unsafeFrame(t)
      val map = MotionSampling.voxelMap(nx, ny, nz, control.zpad, px, py, pz, pose)
      var k = 0
      while k < nz do
        var j = 0
        while j < ny do
          var i = 0
          while i < nx do
            val sx = MotionSampling.sourceX(map, i, j, k)
            val sy = MotionSampling.sourceY(map, i, j, k)
            val sz = MotionSampling.sourceZ(map, i, j, k)
            val dst = i + nx * (j + ny * (k + nz * t))
            out(dst) = MotionSampling.trilinear(run.values.data, nx, ny, nz, nxyz, t, sx, sy, sz, zeroPad)
            i += 1
          j += 1
        k += 1
      t += 1

    NeuroVec.fromLinear(out, run.space.spatialSpace.addDim(nt, Some(Axis.Time)), run.label)
