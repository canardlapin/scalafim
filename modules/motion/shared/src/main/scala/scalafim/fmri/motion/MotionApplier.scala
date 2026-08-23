package scalafim.fmri.motion

import ravel.NDArray
import ravel.Rank
import ravel.Shape
import scalafim.image.NeuroVec

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
          for
            offsets <- control.acquisitionTiming.normalizedSliceOffsets(run.space.spatialDims(2))
            corrected <-
              if offsets.forall(_ == 0.0) then Right(applyLinear(run, trace, control))
              else applyLinearPacketAware(run, trace, control, offsets)
          yield corrected

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
    val px = run.space.spacing(0)
    val py = run.space.spacing(1)
    val pz = run.space.spacing(2)
    val zeroPad = control.padMode == PadMode.Zero || control.zpad > 0

    val out =
      NDArray.build[Double, Rank[4]](Shape(nx, ny, nz, nt)):
        output =>
          var time = 0
          while time < nt do
            val pose = trace.unsafeFrame(time)
            val map =
              MotionSampling.voxelMap(
                nx,
                ny,
                nz,
                control.zpad,
                px,
                py,
                pz,
                pose
              )
            var k = 0
            while k < nz do
              var j = 0
              while j < ny do
                var i = 0
                while i < nx do
                  val sx = MotionSampling.sourceX(map, i, j, k)
                  val sy = MotionSampling.sourceY(map, i, j, k)
                  val sz = MotionSampling.sourceZ(map, i, j, k)
                  val destination =
                    (((i * ny) + j) * nz + k) * nt + time
                  output.writeLinear(
                    destination,
                    MotionSampling.trilinear(
                      run.values,
                      nx,
                      ny,
                      nz,
                      time,
                      sx,
                      sy,
                      sz,
                      zeroPad
                    )
                  )
                  i += 1
                j += 1
              k += 1
            time += 1

    NeuroVec.fromRavel(out, run.space, run.label)

  private def applyLinearPacketAware(
      run: NeuroVec[Double],
      trace: MotionTrace,
      control: ApplyControl,
      normalizedOffsets: Vector[Double]
  ): Either[MotionError, NeuroVec[Double]] =
    val dims = run.space.spatialDims
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nt = run.nVolumes
    val px = run.space.spacing(0)
    val py = run.space.spacing(1)
    val pz = run.space.spacing(2)
    val zeroPad = control.padMode == PadMode.Zero || control.zpad > 0
    val spline = PoseSpline.fromTrace(trace)

    val poses = Vector.newBuilder[RigidPose]
    poses.sizeHint(nt * nz)
    var time = 0
    while time < nt do
      var k = 0
      while k < nz do
        spline.poseAt(time, normalizedOffsets(k)) match
            case Left(error) => return Left(error)
            case Right(value) => poses += value
        k += 1
      time += 1
    val preparedPoses = poses.result()

    val out =
      NDArray.build[Double, Rank[4]](Shape(nx, ny, nz, nt)):
        output =>
          time = 0
          while time < nt do
            var k = 0
            while k < nz do
              val map =
                MotionSampling.voxelMap(
                  nx,
                  ny,
                  nz,
                  control.zpad,
                  px,
                  py,
                  pz,
                  preparedPoses(time * nz + k)
                )
              var j = 0
              while j < ny do
                var i = 0
                while i < nx do
                  val sx = MotionSampling.sourceX(map, i, j, k)
                  val sy = MotionSampling.sourceY(map, i, j, k)
                  val sz = MotionSampling.sourceZ(map, i, j, k)
                  val destination =
                    (((i * ny) + j) * nz + k) * nt + time
                  output.writeLinear(
                    destination,
                    MotionSampling.trilinear(
                      run.values,
                      nx,
                      ny,
                      nz,
                      time,
                      sx,
                      sy,
                      sz,
                      zeroPad
                    )
                  )
                  i += 1
                j += 1
              k += 1
            time += 1

    Right(NeuroVec.fromRavel(out, run.space, run.label))
