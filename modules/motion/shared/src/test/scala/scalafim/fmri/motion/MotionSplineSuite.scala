package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures

class MotionSplineSuite extends munit.FunSuite:

  private val fixture = VolreggerFixtures.coreFixture

  private def poseFrom(values: Vector[Double]): RigidPose =
    assertEquals(values.length, 6)
    RigidPose.unsafe(values(0), values(1), values(2), values(3), values(4), values(5))

  private def assertPoseClose(actual: RigidPose, expected: RigidPose, tol: Double): Unit =
    assertEqualsDouble(actual.tx, expected.tx, tol)
    assertEqualsDouble(actual.ty, expected.ty, tol)
    assertEqualsDouble(actual.tz, expected.tz, tol)
    assertEqualsDouble(actual.rx, expected.rx, tol)
    assertEqualsDouble(actual.ry, expected.ry, tol)
    assertEqualsDouble(actual.rz, expected.rz, tol)

  private def splineTrace(): MotionTrace =
    MotionTrace.unsafe(fixture.doubles("spline_pose_row_major").grouped(6).map(poseFrom).toVector)

  test("pose smoothing matches generated volregger spline fixture") {
    val trace = splineTrace()
    val smoothIter = fixture.doubles("spline_smooth_iter").head.toInt
    val spline = PoseSpline.smooth(trace, smoothIter).fold(err => fail(err.message), identity)
    val expected = fixture.doubles("spline_smoothed_pose_row_major").grouped(6).map(poseFrom).toVector

    assertEquals(spline.trace.length, expected.length)
    var t = 0
    while t < expected.length do
      assertPoseClose(spline.trace.unsafeFrame(t), expected(t), 1e-12)
      t += 1
  }

  test("pose interpolation clamps to valid frame span") {
    val spline = PoseSpline.fromTrace(splineTrace())
    val half = spline.poseAt(1, 0.5).fold(err => fail(err.message), identity)
    val beyondEnd = spline.poseAt(3, 1.0).fold(err => fail(err.message), identity)

    assertPoseClose(half, RigidPose.unsafe(1.5, 3.0, 0.0, 0.05, 0.0, 0.15), 1e-12)
    assertPoseClose(beyondEnd, spline.trace.unsafeFrame(3), 1e-12)
    assert(spline.poseAt(-1, 0.0).isLeft)
    assert(spline.poseAt(0, Double.NaN).isLeft)
  }

  test("packet correction magnitude is zero for all-zero offsets and positive for staggered offsets") {
    val trace = splineTrace()
    val zeroTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0, 0.0, 0.0, 0.0)))
    val staggeredTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(fixture.doubles("spline_slice_times")))
    val zero = PoseSpline.packetCorrectionMagnitude(trace, zeroTiming, nSlices = 4).fold(err => fail(err.message), identity)
    val staggered = PoseSpline.packetCorrectionMagnitude(trace, staggeredTiming, nSlices = 4).fold(err => fail(err.message), identity)

    assertEquals(zero, Vector.fill(trace.length)(0.0))
    assert(staggered.exists(_ > 0.0))
    assert(staggered.forall(value => value.isFinite && value >= 0.0))
    assert(PoseSpline.packetCorrectionMagnitude(trace, staggeredTiming, nSlices = 3).isLeft)
  }
