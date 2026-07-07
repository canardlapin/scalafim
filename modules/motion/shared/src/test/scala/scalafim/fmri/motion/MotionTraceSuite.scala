package scalafim.fmri.motion

class MotionTraceSuite extends munit.FunSuite:

  test("motion trace must be non-empty") {
    assertEquals(MotionTrace.make(Vector.empty).left.map(_.message).isLeft, true)
  }

  test("identity trace has requested length") {
    val trace = MotionTrace.identity(3).fold(err => fail(err.message), identity)
    assertEquals(trace.length, 3)
    assertEquals(trace.frameCount.value, 3)
    assertEquals(trace.poses, Vector.fill(3)(RigidPose.identity))
  }

  test("motion trace exposes frame-aligned pose storage") {
    val trace = MotionTrace.identity(FrameCount.unsafe(2))
    assertEquals(trace.aligned.length, 2)
    val second = trace.aligned(FrameIndex.unsafe(1)).fold(err => fail(err.message), identity)
    assertEquals(second, RigidPose.identity)
  }

  test("frame access validates bounds") {
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val first = trace(FrameIndex.unsafe(0)).fold(err => fail(err.message), identity)
    assertEquals(first, RigidPose.identity)
    assert(trace(FrameIndex.unsafe(2)).isLeft)
  }
