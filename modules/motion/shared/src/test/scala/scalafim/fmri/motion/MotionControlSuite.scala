package scalafim.fmri.motion

class MotionControlSuite extends munit.FunSuite:

  test("pyramid control validates schedule lengths when enabled") {
    val bad =
      PyramidControl.make(
        downsample = Vector(4, 2, 1),
        maxIterations = Vector(10, 5),
        sampleCounts = Vector(100, 200, 300),
        enabled = true
      )
    assert(bad.isLeft)
  }

  test("apply control rejects negative zpad") {
    assert(ApplyControl.make(zpad = -1).isLeft)
  }

  test("fast fMRI profile selects rigid robust engine and component tags") {
    assertEquals(MotionProfile.FastFmri.engine, MotionEngine.RigidRobust)
    assert(MotionProfile.FastFmri.components.contains("dense_sampling"))
    assert(!MotionProfile.FastFmri.components.contains("ic_stencil"))
    assert(MotionProfile.FastFmri.plannedComponents.contains("valid_template_refresh"))
    assert(MotionProfile.FastFmri.plannedComponents.contains("parallel_frames"))
    assert(!MotionProfile.FastFmri.control.execution.parallelFrames)
    assert(MotionProfile.FastFmri.plan().isRight)
  }

  test("slice spline profile selects spline engine") {
    assertEquals(MotionProfile.SliceSpline.engine, MotionEngine.RigidSpline)
    assert(!MotionProfile.SliceSpline.implemented)
    assert(MotionProfile.SliceSpline.plan().isLeft)
  }

  test("whitening and IC profiles are advertised as planned, not executable") {
    assert(MotionProfile.IcStencil.plannedComponents.contains("ic_stencil"))
    assert(MotionProfile.IcWhiten.plannedComponents.contains("whiten"))
    assert(MotionProfile.IcStencil.components.isEmpty)
    assert(MotionProfile.IcWhiten.components.isEmpty)
    assert(MotionProfile.IcStencil.plan().isLeft)
    assert(MotionProfile.IcWhiten.plan().isLeft)
  }
