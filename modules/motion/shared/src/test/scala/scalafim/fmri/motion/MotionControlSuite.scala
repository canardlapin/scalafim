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
    val badSampleCounts =
      PyramidControl.make(
        downsample = Vector(4, 2, 1),
        maxIterations = Vector(10, 5, 2),
        sampleCounts = Vector(100, 200),
        enabled = true
      )
    val sharedBudget =
      PyramidControl.make(
        downsample = Vector(4, 2, 1),
        maxIterations = Vector(5),
        sampleCounts = Vector(0),
        enabled = true
      )
    assert(bad.isLeft)
    assert(badSampleCounts.isLeft)
    assert(sharedBudget.isRight)
  }

  test("apply control rejects negative zpad") {
    assert(ApplyControl.make(zpad = -1).isLeft)
  }

  test("temporal shrink controls validate shrink range and threshold") {
    intercept[IllegalArgumentException] {
      TemporalControl(
        regularizationEnabled = false,
        lowMotionPoseShrink = true,
        lowMotionPoseScale = 0.0,
        lowMotionThresholdMm = 0.25
      )
    }
    intercept[IllegalArgumentException] {
      TemporalControl(
        regularizationEnabled = false,
        lowMotionPoseShrink = true,
        lowMotionPoseScale = 0.5,
        lowMotionThresholdMm = -0.1
      )
    }
  }

  test("capture and temporal controls expose typed policies with Boolean adapters") {
    val capture = CaptureControl.default.withEnabled(false)
    assertEquals(capture.policy, CapturePolicy.Disabled)
    assert(!capture.enabled)

    val temporal =
      TemporalControl(
        regularizationEnabled = false,
        lowMotionPoseShrink = false,
        lowMotionPoseScale = 0.5,
        lowMotionThresholdMm = 0.75
      )
    val shrink = temporal.withLowMotionPoseShrink(true)
    val regularized = temporal.withRegularizationEnabled(true)
    assertEquals(regularized.regularization, TemporalRegularizationPolicy.Enabled)
    assert(regularized.regularizationEnabled)
    assertEquals(shrink.lowMotionPose, LowMotionPosePolicy.Shrink(PoseScale.unsafe(0.5), MotionMagnitudeMm.unsafe(0.75)))
    assert(shrink.lowMotionPoseShrink)
    assertEqualsDouble(shrink.lowMotionPoseScale, 0.5, 1e-12)
    assertEqualsDouble(shrink.lowMotionThresholdMm, 0.75, 1e-12)
  }

  test("residual control exposes frame-mean nuisance policy") {
    val raw = ResidualControl.default
    val nuisance = ResidualControl.fromRemoveFrameMean(removeFrameMean = true)

    assertEquals(raw.nuisance, ResidualNuisancePolicy.Raw)
    assert(!raw.removeFrameMean)
    assertEquals(nuisance.nuisance, ResidualNuisancePolicy.RemoveFrameMean)
    assert(nuisance.removeFrameMean)
  }

  test("fast fMRI profile selects rigid robust engine and component tags") {
    assertEquals(MotionProfile.FastFmri.engine, MotionEngine.RigidRobust)
    assert(MotionProfile.FastFmri.components.contains("dense_sampling"))
    assert(MotionProfile.FastFmri.components.contains("rotational_capture"))
    assert(MotionProfile.FastFmri.components.contains("valid_template_refresh"))
    assert(!MotionProfile.FastFmri.components.contains("ic_stencil"))
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
