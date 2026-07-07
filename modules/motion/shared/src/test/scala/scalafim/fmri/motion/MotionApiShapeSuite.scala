package scalafim.fmri.motion

import scalafim.image.{Axis, NeuroSpace, NeuroVec, NArrayUtil}

class MotionApiShapeSuite extends munit.FunSuite:

  private def run(values: Vector[Double]): NeuroVec[Double] =
    NeuroVec.fromLinear(
      NArrayUtil.tabulate[Double](values.length)(values),
      NeuroSpace(Vector(1, 1, 1)).addDim(values.length, Some(Axis.Time)),
      "api-shape"
    )

  private def diagnostics(n: Int): Vector[FrameFitDiagnostics] =
    Vector.tabulate(n) { i =>
      FrameFitDiagnostics(
        costInitial = 1.0,
        costFinal = if i == 0 then 1.0 else 0.5,
        iterations = 0,
        overlap = 1.0,
        restarted = false,
        converged = true
      )
    }

  test("slice and packet timing are validated typed acquisition descriptions") {
    val slice = SliceTiming.make(Vector(0.0, 0.5, 0.25)).fold(err => fail(err.message), identity)
    assertEquals(slice.nSlices, 3)
    assertEqualsDouble(slice.offset(1).fold(err => fail(err.message), _.value), 0.5, 1e-12)
    assert(SliceTiming.make(Vector(0.0, Double.NaN)).isLeft)

    val packet0 = SlicePacket.make(Vector(0, 2), 0.0).fold(err => fail(err.message), identity)
    val packet1 = SlicePacket.make(Vector(1), 0.5).fold(err => fail(err.message), identity)
    val packet = PacketTiming.make(3, Vector(packet0, packet1)).fold(err => fail(err.message), identity)
    assertEquals(packet.toSliceTiming.offsetSeconds, Vector(0.0, 0.5, 0.0))
    assert(PacketTiming.make(3, Vector(packet0)).isLeft)
    assert(PacketTiming.make(3, Vector(packet0, SlicePacket.unsafe(Vector(2), 0.5))).isLeft)
  }

  test("typed plan surfaces support apply timing and reject planned estimator modes") {
    val x = run(Vector(0.0, 1.0))
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val sliceTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0)))
    val applyControl = ApplyControl.make(acquisitionTiming = sliceTiming).fold(err => fail(err.message), identity)
    val badApplyControl =
      ApplyControl
        .make(acquisitionTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0, 0.5))))
        .fold(err => fail(err.message), identity)

    assert(MotionApplier.apply(x, trace, applyControl).isRight)
    assert(MotionApplier.apply(x, trace, badApplyControl).isLeft)

    val parallel =
      MotionControl.default.copy(
        execution = ExecutionControl(parallelFrames = true, nThreads = 2)
      )
    val plan = MotionPlan.default.copy(control = parallel)
    assert(MotionEstimator.estimate(x, None, plan).isLeft)

    val timedPlan = MotionPlan.default.copy(acquisitionTiming = sliceTiming)
    assert(MotionEstimator.estimate(x, None, timedPlan).isLeft)
  }

  test("motion profiles expose typed active and planned capabilities") {
    assert(MotionProfile.FastFmri.activeCapabilities.contains(MotionCapability.DenseSampling))
    assert(MotionProfile.FastFmri.activeCapabilities.contains(MotionCapability.ValidTemplateRefresh))
    assert(MotionProfile.FastFmri.plannedCapabilities.contains(MotionCapability.ParallelFrames))
    assertEquals(MotionProfile.FastFmri.components, MotionProfile.FastFmri.activeCapabilities.map(_.label))
    assert(MotionProfile.IcWhiten.plannedCapabilities.contains(MotionCapability.Whitening))
    assertEquals(MotionProfile.IcWhiten.control.whitening.policy, WhiteningPolicy.IcWhiten)
    assert(!MotionProfile.IcWhiten.control.whitening.implemented)
  }

  test("motion correction result bundles estimate, corrected run, QC, and controls") {
    val x = run(Vector(0.0, 1.0))
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val estimate =
      MotionEstimate(
        trace = trace,
        diagnostics = diagnostics(2),
        control = MotionControl.default
      )
    val result =
      MotionCorrectionResult
        .fromEstimate(x, estimate, MotionPlan.default)
        .fold(err => fail(err.message), identity)

    assert(result.hasCorrectedRun)
    assert(result.corrected.exists(_.space == x.space))
    assert(result.qc.exists(_.fitCostTrace.isMissing == false))
    assertEquals(result.trace.length, 2)
    assertEquals(result.applyControl, Some(ApplyControl.linear))
  }
