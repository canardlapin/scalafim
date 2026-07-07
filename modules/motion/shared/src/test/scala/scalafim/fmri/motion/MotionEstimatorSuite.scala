package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures
import scalafim.image.{Axis, NeuroSpace, NeuroVec, NeuroVol, NArrayUtil}

class MotionEstimatorSuite extends munit.FunSuite:

  private val dims = VolreggerFixtures.estimatorDims
  private val nxyz = dims.product
  private val space = NeuroSpace(dims)

  private def baseValueAt(x: Double, y: Double, z: Double): Double =
    val dx = x - 3.0
    val dy = y - 2.0
    val dz = z - 2.0
    10.0 * math.exp(-(dx * dx / 5.0 + dy * dy / 3.0 + dz * dz / 4.0)) +
      0.4 * x +
      0.2 * y -
      0.15 * z +
      0.35 * dx * dy +
      0.12 * dx * dx -
      0.08 * dy * dy +
      0.05 * dx * dz

  private def baseValue(i: Int, j: Int, k: Int): Double =
    baseValueAt(i.toDouble, j.toDouble, k.toDouble)

  private def baseFrame: Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val i = lin % dims(0)
      val j = (lin / dims(0)) % dims(1)
      val k = lin / (dims(0) * dims(1))
      baseValue(i, j, k)
    }

  private def shiftedMovingFramePlusOneX(fixed: Array[Double]): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val i = lin % dims(0)
      val j = (lin / dims(0)) % dims(1)
      val k = lin / (dims(0) * dims(1))
      val srcI = math.min(dims(0) - 1, i + 1)
      fixed(srcI + dims(0) * (j + dims(1) * k))
    }

  private def shiftedMovingFrameX(offset: Double): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val i = lin % dims(0)
      val j = (lin / dims(0)) % dims(1)
      val k = lin / (dims(0) * dims(1))
      baseValueAt(i.toDouble + offset, j.toDouble, k.toDouble)
    }

  private def offsetFrame(fixed: Array[Double], offset: Double): Array[Double] =
    Array.tabulate(nxyz)(lin => fixed(lin) + offset)

  private def rotatedMovingFramePositiveZ(rotation: Double): Array[Double] =
    val cx = 0.5 * (dims(0).toDouble - 1.0)
    val cy = 0.5 * (dims(1).toDouble - 1.0)
    val cos = math.cos(rotation)
    val sin = math.sin(rotation)
    Array.tabulate(nxyz) { lin =>
      val i = lin % dims(0)
      val j = (lin / dims(0)) % dims(1)
      val k = lin / (dims(0) * dims(1))
      val dx = i.toDouble - cx
      val dy = j.toDouble - cy
      val targetX = cx + cos * dx - sin * dy
      val targetY = cy + sin * dx + cos * dy
      baseValueAt(targetX, targetY, k.toDouble)
    }

  private def outlierFrame(fixed: Array[Double]): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val i = lin % dims(0)
      val j = (lin / dims(0)) % dims(1)
      val k = lin / (dims(0) * dims(1))
      fixed(lin) + 35.0 + 4.0 * i.toDouble - 3.0 * j.toDouble + 2.0 * k.toDouble
    }

  private def runFromFrames(frames: Vector[Array[Double]]): NeuroVec[Double] =
    val out = NArrayUtil.ofSize[Double](nxyz * frames.length)
    var t = 0
    while t < frames.length do
      var i = 0
      while i < nxyz do
        out(i + t * nxyz) = frames(t)(i)
        i += 1
      t += 1
    NeuroVec.fromLinear(out, space.addDim(frames.length, Some(Axis.Time)), "estimate-fixture")

  private def interiorMask: NeuroVol[Boolean] =
    val data =
      NArrayUtil.tabulate[Boolean](nxyz) { lin =>
        val i = lin % dims(0)
        val j = (lin / dims(0)) % dims(1)
        val k = lin / (dims(0) * dims(1))
        i >= 1 && i < dims(0) - 1 &&
          j >= 1 && j < dims(1) - 1 &&
          k >= 1 && k < dims(2) - 1
      }
    NeuroVol.fromLinear(data, space, "interior")

  private def emptyMask: NeuroVol[Boolean] =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](nxyz, false), space, "empty")

  private def plan: MotionPlan =
    MotionPlan(
      reference = ReferenceStrategy.Frame(FrameIndex.unsafe(0)),
      engine = MotionEngine.RigidRobust,
      control = MotionControl.default
    )

  test("rigid estimator keeps identical frames near zero motion") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone()))
    val est = MotionEstimator.estimate(run, Some(interiorMask), plan).fold(err => fail(err.message), identity)

    assertEquals(est.trace.length, 2)
    assertEqualsDouble(est.trace.unsafeFrame(0).tx, 0.0, 1e-12)
    assertEqualsDouble(est.trace.unsafeFrame(1).tx, 0.0, 1e-3)
    assertEqualsDouble(est.trace.unsafeFrame(1).ty, 0.0, 1e-3)
    assertEqualsDouble(est.trace.unsafeFrame(1).tz, 0.0, 1e-3)
    assert(est.diagnostics(1).costFinal <= est.diagnostics(1).costInitial + 1e-10)
    assert(est.diagnostics(1).converged)
  }

  test("middle reference is anchored and traversal estimates both directions") {
    val fixed = baseFrame
    val moving = shiftedMovingFramePlusOneX(fixed)
    val run = runFromFrames(Vector(moving, fixed, moving))
    val middlePlan = plan.copy(reference = ReferenceStrategy.Middle)
    val est = MotionEstimator.estimate(run, Some(interiorMask), middlePlan).fold(err => fail(err.message), identity)

    assertEqualsDouble(est.trace.unsafeFrame(1).tx, 0.0, 1e-12)
    assertEqualsDouble(est.trace.unsafeFrame(0).tx, VolreggerFixtures.estimatorTranslationX, 0.2)
    assertEqualsDouble(est.trace.unsafeFrame(2).tx, VolreggerFixtures.estimatorTranslationX, 0.2)
    assert(est.diagnostics.forall(d => d.costInitial.isFinite && d.costFinal.isFinite && d.overlap.isFinite))
    assert(est.diagnostics.forall(_.converged))
  }

  test("rigid estimator recovers a one-voxel positive x translation") {
    val fixed = baseFrame
    val moving = shiftedMovingFramePlusOneX(fixed)
    val run = runFromFrames(Vector(fixed, moving))
    val est = MotionEstimator.estimate(run, Some(interiorMask), plan).fold(err => fail(err.message), identity)
    val pose = est.trace.unsafeFrame(1)

    assertEqualsDouble(pose.tx, VolreggerFixtures.estimatorTranslationX, 0.2)
    assertEqualsDouble(pose.ty, 0.0, 0.15)
    assertEqualsDouble(pose.tz, 0.0, 0.15)
    assertEqualsDouble(pose.rx, 0.0, 0.08)
    assertEqualsDouble(pose.ry, 0.0, 0.08)
    assertEqualsDouble(pose.rz, 0.0, 0.08)
    assert(est.diagnostics(1).costFinal < est.diagnostics(1).costInitial)
    assert(est.diagnostics(1).restarted)
    assert(est.diagnostics(1).overlap > 0.8)
  }

  test("rigid estimator recovers a small positive z rotation") {
    val fixed = baseFrame
    val rotation = VolreggerFixtures.estimatorRotationZ
    val moving = rotatedMovingFramePositiveZ(rotation)
    val run = runFromFrames(Vector(fixed, moving))
    val est = MotionEstimator.estimate(run, Some(interiorMask), plan).fold(err => fail(err.message), identity)
    val pose = est.trace.unsafeFrame(1)

    assertEqualsDouble(pose.tx, 0.0, 0.2)
    assertEqualsDouble(pose.ty, 0.0, 0.2)
    assertEqualsDouble(pose.tz, 0.0, 0.15)
    assertEqualsDouble(pose.rz, rotation, 0.06)
    assert(est.diagnostics(1).costFinal < est.diagnostics(1).costInitial)
    assert(est.diagnostics(1).overlap > 0.8)
  }

  test("rotational capture seeds recover a large yaw before optimizer iterations") {
    val fixed = baseFrame
    val rotation = VolreggerFixtures.estimatorCaptureRotationZ
    val moving = rotatedMovingFramePositiveZ(rotation)
    val run = runFromFrames(Vector(fixed, moving))
    val zeroIter =
      PyramidControl
        .make(
          downsample = Vector(1),
          maxIterations = Vector(0),
          sampleCounts = Vector(nxyz),
          enabled = false
        )
        .fold(err => fail(err.message), identity)
    val captureControl =
      plan.control.copy(
        pyramid = zeroIter,
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture =
          CaptureControl(
            enabled = true,
            translationHalfWidthMm = 0.5,
            rotationHalfWidthDeg = math.toDegrees(rotation),
            topK = 4
          )
      )
    val noCaptureControl =
      captureControl.copy(
        capture = captureControl.capture.withEnabled(false)
      )
    val captured =
      MotionEstimator
        .estimate(run, Some(interiorMask), plan.copy(control = captureControl))
        .fold(err => fail(err.message), identity)
    val noCapture =
      MotionEstimator
        .estimate(run, Some(interiorMask), plan.copy(control = noCaptureControl))
        .fold(err => fail(err.message), identity)

    assertEqualsDouble(noCapture.trace.unsafeFrame(1).rz, 0.0, 1e-12)
    assertEqualsDouble(captured.trace.unsafeFrame(1).rz, rotation, 0.03)
    assert(captured.diagnostics(1).costFinal < noCapture.diagnostics(1).costFinal)
    assert(captured.diagnostics(1).restarted)
  }

  test("robust template refresh resists a high-cost outlier frame") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone(), outlierFrame(fixed)))
    val robustPlan =
      plan.copy(
        reference = ReferenceStrategy.RobustMean,
        control =
          plan.control.copy(
            template = TemplateControl(robustTemplate = true, refreshValidOnly = true, edgeExcludeFraction = 0.0)
          )
      )
    val plainPlan =
      robustPlan.copy(
        control =
          robustPlan.control.copy(
            template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0)
          )
      )
    val robust = MotionEstimator.estimate(run, Some(interiorMask), robustPlan).fold(err => fail(err.message), identity)
    val plain = MotionEstimator.estimate(run, Some(interiorMask), plainPlan).fold(err => fail(err.message), identity)

    assert(robust.diagnostics(1).costFinal < plain.diagnostics(1).costFinal * 0.25)
    assert(robust.diagnostics(0).costFinal < plain.diagnostics(0).costFinal * 0.25)
    assert(robust.diagnostics(2).costFinal > robust.diagnostics(1).costFinal)
    assert(robust.diagnostics.forall(d => d.costFinal.isFinite && d.overlap.isFinite))
  }

  test("rigid spline estimator layers smoothing over the rigid estimate") {
    val fixed = baseFrame
    val moving = shiftedMovingFramePlusOneX(fixed)
    val run = runFromFrames(Vector(fixed, moving, fixed.clone(), moving.clone()))
    val rigid = MotionEstimator.estimate(run, Some(interiorMask), plan).fold(err => fail(err.message), identity)
    val splinePlan = plan.copy(engine = MotionEngine.RigidSpline, acquisitionTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector.fill(dims(2))(0.0))))
    val spline = MotionEstimator.estimate(run, Some(interiorMask), splinePlan).fold(err => fail(err.message), identity)
    val badTiming = splinePlan.copy(acquisitionTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0))))

    assertEquals(spline.trace.length, rigid.trace.length)
    assertEquals(spline.diagnostics, rigid.diagnostics)
    assertEqualsDouble(spline.trace.unsafeFrame(0).tx, rigid.trace.unsafeFrame(0).tx, 1e-12)
    assertEqualsDouble(spline.trace.unsafeFrame(0).ty, rigid.trace.unsafeFrame(0).ty, 1e-12)
    assert(math.abs(spline.trace.unsafeFrame(1).tx - rigid.trace.unsafeFrame(1).tx) > 1e-4)
    assert(MotionEstimator.estimate(run, Some(interiorMask), badTiming).isLeft)
  }

  test("enabled pyramid schedule refines on the finest level") {
    val fixed = baseFrame
    val moving = shiftedMovingFramePlusOneX(fixed)
    val run = runFromFrames(Vector(fixed, moving))
    val pyramid =
      PyramidControl
        .make(
          downsample = Vector(3, 1),
          maxIterations = Vector(0, 6),
          sampleCounts = Vector(4, nxyz),
          enabled = true
        )
        .fold(err => fail(err.message), identity)
    val pyramidPlan =
      plan.copy(
        control =
          plan.control.copy(
            pyramid = pyramid,
            template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
            capture = plan.control.capture.withEnabled(false)
          )
      )
    val est = MotionEstimator.estimate(run, Some(interiorMask), pyramidPlan).fold(err => fail(err.message), identity)
    val pose = est.trace.unsafeFrame(1)

    assert(est.control.pyramid.enabled)
    assert(est.diagnostics(1).iterations > 0)
    assert(est.diagnostics(1).iterations <= 6)
    assert(est.diagnostics(1).costFinal < est.diagnostics(1).costInitial)
    assertEqualsDouble(pose.tx, VolreggerFixtures.estimatorTranslationX, 0.35)
  }

  test("low-motion pose shrink scales only subthreshold estimates") {
    val fixed = baseFrame
    val baseControl =
      plan.control.copy(
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture = plan.control.capture.withEnabled(false),
        temporal =
          TemporalControl(
            regularizationEnabled = false,
            lowMotionPoseShrink = false,
            lowMotionPoseScale = 0.5,
            lowMotionThresholdMm = 0.75
          )
      )
    val shrinkControl =
      baseControl.copy(
        temporal = baseControl.temporal.withLowMotionPoseShrink(true)
      )

    val tinyRun = runFromFrames(Vector(fixed, shiftedMovingFrameX(0.12)))
    val plainTiny =
      MotionEstimator
        .estimate(tinyRun, Some(interiorMask), plan.copy(control = baseControl))
        .fold(err => fail(err.message), identity)
    val shrunkTiny =
      MotionEstimator
        .estimate(tinyRun, Some(interiorMask), plan.copy(control = shrinkControl))
        .fold(err => fail(err.message), identity)
    val plainTinyPose = plainTiny.trace.unsafeFrame(1)
    val shrunkTinyPose = shrunkTiny.trace.unsafeFrame(1)

    assert(math.abs(plainTinyPose.tx) > 1e-4)
    assertEqualsDouble(shrunkTinyPose.tx, plainTinyPose.tx * 0.5, 1e-10)
    assertEqualsDouble(shrunkTinyPose.ty, plainTinyPose.ty * 0.5, 1e-10)
    assertEqualsDouble(shrunkTinyPose.rz, plainTinyPose.rz * 0.5, 1e-10)
    assert(shrunkTiny.diagnostics(1).costFinal.isFinite)

    val largeRun = runFromFrames(Vector(fixed, shiftedMovingFramePlusOneX(fixed)))
    val plainLarge =
      MotionEstimator
        .estimate(largeRun, Some(interiorMask), plan.copy(control = baseControl))
        .fold(err => fail(err.message), identity)
    val shrunkLarge =
      MotionEstimator
        .estimate(largeRun, Some(interiorMask), plan.copy(control = shrinkControl))
        .fold(err => fail(err.message), identity)

    assertEqualsDouble(shrunkLarge.trace.unsafeFrame(1).tx, plainLarge.trace.unsafeFrame(1).tx, 1e-10)
  }

  test("temporal regularization smooths an isolated pose while preserving reference") {
    val fixed = baseFrame
    val moving = shiftedMovingFramePlusOneX(fixed)
    val run = runFromFrames(Vector(fixed, moving, fixed.clone()))
    val baseControl =
      plan.control.copy(
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture = plan.control.capture.withEnabled(false),
        temporal =
          TemporalControl(
            regularizationEnabled = false,
            lowMotionPoseShrink = false,
            lowMotionPoseScale = 0.95,
            lowMotionThresholdMm = 0.25
          )
      )
    val plain =
      MotionEstimator
        .estimate(run, Some(interiorMask), plan.copy(control = baseControl))
        .fold(err => fail(err.message), identity)
    val regularized =
      MotionEstimator
        .estimate(
          run,
          Some(interiorMask),
          plan.copy(control = baseControl.copy(temporal = baseControl.temporal.withRegularizationEnabled(true)))
        )
        .fold(err => fail(err.message), identity)

    val plainSpike = plain.trace.unsafeFrame(1).tx
    val regularizedSpike = regularized.trace.unsafeFrame(1).tx
    assertEqualsDouble(regularized.trace.unsafeFrame(0).tx, 0.0, 1e-12)
    assert(math.abs(regularizedSpike) < math.abs(plainSpike) * 0.75)
    assert(math.abs(regularizedSpike) > math.abs(plainSpike) * 0.3)
    assert(regularized.diagnostics.forall(d => d.costFinal.isFinite && d.overlap.isFinite))
  }

  test("enabled temporal regularization is executable") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone()))
    val temporalPlan =
      plan.copy(
        control =
          plan.control.copy(
            temporal = plan.control.temporal.withRegularizationEnabled(true)
          )
      )

    assert(MotionEstimator.estimate(run, Some(interiorMask), temporalPlan).isRight)
  }

  test("frame-mean nuisance residual removes global intensity offsets") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, offsetFrame(fixed, 12.0)))
    val zeroIter =
      PyramidControl
        .make(
          downsample = Vector(1),
          maxIterations = Vector(0),
          sampleCounts = Vector(nxyz),
          enabled = false
        )
        .fold(err => fail(err.message), identity)
    val baseControl =
      plan.control.copy(
        pyramid = zeroIter,
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture = plan.control.capture.withEnabled(false),
        residual = ResidualControl.default
      )
    val nuisanceControl =
      baseControl.copy(
        residual = ResidualControl.fromRemoveFrameMean(removeFrameMean = true)
      )

    val raw =
      MotionEstimator
        .estimate(run, Some(interiorMask), plan.copy(control = baseControl))
        .fold(err => fail(err.message), identity)
    val nuisance =
      MotionEstimator
        .estimate(run, Some(interiorMask), plan.copy(control = nuisanceControl))
        .fold(err => fail(err.message), identity)

    assert(raw.diagnostics(1).costFinal > 15.0)
    assert(nuisance.diagnostics(1).costFinal < 1e-10)
    assertEqualsDouble(nuisance.trace.unsafeFrame(1).tx, 0.0, 1e-12)
    assertEqualsDouble(nuisance.trace.unsafeFrame(1).rz, 0.0, 1e-12)
  }

  test("estimator reports invalid references, non-finite runs, and empty masks as typed errors") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone()))
    val badRefPlan = plan.copy(reference = ReferenceStrategy.Frame(FrameIndex.unsafe(5)))
    val badValues = fixed.clone()
    badValues(0) = Double.NaN
    val nonFiniteRun = runFromFrames(Vector(badValues, fixed.clone()))

    MotionEstimator.estimate(run, Some(interiorMask), badRefPlan) match
      case Left(MotionError.FrameIndexOutOfBounds(5, 2)) => ()
      case other => fail(s"expected FrameIndexOutOfBounds, got $other")

    MotionEstimator.estimate(nonFiniteRun, Some(interiorMask), plan) match
      case Left(MotionError.NonFiniteData("run", 0)) => ()
      case other => fail(s"expected NonFiniteData, got $other")

    MotionEstimator.estimate(run, Some(emptyMask), plan) match
      case Left(MotionError.ShapeMismatch("estimation mask", _, _)) => ()
      case other => fail(s"expected ShapeMismatch, got $other")
  }
