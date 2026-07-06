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
    assertEqualsDouble(pose.rz, rotation, 0.05)
    assert(est.diagnostics(1).costFinal < est.diagnostics(1).costInitial)
    assert(est.diagnostics(1).overlap > 0.8)
  }

  test("rigid spline estimator is explicitly deferred") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone()))
    val splinePlan = plan.copy(engine = MotionEngine.RigidSpline)
    assert(MotionEstimator.estimate(run, Some(interiorMask), splinePlan).isLeft)
  }

  test("unsupported estimator controls are explicit errors") {
    val fixed = baseFrame
    val run = runFromFrames(Vector(fixed, fixed.clone()))
    val pyramidPlan =
      plan.copy(
        control =
          plan.control.copy(
            pyramid =
              PyramidControl
                .make(
                  downsample = Vector(2, 1),
                  maxIterations = Vector(4, 2),
                  sampleCounts = Vector(20, 40),
                  enabled = true
                )
                .fold(err => fail(err.message), identity)
          )
      )
    val temporalPlan =
      plan.copy(
        control =
          plan.control.copy(
            temporal = plan.control.temporal.copy(regularizationEnabled = true)
          )
      )

    assert(MotionEstimator.estimate(run, Some(interiorMask), pyramidPlan).isLeft)
    assert(MotionEstimator.estimate(run, Some(interiorMask), temporalPlan).isLeft)
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
