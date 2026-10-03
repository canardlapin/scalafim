package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures
import scalafim.image.*
import scalafim.image.SampleSpaces.*

/** With `HuberScale.RobustResidual` the Huber threshold is a multiple of a
  * robust residual scale, so robust weighting and the estimated motion do not
  * depend on the image intensity scale. `HuberScale.Absolute` keeps the
  * volregger estimand, whose robust weights do.
  */
class MotionHuberScaleSuite extends munit.FunSuite:

  private val dims = VolreggerFixtures.estimatorDims
  private val nxyz = dims.product
  private val space = SampleSpaces(dims)

  private def baseValueAt(x: Double, y: Double, z: Double): Double =
    val dx = x - 3.0
    val dy = y - 2.0
    val dz = z - 2.0
    10.0 * math.exp(-(dx * dx / 5.0 + dy * dy / 3.0 + dz * dz / 4.0)) +
      0.4 * x + 0.2 * y - 0.15 * z + 0.35 * dx * dy + 0.12 * dx * dx - 0.08 * dy * dy + 0.05 * dx * dz

  /** A shifted frame with a bright artefact block, so large residuals exist and Huber is active. */
  private def frame(offsetX: Double, scale: Double, artefact: Boolean): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val v = space.indexToVoxel3D(lin)
      val clean = baseValueAt(v.x.toDouble + offsetX, v.y.toDouble, v.z.toDouble)
      val spike = if artefact && v.x >= 1 && v.x <= 2 && v.y >= 1 && v.y <= 2 && v.z == 2 then 40.0 else 0.0
      scale * (clean + spike)
    }

  private def run(frames: Vector[Array[Double]]): SomeScalarSeries[Double] =
    val out = PrimitiveBuffers.ofSize[Double](nxyz * frames.length)
    var i = 0
    while i < nxyz do
      var t = 0
      while t < frames.length do
        out(i * frames.length + t) = frames(t)(i)
        t += 1
      i += 1
    SomeScalarSeries.unsafeCopyFromCanonicalArray(out, space.addDim(ProviderAxes.time(frames.length)), "huber-scale")

  private def interiorMask: SomeMaskVolume =
    SomeMaskVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Boolean](nxyz) { lin =>
        val v = space.indexToVoxel3D(lin)
        v.x >= 1 && v.x < dims(0) - 1 && v.y >= 1 && v.y < dims(1) - 1 && v.z >= 1 && v.z < dims(2) - 1
      },
      space,
      "interior"
    )

  private def plan(optimizer: OptimizerControl) =
    MotionPlan(
      reference = ReferenceStrategy.Frame(FrameIndex.unsafe(0)),
      engine = MotionEngine.RigidRobust,
      control = MotionControl.default.copy(optimizer = optimizer)
    )

  private def estimate(scale: Double, offset: Double, optimizer: OptimizerControl = OptimizerControl.default) =
    MotionEstimator
      .estimate(run(Vector(frame(0.0, scale, artefact = false), frame(offset, scale, artefact = true))),
        Some(interiorMask), plan(optimizer))
      .fold(err => fail(err.message), identity)

  private def pose(e: MotionEstimate) = e.trace.unsafeFrame(1)

  private def components(p: RigidPose): Vector[(String, Double)] =
    Vector("tx" -> p.tx, "ty" -> p.ty, "tz" -> p.tz, "rx" -> p.rx, "ry" -> p.ry, "rz" -> p.rz)

  test("the default Huber threshold is relative to a robust residual scale"):
    assertEquals(OptimizerControl.default.huberScale, HuberScale.RobustResidual)
    assertEquals(OptimizerControl.volreggerParity.huberScale, HuberScale.Absolute)
    assertEquals(OptimizerControl.volreggerParity.huberK, OptimizerControl.default.huberK)

  test("with Huber active, estimated motion does not depend on the intensity scale across 1e-6..1e6"):
    for offset <- Vector(0.6, 1.4) do
      val unit = estimate(1.0, offset)
      // Huber must actually be active: an effectively infinite threshold (least squares) lands elsewhere.
      val leastSquares = estimate(1.0, offset, OptimizerControl.unsafe(1e12, 1e-2, 1e-5, 1e-6))
      val robustShift = components(pose(unit)).zip(components(pose(leastSquares))).map((a, b) => math.abs(a._2 - b._2)).max
      assert(robustShift > 1e-3, clues(offset, pose(unit), pose(leastSquares)))
      assert(math.abs(pose(unit).tx) > 0.3, clues(offset, pose(unit)))
      for scale <- Vector(1e-6, 1e-3, 1e3, 1e6) do
        val scaled = estimate(scale, offset)
        for ((name, a), (_, b)) <- components(pose(unit)).zip(components(pose(scaled))) do
          assertEqualsDouble(b, a, 1e-6, clues(offset, scale, name))
        assertEquals(scaled.diagnostics(1).iterations, unit.diagnostics(1).iterations, clues(offset, scale))
        assertEquals(scaled.diagnostics(1).converged, unit.diagnostics(1).converged, clues(offset, scale))

  test("the absolute volregger threshold keeps its intensity-scale dependence"):
    val unit = estimate(1.0, 1.4, OptimizerControl.volreggerParity)
    val bright = estimate(1e3, 1.4, OptimizerControl.volreggerParity)
    val difference = components(pose(unit)).zip(components(pose(bright))).map((a, b) => math.abs(a._2 - b._2)).max
    assert(difference > 1e-3, clues(pose(unit), pose(bright)))

  // A realistically sized volume for accuracy and convergence claims; the 7x5x5 estimator fixture is
  // too small (its searches stall at the capture grid under either threshold) and is used only as a
  // stall witness below.
  private val bigDims = Vector(20, 18, 14)
  private val bigSpace = SampleSpaces(bigDims)
  private val bigN = bigDims.product

  private def anatomy(x: Double, y: Double, z: Double): Double =
    def blob(cx: Double, cy: Double, cz: Double, s: Double, a: Double) =
      a * math.exp(-((x - cx) * (x - cx) + (y - cy) * (y - cy) + (z - cz) * (z - cz)) / (2.0 * s * s))
    100.0 + blob(8.0, 8.0, 6.0, 3.0, 60.0) + blob(12.5, 10.0, 7.5, 2.2, -35.0) + blob(6.0, 12.0, 8.0, 1.8, 40.0) +
      2.0 * x - 1.5 * y + 1.0 * z

  private def bigFrame(offsetX: Double, scale: Double, artefact: Boolean): Array[Double] =
    Array.tabulate(bigN) { lin =>
      val v = bigSpace.indexToVoxel3D(lin)
      val spike = if artefact && v.x >= 9 && v.x <= 11 && v.y >= 5 && v.y <= 7 && v.z >= 6 && v.z <= 7 then 80.0 else 0.0
      scale * (anatomy(v.x.toDouble + offsetX, v.y.toDouble, v.z.toDouble) + spike)
    }

  private def bigRun(frames: Vector[Array[Double]]): SomeScalarSeries[Double] =
    val out = PrimitiveBuffers.ofSize[Double](bigN * frames.length)
    var i = 0
    while i < bigN do
      var t = 0
      while t < frames.length do
        out(i * frames.length + t) = frames(t)(i)
        t += 1
      i += 1
    SomeScalarSeries.unsafeCopyFromCanonicalArray(out, bigSpace.addDim(ProviderAxes.time(frames.length)), "huber-big")

  private def bigMask: SomeMaskVolume =
    SomeMaskVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Boolean](bigN) { lin =>
        val v = bigSpace.indexToVoxel3D(lin)
        v.x >= 2 && v.x < bigDims(0) - 2 && v.y >= 2 && v.y < bigDims(1) - 2 && v.z >= 2 && v.z < bigDims(2) - 2
      },
      bigSpace,
      "big-interior"
    )

  private def bigEstimate(scale: Double, offset: Double, optimizer: OptimizerControl = OptimizerControl.default) =
    MotionEstimator
      .estimate(bigRun(Vector(bigFrame(0.0, scale, artefact = false), bigFrame(offset, scale, artefact = true))),
        Some(bigMask), plan(optimizer))
      .fold(err => fail(err.message), identity)

  test("on a realistic volume the robust fit converges near the true shift where least squares is pulled away"):
    for offset <- Vector(0.6, 1.4) do
      val robust = bigEstimate(1.0, offset)
      val leastSquares = bigEstimate(1.0, offset, OptimizerControl.unsafe(1e12, 1e-2, 1e-5, 1e-6))
      val p = pose(robust)
      val q = pose(leastSquares)
      assert(robust.diagnostics(1).converged, clues(offset, robust.diagnostics(1)))
      assert(robust.diagnostics(1).overlap >= 0.9, clues(robust.diagnostics(1)))
      val robustError = math.abs(math.abs(p.tx) - offset)
      val lsError = math.abs(math.abs(q.tx) - offset)
      assert(robustError < 0.05, clues(offset, p))
      assert(p.tx > 0.0, clues(offset, p))
      assert(math.abs(p.ty) < 0.05 && math.abs(p.tz) < 0.05, clues(offset, p))
      assert(math.abs(p.rx) < 0.02 && math.abs(p.ry) < 0.02 && math.abs(p.rz) < 0.02, clues(offset, p))
      assert(robustError < lsError, clues(offset, p, q))
      for scale <- Vector(1e-6, 1e6) do
        val scaled = pose(bigEstimate(scale, offset))
        for ((name, a), (_, b)) <- components(p).zip(components(scaled)) do
          assertEqualsDouble(b, a, 1e-6, clues(offset, scale, name))

  /** A frame with a small deterministic frame-specific pattern (a stand-in for noise), so the
    * refreshed template (an average of aligned frames) genuinely differs from the reference frame.
    */
  private def patternedFrame(t: Int, offsetX: Double, scale: Double, artefact: Boolean): Array[Double] =
    val clean = bigFrame(offsetX, 1.0, artefact)
    Array.tabulate(bigN) { lin =>
      val v = bigSpace.indexToVoxel3D(lin)
      scale * (clean(lin) + 1.5 * math.sin(1.7 * v.x + 2.3 * v.y + 0.9 * v.z + 1.3 * t))
    }

  private def threeFrameBig(scale: Double, template: TemplateControl) =
    val base = plan(OptimizerControl.default)
    MotionEstimator
      .estimate(bigRun(Vector(patternedFrame(0, 0.0, scale, artefact = false), patternedFrame(1, 0.6, scale, artefact = true),
        patternedFrame(2, 1.0, scale, artefact = false))), Some(bigMask), base.copy(control = base.control.copy(template = template)))
      .fold(err => fail(err.message), identity)

  test("with a template refresh that actually runs (three frames, realistic volume), poses are invariant and costs scale by c^2"):
    val refresh = TemplateControl.default
    val noRefresh = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = refresh.edgeExcludeFraction)
    val unit = threeFrameBig(1.0, refresh)
    // The refresh pass runs: without it the second-pass poses differ.
    val firstPassOnly = threeFrameBig(1.0, noRefresh)
    val refreshShift = (0 until 3).flatMap(t =>
      components(unit.trace.unsafeFrame(t)).zip(components(firstPassOnly.trace.unsafeFrame(t))).map((a, b) => math.abs(a._2 - b._2))).max
    assert(refreshShift > 1e-4, clues(refreshShift))
    for t <- 0 until 3 do assert(unit.diagnostics(t).converged, clues(t, unit.diagnostics(t)))
    for scale <- Vector(1e-6, 1e3) do
      val scaled = threeFrameBig(scale, refresh)
      for t <- 0 until 3 do
        for ((name, a), (_, b)) <- components(unit.trace.unsafeFrame(t)).zip(components(scaled.trace.unsafeFrame(t))) do
          assertEqualsDouble(b, a, 1e-6, clues(scale, t, name))
        // Poses agree to ~1e-7 and cost is quadratic in the residual, so relative 1e-5 is the right tolerance.
        val expectedCost = unit.diagnostics(t).costFinal * scale * scale
        assertEqualsDouble(scaled.diagnostics(t).costFinal, expectedCost, 1e-5 * math.abs(expectedCost), clues(scale, t))
        assertEquals(scaled.diagnostics(t).converged, unit.diagnostics(t).converged, clues(scale, t))

  /** Zero background outside a central blob: more than half the residuals match exactly. */
  private def blobFrame(offsetX: Double, scale: Double): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val v = space.indexToVoxel3D(lin)
      val inside = v.x >= 2 && v.x <= 4 && v.y >= 1 && v.y <= 3 && v.z >= 1 && v.z <= 3
      if inside then scale * baseValueAt(v.x.toDouble + offsetX, v.y.toDouble, v.z.toDouble) else 0.0
    }

  test("the zero-background fallback scale is covariant: motion is invariant with mostly exact-zero residuals"):
    def blobEstimate(scale: Double) =
      MotionEstimator.estimate(run(Vector(blobFrame(0.0, scale), blobFrame(0.3, scale))), None, plan(OptimizerControl.default))
        .fold(err => fail(err.message), identity)
    val unit = blobEstimate(1.0)
    for scale <- Vector(1e-6, 1e6) do
      val scaled = blobEstimate(scale)
      for ((name, a), (_, b)) <- components(pose(unit)).zip(components(pose(scaled))) do
        assertEqualsDouble(b, a, 1e-6, clues(scale, name))

  test("a line search stalled above the step tolerance reports a non-converged frame end to end"):
    // On the 7x5x5 fixture, under either threshold, the search from the capture pose cannot reduce
    // the cost after six halvings of a step well above the tolerance. The previous rule reported that
    // as converged; it is now a non-converged frame after one iteration.
    for optimizer <- Vector(OptimizerControl.default, OptimizerControl.volreggerParity) do
      val stalled = estimate(1.0, 0.6, optimizer)
      assert(!stalled.diagnostics(1).converged, clues(optimizer.huberScale, stalled.diagnostics(1)))
      assertEquals(stalled.diagnostics(1).iterations, 1, clues(optimizer.huberScale))
    // The same rule leaves genuinely converged fits converged (realistic volume).
    assert(bigEstimate(1.0, 0.6).diagnostics(1).converged)

  test("a line search that exhausts its halvings is converged only when its final step is negligible"):
    assert(MotionEstimator.lineSearchResolved(1e-6, 1e-5))
    assert(MotionEstimator.lineSearchResolved(1e-5, 1e-5))
    assert(!MotionEstimator.lineSearchResolved(2e-5, 1e-5))
