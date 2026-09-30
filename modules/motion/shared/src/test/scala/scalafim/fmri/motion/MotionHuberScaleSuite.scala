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

  test("a line search that exhausts its halvings is converged only when its final step is negligible"):
    assert(MotionEstimator.lineSearchResolved(1e-6, 1e-5))
    assert(MotionEstimator.lineSearchResolved(1e-5, 1e-5))
    assert(!MotionEstimator.lineSearchResolved(2e-5, 1e-5))
