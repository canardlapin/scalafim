package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures
import scalafim.image.*
import scalafim.image.SampleSpaces.*

/** The Gauss-Newton step must not depend on the image intensity scale, and a
  * singular normal system must not be reported as convergence.
  */
class MotionSolverScalingSuite extends munit.FunSuite:

  private val dims = VolreggerFixtures.estimatorDims
  private val nxyz = dims.product
  private val space = SampleSpaces(dims)

  private def baseValueAt(x: Double, y: Double, z: Double): Double =
    val dx = x - 3.0
    val dy = y - 2.0
    val dz = z - 2.0
    10.0 * math.exp(-(dx * dx / 5.0 + dy * dy / 3.0 + dz * dz / 4.0)) +
      0.4 * x + 0.2 * y - 0.15 * z + 0.35 * dx * dy + 0.12 * dx * dx - 0.08 * dy * dy + 0.05 * dx * dz

  private def frame(offsetX: Double, scale: Double): Array[Double] =
    Array.tabulate(nxyz) { lin =>
      val v = space.indexToVoxel3D(lin)
      scale * baseValueAt(v.x.toDouble + offsetX, v.y.toDouble, v.z.toDouble)
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
    SomeScalarSeries.unsafeCopyFromCanonicalArray(out, space.addDim(ProviderAxes.time(frames.length)), "solver-scaling")

  private def interiorMask: SomeMaskVolume =
    SomeMaskVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Boolean](nxyz) { lin =>
        val v = space.indexToVoxel3D(lin)
        v.x >= 1 && v.x < dims(0) - 1 && v.y >= 1 && v.y < dims(1) - 1 && v.z >= 1 && v.z < dims(2) - 1
      },
      space,
      "interior"
    )

  private val plan =
    MotionPlan(
      reference = ReferenceStrategy.Frame(FrameIndex.unsafe(0)),
      engine = MotionEngine.RigidRobust,
      control = MotionControl.default
    )

  private def estimate(scale: Double) =
    MotionEstimator
      .estimate(run(Vector(frame(0.0, scale), frame(0.6, scale))), Some(interiorMask), plan)
      .fold(err => fail(err.message), identity)

  // The Huber threshold (MotionControl huberK) is in absolute intensity units,
  // so robust weighting is not scale invariant; these fixtures keep residuals
  // below it at both scales, isolating the solve, damping and stopping rule.
  test("with Huber weighting inactive, estimated motion does not depend on the image intensity scale"):
    val unit = estimate(1.0)
    val tiny = estimate(1e-6)
    val a = unit.trace.unsafeFrame(1)
    val b = tiny.trace.unsafeFrame(1)
    assert(math.abs(a.tx) > 0.3, s"fixture must move: tx=${a.tx}")
    for (name, x, y) <- Vector(("tx", a.tx, b.tx), ("ty", a.ty, b.ty), ("tz", a.tz, b.tz), ("rx", a.rx, b.rx), ("ry", a.ry, b.ry), ("rz", a.rz, b.rz)) do
      assertEqualsDouble(y, x, 1e-6, s"$name differs under intensity scaling")
    assertEquals(tiny.diagnostics(1).iterations, unit.diagnostics(1).iterations)

  test("the relative-drop stopping rule does not stop early on very low-intensity images"):
    val unit = estimate(1.0)
    val faint = estimate(1e-9)
    assertEquals(faint.diagnostics(1).iterations, unit.diagnostics(1).iterations)
    assertEqualsDouble(faint.trace.unsafeFrame(1).tx, unit.trace.unsafeFrame(1).tx, 1e-6)

  test("a singular normal system is not reported as convergence"):
    // A spatially constant moving frame gives zero image gradients, so every
    // Gauss-Newton system is singular.
    val flat = Array.fill(nxyz)(5.0)
    val est = MotionEstimator
      .estimate(run(Vector(frame(0.0, 1.0), flat)), Some(interiorMask), plan)
      .fold(err => fail(err.message), identity)
    assert(!est.diagnostics(1).converged, s"singular system reported as converged: ${est.diagnostics(1)}")

  // Direct solver checks with an independent residual oracle.
  private def spd(scales: Array[Double], seed: Long): Array[Double] =
    val rng = scala.util.Random(seed)
    val m = Array.fill(36)(rng.nextGaussian())
    // A = M^T M + I is well conditioned; H = S A S is badly scaled.
    Array.tabulate(36) { k =>
      val (r, c) = (k / 6, k % 6)
      var a = if r == c then 1.0 else 0.0
      var i = 0
      while i < 6 do
        a += m(i * 6 + r) * m(i * 6 + c)
        i += 1
      scales(r) * a * scales(c)
    }

  private def relativeResidual(h: Array[Double], x: Array[Double], b: Array[Double]): Double =
    val r = Array.tabulate(6)(i => (0 until 6).map(j => h(i * 6 + j) * x(j)).sum - b(i))
    math.sqrt(r.map(v => v * v).sum) / math.sqrt(b.map(v => v * v).sum)

  test("the normal-equation solve is accurate on badly scaled positive definite systems"):
    for (scales, seed) <- Vector(
        (Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0), 1L),
        (Array(1e-6, 1e-6, 1e-6, 1e-6, 1e-6, 1e-6), 2L),
        (Array(1e-8, 1.0, 1e3, 1e-3, 10.0, 1e-5), 3L)
      )
    do
      val h = spd(scales, seed)
      val b = Array.tabulate(6)(i => scales(i) * (i + 1).toDouble)
      val x = MotionEstimator.solveNormal6(h, b).getOrElse(fail(s"refused a positive definite system with scales ${scales.toVector}"))
      assert(relativeResidual(h, x, b) < 1e-10, s"residual ${relativeResidual(h, x, b)}")

  test("the normal-equation solve refuses rank-deficient and indefinite systems"):
    val rankFive = Array.tabulate(36)(k => if k / 6 == k % 6 && k / 6 < 5 then 1.0 else 0.0)
    assertEquals(MotionEstimator.solveNormal6(rankFive, Array.fill(6)(1.0)).map(_.toVector), None)
    // Unit diagonal but indefinite: the (0,1) block has eigenvalues 1 +/- 2.
    val indefinite = Array.tabulate(36)(k => if k / 6 == k % 6 then 1.0 else if k == 1 || k == 6 then 2.0 else 0.0)
    assertEquals(MotionEstimator.solveNormal6(indefinite, Array.fill(6)(1.0)).map(_.toVector), None)

  test("the relative pivot tolerance refuses a numerically singular unit-diagonal system"):
    // The (0,1) block has Schur complement 1 - (1 - 1e-14)^2 ~ 2e-14, below the
    // 1e-12 relative tolerance; with a zero tolerance it would be accepted.
    val nearSingular = Array.tabulate(36)(k => if k / 6 == k % 6 then 1.0 else if k == 1 || k == 6 then 1.0 - 1e-14 else 0.0)
    assertEquals(MotionEstimator.solveNormal6(nearSingular, Array.fill(6)(1.0)).map(_.toVector), None)

