package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.model.{ArCoefficientSpec, AutocorrelationConfig, ReducedRankComponentSpec, ReducedRankGlsConfig, ReducedRankInferencePolicy, ReducedRankSolverConfig}

private[fit] object VoxelwiseReducedRankFixtures:
  val design: DMat = Matrix.tabulate(12, 3) { (row, col) =>
    val t = row.toDouble + 1.0
    col match
      case 0 => (t - 6.5) / 4.0
      case 1 => math.sin(t * 0.8)
      case _ => 1.0
  }
  private val b = Vector(Vector(1.2, -0.7, 0.4, 1.1), Vector(0.3, 1.4, -1.1, 0.8))
  private val intercept = Vector(0.4, -0.2, 0.8, 0.1)
  val response: DMat = Matrix.tabulate(12, 4) { (row, v) =>
    design(row, 0) * b(0)(v) + design(row, 1) * b(1)(v) + intercept(v) +
      0.12 * math.cos((row + 1).toDouble * (0.3 + (v + 1).toDouble * 0.17))
  }
  val segments = Vector(TimeSegment(0, 6, 0), TimeSegment(6, 12, 0))
  val plans = Vector(0.15, 0.65, -0.3, 0.65).map(rho => WhiteningPlan.global(ArmaCoefficients.ar(rho), segments))
  val partition = ReducedRankDesignPartition.fromColumns(3, Vector(0, 1), Vector(2)).toOption.get
  val expected: DMat = Matrix.dense(2, 4, Seq(
    0.34950575931499206, -0.9123357446596343, 0.58390210911918239, -0.014226526073811162,
    -0.51425149176338303, 1.3423813633280772, -0.85913471425145971, 0.020932451214557807
  ))

class VoxelwiseReducedRankGlsSuite extends munit.FunSuite:
  import VoxelwiseReducedRankFixtures.*
  private def checked[A](value: Either[FitError, A]): A = value.fold(e => fail(e.message), identity)
  private def close(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    for r <- 0 until actual.rows; c <- 0 until actual.cols do
      assertEqualsDouble(actual(r, c), expected(r, c), tolerance)

  test("global heterogeneous rank-one fit matches independent projective-circle R oracle") {
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition))
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, ReducedRankSolverConfig.unsafe()))
    close(fit.targetCoefficients, expected, 2e-6)
    assertEqualsDouble(fit.objective, 12.611903907225837, 1e-9)
    assertEquals(fit.achievedRank, 1)
    assert(fit.starts.exists(_.status == ReducedRankSolverStatus.Converged))
    // Known shortcut losses from the independent R oracle: both are larger.
    assert(fit.objective < 13.309738378806633)
    assert(fit.objective < 13.865046016164694)
  }

  test("inactive rank recovers joint-design GLS including nuisance and residual variance") {
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition))
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, partition, 2, ReducedRankSolverConfig.unsafe()))
    for v <- plans.indices do
      val expected = checked(Gls.fitWithPlan(plans(v), design, VoxelwiseReducedRankGls.columns(response, Vector(v))))
      for r <- 0 until design.cols do
        assertEqualsDouble(fit.coefficients(r, v), expected.coefficients(r, 0), 1e-10)
      assertEqualsDouble(fit.residualVariance(v), expected.residualVariance(0), 1e-10)
    assertEqualsDouble(fit.objective, 0.19757163262585969, 1e-10)
  }

  test("inactive rank when voxels are fewer than targets uses unrestricted GLS") {
    val y = VoxelwiseReducedRankGls.columns(response, Vector(1))
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, y, Vector(plans(1)), partition))
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, ReducedRankSolverConfig.unsafe()))
    close(fit.coefficients, gs.head.fullCoefficients, 1e-10)
    assertEquals(fit.starts.head.iterations, 0)
  }

  test("common whitening agrees with the existing shared task QR/SVD estimator") {
    val w = WhiteningPlan.global(ArmaCoefficients.ar(0.35), segments)
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, Vector.fill(4)(w), partition))
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, ReducedRankSolverConfig.unsafe()))
    // Independent spectral construction in the common task geometry.
    val d = gs.head.residualizedTarget
    val q = d.qr(gale.linalg.QROptions(gale.linalg.QRPivoting.Column, Some(1e-7)))
    val ys = Matrix.tabulate(12, 4)((r, v) => gs(v).residualizedResponse(r, 0))
    val qt = q.applyQT(ys).toOption.get
    val scores = Matrix.tabulate(2, 4)((r, c) => qt(r, c))
    val svd = gale.spectral.Svds.svd(scores, gale.spectral.SingularSelection.All).toOption.get.requireConverged.toOption.get
    val basis = Matrix.tabulate(4, 1)((r, _) => svd.vt(0, r))
    val c = q.solveLeastSquares(ys * basis).toOption.get
    close(fit.targetCoefficients, c * basis.t, 2e-6)
  }

  test("solver fails explicitly on iteration exhaustion, singular design and unsupported adaptive rank") {
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition))
    val exhausted = VoxelwiseReducedRankGls.solve(gs, partition, 1, ReducedRankSolverConfig.unsafe(maxIterations = 1))
    assert(exhausted.left.toOption.exists(_.message.contains("no start converged")))
    val singular = Matrix.tabulate(12, 3)((r, c) => if c == 1 then design(r, 0) else design(r, c))
    assert(VoxelwiseReducedRankGls.geometry(singular, response, plans, partition).isLeft)
    val singularWhitening = WhiteningPlan.globalWithInitialCondition(ArmaCoefficients.ar(0.3), segments, scalafim.fmri.ar.InitialConditionPolicy.PrecomputedScale(0.0)).toOption.get
    assert(VoxelwiseReducedRankGls.geometry(design, response, Vector.fill(4)(singularWhitening), partition).left.toOption.exists(_.message.contains("first scale")))
    assert(VoxelwiseReducedRankGls.requestedRank(ReducedRankComponentSpec.unsafeEnergyRetained(0.9), 2, 4).isLeft)
  }

  test("nuisance reparameterization leaves target estimates unchanged") {
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition))
    val baseline = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, ReducedRankSolverConfig.unsafe()))
    val x = Matrix.tabulate(12, 3)((r, c) => if c == 2 then 3.0 else design(r, c))
    val other = checked(VoxelwiseReducedRankGls.solve(checked(VoxelwiseReducedRankGls.geometry(x, response, plans, partition)), partition, 1, ReducedRankSolverConfig.unsafe()))
    close(other.targetCoefficients, baseline.targetCoefficients, 2e-6)
    for v <- 0 until 4 do assertEqualsDouble(other.coefficients(2, v) * 3.0, baseline.coefficients(2, v), 2e-6)
  }

  test("whole-design and response unit changes preserve fitted mean, rank and stopping accuracy") {
    val base = checked(VoxelwiseReducedRankGls.solve(checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition)), partition, 1, ReducedRankSolverConfig.unsafe()))
    Vector((1e-9, 1.0), (1e9, 1.0), (1.0, 1e-8), (1.0, 1e8)).foreach { case (sx, sy) =>
      val x = Matrix.tabulate(12, 3)((r, c) => design(r, c) * sx)
      val y = Matrix.tabulate(12, 4)((r, c) => response(r, c) * sy)
      val fit = checked(VoxelwiseReducedRankGls.solve(checked(VoxelwiseReducedRankGls.geometry(x, y, plans, partition)), partition, 1, ReducedRankSolverConfig.unsafe()))
      val rescaled = Matrix.tabulate(3, 4)((r, c) => fit.coefficients(r, c) * sx / sy)
      close(rescaled, base.coefficients, 2e-6)
      assertEqualsDouble(fit.objective / (sy * sy), base.objective, 1e-9)
      assertEquals(fit.achievedRank, base.achievedRank)
    }
  }

  test("prepared decoder rejects altered response and row or run identity") {
    val x = DesignMatrix.unsafe(design)
    val y = ResponseBlock.unsafe(response)
    val rows = (0 until 12).toVector
    val runs = Vector(RunPartition(0, rows, rows))
    val config = ReducedRankGlsConfig.unsafe(
      components = ReducedRankComponentSpec.unsafeFixed(1),
      autocorrelation = AutocorrelationConfig.unsafe(coefficients = ArCoefficientSpec.Rho(0.3)),
      inference = ReducedRankInferencePolicy.EstimatesOnly
    )
    val prepared = checked(ReducedRankGlsPrepared.prepare(x, y, runs, config, Vector(0, 1, 2, 3), partition))
    val input = FitBlockInput(x, y, Vector(0, 1, 2, 3), rows, runs)
    assert(prepared.fitBlock(input).isRight)
    assert(prepared.fitBlock(input.copy(timepoints = rows.map(_ + 1))).isLeft)
    val differentRuns = Vector(RunPartition(0, rows.take(6), rows.take(6)), RunPartition(1, rows.drop(6), rows.drop(6)))
    assert(prepared.fitBlock(input.copy(partitions = differentRuns)).isLeft)
    val differentY = ResponseBlock.unsafe(Matrix.tabulate(12, 4)((r, v) => response(r, v) + (if r == 0 && v == 0 then 0.1 else 0.0)))
    assert(prepared.fitBlock(input.copy(response = differentY)).isLeft)
  }

  test("rank-two three-target noiseless fit and zero-signal boundary preserve original coefficients") {
    val x = Matrix.tabulate(12, 4)((r, c) => if c < 3 then math.sin((r + 1) * (c + 1) * 0.37) else 1.0)
    val a = Vector(1.2, -0.7, 0.4, 1.1)
    val b = Vector(0.3, 1.4, -1.1, 0.8)
    val beta = Matrix.tabulate(4, 4)((r, v) => r match
      case 0 => a(v)
      case 1 => b(v)
      case 2 => a(v) + 2.0 * b(v)
      case _ => 0.4
    )
    val part = checked(ReducedRankDesignPartition.fromColumns(4, Vector(0, 1, 2), Vector(3)))
    val gs = checked(VoxelwiseReducedRankGls.geometry(x, x * beta, plans, part))
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, part, 2, ReducedRankSolverConfig.unsafe()))
    close(fit.coefficients, beta, 1e-10)
    assertEquals(fit.achievedRank, 2)
    assertEqualsDouble(fit.objective, 0.0, 1e-20)
    val zeroGs = checked(VoxelwiseReducedRankGls.geometry(x, Matrix.zeros(12, 4), plans, part))
    val zero = checked(VoxelwiseReducedRankGls.solve(zeroGs, part, 1, ReducedRankSolverConfig.unsafe()))
    close(zero.coefficients, Matrix.zeros(4, 4), 1e-12)
    assertEquals(zero.achievedRank, 0)
  }
