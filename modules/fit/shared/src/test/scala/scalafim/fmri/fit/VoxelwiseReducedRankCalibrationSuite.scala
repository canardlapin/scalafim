package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.model.*

/** Bounded synthetic pilot, deliberately separate from nominal inference claims.
  * Emits machine-readable evidence on both platforms. Regular scenarios use
  * 80 independent datasets, 99 replicates, and a prespecified first-three subset
  * with 399 replicates to measure interval endpoint Monte Carlo sensitivity.
  */
class VoxelwiseReducedRankCalibrationSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "minutes")
  private def checked[A](value: Either[FitError, A]): A = value.fold(e => fail(e.message), identity)
  private val n = 80
  private val rows = (0 until n).toVector
  private val runs = Vector(RunPartition(0, rows, rows))
  private val ids = Vector(0, 1, 2)
  private val partition = checked(ReducedRankDesignPartition.fromColumns(3, Vector(0, 1), Vector(2)))
  private val x = Matrix.tabulate(n, 3) { (r, c) =>
    val t = r.toDouble + 1.0
    c match
      case 0 => math.sin(t * 0.11) + 0.4 * math.cos(t * 0.29)
      case 1 => math.cos(t * 0.17)
      case _ => 1.0
  }
  private val design = DesignMatrix.unsafe(x)
  private val plans = Vector(0.1, 0.5, -0.2).map(r => WhiteningPlan.global(ArmaCoefficients.ar(r), Vector(TimeSegment(0, n, 0))))

  private final class Gaussian(seed: Int):
    private var state = seed.toLong
    private def uniform(): Double =
      state = state * 48271L % 2147483647L
      state.toDouble / 2147483647.0
    def next(): Double = math.sqrt(-2.0 * math.log(uniform())) * math.cos(2.0 * math.Pi * uniform())

  private def truth(scale: Double): DMat = Matrix.tabulate(3, 3) { (r, v) =>
    if r == 2 then Vector(0.4, -0.2, 0.8)(v)
    else scale * Vector(1.2, -0.8, 0.5)(v) * (if r == 0 then 1.0 else 0.6)
  }

  private def response(seed: Int, correlation: Double, beta: DMat): DMat =
    val rng = new Gaussian(seed)
    val e = Matrix.newBuilder(n, 3)
    for r <- 0 until n do
      val shared = rng.next()
      for v <- 0 until 3 do e(r, v) = 0.6 * (math.sqrt(correlation) * shared + math.sqrt(1.0 - correlation) * rng.next())
    val innovations = e.result()
    val noise = plans.indices.map(v => checked(VoxelwiseReducedRankBootstrap.inverseWhiten(plans(v), VoxelwiseReducedRankGls.columns(innovations, Vector(v)))))
    val mean = x * beta
    Matrix.tabulate(n, 3)((r, v) => mean(r, v) + noise(v)(r, 0))

  private def fit(y: DMat, mode: VoxelwiseBootstrapMode, count: Int, seed: Int): VoxelwiseReducedRankEstimate =
    val config = ReducedRankGlsConfig.unsafe(
      components = ReducedRankComponentSpec.unsafeFixed(1),
      autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true),
      inference = ReducedRankInferencePolicy.VoxelwiseBootstrap(VoxelwiseReducedRankBootstrapConfig.unsafe(
        resampling = ReducedRankBootstrapConfig.unsafe(replicates = count, blockSize = 1, seed = seed), mode = mode
      ))
    )
    val block = ResponseBlock.unsafe(y)
    val prepared = checked(ReducedRankGlsPrepared.prepare(design, block, runs, config, ids, partition))
    checked(prepared.fitBlock(FitBlockInput(design, block, ids, rows, runs))) match
      case value: VoxelwiseReducedRankFitBlockResult => value.estimate
      case _ => fail("expected explicit voxelwise reduced-rank result")

  private def jsonArray(values: Iterable[Double]): String = values.mkString("[", ",", "]")
  private def pilot(correlation: Double, mode: VoxelwiseBootstrapMode, datasets: Int, scale: Double, regular: Boolean): Unit =
    val beta = truth(scale)
    val estimates = Array.ofDim[Double](datasets, 6)
    val variances = Array.ofDim[Double](datasets, 6)
    val coverage = Array.fill(6)(0)
    val endpointDifferences = Vector.newBuilder[Double]
    for d <- 0 until datasets do
      val y = response(10007 + 7919 * d, correlation, beta)
      val result = fit(y, mode, 99, 2017 + 101 * d)
      val u = result.uncertainty.bootstrap.get
      assertEquals(u.diagnostics.replicateObjectives.length, 99)
      assertEquals(u.diagnostics.refittedWhiteningReplicates, if mode == VoxelwiseBootstrapMode.RefitAutocorrelation then 99 else 0)
      for r <- 0 until 2; v <- 0 until 3 do
        val j = r * 3 + v
        estimates(d)(j) = result.coefficients(r, v)
        variances(d)(j) = checked(u.covariance.matrixForVoxelPosition(v))(r, r)
        assert(variances(d)(j).isFinite && variances(d)(j) >= 0.0)
        assert(u.lower(r, v).isFinite && u.upper(r, v).isFinite && u.lower(r, v) <= u.upper(r, v))
        if u.lower(r, v) <= beta(r, v) && beta(r, v) <= u.upper(r, v) then coverage(j) += 1
      if regular && d < 3 then
        val larger = fit(y, mode, 399, 2017 + 101 * d).uncertainty.bootstrap.get
        for r <- 0 until 2; v <- 0 until 3 do
          endpointDifferences += math.abs(larger.lower(r, v) - u.lower(r, v))
          endpointDifferences += math.abs(larger.upper(r, v) - u.upper(r, v))
    val rates = coverage.map(_.toDouble / datasets)
    val means = (0 until 6).map(j => estimates.map(_(j)).sum / datasets)
    val empirical = (0 until 6).map(j => estimates.map(a => math.pow(a(j) - means(j), 2)).sum / (datasets - 1))
    val averageVariance = (0 until 6).map(j => variances.map(_(j)).sum / datasets)
    val ratio = (0 until 6).map(j => averageVariance(j) / empirical(j))
    assert(empirical.forall(v => v.isFinite && v > 0.0), "sampling variance must be finite and positive")
    assert(ratio.forall(_.isFinite), "variance ratios must be finite")
    val bias = (0 until 6).map(j => means(j) - beta(j / 3, j % 3))
    val rmse = (0 until 6).map(j => math.sqrt(estimates.map(a => math.pow(a(j) - beta(j / 3, j % 3), 2)).sum / datasets))
    val wilson = rates.map { p =>
      val z = 1.959963984540054
      val den = 1.0 + z * z / datasets
      val center = (p + z * z / (2.0 * datasets)) / den
      val half = z / den * math.sqrt(p * (1.0 - p) / datasets + z * z / (4.0 * datasets * datasets))
      jsonArray(Vector(center - half, center + half))
    }
    val grossFailure = rates.exists(_ < 0.85) || ratio.exists(r => r < 0.5 || r > 2.0)
    val sensitivity = endpointDifferences.result()
    println(s"RRG_CALIBRATION_JSON={\"mode\":\"$mode\",\"innovation_correlation\":$correlation,\"signal_scale\":$scale,\"regular\":$regular,\"datasets\":$datasets,\"replicates\":99,\"coverage\":${jsonArray(rates.toVector)},\"coverage_wilson95\":${wilson.mkString("[", ",", "]")},\"sampling_variance\":${jsonArray(empirical)},\"mean_bootstrap_variance\":${jsonArray(averageVariance)},\"variance_ratio\":${jsonArray(ratio)},\"bias\":${jsonArray(bias)},\"rmse\":${jsonArray(rmse)},\"higher_replicate_subset\":${if regular then 3 else 0},\"higher_replicate_count\":399,\"mean_absolute_endpoint_change\":${if sensitivity.isEmpty then 0.0 else sensitivity.sum / sensitivity.length},\"gross_failure_flag\":$grossFailure}")
    if regular then assert(!grossFailure, "bounded calibration pilot crossed prespecified investigation thresholds; inspect emitted evidence")

  for correlation <- Vector(0.0, 0.5); mode <- Vector(VoxelwiseBootstrapMode.FrozenWhitening, VoxelwiseBootstrapMode.RefitAutocorrelation) do
    test(s"regular rank-one calibration pilot correlation=$correlation mode=$mode") {
      pilot(correlation, mode, datasets = 80, scale = 1.0, regular = true)
    }

  test("rank-boundary pilot is reported separately and does not authorize nominal coverage") {
    pilot(0.5, VoxelwiseBootstrapMode.RefitAutocorrelation, datasets = 40, scale = 0.0, regular = false)
  }
