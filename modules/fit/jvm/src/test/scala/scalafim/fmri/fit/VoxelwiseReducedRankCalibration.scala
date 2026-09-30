package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{ArmaCoefficients, NoiseEstimationLayout, WhiteningPlan}
import scalafim.fmri.model.*
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.util.control.NonFatal

/** Opt-in, diagnostic-only Gaussian AR(1) bootstrap. No production policy is changed. */
object VoxelwiseReducedRankCalibration:
  private val ids = Vector(0, 1, 2)
  private val contrasts = Vector(
    VoxelwiseBootstrapContrast.unsafe("difference", Vector(0 -> 1.0, 1 -> -1.0)),
    VoxelwiseBootstrapContrast.unsafe("average", Vector(0 -> .5, 1 -> .5))
  )
  private final class Gaussian(seed: Int):
    require(seed > 0 && seed < 2147483647, "seed outside Park-Miller state space")
    private var state = seed.toLong
    private def uniform(): Double =
      state = state * 48271L % 2147483647L
      state.toDouble / 2147483647.0
    def next(): Double = math.sqrt(-2.0 * math.log(uniform())) * math.cos(2.0 * math.Pi * uniform())

  private[fit] def stationaryCovariance(rho: Vector[Double], sigma: DMat): Either[String, DMat] =
    if rho.isEmpty || rho.length != sigma.rows || sigma.rows != sigma.cols || rho.exists(r => !r.isFinite || math.abs(r) >= 1.0) then
      Left("nonfinite or unstable AR coefficient, or covariance dimension mismatch")
    else if !(0 until sigma.rows).forall(r => (0 until sigma.cols).forall(c =>
        sigma(r, c).isFinite && math.abs(sigma(r, c) - sigma(c, r)) <= 1e-14)) then
      Left("innovation covariance must be finite and symmetric")
    else Right(Matrix.tabulate(sigma.rows, sigma.cols)((u, v) => sigma(u, v) / (1.0 - rho(u) * rho(v))))

  private final case class NoiseLaw(rho: Vector[Double], sigma: DMat, initial: DMat, innovation: DMat)
  private def noiseLaw(rho: Vector[Double], sigma: DMat): Either[String, NoiseLaw] =
    for
      gamma <- stationaryCovariance(rho, sigma)
      initial <- gamma.cholesky.left.map(_.getMessage)
      innovation <- sigma.cholesky.left.map(_.getMessage)
    yield NoiseLaw(rho, sigma, initial.lower, innovation.lower)

  private def draw(lower: DMat, rng: Gaussian): Array[Double] =
    val z = Array.fill(lower.cols)(rng.next())
    Array.tabulate(lower.rows)(r => (0 to r).map(c => lower(r, c) * z(c)).sum)

  private def validatePhysical(observed: Vector[Vector[Int]], lengths: Vector[Int], count: Int): Either[String, Unit] =
    Either.cond(count > 0 && observed.length == count && lengths.length == count &&
      observed.zip(lengths).forall { case (rows, n) =>
        n > 0 && rows.nonEmpty && rows == rows.sorted && rows.distinct == rows && rows.forall(t => t >= 0 && t < n)
      }, (), "physical rows must be nonempty, ordered, unique, and within their own run")

  private def generate(laws: Vector[NoiseLaw], observed: Vector[Vector[Int]], lengths: Vector[Int], rng: Gaussian): Vector[Vector[Array[Double]]] =
    laws.indices.toVector.map { run =>
      val law = laws(run)
      val wanted = observed(run).toSet
      var state = draw(law.initial, rng)
      val output = Vector.newBuilder[Array[Double]]
      var t = 0
      while t < lengths(run) do
        if t > 0 then
          val epsilon = draw(law.innovation, rng)
          state = Array.tabulate(law.rho.length)(v => law.rho(v) * state(v) + epsilon(v))
        if wanted(t) then output += state.clone()
        t += 1
      output.result()
    }

  private[fit] def physicalAr1ByRun(
      rhos: Vector[Vector[Double]], sigmas: Vector[DMat], observedPhysicalRows: Vector[Vector[Int]], physicalLengths: Vector[Int], seed: Int
  ): Either[String, Vector[Vector[Array[Double]]]] =
    for
      _ <- validatePhysical(observedPhysicalRows, physicalLengths, rhos.length)
      _ <- Either.cond(sigmas.length == rhos.length && seed > 0 && seed < 2147483647, (), "invalid covariance count or seed")
      laws <- sequence(rhos.zip(sigmas).map((rho, sigma) => noiseLaw(rho, sigma)))
    yield generate(laws, observedPhysicalRows, physicalLengths, new Gaussian(seed))

  private[fit] def physicalAr1(
      rho: Vector[Double], sigma: DMat, observedPhysicalRows: Vector[Vector[Int]], physicalLengths: Vector[Int], seed: Int
  ): Either[String, Vector[Vector[Array[Double]]]] =
    physicalAr1ByRun(Vector.fill(physicalLengths.length)(rho), Vector.fill(physicalLengths.length)(sigma), observedPhysicalRows, physicalLengths, seed)

  private[fit] def pathFromNormals(rho: Vector[Double], sigma: DMat, initialZ: Vector[Double], innovationZ: Vector[Vector[Double]]): Either[String, Vector[Array[Double]]] =
    for
      law <- noiseLaw(rho, sigma)
      _ <- Either.cond((initialZ +: innovationZ).forall(z => z.length == rho.length && z.forall(_.isFinite)), (), "invalid normal vectors")
    yield
      def product(lower: DMat, z: Vector[Double]): Array[Double] = Array.tabulate(rho.length)(v => (0 to v).map(k => lower(v, k) * z(k)).sum)
      var state = product(law.initial, initialZ)
      val out = Vector.newBuilder[Array[Double]]
      out += state.clone()
      innovationZ.foreach { z =>
        val epsilon = product(law.innovation, z)
        state = Array.tabulate(rho.length)(v => rho(v) * state(v) + epsilon(v))
        out += state.clone()
      }
      out.result()

  private def sequence[A](values: Vector[Either[String, A]]): Either[String, Vector[A]] =
    values.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, next) => for a <- acc; b <- next yield a :+ b)

  /** Independent scalar common-factor DGP, with burn-in; does not use the Cholesky generator. */
  private def scalarNoise(rho: Vector[Double], correlation: Double, lengths: Vector[Int], seed: Int, lagged: Boolean): Vector[Vector[Array[Double]]] =
    val rng = new Gaussian(seed)
    lengths.map { length =>
      val state = Array.fill(rho.length)(0.0)
      var history = Vector.fill(3)(rng.next())
      def advance(): Array[Double] =
        val shared = rng.next()
        history = shared +: history.take(2)
        var v = 0
        while v < rho.length do
          val common = if lagged then history(v) else shared
          state(v) = rho(v) * state(v) + .6 * (math.sqrt(correlation) * common + math.sqrt(1.0 - correlation) * rng.next())
          v += 1
        state.clone()
      var t = 0
      while t < 500 do
        advance()
        t += 1
      Vector.tabulate(length)(_ => advance())
    }

  private[fit] def jointLagNoise(rho: Vector[Double], correlation: Double, physicalLengths: Vector[Int], seed: Int): Vector[Vector[Array[Double]]] =
    require(rho.length == 3 && correlation >= 0.0 && correlation <= 1.0 && physicalLengths.forall(_ > 0))
    scalarNoise(rho, correlation, physicalLengths, seed, lagged = true)

  private final case class Input(x: DMat, y: DMat, beta: DMat, runs: Vector[RunPartition], censors: Vector[Int], rho: Vector[Double], sigma: DMat, physical: Vector[Vector[Int]], lengths: Vector[Int], noiseLaw: String)
  private def trueSigma(correlation: Double): DMat = Matrix.tabulate(3, 3)((a, b) => .36 * (if a == b then 1.0 else correlation))

  private def input(inputId: Int, scenario: String): Input =
    val twoRuns = Set("multiple_runs", "censor_continuous")(scenario)
    val runCount = if twoRuns then 2 else 1
    val length = 80 / runCount
    val censored = scenario == "censor_continuous"
    val lagged = scenario == "joint_lag"
    val rho = if scenario == "strong_ar" then Vector(.1, .8, -.5) else Vector(.1, .5, -.2)
    val correlation = if scenario == "strong_ar" then .8 else .5
    val scale = if scenario == "weak" then .15 else if scenario == "boundary" then 0.0 else 1.0
    val selected = (0 until length).filterNot(t => censored && Set(16, 17)(t)).toVector
    val times = Vector.tabulate(runCount)(run => selected.map(_ + run * length)).flatten
    val runs = Vector.tabulate(runCount)(run => RunPartition(run, (run * selected.length until (run + 1) * selected.length).toVector, selected.map(_ + run * length)))
    val censors = if censored then Vector.tabulate(runCount)(run => Vector(8, 28).map(_ + run * length)).flatten else Vector.empty
    val x = Matrix.tabulate(times.length, 2 + runCount) { (r, c) =>
      val t = times(r) + 1.0
      if c == 0 then math.sin(t * .11) + .4 * math.cos(t * .29)
      else if c == 1 then math.cos(t * .17)
      else if r / selected.length == c - 2 then 1.0 else 0.0
    }
    val beta = Matrix.tabulate(2 + runCount, 3) { (r, v) =>
      if r < 2 then scale * Vector(1.2, -.8, .5)(v) * (if r == 0 then 1.0 else .6)
      else Vector(.4, -.2, .8)(v) + .1 * (r - 2)
    }
    val noise = scalarNoise(rho, correlation, Vector.fill(runCount)(length), 10007 + 7919 * inputId, lagged)
    val mean = x * beta
    val y = Matrix.tabulate(x.rows, 3)((r, v) => mean(r, v) + noise(r / selected.length)(selected(r % selected.length))(v))
    Input(x, y, beta, runs, censors, rho, trueSigma(if lagged then 0.0 else correlation),
      Vector.fill(runCount)(selected), Vector.fill(runCount)(length), if lagged then "scalar_ar1_shared_innovation_lags_0_1_2_burnin500" else "scalar_joint_ar1_burnin500")

  private def baselineInput(dataset: Int): Input =
    val (x, y, beta, runs) = VoxelwiseReducedRankQualification.baselineInput(dataset)
    Input(x, y, beta, runs, Vector.empty, Vector(.1, .5, -.2), trueSigma(.5), Vector((0 until 80).toVector), Vector(80), "archived_scalar_joint_ar1_burnin500")

  private def config(data: Input, rank: Int): ReducedRankGlsConfig = ReducedRankGlsConfig.unsafe(
    components = ReducedRankComponentSpec.unsafeFixed(rank),
    autocorrelation = AutocorrelationConfig.unsafe(order = 1, iterations = 1, voxelwise = true, censoredTimepoints = data.censors),
    inference = ReducedRankInferencePolicy.EstimatesOnly
  )
  private def partition(data: Input): ReducedRankDesignPartition = ReducedRankDesignPartition.fromColumns(data.x.cols, Vector(0, 1), (2 until data.x.cols).toVector).toOption.get
  private def estimatedPlans(data: Input): Either[FitError, Vector[WhiteningPlan]] =
    Gls.prepare(DesignMatrix.unsafe(data.x), ResponseBlock.unsafe(data.y), data.runs, config(data, 1).autocorrelation.toLegacy, ids).map { prepared =>
      prepared.whitening match
        case GlsWhitening.Voxelwise(ps) => ps
        case GlsWhitening.Shared(p) => Vector.fill(3)(p)
    }
  private def knownPlans(data: Input, layout: NoiseEstimationLayout): Vector[WhiteningPlan] = data.rho.map(r => WhiteningPlan.global(ArmaCoefficients.ar(r), layout.whiteningSegments))
  private def rhos(ps: Vector[WhiteningPlan], runs: Vector[RunPartition]): Vector[Vector[Double]] =
    runs.map { run =>
      ps.map { p =>
        val coefficients = p.segments.filter(_.runIndex == run.runIndex).map(s => p.coefficientsFor(s).phi)
        require(coefficients.nonEmpty && coefficients.forall(c => c.length == 1 && c == coefficients.head), "requires one AR1 coefficient per voxel/physical run")
        coefficients.head.head
      }
    }
  private def point(data: Input, ps: Vector[WhiteningPlan], rank: Int): Either[FitError, (Vector[VoxelwiseReducedRankGls.Geometry], VoxelwiseReducedRankGls.Solution)] =
    for
      gs <- VoxelwiseReducedRankGls.geometry(data.x, data.y, ps, partition(data))
      fit <- VoxelwiseReducedRankGls.solve(gs, partition(data), rank, config(data, rank).solver)
    yield (gs, fit)

  private[fit] def covarianceForRun(residuals: DMat, residualLeverage: DMat, layout: NoiseEstimationLayout, run: Int): Either[String, DMat] =
    val rows = layout.segmentsForRun(run).flatMap(s => (s.start + 1) until s.endExclusive)
    if rows.length < 2 then Left("requires two eligible innovation vectors per physical run")
    else if rows.exists(r => (0 until residuals.cols).exists(v => !residualLeverage(r, v).isFinite || residualLeverage(r, v) <= 1e-10 || !residuals(r, v).isFinite)) then Left("invalid innovation or HC2 leverage")
    else
      val values = rows.map(r => Vector.tabulate(residuals.cols)(v => residuals(r, v) / math.sqrt(residualLeverage(r, v))))
      val means = Vector.tabulate(residuals.cols)(v => values.map(_(v)).sum / values.length)
      val covariance = Matrix.tabulate(residuals.cols, residuals.cols)((a, b) => values.map(z => (z(a) - means(a)) * (z(b) - means(b))).sum / (values.length - 1))
      if (0 until covariance.rows).forall(a => (0 until covariance.cols).forall(b => covariance(a, b).isFinite)) then Right(covariance)
      else Left("nonfinite fitted innovation covariance")

  private def fittedCovariances(gs: Vector[VoxelwiseReducedRankGls.Geometry], layout: NoiseEstimationLayout, data: Input): Either[String, Vector[DMat]] =
    val residual = Matrix.tabulate(data.x.rows, 3)((r, v) => gs(v).fullResidual(r, 0))
    val leverage = Matrix.tabulate(data.x.rows, 3)((r, v) => gs(v).residualLeverage(r))
    sequence(data.runs.map(run => covarianceForRun(residual, leverage, layout, run.runIndex)))

  private final case class Arm(name: String, rank: Int, knownOriginal: Boolean, trueRho: Boolean, trueSigma: Boolean, refit: Boolean, residual: Option[VoxelwiseBootstrapMode] = None)
  private val arms = Vector(
    Arm("residual_frozen_full", 2, false, false, false, false, Some(VoxelwiseBootstrapMode.FrozenWhitening)),
    Arm("residual_refit_full", 2, false, false, false, true, Some(VoxelwiseBootstrapMode.RefitAutocorrelation)),
    Arm("oracle_known_rank1", 1, true, true, true, false),
    Arm("oracle_known_full", 2, true, true, true, false),
    Arm("oracle_refit_rank1", 1, false, true, true, true),
    Arm("oracle_refit_full", 2, false, true, true, true),
    Arm("plugin_refit_rank1", 1, false, false, false, true),
    Arm("plugin_refit_full", 2, false, false, false, true),
    Arm("true_rho_plugin_sigma_rank1", 1, false, true, false, true),
    Arm("plugin_rho_true_sigma_rank1", 1, false, false, true, true)
  )
  private[fit] def armPolicies: Vector[(String, Boolean, Boolean, Boolean, Boolean)] = arms.map(a => (a.name, a.knownOriginal, a.trueRho, a.trueSigma, a.refit))

  private[fit] def percentile(values: Vector[Double], p: Double): Double =
    require(values.nonEmpty && values.forall(_.isFinite) && p >= 0 && p <= 1)
    val sorted = values.sorted
    val position = p * (sorted.length - 1)
    val lower = position.toInt
    sorted(lower) + (position - lower) * (sorted(math.min(lower + 1, sorted.length - 1)) - sorted(lower))

  private def coordinates(beta: DMat): Vector[Double] =
    (for r <- Vector(0, 1); v <- ids yield beta(r, v)) ++ contrasts.flatMap(c => ids.map(v => c.weights.map((r, w) => w * beta(r, v)).sum))
  private final case class Metrics(estimate: Vector[Double], variance: Vector[Double], lower: Vector[Double], upper: Vector[Double], truth: Vector[Double])
  private def metrics(fit: VoxelwiseReducedRankGls.Solution, draws: Vector[Vector[Double]], truth: DMat): Metrics =
    val means = Vector.tabulate(12)(j => draws.map(_(j)).sum / draws.length)
    Metrics(coordinates(fit.coefficients), Vector.tabulate(12)(j => draws.map(x => math.pow(x(j) - means(j), 2)).sum / (draws.length - 1)),
      Vector.tabulate(12)(j => percentile(draws.map(_(j)), .025)), Vector.tabulate(12)(j => percentile(draws.map(_(j)), .975)), coordinates(truth))

  private def hash(matrix: DMat): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = ByteBuffer.allocate(8)
    for r <- 0 until matrix.rows; c <- 0 until matrix.cols do
      bytes.clear()
      bytes.putDouble(matrix(r, c))
      digest.update(bytes.array())
    digest.digest().map(b => f"${b & 0xff}%02x").mkString
  private def quote(s: String): String = "\"" + s.flatMap {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case c if c < ' ' => f"\\u${c.toInt}%04x"
    case c => c.toString
  } + "\""
  private def vector(xs: Iterable[Double]): String =
    require(xs.forall(_.isFinite), "nonfinite evidence vector")
    xs.mkString("[", ",", "]")
  private def rows(xs: Vector[Vector[Double]]): String = xs.map(vector).mkString("[", ",", "]")
  private def matrix(x: DMat): String = rows(Vector.tabulate(x.rows)(r => Vector.tabulate(x.cols)(c => x(r, c))))
  private def matrices(xs: Vector[DMat]): String = xs.map(matrix).mkString("[", ",", "]")
  private def opt[A](a: Option[A])(render: A => String): String = a.fold("null")(render)
  private def checked[A](a: Either[FitError, A]): A = a.fold(e => throw new IllegalStateException(e.message), identity)
  private def required[A](a: Either[String, A]): A = a.fold(e => throw new IllegalArgumentException(e), identity)

  private def one(writer: PrintWriter, scenario: String, dataset: Int, index: Int, data: Input, arm: Arm, count: Int, seed: Int): Unit =
    val started = System.nanoTime()
    var completed = 0
    var originalRho: Option[Vector[Vector[Double]]] = None
    var generatingRho: Option[Vector[Vector[Double]]] = None
    var pluginSigma: Option[Vector[DMat]] = None
    var generatingSigma: Option[Vector[DMat]] = None
    var covarianceError: Option[String] = None
    var traceMean: Option[Vector[Vector[Double]]] = None
    var traceSd: Option[Vector[Vector[Double]]] = None
    var hits: Option[Vector[Vector[Double]]] = None
    var result: Option[Metrics] = None
    var error: Option[String] = None
    try
      val layout = checked(Gls.noiseEstimationLayout(data.runs, data.censors))
      val ps = if arm.knownOriginal then knownPlans(data, layout) else checked(estimatedPlans(data))
      originalRho = Some(rhos(ps, data.runs))
      val (gs, fit) = checked(point(data, ps, arm.rank))
      fittedCovariances(gs, layout, data) match
        case Right(values) => pluginSigma = Some(values)
        case Left(message) => covarianceError = Some(message)
      arm.residual match
        case Some(mode) =>
          val boot = VoxelwiseReducedRankBootstrapConfig.unsafe(ReducedRankBootstrapConfig.unsafe(count, 1, seed), mode = mode, contrasts = contrasts)
          VoxelwiseReducedRankBootstrap.run(DesignMatrix.unsafe(data.x), data.y, data.runs, ids, ps, gs, partition(data), arm.rank, fit, config(data, arm.rank), boot) match
            case Left(e) =>
              completed = e match
                case FitError.ReducedRankBootstrapFailed(i, _) => i - 1
                case _ => 0
              throw new IllegalStateException(e.message)
            case Right(value) =>
              val u = value.bootstrap.get
              val covariance = ids.map(v => u.covariance.matrixForVoxelPosition(v).toOption.get)
              result = Some(Metrics(coordinates(fit.coefficients),
                (for r <- Vector(0, 1); v <- ids yield covariance(v)(r, r)) ++ u.contrasts.flatMap(_.standardErrors.toSeq.map(x => x * x)),
                (for r <- Vector(0, 1); v <- ids yield u.lower(r, v)) ++ u.contrasts.flatMap(_.lower.toSeq),
                (for r <- Vector(0, 1); v <- ids yield u.upper(r, v)) ++ u.contrasts.flatMap(_.upper.toSeq), coordinates(data.beta)))
              completed = count
        case None =>
          val gr = if arm.trueRho then Vector.fill(data.runs.length)(data.rho) else originalRho.get
          val covariance = if arm.trueSigma then Vector.fill(data.runs.length)(data.sigma)
            else pluginSigma.getOrElse(throw new IllegalArgumentException(covarianceError.getOrElse("missing fitted covariance")))
          generatingRho = Some(gr)
          generatingSigma = Some(covariance)
          val laws = required(sequence(gr.zip(covariance).map((r, s) => noiseLaw(r, s))))
          required(validatePhysical(data.physical, data.lengths, laws.length))
          val mean = data.x * fit.coefficients
          val rng = new Gaussian(seed)
          val samples = Vector.newBuilder[Vector[Double]]
          val traces = Vector.newBuilder[Vector[Vector[Double]]]
          while completed < count do
            val noise = generate(laws, data.physical, data.lengths, rng)
            val y = Matrix.newBuilder(data.y.rows, 3)
            data.runs.zip(noise).foreach { case (run, values) =>
              run.rowIndices.zip(values).foreach { case (r, e) =>
                for v <- ids do y(r, v) = mean(r, v) + e(v)
              }
            }
            val replicate = data.copy(y = y.result())
            val replicatePlans = if arm.refit then checked(estimatedPlans(replicate)) else ps
            val fitted = checked(point(replicate, replicatePlans, arm.rank))._2
            samples += coordinates(fitted.coefficients)
            traces += rhos(replicatePlans, data.runs)
            completed += 1
          val allTraces = traces.result()
          val means = Vector.tabulate(data.runs.length)(run => Vector.tabulate(3)(v => allTraces.map(_(run)(v)).sum / count))
          traceMean = Some(means)
          traceSd = Some(Vector.tabulate(data.runs.length)(run => Vector.tabulate(3)(v => math.sqrt(allTraces.map(t => math.pow(t(run)(v) - means(run)(v), 2)).sum / (count - 1)))))
          hits = Some(Vector.tabulate(data.runs.length)(run => Vector.tabulate(3)(v => allTraces.count(t => math.abs(t(run)(v)) >= .99 - 1e-12).toDouble)))
          result = Some(metrics(fit, samples.result(), data.beta))
      result.foreach { m =>
        require(Vector(m.estimate, m.variance, m.lower, m.upper, m.truth).forall(a => a.length == 12 && a.forall(_.isFinite)))
        require(m.variance.forall(_ >= 0.0) && m.lower.zip(m.upper).forall((a, b) => a <= b))
      }
    catch
      case NonFatal(e) =>
        error = Some(s"after $completed completed replicates: ${e.toString}")
        result = None
    val base = Vector(
      "scenario" -> quote(scenario), "dataset" -> dataset.toString, "dataset_index" -> index.toString,
      "input_seed" -> (10007 + 7919 * dataset).toString, "bootstrap_seed" -> seed.toString,
      "response_sha256" -> quote(hash(data.y)), "input_noise_law" -> quote(data.noiseLaw),
      "physical_run_ids" -> data.runs.map(_.runIndex).mkString("[", ",", "]"),
      "arm" -> quote(arm.name), "rank" -> arm.rank.toString, "replicates" -> count.toString,
      "original_whitening" -> quote(if arm.knownOriginal then "known" else "estimated"),
      "replicate_whitening" -> quote(if arm.refit then "refit" else "frozen"),
      "generator" -> quote(if arm.residual.nonEmpty then "production_residual" else "joint_gaussian_ar1"),
      "seconds" -> ((System.nanoTime() - started).toDouble / 1e9).toString,
      "status" -> quote(if result.nonEmpty then "ok" else "failed"), "completed_replicates" -> completed.toString,
      "error" -> opt(error)(quote), "original_rho_per_run_voxel" -> opt(originalRho)(rows),
      "generating_rho" -> opt(generatingRho)(rows), "fitted_sigma_per_run" -> opt(pluginSigma)(matrices),
      "generating_sigma_per_run" -> opt(generatingSigma)(matrices), "true_sigma" -> matrix(data.sigma),
      "true_rho_per_run_voxel" -> rows(Vector.fill(data.runs.length)(data.rho)),
      "covariance_diagnostic_error" -> opt(covarianceError)(quote),
      "replicate_rho_mean" -> opt(traceMean)(rows), "replicate_rho_sd" -> opt(traceSd)(rows),
      "stationarity_bound_hits" -> opt(hits)(rows)
    )
    val metricsFields = result.toVector.flatMap(m => Vector("estimate" -> vector(m.estimate), "variance" -> vector(m.variance), "lower" -> vector(m.lower), "upper" -> vector(m.upper), "truth" -> vector(m.truth)))
    writer.println((base ++ metricsFields).map((key, value) => quote(key) + ":" + value).mkString("{", ",", "}"))
    writer.flush()
    if writer.checkError() then throw new java.io.IOException("calibration evidence write failed")

  private def withWriter(path: String)(body: PrintWriter => Unit): Unit =
    require(!new java.io.File(path).exists(), "requires a new output JSONL path")
    val writer = new PrintWriter(path, "UTF-8")
    try body(writer)
    finally
      writer.close()
      if writer.checkError() then throw new java.io.IOException("calibration evidence close failed")

  def main(args: Array[String]): Unit =
    args.toList match
      case "development" :: path :: tail =>
        val count = tail.headOption.fold(400)(_.toInt)
        require(tail.length <= 1 && count > 0 && count <= 400)
        withWriter(path) { writer =>
          for d <- 0 until count do
            val data = baselineInput(d)
            for arm <- (if d < 100 then arms else arms.take(8)) do one(writer, "baseline", d, d, data, arm, 399, 2017 + 101 * d)
            if (d + 1) % 20 == 0 then println(s"RRG_CALIBRATION_DEVELOPMENT ${d + 1}/$count")
        }
      case "fresh" :: startText :: countText :: path :: Nil =>
        val start = startText.toInt
        val count = countText.toInt
        require(start >= 0 && count > 0 && start + count <= 2000)
        withWriter(path) { writer =>
          for scenario <- Vector("baseline", "strong_ar", "multiple_runs", "censor_continuous") do
            for index <- start until start + count do
              val id = 100000 + index
              one(writer, scenario, id, index, input(id, scenario), arms(6), 999, 660001 + 101 * id)
              if (index + 1) % 50 == 0 then println(s"RRG_CALIBRATION_FRESH $scenario ${index + 1}")
        }
      case "stress" :: path :: tail =>
        val count = tail.headOption.fold(200)(_.toInt)
        require(tail.length <= 1 && count > 0 && count <= 200)
        withWriter(path) { writer =>
          for scenario <- Vector("weak", "boundary", "joint_lag"); index <- 0 until count do
            val id = 200000 + index
            one(writer, scenario, id, index, input(id, scenario), arms(6), 399, 660001 + 101 * id)
            if (index + 1) % 50 == 0 then println(s"RRG_CALIBRATION_STRESS $scenario ${index + 1}/$count")
        }
      case _ => throw new IllegalArgumentException("development NEW.jsonl [count<=400] | fresh start count NEW.jsonl | stress NEW.jsonl [count<=200]")
