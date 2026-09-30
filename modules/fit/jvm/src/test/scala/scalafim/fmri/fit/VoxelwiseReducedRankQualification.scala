package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.model.*
import java.io.PrintWriter
import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Explicit, opt-in qualification driver; never runs as part of the unit suite.
  * Each dataset is recorded, including failures. Noise uses an independent scalar
  * AR recurrence with burn-in, not the production inverse whitening operation.
  */
object VoxelwiseReducedRankQualification:
  private final case class Scenario(
      name: String, runs: Int = 1, rho: Vector[Double] = Vector(0.1, 0.5, -0.2),
      correlation: Double = 0.5, scale: Double = 1.0, censored: Boolean = false,
      resetNoise: Boolean = false, lagged: Boolean = false, block: Int = 1,
      regular: Boolean = true
  )
  private val scenarios = Vector(
    Scenario("baseline"), Scenario("multiple_runs", runs = 2),
    Scenario("strong_ar", rho = Vector(0.1, 0.8, -0.5), correlation = 0.8),
    Scenario("censor_continuous", runs = 2, censored = true),
    Scenario("censor_reset", runs = 2, censored = true, resetNoise = true, regular = false),
    Scenario("joint_lag_block1", lagged = true, regular = false),
    Scenario("joint_lag_block4", lagged = true, block = 4, regular = false),
    Scenario("weak", scale = 0.15, regular = false),
    Scenario("boundary", scale = 0.0, regular = false)
  )
  private val contrasts = Vector(
    VoxelwiseBootstrapContrast.unsafe("difference", Vector(0 -> 1.0, 1 -> -1.0)),
    VoxelwiseBootstrapContrast.unsafe("average", Vector(0 -> 0.5, 1 -> 0.5))
  )
  private final class Gaussian(seed: Int):
    private var state = seed.toLong
    private def uniform(): Double =
      state = state * 48271L % 2147483647L
      state.toDouble / 2147483647.0
    def next(): Double = math.sqrt(-2.0 * math.log(uniform())) * math.cos(2.0 * math.Pi * uniform())

  private final case class Input(x: DMat, y: DMat, beta: DMat, runs: Vector[RunPartition], censors: Vector[Int])
  private def input(s: Scenario, dataset: Int): Input =
    val rng = new Gaussian(10007 + 7919 * dataset)
    val length = 80 / s.runs
    val selected = (0 until length).filterNot(t => s.censored && (t == 16 || t == 17)).toVector
    val times = (0 until s.runs).flatMap(run => selected.map(_ + run * length)).toVector
    val runs = Vector.tabulate(s.runs) { run =>
      val rows = (run * selected.length until (run + 1) * selected.length).toVector
      RunPartition(run, rows, selected.map(_ + run * length))
    }
    val censors = if s.censored then Vector.tabulate(s.runs)(r => Vector(8, 28).map(_ + r * length)).flatten else Vector.empty
    val x = Matrix.tabulate(times.length, 2 + s.runs) { (r, c) =>
      val t = times(r).toDouble + 1.0
      if c == 0 then math.sin(t * 0.11) + 0.4 * math.cos(t * 0.29)
      else if c == 1 then math.cos(t * 0.17)
      else if r / selected.length == c - 2 then 1.0 else 0.0
    }
    val beta = Matrix.tabulate(2 + s.runs, 3) { (r, v) =>
      if r >= 2 then Vector(0.4, -0.2, 0.8)(v) + 0.1 * (r - 2)
      else s.scale * Vector(1.2, -0.8, 0.5)(v) * (if r == 0 then 1.0 else 0.6)
    }
    val noise = Matrix.newBuilder(times.length, 3)
    for run <- 0 until s.runs do
      val state = Array.fill(3)(0.0)
      var previousShared = Vector.fill(3)(rng.next())
      def advance(): Unit =
        val shared = rng.next()
        previousShared = shared +: previousShared.take(2)
        for v <- 0 until 3 do
          val joint = if s.lagged then previousShared(v) else shared
          val innovation = 0.6 * (math.sqrt(s.correlation) * joint + math.sqrt(1.0 - s.correlation) * rng.next())
          state(v) = s.rho(v) * state(v) + innovation
      for _ <- 0 until 500 do advance()
      var outputRow = run * selected.length
      for t <- 0 until length do
        // Independent reset diagnostic: match the declared segment boundaries,
        // but generate each segment's stationary start by scalar burn-in.
        if s.resetNoise && Set(9, 18, 29).contains(t) then
          java.util.Arrays.fill(state, 0.0)
          for _ <- 0 until 500 do advance()
        advance()
        if selected.contains(t) then
          for v <- 0 until 3 do noise(outputRow, v) = state(v)
          outputRow += 1
    val mean = x * beta
    val errors = noise.result()
    Input(x, Matrix.tabulate(x.rows, 3)((r, v) => mean(r, v) + errors(r, v)), beta, runs, censors)

  /** Reuses the exact baseline generator for the post-study causal diagnostic. */
  private[fit] def baselineInput(dataset: Int): (DMat, DMat, DMat, Vector[RunPartition]) =
    val data = input(scenarios.head, dataset)
    (data.x, data.y, data.beta, data.runs)

  private def fit(data: Input, mode: VoxelwiseBootstrapMode, count: Int, seed: Int, block: Int): Either[FitError, VoxelwiseReducedRankEstimate] =
    val config = ReducedRankGlsConfig.unsafe(
      components = ReducedRankComponentSpec.unsafeFixed(1),
      autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true, censoredTimepoints = data.censors),
      inference = ReducedRankInferencePolicy.VoxelwiseBootstrap(VoxelwiseReducedRankBootstrapConfig.unsafe(
        resampling = ReducedRankBootstrapConfig.unsafe(count, block, seed), mode = mode, contrasts = contrasts
      ))
    )
    val design = DesignMatrix.unsafe(data.x)
    val response = ResponseBlock.unsafe(data.y)
    for
      partition <- ReducedRankDesignPartition.fromColumns(data.x.cols, Vector(0, 1), (2 until data.x.cols).toVector)
      prepared <- ReducedRankGlsPrepared.prepare(design, response, data.runs, config, Vector(0, 1, 2), partition)
      result <- prepared.fitBlock(FitBlockInput(design, response, Vector(0, 1, 2), data.runs.flatMap(_.timepoints), data.runs))
      estimate <- result match
        case value: VoxelwiseReducedRankFitBlockResult => Right(value.estimate)
        case _ => Left(FitError.UnsupportedEngine("qualification requires explicit voxelwise result"))
    yield estimate

  private def quote(s: String): String = "\"" + s.flatMap {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c => c.toString
  } + "\""
  private def array(x: Iterable[Double]): String = x.mkString("[", ",", "]")
  private def measured(writer: PrintWriter, s: Scenario, d: Int, mode: VoxelwiseBootstrapMode, b: Int, seed: Int, budget: String): Unit =
    val started = System.nanoTime()
    try measuredAttempt(writer, s, d, mode, b, seed, budget)
    catch
      case NonFatal(error) =>
        val seconds = (System.nanoTime() - started).toDouble / 1e9
        writer.println(s"{\"scenario\":${quote(s.name)},\"dataset\":$d,\"mode\":${quote(mode.toString)},\"replicates\":$b,\"seed\":$seed,\"budget\":${quote(budget)},\"seconds\":$seconds,\"status\":\"failed\",\"completed_replicates\":null,\"error\":${quote(error.getClass.getName + ": " + Option(error.getMessage).getOrElse(""))}}")
    writer.flush()
    if writer.checkError() then throw new java.io.IOException("qualification evidence write failed")

  private def measuredAttempt(writer: PrintWriter, s: Scenario, d: Int, mode: VoxelwiseBootstrapMode, b: Int, seed: Int, budget: String): Unit =
    val data = input(s, d)
    val start = System.nanoTime()
    val result = fit(data, mode, b, seed, s.block)
    val seconds = (System.nanoTime() - start).toDouble / 1e9
    val base = s"\"scenario\":${quote(s.name)},\"dataset\":$d,\"mode\":${quote(mode.toString)},\"replicates\":$b,\"seed\":$seed,\"budget\":${quote(budget)},\"seconds\":$seconds"
    result match
      case Left(error) =>
        val completed = error match
          case FitError.ReducedRankBootstrapFailed(index, _) => index - 1
          case _ => 0
        writer.println(s"{$base,\"status\":\"failed\",\"completed_replicates\":$completed,\"error\":${quote(error.message)}}")
      case Right(value) =>
        val u = value.uncertainty.bootstrap.get
        val covariance = (0 until 3).map(v => u.covariance.matrixForVoxelPosition(v).toOption.get)
        val estimates = (for r <- 0 until 2; v <- 0 until 3 yield value.coefficients(r, v)) ++ u.contrasts.flatMap(_.estimate.toSeq)
        val variances = (for r <- 0 until 2; v <- 0 until 3 yield covariance(v)(r, r)) ++ u.contrasts.flatMap(_.standardErrors.toSeq.map(x => x * x))
        val lower = (for r <- 0 until 2; v <- 0 until 3 yield u.lower(r, v)) ++ u.contrasts.flatMap(_.lower.toSeq)
        val upper = (for r <- 0 until 2; v <- 0 until 3 yield u.upper(r, v)) ++ u.contrasts.flatMap(_.upper.toSeq)
        val truth = (for r <- 0 until 2; v <- 0 until 3 yield data.beta(r, v)) ++ contrasts.flatMap(c => (0 until 3).map(v => c.weights.map((r, w) => w * data.beta(r, v)).sum))
        require(u.contrasts.map(_.definition) == contrasts, "contrast order differs from request")
        require(Vector(estimates, variances, lower, upper, truth).forall(a => a.length == 12 && a.forall(_.isFinite)), "invalid qualification metric arrays")
        require(variances.forall(_ >= 0.0) && lower.zip(upper).forall((a, b) => a <= b), "invalid uncertainty")
        writer.println(s"{$base,\"status\":\"ok\",\"completed_replicates\":$b,\"estimate\":${array(estimates)},\"variance\":${array(variances)},\"lower\":${array(lower)},\"upper\":${array(upper)},\"truth\":${array(truth)},\"objective\":${value.diagnostics.objective}}")
    writer.flush()

  private def selfcheck(): Unit =
    def identical(a: DMat, b: DMat): Boolean =
      a.rows == b.rows && a.cols == b.cols && (0 until a.rows).forall(r => (0 until a.cols).forall(c => a(r, c) == b(r, c)))
    scenarios.foreach { scenario =>
      val a = input(scenario, 0)
      val b = input(scenario, 0)
      require(identical(a.y, b.y), "noise generator must reproduce the same input exactly")
      require(a.y.rows == (if scenario.censored then 76 else 80), "unexpected selected row count")
      if scenario.censored then
        require(a.censors.length == 4 && a.censors.forall(a.runs.flatMap(_.timepoints).contains), "retained noise censor mismatch")
        val segments = Gls.noiseEstimationLayout(a.runs, a.censors).toOption.get.whiteningSegments
        a.runs.foreach { run =>
          val starts = segments.filter(_.runIndex == run.runIndex).map(s => run.timepoints(s.start - run.rowIndices.head) % 40)
          require(starts == Vector(0, 9, 18, 29), s"independent segment generator differs from declared whitening starts: $starts")
        }
    }
    require(identical(input(scenarios.find(_.name == "joint_lag_block1").get, 7).y, input(scenarios.find(_.name == "joint_lag_block4").get, 7).y), "block comparison must use identical observations")
    println("RRG_GENERATOR_SELFCHECK passed")

  private def study(path: String, kind: String): Unit =
    selfcheck()
    val file = new java.io.File(path)
    require(!file.exists(), "refusing to overwrite qualification evidence")
    val writer = new PrintWriter(file, "UTF-8")
    try
      for s <- scenarios do
        val datasets = if kind == "pilot" then 10 else if kind == "smoke" then 1 else if s.regular then 400 else if s.resetNoise then 100 else 200
        for d <- 0 until datasets; mode <- VoxelwiseBootstrapMode.values do
          val seed = 2017 + 101 * d
          measured(writer, s, d, mode, 399, seed, "main")
          if kind != "pilot" && d < 100 && Set("baseline", "censor_continuous").contains(s.name) then
            measured(writer, s, d, mode, 1999, seed, "nested")
          if kind != "pilot" && s.name == "baseline" && d < 20 then
            for independent <- 1 to 4 do
              measured(writer, s, d, mode, 399, seed + independent * 100003, s"independent_$independent")
          if (d + 1) % 50 == 0 then println(s"RRG_STUDY_PROGRESS ${s.name} $mode ${d + 1}/$datasets")
        println(s"RRG_STUDY_COMPLETE ${s.name} datasets=$datasets")
    finally writer.close()

  private def performance(voxels: Int, replicates: Int, output: String): Unit =
    require(voxels >= 2 && (replicates == 0 || replicates >= 2), "requires >=2 voxels and zero or >=2 replicates")
    require(!new java.io.File(output).exists(), "refusing to overwrite performance evidence")
    // Fixed warm-up is excluded from the preparation timer; external RSS still
    // includes this process's warm-up, generated inputs, and final extraction.
    for d <- 0 until 10 do
      require(fit(input(scenarios.head, d), VoxelwiseBootstrapMode.RefitAutocorrelation, 2, 19, 1).isRight)
    val n = 240
    val p = 8
    val runs = Vector.tabulate(2)(r => RunPartition(r, (r * 120 until (r + 1) * 120).toVector, (r * 120 until (r + 1) * 120).toVector))
    val x = Matrix.tabulate(n, p + 2) { (t, c) =>
      if c < p then math.sin((t + 1) * (c + 1) * 0.031) + 0.3 * math.cos((t + 1) * (c + 2) * 0.073)
      else if t / 120 == c - p then 1.0 else 0.0
    }
    val beta = Matrix.tabulate(p + 2, voxels) { (r, v) =>
      if r < p then math.sin((r + 1) * 0.4) * math.cos((v + 1) * 0.1) + math.cos((r + 1) * 0.7) * math.sin((v + 1) * 0.17)
      else 0.1 * (r - p + 1)
    }
    val rng = new Gaussian(73019)
    val y = Matrix.newBuilder(n, voxels)
    val mean = x * beta
    for v <- 0 until voxels do
      val rho = -0.2 + 0.8 * (v % 101).toDouble / 100.0
      var noise = 0.0
      for t <- 0 until n do
        if t % 120 == 0 then
          noise = 0.0
          for _ <- 0 until 500 do noise = rho * noise + 0.6 * rng.next()
        noise = rho * noise + 0.6 * rng.next()
        y(t, v) = mean(t, v) + noise
    val design = DesignMatrix.unsafe(x)
    val response = ResponseBlock.unsafe(y.result())
    val partition = ReducedRankDesignPartition.fromColumns(p + 2, (0 until p).toVector, Vector(p, p + 1)).toOption.get
    val config = ReducedRankGlsConfig.unsafe(
      components = ReducedRankComponentSpec.unsafeFixed(2),
      autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true),
      inference = if replicates == 0 then ReducedRankInferencePolicy.EstimatesOnly else
        ReducedRankInferencePolicy.VoxelwiseBootstrap(VoxelwiseReducedRankBootstrapConfig.unsafe(
          resampling = ReducedRankBootstrapConfig.unsafe(replicates, 1, 17029), mode = VoxelwiseBootstrapMode.RefitAutocorrelation, contrasts = contrasts
        ))
    )
    val ids = (0 until voxels).toVector
    val collectors = ManagementFactory.getGarbageCollectorMXBeans.asScala.toVector
    val gcBefore = collectors.map(x => (x.getCollectionCount, x.getCollectionTime))
    val heapPools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == java.lang.management.MemoryType.HEAP).toVector
    heapPools.foreach(_.resetPeakUsage())
    val start = System.nanoTime()
    val result = ReducedRankGlsPrepared.prepare(design, response, runs, config, ids, partition)
    val seconds = (System.nanoTime() - start).toDouble / 1e9
    val gc = collectors.zip(gcBefore).map { case (collector, (count, time)) => (collector.getCollectionCount - count, collector.getCollectionTime - time) }
    val heapPeak = heapPools.map(_.getPeakUsage.getUsed).sum
    val outcome = result match
      case Left(error) => s"\"status\":\"failed\",\"error\":${quote(error.message)}"
      case Right(prepared) =>
        val block = prepared.fitBlock(FitBlockInput(design, response, ids, (0 until n).toVector, runs)).toOption.get.asInstanceOf[VoxelwiseReducedRankFitBlockResult]
        s"\"status\":\"ok\",\"objective\":${block.estimate.diagnostics.objective},\"rank\":${block.estimate.diagnostics.achievedRank}"
    val writer = new PrintWriter(output, "UTF-8")
    try writer.println(s"{\"timepoints\":$n,\"targets\":$p,\"nuisance\":2,\"voxels\":$voxels,\"replicates\":$replicates,\"seconds\":$seconds,\"heap_pool_peak_sum_bytes\":$heapPeak,\"gc_count\":${gc.map(_._1).sum},\"gc_ms\":${gc.map(_._2).sum},$outcome}")
    finally
      writer.close()
      if writer.checkError() then throw new java.io.IOException("performance evidence write failed")

  def main(args: Array[String]): Unit =
    args.toList match
      case kind :: path :: Nil if Set("pilot", "study", "smoke").contains(kind) => study(path, kind)
      case "selfcheck" :: Nil => selfcheck()
      case "performance" :: voxels :: replicates :: path :: Nil => performance(voxels.toInt, replicates.toInt, path)
      case _ => throw new IllegalArgumentException("pilot|study|smoke output.jsonl OR performance voxels replicates output.json OR selfcheck")
