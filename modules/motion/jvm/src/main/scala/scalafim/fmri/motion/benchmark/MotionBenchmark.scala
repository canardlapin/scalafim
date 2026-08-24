package scalafim.fmri.motion.benchmark

import scalafim.image.SampleSpaces.*

import scalafim.fmri.motion.*
import scalafim.fmri.motion.io.MotionReportWriter
import scalafim.image.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

enum MotionBenchmarkError:
  case InvalidScenario(name: String, reason: String)
  case MotionFailure(scenario: String, error: MotionError)
  case Io(path: Path, reason: String)
  case InvalidExternalCsv(path: Path, reason: String)

  def message: String =
    this match
      case InvalidScenario(name, reason) =>
        s"invalid motion benchmark scenario '$name': $reason"
      case MotionFailure(scenario, error) =>
        s"motion benchmark '$scenario' failed: ${error.message}"
      case Io(path, reason) =>
        s"motion benchmark I/O failure at $path: $reason"
      case InvalidExternalCsv(path, reason) =>
        s"invalid external motion benchmark CSV $path: $reason"

final case class MotionBenchmarkScenario(
    name: String,
    dims: Vector[Int],
    truth: MotionTrace,
    noiseScale: Double,
    nuisanceGain: Double
):
  def nVolumes: Int = truth.length

final case class MotionBenchmarkThresholds(
    maxFdError: Double,
    maxDispP95: Double,
    minTsnrRatio: Double,
    maxElapsedSec: Double
)

object MotionBenchmarkThresholds:
  val default: MotionBenchmarkThresholds =
    MotionBenchmarkThresholds(
      maxFdError = 10.0,
      maxDispP95 = 20.0,
      minTsnrRatio = 0.50,
      maxElapsedSec = 30.0
    )

final case class MotionBenchmarkResult(
    scenario: String,
    status: String,
    nFrames: Int,
    estimateSec: Double,
    applySec: Double,
    reportSec: Double,
    elapsedSec: Double,
    approxBytes: Long,
    tsnrRatio: Double,
    dispP95: Double,
    fdError: Double,
    costFinalMean: Double
):
  def csvRow: Vector[String] =
    Vector(
      scenario,
      status,
      nFrames.toString,
      MotionBenchmark.formatDouble(estimateSec),
      MotionBenchmark.formatDouble(applySec),
      MotionBenchmark.formatDouble(reportSec),
      MotionBenchmark.formatDouble(elapsedSec),
      approxBytes.toString,
      MotionBenchmark.formatDouble(tsnrRatio),
      MotionBenchmark.formatDouble(dispP95),
      MotionBenchmark.formatDouble(fdError),
      MotionBenchmark.formatDouble(costFinalMean)
    )

final case class MotionBenchmarkCheck(metric: String, actual: Double, threshold: Double, passed: Boolean, direction: String):
  def csvRow: Vector[String] =
    Vector(metric, MotionBenchmark.formatDouble(actual), MotionBenchmark.formatDouble(threshold), direction, passed.toString)

final case class MotionBenchmarkFiles(rawCsv: Path, checksCsv: Path, summaryMarkdown: Path)

final case class ExternalBenchmarkSummary(
    path: Path,
    nRows: Int,
    scenarios: Vector[String],
    methods: Vector[String],
    missingColumns: Vector[String]
):
  def isUsable: Boolean =
    missingColumns.isEmpty && nRows > 0

object MotionBenchmark:
  private def timeAxis(extent: Int): image4s.Axis =
    image4s.Axis
      .ordinal("time", image4s.AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  val RawColumns: Vector[String] =
    Vector(
      "scenario",
      "status",
      "n_frames",
      "estimate_sec",
      "apply_sec",
      "report_sec",
      "elapsed_sec",
      "approx_bytes",
      "tsnr_ratio",
      "disp_p95",
      "fd_error",
      "cost_final_mean"
    )

  val ExternalRequiredColumns: Vector[String] =
    Vector("estimate_sec", "apply_sec", "report_sec", "elapsed_sec", "tsnr_ratio", "disp_p95", "fd_error")

  def smokeScenarios: Vector[MotionBenchmarkScenario] =
    Vector(
      scenario(
        name = "smoke_low_motion",
        dims = Vector(8, 8, 4),
        translations = Vector((0.0, 0.0, 0.0), (0.25, -0.15, 0.05), (0.45, -0.25, 0.10)),
        noiseScale = 0.05,
        nuisanceGain = 0.01
      )
    )

  def defaultScenarios: Vector[MotionBenchmarkScenario] =
    Vector(
      scenario(
        name = "low_motion",
        dims = Vector(12, 12, 6),
        translations = Vector((0.0, 0.0, 0.0), (0.35, -0.20, 0.05), (0.65, -0.35, 0.15), (0.40, -0.10, 0.05)),
        noiseScale = 0.08,
        nuisanceGain = 0.01
      ),
      scenario(
        name = "moderate_motion",
        dims = Vector(12, 12, 6),
        translations = Vector((0.0, 0.0, 0.0), (0.70, -0.40, 0.20), (1.00, -0.60, 0.25), (0.55, -0.20, 0.10)),
        noiseScale = 0.12,
        nuisanceGain = 0.03
      ),
      scenario(
        name = "hard_motion_plus_nuisance",
        dims = Vector(12, 12, 6),
        translations = Vector((0.0, 0.0, 0.0), (1.20, -0.70, 0.35), (1.45, -0.85, 0.45), (0.80, -0.30, 0.20)),
        noiseScale = 0.18,
        nuisanceGain = 0.06
      )
    )

  def runSynthetic(
      scenarios: Vector[MotionBenchmarkScenario] = defaultScenarios,
      plan: MotionPlan = MotionPlan.default,
      thresholds: MotionBenchmarkThresholds = MotionBenchmarkThresholds.default,
      nowNanos: () => Long = () => System.nanoTime()
  ): Either[MotionBenchmarkError, Vector[MotionBenchmarkResult]] =
    val out = Vector.newBuilder[MotionBenchmarkResult]
    var i = 0
    while i < scenarios.length do
      val scenario = scenarios(i)
      validate(scenario) match
        case Left(error) => return Left(error)
        case Right(())  => ()
      runOne(scenario, plan, thresholds, nowNanos) match
        case Left(error)  => return Left(error)
        case Right(value) => out += value
      i += 1
    Right(out.result())

  def checks(result: MotionBenchmarkResult, thresholds: MotionBenchmarkThresholds): Vector[MotionBenchmarkCheck] =
    Vector(
      MotionBenchmarkCheck("fd_error", result.fdError, thresholds.maxFdError, result.fdError <= thresholds.maxFdError, "lte"),
      MotionBenchmarkCheck("disp_p95", result.dispP95, thresholds.maxDispP95, result.dispP95 <= thresholds.maxDispP95, "lte"),
      MotionBenchmarkCheck("tsnr_ratio", result.tsnrRatio, thresholds.minTsnrRatio, result.tsnrRatio >= thresholds.minTsnrRatio, "gte"),
      MotionBenchmarkCheck("elapsed_sec", result.elapsedSec, thresholds.maxElapsedSec, result.elapsedSec <= thresholds.maxElapsedSec, "lte")
    )

  def writeReport(
      directory: Path,
      results: Vector[MotionBenchmarkResult],
      thresholds: MotionBenchmarkThresholds = MotionBenchmarkThresholds.default
  ): Either[MotionBenchmarkError, MotionBenchmarkFiles] =
    val rawPath = directory.resolve("motion_benchmark_raw.csv")
    val checksPath = directory.resolve("motion_benchmark_checks.csv")
    val summaryPath = directory.resolve("motion_benchmark_summary.md")
    try
      Files.createDirectories(directory)
      Files.writeString(rawPath, rawCsv(results), StandardCharsets.UTF_8)
      Files.writeString(checksPath, checksCsv(results, thresholds), StandardCharsets.UTF_8)
      Files.writeString(summaryPath, summaryMarkdown(results, thresholds), StandardCharsets.UTF_8)
      Right(MotionBenchmarkFiles(rawPath, checksPath, summaryPath))
    catch
      case NonFatal(e) =>
        Left(MotionBenchmarkError.Io(directory, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))

  def externalSummary(path: Path): Either[MotionBenchmarkError, ExternalBenchmarkSummary] =
    try
      val text = Files.readString(path, StandardCharsets.UTF_8)
      parseCsv(text) match
        case Left(reason) =>
          Left(MotionBenchmarkError.InvalidExternalCsv(path, reason))
        case Right((columns, rows)) =>
          val missing = ExternalRequiredColumns.filterNot(columns.contains)
          val scenarioIndex = columns.indexOf("scenario")
          val methodIndex = columns.indexOf("method")
          val scenarios = distinctColumn(rows, scenarioIndex)
          val methods = distinctColumn(rows, methodIndex)
          Right(ExternalBenchmarkSummary(path, rows.length, scenarios, methods, missing))
    catch
      case NonFatal(e) =>
        Left(MotionBenchmarkError.Io(path, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))

  private def runOne(
      scenario: MotionBenchmarkScenario,
      plan: MotionPlan,
      thresholds: MotionBenchmarkThresholds,
      nowNanos: () => Long
  ): Either[MotionBenchmarkError, MotionBenchmarkResult] =
    val run = renderRun(scenario)
    val elapsedStart = nowNanos()
    val estimateStart = nowNanos()
    val estimate =
      MotionEstimator.estimate(run, mask = None, plan = plan) match
        case Left(error)  => return Left(MotionBenchmarkError.MotionFailure(scenario.name, error))
        case Right(value) => value
    val estimateEnd = nowNanos()

    val applyStart = nowNanos()
    val corrected =
      MotionCorrectionResult
        .fromEstimate(run, estimate, plan, mask = None, applyControl = ApplyControl.linear, qcPolicy = MotionQcPolicy.default)
        .left
        .map(MotionBenchmarkError.MotionFailure(scenario.name, _)) match
        case Left(error)  => return Left(error)
        case Right(value) => value
    val applyEnd = nowNanos()

    val reportStart = nowNanos()
    val report = MotionReportWriter.render(corrected)
    val reportChecksum = report.motionTsv.length + report.matricesCsv.length + report.summaryCsv.length
    val reportEnd = nowNanos()
    val elapsedEnd = nowNanos()

    val fd = MotionMetrics.framewiseDisplacement(estimate.trace)
    val truthFd = MotionMetrics.framewiseDisplacement(scenario.truth)
    val fdError = meanAbsDiff(fd, truthFd)
    val displacements =
      estimate.trace.poses.zip(scenario.truth.poses).map { case (actual, expected) =>
        MotionMetrics.transformDisplacement(actual, reference = Some(expected))
      }
    val rawTsnr = tsnr(run)
    val correctedTsnr = corrected.corrected.map(tsnr).getOrElse(Double.NaN)
    val approxBytes = run.values.size.toLong * 8L + reportChecksum.toLong

    val result =
      MotionBenchmarkResult(
        scenario = scenario.name,
        status = "ok",
        nFrames = scenario.nVolumes,
        estimateSec = nanosToSec(estimateEnd - estimateStart),
        applySec = nanosToSec(applyEnd - applyStart),
        reportSec = nanosToSec(reportEnd - reportStart),
        elapsedSec = nanosToSec(elapsedEnd - elapsedStart),
        approxBytes = approxBytes,
        tsnrRatio = if rawTsnr > 0.0 then correctedTsnr / rawTsnr else Double.NaN,
        dispP95 = quantile(displacements, 0.95),
        fdError = fdError,
        costFinalMean = meanFinite(estimate.diagnostics.map(_.costFinal))
      )
    if checks(result, thresholds).exists(check => !check.actual.isFinite) then
      Left(MotionBenchmarkError.InvalidScenario(scenario.name, "benchmark produced non-finite summary metrics"))
    else Right(result)

  private def validate(scenario: MotionBenchmarkScenario): Either[MotionBenchmarkError, Unit] =
    if scenario.name.trim.isEmpty then Left(MotionBenchmarkError.InvalidScenario(scenario.name, "name must be non-empty"))
    else if scenario.dims.length != 3 then Left(MotionBenchmarkError.InvalidScenario(scenario.name, "dims must be 3D"))
    else if scenario.dims.exists(_ <= 1) then Left(MotionBenchmarkError.InvalidScenario(scenario.name, "all dims must be greater than one"))
    else if scenario.truth.length < 2 then Left(MotionBenchmarkError.InvalidScenario(scenario.name, "truth trace must contain at least two frames"))
    else if !scenario.noiseScale.isFinite || scenario.noiseScale < 0.0 then
      Left(MotionBenchmarkError.InvalidScenario(scenario.name, "noiseScale must be non-negative and finite"))
    else if !scenario.nuisanceGain.isFinite || scenario.nuisanceGain < 0.0 then
      Left(MotionBenchmarkError.InvalidScenario(scenario.name, "nuisanceGain must be non-negative and finite"))
    else Right(())

  private def scenario(
      name: String,
      dims: Vector[Int],
      translations: Vector[(Double, Double, Double)],
      noiseScale: Double,
      nuisanceGain: Double
  ): MotionBenchmarkScenario =
    val poses = translations.map { case (tx, ty, tz) => RigidPose.unsafe(tx, ty, tz, 0.0, 0.0, 0.0) }
    MotionBenchmarkScenario(name, dims, MotionTrace.unsafe(poses), noiseScale, nuisanceGain)

  private def renderRun(scenario: MotionBenchmarkScenario): SomeScalarSeries[Double] =
    val dims = scenario.dims
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nSpatial = dims.product
    val data = PrimitiveBuffers.tabulate[Double](nSpatial * scenario.nVolumes) { ordinal =>
      val t = ordinal % scenario.nVolumes
      val lin = ordinal / scenario.nVolumes
      val k = lin % nz
      val xy = lin / nz
      val j = xy % ny
      val i = xy / ny
      val pose = scenario.truth.poses(t)
      val base = templateValue(i.toDouble - pose.tx, j.toDouble - pose.ty, k.toDouble - pose.tz, dims)
      val phase = if scenario.nVolumes > 1 then t.toDouble / (scenario.nVolumes - 1).toDouble else 0.0
      val nuisance = 1.0 + scenario.nuisanceGain * math.sin(2.0 * math.Pi * phase)
      val noise = scenario.noiseScale * math.sin(12.9898 * (lin + 1).toDouble + 78.233 * (t + 1).toDouble)
      nuisance * base + noise
    }
    val space = SampleSpaces(dims, spacing = Some(Vector(2.0, 2.0, 2.0))).addDim(timeAxis(scenario.nVolumes))
    SomeNeuroSeries.unsafeCopyFromCanonicalArray[Double, image4s.Continuous](
      data,
      space,
      label = scenario.name
    )

  private def templateValue(x: Double, y: Double, z: Double, dims: Vector[Int]): Double =
    val cx = 0.5 * (dims(0) - 1).toDouble
    val cy = 0.5 * (dims(1) - 1).toDouble
    val cz = 0.5 * (dims(2) - 1).toDouble
    val dx = (x - cx) / math.max(cx, 1.0)
    val dy = (y - cy) / math.max(cy, 1.0)
    val dz = (z - cz) / math.max(cz, 1.0)
    val brain = 90.0 * math.exp(-2.2 * (dx * dx + 1.2 * dy * dy + 1.8 * dz * dz))
    val asym = 24.0 * math.exp(-18.0 * ((dx - 0.28) * (dx - 0.28) + (dy + 0.18) * (dy + 0.18) + dz * dz))
    val texture = 4.0 * math.sin(0.9 * x + 0.35 * y) + 3.0 * math.cos(0.5 * y - 0.7 * z)
    brain + asym + texture

  private def rawCsv(results: Vector[MotionBenchmarkResult]): String =
    val rows = results.map(_.csvRow)
    csv(RawColumns, rows)

  private def checksCsv(results: Vector[MotionBenchmarkResult], thresholds: MotionBenchmarkThresholds): String =
    val rows =
      results.flatMap { result =>
        checks(result, thresholds).map(check => result.scenario +: check.csvRow)
      }
    csv(Vector("scenario", "metric", "actual", "threshold", "direction", "passed"), rows)

  private def summaryMarkdown(results: Vector[MotionBenchmarkResult], thresholds: MotionBenchmarkThresholds): String =
    val rows =
      results.map { result =>
        val passed = checks(result, thresholds).count(_.passed)
        s"| ${result.scenario} | ${result.status} | ${formatDouble(result.elapsedSec)} | ${formatDouble(result.tsnrRatio)} | ${formatDouble(result.dispP95)} | $passed/4 |"
      }
    (Vector(
      "# Motion Benchmark Summary",
      "",
      "| Scenario | Status | Elapsed sec | TSNR ratio | Disp p95 | Checks |",
      "|---|---:|---:|---:|---:|---:|"
    ) ++ rows).mkString("\n") + "\n"

  private def parseCsv(text: String): Either[String, (Vector[String], Vector[Vector[String]])] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    if lines.isEmpty then Left("input is empty")
    else
      val header = lines.head.split(",", -1).toVector.map(_.trim)
      if header.exists(_.isEmpty) then Left("header contains an empty column")
      else
        val rows = lines.tail.map(_.split(",", -1).toVector.map(_.trim))
        rows.find(_.length != header.length) match
          case Some(row) => Left(s"row has ${row.length} cells but header has ${header.length}")
          case None      => Right(header -> rows)

  private def distinctColumn(rows: Vector[Vector[String]], index: Int): Vector[String] =
    if index < 0 then Vector.empty
    else rows.flatMap(row => row.lift(index)).filter(_.nonEmpty).distinct.sorted

  private def csv(columns: Vector[String], rows: Vector[Vector[String]]): String =
    (columns +: rows).map(_.map(escapeCsv).mkString(",")).mkString("", "\n", "\n")

  private def escapeCsv(value: String): String =
    if value.exists(ch => ch == ',' || ch == '"' || ch == '\n' || ch == '\r') then
      "\"" + value.replace("\"", "\"\"") + "\""
    else value

  private def tsnr(run: SomeScalarSeries[Double]): Double =
    val nSpatial = run.space.spatialDims.product
    val nt = run.nVolumes
    val values = Vector.newBuilder[Double]
    var lin = 0
    while lin < nSpatial do
      val voxel = run.space.indexToVoxel3D(lin)
      var sum = 0.0
      var t = 0
      while t < nt do
        sum += run(voxel.x, voxel.y, voxel.z, t)
        t += 1
      val mean = sum / nt.toDouble
      var ss = 0.0
      t = 0
      while t < nt do
        val centered = run(voxel.x, voxel.y, voxel.z, t) - mean
        ss += centered * centered
        t += 1
      val sd = math.sqrt(ss / math.max(nt - 1, 1).toDouble)
      if sd > 0.0 && mean.isFinite then values += math.abs(mean) / sd
      lin += 1
    meanFinite(values.result())

  private def meanAbsDiff(a: Vector[Double], b: Vector[Double]): Double =
    val n = math.min(a.length, b.length)
    if n == 0 then Double.NaN
    else
      var sum = 0.0
      var i = 0
      while i < n do
        sum += math.abs(a(i) - b(i))
        i += 1
      sum / n.toDouble

  private def quantile(values: Vector[Double], p: Double): Double =
    val finite = values.filter(_.isFinite).sorted
    if finite.isEmpty then Double.NaN
    else if finite.length == 1 then finite.head
    else
      val h = (finite.length - 1).toDouble * p
      val lo = math.floor(h).toInt
      val hi = math.ceil(h).toInt
      if lo == hi then finite(lo)
      else
        val w = h - lo.toDouble
        finite(lo) * (1.0 - w) + finite(hi) * w

  private def meanFinite(values: Vector[Double]): Double =
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then Double.NaN else finite.sum / finite.length.toDouble

  private def nanosToSec(nanos: Long): Double =
    nanos.toDouble / 1.0e9

  private[benchmark] def formatDouble(value: Double): String =
    if value.isFinite then java.lang.Double.toString(value) else "NA"
