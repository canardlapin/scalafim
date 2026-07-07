package scalafim.fmri.motion.benchmark

import java.nio.file.Files

class MotionBenchmarkSuite extends munit.FunSuite:

  test("synthetic benchmark harness records required finite metrics") {
    val results =
      MotionBenchmark
        .runSynthetic(MotionBenchmark.smokeScenarios)
        .fold(err => fail(err.message), identity)

    assertEquals(results.length, 1)
    val result = results.head
    assertEquals(result.scenario, "smoke_low_motion")
    assertEquals(result.status, "ok")
    assertEquals(result.nFrames, 3)
    assert(result.estimateSec >= 0.0)
    assert(result.applySec >= 0.0)
    assert(result.reportSec >= 0.0)
    assert(result.elapsedSec >= 0.0)
    assert(result.approxBytes > 0L)
    assert(result.tsnrRatio.isFinite)
    assert(result.dispP95.isFinite)
    assert(result.fdError.isFinite)
    assert(result.costFinalMean.isFinite)

    val checks = MotionBenchmark.checks(result, MotionBenchmarkThresholds.default)
    assertEquals(checks.map(_.metric), Vector("fd_error", "disp_p95", "tsnr_ratio", "elapsed_sec"))
    assert(checks.forall(_.actual.isFinite))
  }

  test("benchmark report writer emits raw, check, and markdown summaries") {
    val dir = Files.createTempDirectory("scalafim-motion-benchmark")
    val results =
      MotionBenchmark
        .runSynthetic(MotionBenchmark.smokeScenarios)
        .fold(err => fail(err.message), identity)
    val files = MotionBenchmark.writeReport(dir, results).fold(err => fail(err.message), identity)

    assert(Files.isRegularFile(files.rawCsv))
    assert(Files.isRegularFile(files.checksCsv))
    assert(Files.isRegularFile(files.summaryMarkdown))
    val raw = Files.readString(files.rawCsv)
    assert(raw.startsWith(MotionBenchmark.RawColumns.mkString(",")))
    assert(raw.contains("estimate_sec"))
    assert(raw.contains("smoke_low_motion"))
    val summary = Files.readString(files.summaryMarkdown)
    assert(summary.contains("Motion Benchmark Summary"))
  }

  test("external benchmark validator requires volregger guardrail columns") {
    val dir = Files.createTempDirectory("scalafim-motion-external-benchmark")
    val path = dir.resolve("external.csv")
    Files.writeString(
      path,
      "scenario,method,estimate_sec,apply_sec,report_sec,elapsed_sec,tsnr_ratio,disp_p95,fd_error\n" +
        "low_motion,volregger,0.1,0.2,0.0,0.3,1.2,0.4,0.05\n"
    )

    val summary = MotionBenchmark.externalSummary(path).fold(err => fail(err.message), identity)
    assert(summary.isUsable)
    assertEquals(summary.nRows, 1)
    assertEquals(summary.scenarios, Vector("low_motion"))
    assertEquals(summary.methods, Vector("volregger"))
    assertEquals(summary.missingColumns, Vector.empty)

    val bad = dir.resolve("bad.csv")
    Files.writeString(bad, "scenario,method,elapsed_sec\nlow_motion,volregger,0.3\n")
    val badSummary = MotionBenchmark.externalSummary(bad).fold(err => fail(err.message), identity)
    assert(!badSummary.isUsable)
    assert(badSummary.missingColumns.contains("estimate_sec"))
    assert(badSummary.missingColumns.contains("fd_error"))
  }

  test("CLI smoke profile writes reproducible benchmark files") {
    val dir = Files.createTempDirectory("scalafim-motion-benchmark-cli")
    val files =
      MotionBenchmarkCli
        .run(Vector("--profile", "smoke", "--out", dir.toString))
        .fold(err => fail(err.message), identity)

    assert(Files.isRegularFile(files.rawCsv))
    assert(Files.readString(files.rawCsv).contains("smoke_low_motion"))
  }
