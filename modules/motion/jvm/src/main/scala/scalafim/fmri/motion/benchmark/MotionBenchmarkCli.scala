package scalafim.fmri.motion.benchmark

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object MotionBenchmarkCli:
  def main(args: Array[String]): Unit =
    run(args.toVector) match
      case Right(files) =>
        println(s"motion benchmark raw: ${files.rawCsv}")
        println(s"motion benchmark checks: ${files.checksCsv}")
        println(s"motion benchmark summary: ${files.summaryMarkdown}")
      case Left(error) =>
        Console.err.println(error.message)
        sys.exit(2)

  def run(args: Vector[String]): Either[MotionBenchmarkError, MotionBenchmarkFiles] =
    for
      config <- parse(args)
      results <-
        if config.synthetic then
          MotionBenchmark.runSynthetic(config.scenarios, thresholds = config.thresholds)
        else Right(Vector.empty)
      files <- MotionBenchmark.writeReport(config.outputDir, results, config.thresholds)
      _ <- config.externalCsv match
        case None => Right(())
        case Some(path) => writeExternalSummary(config.outputDir, path)
    yield files

  private final case class Config(
      outputDir: Path,
      scenarios: Vector[MotionBenchmarkScenario],
      thresholds: MotionBenchmarkThresholds,
      synthetic: Boolean,
      externalCsv: Option[Path]
  )

  private def parse(args: Vector[String]): Either[MotionBenchmarkError, Config] =
    parseFlags(args).flatMap { flags =>
      val out = flags.get("out").map(Paths.get(_)).getOrElse(Paths.get("modules/motion/jvm/target/motion-benchmark"))
      val profile = flags.getOrElse("profile", "default")
      val scenarios =
        profile match
          case "smoke"   => Right(MotionBenchmark.smokeScenarios)
          case "default" => Right(MotionBenchmark.defaultScenarios)
          case other     => Left(MotionBenchmarkError.InvalidScenario(other, "profile must be 'smoke' or 'default'"))
      scenarios.map { selected =>
        Config(
          outputDir = out,
          scenarios = selected,
          thresholds = MotionBenchmarkThresholds.default,
          synthetic = !flags.contains("external-only"),
          externalCsv = flags.get("external-csv").map(Paths.get(_))
        )
      }
    }

  private def writeExternalSummary(outputDir: Path, path: Path): Either[MotionBenchmarkError, Unit] =
    MotionBenchmark.externalSummary(path).flatMap { summary =>
      val out = outputDir.resolve("motion_external_summary.csv")
      val text =
        Vector(
          "path,n_rows,scenarios,methods,missing_columns,usable",
          Vector(
            summary.path.toString,
            summary.nRows.toString,
            summary.scenarios.mkString(";"),
            summary.methods.mkString(";"),
            summary.missingColumns.mkString(";"),
            summary.isUsable.toString
          ).map(escapeCsv).mkString(",")
        ).mkString("\n") + "\n"
      try
        Files.createDirectories(outputDir)
        Files.writeString(out, text, StandardCharsets.UTF_8)
        Right(())
      catch
        case e: Exception =>
          Left(MotionBenchmarkError.Io(out, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))
    }

  private def parseFlags(tokens: Vector[String]): Either[MotionBenchmarkError, Map[String, String]] =
    var i = 0
    var acc = Map.empty[String, String]
    while i < tokens.length do
      val flag = tokens(i)
      if !flag.startsWith("--") then
        return Left(MotionBenchmarkError.InvalidScenario("cli", s"unexpected positional argument '$flag'"))
      val key = flag.drop(2)
      if key == "external-only" then
        acc = acc.updated(key, "true")
        i += 1
      else
        if i + 1 >= tokens.length || tokens(i + 1).startsWith("--") then
          return Left(MotionBenchmarkError.InvalidScenario("cli", s"flag '$flag' requires a value"))
        if acc.contains(key) then
          return Left(MotionBenchmarkError.InvalidScenario("cli", s"duplicate flag '$flag'"))
        acc = acc.updated(key, tokens(i + 1))
        i += 2
    Right(acc)

  private def escapeCsv(value: String): String =
    if value.exists(ch => ch == ',' || ch == '"' || ch == '\n' || ch == '\r') then
      "\"" + value.replace("\"", "\"\"") + "\""
    else value
