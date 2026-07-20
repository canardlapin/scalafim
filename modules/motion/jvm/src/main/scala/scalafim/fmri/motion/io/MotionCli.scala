package scalafim.fmri.motion.io

import scalafim.fmri.motion.*

import java.nio.file.{Path, Paths}

object MotionCli:
  def parse(args: Array[String]): Either[MotionIoError, MotionCommand] =
    parse(args.toVector)

  def run(args: Array[String]): Either[MotionIoError, MotionCliResult] =
    run(args.toVector)

  def run(args: Vector[String]): Either[MotionIoError, MotionCliResult] =
    parse(args).flatMap(run)

  def run(command: MotionCommand): Either[MotionIoError, MotionCliResult] =
    command match
      case MotionCommand.Estimate(input, outputPrefix, plan) =>
        runEstimate(command, input, outputPrefix, plan)
      case MotionCommand.Apply(input, motionTsv, output, control) =>
        runApply(command, input, motionTsv, output, control)
      case MotionCommand.Run(input, outputPrefix, plan, applyControl) =>
        runEstimateAndApply(command, input, outputPrefix, plan, applyControl)
      case MotionCommand.Report(motionTsv, outputDir, prefix) =>
        runReport(command, motionTsv, outputDir, prefix)

  def main(args: Array[String]): Unit =
    run(args) match
      case Left(error) =>
        System.err.println(error.message)
        sys.exit(2)
      case Right(result) =>
        result.outputs.foreach(path => println(path.toString))

  def parse(args: Vector[String]): Either[MotionIoError, MotionCommand] =
    args.headOption match
      case None =>
        Left(MotionIoError.InvalidCommand("expected one of: estimate, apply, run, report"))
      case Some(command) =>
        parseFlags(args.drop(1)).flatMap { flags =>
          command match
            case "estimate" => estimate(flags)
            case "apply"    => apply(flags)
            case "run"      => run(flags)
            case "report"   => report(flags)
            case other      => Left(MotionIoError.InvalidCommand(s"unknown command '$other'"))
        }

  private def estimate(flags: Map[String, String]): Either[MotionIoError, MotionCommand] =
    for
      input <- requiredPath(flags, "input")
      outputPrefix <- requiredPath(flags, "output-prefix")
      _ <- rejectUnknown(flags, Set("input", "output-prefix"))
    yield MotionCommand.Estimate(input, outputPrefix, MotionPlan.default)

  private def apply(flags: Map[String, String]): Either[MotionIoError, MotionCommand] =
    for
      input <- requiredPath(flags, "input")
      motionTsv <- requiredPath(flags, "motion")
      output <- requiredPath(flags, "output")
      _ <- rejectUnknown(flags, Set("input", "motion", "output"))
    yield MotionCommand.Apply(input, motionTsv, output, ApplyControl.linear)

  private def run(flags: Map[String, String]): Either[MotionIoError, MotionCommand] =
    for
      input <- requiredPath(flags, "input")
      outputPrefix <- requiredPath(flags, "output-prefix")
      _ <- rejectUnknown(flags, Set("input", "output-prefix"))
    yield MotionCommand.Run(input, outputPrefix, MotionPlan.default, ApplyControl.linear)

  private def report(flags: Map[String, String]): Either[MotionIoError, MotionCommand] =
    for
      motionTsv <- requiredPath(flags, "motion")
      outputDir <- requiredPath(flags, "output-dir")
      prefix <- required(flags, "prefix")
      _ <- rejectUnknown(flags, Set("motion", "output-dir", "prefix"))
    yield MotionCommand.Report(motionTsv, outputDir, prefix)

  private def runEstimate(
      command: MotionCommand,
      input: Path,
      outputPrefix: Path,
      plan: MotionPlan
  ): Either[MotionIoError, MotionCliResult] =
    for
      run <- MotionNifti.read(input)
      estimate <- MotionEstimator.estimate(run.run, None, plan).left.map(MotionIoError.fromMotion)
      bundle <- writeBundle(outputPrefix, MotionCorrectionResult.estimateOnly(estimate, plan))
    yield MotionCliResult(command, Vector(bundle.motionTsv, bundle.matricesCsv, bundle.summaryCsv))

  private def runApply(
      command: MotionCommand,
      input: Path,
      motionTsv: Path,
      output: Path,
      control: ApplyControl
  ): Either[MotionIoError, MotionCliResult] =
    for
      run <- MotionNifti.read(input)
      estimate <- MotionReportWriter.readEstimate(motionTsv)
      corrected <- MotionApplier.apply(run.run, estimate.trace, control).left.map(MotionIoError.fromMotion)
      out <- MotionNifti.write(output, corrected, Some(run.metadata.copy(path = output)))
    yield MotionCliResult(command, Vector(out, MotionNifti.defaultSidecar(out)))

  private def runEstimateAndApply(
      command: MotionCommand,
      input: Path,
      outputPrefix: Path,
      plan: MotionPlan,
      applyControl: ApplyControl
  ): Either[MotionIoError, MotionCliResult] =
    val (directory, prefix) = splitOutputPrefix(outputPrefix)
    val correctedPath = directory.resolve(s"${prefix}_corrected.nii")
    for
      run <- MotionNifti.read(input)
      estimate <- MotionEstimator.estimate(run.run, None, plan).left.map(MotionIoError.fromMotion)
      corrected <- MotionCorrectionResult
        .fromEstimate(run.run, estimate, plan, applyControl = applyControl)
        .left
        .map(MotionIoError.fromMotion)
      out <- corrected.corrected match
        case None => Left(MotionIoError.InvalidCommand("run command did not produce a corrected image"))
        case Some(image) => MotionNifti.write(correctedPath, image, Some(run.metadata.copy(path = correctedPath)))
      bundle <- MotionReportWriter.writeBundle(directory, prefix, corrected)
    yield MotionCliResult(command, Vector(out, MotionNifti.defaultSidecar(out), bundle.motionTsv, bundle.matricesCsv, bundle.summaryCsv))

  private def runReport(
      command: MotionCommand,
      motionTsv: Path,
      outputDir: Path,
      prefix: String
  ): Either[MotionIoError, MotionCliResult] =
    for
      estimate <- MotionReportWriter.readEstimate(motionTsv)
      bundle <- MotionReportWriter.writeBundle(outputDir, prefix, MotionCorrectionResult.estimateOnly(estimate, MotionPlan.default))
    yield MotionCliResult(command, Vector(bundle.motionTsv, bundle.matricesCsv, bundle.summaryCsv))

  private def writeBundle(
      outputPrefix: Path,
      result: MotionCorrectionResult
  ): Either[MotionIoError, MotionReportBundle] =
    val (directory, prefix) = splitOutputPrefix(outputPrefix)
    MotionReportWriter.writeBundle(directory, prefix, result)

  private def splitOutputPrefix(path: Path): (Path, String) =
    val parent = Option(path.getParent).getOrElse(Paths.get("."))
    val prefix = Option(path.getFileName).map(_.toString).getOrElse(path.toString)
    (parent, prefix)

  private def parseFlags(tokens: Vector[String]): Either[MotionIoError, Map[String, String]] =
    var i = 0
    var acc = Map.empty[String, String]
    while i < tokens.length do
      val flag = tokens(i)
      if !flag.startsWith("--") then
        return Left(MotionIoError.InvalidCommand(s"unexpected positional argument '$flag'"))
      if i + 1 >= tokens.length then
        return Left(MotionIoError.InvalidCommand(s"flag '$flag' requires a value"))
      val value = tokens(i + 1)
      if value.startsWith("--") then
        return Left(MotionIoError.InvalidCommand(s"flag '$flag' requires a value"))
      val key = flag.drop(2)
      if key.isEmpty then
        return Left(MotionIoError.InvalidCommand("empty flag name"))
      if acc.contains(key) then
        return Left(MotionIoError.InvalidCommand(s"duplicate flag '--$key'"))
      acc = acc.updated(key, value)
      i += 2
    Right(acc)

  private def required(flags: Map[String, String], key: String): Either[MotionIoError, String] =
    flags.get(key).map(_.trim).filter(_.nonEmpty) match
      case Some(value) => Right(value)
      case None        => Left(MotionIoError.InvalidCommand(s"missing required flag '--$key'"))

  private def requiredPath(flags: Map[String, String], key: String): Either[MotionIoError, Path] =
    required(flags, key).map(Paths.get(_))

  private def rejectUnknown(flags: Map[String, String], allowed: Set[String]): Either[MotionIoError, Unit] =
    val unknown = flags.keySet.diff(allowed).toVector.sorted
    if unknown.isEmpty then Right(())
    else Left(MotionIoError.InvalidCommand(s"unknown flag '--${unknown.head}'"))
