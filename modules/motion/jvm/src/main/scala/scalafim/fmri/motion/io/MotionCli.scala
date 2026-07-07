package scalafim.fmri.motion.io

import scalafim.fmri.motion.*

import java.nio.file.{Path, Paths}

object MotionCli:
  def parse(args: Array[String]): Either[MotionIoError, MotionCommand] =
    parse(args.toVector)

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
