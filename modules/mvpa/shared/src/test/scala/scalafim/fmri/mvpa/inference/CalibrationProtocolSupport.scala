package scalafim.fmri.mvpa.inference

import gale.linalg.DMat
import resample4s.kernel.{Seed, StreamDomain, StreamPath}
import resample4s.kernel.derive
import scala.util.control.NonFatal

/** Test-only stream/input contract. It never promotes a numeric result to release. */
object CalibrationProtocolSupport:
  val namespace: String = "scalafim/umvpa/inference-calibration/v1"
  val phases: Set[String] = Set("fixture", "simulator", "pilot", "confirmation")
  val nullDatasets: Int = 10000
  val alternativeDatasets: Int = 5000
  val confirmationDraws: Int = 1999

  /** Internal identifier syntax must not normalize scientific scenario names. */
  def bindingIdentity(scenario: String, ordinal: Int, rootSeed: Long): Either[String, String] =
    if scenario.isEmpty || scenario.contains('\u0000') || ordinal < 0 then Left("invalid case binding identity")
    else Right("case-" + CalibrationPlatform.sha256Utf8(
      "scalafim/calibration-binding/v1\u0000" + scenario + "\u0000" + ordinal.toString + "\u0000" + rootSeed.toString))

  /** An exception evaluating an assigned case is a retained failed dataset. */
  def retainEvaluation(prefix: => String)(evaluate: => Either[String, String]): String =
    val result: Either[String, String] = try evaluate
      catch case NonFatal(error) => Left(error.getClass.getName + ": " + Option(error.getMessage).getOrElse(""))
    result match
      case Right(record) => record
      case Left(reason) =>
        val escaped = reason.flatMap:
          case '\\' => "\\\\"
          case '"' => "\\\""
          case character if character < ' ' => f"\\u${character.toInt}%04x"
          case character => character.toString
        prefix + ",\"status\":\"failed\",\"reason\":\"" + escaped + "\",\"p_values\":null,\"reject\":null}\n"

  def seed(phase: String, scenario: String, ordinal: Int): Either[String, Long] =
    if !phases.contains(phase) || scenario.isEmpty || scenario.contains('\u0000') || ordinal < 0 then
      Left("invalid protocol seed assignment")
    else
      val text = namespace + "\u0000" + phase + "\u0000" + scenario + "\u0000" + ordinal.toString
      val digest = CalibrationPlatform.sha256Utf8(text)
      val masked = (BigInt(digest.take(16), 16) & ((BigInt(1) << 63) - 1)).toLong
      Right(if masked == 0L then 1L else masked)

  def child(root: Long, tag: Int, ordinal: Int): Either[String, Long] =
    for
      domain <- StreamDomain.custom(tag).left.map(_.toString)
      path <- StreamPath.of(domain, ordinal).left.map(_.toString)
    yield Seed.fromLong(root).derive(path).value

  final case class Case(
      phase: String, scenario: String, ordinal: Int, rootSeed: Long, draws: Int,
      x: DMat, y: DMat, nuisance: DMat, populationCorrelations: Vector[Double]
  )

  /** Independent generated TSV input; raw values are shared with the external oracle. */
  def readCase(text: String): Either[String, Case] =
    val lines = text.linesIterator.filter(_.nonEmpty).toVector
    val records = lines.map(_.split("\t", -1).toVector)
    def field(name: String): Either[String, Vector[String]] =
      val values = records.filter(_.headOption.contains(name))
      if values.size == 1 then Right(values.head.tail) else Left("missing/duplicate field " + name)
    def matrix(name: String): Either[String, DMat] =
      field(name).flatMap: value =>
        try
          if value.size != 3 then Left("invalid matrix record " + name)
          else
            val rows = value(0).toInt; val cols = value(1).toInt
            val data = value(2).split(",", -1).toVector.map(_.toDouble)
            if rows <= 0 || cols <= 0 || BigInt(rows) * cols != data.size || data.exists(x => !x.isFinite) then
              Left("invalid finite matrix " + name)
            else Right(DMat.dense(rows, cols, data))
        catch case error: NumberFormatException => Left(error.toString)
    for
      header <- field("case")
      _ <- if header.size == 6 then Right(()) else Left("case header shape")
      _ <- if Set("fixture", "pilot").contains(header(0)) then Right(())
        else Left("initial statistic adapter refuses simulator/confirmation; no environment flag establishes qualification")
      x <- matrix("X")
      y <- matrix("Y")
      z <- matrix("Z")
      truth <- field("rho")
      out <-
        try
          val phase = header(0); val scenario = header(1); val ordinal = header(2).toInt
          val root = header(3).toLong; val draws = header(4).toInt; val declaredRows = header(5).toInt
          val correlations = truth.headOption.toVector.flatMap(_.split(",").map(_.toDouble))
          seed(phase, scenario, ordinal).flatMap: expected =>
            if root != expected || x.rows != y.rows || z.rows != x.rows || declaredRows != x.rows then Left("input assignment/rows mismatch")
            else if phase == "confirmation" && draws != confirmationDraws then Left("confirmation B must be 1999")
            else if phase == "pilot" && draws != 199 then Left("pilot B must be 199")
            else if draws <= 0 || correlations.size != math.min(x.cols, y.cols) || correlations.exists(v => !v.isFinite || v < 0.0 || v >= 1.0) then
              Left("invalid rank truth/reference count")
            else Right(Case(phase, scenario, ordinal, root, draws, x, y, z, correlations))
        catch case error: NumberFormatException => Left(error.toString)
    yield out
