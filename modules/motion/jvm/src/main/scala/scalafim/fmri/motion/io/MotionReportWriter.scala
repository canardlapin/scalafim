package scalafim.fmri.motion.io

import scalafim.fmri.motion.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

object MotionReportWriter:
  def render(result: MotionCorrectionResult): MotionReportContent =
    MotionReportContent(
      motionTsv = motionTsv(result),
      matricesCsv = matricesCsv(result.estimate.trace),
      summaryCsv = summaryCsv(result)
    )

  def writeBundle(
      directory: Path,
      prefix: String,
      result: MotionCorrectionResult
  ): Either[MotionIoError, MotionReportBundle] =
    val cleanPrefix = prefix.trim
    if cleanPrefix.isEmpty then Left(MotionIoError.InvalidInput(directory, "report prefix must be non-empty"))
    else
      val texts = render(result)
      val motionPath = directory.resolve(s"${cleanPrefix}_motion.tsv")
      val matricesPath = directory.resolve(s"${cleanPrefix}_matrices.csv")
      val summaryPath = directory.resolve(s"${cleanPrefix}_summary.csv")
      try
        Files.createDirectories(directory)
        Files.writeString(motionPath, texts.motionTsv, StandardCharsets.UTF_8)
        Files.writeString(matricesPath, texts.matricesCsv, StandardCharsets.UTF_8)
        Files.writeString(summaryPath, texts.summaryCsv, StandardCharsets.UTF_8)
        Right(MotionReportBundle(motionPath, matricesPath, summaryPath, reportSummary(result)))
      catch case NonFatal(e) => Left(MotionIoError.WriteFailed(directory, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))

  private def motionTsv(result: MotionCorrectionResult): String =
    val trace = result.estimate.trace
    val diagnostics = result.estimate.diagnostics
    val fd = result.qc.map(_.fd).getOrElse(MotionMetrics.framewiseDisplacement(trace))
    val dvars = result.qc.map(_.dvars).getOrElse(Vector.fill(trace.length)(Double.NaN))
    val robustDvars = result.qc.map(_.robustDvars).getOrElse(Vector.fill(trace.length)(Double.NaN))
    val censor = result.qc.map(_.censorSuggest).getOrElse(Vector.fill(trace.length)(false))

    val lines = Vector.newBuilder[String]
    lines += Vector(
      "frame",
      "tx",
      "ty",
      "tz",
      "rx",
      "ry",
      "rz",
      "fd",
      "dvars",
      "robust_dvars",
      "cost_initial",
      "cost_final",
      "iterations",
      "overlap",
      "converged",
      "restarted",
      "censor_suggest"
    ).mkString("\t")

    var t = 0
    while t < trace.length do
      val pose = trace.unsafeFrame(t)
      val d = diagnostics(t)
      lines += Vector(
        t.toString,
        number(pose.tx),
        number(pose.ty),
        number(pose.tz),
        number(pose.rx),
        number(pose.ry),
        number(pose.rz),
        number(fd(t)),
        number(dvars(t)),
        number(robustDvars(t)),
        number(d.costInitial),
        number(d.costFinal),
        d.iterations.toString,
        number(d.overlap),
        d.converged.toString,
        d.restarted.toString,
        censor(t).toString
      ).mkString("\t")
      t += 1

    lines.result().mkString("", "\n", "\n")

  private def matricesCsv(trace: MotionTrace): String =
    val lines = Vector.newBuilder[String]
    lines += "frame,row,c0,c1,c2,c3"
    var t = 0
    while t < trace.length do
      val matrix = trace.unsafeFrame(t).toMatrix
      var row = 0
      while row < 4 do
        lines += Vector(
          t.toString,
          row.toString,
          number(matrix(row, 0)),
          number(matrix(row, 1)),
          number(matrix(row, 2)),
          number(matrix(row, 3))
        ).mkString(",")
        row += 1
      t += 1
    lines.result().mkString("", "\n", "\n")

  private def summaryCsv(result: MotionCorrectionResult): String =
    val summary = reportSummary(result)
    val rows = Vector(
      "metric,value",
      s"n_volumes,${summary.nFrames}",
      s"mean_fd,${optionNumber(summary.fdMean)}",
      s"mean_dvars,${optionNumber(summary.dvarsMean)}",
      s"mean_final_cost,${optionNumber(summary.costFinalMean)}",
      s"mean_packet_correction,${optionNumber(summary.packetCorrectionMagnitudeMean)}",
      s"max_packet_correction,${optionNumber(summary.packetCorrectionMagnitudeMax)}",
      s"has_corrected_run,${result.hasCorrectedRun}"
    )
    rows.mkString("", "\n", "\n")

  private def reportSummary(result: MotionCorrectionResult): MotionReportSummary =
    val trace = result.estimate.trace
    val fd = result.qc.map(_.fd).getOrElse(MotionMetrics.framewiseDisplacement(trace))
    val dvars = result.qc.map(_.dvars).getOrElse(Vector.empty)
    val diagnostics = result.estimate.diagnostics
    val finalCosts = diagnostics.map(_.costFinal).filter(_.isFinite)
    val packet = result.qc.flatMap(_.packetCorrectionMagnitude).getOrElse(Vector.empty)

    MotionReportSummary(
      nFrames = trace.length,
      fdMean = meanFiniteOption(fd),
      dvarsMean = meanFiniteOption(dvars),
      costFinalMean = meanFiniteOption(finalCosts),
      packetCorrectionMagnitudeMean = meanFiniteOption(packet),
      packetCorrectionMagnitudeMax = maxFiniteOption(packet)
    )

  private def number(value: Double): String =
    if value.isFinite then java.lang.Double.toString(value) else "NA"

  private def optionNumber(value: Option[Double]): String =
    value.map(number).getOrElse("NA")

  private def meanFiniteOption(values: Vector[Double]): Option[Double] =
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then None else Some(finite.sum / finite.length.toDouble)

  private def maxFiniteOption(values: Vector[Double]): Option[Double] =
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then None else Some(finite.max)
