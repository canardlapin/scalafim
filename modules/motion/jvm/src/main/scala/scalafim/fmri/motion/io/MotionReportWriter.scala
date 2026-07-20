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

  def readEstimate(
      path: Path,
      control: MotionControl = MotionControl.default
  ): Either[MotionIoError, MotionEstimate] =
    if !Files.isRegularFile(path) then Left(MotionIoError.MissingFile(path))
    else
      try parseEstimate(Files.readString(path, StandardCharsets.UTF_8), path, control)
      catch case NonFatal(e) => Left(MotionIoError.fromThrowable(path, e))

  def parseEstimate(
      text: String,
      source: Path,
      control: MotionControl = MotionControl.default
  ): Either[MotionIoError, MotionEstimate] =
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toVector
    if lines.isEmpty then Left(MotionIoError.InvalidInput(source, "motion TSV is empty"))
    else
      val header = lines.head.split("\t", -1).toVector
      val index = header.zipWithIndex.toMap
      val required =
        Vector(
          "frame",
          "tx",
          "ty",
          "tz",
          "rx",
          "ry",
          "rz",
          "cost_initial",
          "cost_final",
          "iterations",
          "overlap",
          "converged",
          "restarted"
        )
      val missing = required.filterNot(index.contains)
      if missing.nonEmpty then Left(MotionIoError.InvalidInput(source, s"motion TSV missing column '${missing.head}'"))
      else parseRows(lines.tail, index, source, control)

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

  private def parseRows(
      rows: Vector[String],
      index: Map[String, Int],
      source: Path,
      control: MotionControl
  ): Either[MotionIoError, MotionEstimate] =
    if rows.isEmpty then Left(MotionIoError.InvalidInput(source, "motion TSV has no frame rows"))
    else
      val poses = Array.ofDim[RigidPose](rows.length)
      val diagnostics = Array.ofDim[FrameFitDiagnostics](rows.length)
      var rowIndex = 0
      while rowIndex < rows.length do
        val cells = rows(rowIndex).split("\t", -1).toVector
        val parsed: Either[MotionIoError, Unit] =
          for
            frame <- intCell(cells, index, "frame", source, rowIndex)
            _ <-
              if frame == rowIndex then Right(())
              else Left(MotionIoError.InvalidInput(source, s"frame rows must be ordered 0..n-1; found $frame at row $rowIndex"))
            tx <- doubleCell(cells, index, "tx", source, rowIndex)
            ty <- doubleCell(cells, index, "ty", source, rowIndex)
            tz <- doubleCell(cells, index, "tz", source, rowIndex)
            rx <- doubleCell(cells, index, "rx", source, rowIndex)
            ry <- doubleCell(cells, index, "ry", source, rowIndex)
            rz <- doubleCell(cells, index, "rz", source, rowIndex)
            pose <- RigidPose.make(tx, ty, tz, rx, ry, rz).left.map(MotionIoError.fromMotion)
            costInitial <- doubleCell(cells, index, "cost_initial", source, rowIndex)
            costFinal <- doubleCell(cells, index, "cost_final", source, rowIndex)
            iterations <- intCell(cells, index, "iterations", source, rowIndex)
            overlap <- doubleCell(cells, index, "overlap", source, rowIndex)
            converged <- booleanCell(cells, index, "converged", source, rowIndex)
            restarted <- booleanCell(cells, index, "restarted", source, rowIndex)
          yield
            poses(rowIndex) = pose
            diagnostics(rowIndex) =
              FrameFitDiagnostics(
                costInitial = costInitial,
                costFinal = costFinal,
                iterations = iterations,
                overlap = overlap,
                restarted = restarted,
                converged = converged
              )
        parsed match
          case Left(error) => return Left(error)
          case Right(_) => ()
        rowIndex += 1

      MotionTrace
        .make(poses.toVector)
        .left
        .map(MotionIoError.fromMotion)
        .map(trace => MotionEstimate(trace, diagnostics.toVector, control))

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

  private def cell(
      cells: Vector[String],
      index: Map[String, Int],
      column: String,
      source: Path,
      row: Int
  ): Either[MotionIoError, String] =
    val i = index(column)
    if i >= cells.length then Left(MotionIoError.InvalidInput(source, s"row $row is missing column '$column'"))
    else Right(cells(i).trim)

  private def doubleCell(
      cells: Vector[String],
      index: Map[String, Int],
      column: String,
      source: Path,
      row: Int
  ): Either[MotionIoError, Double] =
    cell(cells, index, column, source, row).flatMap { raw =>
      raw.toDoubleOption.filter(_.isFinite) match
        case Some(value) => Right(value)
        case None => Left(MotionIoError.InvalidInput(source, s"row $row column '$column' must be a finite number"))
    }

  private def intCell(
      cells: Vector[String],
      index: Map[String, Int],
      column: String,
      source: Path,
      row: Int
  ): Either[MotionIoError, Int] =
    cell(cells, index, column, source, row).flatMap { raw =>
      raw.toIntOption match
        case Some(value) if value >= 0 => Right(value)
        case _ => Left(MotionIoError.InvalidInput(source, s"row $row column '$column' must be a non-negative integer"))
    }

  private def booleanCell(
      cells: Vector[String],
      index: Map[String, Int],
      column: String,
      source: Path,
      row: Int
  ): Either[MotionIoError, Boolean] =
    cell(cells, index, column, source, row).flatMap {
      case "true" => Right(true)
      case "false" => Right(false)
      case _ => Left(MotionIoError.InvalidInput(source, s"row $row column '$column' must be true or false"))
    }

  private def optionNumber(value: Option[Double]): String =
    value.map(number).getOrElse("NA")

  private def meanFiniteOption(values: Vector[Double]): Option[Double] =
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then None else Some(finite.sum / finite.length.toDouble)

  private def maxFiniteOption(values: Vector[Double]): Option[Double] =
    val finite = values.filter(_.isFinite)
    if finite.isEmpty then None else Some(finite.max)
