package scalafim.fmri.motion.io

import bids4s.{BidsFile, JsonValue}
import scalafim.fmri.motion.*
import scalafim.image.{DMat, NeuroVec}

import java.nio.file.Path

enum MotionIoError:
  case Bids(detail: String)
  case Motion(error: MotionError)
  case MissingFile(path: Path)
  case InvalidInput(path: Path, reason: String)
  case InvalidSidecar(path: Path, reason: String)
  case Io(path: Path, reason: String)
  case WriteFailed(path: Path, reason: String)
  case InvalidCommand(reason: String)

  def message: String =
    this match
      case Bids(detail) =>
        s"BIDS error: $detail"
      case Motion(error) =>
        error.message
      case MissingFile(path) =>
        s"missing file: $path"
      case InvalidInput(path, reason) =>
        s"invalid motion input $path: $reason"
      case InvalidSidecar(path, reason) =>
        s"invalid BIDS sidecar $path: $reason"
      case Io(path, reason) =>
        s"I/O failure at $path: $reason"
      case WriteFailed(path, reason) =>
        s"failed to write $path: $reason"
      case InvalidCommand(reason) =>
        s"invalid motion command: $reason"

object MotionIoError:
  def fromMotion(error: MotionError): MotionIoError =
    MotionIoError.Motion(error)

  def fromBids(detail: String): MotionIoError =
    MotionIoError.Bids(detail)

  def fromThrowable(path: Path, throwable: Throwable): MotionIoError =
    MotionIoError.Io(path, Option(throwable.getMessage).getOrElse(throwable.getClass.getSimpleName))

final case class MotionNiftiMetadata(
    path: Path,
    dims: Vector[Int],
    voxelSize: Vector[Double],
    affine: Option[DMat],
    repetitionTime: Option[Double],
    acquisitionTiming: Option[AcquisitionTiming]
)

final case class MotionNiftiRun(run: NeuroVec[Double], metadata: MotionNiftiMetadata)

final case class MotionBidsScan(
    file: BidsFile,
    path: Path,
    repetitionTimeSeconds: Option[Double],
    acquisitionTiming: AcquisitionTiming,
    metadata: JsonValue.Obj
):
  def subject: Option[String] = file.entities.get(bids4s.EntityKey.Subject)
  def session: Option[String] = file.entities.get(bids4s.EntityKey.Session)
  def task: Option[String] = file.entities.get(bids4s.EntityKey.Task)
  def run: Option[String] = file.entities.get(bids4s.EntityKey.Run)
  def space: Option[String] = file.entities.get(bids4s.EntityKey.Space)
  def desc: Option[String] = file.entities.get(bids4s.EntityKey.Description)

  def plan(base: MotionPlan = MotionPlan.default): MotionPlan =
    base.copy(acquisitionTiming = acquisitionTiming)

final case class MotionReportSummary(
    nFrames: Int,
    fdMean: Option[Double],
    dvarsMean: Option[Double],
    costFinalMean: Option[Double],
    packetCorrectionMagnitudeMean: Option[Double],
    packetCorrectionMagnitudeMax: Option[Double]
)

final case class MotionReportContent(
    motionTsv: String,
    matricesCsv: String,
    summaryCsv: String
)

final case class MotionReportBundle(
    motionTsv: Path,
    matricesCsv: Path,
    summaryCsv: Path,
    summary: MotionReportSummary
)

enum MotionCommand:
  case Estimate(input: Path, outputPrefix: Path, plan: MotionPlan)
  case Apply(input: Path, motionTsv: Path, output: Path, control: ApplyControl)
  case Run(input: Path, outputPrefix: Path, plan: MotionPlan, applyControl: ApplyControl)
  case Report(motionTsv: Path, outputDir: Path, prefix: String)

final case class MotionCliResult(command: MotionCommand, outputs: Vector[Path])
