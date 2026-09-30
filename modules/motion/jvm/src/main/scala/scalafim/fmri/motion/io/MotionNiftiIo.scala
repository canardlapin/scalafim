package scalafim.fmri.motion.io

import scalafim.fmri.motion.*
import scalafim.image.*
import scalafim.image.io.Nifti
import scalafim.image.world.SpaceEvidence

import java.nio.file.Path
import scala.util.control.NonFatal

object MotionNiftiIo:
  def readRun(path: Path, evidence: SpaceEvidence): Either[MotionIoError, SomeScalarSeries[Double]] =
    MotionNifti.read(path, evidence).map(_.run)

  def readMask(path: Path, evidence: SpaceEvidence): Either[MotionIoError, SomeMaskVolume] =
    try
      Nifti
        .readVolume(path, evidence)
        .left
        .map(error => MotionIoError.InvalidInput(path, error.message))
        .map(_.image.mapValues[Boolean, image4s.Mask](value => value.isFinite && value > 0.0))
    catch case NonFatal(e) => Left(MotionIoError.fromThrowable(path, e))

  def estimate(
      runPath: Path,
      evidence: SpaceEvidence,
      maskPath: Option[Path] = None,
      maskEvidence: Option[SpaceEvidence] = None,
      plan: MotionPlan = MotionPlan.default
  ): Either[MotionIoError, MotionEstimate] =
    for
      run <- readRun(runPath, evidence)
      mask <- readOptionalMask(maskPath, maskEvidence)
      estimate <- MotionEstimator.estimate(run, mask, plan).left.map(MotionIoError.fromMotion)
    yield estimate

  def estimateAndApply(
      runPath: Path,
      evidence: SpaceEvidence,
      maskPath: Option[Path] = None,
      maskEvidence: Option[SpaceEvidence] = None,
      plan: MotionPlan = MotionPlan.default,
      applyControl: ApplyControl = ApplyControl.linear,
      qcPolicy: MotionQcPolicy = MotionQcPolicy.default
  ): Either[MotionIoError, MotionCorrectionResult] =
    for
      run <- readRun(runPath, evidence)
      mask <- readOptionalMask(maskPath, maskEvidence)
      result <- MotionCorrectionResult
        .estimateAndApply(run, mask, plan, applyControl, qcPolicy)
        .left
        .map(MotionIoError.fromMotion)
    yield result

  private def readOptionalMask(path: Option[Path], evidence: Option[SpaceEvidence]): Either[MotionIoError, Option[SomeMaskVolume]] =
    (path, evidence) match
      case (None, None) => Right(None)
      case (Some(value), Some(identity)) => readMask(value, identity).map(Some(_))
      case (Some(value), None) => Left(MotionIoError.InvalidInput(value, "mask requires its own explicit world-space evidence"))
      case (None, Some(_)) => Left(MotionIoError.InvalidCommand("mask evidence was supplied without a mask path"))
