package scalafim.fmri.motion.io

import scalafim.fmri.motion.*
import scalafim.image.*
import scalafim.image.io.Nifti

import java.nio.file.Path
import scala.util.control.NonFatal

object MotionNiftiIo:
  def readRun(path: Path): Either[MotionIoError, SomeScalarSeries[Double]] =
    MotionNifti.read(path).map(_.run)

  def readMask(path: Path): Either[MotionIoError, SomeMaskVolume] =
    try
      Nifti
        .readVolume(path)
        .left
        .map(error => MotionIoError.InvalidInput(path, error.message))
        .map(_.image.mapValues[Boolean, image4s.Mask](value => value.isFinite && value > 0.0))
    catch case NonFatal(e) => Left(MotionIoError.fromThrowable(path, e))

  def estimate(
      runPath: Path,
      maskPath: Option[Path] = None,
      plan: MotionPlan = MotionPlan.default
  ): Either[MotionIoError, MotionEstimate] =
    for
      run <- readRun(runPath)
      mask <- readOptionalMask(maskPath)
      estimate <- MotionEstimator.estimate(run, mask, plan).left.map(MotionIoError.fromMotion)
    yield estimate

  def estimateAndApply(
      runPath: Path,
      maskPath: Option[Path] = None,
      plan: MotionPlan = MotionPlan.default,
      applyControl: ApplyControl = ApplyControl.linear,
      qcPolicy: MotionQcPolicy = MotionQcPolicy.default
  ): Either[MotionIoError, MotionCorrectionResult] =
    for
      run <- readRun(runPath)
      mask <- readOptionalMask(maskPath)
      result <- MotionCorrectionResult
        .estimateAndApply(run, mask, plan, applyControl, qcPolicy)
        .left
        .map(MotionIoError.fromMotion)
    yield result

  private def readOptionalMask(path: Option[Path]): Either[MotionIoError, Option[SomeMaskVolume]] =
    path match
      case None => Right(None)
      case Some(value) => readMask(value).map(Some(_))
