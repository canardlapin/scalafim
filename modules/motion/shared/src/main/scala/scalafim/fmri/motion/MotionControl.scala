package scalafim.fmri.motion

final case class PyramidControl private (
    downsample: Vector[Int],
    maxIterations: Vector[Int],
    sampleCounts: Vector[Int],
    enabled: Boolean
):
  require(downsample.nonEmpty && downsample.forall(_ >= 1), "downsample must contain positive integers")
  require(maxIterations.nonEmpty && maxIterations.forall(_ >= 0), "maxIterations must contain non-negative integers")
  require(sampleCounts.nonEmpty && sampleCounts.forall(_ >= 0), "sampleCounts must contain non-negative integers")

object PyramidControl:
  val default: PyramidControl =
    unsafe(Vector(4, 2, 1), Vector(30, 20, 12), Vector(12000, 24000, 40000), enabled = false)

  def make(
      downsample: Vector[Int],
      maxIterations: Vector[Int],
      sampleCounts: Vector[Int],
      enabled: Boolean
  ): Either[MotionError, PyramidControl] =
    if downsample.isEmpty || downsample.exists(_ < 1) then
      Left(MotionError.InvalidInt("downsample", downsample.find(_ < 1).getOrElse(0), "all entries must be >= 1"))
    else if maxIterations.isEmpty || maxIterations.exists(_ < 0) then
      Left(MotionError.InvalidInt("maxIterations", maxIterations.find(_ < 0).getOrElse(-1), "all entries must be >= 0"))
    else if sampleCounts.isEmpty || sampleCounts.exists(_ < 0) then
      Left(MotionError.InvalidInt("sampleCounts", sampleCounts.find(_ < 0).getOrElse(-1), "all entries must be >= 0"))
    else if enabled && maxIterations.length > 1 && maxIterations.length != downsample.length then
      Left(MotionError.InvalidInt("maxIterations.length", maxIterations.length, "must match downsample length when pyramid is enabled"))
    else if enabled && sampleCounts.length > 1 && sampleCounts.length != downsample.length then
      Left(MotionError.InvalidInt("sampleCounts.length", sampleCounts.length, "must match downsample length when pyramid is enabled"))
    else Right(unsafe(downsample, maxIterations, sampleCounts, enabled))

  def unsafe(
      downsample: Vector[Int],
      maxIterations: Vector[Int],
      sampleCounts: Vector[Int],
      enabled: Boolean
  ): PyramidControl =
    new PyramidControl(downsample, maxIterations, sampleCounts, enabled)

final case class OptimizerControl private (
    huberK: Double,
    lambda0: Double,
    stepTolerance: Double,
    costTolerance: Double
):
  require(huberK.isFinite && huberK > 0.0, "huberK must be positive")
  require(lambda0.isFinite && lambda0 >= 0.0, "lambda0 must be non-negative")
  require(stepTolerance.isFinite && stepTolerance >= 0.0, "stepTolerance must be non-negative")
  require(costTolerance.isFinite && costTolerance >= 0.0, "costTolerance must be non-negative")

object OptimizerControl:
  val default: OptimizerControl =
    unsafe(huberK = 1.5, lambda0 = 1e-2, stepTolerance = 1e-5, costTolerance = 1e-6)

  def make(
      huberK: Double,
      lambda0: Double,
      stepTolerance: Double,
      costTolerance: Double
  ): Either[MotionError, OptimizerControl] =
    if !huberK.isFinite || huberK <= 0.0 then Left(MotionError.InvalidScalar("huberK", huberK, "must be positive"))
    else if !lambda0.isFinite || lambda0 < 0.0 then Left(MotionError.InvalidScalar("lambda0", lambda0, "must be non-negative"))
    else if !stepTolerance.isFinite || stepTolerance < 0.0 then Left(MotionError.InvalidScalar("stepTolerance", stepTolerance, "must be non-negative"))
    else if !costTolerance.isFinite || costTolerance < 0.0 then Left(MotionError.InvalidScalar("costTolerance", costTolerance, "must be non-negative"))
    else Right(unsafe(huberK, lambda0, stepTolerance, costTolerance))

  def unsafe(huberK: Double, lambda0: Double, stepTolerance: Double, costTolerance: Double): OptimizerControl =
    new OptimizerControl(huberK, lambda0, stepTolerance, costTolerance)

final case class TemplateControl(
    robustTemplate: Boolean,
    refreshValidOnly: Boolean,
    edgeExcludeFraction: Double
):
  require(edgeExcludeFraction.isFinite && edgeExcludeFraction >= 0.0 && edgeExcludeFraction < 1.0,
    "edgeExcludeFraction must be in [0, 1)")

object TemplateControl:
  val default: TemplateControl =
    TemplateControl(robustTemplate = true, refreshValidOnly = true, edgeExcludeFraction = 0.05)

final case class CaptureControl(
    enabled: Boolean,
    translationHalfWidthMm: Double,
    rotationHalfWidthDeg: Double,
    topK: Int
):
  require(translationHalfWidthMm.isFinite && translationHalfWidthMm > 0.0, "translationHalfWidthMm must be positive")
  require(rotationHalfWidthDeg.isFinite && rotationHalfWidthDeg > 0.0, "rotationHalfWidthDeg must be positive")
  require(topK >= 1, "topK must be positive")

object CaptureControl:
  val default: CaptureControl =
    CaptureControl(enabled = true, translationHalfWidthMm = 8.0, rotationHalfWidthDeg = 8.0, topK = 4)

final case class TemporalControl(
    regularizationEnabled: Boolean,
    lowMotionPoseShrink: Boolean,
    lowMotionPoseScale: Double,
    lowMotionThresholdMm: Double = 0.25
):
  require(lowMotionPoseScale.isFinite && lowMotionPoseScale > 0.0 && lowMotionPoseScale <= 1.0,
    "lowMotionPoseScale must be in (0, 1]")
  require(lowMotionThresholdMm.isFinite && lowMotionThresholdMm >= 0.0,
    "lowMotionThresholdMm must be non-negative")

object TemporalControl:
  val default: TemporalControl =
    TemporalControl(
      regularizationEnabled = false,
      lowMotionPoseShrink = false,
      lowMotionPoseScale = 0.95,
      lowMotionThresholdMm = 0.25
    )

final case class ExecutionControl(
    parallelFrames: Boolean,
    nThreads: Int
):
  require(nThreads >= 1, "nThreads must be positive")

object ExecutionControl:
  val default: ExecutionControl =
    ExecutionControl(parallelFrames = false, nThreads = 1)

final case class MotionControl(
    pyramid: PyramidControl,
    optimizer: OptimizerControl,
    template: TemplateControl,
    capture: CaptureControl,
    temporal: TemporalControl,
    execution: ExecutionControl
)

object MotionControl:
  val default: MotionControl =
    MotionControl(
      pyramid = PyramidControl.default,
      optimizer = OptimizerControl.default,
      template = TemplateControl.default,
      capture = CaptureControl.default,
      temporal = TemporalControl.default,
      execution = ExecutionControl.default
    )

  val fastFmri: MotionControl =
    default
