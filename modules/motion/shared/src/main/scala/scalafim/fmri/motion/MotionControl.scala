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

enum CapturePolicy:
  case Disabled
  case GridSearch

  def enabled: Boolean =
    this match
      case Disabled => false
      case GridSearch => true

object CapturePolicy:
  def fromEnabled(enabled: Boolean): CapturePolicy =
    if enabled then CapturePolicy.GridSearch else CapturePolicy.Disabled

final case class CaptureControl private (
    policy: CapturePolicy,
    translationHalfWidthMm: Double,
    rotationHalfWidthDeg: Double,
    topK: Int
):
  require(translationHalfWidthMm.isFinite && translationHalfWidthMm > 0.0, "translationHalfWidthMm must be positive")
  require(rotationHalfWidthDeg.isFinite && rotationHalfWidthDeg > 0.0, "rotationHalfWidthDeg must be positive")
  require(topK >= 1, "topK must be positive")

  def enabled: Boolean = policy.enabled

  def withEnabled(enabled: Boolean): CaptureControl =
    copy(policy = CapturePolicy.fromEnabled(enabled))

object CaptureControl:
  val default: CaptureControl =
    unsafe(CapturePolicy.GridSearch, translationHalfWidthMm = 8.0, rotationHalfWidthDeg = 8.0, topK = 4)

  def apply(
      enabled: Boolean,
      translationHalfWidthMm: Double,
      rotationHalfWidthDeg: Double,
      topK: Int
  ): CaptureControl =
    unsafe(CapturePolicy.fromEnabled(enabled), translationHalfWidthMm, rotationHalfWidthDeg, topK)

  def make(
      policy: CapturePolicy,
      translationHalfWidthMm: Double,
      rotationHalfWidthDeg: Double,
      topK: Int
  ): Either[MotionError, CaptureControl] =
    if !translationHalfWidthMm.isFinite || translationHalfWidthMm <= 0.0 then
      Left(MotionError.InvalidScalar("translationHalfWidthMm", translationHalfWidthMm, "must be positive"))
    else if !rotationHalfWidthDeg.isFinite || rotationHalfWidthDeg <= 0.0 then
      Left(MotionError.InvalidScalar("rotationHalfWidthDeg", rotationHalfWidthDeg, "must be positive"))
    else if topK < 1 then Left(MotionError.InvalidInt("topK", topK, "must be positive"))
    else Right(unsafe(policy, translationHalfWidthMm, rotationHalfWidthDeg, topK))

  def unsafe(
      policy: CapturePolicy,
      translationHalfWidthMm: Double,
      rotationHalfWidthDeg: Double,
      topK: Int
  ): CaptureControl =
    new CaptureControl(policy, translationHalfWidthMm, rotationHalfWidthDeg, topK)

enum TemporalRegularizationPolicy:
  case Disabled
  case Enabled

  def enabled: Boolean =
    this match
      case Disabled => false
      case Enabled => true

object TemporalRegularizationPolicy:
  def fromEnabled(enabled: Boolean): TemporalRegularizationPolicy =
    if enabled then TemporalRegularizationPolicy.Enabled else TemporalRegularizationPolicy.Disabled

enum LowMotionPosePolicy:
  case Disabled(poseScale: PoseScale, thresholdMm: MotionMagnitudeMm)
  case Shrink(poseScale: PoseScale, thresholdMm: MotionMagnitudeMm)

  def enabled: Boolean =
    this match
      case Disabled(_, _) => false
      case Shrink(_, _) => true

  def scale: PoseScale =
    this match
      case Disabled(poseScale, _) => poseScale
      case Shrink(poseScale, _) => poseScale

  def threshold: MotionMagnitudeMm =
    this match
      case Disabled(_, thresholdMm) => thresholdMm
      case Shrink(_, thresholdMm) => thresholdMm

  def withEnabled(enabled: Boolean): LowMotionPosePolicy =
    if enabled then LowMotionPosePolicy.Shrink(scale, threshold)
    else LowMotionPosePolicy.Disabled(scale, threshold)

object LowMotionPosePolicy:
  val default: LowMotionPosePolicy =
    Disabled(PoseScale.unsafe(0.95), MotionMagnitudeMm.unsafe(0.25))

  def fromBoolean(enabled: Boolean, scale: Double, thresholdMm: Double): LowMotionPosePolicy =
    val typedScale = PoseScale.unsafe(scale)
    val typedThreshold = MotionMagnitudeMm.unsafe(thresholdMm)
    if enabled then Shrink(typedScale, typedThreshold)
    else Disabled(typedScale, typedThreshold)

  def make(enabled: Boolean, scale: Double, thresholdMm: Double): Either[MotionError, LowMotionPosePolicy] =
    for
      typedScale <- PoseScale(scale)
      typedThreshold <- MotionMagnitudeMm(thresholdMm)
    yield if enabled then Shrink(typedScale, typedThreshold) else Disabled(typedScale, typedThreshold)

final case class TemporalControl private (
    regularization: TemporalRegularizationPolicy,
    lowMotionPose: LowMotionPosePolicy
):
  def regularizationEnabled: Boolean = regularization.enabled
  def lowMotionPoseShrink: Boolean = lowMotionPose.enabled
  def lowMotionPoseScale: Double = lowMotionPose.scale.value
  def lowMotionThresholdMm: Double = lowMotionPose.threshold.value

  def withRegularizationEnabled(enabled: Boolean): TemporalControl =
    copy(regularization = TemporalRegularizationPolicy.fromEnabled(enabled))

  def withLowMotionPoseShrink(enabled: Boolean): TemporalControl =
    copy(lowMotionPose = lowMotionPose.withEnabled(enabled))

object TemporalControl:
  val default: TemporalControl =
    unsafe(TemporalRegularizationPolicy.Disabled, LowMotionPosePolicy.default)

  def apply(
      regularizationEnabled: Boolean,
      lowMotionPoseShrink: Boolean,
      lowMotionPoseScale: Double,
      lowMotionThresholdMm: Double = 0.25
  ): TemporalControl =
    unsafe(
      TemporalRegularizationPolicy.fromEnabled(regularizationEnabled),
      LowMotionPosePolicy.fromBoolean(lowMotionPoseShrink, lowMotionPoseScale, lowMotionThresholdMm)
    )

  def make(
      regularization: TemporalRegularizationPolicy,
      lowMotionPose: LowMotionPosePolicy
  ): Either[MotionError, TemporalControl] =
    Right(unsafe(regularization, lowMotionPose))

  def make(
      regularizationEnabled: Boolean,
      lowMotionPoseShrink: Boolean,
      lowMotionPoseScale: Double,
      lowMotionThresholdMm: Double = 0.25
  ): Either[MotionError, TemporalControl] =
    LowMotionPosePolicy
      .make(lowMotionPoseShrink, lowMotionPoseScale, lowMotionThresholdMm)
      .map(unsafe(TemporalRegularizationPolicy.fromEnabled(regularizationEnabled), _))

  def unsafe(
      regularization: TemporalRegularizationPolicy,
      lowMotionPose: LowMotionPosePolicy
  ): TemporalControl =
    new TemporalControl(regularization, lowMotionPose)

enum ExecutionPolicy:
  case Deterministic
  case ParallelFrames

  def parallelFrames: Boolean =
    this match
      case Deterministic => false
      case ParallelFrames => true

object ExecutionPolicy:
  def fromParallelFrames(parallelFrames: Boolean): ExecutionPolicy =
    if parallelFrames then ExecutionPolicy.ParallelFrames else ExecutionPolicy.Deterministic

final case class ExecutionControl private (
    policy: ExecutionPolicy,
    nThreads: Int
):
  require(nThreads >= 1, "nThreads must be positive")

  def parallelFrames: Boolean =
    policy.parallelFrames

object ExecutionControl:
  val default: ExecutionControl =
    unsafe(ExecutionPolicy.Deterministic, nThreads = 1)

  def apply(parallelFrames: Boolean, nThreads: Int): ExecutionControl =
    unsafe(ExecutionPolicy.fromParallelFrames(parallelFrames), nThreads)

  def make(policy: ExecutionPolicy, nThreads: Int): Either[MotionError, ExecutionControl] =
    if nThreads < 1 then Left(MotionError.InvalidInt("nThreads", nThreads, "must be positive"))
    else Right(unsafe(policy, nThreads))

  def make(parallelFrames: Boolean, nThreads: Int): Either[MotionError, ExecutionControl] =
    make(ExecutionPolicy.fromParallelFrames(parallelFrames), nThreads)

  def unsafe(policy: ExecutionPolicy, nThreads: Int): ExecutionControl =
    new ExecutionControl(policy, nThreads)

final case class StencilBins private (x: Int, y: Int, z: Int):
  require(x >= 1 && y >= 1 && z >= 1, "stencil bins must be positive")

object StencilBins:
  val default: StencilBins =
    unsafe(4, 4, 2)

  def make(x: Int, y: Int, z: Int): Either[MotionError, StencilBins] =
    if x < 1 then Left(MotionError.InvalidInt("stencil.bins.x", x, "must be positive"))
    else if y < 1 then Left(MotionError.InvalidInt("stencil.bins.y", y, "must be positive"))
    else if z < 1 then Left(MotionError.InvalidInt("stencil.bins.z", z, "must be positive"))
    else Right(unsafe(x, y, z))

  def unsafe(x: Int, y: Int, z: Int): StencilBins =
    new StencilBins(x, y, z)

final case class InformationContentStencil private (
    sampleCount: Int,
    bins: StencilBins,
    gamma: Double
):
  require(sampleCount >= 1, "sampleCount must be positive")
  require(gamma.isFinite && gamma > 0.0, "gamma must be positive and finite")

object InformationContentStencil:
  val default: InformationContentStencil =
    unsafe(sampleCount = 1000, bins = StencilBins.default, gamma = 0.5)

  def make(sampleCount: Int, bins: StencilBins, gamma: Double): Either[MotionError, InformationContentStencil] =
    if sampleCount < 1 then Left(MotionError.InvalidInt("stencil.sampleCount", sampleCount, "must be positive"))
    else if !gamma.isFinite || gamma <= 0.0 then Left(MotionError.InvalidScalar("stencil.gamma", gamma, "must be positive and finite"))
    else Right(unsafe(sampleCount, bins, gamma))

  def make(
      sampleCount: Int,
      binsX: Int,
      binsY: Int,
      binsZ: Int,
      gamma: Double
  ): Either[MotionError, InformationContentStencil] =
    StencilBins.make(binsX, binsY, binsZ).flatMap(bins => make(sampleCount, bins, gamma))

  def unsafe(sampleCount: Int, bins: StencilBins, gamma: Double): InformationContentStencil =
    new InformationContentStencil(sampleCount, bins, gamma)

enum StencilPolicy:
  case Dense
  case InformationContent(stencil: InformationContentStencil)

  def enabled: Boolean =
    this match
      case Dense => false
      case InformationContent(_) => true

final case class StencilControl(policy: StencilPolicy):
  def enabled: Boolean =
    policy.enabled

object StencilControl:
  val default: StencilControl =
    StencilControl(StencilPolicy.Dense)

  val defaultInformationContent: StencilControl =
    StencilControl(StencilPolicy.InformationContent(InformationContentStencil.default))

  def informationContent(stencil: InformationContentStencil): StencilControl =
    StencilControl(StencilPolicy.InformationContent(stencil))

enum WhiteningPolicy:
  case Disabled
  case FrameMeanOnly
  case IcWhiten

  def implemented: Boolean =
    this match
      case Disabled | FrameMeanOnly => true
      case IcWhiten => false

  def removesFrameMean: Boolean =
    this match
      case FrameMeanOnly => true
      case Disabled | IcWhiten => false

final case class WhiteningControl(policy: WhiteningPolicy, ridge: Double = WhiteningControl.defaultRidge):
  require(ridge.isFinite && ridge > 0.0, "ridge must be positive and finite")

  def implemented: Boolean =
    policy.implemented

  def removeFrameMean: Boolean =
    policy.removesFrameMean

object WhiteningControl:
  val defaultRidge: Double = 1e-6

  val default: WhiteningControl =
    WhiteningControl(WhiteningPolicy.Disabled)

  def make(policy: WhiteningPolicy, ridge: Double): Either[MotionError, WhiteningControl] =
    if !ridge.isFinite || ridge <= 0.0 then Left(MotionError.InvalidScalar("whitening.ridge", ridge, "must be positive and finite"))
    else Right(WhiteningControl(policy, ridge))

enum ResidualNuisancePolicy:
  case Raw
  case RemoveFrameMean

  def removesFrameMean: Boolean =
    this match
      case Raw => false
      case RemoveFrameMean => true

final case class ResidualControl(nuisance: ResidualNuisancePolicy):
  def removeFrameMean: Boolean = nuisance.removesFrameMean

object ResidualControl:
  val default: ResidualControl =
    ResidualControl(ResidualNuisancePolicy.Raw)

  def fromRemoveFrameMean(removeFrameMean: Boolean): ResidualControl =
    if removeFrameMean then ResidualControl(ResidualNuisancePolicy.RemoveFrameMean)
    else default

final case class MotionControl(
    pyramid: PyramidControl,
    optimizer: OptimizerControl,
    template: TemplateControl,
    capture: CaptureControl,
    temporal: TemporalControl,
    execution: ExecutionControl,
    stencil: StencilControl = StencilControl.default,
    residual: ResidualControl = ResidualControl.default,
    whitening: WhiteningControl = WhiteningControl.default
):
  def removeFrameMeanResidual: Boolean =
    residual.removeFrameMean || whitening.removeFrameMean

object MotionControl:
  val default: MotionControl =
    MotionControl(
      pyramid = PyramidControl.default,
      optimizer = OptimizerControl.default,
      template = TemplateControl.default,
      capture = CaptureControl.default,
      temporal = TemporalControl.default,
      execution = ExecutionControl.default,
      stencil = StencilControl.default,
      residual = ResidualControl.default,
      whitening = WhiteningControl.default
    )

  val fastFmri: MotionControl =
    default
