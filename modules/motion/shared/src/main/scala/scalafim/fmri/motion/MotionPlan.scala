package scalafim.fmri.motion

enum ReferenceStrategy:
  case Middle
  case RobustMean
  case Frame(index: FrameIndex)

enum MotionEngine:
  case RigidRobust
  case RigidSpline

enum Interpolation:
  case Linear

enum PadMode:
  case Clamp
  case Zero

final case class MotionPlan(
    reference: ReferenceStrategy,
    engine: MotionEngine,
    control: MotionControl,
    acquisitionTiming: AcquisitionTiming = AcquisitionTiming.Volume
)

object MotionPlan:
  val default: MotionPlan =
    MotionPlan(
      reference = ReferenceStrategy.Middle,
      engine = MotionEngine.RigidRobust,
      control = MotionControl.fastFmri
    )
