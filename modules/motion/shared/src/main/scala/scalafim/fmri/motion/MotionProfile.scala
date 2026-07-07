package scalafim.fmri.motion

enum MotionCapability(val label: String):
  case CaptureBoost extends MotionCapability("capture_boost")
  case RotationalCapture extends MotionCapability("rotational_capture")
  case DenseSampling extends MotionCapability("dense_sampling")
  case EdgeExclude extends MotionCapability("edge_exclude")
  case RobustTemplate extends MotionCapability("robust_template")
  case ValidTemplateRefresh extends MotionCapability("valid_template_refresh")
  case FastNative extends MotionCapability("fast_native")
  case ParallelFrames extends MotionCapability("parallel_frames")
  case IcStencil extends MotionCapability("ic_stencil")
  case Whitening extends MotionCapability("whiten")
  case SliceSpline extends MotionCapability("slice_spline")

enum MotionProfile:
  case DenseBaseline
  case FastNative
  case FastFmri
  case IcStencil
  case IcWhiten
  case SliceSpline

  def engine: MotionEngine =
    this match
      case SliceSpline => MotionEngine.RigidSpline
      case _ => MotionEngine.RigidRobust

  def activeCapabilities: Vector[MotionCapability] =
    this match
      case DenseBaseline =>
        Vector(
          MotionCapability.CaptureBoost,
          MotionCapability.RotationalCapture,
          MotionCapability.DenseSampling,
          MotionCapability.RobustTemplate
        )
      case FastNative | FastFmri =>
        Vector(
          MotionCapability.CaptureBoost,
          MotionCapability.RotationalCapture,
          MotionCapability.DenseSampling,
          MotionCapability.EdgeExclude,
          MotionCapability.RobustTemplate,
          MotionCapability.ValidTemplateRefresh
        )
      case IcStencil => Vector(MotionCapability.CaptureBoost, MotionCapability.IcStencil)
      case IcWhiten => Vector.empty
      case SliceSpline => Vector.empty

  def plannedCapabilities: Vector[MotionCapability] =
    this match
      case DenseBaseline => activeCapabilities
      case FastNative => activeCapabilities ++ Vector(MotionCapability.FastNative)
      case FastFmri => activeCapabilities ++ Vector(MotionCapability.FastNative, MotionCapability.ParallelFrames)
      case IcStencil => Vector(MotionCapability.CaptureBoost, MotionCapability.IcStencil)
      case IcWhiten => Vector(MotionCapability.CaptureBoost, MotionCapability.IcStencil, MotionCapability.Whitening)
      case SliceSpline => Vector(MotionCapability.CaptureBoost, MotionCapability.SliceSpline)

  def components: Vector[String] =
    activeCapabilities.map(_.label)

  def plannedComponents: Vector[String] =
    plannedCapabilities.map(_.label)

  def implemented: Boolean =
    this match
      case DenseBaseline | FastNative | FastFmri | IcStencil => true
      case IcWhiten | SliceSpline => false

  def plan(reference: ReferenceStrategy = ReferenceStrategy.Middle): Either[MotionError, MotionPlan] =
    if implemented then Right(MotionPlan(reference, engine, control))
    else Left(MotionError.NotImplemented(s"$this motion profile"))

  def control: MotionControl =
    this match
      case DenseBaseline =>
        MotionControl.default.copy(
          template = TemplateControl(robustTemplate = true, refreshValidOnly = false, edgeExcludeFraction = 0.0)
        )
      case FastNative =>
        MotionControl.default
      case FastFmri =>
        MotionControl.fastFmri
      case IcStencil =>
        MotionControl.default.copy(stencil = StencilControl.defaultInformationContent)
      case IcWhiten =>
        MotionControl.default.copy(
          stencil = StencilControl.defaultInformationContent,
          whitening = WhiteningControl(WhiteningPolicy.IcWhiten)
        )
      case SliceSpline =>
        MotionControl.default
