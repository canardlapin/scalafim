package scalafim.fmri.motion

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

  def components: Vector[String] =
    this match
      case DenseBaseline => Vector("capture_boost", "dense_sampling")
      case FastNative => Vector("capture_boost", "dense_sampling", "edge_exclude")
      case FastFmri => Vector("capture_boost", "dense_sampling", "edge_exclude")
      case IcStencil => Vector.empty
      case IcWhiten => Vector.empty
      case SliceSpline => Vector.empty

  def plannedComponents: Vector[String] =
    this match
      case DenseBaseline => components
      case FastNative => components ++ Vector("fast_native")
      case FastFmri => components ++ Vector("fast_native", "valid_template_refresh", "parallel_frames")
      case IcStencil => Vector("capture_boost", "ic_stencil")
      case IcWhiten => Vector("capture_boost", "ic_stencil", "whiten")
      case SliceSpline => Vector("capture_boost", "slice_spline")

  def implemented: Boolean =
    this match
      case DenseBaseline | FastNative | FastFmri => true
      case IcStencil | IcWhiten | SliceSpline => false

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
        MotionControl.default
      case IcWhiten =>
        MotionControl.default
      case SliceSpline =>
        MotionControl.default
