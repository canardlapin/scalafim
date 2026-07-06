package scalafim.fmri.motion

final case class FrameFitDiagnostics(
    costInitial: Double,
    costFinal: Double,
    iterations: Int,
    overlap: Double,
    restarted: Boolean,
    converged: Boolean
)

final case class MotionEstimate(
    trace: MotionTrace,
    diagnostics: Vector[FrameFitDiagnostics],
    control: MotionControl
):
  require(diagnostics.length == trace.length, "diagnostics length must match trace length")
