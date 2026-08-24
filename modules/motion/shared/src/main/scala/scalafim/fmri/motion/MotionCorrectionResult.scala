package scalafim.fmri.motion

import scalafim.image.*

final case class MotionCorrectionResult(
    estimate: MotionEstimate,
    corrected: Option[SomeScalarSeries[Double]],
    qc: Option[MotionQc],
    plan: MotionPlan,
    applyControl: Option[ApplyControl]
):
  def trace: MotionTrace =
    estimate.trace

  def diagnostics: Vector[FrameFitDiagnostics] =
    estimate.diagnostics

  def hasCorrectedRun: Boolean =
    corrected.isDefined

object MotionCorrectionResult:
  def estimateOnly(estimate: MotionEstimate, plan: MotionPlan): MotionCorrectionResult =
    MotionCorrectionResult(
      estimate = estimate,
      corrected = None,
      qc = None,
      plan = plan,
      applyControl = None
    )

  def fromEstimate(
      run: SomeScalarSeries[Double],
      estimate: MotionEstimate,
      plan: MotionPlan,
      mask: Option[SomeMaskVolume] = None,
      applyControl: ApplyControl = ApplyControl.linear,
      qcPolicy: MotionQcPolicy = MotionQcPolicy.default
  ): Either[MotionError, MotionCorrectionResult] =
    for
      corrected <- MotionApplier.apply(run, estimate.trace, applyControl)
      packetCorrection <-
        if applyControl.acquisitionTiming.isVolume then Right(None)
        else PoseSpline
          .packetCorrectionMagnitude(estimate.trace, applyControl.acquisitionTiming, run.space.spatialDims(2))
          .map(Some(_))
      qc <- MotionQc.from(
        run = run,
        trace = estimate.trace,
        corrected = Some(corrected),
        mask = mask,
        costInit = Some(estimate.diagnostics.map(_.costInitial)),
        costFinal = Some(estimate.diagnostics.map(_.costFinal)),
        policy = qcPolicy,
        packetCorrectionMagnitude = packetCorrection
      )
    yield
      MotionCorrectionResult(
        estimate = estimate,
        corrected = Some(corrected),
        qc = Some(qc),
        plan = plan,
        applyControl = Some(applyControl)
      )

  def estimateAndApply(
      run: SomeScalarSeries[Double],
      mask: Option[SomeMaskVolume] = None,
      plan: MotionPlan = MotionPlan.default,
      applyControl: ApplyControl = ApplyControl.linear,
      qcPolicy: MotionQcPolicy = MotionQcPolicy.default
  ): Either[MotionError, MotionCorrectionResult] =
    for
      estimate <- MotionEstimator.estimate(run, mask, plan)
      result <- fromEstimate(run, estimate, plan, mask, applyControl, qcPolicy)
    yield result
