package scalafim.fmri.motion

import scalafim.image.{NeuroVec, NeuroVol}

final case class MotionQcThresholds(
    fdSpike: FramewiseDisplacementMm,
    dvarsSpike: Option[DvarsRms],
    fitCostIncreaseTolerance: FitCostTolerance
)

object MotionQcThresholds:
  val default: MotionQcThresholds =
    unsafe(fdSpikeMm = 0.5, dvarsSpikeRms = None, fitCostIncreaseTolerance = FitCostTolerance.default.value)

  def unsafe(
      fdSpikeMm: Double,
      dvarsSpikeRms: Option[Double],
      fitCostIncreaseTolerance: Double
  ): MotionQcThresholds =
    MotionQcThresholds(
      fdSpike = FramewiseDisplacementMm.unsafe(fdSpikeMm),
      dvarsSpike = dvarsSpikeRms.map(DvarsRms.unsafe),
      fitCostIncreaseTolerance = FitCostTolerance.unsafe(fitCostIncreaseTolerance)
    )

enum CensorPolicy:
  case MotionOrFitFailure
  case MotionFitOrDvars
  case MotionOnly
  case FitFailureOnly
  case Never

  def shouldCensor(motionSpike: Boolean, fitFailure: Boolean, dvarsSpike: Boolean): Boolean =
    this match
      case MotionOrFitFailure => motionSpike || fitFailure
      case MotionFitOrDvars => motionSpike || fitFailure || dvarsSpike
      case MotionOnly => motionSpike
      case FitFailureOnly => fitFailure
      case Never => false

final case class MotionQcPolicy(
    thresholds: MotionQcThresholds,
    dvarsPolicy: DvarsPolicy,
    censorPolicy: CensorPolicy
)

object MotionQcPolicy:
  val default: MotionQcPolicy =
    MotionQcPolicy(
      thresholds = MotionQcThresholds.default,
      dvarsPolicy = DvarsPolicy.RobustClip3xMedian,
      censorPolicy = CensorPolicy.MotionOrFitFailure
    )

enum FitCostTrace:
  case Missing(frameCount: FrameCount)
  case Observed(initial: FrameAligned[FitCost], finalCost: FrameAligned[FitCost])

  def isMissing: Boolean =
    this match
      case Missing(_) => true
      case Observed(_, _) => false

  def costDrop: Vector[Double] =
    this match
      case Missing(frameCount) =>
        Vector.fill(frameCount.value)(Double.NaN)
      case Observed(initial, finalCost) =>
        Vector.tabulate(initial.length)(t => initial.unsafeFrame(t).value - finalCost.unsafeFrame(t).value)

  def fitFailure(tolerance: FitCostTolerance): Vector[Boolean] =
    this match
      case Missing(frameCount) =>
        Vector.fill(frameCount.value)(false)
      case Observed(initial, finalCost) =>
        Vector.tabulate(initial.length) { t =>
          finalCost.unsafeFrame(t).value > initial.unsafeFrame(t).value + tolerance.value
        }

object FitCostTrace:
  def fromOptions(
      costInit: Option[Vector[Double]],
      costFinal: Option[Vector[Double]],
      frameCount: FrameCount
  ): Either[MotionError, FitCostTrace] =
    (costInit, costFinal) match
      case (None, None) =>
        Right(FitCostTrace.Missing(frameCount))
      case (Some(_), None) =>
        Left(MotionError.IncompleteFitCostTrace("costFinal"))
      case (None, Some(_)) =>
        Left(MotionError.IncompleteFitCostTrace("costInit"))
      case (Some(init), Some(fin)) =>
        for
          initial <- fitCostAligned("costInit", init, frameCount)
          finalCost <- fitCostAligned("costFinal", fin, frameCount)
        yield FitCostTrace.Observed(initial, finalCost)

  private def fitCostAligned(
      name: String,
      values: Vector[Double],
      frameCount: FrameCount
  ): Either[MotionError, FrameAligned[FitCost]] =
    if values.length != frameCount.value then Left(MotionError.ShapeMismatch(name, Vector(frameCount.value), Vector(values.length)))
    else
      val out = Vector.newBuilder[FitCost]
      var i = 0
      while i < values.length do
        FitCost(values(i)) match
          case Left(err) => return Left(err)
          case Right(value) => out += value
        i += 1
      Right(FrameAligned.unsafe(out.result(), frameCount))

final case class MotionQc(
    fd: Vector[Double],
    dvars: Vector[Double],
    robustDvars: Vector[Double],
    costDrop: Vector[Double],
    motionSpike: Vector[Boolean],
    dvarsSpike: Vector[Boolean],
    fitFailure: Vector[Boolean],
    censorSuggest: Vector[Boolean],
    fdPairs: Vector[FramewiseDisplacementMetric],
    dvarsPairs: Vector[DvarsMetric],
    robustDvarsPairs: Vector[DvarsMetric],
    fitCostTrace: FitCostTrace,
    policy: MotionQcPolicy
)

object MotionQc:
  def from(
      run: NeuroVec[Double],
      trace: MotionTrace,
      corrected: Option[NeuroVec[Double]] = None,
      mask: Option[NeuroVol[Boolean]] = None,
      costInit: Option[Vector[Double]] = None,
      costFinal: Option[Vector[Double]] = None,
      radius: HeadRadius = HeadRadius.default,
      policy: MotionQcPolicy = MotionQcPolicy.default
  ): Either[MotionError, MotionQc] =
    if trace.length != run.nVolumes then Left(MotionError.TraceLengthMismatch(trace.length, run.nVolumes))
    else
      corrected match
        case Some(corr) if corr.space.dims.take(4) != run.space.dims.take(4) =>
          Left(MotionError.ShapeMismatch("corrected", run.space.dims.take(4), corr.space.dims.take(4)))
        case _ =>
          val nt = run.nVolumes
          val frameCount = FrameCount.unsafe(nt)
          val qcRun = corrected.getOrElse(run)
          for
            fitCostTrace <- FitCostTrace.fromOptions(costInit, costFinal, frameCount)
            dvarsPairs <- MotionMetrics.dvarsPairs(qcRun, mask, DvarsPolicy.Raw)
            robustDvarsPairs <- MotionMetrics.dvarsPairs(qcRun, mask, policy.dvarsPolicy)
          yield
            val fdPairs = MotionMetrics.framewiseDisplacementPairs(trace, radius)
            val fd = fdCompat(frameCount, fdPairs)
            val dv = dvarsCompat(frameCount, dvarsPairs)
            val rdv = dvarsCompat(frameCount, robustDvarsPairs)
            val costDrop = fitCostTrace.costDrop
            val motionSpike = fd.map(_ > policy.thresholds.fdSpike.value)
            val dvarsSpike = policy.thresholds.dvarsSpike match
              case None => Vector.fill(nt)(false)
              case Some(threshold) => rdv.map(value => value.isFinite && value > threshold.value)
            val fitFailure = fitCostTrace.fitFailure(policy.thresholds.fitCostIncreaseTolerance)
            val censor =
              Vector.tabulate(nt) { t =>
                policy.censorPolicy.shouldCensor(motionSpike(t), fitFailure(t), dvarsSpike(t))
              }
            MotionQc(
              fd = fd,
              dvars = dv,
              robustDvars = rdv,
              costDrop = costDrop,
              motionSpike = motionSpike,
              dvarsSpike = dvarsSpike,
              fitFailure = fitFailure,
              censorSuggest = censor,
              fdPairs = fdPairs,
              dvarsPairs = dvarsPairs,
              robustDvarsPairs = robustDvarsPairs,
              fitCostTrace = fitCostTrace,
              policy = policy
            )

  private def fdCompat(
      frameCount: FrameCount,
      metrics: Vector[FramewiseDisplacementMetric]
  ): Vector[Double] =
    val out = Array.fill(frameCount.value)(0.0)
    var i = 0
    while i < metrics.length do
      val metric = metrics(i)
      out(metric.pair.current.value) = metric.value.value
      i += 1
    out.toVector

  private def dvarsCompat(
      frameCount: FrameCount,
      metrics: Vector[DvarsMetric]
  ): Vector[Double] =
    val out = Array.fill(frameCount.value)(Double.NaN)
    var i = 0
    while i < metrics.length do
      val metric = metrics(i)
      out(metric.pair.current.value) = metric.value.value
      i += 1
    out.toVector
