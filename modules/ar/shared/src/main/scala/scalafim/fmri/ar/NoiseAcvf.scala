package scalafim.fmri.ar

import gale.linalg.DMat

/** Autocovariance of one pooling unit: a run, or all runs pooled.
  *
  * @param runIndex the run this unit estimates, or `None` when runs were pooled
  * @param acvf lags zero upward; lags with no surviving pairs are dropped rather than reported as zero
  * @param pairs the pair count behind each reported lag
  * @param corrected whether the residual-bias correction was actually applied to every run behind this unit
  */
final case class NoiseAcvfUnit(
    runIndex: Option[Int],
    acvf: Vector[Double],
    pairs: Vector[Long],
    segmentCount: Int,
    segmentLengths: Vector[Int],
    corrected: Boolean,
    fallback: Option[CorrectionFallback] = None
)

/** @param corrections the outcome per run (all runs, not just those reported in `units`): the conditioning gate,
  *                    overridden by [[RunCorrection.SolveFallback]] when the solve fell back to the raw estimate
  */
final case class NoiseAcvfEstimate(
    units: Vector[NoiseAcvfUnit],
    maxLag: Int,
    pooling: NoisePooling,
    corrected: Boolean,
    corrections: Vector[RunCorrection]
)

/** Run- and censor-aware noise autocovariance, mirroring fmriAR's `noise_acvf()`.
  *
  * Lag products never cross a run boundary or a censoring gap, and the mean is removed per run rather than per
  * fragment. With [[EstimationPolicy.DesignCorrected]] the residual-bias correction is solved over its own lag
  * budget (independent of `maxLag`) before truncating to `maxLag` and repairing positive definiteness.
  */
object NoiseAcvf:

  def estimate(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      pooling: NoisePooling = NoisePooling.Global,
      policy: EstimationPolicy = EstimationPolicy.Raw
  ): Either[ArError, NoiseAcvfEstimate] =
    for
      lag <- ArLag(maxLag)
      _ <- layout.coveredSegments.validateRows(residuals.rows)
      _ <- ArEstimation.validateFinite(residuals)
      prepared <- ArEstimation.resolveCorrection(residuals, layout, math.max(1, lag.value), policy)
      estimate <- estimateWith(residuals, layout, math.min(lag.value, residuals.rows), pooling, prepared)
    yield estimate

  /** As above with a correction prepared once by [[AcvfBias.prepare]] for `max(1, maxLag)` as its target order;
    * `design` must be exactly the prepared design (see [[AcvfBias.bind]]).
    */
  def estimate(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      pooling: NoisePooling,
      design: DMat,
      prepared: PreparedCorrection
  ): Either[ArError, NoiseAcvfEstimate] =
    for
      lag <- ArLag(maxLag)
      _ <- layout.coveredSegments.validateRows(residuals.rows)
      _ <- ArEstimation.validateFinite(residuals)
      bound <- AcvfBias.bind(residuals, layout, design, math.max(1, lag.value), prepared)
      estimate <- estimateWith(residuals, layout, math.min(lag.value, residuals.rows), pooling, bound)
    yield estimate

  private[ar] def estimateWith(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      pooling: NoisePooling,
      prepared: PreparedCorrection
  ): Either[ArError, NoiseAcvfEstimate] =
    perRunUnits(residuals, layout, maxLag, prepared).map(finish(_, maxLag, pooling))

  /** Units per run (runs without usable data omitted) and the honest status of every run. The status is carried
    * independently of whether a unit exists: a run whose raw variance is not positive has no unit but did fall
    * back, and a run without data never attempted a solve.
    */
  private[ar] final case class RunUnits(units: Vector[NoiseAcvfUnit], statuses: Vector[RunCorrection])

  private[ar] def perRunUnits(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      prepared: PreparedCorrection
  ): Either[ArError, RunUnits] =
    val units = Vector.newBuilder[NoiseAcvfUnit]
    val statuses = Vector.newBuilder[RunCorrection]
    var run = 0
    while run < layout.runCount do
      oneUnit(residuals, layout, run, maxLag, prepared.usable(run)) match
        case Left(error) => return Left(error)
        case Right(outcome) =>
          outcome.unit.foreach(units += _)
          statuses += finalStatus(prepared.runs(run), outcome)
      run += 1
    val perRun = units.result()
    if perRun.isEmpty then Left(ArError.NoEstimableRows) else Right(RunUnits(perRun, statuses.result()))

  private[ar] def perRunUnits(summary: ArNoiseSummary, maxLag: Int): Either[ArError, RunUnits] =
    val units = Vector.newBuilder[NoiseAcvfUnit]
    val statuses = Vector.newBuilder[RunCorrection]
    var run = 0
    while run < summary.layout.runCount do
      val segments = summary.layout.segmentsForRun(run)
      val outcome =
        if segments.map(_.length).sum < 2 then Right(RunOutcome(None, Some(CorrectionSkip.FewerThanTwoObservations), None))
        else
          fromPooled(
            ArEstimation.PooledAutocovariance(summary.sumsByRun(run).toArray,
              summary.countsByRun(run).toArray, summary.correction.usable(run)),
            run, segments, maxLag
          )
      outcome match
        case Left(error) => return Left(error)
        case Right(value) =>
          value.unit.foreach(units += _)
          statuses += finalStatus(summary.correction.runs(run), value)
      run += 1
    val perRun = units.result()
    if perRun.isEmpty then Left(ArError.NoEstimableRows) else Right(RunUnits(perRun, statuses.result()))

  /** The gate status stands for runs that were never going to be solved (uncorrected, or rejected by the gate);
    * otherwise what actually happened at estimation time wins.
    */
  private def finalStatus(gate: RunCorrection, outcome: RunOutcome): RunCorrection =
    gate match
      case RunCorrection.Applied(_) =>
        outcome.skipped
          .map(RunCorrection.NotAttempted(_))
          .orElse(outcome.fallback.map(RunCorrection.SolveFallback(_)))
          .getOrElse(gate)
      case other => other

  private final case class RunOutcome(
      unit: Option[NoiseAcvfUnit],
      skipped: Option[CorrectionSkip],
      fallback: Option[CorrectionFallback]
  )

  private[ar] def finish(
      runUnits: RunUnits,
      maxLag: Int,
      pooling: NoisePooling
  ): NoiseAcvfEstimate =
      val perRun = runUnits.units
      val corrections = runUnits.statuses
      val reported =
        pooling match
          case NoisePooling.Run    => perRun
          case NoisePooling.Global => if perRun.length > 1 then Vector(poolUnits(perRun)) else perRun.map(_.copy(runIndex = None))
      NoiseAcvfEstimate(reported, maxLag, pooling, reported.forall(_.corrected), corrections)

  private def oneUnit(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      run: Int,
      maxLag: Int,
      correction: Option[DMat]
  ): Either[ArError, RunOutcome] =
    val segments = layout.segmentsForRun(run)
    val observations = segments.map(_.length).sum
    if observations < 2 then Right(RunOutcome(None, Some(CorrectionSkip.FewerThanTwoObservations), None))
    else
      val accumulate = correction.fold(maxLag)(matrix => math.max(maxLag, matrix.rows - 1))
      ArEstimation.pooledAutocovariance(residuals, segments, ArOrderValue.unsafe(accumulate), correction)
        .flatMap(fromPooled(_, run, segments, maxLag))

  private def fromPooled(
      pooled: ArEstimation.PooledAutocovariance,
      run: Int,
      segments: Vector[TimeSegment],
      maxLag: Int
  ): Either[ArError, RunOutcome] =
    if pooled.pairCounts(0) <= 0L then Right(RunOutcome(None, Some(CorrectionSkip.NoLagZeroPairs), None))
    else
      val order = math.min(maxLag, pooled.maxLag.value)
      pooled.through(ArOrderValue.unsafe(order)).map { gamma =>
        val unit =
          if gamma.lagZero <= 0.0 then None
          else Some(NoiseAcvfUnit(
            runIndex = Some(run), acvf = gamma.toVector,
            pairs = pooled.pairCounts.take(gamma.length).toVector,
            segmentCount = segments.length, segmentLengths = segments.map(_.length),
            corrected = pooled.correctionApplied, fallback = pooled.correctionFallback
          ))
        RunOutcome(unit, None, pooled.correctionFallback)
      }

  /** Length-weighted pooling truncated to the shortest unit. A zero-padded autocovariance is not a covariance:
    * averaging equal-length positive-definite Toeplitz matrices stays positive definite by convexity, while
    * averaging padded ones does not.
    */
  private def poolUnits(units: Vector[NoiseAcvfUnit]): NoiseAcvfUnit =
    val length = units.map(_.acvf.length).min
    val weights = units.map(_.segmentLengths.sum.toDouble)
    val total = weights.sum
    val acvf = Vector.tabulate(length) { lag =>
      var sum = 0.0
      var i = 0
      while i < units.length do
        sum += weights(i) / total * units(i).acvf(lag)
        i += 1
      sum
    }
    val pairs = Vector.tabulate(length)(lag => units.map(_.pairs(lag)).sum)
    NoiseAcvfUnit(
      runIndex = None,
      acvf = acvf,
      pairs = pairs,
      segmentCount = units.map(_.segmentCount).sum,
      segmentLengths = units.flatMap(_.segmentLengths),
      corrected = units.forall(_.corrected),
      // The pooled unit summarises with the first run's fallback only; the full per-run picture is
      // `NoiseAcvfEstimate.corrections`.
      fallback = units.flatMap(_.fallback).headOption
    )
