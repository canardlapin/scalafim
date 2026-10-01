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

  /** As above with a correction prepared once by [[AcvfBias.prepare]]; bound to its design and layout. */
  def estimate(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      pooling: NoisePooling,
      prepared: PreparedCorrection
  ): Either[ArError, NoiseAcvfEstimate] =
    for
      lag <- ArLag(maxLag)
      _ <- layout.coveredSegments.validateRows(residuals.rows)
      _ <- ArEstimation.validateFinite(residuals)
      bound <- AcvfBias.bind(residuals, layout, prepared)
      estimate <- estimateWith(residuals, layout, math.min(lag.value, residuals.rows), pooling, bound)
    yield estimate

  private[ar] def estimateWith(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      pooling: NoisePooling,
      prepared: PreparedCorrection
  ): Either[ArError, NoiseAcvfEstimate] =
    perRunUnits(residuals, layout, maxLag, prepared).map(finish(_, maxLag, pooling, prepared))

  /** Per-run units (before pooling), runs without usable data omitted. */
  private[ar] def perRunUnits(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      maxLag: Int,
      prepared: PreparedCorrection
  ): Either[ArError, Vector[NoiseAcvfUnit]] =
    val units = Vector.newBuilder[NoiseAcvfUnit]
    var run = 0
    while run < layout.runCount do
      oneUnit(residuals, layout, run, maxLag, prepared.usable(run)) match
        case Left(error)       => return Left(error)
        case Right(Some(unit)) => units += unit
        case Right(None)       => ()
      run += 1
    val perRun = units.result()
    if perRun.isEmpty then Left(ArError.NoEstimableRows) else Right(perRun)

  private[ar] def finish(
      perRun: Vector[NoiseAcvfUnit],
      maxLag: Int,
      pooling: NoisePooling,
      prepared: PreparedCorrection
  ): NoiseAcvfEstimate =
      val corrections = prepared.runs.zipWithIndex.map { case (gate, run) =>
        perRun.find(_.runIndex.contains(run)).flatMap(_.fallback).fold(gate)(RunCorrection.SolveFallback(_))
      }
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
  ): Either[ArError, Option[NoiseAcvfUnit]] =
    val segments = layout.segmentsForRun(run)
    val observations = segments.map(_.length).sum
    if observations < 2 then Right(None)
    else
      val accumulate = correction.fold(maxLag)(matrix => math.max(maxLag, matrix.rows - 1))
      ArEstimation.pooledAutocovariance(residuals, segments, ArOrderValue.unsafe(accumulate), correction).flatMap { pooled =>
        if pooled.pairCounts(0) <= 0L then Right(None)
        else
          val order = math.min(maxLag, pooled.maxLag.value)
          pooled.through(ArOrderValue.unsafe(order)).map { gamma =>
            if gamma.lagZero <= 0.0 then None
            else
              Some(
                NoiseAcvfUnit(
                  runIndex = Some(run),
                  acvf = gamma.toVector,
                  pairs = pooled.pairCounts.take(gamma.length).toVector,
                  segmentCount = segments.length,
                  segmentLengths = segments.map(_.length),
                  corrected = pooled.correctionApplied,
                  fallback = pooled.correctionFallback
                )
              )
          }
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
      fallback = units.flatMap(_.fallback).headOption
    )
