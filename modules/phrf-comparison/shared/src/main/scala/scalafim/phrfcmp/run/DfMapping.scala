package scalafim.phrfcmp.run

import scalafim.fmri.fit.RidgeLssPrepared
import scalafim.fmri.fit.profile.TrialBandedPreparation

/** Typed reason the df mapping (design 2.2) refused; no exception crosses this boundary. */
enum DfMappingRefusal:
  case EmptyDesign
  case NonPositiveInformation(trial: Int, q: Double)
  case NonFinite(detail: String)
  case TargetOutOfRange(target: Double)
  case NotConverged(target: Double)
  case Inconsistent(detail: String)

  def message: String = this match
    case EmptyDesign                   => "df mapping needs at least one trial"
    case NonPositiveInformation(i, q)  => s"trial $i has q = $q; the effective df needs q > 0 for every trial"
    case NonFinite(d)                  => s"non-finite value in the df mapping: $d"
    case TargetOutOfRange(t)           => s"target edf $t is outside (0, 1]"
    case NotConverged(t)               => s"the ridge solve for edf $t did not converge"
    case Inconsistent(d)               => s"df mapping inputs disagree: $d"

  /** Short status token (letters, digits, `_`), safe for ledger codes. */
  def code: String = this match
    case EmptyDesign              => "df_empty_design"
    case NonPositiveInformation(_, _) => "df_nonpositive_q"
    case NonFinite(_)             => "df_nonfinite"
    case TargetOutOfRange(_)      => "df_target_out_of_range"
    case NotConverged(_)          => "df_not_converged"
    case Inconsistent(_)          => "df_inconsistent"

/**
  * The q-based INFORMATION RATIO `(1/N) sum_i q_i / (q_i + p)` of the penalised deviation block, and its inverse. This is a
  * diagnostic surrogate (it ignores the joint amplitude structure and the condition levels); it is NOT the effective
  * degrees of freedom used to match the arms, see [[DfMapping]] and [[EdfCurve]].
  *
  * On an orthogonal design the ridge hat-matrix trace is `sum_i d_i^2 / (d_i^2 + alpha)` with `d_i^2 = q_i`, so the ratio is
  * that trace divided by `N`.
  */
object InformationRatio:

  def apply(q: Array[Double], penalty: Double): Double = RidgeLssPrepared.informationRatio(q, penalty)

  private def validate(q: Array[Double]): Either[DfMappingRefusal, Unit] =
    if q.isEmpty then Left(DfMappingRefusal.EmptyDesign)
    else
      var i = 0
      while i < q.length do
        if !q(i).isFinite then return Left(DfMappingRefusal.NonFinite(s"q($i)"))
        if !(q(i) > 0.0) then return Left(DfMappingRefusal.NonPositiveInformation(i, q(i)))
        i += 1
      Right(())

  /** The unique penalty `p >= 0` with `ratio(p) = target`, `target` in `(0, 1]`. */
  def solvePenalty(q: Array[Double], target: Double): Either[DfMappingRefusal, Double] =
    validate(q).flatMap { _ =>
      if !(target > 0.0 && target <= 1.0) then Left(DfMappingRefusal.TargetOutOfRange(target))
      else if target == 1.0 then Right(0.0)
      else EdfCurve.bisect(p => apply(q, p), target)
    }

/**
  * A strictly decreasing, continuous effective-df curve in the ridge, from `atZero` (ridge 0) down to the limit
  * `atInfinity`. For rLSS see [[EdfCurve.rlss]].
  */
final case class EdfCurve(edf: Double => Double, atZero: Double, atInfinity: Double):
  require(atZero.isFinite && atInfinity.isFinite && atZero > atInfinity, "edf curve must decrease from atZero to atInfinity")

  /**
    * The ridge whose edf equals `target`, saturating when the target is out of reach: 0 above `atZero`, a large cap (edf
    * within 1e-9 of the limit) below `atInfinity`. Returns the ridge and the edf it achieves.
    */
  def ridgeFor(target: Double): Either[DfMappingRefusal, (Double, Double)] =
    if !(target > 0.0) || !target.isFinite then Left(DfMappingRefusal.TargetOutOfRange(target))
    else if target >= atZero then Right((0.0, atZero))
    else if target <= atInfinity then
      var hi = 1.0
      var guard = 0
      while edf(hi) - atInfinity > 1e-9 * (atZero - atInfinity) && guard < 2000 do
        hi *= 2.0
        guard += 1
      if guard >= 2000 then Left(DfMappingRefusal.NotConverged(target)) else Right((hi, edf(hi)))
    else EdfCurve.bisect(edf, target).map(r => (r, edf(r)))

object EdfCurve:

  /** The rLSS amplitude edf of a prepared design ([[RidgeLssPrepared.amplitudeEdf]]): `trials` down to `groupCount`. */
  def rlss(prepared: RidgeLssPrepared): EdfCurve =
    EdfCurve(prepared.amplitudeEdf, prepared.trials.toDouble, prepared.groupCount.toDouble)

  /**
    * Deterministic root of a strictly decreasing `f(p) = target` on `[0, inf)`: bracket by doubling, then up to 200
    * bisection steps (stops at the floating-point neighbours).
    */
  private[run] def bisect(f: Double => Double, target: Double): Either[DfMappingRefusal, Double] =
    var lo = 0.0
    var hi = 1.0
    var guard = 0
    while f(hi) > target && guard < 2000 do
      lo = hi
      hi *= 2.0
      guard += 1
    if guard >= 2000 || !hi.isFinite then Left(DfMappingRefusal.NotConverged(target))
    else
      var step = 0
      var done = false
      while step < 200 && !done do
        val mid = 0.5 * (lo + hi)
        if mid <= lo || mid >= hi then done = true
        else if f(mid) > target then lo = mid
        else hi = mid
        step += 1
      Right(0.5 * (lo + hi))

/**
  * DIAGNOSTIC ONLY (owner decision 2026-10-02: the mapping is solved on true effective df, never by this factor). How PHRF's
  * native deviation penalty `lambda = 1 / alpha` would map onto the rLSS ridge scale, as two derived factors
  * (design 2.2: "S4 derives and unit-tests the scale factor against `TrialBanded`'s `lambda` use").
  *
  * `TrialBanded` adds `lambda (I - M)` to the trial Gram of its shape regressors, `M` the within-condition averaging
  * projector, so the penalty on trial amplitudes in PHRF units `u` is `lambda u'(I - M)u`.
  *
  *  - [[shape]] (`kappa^2`): rLSS regressors are `x^R = kappa x^P` with `x^P` the PHRF regressor at the canonical shape,
  *    so `u = kappa a` for the rLSS amplitude `a`, and the penalty becomes `lambda kappa^2 a^2`.
  *  - [[centring]]: rLSS moves ONE trial by `delta` against a common condition level, which is the direction
  *    `e_i` in `u'(I - M)u`: `e_i'(I - M)e_i = 1 - 1/n_c(i)`. The mean over trials is used (exact when conditions are
  *    balanced, as in the pilot).
  *
  * `p = kappa^2 * (1 - 1/n_c) * lambda` (exact only for balanced orthogonal designs). `shapeSpread` and `centringSpread` are the largest relative deviations of a
  * trial's own factor from the one used; they are receipts, not gates.
  */
final case class PenaltyScale(shape: Double, centring: Double, shapeSpread: Double, centringSpread: Double):
  require(shape > 0.0 && shape.isFinite, "shape factor must be positive and finite")
  require(centring > 0.0 && centring <= 1.0, "centring factor must be in (0, 1]")

  def factor: Double = shape * centring

  /** The rLSS-scale penalty equivalent to PHRF's `lambda`. */
  def penalty(lambda: Double): Double = factor * lambda

object PenaltyScale:

  /** No rescaling (tests only; the mapping never uses the scale). */
  private[phrfcmp] val Identity: PenaltyScale = PenaltyScale(1.0, 1.0, 0.0, 0.0)

  /** `1 - 1/n_c(i)` per trial, `conditionOfTrial` non-negative ids; `Left` if a condition has a single trial. */
  def centringByTrial(conditionOfTrial: Array[Int]): Either[DfMappingRefusal, Array[Double]] =
    if conditionOfTrial.isEmpty then Left(DfMappingRefusal.EmptyDesign)
    else if conditionOfTrial.exists(_ < 0) then Left(DfMappingRefusal.Inconsistent("negative condition id"))
    else
      val counts = new Array[Int](conditionOfTrial.max + 1)
      conditionOfTrial.foreach(c => counts(c) += 1)
      if conditionOfTrial.exists(c => counts(c) < 2) then
        Left(DfMappingRefusal.Inconsistent("a condition with one trial has no within-condition deviation"))
      else Right(Array.tabulate(conditionOfTrial.length)(i => 1.0 - 1.0 / counts(conditionOfTrial(i))))

  /**
    * `x^P_i' x^P_i = c' G_ii c` for every trial of a `TrialBanded` preparation at shape-basis coefficients `c` (the
    * kernel basis coefficients of the canonical shape), read from the preparation's own packed Gram blocks, so it is
    * exactly the quantity `lambda` is added to.
    */
  def phrfNorm2(prep: TrialBandedPreparation, coefficients: Array[Double]): Either[DfMappingRefusal, Array[Double]] =
    if coefficients.length != prep.basisRank then
      Left(DfMappingRefusal.Inconsistent(s"${coefficients.length} coefficients for a rank-${prep.basisRank} basis"))
    else
      val out = new Array[Double](prep.trials)
      var p = 0
      while p < prep.basisRank do
        var q = p
        while q < prep.basisRank do
          val block = prep.gramBlock(p, q)
          // the off-diagonal block (p < q) already holds the symmetrised cross term x_p x_q + x_q x_p
          val w = coefficients(p) * coefficients(q)
          var i = 0
          while i < prep.trials do
            out(i) += w * block(i, 0)
            i += 1
          q += 1
        p += 1
      Right(out)

  /**
    * Pool the two factors. `rlssNorm2(i) = x^R_i' x^R_i` (whitened canonical regressor), `phrfNorm2(i)` from
    * [[phrfNorm2]]; `kappa^2 = sum rlss / sum phrf`.
    */
  def derive(
      rlssNorm2: Array[Double],
      phrfNorm2: Array[Double],
      conditionOfTrial: Array[Int]
  ): Either[DfMappingRefusal, PenaltyScale] =
    if rlssNorm2.length != phrfNorm2.length || rlssNorm2.length != conditionOfTrial.length then
      Left(DfMappingRefusal.Inconsistent("per-trial arrays differ in length"))
    else if rlssNorm2.isEmpty then Left(DfMappingRefusal.EmptyDesign)
    else if rlssNorm2.exists(x => !(x > 0.0 && x.isFinite)) || phrfNorm2.exists(x => !(x > 0.0 && x.isFinite)) then
      Left(DfMappingRefusal.NonFinite("a trial regressor has zero or non-finite norm"))
    else
      centringByTrial(conditionOfTrial).map { c =>
        val kappa2 = rlssNorm2.sum / phrfNorm2.sum
        val cMean = c.sum / c.length
        val shapeSpread = rlssNorm2.indices.map(i => math.abs(rlssNorm2(i) / phrfNorm2(i) / kappa2 - 1.0)).max
        val cSpread = c.map(x => math.abs(x / cMean - 1.0)).max
        PenaltyScale(kappa2, cMean, shapeSpread, cSpread)
      }

/**
  * The df mapping of one design: for grid point `k`, `target(k)` is PHRF's effective df at `lambda_k` (supplied by the
  * PHRF side, computed from the design alone) and `ridges(k)` the rLSS ridge with `edf_R(r_k) = target(k)`.
  *
  * @param flagged `true` when the achieved edf at either grid END misses its target by more than 5 % (the two edf ranges
  *                fail to overlap there); sealed, never altered.
  * @param scaleDiagnosticRidges the old penalty-scale ridges `kappa^2 (1 - 1/n_c) lambda_k`, REPORTED ONLY (never used)
  */
final case class DfMap(
    grid: AlphaGrid,
    targetEdf: Vector[Double],
    ridges: Vector[Double],
    achievedEdf: Vector[Double],
    flagged: Boolean,
    scaleDiagnosticRidges: Option[Vector[Double]]
):
  def maxEdfResidual: Double = targetEdf.zip(achievedEdf).map((t, a) => math.abs(t - a)).max

object DfMapping:

  /** The fraction by which an end-point edf may miss its target before the dataset is flagged. */
  val EndTolerance: Double = 0.05

  /**
    * Map every grid point: `targetEdf(k)` is PHRF's effective df at `lambda_k`; the ridge is solved on `curve` (the rLSS
    * edf of the design that will be fitted). `diagnostic` carries the penalty-scale ridges for reporting.
    */
  def map(
      grid: AlphaGrid,
      targetEdf: Vector[Double],
      curve: EdfCurve,
      diagnostic: Option[Vector[Double]] = None
  ): Either[DfMappingRefusal, DfMap] =
    if targetEdf.length != grid.size then Left(DfMappingRefusal.Inconsistent(s"${targetEdf.length} targets for a ${grid.size}-point grid"))
    else if diagnostic.exists(_.length != grid.size) then Left(DfMappingRefusal.Inconsistent("diagnostic ridges differ from the grid size"))
    else
      val solved = Vector.newBuilder[(Double, Double)]
      var failure: Option[DfMappingRefusal] = None
      var k = 0
      while failure.isEmpty && k < grid.size do
        curve.ridgeFor(targetEdf(k)) match
          case Left(e)  => failure = Some(e)
          case Right(r) => solved += r
        k += 1
      failure match
        case Some(e) => Left(e)
        case None =>
          val sol = solved.result()
          val ends = Vector(0, grid.size - 1)
          val flagged = ends.exists(e => math.abs(sol(e)._2 - targetEdf(e)) > EndTolerance * targetEdf(e))
          Right(DfMap(grid, targetEdf, sol.map(_._1), sol.map(_._2), flagged, diagnostic))
