package scalafim.fmri.ar

import gale.linalg.DMat

/** How many autocovariance lags the residual-bias system is solved over.
  *
  * Correcting needs a wider tail than the AR order alone: the residual projection mixes the whole covariance tail
  * into the low lags, so solving only a handful of lags discards most of what makes the correction work.
  */
enum CorrectionBudget:
  /** fmriAR's `correction_max_lag`: use `maxLag` lags (at least one), reduced when the design leaves too few
    * residual degrees of freedom to recover them.
    */
  case Fixed(maxLag: Int)

  /** fmrireg's adaptive rule: `max(order, min(ceiling, max(5, 2 * order + 1), floor(min run rdf / 3)))`, so that
    * roughly three residual degrees of freedom back every corrected lag.
    */
  case Adaptive(ceiling: Int)

object CorrectionBudget:
  val DefaultMaxLag: Int = 25
  val Default: CorrectionBudget = Fixed(DefaultMaxLag)

/** Whether noise estimation undoes the bias that residualising against a design puts into the raw autocovariance.
  *
  * `Raw` is the historical behaviour. `DesignCorrected` is an explicit statement that the residuals are ordinary
  * least-squares residuals of exactly `design`; applying it to re-whitened or robust residuals would undo a
  * projection that never occurred, so it is never chosen implicitly.
  */
enum EstimationPolicy:
  case Raw
  case DesignCorrected(design: DMat, budget: CorrectionBudget = CorrectionBudget.Default)

/** Why the bias-system solve returned the raw autocovariance. fmriAR falls back silently in every one of these
  * cases (`.apply_acvf_correction_result`); here the reason is reported.
  */
enum CorrectionFallback:
  /** The leading block of A actually solved (as long as the autocovariance that has pairs) fails the
    * reciprocal-condition gate even though the full matrix passed.
    */
  case IllConditionedBlock(reciprocalCondition: Double)

  /** The linear solve itself failed. */
  case SingularSystem

  /** The solution contains a non-finite value. */
  case NonFiniteSolution

  /** The corrected lag-zero variance is not positive. */
  case NonPositiveVariance(correctedLagZero: Double)

  /** The raw lag-zero variance is not positive, so no correction was attempted. */
  case NonPositiveRawVariance(rawLagZero: Double)

/** Outcome of the conditioning gate for one run. */
enum RunCorrection:
  /** No correction was requested for this run. */
  case Uncorrected

  /** The bias matrix passed the reciprocal-condition gate and is applied. */
  case Applied(reciprocalCondition: Double)

  /** The bias matrix is too ill-conditioned to solve against; this run's estimates are left uncorrected. */
  case IllConditioned(reciprocalCondition: Double)

  /** The matrix passed the gate, but at estimation time the solve fell back to the raw autocovariance for this
    * run (see [[CorrectionFallback]]). The estimates are uncorrected, as in fmriAR, and are reported as such.
    */
  case SolveFallback(reason: CorrectionFallback)
