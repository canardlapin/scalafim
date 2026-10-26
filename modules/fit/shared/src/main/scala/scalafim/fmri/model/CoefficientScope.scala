package scalafim.fmri.model

/** The estimand scope of coefficients when a sampling frame has multiple runs.
  *
  * This is deliberately separate from baseline/intercept construction. A
  * model may use run-specific nuisance columns while still estimating one
  * shared task effect, or it may estimate a separate task effect per run.
  * `SeparateRunsThenFixedEffects` names a third estimand whose result retains
  * the per-run sufficient statistics used for the combination.
  */
enum CoefficientScope:
  case SharedAcrossRuns
  case RunSpecific
  case SeparateRunsThenFixedEffects

  def label: String =
    this match
      case SharedAcrossRuns             => "shared-across-runs"
      case RunSpecific                  => "run-specific"
      case SeparateRunsThenFixedEffects => "separate-runs-then-fixed-effects"

  def executable: Boolean =
    this match
      case SharedAcrossRuns | RunSpecific | SeparateRunsThenFixedEffects => true
