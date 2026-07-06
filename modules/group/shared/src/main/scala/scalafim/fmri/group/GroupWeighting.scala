package scalafim.fmri.group

/** How subjects are weighted when the group model is fit — the single choice
  * that turns one weighted-GLM engine into OLS or meta-analysis.
  *
  *   - `Unweighted` — ordinary least squares. Estimates residual variance;
  *     yields the classic one-/two-sample group t-test and ANCOVA.
  *   - `InverseVariance` — fixed-effects meta-analysis, `w = 1/var`. Treats the
  *     first-level variances as known; yields normal (z) statistics.
  *   - `RandomEffects` — random-effects meta-analysis, `w* = 1/(var + tau^2)`,
  *     with between-subject variance `tau^2` estimated by `tau`.
  *
  * With an intercept-only design each collapses to the classic meta-analytic
  * aggregation; with a covariate design each becomes meta-regression.
  */
enum GroupWeighting:
  case Unweighted
  case InverseVariance
  case RandomEffects(tau: TauEstimator = TauEstimator.DerSimonianLaird)

  /** Whether this estimator needs per-subject variances to run. */
  def requiresVariance: Boolean =
    this match
      case Unweighted        => false
      case InverseVariance   => true
      case RandomEffects(_)  => true

  def label: String =
    this match
      case Unweighted          => "ols"
      case InverseVariance     => "meta:fe"
      case RandomEffects(tau)  => s"meta:re(${tau.label})"

/** Estimator of the between-subject heterogeneity variance `tau^2`. */
enum TauEstimator:
  case DerSimonianLaird

  def label: String =
    this match
      case DerSimonianLaird => "DL"
