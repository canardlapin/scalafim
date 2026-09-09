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
  case RandomEffects(tau: TauEstimator = TauEstimator.DerSimonianLaird, inference: MetaInference = MetaInference.Normal)

  /** Whether this estimator needs per-subject variances to run. */
  def requiresVariance: Boolean =
    this match
      case Unweighted        => false
      case InverseVariance   => true
      case RandomEffects(_, _)  => true

  def label: String =
    this match
      case Unweighted          => "ols"
      case InverseVariance     => "meta:fe"
      case RandomEffects(tau, inference) => s"meta:re(${tau.label},${inference.label})"

/** Estimator of the between-subject heterogeneity variance `tau^2`. */
enum TauEstimator:
  case DerSimonianLaird
  case PauleMandel

  def label: String =
    this match
      case DerSimonianLaird => "DL"
      case PauleMandel => "PM"

/** Reference-distribution policy is independent of heterogeneity estimation.
  * Normal preserves the legacy plug-in z test. ModifiedKnappHartung scales
  * covariance by max(1, Q_RE / (n-p)) and uses t(n-p). It is a pointwise
  * small-sample method, not a guarantee under arbitrary variance misspecification.
  */
enum MetaInference:
  case Normal
  case ModifiedKnappHartung

  def label: String = this match
    case Normal => "z"
    case ModifiedKnappHartung => "mKH"
