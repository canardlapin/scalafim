package scalafim.fmri.group

/** The reference distribution a group statistic is tested against.
  *
  * OLS-style estimators estimate the residual variance, so they test against a
  * Student-t with `df` degrees of freedom. Inverse-variance meta-analytic
  * estimators treat the per-subject variances as known, so they test against a
  * standard normal.
  */
enum GroupStatistic:
  case StudentT(df: Int)
  case Normal

  /** Two-sided p-value for a statistic value under this distribution. */
  def twoSidedP(statistic: Double): Double =
    this match
      case StudentT(df) => Distributions.studentTTwoSidedP(statistic, df)
      case Normal       => Distributions.normalTwoSidedP(statistic)

  def label: String =
    this match
      case StudentT(df) => s"t($df)"
      case Normal       => "z"
