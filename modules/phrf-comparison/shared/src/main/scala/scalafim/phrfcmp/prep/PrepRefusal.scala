package scalafim.phrfcmp.prep

import scalafim.fmri.ar.{CorrectionFallback, CorrectionSkip}

/** Typed reason common preparation refused; no exception crosses this boundary. */
enum PrepRefusal:
  case Inconsistent(detail: String)
  case RunLayout(detail: String)
  case NuisanceIntercepts(run: Int, found: Int)
  case Design(detail: String)
  case Rho(value: Double)
  case Whitening(detail: String)
  case PlanConfigMismatch(detail: String)
  case Sigma2(detail: String)
  case ArEstimation(detail: String)
  case ArSolveFallback(run: Int, reason: CorrectionFallback)
  case ArNotAttempted(run: Int, reason: CorrectionSkip)
  case ArNotCorrected(run: Int)
  case InputsDiffer(expected: String, found: String)
  case RankMismatch(linearRank: Int, arRank: Int)
  case StationarityClamp(phi: Double, bound: Double)
  case ArIllConditioned(run: Int, reciprocalCondition: Double)

  def message: String = this match
    case Inconsistent(d)           => s"inconsistent inputs: $d"
    case RunLayout(d)              => s"run layout: $d"
    case NuisanceIntercepts(r, n)  => s"run $r has $n generator intercept columns (expected exactly 1)"
    case Design(d)                 => s"pre-fit design: $d"
    case Rho(v)                    => s"pooled AR(1) estimate $v is not finite with abs < 1"
    case Whitening(d)              => s"whitening: $d"
    case PlanConfigMismatch(d)     => s"whitening plan and FitConfig disagree: $d"
    case Sigma2(d)                 => s"sigma2: $d"
    case ArEstimation(d)           => s"AR estimation: $d"
    case ArSolveFallback(r, w)     => s"AR bias solve fell back to the raw autocovariance for run $r ($w); the pilot refuses"
    case ArNotAttempted(r, w)      => s"AR bias correction was not attempted for run $r ($w); the pilot refuses"
    case ArNotCorrected(r)         => s"AR estimate for run $r is uncorrected; the pilot refuses"
    case InputsDiffer(e, f)        => s"inputs differ from those the preparation was built from (fingerprint $f, expected $e)"
    case RankMismatch(l, a)        => s"design rank differs between the residual-forming fit ($l) and the AR module ($a); the correction would be wrong"
    case StationarityClamp(p, b)   => s"pooled AR(1) $p sits at the stationarity clamp (bound $b); something is badly wrong, refusing"
    case ArIllConditioned(r, rc)   =>
      s"AR bias correction is ill-conditioned for run $r (reciprocal condition $rc); the pilot refuses rather than fall back to the raw estimator"
