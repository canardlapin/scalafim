package scalafim.group.research.bootstrap

/** Smyth (2004) hyperparameters of 1/sigma_i^2 ~ chi^2_{d0}/(d0 s0^2); d0 may be +infinity. */
final case class SmythFit(d0: Double, s0Squared: Double):
  require(d0 > 0.0 && s0Squared > 0.0 && s0Squared.isFinite, "invalid Smyth hyperparameters")

  /** Posterior scale s~_i^2 = (d0 s0^2 + nu_i v_i)/(d0 + nu_i); s0^2 when d0 is infinite; v_i when nu_i is. */
  def posteriorScale(v: Double, nu: Double): Double =
    if nu.isInfinite then v
    else if d0.isInfinite then s0Squared
    else (d0 * s0Squared + nu * v) / (d0 + nu)

  /** One posterior draw sigma*^2 = (d0 + nu) s~^2 / chi, chi a chi^2_{d0+nu} variate (§1, B-EB);
    * s0^2 when d0 is infinite; v when nu is (known variance).
    */
  def posteriorDraw(v: Double, nu: Double, chi: Double): Double =
    if nu.isInfinite then v
    else if d0.isInfinite then s0Squared
    else (d0 + nu) * posteriorScale(v, nu) / chi

object SmythFit:
  /** The Smyth 2004 moment fit (limma fitFDist without covariates), written out:
    *   e_i = log v_i - digamma(nu_i/2) + log(nu_i/2),  ebar = mean e,
    *   evar = sum (e_i - ebar)^2 / (n - 1) - mean trigamma(nu_i/2),
    *   evar > 0: d0 = 2 trigammaInverse(evar), s0^2 = exp(ebar + digamma(d0/2) - log(d0/2));
    *   otherwise d0 = infinity and s0^2 = exp(ebar).
    */
  def fit(v: Array[Double], nu: Array[Double]): Either[String, SmythFit] =
    val n = v.length
    if n < 2 || nu.length != n then Left("Smyth fit needs at least two subjects with matching df")
    else if !v.forall(x => x > 0.0 && x.isFinite) then Left("Smyth fit needs positive finite variances")
    else if !nu.forall(d => d > 0.0 && d.isFinite) then Left("Smyth fit needs finite positive df")
    else
      val e = Array.tabulate(n)(i => math.log(v(i)) - Special.digamma(nu(i) / 2.0) + math.log(nu(i) / 2.0))
      val mean = e.sum / n
      val spread = e.map(x => (x - mean) * (x - mean)).sum / (n - 1)
      val evar = spread - nu.map(d => Special.trigamma(d / 2.0)).sum / n
      if evar > 0.0 then
        Special.trigammaInverse(evar).map { half =>
          val d0 = 2.0 * half
          SmythFit(d0, math.exp(mean + Special.digamma(d0 / 2.0) - math.log(d0 / 2.0)))
        }
      else Right(SmythFit(Double.PositiveInfinity, math.exp(mean)))

enum SchemeRole:
  case Candidate, DiscriminatingControl, HypothesisControl

/** The three declared candidates (§1, O1) and the controls of §2 and §7.6. */
enum Scheme(val code: String, val role: SchemeRole, val prediction: String):
  case Plug extends Scheme("B-plug", SchemeRole.Candidate, "candidate: sigma~^2 = v, e* ~ N(0, v), v* = v chi2_nu/nu")
  case FixV extends Scheme("B-fixV", SchemeRole.Candidate, "candidate: e* ~ N(0, v), v* = v")
  case EmpiricalBayes extends Scheme("B-EB", SchemeRole.Candidate, "candidate: sigma*^2 = (d0+nu) s~^2 / chi2_{d0+nu}, e* ~ N(0, sigma*^2), v* = sigma*^2 chi2_nu/nu")
  case UnrestrictedUncentred extends Scheme(
        "C-uncentred", SchemeRole.DiscriminatingControl,
        "analytic: world at the unrestricted fit, T* = (c'b* - b0)/SE* is centred at T, so the test is conservative at the null and its power collapses"
      )
  case UnrestrictedRecentred extends Scheme(
        "C-recentred", SchemeRole.HypothesisControl,
        "descriptive: unrestricted world with T* = (c'b* - c'b)/SE*; kept as a control only (no frozen direction)"
      )
  case KnownVFrozenTau extends Scheme(
        "C-knownv-frozen-tau", SchemeRole.HypothesisControl,
        "hypothesis (v1): v treated as known, tau^2 frozen at the observed estimate, no refit; reproduces the native PM inflation in C-n80-DG-Vrev-T0-N8"
      )
  case OmitU extends Scheme(
        "C-omit-u", SchemeRole.HypothesisControl,
        "hypothesis (v1): B-plug without u*; anti-conservative at tau^2 = .2 (a heterogeneity refit may partly absorb it)"
      )
  case NuSwap extends Scheme(
        "C-nu-swap", SchemeRole.HypothesisControl,
        "hypothesis (v1): B-plug with nu replaced by n - p; a detectable shift at nu = 8 (no sign declared)"
      )

  def restricted: Boolean = this match
    case UnrestrictedUncentred | UnrestrictedRecentred => false
    case _ => true

/** The statistic being bootstrapped. `KnownVarianceZ` fixes tau^2 = 0 everywhere
  * (observed fit, null fit and draws) and studentizes with scale 1: the exact
  * pivot of the bootstrap-of-z control.
  */
enum StatisticKind:
  case PmMkh, KnownVarianceZ

/** Standardized variates for one bootstrap draw. For draw b and subject i:
  * zu, ze ~ N(0,1); chi = chi^2_df/df for the scheme's df (1 when df is infinite);
  * post = a chi^2_{d0 + nu_i} variate (NaN when the posterior is degenerate).
  */
trait DrawVariates:
  def fill(b: Int, zu: Array[Double], ze: Array[Double], chi: Array[Double], post: Array[Double]): Unit

/** Variates from the keyed Bootstrap stream, one SplitMix64 lane per kind, drawn in order b = 0, 1, ... */
final class StreamDrawVariates(key: StreamKey, chiDf: Array[Double], postDf: Array[Double]) extends DrawVariates:
  private val u = key.lane(Lane.DrawU)
  private val e = key.lane(Lane.DrawE)
  private val c = key.lane(Lane.DrawChi)
  private val q = key.lane(Lane.DrawPosterior)
  private var next = 0

  def fill(b: Int, zu: Array[Double], ze: Array[Double], chi: Array[Double], post: Array[Double]): Unit =
    require(b == next, s"stream variates are sequential: expected draw $next, got $b")
    next += 1
    var i = 0
    while i < zu.length do
      zu(i) = u.nextGaussian()
      ze(i) = e.nextGaussian()
      val df = chiDf(i)
      chi(i) = if df.isInfinite then 1.0 else c.nextChiSquare(df) / df
      val pd = postDf(i)
      post(i) = if pd.isInfinite || pd.isNaN then Double.NaN else q.nextChiSquare(pd)
      i += 1

/** Explicit pre-drawn variates (draws x n, row-major): the R reference and the
  * pathwise-invariance tests consume these.
  */
final case class MatrixDrawVariates(n: Int, draws: Int, zu: Array[Double], ze: Array[Double], chi: Array[Double], post: Array[Double])
    extends DrawVariates:
  require(Seq(zu, ze, chi, post).forall(_.length == n * draws), "variate matrices must be draws x n")

  def fill(b: Int, zuOut: Array[Double], zeOut: Array[Double], chiOut: Array[Double], postOut: Array[Double]): Unit =
    System.arraycopy(zu, b * n, zuOut, 0, n)
    System.arraycopy(ze, b * n, zeOut, 0, n)
    System.arraycopy(chi, b * n, chiOut, 0, n)
    System.arraycopy(post, b * n, postOut, 0, n)

  /** Normal innovations negated, chi-square draws unchanged (the p(y) = p(-y) pairing). */
  def negatedNormals: MatrixDrawVariates = copy(zu = zu.map(-_), ze = ze.map(-_))

  /** Subject i of the result carries subject perm(i)'s variates. */
  def permuted(perm: Array[Int]): MatrixDrawVariates =
    def move(a: Array[Double]) = Array.tabulate(n * draws)(k => a((k / n) * n + perm(k % n)))
    MatrixDrawVariates(n, draws, move(zu), move(ze), move(chi), move(post))

object MatrixDrawVariates:
  def materialize(key: StreamKey, n: Int, draws: Int, chiDf: Array[Double], postDf: Array[Double]): MatrixDrawVariates =
    val source = new StreamDrawVariates(key, chiDf, postDf)
    val out = MatrixDrawVariates(n, draws, new Array(n * draws), new Array(n * draws), new Array(n * draws), new Array(n * draws))
    val zu = new Array[Double](n); val ze = new Array[Double](n); val chi = new Array[Double](n); val post = new Array[Double](n)
    var b = 0
    while b < draws do
      source.fill(b, zu, ze, chi, post)
      System.arraycopy(zu, 0, out.zu, b * n, n)
      System.arraycopy(ze, 0, out.ze, b * n, n)
      System.arraycopy(chi, 0, out.chi, b * n, n)
      System.arraycopy(post, 0, out.post, b * n, n)
      b += 1
    out

/** One observed study: design, effects y, first-level variances v, declared df per subject (may be +inf), null value b0. */
final case class StudyData(design: ResearchDesign, y: Array[Double], v: Array[Double], nu: Array[Double], b0: Double):
  require(y.length == design.n && v.length == design.n && nu.length == design.n, "study arrays must have n entries")
  require(nu.forall(d => d > 0.0), "df must be positive (or +inf)")

  def scaled(c: Double): StudyData = copy(y = y.map(_ * c), v = v.map(_ * c * c), b0 = b0 * c)
  def negated: StudyData = copy(y = y.map(-_), b0 = -b0)
  def permuted(perm: Array[Int]): Either[DesignError, StudyData] =
    design.permuted(perm).map(d => StudyData(d, perm.map(y), perm.map(v), perm.map(nu), b0))

/** Why a whole study failed (counted as a study failure, never dropped). */
enum StudyFailure(val message: String):
  case ObservedFit(status: FitStatus) extends StudyFailure(s"observed fit: ${status.message}")
  case RestrictedFit(status: FitStatus) extends StudyFailure(s"restricted null fit: ${status.message}")
  case Hyperparameters(detail: String) extends StudyFailure(s"Smyth fit: $detail")
  case MixedDf extends StudyFailure("B-EB needs all-finite or all-infinite df")

enum StudyDecision:
  case Reject, Retain, Unresolved

/** Bootstrap result for one study and scheme. p in [(1+k)/(B+1), (1+k+f)/(B+1)] (§2):
  * failed draws are never replaced and the denominator is never shrunk.
  */
final case class BootstrapOutcome(
    scheme: Scheme,
    statistic: Double,
    draws: Int,
    exceed: Int,
    failed: Int,
    nearTies: Int,
    tau2Hat: Double,
    tau2World: Double,
    smyth: Option[SmythFit]
):
  def pLower: Double = (1.0 + exceed) / (draws + 1.0)
  def pUpper: Double = (1.0 + exceed + failed) / (draws + 1.0)

  def decision(alpha: Double): StudyDecision =
    if pUpper <= alpha then StudyDecision.Reject
    else if pLower > alpha then StudyDecision.Retain
    else StudyDecision.Unresolved

/** The bootstrap of §2 for one study. Allocation happens once per call (work
  * arrays); the B-draw loop itself allocates nothing.
  */
final class BootstrapEngine(val design: ResearchDesign, maxIterations: Int = 200):
  private val n = design.n
  val fitter = new StudyFitter(design, maxIterations)
  private val mean = new Array[Double](n)
  private val yStar = new Array[Double](n)
  private val vStar = new Array[Double](n)
  private val zu = new Array[Double](n)
  private val ze = new Array[Double](n)
  private val chi = new Array[Double](n)
  private val post = new Array[Double](n)
  private val zeroTau = TauPolicy.Fixed(0.0)

  /** Relative band inside which |T*| and |T| are flagged as a near tie (§7.5). */
  val TieBand = 1e-7

  /** The df each scheme's chi lane uses: nu_i, or n - p for the nu-swap control. */
  def chiDf(study: StudyData, scheme: Scheme): Array[Double] =
    if scheme == Scheme.NuSwap then Array.fill(n)(design.residualDf.toDouble) else study.nu.clone()

  /** Posterior chi-square df d0 + nu_i for B-EB (+inf when degenerate), else +inf. */
  def posteriorDf(study: StudyData, scheme: Scheme, smyth: Option[SmythFit]): Array[Double] =
    (scheme, smyth) match
      case (Scheme.EmpiricalBayes, Some(fit)) if fit.d0.isFinite => study.nu.map(nu => fit.d0 + nu)
      case _ => Array.fill(n)(Double.PositiveInfinity)

  /** The Smyth fit B-EB would use for this study (None when every df is infinite). */
  def hyperparameters(study: StudyData): Either[StudyFailure, Option[SmythFit]] =
    if study.nu.forall(_.isInfinite) then Right(None)
    else if study.nu.exists(_.isInfinite) then Left(StudyFailure.MixedDf)
    else SmythFit.fit(study.v, study.nu).left.map(StudyFailure.Hyperparameters.apply).map(Some(_))

  /** Runs `draws` bootstrap draws. `variates` receives (chiDf, postDf) and returns the source.
    * If `sink` is given, T*_b is written to sink(b) (NaN for a failed draw).
    */
  def run(
      study: StudyData,
      scheme: Scheme,
      draws: Int,
      variates: (Array[Double], Array[Double]) => DrawVariates,
      statistic: StatisticKind = StatisticKind.PmMkh,
      sink: Option[Array[Double]] = None
  ): Either[StudyFailure, BootstrapOutcome] =
    require(study.design eq design, "study must use this engine's design")
    require(draws >= 1, "at least one draw")
    val observedPolicy = if statistic == StatisticKind.KnownVarianceZ then zeroTau else TauPolicy.PauleMandel
    val observed = fitter.fitFull(study.y, study.v, observedPolicy)
    val t =
      if !observed.ok then Double.NaN
      else if statistic == StatisticKind.KnownVarianceZ then fitter.waldStatistic(study.b0)
      else fitter.mkhStatistic(study.b0)
    if !observed.ok then Left(StudyFailure.ObservedFit(observed))
    else if !t.isFinite then Left(StudyFailure.ObservedFit(FitStatus.NonFiniteResult))
    else
      val tau2Hat = fitter.full.tau2
      val estimateHat = fitter.estimate
      val world: Either[StudyFailure, Double] =
        if scheme.restricted then
          val status = fitter.fitRestricted(study.y, study.v, study.b0, observedPolicy, mean)
          if status.ok then Right(fitter.restricted.tau2) else Left(StudyFailure.RestrictedFit(status))
        else
          var i = 0
          while i < n do
            mean(i) = study.y(i) - fitter.full.residual(i)
            i += 1
          Right(tau2Hat)
      val smyth = if scheme == Scheme.EmpiricalBayes then hyperparameters(study) else Right(None)
      for
        tau2World <- world
        eb <- smyth
      yield
        val source = variates(chiDf(study, scheme), posteriorDf(study, scheme, eb))
        val frozen = TauPolicy.Fixed(tau2Hat)
        val drawPolicy = statistic match
          case StatisticKind.KnownVarianceZ => zeroTau
          case StatisticKind.PmMkh => if scheme == Scheme.KnownVFrozenTau then frozen else TauPolicy.PauleMandel
        val tauU = if scheme == Scheme.OmitU then 0.0 else math.sqrt(tau2World)
        val absT = math.abs(t)
        val record = sink.isDefined
        val out = sink.getOrElse(Array.emptyDoubleArray)
        var exceed = 0
        var failed = 0
        var ties = 0
        var b = 0
        while b < draws do
          source.fill(b, zu, ze, chi, post)
          var i = 0
          while i < n do
            val v = study.v(i)
            val sigma2 = scheme match
              case Scheme.EmpiricalBayes =>
                eb match
                  case Some(fit) => fit.posteriorDraw(v, study.nu(i), post(i))
                  case None => v
              case _ => v
            yStar(i) = mean(i) + tauU * zu(i) + math.sqrt(sigma2) * ze(i)
            vStar(i) = scheme match
              case Scheme.FixV | Scheme.KnownVFrozenTau => v
              case _ => sigma2 * chi(i)
            i += 1
          val status = fitter.fitFull(yStar, vStar, drawPolicy)
          if !status.ok then
            failed += 1
            if record then out(b) = Double.NaN
          else
            val tStar = statistic match
              case StatisticKind.KnownVarianceZ => fitter.waldStatistic(study.b0)
              case StatisticKind.PmMkh =>
                scheme match
                  case Scheme.UnrestrictedRecentred => fitter.mkhStatistic(estimateHat)
                  case Scheme.KnownVFrozenTau => fitter.waldStatistic(study.b0)
                  case _ => fitter.mkhStatistic(study.b0)
            if !tStar.isFinite then
              failed += 1
              if record then out(b) = Double.NaN
            else
              val absStar = math.abs(tStar)
              if absStar >= absT then exceed += 1
              if math.abs(absStar - absT) <= TieBand * absT then ties += 1
              if record then out(b) = tStar
          b += 1
        BootstrapOutcome(scheme, t, draws, exceed, failed, ties, tau2Hat, tau2World, eb)

object BootstrapEngine:
  /** A variates factory reading the keyed Bootstrap stream. */
  def streamVariates(key: StreamKey): (Array[Double], Array[Double]) => DrawVariates =
    (chiDf, postDf) => new StreamDrawVariates(key, chiDf, postDf)

  /** A factory that ignores the df arguments and returns fixed pre-drawn variates. */
  def fixedVariates(variates: MatrixDrawVariates): (Array[Double], Array[Double]) => DrawVariates =
    (_, _) => variates
