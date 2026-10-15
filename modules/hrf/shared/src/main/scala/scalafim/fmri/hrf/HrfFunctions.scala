package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.Vec
// Pure Scala log-gamma via Lanczos approximation (JS-friendly)

object HrfFunctions:

  private val lanczosCoeffs: Array[Double] = Array(
    0.99999999999980993,
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716e-6,
    1.5056327351493116e-7
  )

  private val lanczosG = 7.0

  private[hrf] def logGamma(z: Double): Double =
    if z < 0.5 then
      math.log(math.Pi) - math.log(math.sin(math.Pi * z)) - logGamma(1.0 - z)
    else
      val z1 = z - 1.0
      var x = lanczosCoeffs(0)
      var i = 1
      while i < lanczosCoeffs.length do
        x += lanczosCoeffs(i) / (z1 + i.toDouble)
        i += 1
      val t = z1 + lanczosG + 0.5
      0.5 * math.log(2.0 * math.Pi) + (z1 + 0.5) * math.log(t) - t + math.log(x)

  def time(lag: Lag, maxt: Seconds = 22.s): Double =
    val x = lag.value
    if x > 0.0 && x < maxt.value then x else 0.0

  def ident(lag: Lag): Double =
    if lag.value == 0.0 then 1.0 else 0.0

  def gammaPdf(lag: Lag, shape: Double = 6.0, rate: Double = 1.0): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else if x == 0.0 then
      if shape == 1.0 then rate
      else if shape > 1.0 then 0.0
      else Double.PositiveInfinity
    else
      val logp =
        shape * math.log(rate) +
          (shape - 1.0) * math.log(x) -
          rate * x -
          logGamma(shape)
      math.exp(logp)

  def gaussianPdf(lag: Lag, mean: Double = 6.0, sd: Double = 2.0): Double =
    val x = lag.value
    val z = (x - mean) / sd
    val norm = 1.0 / (sd * math.sqrt(2.0 * math.Pi))
    norm * math.exp(-0.5 * z * z)

  def mexhat(lag: Lag, mean: Double = 6.0, sd: Double = 2.0): Double =
    val t0 = lag.value - mean
    val a = (1.0 - (t0 / sd) * (t0 / sd)) * math.exp(-(t0 * t0) / (2.0 * sd * sd))
    val scale = math.sqrt(2.0 / (3.0 * sd * math.pow(math.Pi, 0.25)))
    scale * a

  private[hrf] val spmg1C = 1.0 / (6.0 * 1307674368000.0)

  def spmg1(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else math.exp(-x) * (A1 * math.pow(x, P1) - spmg1C * math.pow(x, P2))

  def spmg1Deriv(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else
      val term1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val term2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)
      math.exp(-x) * (term1 - term2)

  def spmg1SecondDeriv(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else
      val d1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val d2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)

      val d1p = A1 * ((P1 - 1.0) * math.pow(x, P1 - 2.0) * (P1 - x) - math.pow(x, P1 - 1.0))
      val d2p = spmg1C * ((P2 - 1.0) * math.pow(x, P2 - 2.0) * (P2 - x) - math.pow(x, P2 - 1.0))

      math.exp(-x) * (d1p - d2p - (d1 - d2))

  /** Raw SPM-sign dispersion difference, with fixed positive-component mean
    * and mass. The undershoot is unchanged and cancels. Scaling and
    * orthogonalization are separate from this continuous kernel.
    */
  def spmg1DispersionDeriv(lag: Lag, P1: Double = 5.0, A1: Double = 1.0 / 120.0): Double =
    val mass = A1 * math.exp(logGamma(P1 + 1.0))
    mass * (gammaPdf(lag, P1 + 1.0, 1.0) - gammaPdf(lag, (P1 + 1.0) / 1.01, 1.0 / 1.01)) / 0.01

  /** Time derivative of the dispersion column. */
  def spmg1DispersionTimeDeriv(lag: Lag, P1: Double = 5.0, A1: Double = 1.0 / 120.0): Double =
    val x = lag.value
    if x <= 0.0 then 0.0
    else
      val a = P1 + 1.0
      val d = 1.01
      val first = gammaPdf(lag, a, 1.0) * ((a - 1.0) / x - 1.0)
      val second = gammaPdf(lag, a / d, 1.0 / d) * ((a / d - 1.0) / x - 1.0 / d)
      A1 * math.exp(logGamma(a)) * (first - second) / 0.01

  def sineBasis(lag: Lag, span: Seconds = 24.s, nBasis: Int = 5): Array[Double] =
    val x = lag.value
    val w = span.value
    Array.tabulate(nBasis)(n => math.sin(2.0 * math.Pi * (n + 1) * x / w))

  def fourierBasis(lag: Lag, span: Seconds = 24.s, nBasis: Int = 5): Array[Double] =
    val x = lag.value
    val w = span.value
    val freqs = Array.tabulate(nBasis)(k => (k / 2) + 1)
    Array.tabulate(nBasis) { k =>
      val n = freqs(k)
      if (k % 2 == 0) math.sin(2.0 * math.Pi * n * x / w)
      else math.cos(2.0 * math.Pi * n * x / w)
    }

  /** @param shift
    *   delays the whole kernel; it is a width, not a displacement from an onset.
    */
  def invLogit(lag: Lag, mu1: Double = 6.0, s1: Double = 1.0, mu2: Double = 16.0, s2: Double = 1.0, shift: Seconds = 0.0.s): Double =
    val x = lag.rewound(shift).value
    val inv1 = 1.0 / (1.0 + math.exp(-(x - mu1) / s1))
    val inv2 = 1.0 / (1.0 + math.exp(-(x - mu2) / s2))
    inv1 - inv2

  def halfCosine(
      lag: Lag,
      h1: Seconds = 1.s,
      h2: Seconds = 5.s,
      h3: Seconds = 7.s,
      h4: Seconds = 7.s,
      f1: Double = 0.0,
      f2: Double = 0.0
  ): Double =
    val x = lag.value
    val t1 = h1.value
    val t2 = t1 + h2.value
    val t3 = t2 + h3.value
    val t4 = t3 + h4.value

    if x < 0.0 || x > t4 then 0.0
    else
      def trans(tt: Double, a: Double, b: Double, t0: Double, w: Double): Double =
        a + 0.5 * (b - a) * (1.0 - math.cos(math.Pi * (tt - t0) / w))

      if x <= t1 then trans(x, 0.0, f1, 0.0, h1.value)
      else if x <= t2 then trans(x, f1, 1.0, t1, h2.value)
      else if x <= t3 then trans(x, 1.0, f2, t2, h3.value)
      else trans(x, f2, 0.0, t3, h4.value)

  enum LwuNormalize:
    case None, Height, Area

  def lwu(
      lag: Lag,
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: LwuNormalize = LwuNormalize.None
  ): Double =
    val x = lag.value
    val term1 = math.exp(-math.pow(x - tau, 2.0) / (2.0 * sigma * sigma))
    val term2 =
      rho * math.exp(-math.pow(x - (tau + 2.0 * sigma), 2.0) / (2.0 * math.pow(1.6 * sigma, 2.0)))
    val resp = term1 - term2
    normalize match
      case LwuNormalize.Height =>
        val maxAbs = math.abs(resp)
        if maxAbs > 1e-10 then resp / maxAbs else resp
      case _ => resp

  /** Vectorised LWU HRF with R-like normalization semantics.
    *
    * When `normalize = Height`, the response is scaled so that
    * `max(abs(response)) == 1` over the provided `times`.
    * `Area` normalization is currently treated as `None` (matching the R package).
    */
  def lwuSeries(
      lags: Seq[Lag],
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: LwuNormalize = LwuNormalize.None
  ): Array[Double] =
    val raw = lags.map(l => lwu(l, tau, sigma, rho, LwuNormalize.None)).toArray
    normalize match
      case LwuNormalize.Height =>
        val m = raw.map(math.abs).maxOption.getOrElse(1.0)
        if m > 1e-10 then raw.map(_ / m) else raw
      case LwuNormalize.Area =>
        raw
      case LwuNormalize.None =>
        raw

  def daguerreBasis(lag: Lag, nBasis: Int = 3, scale: Double = 1.0): Array[Double] =
    val x = lag.value / scale
    val basis = Array.fill(nBasis)(0.0)
    if nBasis >= 1 then basis(0) = math.exp(-x / 2.0)
    if nBasis >= 2 then basis(1) = (1.0 - x) * math.exp(-x / 2.0)
    var n = 2
    while n < nBasis do
      val k = n.toDouble
      basis(n) = ((2.0 * k - 1.0 - x) * basis(n - 1) - (k - 1.0) * basis(n - 2)) / k
      n += 1
    basis

  /** Cox–de Boor B-spline basis at a point for an arbitrary nondecreasing knot vector.
    *
    * `knots` is the full knot sequence (including repeated boundary knots).
    * The number of basis functions is `knots.length - degree - 1`.
    */
  private def bsplineAt(x: Double, knots: Array[Double], degree: Int): Array[Double] =
    val nBasis = knots.length - degree - 1
    val n0 = new Array[Double](nBasis)
    var i = 0
    while i < nBasis do
      val k0 = knots(i)
      val k1 = knots(i + 1)
      n0(i) =
        if (x >= k0 && x < k1) 1.0
        else if (x == knots.last && i == nBasis - 1) 1.0
        else 0.0
      i += 1
    var p = 1
    var prev = n0
    while p <= degree do
      val next = new Array[Double](nBasis)
      i = 0
      while i < nBasis do
        val leftDen = knots(i + p) - knots(i)
        val rightDen = knots(i + p + 1) - knots(i + 1)
        val left =
          if leftDen == 0.0 then 0.0
          else (x - knots(i)) / leftDen * prev(i)
        val right =
          if rightDen == 0.0 || i + 1 >= nBasis then 0.0
          else (knots(i + p + 1) - x) / rightDen * prev(i + 1)
        next(i) = left + right
        i += 1
      prev = next
      p += 1
    prev

  /** With [[Hrfs.BsplineConvention.Complete]], a complete clamped B-spline
    * basis with uniformly spaced knots over the actual span and zero outside
    * support. [[Hrfs.BsplineConvention.EndpointAnchored]] drops both boundary
    * columns of a complete basis of width `nBasis + 2`, matching fmrihrf
    * commit `18d418f`. With [[Hrfs.BsplineConvention.LegacyR]], the R-parity B-spline
    * basis used by `hrf_bspline` / `splines::bs` in fmrihrf 0.4.0:
    *
    * - Interior knots are equally spaced quantiles of `seq(0, span)` (step 1),
    *   i.e. on `[0, floor(span)]`.
    * - Boundary knots are `0` and `span`, each repeated `degree + 1` times.
    * - The intercept column from `splineDesign` is dropped (so columns = max(nBasis, degree+1)).
    * - Times outside `[0, span]` are clamped to `0` (matching R wrapper).
    */
  def bsplineBasis(lag: Lag, span: Seconds = 24.s, nBasis: Int = 5, degree: Int = 3,
      convention: Hrfs.BsplineConvention = Hrfs.BsplineConvention.LegacyR): Array[Double] =
    convention match
      case Hrfs.BsplineConvention.LegacyR => legacyBsplineBasis(lag, span, nBasis, degree)
      case Hrfs.BsplineConvention.Complete => completeBsplineBasis(lag, span, nBasis, degree)
      case Hrfs.BsplineConvention.EndpointAnchored =>
        val full = completeBsplineBasis(lag, span, endpointAnchoredWidth(nBasis, degree), degree)
        full.slice(1, full.length - 1)

  private[hrf] def endpointAnchoredWidth(nBasis: Int, degree: Int): Int =
    require(degree >= 1, "Endpoint-anchored B-spline degree must be positive")
    require(nBasis >= math.max(1, degree - 1),
      s"Endpoint-anchored B-spline basis count must be at least ${math.max(1, degree - 1)} for degree $degree")
    require(nBasis <= Int.MaxValue - 2, "Endpoint-anchored B-spline basis count is too large")
    nBasis + 2

  private def completeBsplineBasis(lag: Lag, span: Seconds, nBasis: Int, degree: Int): Array[Double] =
    require(degree >= 0, "B-spline degree must be nonnegative")
    require(nBasis > 0, "B-spline basis count must be positive")
    require(span.value > 0.0 && span.value.isFinite, "B-spline span must be positive and finite")
    val width = math.max(nBasis, degree + 1)
    if lag.value < 0.0 || lag.value > span.value then new Array[Double](width)
    else
      val breaks = bsplineBreaks(span, nBasis, degree, Hrfs.BsplineConvention.Complete).map(_.value)
      val knots = Array.fill(degree + 1)(0.0) ++ breaks.slice(1, breaks.length - 1) ++
        Array.fill(degree + 1)(span.value)
      bsplineAt(lag.value, knots, degree)

  private def legacyBsplineBasis(lag: Lag, span: Seconds, nBasis: Int, degree: Int): Array[Double] =
    val w = span.value
    val x0 = lag.value
    val x = if x0 < 0.0 || x0 > w then 0.0 else x0
    val ord = degree + 1
    val nIknots0 = nBasis - ord + 1 // = nBasis - degree
    val nIknots = if nIknots0 < 0 then 0 else nIknots0

    val internalKnots: Array[Double] =
      if nIknots == 0 then Array(0.0)
      else
        val m = math.floor(w)
        val denom = nIknots.toDouble + 1.0
        Array.tabulate(nIknots)(i => m * (i + 1).toDouble / denom)

    val fullKnots =
      Array.fill(ord)(0.0) ++ internalKnots ++ Array.fill(ord)(w)

    val basisWithIntercept = bsplineAt(x, fullKnots, degree)
    // drop intercept column to match splines::bs(intercept = FALSE)
    basisWithIntercept.drop(1)

  /** Distinct knot positions of [[bsplineBasis]] on `[0, span]`.
    *
    * The basis is a polynomial of `degree` between consecutive entries, which
    * is what lets it be integrated exactly piece by piece. Kept next to
    * `bsplineBasis` so the two knot computations cannot drift apart.
    */
  private[hrf] def bsplineBreaks(span: Seconds, nBasis: Int, degree: Int,
      convention: Hrfs.BsplineConvention = Hrfs.BsplineConvention.LegacyR): Vector[Seconds] =
    convention match
      case Hrfs.BsplineConvention.Complete =>
        val intervals = math.max(nBasis, degree + 1) - degree
        Vector.tabulate(intervals + 1)(i => Seconds(span.value * i.toDouble / intervals.toDouble))
      case Hrfs.BsplineConvention.LegacyR => legacyBsplineBreaks(span, nBasis, degree)
      case Hrfs.BsplineConvention.EndpointAnchored =>
        bsplineBreaks(span, endpointAnchoredWidth(nBasis, degree), degree, Hrfs.BsplineConvention.Complete)

  private def legacyBsplineBreaks(span: Seconds, nBasis: Int, degree: Int): Vector[Seconds] =
    val w = span.value
    val ord = degree + 1
    val nIknots0 = nBasis - ord + 1
    val nIknots = if nIknots0 < 0 then 0 else nIknots0
    val internal =
      if nIknots == 0 then Vector.empty[Double]
      else
        val m = math.floor(w)
        val denom = nIknots.toDouble + 1.0
        Vector.tabulate(nIknots)(i => m * (i + 1).toDouble / denom)
    ((0.0 +: internal) :+ w)
      .filter(x => x >= 0.0 && x <= w)
      .distinct
      .sorted
      .map(Seconds(_))


object Hrfs:

  /** Continuous Cascade34 in its intrinsic positive-component-area convention. */
  def cascade34(params: Cascade34Params = Cascade34Params.Default, span: Seconds = 32.0.s): Hrf =
    Cascade34.toHrf(params, span)

  enum WeightedMethod:
    case Constant, Linear

  /** Knot and column convention of [[bspline]]. */
  enum BsplineConvention:
    /** fmrihrf 0.4.0 `hrf_bspline`: `splines::bs(intercept = FALSE)` with
      * interior knots at quantiles of `seq(0, span)`, i.e. on `[0, floor(span)]`,
      * and the first spline dropped. With interior knots the basis is zero at
      * onset and cannot represent a constant; at the minimum width
      * (`nBasis <= degree + 1`) it is the full Bernstein basis.
      */
    case LegacyR

    /** The full clamped basis: `max(nBasis, degree + 1)` columns, interior
      * knots uniform over the actual span, a partition of unity on
      * `[0, span]` and zero outside it.
      */
    case Complete

    /** fmrihrf `18d418f` `hrf_bspline`: exactly `nBasis` columns from a
      * complete basis of width `nBasis + 2`, with both boundary columns
      * removed. Knots are uniform over the actual span; values are zero at
      * both endpoints and outside support. Requires `degree >= 1` and
      * `nBasis >= max(1, degree - 1)`. Constants are not representable.
      */
    case EndpointAnchored

  def gamma(shape: Double = 6.0, rate: Double = 1.0, span: Seconds = 24.s): Hrf =
    val params = HrfParams.Gamma(shape, rate)
    // The incomplete gamma is only defined for a positive shape and rate;
    // degenerate parameters fall back to quadrature rather than throwing from
    // inside an evaluator.
    val integration =
      if shape > 0.0 && rate > 0.0 && shape.isFinite && rate.isFinite then IntegrationPolicy.Gamma(shape, rate)
      else IntegrationPolicy.Quadrature
    val descriptor = HrfDescriptor.scalar(HrfKind.Gamma, span, params, integration = integration)
    Hrf.of("gamma", nbasis = 1, span = span, descriptor = Some(descriptor)) { t =>
      Vec.unsafe(Array(HrfFunctions.gammaPdf(t, shape, rate)))
    }

  def gaussian(mean: Double = 6.0, sd: Double = 2.0, span: Seconds = 24.s): Hrf =
    val params = HrfParams.Gaussian(mean, sd)
    val integration =
      if sd > 0.0 && sd.isFinite && mean.isFinite then IntegrationPolicy.Gaussian(mean, sd)
      else IntegrationPolicy.Quadrature
    val descriptor = HrfDescriptor.scalar(HrfKind.Gaussian, span, params, integration = integration)
    Hrf.of("gaussian", nbasis = 1, span = span, descriptor = Some(descriptor)) { t =>
      Vec.unsafe(Array(HrfFunctions.gaussianPdf(t, mean, sd)))
    }

  def spmg1(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0, span: Seconds = 24.s): Hrf =
    val params = SpmgParams(P1, P2, A1)
    Hrf.of("SPMG1", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.spmg(HrfKind.Spmg1, 1, span, params))) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1(t, P1, P2, A1)))
    }

  def spmg1TemporalDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0, span: Seconds = 24.s): Hrf =
    val params = SpmgParams(P1, P2, A1)
    val descriptor = HrfDescriptor.derived(
      "SPMG1_temporal_deriv",
      1,
      span,
      params = HrfParams.Spmg(params),
      integration = IntegrationPolicy.SpmgTemporalDeriv(params)
    )
    Hrf.of("SPMG1_temporal_deriv", nbasis = 1, span = span, descriptor = Some(descriptor)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1Deriv(t, P1, P2, A1)))
    }

  def spmg1DispersionDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 1.0 / 120.0, span: Seconds = 24.s): Hrf =
    val params = SpmgParams(P1, P2, A1)
    val descriptor = HrfDescriptor.derived(
      "SPMG1_dispersion_deriv",
      1,
      span,
      params = HrfParams.Spmg(params),
      integration = IntegrationPolicy.SpmgDispersionDeriv(params)
    )
    Hrf.of("SPMG1_dispersion_deriv", nbasis = 1, span = span, descriptor = Some(descriptor)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1DispersionDeriv(t, P1, A1)))
    }

  def mexhat(mean: Double = 6.0, sd: Double = 2.0, span: Seconds = 24.s): Hrf =
    val params = HrfParams.Mexhat(mean, sd)
    Hrf.of("mexhat", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.scalar(HrfKind.Mexhat, span, params))) { t =>
      Vec.unsafe(Array(HrfFunctions.mexhat(t, mean, sd)))
    }

  def invLogit(mu1: Double = 6.0, s1: Double = 1.0, mu2: Double = 16.0, s2: Double = 1.0, lag: Seconds = 0.0.s, span: Seconds = 24.s): Hrf =
    val params = HrfParams.InvLogit(mu1, s1, mu2, s2, lag)
    Hrf.of("inv_logit", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.scalar(HrfKind.InvLogit, span, params))) { t =>
      Vec.unsafe(Array(HrfFunctions.invLogit(t, mu1, s1, mu2, s2, shift = lag)))
    }

  def halfCosine(
      h1: Seconds = 1.s,
      h2: Seconds = 5.s,
      h3: Seconds = 7.s,
      h4: Seconds = 7.s,
      f1: Double = 0.0,
      f2: Double = 0.0
  ): Hrf =
    val spanSec = h1 + h2 + h3 + h4
    val params = HrfParams.HalfCosine(h1, h2, h3, h4, f1, f2)
    Hrf.of(
      "half_cosine",
      nbasis = 1,
      span = spanSec,
      descriptor = Some(HrfDescriptor.scalar(HrfKind.HalfCosine, spanSec, params)),
      support = Support.Compact(spanSec)
    ) { t =>
      Vec.unsafe(Array(HrfFunctions.halfCosine(t, h1, h2, h3, h4, f1, f2)))
    }

  def lwu(
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: HrfFunctions.LwuNormalize = HrfFunctions.LwuNormalize.None,
      span: Seconds = 24.s
  ): Hrf =
    lwuValidated(tau, sigma, rho, normalize, span)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Typed constructor admission; `None` requires no calibration samples. */
  def lwuValidated(
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: HrfFunctions.LwuNormalize = HrfFunctions.LwuNormalize.None,
      span: Seconds = 24.s
  ): Either[HrfConstructorError, Hrf] =
    HrfConstructorCalibration.lwu(tau, sigma, rho, normalize, span).flatMap: nSamples =>
      val raw = (l: Lag) => HrfFunctions.lwu(l, tau, sigma, rho, HrfFunctions.LwuNormalize.None)
      calibrateLwu(raw, normalize, nSamples).map: scale =>
        val params = HrfParams.Lwu(LwuParams(tau, sigma, rho), normalize)
        Hrf.of("lwu", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.scalar(HrfKind.Lwu, span, params))) { t =>
          Vec.unsafe(Array(raw(t) / scale))
        }

  private def calibrateLwu(
      raw: Lag => Double,
      normalize: HrfFunctions.LwuNormalize,
      nSamples: Int
  ): Either[HrfConstructorError, Double] =
    // Resolve both modes once against the causal ceil grid, never a query grid.
    val dt = HrfConstructorCalibration.LwuStep.value
    val factor = normalize match
      case HrfFunctions.LwuNormalize.None => 1.0
      case HrfFunctions.LwuNormalize.Height =>
        var m = 0.0
        var i = 0
        while i < nSamples do
          val value = raw(Lag(i * dt))
          if !value.isFinite then
            return Left(HrfConstructorError.NonFiniteCalibrationValue("lwu", i, 0, value))
          val a = math.abs(value)
          if a > m then m = a
          i += 1
        m
      case HrfFunctions.LwuNormalize.Area =>
        var acc = 0.0
        var i = 0
        while i < nSamples do
          val value = raw(Lag(i * dt))
          if !value.isFinite then
            return Left(HrfConstructorError.NonFiniteCalibrationValue("lwu", i, 0, value))
          val w = if i == 0 || i == nSamples - 1 then 0.5 else 1.0
          acc += w * value
          i += 1
        acc * dt
    if !factor.isFinite then
      Left(HrfConstructorError.NonFiniteCalibrationScale("lwu", 0, factor))
    else Right(if math.abs(factor) > 1e-10 then factor else 1.0)

  def boxcar(width: Seconds, amplitude: Double = 1.0, normalize: Boolean = false): Hrf =
    require(width.value.isFinite && width.value > 0.0, "`width` must be > 0")
    require(amplitude.isFinite, "`amplitude` must be finite")
    val amp = if normalize then 1.0 / width.value else amplitude
    val params = HrfParams.Boxcar(width, amplitude, normalize)
    val descriptor = HrfDescriptor.scalar(
      HrfKind.Boxcar,
      width,
      params,
      // `amp`, not `amplitude`: normalization has already been folded in.
      integration = IntegrationPolicy.Boxcar(width, amp)
    )
    Hrf.of(
      s"boxcar[${width.value}]",
      nbasis = 1,
      span = width,
      descriptor = Some(descriptor),
      support = Support.Compact(width)
    ) { t =>
      val x = t.value
      Vec.unsafe(Array(if x >= 0.0 && x < width.value then amp else 0.0))
    }

  /** Validated twin of [[weighted]]; the only constructor here that had none. */
  def weightedValidated(
      weights: Vector[Double],
      width: Option[Seconds] = None,
      times: Option[Vector[Seconds]] = None,
      method: WeightedMethod = WeightedMethod.Constant,
      normalize: Boolean = false
  ): Either[SampledProfileError, Hrf] =
    weightedProfile(weights, width, times).map(weightedFrom(_, method, normalize))

  private def weightedProfile(
      weights: Vector[Double],
      width: Option[Seconds],
      times: Option[Vector[Seconds]]
  ): Either[SampledProfileError, WeightedProfile] =
    times match
      case Some(tvec) => WeightedProfile.fromExplicit(weights, tvec)
      case None =>
        width match
          case Some(w) => WeightedProfile.fromUniform(weights, w)
          case None    => Left(SampledProfileError.TooShort("width or times", 1, 0))

  def weighted(
      weights: Vector[Double],
      width: Option[Seconds] = None,
      times: Option[Vector[Seconds]] = None,
      method: WeightedMethod = WeightedMethod.Constant,
      normalize: Boolean = false
  ): Hrf =
    weightedValidated(weights, width, times, method, normalize)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private def weightedFrom(
      profile0: WeightedProfile,
      method: WeightedMethod,
      normalize: Boolean
  ): Hrf =

    val timesD = profile0.times.toVector.map(_.value).toArray
    var ws = profile0.weights.toArray

    if normalize then
      method match
        case WeightedMethod.Constant =>
          val sum = ws.slice(0, ws.length - 1).sum
          if math.abs(sum) > 1e-10 then ws = ws.map(_ / sum)
        case WeightedMethod.Linear =>
          val intervals = timesD.sliding(2).map { case Array(a, b) => b - a }.toArray
          val avgWs = ws.sliding(2).map { case Array(a, b) => (a + b) / 2.0 }.toArray
          val integral = intervals.zip(avgWs).map(_ * _).sum
          if math.abs(integral) > 1e-10 then ws = ws.map(_ / integral)

    def findInterval(x: Double): Int =
      var lo = 0
      var hi = timesD.length - 2
      while lo <= hi do
        val mid = (lo + hi) >>> 1
        if x < timesD(mid) then hi = mid - 1
        else if x >= timesD(mid + 1) then lo = mid + 1
        else return mid
      math.max(0, math.min(timesD.length - 2, hi))

    val span = Seconds(timesD.last)
    val params = HrfParams.Weighted(profile0, method, normalize)

    Hrf.of(
      "weighted",
      nbasis = 1,
      span = span,
      descriptor = Some(HrfDescriptor.scalar(HrfKind.Weighted, span, params)),
      support = Support.Compact(span)
    ) { t =>
      val x = t.value
      val v =
        if x < timesD.head || x > timesD.last then 0.0
        else if x == timesD.last then ws.last
        else
          val i = findInterval(x)
          method match
            case WeightedMethod.Constant =>
              ws(i)
            case WeightedMethod.Linear =>
              val t0 = timesD(i)
              val t1 = timesD(i + 1)
              val w0 = ws(i)
              val w1 = ws(i + 1)
              val a = (x - t0) / (t1 - t0)
              w0 + a * (w1 - w0)
      Vec.unsafe(Array(v))
    }

  def empirical(
      times: Seq[Seconds],
      values: Seq[Double],
      name: String = "empirical_hrf"
  ): Hrf =
    val curve = SampledCurve
      .fromUnsorted(times.toVector, values.toVector)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
    val ts = curve.times.toVector.map(_.value).toArray
    val ys = curve.values.toArray
    val span = curve.span

    def interp(x: Double): Double =
      if x < ts.head || x > ts.last then 0.0
      else if x == ts.last then ys.last
      else
        var lo = 0
        var hi = ts.length - 2
        while lo <= hi do
          val mid = (lo + hi) >>> 1
          if x < ts(mid) then hi = mid - 1
          else if x >= ts(mid + 1) then lo = mid + 1
          else
            val t0 = ts(mid)
            val t1 = ts(mid + 1)
            val y0 = ys(mid)
            val y1 = ys(mid + 1)
            val a = (x - t0) / (t1 - t0)
            return y0 + a * (y1 - y0)
        ys.last

    val descriptor = HrfDescriptor.custom(name, 1, span, HrfParams.Empirical(curve))
    Hrf.scalar(name, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      interp(t.value)
    }

  def sine(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    val basis = BasisCount(nBasis)
    val descriptor = HrfDescriptor.known(HrfKind.Sine, basis.value, span, HrfParams.Sine(basis))
    Hrf.of("sine", nbasis = basis.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      Vec.unsafe(HrfFunctions.sineBasis(t, span, nBasis))
    }

  def fourier(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    val basis = BasisCount(nBasis)
    val descriptor = HrfDescriptor.known(HrfKind.Fourier, basis.value, span, HrfParams.Fourier(basis), penalty = PenaltyPolicy.FourierFrequency)
    Hrf.of("fourier", nbasis = basis.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      Vec.unsafe(HrfFunctions.fourierBasis(t, span, nBasis))
    }

  def daguerre(nBasis: Int = 3, scale: Double = 4.0, span: Seconds = 24.s): Hrf =
    daguerreValidated(nBasis, scale, span)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Typed admission for the fixed peak-calibration grid and basis work. */
  def daguerreValidated(nBasis: Int = 3, scale: Double = 4.0, span: Seconds = 24.s): Either[HrfConstructorError, Hrf] =
    HrfConstructorCalibration.daguerre(nBasis, scale, span).flatMap: nSamples =>
      val basis = BasisCount(nBasis)
      calibrateDaguerre(basis.value, scale, nSamples).map: scales =>
        val descriptor = HrfDescriptor.known(HrfKind.Daguerre, basis.value, span, HrfParams.Daguerre(basis, scale), penalty = PenaltyPolicy.DaguerreDecay)
        Hrf.of("daguerre", nbasis = basis.value, span = span, descriptor = Some(descriptor)) { t =>
          val raw = HrfFunctions.daguerreBasis(t, basis.value, scale)
          val normed = raw.zip(scales).map(_ / _)
          Vec.unsafe(normed)
        }

  private def calibrateDaguerre(
      nBasis: Int,
      scale: Double,
      nSamples: Int
  ): Either[HrfConstructorError, Array[Double]] =
    val dt = HrfConstructorCalibration.DaguerreStep.value
    val maxAbs = Array.fill(nBasis)(0.0)
    var i = 0
    while i < nSamples do
      val raw = HrfFunctions.daguerreBasis(Lag(i * dt), nBasis, scale)
      var j = 0
      while j < nBasis do
        val value = raw(j)
        if !value.isFinite then
          return Left(HrfConstructorError.NonFiniteCalibrationValue("daguerre", i, j, value))
        val a = math.abs(value)
        if a > maxAbs(j) then maxAbs(j) = a
        j += 1
      i += 1
    Right(maxAbs.map(m => if m > 1e-10 then m else 1.0))

  def fir(nBasis: Int = 12, span: Seconds = 24.s): Hrf =
    val basis = BasisCount(nBasis)
    val binWidth = span.value / basis.value.toDouble
    // Piecewise constant, with a jump at every bin edge.
    val firBreaks = Vector.tabulate(basis.value + 1)(i => Seconds(i * binWidth))
    val descriptor = HrfDescriptor.known(
      HrfKind.Fir,
      basis.value,
      span,
      HrfParams.Fir(basis),
      penalty = PenaltyPolicy.Roughness,
      integration = IntegrationPolicy.PiecewisePolynomial(firBreaks, degree = 0)
    )
    Hrf.of("fir", nbasis = basis.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      val x = t.value
      val out = Array.fill(basis.value)(0.0)
      if x >= 0.0 && x < span.value then
        val idx = if x == 0.0 then 0 else math.floor(x / binWidth).toInt
        out(math.min(idx, basis.value - 1)) = 1.0
      Vec.unsafe(out)
    }

  /** B-spline response basis. Use [[BsplineConvention.Complete]] for a complete,
    * partition-of-unity basis that can represent a constant on `[0, span]`.
    * This mode returns `max(nBasis, degree + 1)` columns and spaces interior
    * knots uniformly over the actual span, including noninteger spans.
    *
    * [[BsplineConvention.EndpointAnchored]] matches fmrihrf commit `18d418f`:
    * exactly `nBasis` columns, zero at both endpoints, with uniform knots over
    * the actual span. Requires `degree >= 1` and `nBasis >= max(1, degree - 1)`.
    *
    * The default, [[BsplineConvention.LegacyR]], is frozen to fmrihrf 0.4.0. With
    * interior knots it omits the first spline, forces zero at onset and cannot
    * represent a constant. No convention applies column normalization.
    */
  def bspline(nBasis: Int = 5, span: Seconds = 24.s, degree: Int = 3,
      convention: BsplineConvention = BsplineConvention.LegacyR): Hrf =
    require(degree >= 0, "B-spline degree must be nonnegative")
    require(span.value > 0.0 && span.value.isFinite, "B-spline span must be positive and finite")
    val requested = BasisCount(nBasis)
    val ord = degree + 1
    val effective = convention match
      case BsplineConvention.EndpointAnchored =>
        HrfFunctions.endpointAnchoredWidth(requested.value, degree)
        requested
      case BsplineConvention.LegacyR | BsplineConvention.Complete =>
        BasisCount(math.max(requested.value, ord))
    val descriptor = HrfDescriptor.known(
      HrfKind.Bspline,
      effective.value,
      span,
      HrfParams.Bspline(requested, degree, convention),
      penalty = PenaltyPolicy.Roughness,
      integration = IntegrationPolicy.PiecewisePolynomial(
        HrfFunctions.bsplineBreaks(span, requested.value, degree, convention),
        degree
      )
    )
    Hrf.of("bspline", nbasis = effective.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      Vec.unsafe(HrfFunctions.bsplineBasis(t, span, requested.value, degree, convention))
    }

  def tent(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    val requested = BasisCount(nBasis)
    val degree = 1
    val ord = degree + 1
    val effective = BasisCount(math.max(requested.value, ord))
    val descriptor = HrfDescriptor.known(
      HrfKind.Tent,
      effective.value,
      span,
      HrfParams.Tent(requested),
      penalty = PenaltyPolicy.Roughness,
      integration = IntegrationPolicy.PiecewisePolynomial(
        HrfFunctions.bsplineBreaks(span, requested.value, degree),
        degree
      )
    )
    Hrf.of("tent", nbasis = effective.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      Vec.unsafe(HrfFunctions.bsplineBasis(t, span, requested.value, degree = degree))
    }

  val Gamma: Hrf = gamma()
  val Gaussian: Hrf = gaussian()
  val SPMG1: Hrf = spmg1()
  /** Raw informed basis; no implicit orthogonalization or equal-norm scaling. */
  val SPMG2: Hrf =
    HrfCombinators.bindBasis(
      Seq(spmg1(), spmg1TemporalDeriv()),
      name = Some("SPMG2")
    )

  /** Canonical, analytic time derivative, and response dispersion difference. */
  val SPMG3: Hrf =
    HrfCombinators.bindBasis(
      Seq(spmg1(), spmg1TemporalDeriv(), spmg1DispersionDeriv()),
      name = Some("SPMG3")
    )
