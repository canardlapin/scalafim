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

  private[hrf] val spmg1C = 1.274527e-13

  def spmg1(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else math.exp(-x) * (A1 * math.pow(x, P1) - spmg1C * math.pow(x, P2))

  def spmg1Deriv(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else
      val term1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val term2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)
      math.exp(-x) * (term1 - term2)

  def spmg1SecondDeriv(lag: Lag, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = lag.value
    if x < 0.0 then 0.0
    else
      val d1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val d2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)

      val d1p = A1 * ((P1 - 1.0) * math.pow(x, P1 - 2.0) * (P1 - x) - math.pow(x, P1 - 1.0))
      val d2p = spmg1C * ((P2 - 1.0) * math.pow(x, P2 - 2.0) * (P2 - x) - math.pow(x, P2 - 1.0))

      math.exp(-x) * (d1p - d2p - (d1 - d2))

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

  /** R-parity B-spline basis used by `hrf_bspline` / `splines::bs` in the original package.
    *
    * - Interior knots are equally spaced quantiles of `seq(0, span)` (step 1),
    *   i.e. on `[0, floor(span)]`.
    * - Boundary knots are `0` and `span`, each repeated `degree + 1` times.
    * - The intercept column from `splineDesign` is dropped (so columns = max(nBasis, degree+1)).
    * - Times outside `[0, span]` are clamped to `0` (matching R wrapper).
    */
  def bsplineBasis(lag: Lag, span: Seconds = 24.s, nBasis: Int = 5, degree: Int = 3): Array[Double] =
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
  private[hrf] def bsplineBreaks(span: Seconds, nBasis: Int, degree: Int): Vector[Seconds] =
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

  def spmg1(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
    val params = SpmgParams(P1, P2, A1)
    Hrf.of("SPMG1", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.spmg(HrfKind.Spmg1, 1, span, params))) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1(t, P1, P2, A1)))
    }

  def spmg1TemporalDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
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

  def spmg1DispersionDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
    val params = SpmgParams(P1, P2, A1)
    val descriptor = HrfDescriptor.derived(
      "SPMG1_dispersion_deriv",
      1,
      span,
      params = HrfParams.Spmg(params),
      integration = IntegrationPolicy.SpmgDispersionDeriv(params)
    )
    Hrf.of("SPMG1_dispersion_deriv", nbasis = 1, span = span, descriptor = Some(descriptor)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1SecondDeriv(t, P1, P2, A1)))
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
    val raw = (l: Lag) => HrfFunctions.lwu(l, tau, sigma, rho, HrfFunctions.LwuNormalize.None)
    // Both normalizations are properties of the whole causal kernel, not of a
    // single lag, so they are resolved once here against `[0, span]` rather
    // than against whatever grid a caller later supplies.
    val dt = 0.005
    val nSamples = math.ceil(span.value / dt).toInt + 1
    val scale =
      normalize match
        case HrfFunctions.LwuNormalize.None => 1.0
        case HrfFunctions.LwuNormalize.Height =>
          var m = 0.0
          var i = 0
          while i < nSamples do
            val a = math.abs(raw(Lag(i * dt)))
            if a > m then m = a
            i += 1
          if m > 1e-10 then m else 1.0
        case HrfFunctions.LwuNormalize.Area =>
          // Trapezoid over the causal support; `Area` used to be a silent
          // no-op, so an area-normalized LWU was quietly unnormalized.
          var acc = 0.0
          var i = 0
          while i < nSamples do
            val w = if i == 0 || i == nSamples - 1 then 0.5 else 1.0
            acc += w * raw(Lag(i * dt))
            i += 1
          val area = acc * dt
          if math.abs(area) > 1e-10 then area else 1.0
    val params = HrfParams.Lwu(LwuParams(tau, sigma, rho), normalize)
    Hrf.of("lwu", nbasis = 1, span = span, descriptor = Some(HrfDescriptor.scalar(HrfKind.Lwu, span, params))) { t =>
      Vec.unsafe(Array(raw(t) / scale))
    }

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
    val basis = BasisCount(nBasis)
    val dt = 0.1
    val nSamples = math.ceil(span.value / dt).toInt + 1
    val maxAbs = Array.fill(basis.value)(0.0)
    var i = 0
    while i < nSamples do
      val t = Lag(i * dt)
      val raw = HrfFunctions.daguerreBasis(t, basis.value, scale)
      var j = 0
      while j < basis.value do
        val a = math.abs(raw(j))
        if a > maxAbs(j) then maxAbs(j) = a
        j += 1
      i += 1
    val scales = maxAbs.map(m => if m > 1e-10 then m else 1.0)
    val descriptor = HrfDescriptor.known(HrfKind.Daguerre, basis.value, span, HrfParams.Daguerre(basis, scale), penalty = PenaltyPolicy.DaguerreDecay)
    Hrf.of("daguerre", nbasis = basis.value, span = span, descriptor = Some(descriptor)) { t =>
      val raw = HrfFunctions.daguerreBasis(t, basis.value, scale)
      val normed = raw.zip(scales).map(_ / _)
      Vec.unsafe(normed)
    }

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

  def bspline(nBasis: Int = 5, span: Seconds = 24.s, degree: Int = 3): Hrf =
    val requested = BasisCount(nBasis)
    val ord = degree + 1
    val effective = BasisCount(math.max(requested.value, ord))
    val descriptor = HrfDescriptor.known(
      HrfKind.Bspline,
      effective.value,
      span,
      HrfParams.Bspline(requested, degree),
      penalty = PenaltyPolicy.Roughness,
      integration = IntegrationPolicy.PiecewisePolynomial(
        HrfFunctions.bsplineBreaks(span, requested.value, degree),
        degree
      )
    )
    Hrf.of("bspline", nbasis = effective.value, span = span, descriptor = Some(descriptor), support = Support.Compact(span)) { t =>
      Vec.unsafe(HrfFunctions.bsplineBasis(t, span, requested.value, degree))
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
  val SPMG2: Hrf =
    HrfCombinators.bindBasis(
      Seq(spmg1(), spmg1TemporalDeriv()),
      name = Some("SPMG2")
    )

  val SPMG3: Hrf =
    HrfCombinators.bindBasis(
      Seq(spmg1(), spmg1TemporalDeriv(), spmg1DispersionDeriv()),
      name = Some("SPMG3")
    )
