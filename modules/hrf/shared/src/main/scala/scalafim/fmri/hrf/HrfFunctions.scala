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

  private def logGamma(z: Double): Double =
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

  def time(t: Seconds, maxt: Seconds = 22.s): Double =
    val x = t.value
    if x > 0.0 && x < maxt.value then x else 0.0

  def ident(t: Seconds): Double =
    if t.value == 0.0 then 1.0 else 0.0

  def gammaPdf(t: Seconds, shape: Double = 6.0, rate: Double = 1.0): Double =
    val x = t.value
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

  def gaussianPdf(t: Seconds, mean: Double = 6.0, sd: Double = 2.0): Double =
    val x = t.value
    val z = (x - mean) / sd
    val norm = 1.0 / (sd * math.sqrt(2.0 * math.Pi))
    norm * math.exp(-0.5 * z * z)

  def mexhat(t: Seconds, mean: Double = 6.0, sd: Double = 2.0): Double =
    val t0 = t.value - mean
    val a = (1.0 - (t0 / sd) * (t0 / sd)) * math.exp(-(t0 * t0) / (2.0 * sd * sd))
    val scale = math.sqrt(2.0 / (3.0 * sd * math.pow(math.Pi, 0.25)))
    scale * a

  private val spmg1C = 1.274527e-13

  def spmg1(t: Seconds, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = t.value
    if x < 0.0 then 0.0
    else math.exp(-x) * (A1 * math.pow(x, P1) - spmg1C * math.pow(x, P2))

  def spmg1Deriv(t: Seconds, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = t.value
    if x < 0.0 then 0.0
    else
      val term1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val term2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)
      math.exp(-x) * (term1 - term2)

  def spmg1SecondDeriv(t: Seconds, P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Double =
    val x = t.value
    if x < 0.0 then 0.0
    else
      val d1 = A1 * math.pow(x, P1 - 1.0) * (P1 - x)
      val d2 = spmg1C * math.pow(x, P2 - 1.0) * (P2 - x)

      val d1p = A1 * ((P1 - 1.0) * math.pow(x, P1 - 2.0) * (P1 - x) - math.pow(x, P1 - 1.0))
      val d2p = spmg1C * ((P2 - 1.0) * math.pow(x, P2 - 2.0) * (P2 - x) - math.pow(x, P2 - 1.0))

      math.exp(-x) * (d1p - d2p - (d1 - d2))

  def sineBasis(t: Seconds, span: Seconds = 24.s, nBasis: Int = 5): Array[Double] =
    val x = t.value
    val w = span.value
    Array.tabulate(nBasis)(n => math.sin(2.0 * math.Pi * (n + 1) * x / w))

  def fourierBasis(t: Seconds, span: Seconds = 24.s, nBasis: Int = 5): Array[Double] =
    val x = t.value
    val w = span.value
    val freqs = Array.tabulate(nBasis)(k => (k / 2) + 1)
    Array.tabulate(nBasis) { k =>
      val n = freqs(k)
      if (k % 2 == 0) math.sin(2.0 * math.Pi * n * x / w)
      else math.cos(2.0 * math.Pi * n * x / w)
    }

  def invLogit(t: Seconds, mu1: Double = 6.0, s1: Double = 1.0, mu2: Double = 16.0, s2: Double = 1.0, lag: Seconds = 0.0.s): Double =
    val x = t.value - lag.value
    val inv1 = 1.0 / (1.0 + math.exp(-(x - mu1) / s1))
    val inv2 = 1.0 / (1.0 + math.exp(-(x - mu2) / s2))
    inv1 - inv2

  def halfCosine(
      t: Seconds,
      h1: Seconds = 1.s,
      h2: Seconds = 5.s,
      h3: Seconds = 7.s,
      h4: Seconds = 7.s,
      f1: Double = 0.0,
      f2: Double = 0.0
  ): Double =
    val x = t.value
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
      t: Seconds,
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: LwuNormalize = LwuNormalize.None
  ): Double =
    val x = t.value
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
      times: Seq[Seconds],
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: LwuNormalize = LwuNormalize.None
  ): Array[Double] =
    val raw = times.map(t => lwu(t, tau, sigma, rho, LwuNormalize.None)).toArray
    normalize match
      case LwuNormalize.Height =>
        val m = raw.map(math.abs).maxOption.getOrElse(1.0)
        if m > 1e-10 then raw.map(_ / m) else raw
      case LwuNormalize.Area =>
        raw
      case LwuNormalize.None =>
        raw

  def daguerreBasis(t: Seconds, nBasis: Int = 3, scale: Double = 1.0): Array[Double] =
    val x = t.value / scale
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
  def bsplineBasis(t: Seconds, span: Seconds = 24.s, nBasis: Int = 5, degree: Int = 3): Array[Double] =
    val w = span.value
    val x0 = t.value
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


object Hrfs:

  enum WeightedMethod:
    case Constant, Linear

  def gamma(shape: Double = 6.0, rate: Double = 1.0, span: Seconds = 24.s): Hrf =
    Hrf.of("gamma", nbasis = 1, span = span, params = Map("shape" -> shape, "rate" -> rate)) { t =>
      Vec.unsafe(Array(HrfFunctions.gammaPdf(t, shape, rate)))
    }

  def gaussian(mean: Double = 6.0, sd: Double = 2.0, span: Seconds = 24.s): Hrf =
    Hrf.of("gaussian", nbasis = 1, span = span, params = Map("mean" -> mean, "sd" -> sd)) { t =>
      Vec.unsafe(Array(HrfFunctions.gaussianPdf(t, mean, sd)))
    }

  def spmg1(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
    Hrf.of("SPMG1", nbasis = 1, span = span, params = Map("P1" -> P1, "P2" -> P2, "A1" -> A1)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1(t, P1, P2, A1)))
    }

  def spmg1TemporalDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
    Hrf.of("SPMG1_temporal_deriv", nbasis = 1, span = span, params = Map("P1" -> P1, "P2" -> P2, "A1" -> A1)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1Deriv(t, P1, P2, A1)))
    }

  def spmg1DispersionDeriv(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833, span: Seconds = 24.s): Hrf =
    Hrf.of("SPMG1_dispersion_deriv", nbasis = 1, span = span, params = Map("P1" -> P1, "P2" -> P2, "A1" -> A1)) { t =>
      Vec.unsafe(Array(HrfFunctions.spmg1SecondDeriv(t, P1, P2, A1)))
    }

  def mexhat(mean: Double = 6.0, sd: Double = 2.0, span: Seconds = 24.s): Hrf =
    Hrf.of("mexhat", nbasis = 1, span = span) { t =>
      Vec.unsafe(Array(HrfFunctions.mexhat(t, mean, sd)))
    }

  def invLogit(mu1: Double = 6.0, s1: Double = 1.0, mu2: Double = 16.0, s2: Double = 1.0, lag: Seconds = 0.0.s, span: Seconds = 24.s): Hrf =
    Hrf.of("inv_logit", nbasis = 1, span = span) { t =>
      Vec.unsafe(Array(HrfFunctions.invLogit(t, mu1, s1, mu2, s2, lag)))
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
    Hrf.of("half_cosine", nbasis = 1, span = spanSec) { t =>
      Vec.unsafe(Array(HrfFunctions.halfCosine(t, h1, h2, h3, h4, f1, f2)))
    }

  def lwu(
      tau: Double = 6.0,
      sigma: Double = 2.5,
      rho: Double = 0.35,
      normalize: HrfFunctions.LwuNormalize = HrfFunctions.LwuNormalize.None,
      span: Seconds = 24.s
  ): Hrf =
    val raw = (t: Seconds) => HrfFunctions.lwu(t, tau, sigma, rho, HrfFunctions.LwuNormalize.None)
    val scale =
      normalize match
        case HrfFunctions.LwuNormalize.Height =>
          // Use a fairly fine grid to approximate global peak robustly.
          val dt = 0.005
          val nSamples = math.ceil(span.value / dt).toInt + 1
          var m = 0.0
          var i = 0
          while i < nSamples do
            val a = math.abs(raw(Seconds(i * dt)))
            if a > m then m = a
            i += 1
          if m > 1e-10 then m else 1.0
        case _ => 1.0
    Hrf.of("lwu", nbasis = 1, span = span) { t =>
      Vec.unsafe(Array(raw(t) / scale))
    }

  def boxcar(width: Seconds, amplitude: Double = 1.0, normalize: Boolean = false): Hrf =
    require(width.value.isFinite && width.value > 0.0, "`width` must be > 0")
    require(amplitude.isFinite, "`amplitude` must be finite")
    val amp = if normalize then 1.0 / width.value else amplitude
    Hrf.of(s"boxcar[${width.value}]", nbasis = 1, span = width) { t =>
      val x = t.value
      Vec.unsafe(Array(if x >= 0.0 && x < width.value then amp else 0.0))
    }

  def weighted(
      weights: Vector[Double],
      width: Option[Seconds] = None,
      times: Option[Vector[Seconds]] = None,
      method: WeightedMethod = WeightedMethod.Constant,
      normalize: Boolean = false
  ): Hrf =
    require(weights.length >= 2, "`weights` must have at least 2 elements")

    val ts: Vector[Seconds] =
      times match
        case Some(tvec) =>
          require(tvec.length == weights.length, "`times` and `weights` must have same length")
          require(tvec.head.value >= 0.0, "`times` must start at >= 0")
          require(tvec.sliding(2).forall(w => w.length == 2 && w(1).value > w(0).value), "`times` must be strictly increasing")
          tvec
        case None =>
          val w = width.getOrElse(throw new IllegalArgumentException("Either `width` or `times` must be provided"))
          require(w.value > 0.0, "`width` must be > 0")
          val n = weights.length
          Vector.tabulate(n)(i => Seconds(i.toDouble * w.value / (n - 1).toDouble))

    val timesD = ts.map(_.value).toArray
    var ws = weights.toArray

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

    Hrf.of("weighted", nbasis = 1, span = span) { t =>
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
    require(times.nonEmpty, "`times` must be non-empty")
    require(times.length == values.length, "`times` and `values` must match length")
    val pairs = times.zip(values).sortBy(_._1.value)
    val ts = pairs.map(_._1.value).toArray
    val ys = pairs.map(_._2).toArray
    require(ts.sliding(2).forall { case Array(a, b) => b > a }, "`times` must be strictly increasing")
    val span = Seconds(ts.last)

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

    Hrf.scalar(name, span = span, params = Map("times" -> ts.toVector, "values" -> ys.toVector)) { t =>
      interp(t.value)
    }

  def sine(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    Hrf.of("sine", nbasis = nBasis, span = span) { t =>
      Vec.unsafe(HrfFunctions.sineBasis(t, span, nBasis))
    }

  def fourier(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    Hrf.of("fourier", nbasis = nBasis, span = span) { t =>
      Vec.unsafe(HrfFunctions.fourierBasis(t, span, nBasis))
    }

  def daguerre(nBasis: Int = 3, scale: Double = 4.0, span: Seconds = 24.s): Hrf =
    val dt = 0.1
    val nSamples = math.ceil(span.value / dt).toInt + 1
    val maxAbs = Array.fill(nBasis)(0.0)
    var i = 0
    while i < nSamples do
      val t = Seconds(i * dt)
      val raw = HrfFunctions.daguerreBasis(t, nBasis, scale)
      var j = 0
      while j < nBasis do
        val a = math.abs(raw(j))
        if a > maxAbs(j) then maxAbs(j) = a
        j += 1
      i += 1
    val scales = maxAbs.map(m => if m > 1e-10 then m else 1.0)
    Hrf.of("daguerre", nbasis = nBasis, span = span) { t =>
      val raw = HrfFunctions.daguerreBasis(t, nBasis, scale)
      val normed = raw.zip(scales).map(_ / _)
      Vec.unsafe(normed)
    }

  def fir(nBasis: Int = 12, span: Seconds = 24.s): Hrf =
    val binWidth = span.value / nBasis.toDouble
    Hrf.of("fir", nbasis = nBasis, span = span) { t =>
      val x = t.value
      val out = Array.fill(nBasis)(0.0)
      if x >= 0.0 && x < span.value then
        val idx = if x == 0.0 then 0 else math.floor(x / binWidth).toInt
        out(math.min(idx, nBasis - 1)) = 1.0
      Vec.unsafe(out)
    }

  def bspline(nBasis: Int = 5, span: Seconds = 24.s, degree: Int = 3): Hrf =
    val ord = degree + 1
    val effectiveN = math.max(nBasis, ord)
    Hrf.of("bspline", nbasis = effectiveN, span = span, params = Map("nbasis" -> nBasis, "degree" -> degree)) { t =>
      Vec.unsafe(HrfFunctions.bsplineBasis(t, span, nBasis, degree))
    }

  def tent(nBasis: Int = 5, span: Seconds = 24.s): Hrf =
    val degree = 1
    val ord = degree + 1
    val effectiveN = math.max(nBasis, ord)
    Hrf.of("tent", nbasis = effectiveN, span = span, params = Map("nbasis" -> nBasis)) { t =>
      Vec.unsafe(HrfFunctions.bsplineBasis(t, span, nBasis, degree = degree))
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
