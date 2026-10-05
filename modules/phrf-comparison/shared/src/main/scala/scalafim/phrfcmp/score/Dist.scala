package scalafim.phrfcmp.score

/** Portable (JVM and JS) distribution functions: regularized incomplete beta and gamma, the F and chi-square
  * quantiles. Used for the ICC upper limit and the author-side sigma UCL factor. Double precision, about 1e-13
  * relative against R (`qf`, `qchisq`) in the tested range.
  */
object Dist:
  private val LanczosG = 7.0
  private val LanczosC = Array(
    0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059,
    12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  )

  /** log Gamma(x) for x > 0 (Lanczos, g = 7, n = 9). */
  def lgamma(x: Double): Double =
    if x < 0.5 then math.log(math.Pi / math.abs(math.sin(math.Pi * x))) - lgamma(1.0 - x)
    else
      val xm = x - 1.0
      var a = LanczosC(0)
      val t = xm + LanczosG + 0.5
      var i = 1
      while i < 9 do
        a += LanczosC(i) / (xm + i)
        i += 1
      0.5 * math.log(2.0 * math.Pi) + (xm + 0.5) * math.log(t) - t + math.log(a)

  private def betaCf(x: Double, a: Double, b: Double): Double =
    val tiny = 1e-300
    val qab = a + b
    val qap = a + 1.0
    val qam = a - 1.0
    var c = 1.0
    var d = 1.0 - qab * x / qap
    if math.abs(d) < tiny then d = tiny
    d = 1.0 / d
    var h = d
    var m = 1
    var done = false
    while !done && m <= 20000 do
      val m2 = 2 * m
      var aa = m * (b - m) * x / ((qam + m2) * (a + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      h *= d * c
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      val del = d * c
      h *= del
      if math.abs(del - 1.0) < 1e-16 then done = true
      m += 1
    h

  /** Regularized incomplete beta I_x(a, b). */
  def regIncBeta(x: Double, a: Double, b: Double): Double =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      val bt = math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * math.log(x) + b * math.log1p(-x))
      if x < (a + 1.0) / (a + b + 2.0) then bt * betaCf(x, a, b) / a
      else 1.0 - bt * betaCf(1.0 - x, b, a) / b

  /** Regularized lower incomplete gamma P(a, x). */
  def regGammaP(a: Double, x: Double): Double =
    if x <= 0.0 then 0.0
    else if x < a + 1.0 then
      var ap = a
      var del = 1.0 / a
      var sum = del
      var n = 0
      while n < 100000 && math.abs(del) > math.abs(sum) * 1e-16 do
        ap += 1.0
        del *= x / ap
        sum += del
        n += 1
      sum * math.exp(-x + a * math.log(x) - lgamma(a))
    else
      val tiny = 1e-300
      var b = x + 1.0 - a
      var c = 1.0 / tiny
      var d = 1.0 / b
      var h = d
      var i = 1
      var done = false
      while !done && i < 100000 do
        val an = -i * (i - a)
        b += 2.0
        d = an * d + b
        if math.abs(d) < tiny then d = tiny
        c = b + an / c
        if math.abs(c) < tiny then c = tiny
        d = 1.0 / d
        val del = d * c
        h *= del
        if math.abs(del - 1.0) < 1e-16 then done = true
        i += 1
      1.0 - math.exp(-x + a * math.log(x) - lgamma(a)) * h

  def fCdf(f: Double, d1: Double, d2: Double): Double =
    if f <= 0.0 then 0.0 else regIncBeta(d1 * f / (d1 * f + d2), d1 / 2.0, d2 / 2.0)

  def chi2Cdf(x: Double, df: Double): Double = regGammaP(df / 2.0, x / 2.0)

  private def invertMonotone(cdf: Double => Double, p: Double): Double =
    var hi = 1.0
    while cdf(hi) < p && hi < 1e300 do hi *= 2.0
    var lo = 0.0
    var i = 0
    while i < 300 do
      val mid = 0.5 * (lo + hi)
      if cdf(mid) < p then lo = mid else hi = mid
      i += 1
    0.5 * (lo + hi)

  /** Quantile of the F distribution with (d1, d2) degrees of freedom, 0 < p < 1. */
  def fQuantile(p: Double, d1: Double, d2: Double): Double =
    require(p > 0.0 && p < 1.0 && d1 > 0.0 && d2 > 0.0, "fQuantile domain")
    invertMonotone(f => fCdf(f, d1, d2), p)

  /** Quantile of the chi-square distribution, 0 < p < 1. */
  def chi2Quantile(p: Double, df: Double): Double =
    require(p > 0.0 && p < 1.0 && df > 0.0, "chi2Quantile domain")
    invertMonotone(x => chi2Cdf(x, df), p)

  /** Author-side 80 % UCL factor on a standard deviation with `df` degrees of freedom:
    * `sqrt(df / chi2_{0.20, df})` (about 1.18 at 19 df). The whitelist emits sigma and df; this is the factor the
    * author multiplies by.
    */
  def sigmaUclFactor(df: Int): Double =
    require(df >= 1, "df must be positive")
    math.sqrt(df.toDouble / chi2Quantile(0.20, df.toDouble))
