package scalafim.group.research.bootstrap

/** Special functions for the harness: accurate log-gamma, polygammas (for the
  * Smyth moment fit), the regularized incomplete beta and its quantile (for the
  * Clopper-Pearson rules). Pure Scala, so the same code runs on JVM and JS.
  */
object Special:
  private val HalfLogTwoPi = 0.5 * math.log(2.0 * math.Pi)

  /** log Gamma(x) for x > 0: recurrence up to x >= 15, then the Stirling series. */
  def logGamma(x: Double): Double =
    require(x > 0.0, s"logGamma needs x > 0, got $x")
    var shift = 0.0
    var z = x
    while z < 15.0 do
      shift += math.log(z)
      z += 1.0
    val inv = 1.0 / z
    val inv2 = inv * inv
    val series = inv * (1.0 / 12.0 - inv2 * (1.0 / 360.0 - inv2 * (1.0 / 1260.0 - inv2 * (1.0 / 1680.0 - inv2 / 1188.0))))
    (z - 0.5) * math.log(z) - z + HalfLogTwoPi + series - shift

  /** digamma(x) for x > 0. */
  def digamma(x: Double): Double =
    require(x > 0.0, s"digamma needs x > 0, got $x")
    var acc = 0.0
    var z = x
    while z < 10.0 do
      acc -= 1.0 / z
      z += 1.0
    val inv2 = 1.0 / (z * z)
    acc + math.log(z) - 0.5 / z -
      inv2 * (1.0 / 12.0 - inv2 * (1.0 / 120.0 - inv2 * (1.0 / 252.0 - inv2 * (1.0 / 240.0 - inv2 / 132.0))))

  /** trigamma(x) for x > 0. */
  def trigamma(x: Double): Double =
    require(x > 0.0, s"trigamma needs x > 0, got $x")
    var acc = 0.0
    var z = x
    while z < 10.0 do
      acc += 1.0 / (z * z)
      z += 1.0
    val inv = 1.0 / z
    val inv2 = inv * inv
    acc + inv + 0.5 * inv2 +
      inv * inv2 * (1.0 / 6.0 - inv2 * (1.0 / 30.0 - inv2 * (1.0 / 42.0 - inv2 * (1.0 / 30.0 - inv2 * 5.0 / 66.0))))

  /** psi''(x) (tetragamma) for x > 0. */
  def tetragamma(x: Double): Double =
    require(x > 0.0, s"tetragamma needs x > 0, got $x")
    var acc = 0.0
    var z = x
    while z < 10.0 do
      acc -= 2.0 / (z * z * z)
      z += 1.0
    val inv = 1.0 / z
    val inv2 = inv * inv
    acc - inv2 - inv * inv2 -
      inv2 * inv2 * (0.5 - inv2 * (1.0 / 6.0 - inv2 * (1.0 / 6.0 - inv2 * (3.0 / 10.0 - inv2 * 5.0 / 6.0))))

  /** The y > 0 solving trigamma(y) = x, by the Newton iteration on 1/trigamma
    * used in Smyth (2004), started at 0.5 + 1/x.
    */
  def trigammaInverse(x: Double): Either[String, Double] =
    if !(x > 0.0) || !x.isFinite then Left(s"trigammaInverse needs a positive finite argument, got $x")
    else if x > 1e7 then Right(1.0 / math.sqrt(x))
    else if x < 1e-6 then Right(1.0 / x)
    else
      var y = 0.5 + 1.0 / x
      var iter = 0
      var done = false
      while !done && iter < 50 do
        val tri = trigamma(y)
        val dif = tri * (1.0 - tri / x) / tetragamma(y)
        y += dif
        if -dif / y < 1e-12 then done = true
        iter += 1
      if done && y > 0.0 && y.isFinite then Right(y) else Left(s"trigammaInverse did not converge at $x")

  /** Regularized incomplete beta I_x(a, b) by Lentz's continued fraction. */
  def incompleteBeta(a: Double, b: Double, x: Double): Double =
    require(a > 0.0 && b > 0.0, s"incomplete beta needs positive shapes, got $a, $b")
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      val logFront = logGamma(a + b) - logGamma(a) - logGamma(b) + a * math.log(x) + b * math.log1p(-x)
      if x < (a + 1.0) / (a + b + 2.0) then math.exp(logFront) * betaFraction(a, b, x) / a
      else 1.0 - math.exp(logFront) * betaFraction(b, a, 1.0 - x) / b

  private def betaFraction(a: Double, b: Double, x: Double): Double =
    val tiny = 1e-300
    var c = 1.0
    var d = 1.0 - (a + b) * x / (a + 1.0)
    if math.abs(d) < tiny then d = tiny
    d = 1.0 / d
    var h = d
    var m = 1
    var done = false
    while !done && m <= 10000 do
      val m2 = 2.0 * m
      var aa = m * (b - m) * x / ((a - 1.0 + m2) * (a + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      h *= d * c
      aa = -(a + m) * (a + b + m) * x / ((a + m2) * (a + 1.0 + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      val del = d * c
      h *= del
      if math.abs(del - 1.0) < 1e-15 then done = true
      m += 1
    h

  /** Quantile of Beta(a, b) by bisection on I_x(a, b); monotone and exact to ~1e-15 in x. */
  def betaQuantile(p: Double, a: Double, b: Double): Double =
    require(p >= 0.0 && p <= 1.0, s"probability must be in [0,1], got $p")
    if p == 0.0 then 0.0
    else if p == 1.0 then 1.0
    else
      var lo = 0.0
      var hi = 1.0
      var iter = 0
      while iter < 200 && hi - lo > 1e-16 do
        val mid = 0.5 * (lo + hi)
        if incompleteBeta(a, b, mid) < p then lo = mid else hi = mid
        iter += 1
      0.5 * (lo + hi)
