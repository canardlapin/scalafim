package scalafim.fmri.group

/** Self-contained, cross-platform statistical distributions used to turn group
  * test statistics into p-values. Pure Scala (no Breeze), so the group core
  * cross-compiles to JVM and Scala.js.
  *
  * Accuracy targets ~1e-7, sufficient for parity with R's `pnorm`/`pt`/`p.adjust`
  * at the tolerances used in tests.
  */
object Distributions:

  /** Two-sided p-value for a standard normal statistic `z`. Clamped to `[0, 1]`
    * so the `erfc` approximation error cannot push a p-value above one.
    */
  def normalTwoSidedP(z: Double): Double =
    if z.isNaN then Double.NaN
    else math.min(1.0, erfc(math.abs(z) / math.sqrt(2.0)))

  /** Upper-tail (survival) probability for a standard normal statistic. */
  def normalSf(z: Double): Double =
    if z.isNaN then Double.NaN
    else
      val p = 0.5 * erfc(z / math.sqrt(2.0))
      if p < 0.0 then 0.0 else if p > 1.0 then 1.0 else p

  /** Two-sided p-value for a Student-t statistic with `df` degrees of freedom.
    *
    * Uses the identity `P(|T| > |t|) = I_x(df/2, 1/2)` with `x = df/(df + t^2)`,
    * where `I` is the regularized incomplete beta function.
    */
  def studentTTwoSidedP(t: Double, df: Int): Double =
    if t.isNaN || df <= 0 then Double.NaN
    else
      val x = df.toDouble / (df.toDouble + t * t)
      regularizedIncompleteBeta(df.toDouble / 2.0, 0.5, x)

  /** Complementary error function, `erfc(x) = 1 - erf(x)`.
    *
    * Numerical Recipes rational Chebyshev approximation; fractional error < 1.2e-7.
    */
  def erfc(x: Double): Double =
    val z = math.abs(x)
    val tt = 1.0 / (1.0 + 0.5 * z)
    val ans = tt * math.exp(
      -z * z - 1.26551223 +
        tt * (1.00002368 +
          tt * (0.37409196 +
            tt * (0.09678418 +
              tt * (-0.18628806 +
                tt * (0.27886807 +
                  tt * (-1.13520398 +
                    tt * (1.48851587 +
                      tt * (-0.82215223 +
                        tt * 0.17087277))))))))
    )
    if x >= 0.0 then ans else 2.0 - ans

  /** Natural log of the gamma function (Lanczos approximation). */
  def logGamma(x: Double): Double =
    val cof = Array(
      76.18009172947146, -86.50532032941677, 24.01409824083091,
      -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5
    )
    var y = x
    val tmp0 = x + 5.5
    val tmp = tmp0 - (x + 0.5) * math.log(tmp0)
    var ser = 1.000000000190015
    var j = 0
    while j < cof.length do
      y += 1.0
      ser += cof(j) / y
      j += 1
    -tmp + math.log(2.5066282746310005 * ser / x)

  /** Regularized incomplete beta function `I_x(a, b)`. Continued-fraction
    * evaluation (Numerical Recipes `betai`/`betacf`).
    */
  def regularizedIncompleteBeta(a: Double, b: Double, x: Double): Double =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      val bt = math.exp(
        logGamma(a + b) - logGamma(a) - logGamma(b) +
          a * math.log(x) + b * math.log(1.0 - x)
      )
      if x < (a + 1.0) / (a + b + 2.0) then bt * betaContinuedFraction(a, b, x) / a
      else 1.0 - bt * betaContinuedFraction(b, a, 1.0 - x) / b

  private def betaContinuedFraction(a: Double, b: Double, x: Double): Double =
    val fpmin = 1e-30
    val qab = a + b
    val qap = a + 1.0
    val qam = a - 1.0
    var c = 1.0
    var d = 1.0 - qab * x / qap
    if math.abs(d) < fpmin then d = fpmin
    d = 1.0 / d
    var h = d
    var m = 1
    var converged = false
    while m <= 200 && !converged do
      val m2 = 2 * m
      var aa = m * (b - m) * x / ((qam + m2) * (a + m2))
      d = 1.0 + aa * d
      if math.abs(d) < fpmin then d = fpmin
      c = 1.0 + aa / c
      if math.abs(c) < fpmin then c = fpmin
      d = 1.0 / d
      h *= d * c
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
      d = 1.0 + aa * d
      if math.abs(d) < fpmin then d = fpmin
      c = 1.0 + aa / c
      if math.abs(c) < fpmin then c = fpmin
      d = 1.0 / d
      val del = d * c
      h *= del
      if math.abs(del - 1.0) < 3.0e-11 then converged = true
      m += 1
    h
