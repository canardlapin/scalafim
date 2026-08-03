package scalafim.fmri.hrf

/** How the box response `∫₀ᵈ h(l - u) du` is computed.
  *
  * [[Exact]] is the default because the box response is an integral with a
  * value, not an approximation with a parameter: where the family admits a
  * primitive, `precision` stops influencing the answer at all.
  *
  * [[Trapezoid]] reproduces R `fmrihrf`'s `evaluate.HRF`, whose
  * `.block_offsets_weights` applies the trapezoid rule at the caller's
  * `precision`. It is kept — and pinned by the R parity corpus — because
  * matching R's arithmetic exactly is a separate, legitimate question from
  * getting the integral right.
  */
enum Integration:
  case Exact
  case Trapezoid

/** Whether a kernel's antiderivative is known in closed form.
  *
  * This is symbolic provenance in the same sense as [[DerivativePolicy]] and
  * [[PenaltyPolicy]]: it records what the kernel *is*, so the integrator can
  * dispatch instead of guessing. Anything not named here integrates by
  * quadrature, which is always correct and merely slower.
  *
  * Crucially the policy is attached at construction and cleared by
  * [[HrfDescriptor.derived]], so a lagged, blocked or rescaled kernel cannot
  * inherit a primitive that no longer describes it.
  */
enum IntegrationPolicy:
  /** No closed form registered; integrate numerically. */
  case Quadrature

  /** `dgamma(τ; shape, rate)` — regularized lower incomplete gamma. */
  case Gamma(shape: Double, rate: Double)

  /** `dnorm(τ; mean, sd)` — normal CDF. */
  case Gaussian(mean: Double, sd: Double)

  /** The SPM canonical `e^{-τ}(A₁τ^{P₁} - Cτ^{P₂})`. */
  case Spmg1(params: SpmgParams)

  /** The SPM temporal derivative; its primitive is the canonical itself. */
  case SpmgTemporalDeriv(params: SpmgParams)

  /** The SPM dispersion derivative; its primitive is the temporal derivative. */
  case SpmgDispersionDeriv(params: SpmgParams)

  /** `amplitude` on `[0, width)`. */
  case Boxcar(width: Seconds, amplitude: Double)

  /** Polynomial of `degree` between consecutive `breaks`, zero outside them.
    *
    * FIR (degree 0), tent (1) and B-spline (p) are all of this shape. Splitting
    * at the breaks and applying Gauss–Legendre of sufficient order integrates
    * each piece exactly, which is what removes the `O(h)` convergence floor a
    * trapezoid straddling a knot or a bin edge otherwise suffers.
    */
  case PiecewisePolynomial(breaks: Vector[Seconds], degree: Int)

  /** Columns are the concatenation of `components`' columns. */
  case Stacked

/** Special functions needed by the closed forms, in portable Scala.
  *
  * Deliberately dependency-free: `shared` code runs on Scala.js, so no Breeze
  * and no `java.lang.Math` beyond what Scala.js implements.
  */
private[hrf] object Special:

  private inline val Eps = 1e-16
  private inline val Tiny = 1e-300
  private inline val MaxIter = 500

  /** Regularized lower incomplete gamma `P(a, x) = γ(a, x) / Γ(a)`. */
  def lowerGammaP(a: Double, x: Double): Double =
    require(a > 0.0, s"`a` must be > 0, got $a")
    if x <= 0.0 then 0.0
    else if x < a + 1.0 then seriesP(a, x)
    else 1.0 - continuedFractionQ(a, x)

  /** Series expansion, convergent for `x < a + 1`. */
  private def seriesP(a: Double, x: Double): Double =
    var ap = a
    var del = 1.0 / a
    var sum = del
    var n = 0
    var done = false
    while n < MaxIter && !done do
      ap += 1.0
      del *= x / ap
      sum += del
      if math.abs(del) < math.abs(sum) * Eps then done = true
      n += 1
    sum * math.exp(-x + a * math.log(x) - HrfFunctions.logGamma(a))

  /** Modified Lentz continued fraction for `Q(a, x)`, convergent for `x >= a + 1`. */
  private def continuedFractionQ(a: Double, x: Double): Double =
    var b = x + 1.0 - a
    var c = 1.0 / Tiny
    var d = 1.0 / b
    var h = d
    var i = 1
    var done = false
    while i <= MaxIter && !done do
      val an = -i.toDouble * (i.toDouble - a)
      b += 2.0
      d = an * d + b
      if math.abs(d) < Tiny then d = Tiny
      c = b + an / c
      if math.abs(c) < Tiny then c = Tiny
      d = 1.0 / d
      val del = d * c
      h *= del
      if math.abs(del - 1.0) < Eps then done = true
      i += 1
    math.exp(-x + a * math.log(x) - HrfFunctions.logGamma(a)) * h

  /** `∫₀ˣ e^{-τ} τ^p dτ = γ(p+1, x)`. */
  def lowerGammaIncomplete(p: Double, x: Double): Double =
    if x <= 0.0 then 0.0
    else math.exp(HrfFunctions.logGamma(p + 1.0)) * lowerGammaP(p + 1.0, x)

  def erf(x: Double): Double =
    if x == 0.0 then 0.0
    else if x > 0.0 then lowerGammaP(0.5, x * x)
    else -lowerGammaP(0.5, x * x)

  /** Standard normal CDF. */
  def normalCdf(z: Double): Double =
    0.5 * (1.0 + erf(z / math.sqrt(2.0)))

/** Exact integration of a kernel against a box, where the family allows it.
  *
  * The box response is a definite integral of the kernel:
  * `(q_d * h)(l) = ∫₀ᵈ h(l - u) du = ∫_{l-d}^{l} h(τ) dτ`, so a family with a
  * primitive `H(x) = ∫₀ˣ h` gives it as `H(l) - H(l - d)` with no quadrature
  * and no `precision`.
  */
object Primitive:

  /** `∫ₐᵇ h(τ) dτ`, one entry per basis column, or `None` if no exact rule
    * applies to this kernel.
    */
  def definiteIntegral(hrf: Hrf, from: Lag, to: Lag): Option[Array[Double]] =
    hrf.descriptor.integration match
      case IntegrationPolicy.PiecewisePolynomial(breaks, degree) =>
        Some(piecewise(hrf, breaks.map(_.value).toArray, degree, from.value, to.value))
      case _ =>
        closedForm(hrf.descriptor).map { h =>
          val hi = h(to)
          val lo = h(from)
          var i = 0
          val out = new Array[Double](hi.length)
          while i < out.length do
            out(i) = hi(i) - lo(i)
            i += 1
          out
        }

  /** `∫₀ˣ h(τ) dτ` per column for the families whose primitive needs no kernel
    * evaluation at all.
    */
  private def closedForm(descriptor: HrfDescriptor): Option[Lag => Array[Double]] =
    descriptor.integration match
      case IntegrationPolicy.Quadrature => None
      case IntegrationPolicy.PiecewisePolynomial(_, _) => None

      case IntegrationPolicy.Gamma(shape, rate) =>
        Some(x => Array(if x.value <= 0.0 then 0.0 else Special.lowerGammaP(shape, rate * x.value)))

      case IntegrationPolicy.Gaussian(mean, sd) =>
        val atZero = Special.normalCdf((0.0 - mean) / sd)
        Some(x => Array(if x.value <= 0.0 then 0.0 else Special.normalCdf((x.value - mean) / sd) - atZero))

      case IntegrationPolicy.Spmg1(p) =>
        Some(x => Array(if x.value <= 0.0 then 0.0 else spmg1Integral(p, x.value)))

      // ∫₀ˣ h' = h(x) - h(0): the primitive of the temporal derivative is the
      // canonical kernel, and of the dispersion derivative the temporal one.
      case IntegrationPolicy.SpmgTemporalDeriv(p) =>
        val atZero = HrfFunctions.spmg1(Lag.unsafe(0.0), p.p1, p.p2, p.a1)
        Some(x =>
          Array(if x.value <= 0.0 then 0.0 else HrfFunctions.spmg1(x, p.p1, p.p2, p.a1) - atZero)
        )

      case IntegrationPolicy.SpmgDispersionDeriv(p) =>
        val atZero = HrfFunctions.spmg1Deriv(Lag.unsafe(0.0), p.p1, p.p2, p.a1)
        Some(x =>
          Array(if x.value <= 0.0 then 0.0 else HrfFunctions.spmg1Deriv(x, p.p1, p.p2, p.a1) - atZero)
        )

      case IntegrationPolicy.Boxcar(width, amplitude) =>
        val w = width.value
        Some(x =>
          val clamped = if x.value <= 0.0 then 0.0 else if x.value >= w then w else x.value
          Array(amplitude * clamped)
        )

      case IntegrationPolicy.Stacked =>
        val components = descriptor.components
        if components.isEmpty then None
        else
          val parts = components.map(closedForm)
          if parts.exists(_.isEmpty) then None
          else if components.map(_.nbasis).sum != descriptor.nbasis then None
          else
            val fns = parts.map(_.get)
            Some { x =>
              val out = new Array[Double](descriptor.nbasis)
              var offset = 0
              var i = 0
              while i < fns.length do
                val v = fns(i)(x)
                System.arraycopy(v, 0, out, offset, v.length)
                offset += v.length
                i += 1
              out
            }

  /** `∫₀ˣ e^{-τ}(A₁τ^{P₁} - Cτ^{P₂}) dτ = A₁γ(P₁+1, x) - Cγ(P₂+1, x)`. */
  private def spmg1Integral(p: SpmgParams, x: Double): Double =
    p.a1 * Special.lowerGammaIncomplete(p.p1, x) -
      HrfFunctions.spmg1C * Special.lowerGammaIncomplete(p.p2, x)

  // --- piecewise-polynomial integration ------------------------------------

  /** Gauss–Legendre nodes and weights on `[-1, 1]`; `n` points integrate a
    * polynomial of degree `2n - 1` exactly.
    */
  private val glNodes: Array[Array[Double]] = Array(
    Array(0.0),
    Array(-0.5773502691896257, 0.5773502691896257),
    Array(-0.7745966692414834, 0.0, 0.7745966692414834),
    Array(-0.8611363115940526, -0.3399810435848563, 0.3399810435848563, 0.8611363115940526)
  )

  private val glWeights: Array[Array[Double]] = Array(
    Array(2.0),
    Array(1.0, 1.0),
    Array(0.5555555555555556, 0.8888888888888888, 0.5555555555555556),
    Array(0.3478548451374538, 0.6521451548625461, 0.6521451548625461, 0.3478548451374538)
  )

  private def pointsFor(degree: Int): Int =
    val needed = (degree + 2) / 2
    if needed < 1 then 1 else if needed > 4 then 4 else needed

  private def piecewise(
      hrf: Hrf,
      breaks: Array[Double],
      degree: Int,
      from: Double,
      to: Double
  ): Array[Double] =
    val nb = hrf.nbasis
    val out = new Array[Double](nb)
    if breaks.length < 2 then return out

    // The kernel is zero outside `[0, last break]`, so clip rather than
    // extrapolate a polynomial that does not apply there.
    val lo = math.max(math.max(from, 0.0), breaks(0))
    val hi = math.min(to, breaks(breaks.length - 1))
    if !(hi > lo) then return out

    val n = pointsFor(degree)
    val nodes = glNodes(n - 1)
    val weights = glWeights(n - 1)

    var b = 0
    while b < breaks.length - 1 do
      val pieceLo = math.max(lo, breaks(b))
      val pieceHi = math.min(hi, breaks(b + 1))
      if pieceHi > pieceLo then
        val half = 0.5 * (pieceHi - pieceLo)
        val mid = 0.5 * (pieceHi + pieceLo)
        var k = 0
        while k < n do
          val v = hrf(Lag.unsafe(half * nodes(k) + mid)).data
          val w = half * weights(k)
          var j = 0
          while j < nb do
            out(j) += w * v(j)
            j += 1
          k += 1
      b += 1
    out
