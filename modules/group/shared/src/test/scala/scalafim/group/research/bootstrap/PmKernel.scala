package scalafim.group.research.bootstrap

/** Outcome of one weighted fit. Singleton cases, so returning one allocates nothing. */
enum FitStatus(val message: String):
  case Converged extends FitStatus("converged")
  case NonFiniteInput extends FitStatus("non-finite effect or variance")
  case NonPositiveVariance extends FitStatus("non-positive first-level variance")
  case SingularWeightedDesign extends FitStatus("weighted design is numerically singular")
  case NoConvergence extends FitStatus("Paule-Mandel root did not converge")
  case NonFiniteResult extends FitStatus("fit produced a non-finite quantity")

  def ok: Boolean = this == FitStatus.Converged

/** How tau^2 is set for a fit: the Paule-Mandel root (declaration §2), or held fixed. */
enum TauPolicy:
  case PauleMandel
  case Fixed(tau2: Double)

/** Errors building a research design. */
enum DesignError(val message: String):
  case Shape(detail: String) extends DesignError(s"design shape: $detail")
  case RankDeficient extends DesignError("design is not of full column rank")
  case ZeroContrast extends DesignError("contrast vector is zero")

/** A full-rank group design X (n x p, row-major) with a scalar contrast c, plus
  * the restricted-null reparameterization of declaration §2:
  * beta = beta0 + N gamma with beta0 = b0 c / |c|^2 and N an orthonormal basis of
  * the null space of c' (p0 = p - 1 columns). `offsetUnit` is X c / |c|^2 (the
  * offset per unit b0) and `nullDesign` is X N (n x p0, row-major).
  */
final class ResearchDesign private (
    val n: Int,
    val p: Int,
    val x: Array[Double],
    val terms: Vector[String],
    val contrast: Array[Double],
    val nullBasis: Array[Double],
    val offsetUnit: Array[Double],
    val nullDesign: Array[Double]
):
  def p0: Int = p - 1
  def residualDf: Int = n - p
  def restrictedDf: Int = n - p0
  def row(i: Int, j: Int): Double = x(i * p + j)

  /** The design term whose unit vector is the contrast, if the contrast is one. */
  def contrastTerm: Option[String] =
    val hits = (0 until p).filter(j => contrast(j) != 0.0)
    if hits.length == 1 && contrast(hits.head) == 1.0 then Some(terms(hits.head)) else None

  /** X L and contrast L'c, the reparameterization used by the invariance tests. */
  def reparameterized(l: Array[Double]): Either[DesignError, ResearchDesign] =
    if l.length != p * p then Left(DesignError.Shape("L must be p x p"))
    else
      val xl = new Array[Double](n * p)
      val lc = new Array[Double](p)
      var i = 0
      while i < n do
        var j = 0
        while j < p do
          var s = 0.0
          var k = 0
          while k < p do
            s += x(i * p + k) * l(k * p + j)
            k += 1
          xl(i * p + j) = s
          j += 1
        i += 1
      var j = 0
      while j < p do
        var s = 0.0
        var k = 0
        while k < p do
          s += l(k * p + j) * contrast(k)
          k += 1
        lc(j) = s
        j += 1
      ResearchDesign.of(n, p, xl, terms, lc)

  /** Rows permuted: row i of the result is row perm(i) of this design. */
  def permuted(perm: Array[Int]): Either[DesignError, ResearchDesign] =
    if perm.length != n || perm.sorted.toSeq != (0 until n) then Left(DesignError.Shape("not a permutation"))
    else ResearchDesign.of(n, p, Array.tabulate(n * p)(k => x(perm(k / p) * p + k % p)), terms, contrast.clone())

object ResearchDesign:
  def of(n: Int, p: Int, x: Array[Double], terms: Vector[String], contrast: Array[Double]): Either[DesignError, ResearchDesign] =
    if p < 1 || n <= p then Left(DesignError.Shape(s"need n > p >= 1, got n=$n p=$p"))
    else if x.length != n * p then Left(DesignError.Shape("x must have n * p entries"))
    else if terms.length != p then Left(DesignError.Shape("one term name per column"))
    else if contrast.length != p then Left(DesignError.Shape("contrast must have p entries"))
    else if !x.forall(_.isFinite) || !contrast.forall(_.isFinite) then Left(DesignError.Shape("non-finite entries"))
    else
      val cc = contrast.map(c => c * c).sum
      if cc == 0.0 then Left(DesignError.ZeroContrast)
      else if !fullRank(n, p, x) then Left(DesignError.RankDeficient)
      else
        val basis = nullSpace(p, contrast)
        val p0 = p - 1
        val offset = Array.tabulate(n) { i =>
          var s = 0.0
          var j = 0
          while j < p do
            s += x(i * p + j) * contrast(j)
            j += 1
          s / cc
        }
        val nullDesign = Array.tabulate(n * p0) { k =>
          val i = k / p0
          val a = k % p0
          var s = 0.0
          var j = 0
          while j < p do
            s += x(i * p + j) * basis(j * p0 + a)
            j += 1
          s
        }
        Right(new ResearchDesign(n, p, x.clone(), terms, contrast.clone(), basis, offset, nullDesign))

  /** Orthonormal basis of {b : c'b = 0} as a p x (p-1) row-major matrix (Gram-Schmidt on the unit vectors). */
  private def nullSpace(p: Int, c: Array[Double]): Array[Double] =
    val norm = math.sqrt(c.map(v => v * v).sum)
    val accepted = scala.collection.mutable.ArrayBuffer(c.map(_ / norm))
    var j = 0
    while j < p && accepted.length < p do
      val v = Array.tabulate(p)(k => if k == j then 1.0 else 0.0)
      accepted.foreach { u =>
        val dot = (0 until p).map(k => v(k) * u(k)).sum
        var k = 0
        while k < p do
          v(k) -= dot * u(k)
          k += 1
      }
      val len = math.sqrt(v.map(a => a * a).sum)
      if len > 1e-6 then accepted += v.map(_ / len)
      j += 1
    val p0 = p - 1
    Array.tabulate(p * p0)(k => accepted(1 + k % p0)(k / p0))

  private def fullRank(n: Int, p: Int, x: Array[Double]): Boolean =
    val g = Array.tabulate(p * p) { k =>
      val a = k / p
      val b = k % p
      var s = 0.0
      var i = 0
      while i < n do
        s += x(i * p + a) * x(i * p + b)
        i += 1
      s
    }
    Cholesky.factor(g, p, 1e-12)

/** In-place lower Cholesky of a symmetric p x p row-major matrix (lower triangle read). */
private[bootstrap] object Cholesky:
  def factor(a: Array[Double], p: Int, relativeTolerance: Double): Boolean =
    var maxDiag = 0.0
    var d = 0
    while d < p do
      maxDiag = math.max(maxDiag, a(d * p + d))
      d += 1
    var ok = maxDiag > 0.0 && maxDiag.isFinite
    var j = 0
    while ok && j < p do
      var s = a(j * p + j)
      var k = 0
      while k < j do
        s -= a(j * p + k) * a(j * p + k)
        k += 1
      if !(s > relativeTolerance * maxDiag) then ok = false
      else
        val l = math.sqrt(s)
        a(j * p + j) = l
        var i = j + 1
        while i < p do
          var t = a(i * p + j)
          k = 0
          while k < j do
            t -= a(i * p + k) * a(j * p + k)
            k += 1
          a(i * p + j) = t / l
          i += 1
      j += 1
    ok

/** A reusable weighted least-squares + Paule-Mandel solver for one design Z
  * (n x q, row-major, q may be 0). All work arrays are allocated once; `fit`
  * allocates nothing. Equations (declaration §2):
  *   w_i = 1/(v_i + tau^2),  beta(tau^2) = (Z'WZ)^{-1} Z'W y,
  *   Q(tau^2) = sum_i w_i r_i^2,  r = y - Z beta(tau^2),
  *   PM: tau^2 = 0 if Q(0) <= target, else the root of Q(tau^2) = target.
  * Q is convex and decreasing in tau^2 (partial minimum of the jointly convex
  * r^2/s), so Newton from the left with dQ/dtau^2 = -sum w_i^2 r_i^2 is monotone;
  * a bisection safeguard is kept anyway. Stopping: |Q - target| <= 1e-12 target.
  */
final class PmSolver(val n: Int, val q: Int, z: Array[Double]):
  require(n > 0 && q >= 0 && z.length == n * q, "solver shape mismatch")
  val beta = new Array[Double](q)
  val residual = new Array[Double](n)
  private val factor = new Array[Double](math.max(1, q * q))
  private val rhs = new Array[Double](math.max(1, q))
  private val scratch = new Array[Double](math.max(1, q))
  var tau2 = 0.0
  var qStatistic = 0.0
  var iterations = 0
  private var derivative = 0.0

  /** Relative stopping tolerance on Q - target. */
  val RootTolerance = 1e-12

  def fit(y: Array[Double], v: Array[Double], target: Int, policy: TauPolicy): FitStatus =
    val input = checkInput(y, v)
    if !input.ok then input
    else
      policy match
        case TauPolicy.Fixed(t) =>
          iterations = 0
          tau2 = t
          solveAt(y, v, t)
        case TauPolicy.PauleMandel =>
          iterations = 0
          val first = solveAt(y, v, 0.0)
          if !first.ok then first
          else if qStatistic <= target then
            tau2 = 0.0
            FitStatus.Converged
          else root(y, v, target.toDouble)

  private def root(y: Array[Double], v: Array[Double], m: Double): FitStatus =
    var t = 0.0
    var lo = 0.0
    var hi = Double.PositiveInfinity
    var status: FitStatus = FitStatus.Converged
    var done = false
    while !done do
      val error = qStatistic - m
      if math.abs(error) <= RootTolerance * m then done = true
      else if iterations >= 200 then
        status = FitStatus.NoConvergence
        done = true
      else
        if error > 0.0 then lo = t else hi = t
        var next = if derivative > 0.0 then t + error / derivative else Double.NaN
        if !(next > lo && next < hi) then next = if hi.isFinite then 0.5 * (lo + hi) else 2.0 * lo + 1.0
        if next == t then
          done = true
          if math.abs(error) > 1e-9 * m then status = FitStatus.NoConvergence
        else
          t = next
          iterations += 1
          val solved = solveAt(y, v, t)
          if !solved.ok then
            status = solved
            done = true
    tau2 = t
    status

  private def checkInput(y: Array[Double], v: Array[Double]): FitStatus =
    var i = 0
    var status: FitStatus = FitStatus.Converged
    while i < n && status.ok do
      if !y(i).isFinite || !v(i).isFinite then status = FitStatus.NonFiniteInput
      else if v(i) <= 0.0 then status = FitStatus.NonPositiveVariance
      i += 1
    status

  /** WLS at a given tau^2: fills beta, residual, the Cholesky factor, Q and dQ. */
  private def solveAt(y: Array[Double], v: Array[Double], t: Double): FitStatus =
    var k = 0
    while k < q * q do
      factor(k) = 0.0
      k += 1
    k = 0
    while k < q do
      rhs(k) = 0.0
      k += 1
    var i = 0
    while i < n do
      val w = 1.0 / (v(i) + t)
      val yi = y(i)
      val o = i * q
      var a = 0
      while a < q do
        val wa = w * z(o + a)
        rhs(a) += wa * yi
        var b = 0
        while b <= a do
          factor(a * q + b) += wa * z(o + b)
          b += 1
        a += 1
      i += 1
    if q > 0 && !Cholesky.factor(factor, q, 1e-13) then FitStatus.SingularWeightedDesign
    else
      solveFactor(rhs, beta)
      var qs = 0.0
      var dq = 0.0
      i = 0
      while i < n do
        var fitted = 0.0
        val o = i * q
        var a = 0
        while a < q do
          fitted += z(o + a) * beta(a)
          a += 1
        val r = y(i) - fitted
        residual(i) = r
        val w = 1.0 / (v(i) + t)
        val wr2 = w * r * r
        qs += wr2
        dq += w * wr2
        i += 1
      qStatistic = qs
      derivative = dq
      var finite = qs.isFinite && dq.isFinite
      var a = 0
      while a < q do
        finite &&= beta(a).isFinite
        a += 1
      if finite then FitStatus.Converged else FitStatus.NonFiniteResult

  /** Solves (L L') out = in with the current factor. */
  private def solveFactor(in: Array[Double], out: Array[Double]): Unit =
    var a = 0
    while a < q do
      var s = in(a)
      var k = 0
      while k < a do
        s -= factor(a * q + k) * out(k)
        k += 1
      out(a) = s / factor(a * q + a)
      a += 1
    a = q - 1
    while a >= 0 do
      var s = out(a)
      var k = a + 1
      while k < q do
        s -= factor(k * q + a) * out(k)
        k += 1
      out(a) = s / factor(a * q + a)
      a -= 1

  /** c'(Z'WZ)^{-1}c at the last fitted tau^2. */
  def quadratic(c: Array[Double]): Double =
    var a = 0
    var s = 0.0
    while a < q do
      var t = c(a)
      var k = 0
      while k < a do
        t -= factor(a * q + k) * scratch(k)
        k += 1
      scratch(a) = t / factor(a * q + a)
      s += scratch(a) * scratch(a)
      a += 1
    s

  def dot(c: Array[Double]): Double =
    var s = 0.0
    var a = 0
    while a < q do
      s += c(a) * beta(a)
      a += 1
    s

/** The unrestricted and restricted fits of one design, sharing work arrays.
  * The statistic is the mKH-studentized contrast of §2:
  *   T = (c'beta_hat - b0) / sqrt(max(1, Q(tau_hat^2)/(n-p)) * c'(X'WX)^{-1}c),
  * referred (by the native baseline) to t(n-p).
  */
final class StudyFitter(val design: ResearchDesign):
  val full = new PmSolver(design.n, design.p, design.x)
  val restricted = new PmSolver(design.n, design.p0, design.nullDesign)
  private val shifted = new Array[Double](design.n)

  /** Unrestricted fit (target n - p) under `policy`. */
  def fitFull(y: Array[Double], v: Array[Double], policy: TauPolicy): FitStatus =
    full.fit(y, v, design.residualDf, policy)

  /** The mKH scale max(1, q), q = Q(tau_hat^2)/(n - p), of the last full fit. */
  def mkhScale: Double = math.max(1.0, full.qStatistic / design.residualDf)

  def estimate: Double = full.dot(design.contrast)

  def contrastVariance: Double = full.quadratic(design.contrast)

  /** mKH statistic of the last full fit, centred at `centre`. */
  def mkhStatistic(centre: Double): Double = (estimate - centre) / math.sqrt(mkhScale * contrastVariance)

  /** Wald statistic (scale 1) of the last full fit, for fixed-tau^2 statistics. */
  def waldStatistic(centre: Double): Double = (estimate - centre) / math.sqrt(contrastVariance)

  /** Restricted null fit of §2: y - b0 X c/|c|^2 on X N, PM target n - p0.
    * Writes the restricted fitted mean X beta_hat_0 into `mean`; the restricted
    * tau^2 is `restricted.tau2` afterwards.
    */
  def fitRestricted(y: Array[Double], v: Array[Double], b0: Double, policy: TauPolicy, mean: Array[Double]): FitStatus =
    val n = design.n
    var i = 0
    while i < n do
      shifted(i) = y(i) - b0 * design.offsetUnit(i)
      i += 1
    val status = restricted.fit(shifted, v, design.restrictedDf, policy)
    if status.ok then
      i = 0
      while i < n do
        mean(i) = y(i) - restricted.residual(i)
        i += 1
    status

  /** beta_hat_0 = beta0 + N gamma_hat in the original coordinates (after `fitRestricted`). */
  def restrictedCoefficients(b0: Double): Array[Double] =
    val p = design.p
    val p0 = design.p0
    val cc = design.contrast.map(c => c * c).sum
    Array.tabulate(p) { j =>
      var s = b0 * design.contrast(j) / cc
      var a = 0
      while a < p0 do
        s += design.nullBasis(j * p0 + a) * restricted.beta(a)
        a += 1
      s
    }
