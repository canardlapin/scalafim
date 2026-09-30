package scalafim.group.research.bootstrap

/** The exact sign-flip reference of declaration §4 (narrow contract): an
  * intercept-only test of b0 with v held fixed. Orbit point g (bit i set means
  * s_i = -1) maps y_i to b0 + s_i (y_i - b0). The total statistic is |T| of the
  * full PM/mKH fit, and |T| := 0 on any failed fit (observed or transformed);
  * failures are counted, never dropped.
  *
  * p(gy) = #{h : S(hgy) >= S(gy)} / |G|. For every y, the fraction of the orbit
  * with p <= alpha is at most alpha; ties are retained, so the level is <= alpha.
  * Mathematically equal statistics can differ in the last bits (different
  * summation orders), so ">=" is taken with a relative tie tolerance: this only
  * raises p, so each rejection set is a subset of the exact one and level <= alpha holds.
  */
object SignFlip:
  /** Relative tie tolerance of the ">=" comparison. */
  val TieTolerance = 1e-10

  def atLeast(candidate: Double, reference: Double): Boolean = candidate >= reference - TieTolerance * math.abs(reference)

  /** A statistic on a transformed dataset; None means the fit failed. */
  type Statistic = Array[Double] => Option[Double]

  final case class Orbit(statistics: Array[Double], failures: Int):
    def size: Int = statistics.length

    /** p-value of orbit point g, the group being closed under composition. */
    def pValue(g: Int): Double =
      val s = statistics(g)
      var count = 0
      var h = 0
      while h < statistics.length do
        if atLeast(statistics(h), s) then count += 1
        h += 1
      count.toDouble / statistics.length

    /** #{g : p(gy) <= alpha}, which the theorem bounds by floor(alpha |G|). */
    def rejections(alpha: Double): Int = (0 until size).count(g => pValue(g) <= alpha)

  final case class MonteCarlo(observed: Double, exceed: Int, draws: Int, failures: Int):
    /** p = #{j in 0..B : S_j >= S_0} / (B + 1), the identity (j = 0) included. */
    def pValue: Double = exceed.toDouble / (draws + 1)

  def transform(y: Array[Double], b0: Double, g: Long, out: Array[Double]): Unit =
    var i = 0
    while i < y.length do
      val d = y(i) - b0
      out(i) = if ((g >>> i) & 1L) == 1L then b0 - d else b0 + d
      i += 1

  /** Exact enumeration of all 2^n orbit points (n <= 20). */
  def enumerate(y: Array[Double], b0: Double, statistic: Statistic): Orbit =
    val n = y.length
    require(n >= 1 && n <= 20, s"exact enumeration is for n <= 20, got $n")
    val size = 1 << n
    val out = new Array[Double](n)
    val stats = new Array[Double](size)
    var failures = 0
    var g = 0
    while g < size do
      transform(y, b0, g.toLong, out)
      stats(g) = statistic(out) match
        case Some(s) if s.isFinite => s
        case _ =>
          failures += 1
          0.0
      g += 1
    Orbit(stats, failures)

  /** Monte Carlo sign flips: B random orbit points plus the identity (Hemerik & Goeman 2018). */
  def monteCarlo(y: Array[Double], b0: Double, draws: Int, signs: SplitMix64, statistic: Statistic): MonteCarlo =
    val n = y.length
    val out = new Array[Double](n)
    var failures = 0
    def total(data: Array[Double]): Double = statistic(data) match
      case Some(s) if s.isFinite => s
      case _ =>
        failures += 1
        0.0
    val observed = total(y)
    var exceed = 1
    var b = 0
    while b < draws do
      var i = 0
      while i < n do
        val d = y(i) - b0
        out(i) = b0 + signs.nextSign() * d
        i += 1
      if atLeast(total(out), observed) then exceed += 1
      b += 1
    MonteCarlo(observed, exceed, draws, failures)

  /** |T| of the full PM/mKH fit with v fixed; None if the fit fails. */
  def pmStatistic(design: ResearchDesign, v: Array[Double], b0: Double): Statistic =
    require(design.p == 1 && design.contrast(0) == 1.0, "sign-flip contract: intercept-only design, c = (1)")
    val fitter = new StudyFitter(design)
    data =>
      if !fitter.fitFull(data, v, TauPolicy.PauleMandel).ok then None
      else Some(math.abs(fitter.mkhStatistic(b0)))
