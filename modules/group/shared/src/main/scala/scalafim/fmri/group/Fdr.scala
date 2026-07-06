package scalafim.fmri.group

/** False-discovery-rate control for a map of group p-values.
  *
  * Both methods are step-up procedures that return *adjusted* p-values
  * (q-values) in the input order, matching R's `p.adjust(method = "BH" | "BY")`.
  * Non-finite p-values are passed through as `NaN` and excluded from the
  * comparison count `m`.
  */
object Fdr:

  /** Benjamini–Hochberg adjusted p-values. */
  def benjaminiHochberg(p: Array[Double]): Array[Double] =
    adjust(p, cm = 1.0)

  /** Benjamini–Yekutieli adjusted p-values (valid under arbitrary dependence).
    * Scales the BH multiplier by the harmonic number `c(m) = Σ_{i=1..m} 1/i`.
    */
  def benjaminiYekutieli(p: Array[Double]): Array[Double] =
    val m = p.count(_.isFinite)
    var cm = 0.0
    var i = 1
    while i <= m do
      cm += 1.0 / i
      i += 1
    adjust(p, cm = if m == 0 then 1.0 else cm)

  /** Shared step-up core. `cm` is the dependence-correction multiplier
    * (1.0 for BH, the harmonic number for BY).
    */
  private def adjust(p: Array[Double], cm: Double): Array[Double] =
    val n = p.length
    val out = new Array[Double](n)

    // Order the finite p-values ascending; non-finite values are handled separately.
    val finiteIdx = (0 until n).filter(i => p(i).isFinite).toArray
    val m = finiteIdx.length
    if m == 0 then
      java.util.Arrays.fill(out, Double.NaN)
      return out

    val order = finiteIdx.sortBy(i => p(i))

    // Step-up with enforced monotonicity from the largest rank downward.
    var running = Double.PositiveInfinity
    var rank = m
    while rank >= 1 do
      val idx = order(rank - 1)
      val raw = p(idx) * cm * m / rank
      running = math.min(running, raw)
      out(idx) = math.min(running, 1.0)
      rank -= 1

    var i = 0
    while i < n do
      if !p(i).isFinite then out(i) = Double.NaN
      i += 1
    out
