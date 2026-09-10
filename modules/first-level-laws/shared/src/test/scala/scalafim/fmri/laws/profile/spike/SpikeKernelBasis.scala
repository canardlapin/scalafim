package scalafim.fmri.laws.profile.spike

import gale.linalg.DMat

/** Kernel basis `Phi` (`m x nFine`, row-major by basis column) from an SVD of
  * the family and its parameter derivatives sampled over a shape grid.
  *
  * `h_theta ~= Phi' c(theta)` with `c(theta) = Phi h_theta`; coefficient jets
  * follow from the family's analytic jets by linearity.
  */
final class SpikeKernelBasis(
    val fineTimes: Array[Double],
    val phi: Array[Double],
    val m: Int,
    val singularValues: Array[Double]):

  val nFine: Int = fineTimes.length

  def truncated(rank: Int): SpikeKernelBasis =
    require(rank >= 1 && rank <= m, s"rank $rank outside 1..$m")
    new SpikeKernelBasis(fineTimes, java.util.Arrays.copyOf(phi, rank * nFine), rank, java.util.Arrays.copyOf(singularValues, rank))

  /** `c(theta)` into `out(0 until m)`; `kernelScratch` has length `nFine`. */
  def coefficientsInto(tau: Double, logSd: Double, kernelScratch: Array[Double], out: Array[Double]): Unit =
    SpikeGaussianFamily.valueInto(fineTimes, tau, logSd, kernelScratch)
    projectInto(kernelScratch, 0, out, 0)

  /** Six-component coefficient jet, component-major: `out(comp * m + j)`;
    * `kernelScratch` has length `6 * nFine`.
    */
  def coefficientJetInto(tau: Double, logSd: Double, kernelScratch: Array[Double], out: Array[Double]): Unit =
    coefficientJetInto(tau, logSd, kernelScratch, out, SpikeGaussianFamily.JetComponents)

  /** Coefficient jet with `components` in {3, 6}: value and first derivatives, or all six. */
  def coefficientJetInto(tau: Double, logSd: Double, kernelScratch: Array[Double], out: Array[Double], components: Int): Unit =
    if components >= SpikeGaussianFamily.JetComponents then SpikeGaussianFamily.jetInto(fineTimes, tau, logSd, kernelScratch)
    else SpikeGaussianFamily.firstOrderJetInto(fineTimes, tau, logSd, kernelScratch)
    var comp = 0
    while comp < components do
      projectInto(kernelScratch, comp * nFine, out, comp * m)
      comp += 1

  private def projectInto(source: Array[Double], sourceOffset: Int, out: Array[Double], outOffset: Int): Unit =
    var j = 0
    while j < m do
      val base = j * nFine
      var acc = 0.0
      var i = 0
      while i < nFine do
        acc += phi(base + i) * source(sourceOffset + i)
        i += 1
      out(outOffset + j) = acc
      j += 1

  /** Reconstruct `Phi' c` into `out(0 until nFine)`. */
  def reconstructInto(c: Array[Double], out: Array[Double]): Unit =
    java.util.Arrays.fill(out, 0, nFine, 0.0)
    var j = 0
    while j < m do
      val base = j * nFine
      val cj = c(j)
      var i = 0
      while i < nFine do
        out(i) += phi(base + i) * cj
        i += 1
      j += 1

object SpikeKernelBasis:

  final case class ErrorCurve(valueError: Array[Double], firstDerivativeError: Array[Double], secondDerivativeError: Array[Double]):
    /** Smallest rank whose held-out value error is below `tolerance`, if any. */
    def rankFor(tolerance: Double): Option[Int] =
      val idx = valueError.indexWhere(_ <= tolerance)
      if idx < 0 then None else Some(idx + 1)

  /** Sample the family (and, optionally, its derivatives) over a grid of
    * shapes and take the SVD; the left singular vectors are the basis.
    */
  def compile(
      domain: SpikeGaussianFamily.Domain,
      fineDt: Double,
      horizon: Double,
      tauNodes: Int,
      logSdNodes: Int,
      includeDerivatives: Boolean,
      maxRank: Int
  ): SpikeKernelBasis =
    val nFine = math.round(horizon / fineDt).toInt
    val times = Array.tabulate(nFine)(i => i * fineDt)
    val comps = if includeDerivatives then SpikeGaussianFamily.JetComponents else 1
    val samples = tauNodes * logSdNodes * comps
    val builder = DMat.newBuilder(nFine, samples)
    val scratch = new Array[Double](SpikeGaussianFamily.JetComponents * nFine)
    var col = 0
    var a = 0
    while a < tauNodes do
      val tau = domain.tauMin + (domain.tauMax - domain.tauMin) * a / math.max(1, tauNodes - 1)
      var b = 0
      while b < logSdNodes do
        val v = domain.logSdMin + (domain.logSdMax - domain.logSdMin) * b / math.max(1, logSdNodes - 1)
        SpikeGaussianFamily.jetInto(times, tau, v, scratch)
        var comp = 0
        while comp < comps do
          // Scale each component so its sample has unit norm: the basis should
          // represent shapes and directions equally, not favour large lobes.
          var norm = 0.0
          var i = 0
          while i < nFine do
            val x = scratch(comp * nFine + i)
            norm += x * x
            i += 1
          val scale = if norm > 0.0 then 1.0 / math.sqrt(norm) else 0.0
          i = 0
          while i < nFine do
            builder.update(i, col, scratch(comp * nFine + i) * scale)
            i += 1
          col += 1
          comp += 1
        b += 1
      a += 1
    val svd = builder.result().svd.fold(err => throw new IllegalStateException(err.getMessage), identity)
    val rank = math.min(maxRank, math.min(svd.u.cols, svd.singularValues.length))
    val phi = new Array[Double](rank * nFine)
    var j = 0
    while j < rank do
      var i = 0
      while i < nFine do
        phi(j * nFine + i) = svd.u(i, j)
        i += 1
      j += 1
    val sv = Array.tabulate(rank)(j => svd.singularValues(j))
    new SpikeKernelBasis(times, phi, rank, sv)

  /** Maximum relative reconstruction error over held-out shapes, per rank. */
  def heldOutErrors(basis: SpikeKernelBasis, domain: SpikeGaussianFamily.Domain, points: Int, seed: Long): ErrorCurve =
    val rng = new scala.util.Random(seed)
    val n = basis.nFine
    val value = new Array[Double](basis.m)
    val first = new Array[Double](basis.m)
    val second = new Array[Double](basis.m)
    val jet = new Array[Double](SpikeGaussianFamily.JetComponents * n)
    val coeff = new Array[Double](basis.m)
    var p = 0
    while p < points do
      val tau = domain.tauMin + rng.nextDouble() * (domain.tauMax - domain.tauMin)
      val v = domain.logSdMin + rng.nextDouble() * (domain.logSdMax - domain.logSdMin)
      SpikeGaussianFamily.jetInto(basis.fineTimes, tau, v, jet)
      var comp = 0
      while comp < SpikeGaussianFamily.JetComponents do
        var norm = 0.0
        var i = 0
        while i < n do
          val x = jet(comp * n + i)
          norm += x * x
          i += 1
        // Coefficients of this component in the full basis.
        var j = 0
        while j < basis.m do
          var acc = 0.0
          i = 0
          while i < n do
            acc += basis.phi(j * n + i) * jet(comp * n + i)
            i += 1
          coeff(j) = acc
          j += 1
        var residual = norm
        val target = if comp == 0 then value else if comp < 3 then first else second
        j = 0
        while j < basis.m do
          residual -= coeff(j) * coeff(j)
          val rel = if norm > 0.0 then math.sqrt(math.max(0.0, residual) / norm) else 0.0
          if rel > target(j) then target(j) = rel
          j += 1
        comp += 1
      p += 1
    ErrorCurve(value, first, second)
