package scalafim.fmri.laws.profile.spike

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}

/** Row-major array helpers and a tiny in-place Cholesky for `C x C` systems. */
private[spike] object Dense:

  def toDMat(rows: Int, cols: Int, rowMajor: Array[Double]): DMat =
    val builder = DMat.newBuilder(rows, cols)
    var i = 0
    val n = rows * cols
    while i < n do
      builder.writeLinear(i, rowMajor(i))
      i += 1
    builder.result()

  def fromDMat(matrix: DMat): Array[Double] =
    val out = new Array[Double](matrix.rows * matrix.cols)
    matrix.copyRowMajorTo(out)
    out

  /** In-place Cholesky of the lower triangle of `a` (`n x n` row-major).
    * Returns false when a pivot is not positive.
    */
  def choleskyInPlace(n: Int, a: Array[Double]): Boolean =
    var j = 0
    while j < n do
      var d = a(j * n + j)
      var k = 0
      while k < j do
        val l = a(j * n + k)
        d -= l * l
        k += 1
      if !(d > 0.0) then return false
      val ljj = math.sqrt(d)
      a(j * n + j) = ljj
      var i = j + 1
      while i < n do
        var s = a(i * n + j)
        k = 0
        while k < j do
          s -= a(i * n + k) * a(j * n + k)
          k += 1
        a(i * n + j) = s / ljj
        i += 1
      j += 1
    true

  /** Solve `L L' x = b` in place with `L` from [[choleskyInPlace]]. */
  def choleskySolve(n: Int, l: Array[Double], b: Array[Double]): Unit =
    var i = 0
    while i < n do
      var s = b(i)
      var k = 0
      while k < i do
        s -= l(i * n + k) * b(k)
        k += 1
      b(i) = s / l(i * n + i)
      i += 1
    i = n - 1
    while i >= 0 do
      var s = b(i)
      var k = i + 1
      while k < n do
        s -= l(k * n + i) * b(k)
        k += 1
      b(i) = s / l(i * n + i)
      i -= 1

/** Fine-grid impulse schedule: one onset list per condition, plus the
  * sampled-row geometry (`nT` rows, `stride` fine samples per row).
  */
final case class SpikeSchedule(onsets: Array[Array[Int]], nFine: Int, nT: Int, stride: Int):
  def conditions: Int = onsets.length

object SpikeSchedule:

  def random(conditions: Int, perCondition: Int, nFine: Int, kernelLength: Int, nT: Int, stride: Int, rng: scala.util.Random): SpikeSchedule =
    val onsets = Array.tabulate(conditions) { _ =>
      Array.fill(perCondition)(rng.nextInt(nFine - kernelLength)).sorted
    }
    SpikeSchedule(onsets, nFine, nT, stride)

  /** Convolve an impulse train with `kernel(kernelOffset until kernelOffset + kernelLength)`
    * on the fine grid and write the sampled rows into column `column` of a
    * row-major matrix with `cols` columns. `fine` is scratch of length `nFine`.
    */
  def convolveSampledInto(
      schedule: SpikeSchedule,
      condition: Int,
      kernel: Array[Double],
      kernelOffset: Int,
      kernelLength: Int,
      fine: Array[Double],
      out: Array[Double],
      column: Int,
      cols: Int
  ): Unit =
    java.util.Arrays.fill(fine, 0.0)
    val events = schedule.onsets(condition)
    var e = 0
    while e < events.length do
      val start = events(e)
      val len = math.min(kernelLength, schedule.nFine - start)
      var j = 0
      while j < len do
        fine(start + j) += kernel(kernelOffset + j)
        j += 1
      e += 1
    var t = 0
    while t < schedule.nT do
      out(t * cols + column) = fine(t * schedule.stride)
      t += 1

/** Shared AR whitening through the existing `ar` module (pure AR(1)). */
final class SpikeWhitener(val nT: Int, val phi: Double):
  private val plan = WhiteningPlan.global(ArmaCoefficients.ar(phi), Vector(TimeSegment(0, nT, 0)))

  /** Whiten every column of a row-major `nT x cols` matrix. */
  def apply(cols: Int, rowMajor: Array[Double]): Array[Double] =
    val whitened = WhiteningTransform
      .matrix(plan, Dense.toDMat(nT, cols, rowMajor))
      .fold(err => throw new IllegalStateException(err.toString), identity)
    Dense.fromDMat(whitened)

object SpikeNuisance:
  /** Intercept, linear and quadratic trends, and three low-frequency cosines. */
  def columns(nT: Int): Array[Double] =
    val cols = 6
    val out = new Array[Double](nT * cols)
    var t = 0
    while t < nT do
      val x = (t.toDouble / (nT - 1)) * 2.0 - 1.0
      out(t * cols) = 1.0
      out(t * cols + 1) = x
      out(t * cols + 2) = x * x - 1.0 / 3.0
      var k = 1
      while k <= 3 do
        out(t * cols + 2 + k) = math.cos(math.Pi * k * (t + 0.5) / nT)
        k += 1
      t += 1
    out

/** Shared preparation: whitened nuisance basis `qF`, and the rank-revealing
  * factor `U R` of the whitened, nuisance-projected expanded design
  * `A_tilde = [S_1 Phi' ... S_C Phi']`, with `R` un-permuted so that
  * `D(theta) = R (I_C kron c(theta))`.
  */
final class SpikePreparation(
    val nT: Int,
    val conditions: Int,
    val m: Int,
    val rank: Int,
    val nuisanceRank: Int,
    val qF: Array[Double],
    val u: Array[Double],
    val rHat: Array[Double]):

  /** `z = U' wy`, `qy = qF' wy`; returns `e = ||wy||^2 - ||qy||^2`. */
  def project(wy: Array[Double], offset: Int, z: Array[Double], qy: Array[Double]): Double =
    val k = rank
    val f = nuisanceRank
    java.util.Arrays.fill(z, 0.0)
    java.util.Arrays.fill(qy, 0.0)
    var total = 0.0
    var t = 0
    while t < nT do
      val y = wy(offset + t)
      total += y * y
      val ub = t * k
      var i = 0
      while i < k do
        z(i) += u(ub + i) * y
        i += 1
      val qb = t * f
      i = 0
      while i < f do
        qy(i) += qF(qb + i) * y
        i += 1
      t += 1
    var q2 = 0.0
    var i = 0
    while i < f do
      q2 += qy(i) * qy(i)
      i += 1
    total - q2

  /** Project four voxels at once from a time-major block
    * (`wy(t * stride + v0 + j)`, `j < 4`), reading each row of `U` once.
    * Writes `z(zOffset + j * rank ...)` and `es(v0 + j)`.
    */
  def projectFour(wy: Array[Double], stride: Int, v0: Int, z: Array[Double], zOffset: Int, es: Array[Double], qy: Array[Double]): Unit =
    val k = rank
    val f = nuisanceRank
    java.util.Arrays.fill(z, zOffset, zOffset + 4 * k, 0.0)
    java.util.Arrays.fill(qy, 0, 4 * f, 0.0)
    var t0 = 0.0
    var t1 = 0.0
    var t2 = 0.0
    var t3 = 0.0
    var t = 0
    while t < nT do
      val row = t * stride + v0
      val y0 = wy(row)
      val y1 = wy(row + 1)
      val y2 = wy(row + 2)
      val y3 = wy(row + 3)
      t0 += y0 * y0
      t1 += y1 * y1
      t2 += y2 * y2
      t3 += y3 * y3
      val ub = t * k
      var i = 0
      while i < k do
        val ui = u(ub + i)
        z(zOffset + i) += ui * y0
        z(zOffset + k + i) += ui * y1
        z(zOffset + 2 * k + i) += ui * y2
        z(zOffset + 3 * k + i) += ui * y3
        i += 1
      val qb = t * f
      i = 0
      while i < f do
        val qi = qF(qb + i)
        qy(i) += qi * y0
        qy(f + i) += qi * y1
        qy(2 * f + i) += qi * y2
        qy(3 * f + i) += qi * y3
        i += 1
      t += 1
    var j = 0
    while j < 4 do
      var q2 = 0.0
      var i = 0
      while i < f do
        q2 += qy(j * f + i) * qy(j * f + i)
        i += 1
      es(v0 + j) = (if j == 0 then t0 else if j == 1 then t1 else if j == 2 then t2 else t3) - q2
      j += 1

  /** Remove the nuisance projection from a whitened response in place. */
  def residualizeInPlace(wy: Array[Double], qy: Array[Double]): Unit =
    val f = nuisanceRank
    var t = 0
    while t < nT do
      var acc = 0.0
      var i = 0
      while i < f do
        acc += qF(t * f + i) * qy(i)
        i += 1
      wy(t) -= acc
      t += 1

object SpikePreparation:

  def compile(schedule: SpikeSchedule, basis: SpikeKernelBasis, nuisance: Array[Double], nuisanceCols: Int, whitener: SpikeWhitener): SpikePreparation =
    val nT = schedule.nT
    val c = schedule.conditions
    val m = basis.m
    val cm = c * m
    val expanded = new Array[Double](nT * cm)
    val fine = new Array[Double](schedule.nFine)
    var cond = 0
    while cond < c do
      var j = 0
      while j < m do
        SpikeSchedule.convolveSampledInto(schedule, cond, basis.phi, j * basis.nFine, basis.nFine, fine, expanded, cond * m + j, cm)
        j += 1
      cond += 1
    val wF = whitener(nuisanceCols, nuisance)
    val wA = whitener(cm, expanded)
    val options = QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10))
    val qrF = Dense.toDMat(nT, nuisanceCols, wF).qr(options)
    val rF = qrF.diagnostics.rank.getOrElse(throw new IllegalStateException("nuisance QR reported no rank"))
    val qF = Dense.fromDMat(qrF.q.slice(0, nT, 0, rF))
    // Project the expanded design: A_proj = (I - qF qF') W A.
    val coeffs = new Array[Double](rF * cm)
    var t = 0
    while t < nT do
      var i = 0
      while i < rF do
        val q = qF(t * rF + i)
        var j = 0
        while j < cm do
          coeffs(i * cm + j) += q * wA(t * cm + j)
          j += 1
        i += 1
      t += 1
    val projected = new Array[Double](nT * cm)
    t = 0
    while t < nT do
      var j = 0
      while j < cm do
        var acc = wA(t * cm + j)
        var i = 0
        while i < rF do
          acc -= qF(t * rF + i) * coeffs(i * cm + j)
          i += 1
        projected(t * cm + j) = acc
        j += 1
      t += 1
    val qrA = Dense.toDMat(nT, cm, projected).qr(options)
    val k = qrA.diagnostics.rank.getOrElse(throw new IllegalStateException("expanded-design QR reported no rank"))
    val u = Dense.fromDMat(qrA.q.slice(0, nT, 0, k))
    val rPermuted = Dense.fromDMat(qrA.r.slice(0, k, 0, cm))
    val perm = qrA.columnPermutation.toArray
    val rHat = new Array[Double](k * cm)
    var i = 0
    while i < k do
      var j = 0
      while j < cm do
        rHat(i * cm + perm(j)) = rPermuted(i * cm + j)
        j += 1
      i += 1
    new SpikePreparation(nT, c, m, k, rF, qF, u, rHat)

/** Strict work counters: every scored node, jet, exact evaluation and family
  * evaluation is counted, whatever the wall clock says.
  */
final class SpikeCounters:
  var nodeScores: Long = 0L
  var jets: Long = 0L
  var exactEvaluations: Long = 0L
  var familyEvaluations: Long = 0L
  var smallSolves: Long = 0L
  var voxels: Long = 0L
  var fullJets: Long = 0L
  var gaussNewtonJets: Long = 0L
  var scanNanos: Long = 0L
  var jetNanos: Long = 0L
  var exactNanos: Long = 0L
  def perVoxel(value: Long): Double = if voxels == 0 then 0.0 else value.toDouble / voxels

object SpikeStatus:
  val Accepted = 0
  val WeaklyIdentified = 1
  val Boundary = 2
  val CurvatureNotPositive = 3

final class SpikeVoxelResult(conditions: Int):
  var tau: Double = 0.0
  var logSd: Double = 0.0
  val beta: Array[Double] = new Array[Double](conditions)
  var energy: Double = 0.0
  var nodeEnergy: Double = 0.0
  var nodeIndex: Int = -1
  var status: Int = SpikeStatus.Accepted
  var newtonSteps: Int = 0
  var sdTau: Double = Double.NaN
  var sigma2: Double = Double.NaN
  var predictedDecrease: Double = 0.0
  var actualDecrease: Double = 0.0

/** Condition post-solve in compact coordinates: exhaustive node scan, then
  * safeguarded Newton refinement verified by exact compact re-evaluation.
  */
final class SpikeProfileSolver(
    prep: SpikePreparation,
    basis: SpikeKernelBasis,
    domain: SpikeGaussianFamily.Domain,
    tauNodes: Int,
    logSdNodes: Int,
    maxNewtonSteps: Int = 2,
    maxExactEvaluations: Int = 6,
    refinement: Int = SpikeProfileSolver.FullNewton,
    coarseStep: Int = 1):

  private val k = prep.rank
  private val c = prep.conditions
  private val m = prep.m
  private val cm = c * m
  private val nodes = tauNodes * logSdNodes
  private val nodeTau = new Array[Double](nodes)
  private val nodeLogSd = new Array[Double](nodes)
  private val stacked = new Array[Double](nodes * c * k)
  private val tauStep = (domain.tauMax - domain.tauMin) / (tauNodes - 1)
  private val logSdStep = (domain.logSdMax - domain.logSdMin) / (logSdNodes - 1)

  // Workspaces (allocated once).
  private val kernelScratch = new Array[Double](SpikeGaussianFamily.JetComponents * basis.nFine)
  private val coeff = new Array[Double](SpikeGaussianFamily.JetComponents * m)
  private val design = new Array[Double](SpikeGaussianFamily.JetComponents * k * c)
  private val gram = new Array[Double](c * c)
  private val factor = new Array[Double](c * c)
  private val b = new Array[Double](c)
  private val w = new Array[Double](c)
  private val bp = new Array[Double](2 * c)
  private val bpq = new Array[Double](3 * c)
  private val gp = new Array[Double](2 * c * c)
  private val gpq = new Array[Double](3 * c * c)
  private val rp = new Array[Double](2 * c)
  private val sp = new Array[Double](2 * c)
  private val tmp = new Array[Double](c)
  private val nodeEnergy = new Array[Double](nodes)
  private val betaAccepted = new Array[Double](c)
  private val residual = new Array[Double](k)
  private val jac = new Array[Double](2 * k)
  /** Design jets at every node (`nodes x 6 x K x C`): the first jet of every
    * voxel is at a node, so it costs only small Gram products.
    */
  private val nodeDesignJets = new Array[Double](nodes * SpikeGaussianFamily.JetComponents * k * c)

  locally {
    var g = 0
    var a = 0
    while a < tauNodes do
      var bb = 0
      while bb < logSdNodes do
        nodeTau(g) = domain.tauMin + a * tauStep
        nodeLogSd(g) = domain.logSdMin + bb * logSdStep
        g += 1
        bb += 1
      a += 1
    g = 0
    while g < nodes do
      basis.coefficientJetInto(nodeTau(g), nodeLogSd(g), kernelScratch, coeff)
      var comp = 0
      while comp < SpikeGaussianFamily.JetComponents do
        designFromCoefficients(comp, comp)
        comp += 1
      System.arraycopy(design, 0, nodeDesignJets, g * SpikeGaussianFamily.JetComponents * k * c, SpikeGaussianFamily.JetComponents * k * c)
      gramOf(0, gram)
      System.arraycopy(gram, 0, factor, 0, c * c)
      require(Dense.choleskyInPlace(c, factor), s"node $g has a singular compact Gram")
      // Q_g' rows: q_kk = L^-1 d_kk for each compact row.
      var kk = 0
      while kk < k do
        var i = 0
        while i < c do
          tmp(i) = design(kk * c + i)
          i += 1
        // forward substitution with L
        i = 0
        while i < c do
          var s = tmp(i)
          var j = 0
          while j < i do
            s -= factor(i * c + j) * tmp(j)
            j += 1
          tmp(i) = s / factor(i * c + i)
          i += 1
        i = 0
        while i < c do
          stacked((g * c + i) * k + kk) = tmp(i)
          i += 1
        kk += 1
      g += 1
  }

  def nodeCount: Int = nodes
  def compactRank: Int = k

  /** `D_comp = R (I kron c_comp)` for `comp`, into `design(comp * k * c ...)`. */
  private def designFromCoefficients(comp: Int, coeffComp: Int): Unit =
    val base = comp * k * c
    val cb = coeffComp * m
    var kk = 0
    while kk < k do
      val rb = kk * cm
      var cond = 0
      while cond < c do
        var acc = 0.0
        var j = 0
        while j < m do
          acc += prep.rHat(rb + cond * m + j) * coeff(cb + j)
          j += 1
        design(base + kk * c + cond) = acc
        cond += 1
      kk += 1

  private def gramOf(comp: Int, out: Array[Double]): Unit =
    crossGram(comp, comp, out, 0)

  /** `out = D_a' D_b` (`c x c`) into `out(offset ...)`. */
  private def crossGram(compA: Int, compB: Int, out: Array[Double], offset: Int): Unit =
    val ba = compA * k * c
    val bb = compB * k * c
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        var acc = 0.0
        var kk = 0
        while kk < k do
          acc += design(ba + kk * c + i) * design(bb + kk * c + j)
          kk += 1
        out(offset + i * c + j) = acc
        j += 1
      i += 1

  private def projectResponse(comp: Int, z: Array[Double], out: Array[Double], offset: Int): Unit =
    val base = comp * k * c
    var i = 0
    while i < c do
      var acc = 0.0
      var kk = 0
      while kk < k do
        acc += design(base + kk * c + i) * z(kk)
        kk += 1
      out(offset + i) = acc
      i += 1

  /** Exact compact energy at a continuous shape; leaves `w` = amplitudes. */
  private def exactEnergy(tau: Double, logSd: Double, z: Array[Double], e: Double, counters: SpikeCounters): Double =
    val t0 = System.nanoTime()
    counters.exactEvaluations += 1
    counters.familyEvaluations += 1
    basis.coefficientsInto(tau, logSd, kernelScratch, coeff)
    designFromCoefficients(0, 0)
    val energy = energyFromDesign(z, e, counters)
    counters.exactNanos += System.nanoTime() - t0
    energy

  private def energyFromDesign(z: Array[Double], e: Double, counters: SpikeCounters): Double =
    gramOf(0, gram)
    projectResponse(0, z, b, 0)
    System.arraycopy(gram, 0, factor, 0, c * c)
    if !Dense.choleskyInPlace(c, factor) then return Double.PositiveInfinity
    System.arraycopy(b, 0, w, 0, c)
    Dense.choleskySolve(c, factor, w)
    counters.smallSolves += 1
    var bw = 0.0
    var i = 0
    while i < c do
      bw += b(i) * w(i)
      i += 1
    e - bw

  private def quadratic(matrix: Array[Double], offset: Int, x: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        acc += x(i) * matrix(offset + i * c + j) * x(j)
        j += 1
      i += 1
    acc

  /** Gauss-Newton jet from value and first derivatives only: exact energy and
    * gradient, curvature `2 J_p'(I - P) J_q` with `J_p = D_p w`.
    */
  private def jetGaussNewton(tau: Double, logSd: Double, z: Array[Double], e: Double, grad: Array[Double], hess: Array[Double], counters: SpikeCounters, updateHessian: Boolean): Double =
    val t0 = System.nanoTime()
    counters.jets += 1
    counters.gaussNewtonJets += 1
    counters.familyEvaluations += 1
    basis.coefficientJetInto(tau, logSd, kernelScratch, coeff, 3)
    var comp = 0
    while comp < 3 do
      designFromCoefficients(comp, comp)
      comp += 1
    val energy = energyFromDesign(z, e, counters) // w, factor valid
    if energy.isInfinite then
      grad(0) = Double.NaN
      grad(1) = Double.NaN
      counters.jetNanos += System.nanoTime() - t0
      return energy
    // residual r = z - D w
    var kk = 0
    while kk < k do
      var acc = z(kk)
      var i = 0
      while i < c do
        acc -= design(kk * c + i) * w(i)
        i += 1
      residual(kk) = acc
      kk += 1
    var p = 0
    while p < 2 do
      val base = (1 + p) * k * c
      var g = 0.0
      kk = 0
      while kk < k do
        var acc = 0.0
        var i = 0
        while i < c do
          acc += design(base + kk * c + i) * w(i)
          i += 1
        jac(p * k + kk) = acc
        g += acc * residual(kk)
        kk += 1
      grad(p) = -2.0 * g
      // project J_p off col(D): J_p - D G^-1 D' J_p
      var i = 0
      while i < c do
        var acc = 0.0
        kk = 0
        while kk < k do
          acc += design(kk * c + i) * jac(p * k + kk)
          kk += 1
        tmp(i) = acc
        i += 1
      Dense.choleskySolve(c, factor, tmp)
      counters.smallSolves += 1
      kk = 0
      while kk < k do
        var acc = 0.0
        i = 0
        while i < c do
          acc += design(kk * c + i) * tmp(i)
          i += 1
        jac(p * k + kk) -= acc
        kk += 1
      p += 1
    if updateHessian then
      var h00 = 0.0
      var h01 = 0.0
      var h11 = 0.0
      kk = 0
      while kk < k do
        val a = jac(kk)
        val b2 = jac(k + kk)
        h00 += a * a
        h01 += a * b2
        h11 += b2 * b2
        kk += 1
      hess(0) = 2.0 * h00
      hess(1) = 2.0 * h01
      hess(2) = 2.0 * h01
      hess(3) = 2.0 * h11
    counters.jetNanos += System.nanoTime() - t0
    energy

  /** Continuous-step jet according to the refinement mode. */
  private def refinementJet(tau: Double, logSd: Double, z: Array[Double], e: Double, grad: Array[Double], hess: Array[Double], counters: SpikeCounters): Double =
    refinement match
      case SpikeProfileSolver.GaussNewton    => jetGaussNewton(tau, logSd, z, e, grad, hess, counters, updateHessian = true)
      case SpikeProfileSolver.FrozenHessian  => jetGaussNewton(tau, logSd, z, e, grad, hess, counters, updateHessian = false)
      case _                                 => jet(tau, logSd, z, e, grad, hess, counters)

  private val grad = new Array[Double](2)
  private val hess = new Array[Double](4)
  private val sym = new Array[Double](c * c)
  // second-order jet components and the first-order components they pair
  private val pairComponent = Array(3, 4, 5)
  private val pairA = Array(1, 1, 2)
  private val pairB = Array(1, 2, 2)

  /** `dst = src + src'` for a `c x c` block. */
  private def symmetricSum(src: Array[Double], srcOffset: Int, dst: Array[Double], dstOffset: Int): Unit =
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        sym(i * c + j) = src(srcOffset + i * c + j) + src(srcOffset + j * c + i)
        j += 1
      i += 1
    System.arraycopy(sym, 0, dst, dstOffset, c * c)

  /** Energy, gradient and Hessian at a shape. Returns energy; fills `grad`
    * (2) and `hess` (row-major 2x2). Formulas: with `w = G^-1 b`,
    * `E_p = -2 b_p'w + w'G_p w` and
    * `E_pq = -2 b_pq'w + w'G_pq w - 2 r_p'G^-1 r_q`, `r_p = b_p - G_p w`;
    * `s = e` is shape-free in the condition regime.
    */
  private def jet(tau: Double, logSd: Double, z: Array[Double], e: Double, grad: Array[Double], hess: Array[Double], counters: SpikeCounters): Double =
    val t0 = System.nanoTime()
    counters.jets += 1
    counters.fullJets += 1
    counters.familyEvaluations += 1
    basis.coefficientJetInto(tau, logSd, kernelScratch, coeff)
    var comp = 0
    while comp < SpikeGaussianFamily.JetComponents do
      designFromCoefficients(comp, comp)
      comp += 1
    val energy = jetFromDesign(z, e, grad, hess, counters)
    counters.jetNanos += System.nanoTime() - t0
    energy

  /** Jet at node `g` from the precomputed design bank. */
  private def jetAtNode(g: Int, z: Array[Double], e: Double, grad: Array[Double], hess: Array[Double], counters: SpikeCounters): Double =
    val t0 = System.nanoTime()
    counters.jets += 1
    System.arraycopy(nodeDesignJets, g * SpikeGaussianFamily.JetComponents * k * c, design, 0, SpikeGaussianFamily.JetComponents * k * c)
    val energy = jetFromDesign(z, e, grad, hess, counters)
    counters.jetNanos += System.nanoTime() - t0
    energy

  private def jetFromDesign(z: Array[Double], e: Double, grad: Array[Double], hess: Array[Double], counters: SpikeCounters): Double =
    val energy = energyFromDesign(z, e, counters) // leaves w and the Cholesky factor of G
    if energy.isInfinite then
      grad(0) = Double.NaN
      grad(1) = Double.NaN
      return energy
    // First-order blocks: b_p = D_p'z, G_p = D_p'D + D'D_p.
    var p = 0
    while p < 2 do
      projectResponse(1 + p, z, bp, p * c)
      crossGram(1 + p, 0, gram, 0)
      symmetricSum(gram, 0, gp, p * c * c)
      p += 1
    // Second-order blocks: b_pq = D_pq'z, G_pq = D_pq'D + D'D_pq + D_p'D_q + D_q'D_p.
    var idx = 0
    while idx < 3 do
      projectResponse(pairComponent(idx), z, bpq, idx * c)
      crossGram(pairComponent(idx), 0, gram, 0)
      symmetricSum(gram, 0, gpq, idx * c * c)
      crossGram(pairA(idx), pairB(idx), gram, 0)
      symmetricSum(gram, 0, sym, 0)
      var i = 0
      while i < c * c do
        gpq(idx * c * c + i) += sym(i)
        i += 1
      idx += 1
    // Gradient and the solved residual directions.
    p = 0
    while p < 2 do
      var g = 0.0
      var i = 0
      while i < c do
        g += -2.0 * bp(p * c + i) * w(i)
        i += 1
      g += quadratic(gp, p * c * c, w)
      grad(p) = g
      i = 0
      while i < c do
        var acc = bp(p * c + i)
        var j = 0
        while j < c do
          acc -= gp(p * c * c + i * c + j) * w(j)
          j += 1
        rp(p * c + i) = acc
        tmp(i) = acc
        i += 1
      Dense.choleskySolve(c, factor, tmp)
      counters.smallSolves += 1
      System.arraycopy(tmp, 0, sp, p * c, c)
      p += 1
    idx = 0
    while idx < 3 do
      val pa = pairA(idx) - 1
      val pb = pairB(idx) - 1
      var h = 0.0
      var i = 0
      while i < c do
        h += -2.0 * bpq(idx * c + i) * w(i)
        h -= 2.0 * rp(pa * c + i) * sp(pb * c + i)
        i += 1
      h += quadratic(gpq, idx * c * c, w)
      idx match
        case 0 => hess(0) = h
        case 1 =>
          hess(1) = h
          hess(2) = h
        case _ => hess(3) = h
      idx += 1
    energy

  /** Test hook: exact compact energy at a shape; amplitudes in [[lastAmplitudes]]. */
  def exactEnergyForTest(tau: Double, logSd: Double, z: Array[Double], e: Double, counters: SpikeCounters): Double =
    exactEnergy(tau, logSd, z, e, counters)

  def lastAmplitudes: Array[Double] = w

  private def scoreNode(g: Int, z: Array[Double], e: Double): Double =
    var fit = 0.0
    var i = 0
    while i < c do
      val row = (g * c + i) * k
      var acc = 0.0
      var kk = 0
      while kk < k do
        acc += stacked(row + kk) * z(kk)
        kk += 1
      fit += acc * acc
      i += 1
    e - fit

  def solve(z: Array[Double], e: Double, counters: SpikeCounters, result: SpikeVoxelResult): Unit =
    counters.voxels += 1
    val tScan = System.nanoTime()
    // 1. node scan: exhaustive, or coarse-then-neighbourhood when coarseStep > 1
    var best = -1
    var bestE = Double.PositiveInfinity
    var scored = 0
    if coarseStep <= 1 then
      var g = 0
      while g < nodes do
        val en = scoreNode(g, z, e)
        nodeEnergy(g) = en
        if en < bestE then
          bestE = en
          best = g
        g += 1
      scored = nodes
    else
      java.util.Arrays.fill(nodeEnergy, Double.NaN)
      var a = 0
      while a < tauNodes do
        var bb = 0
        while bb < logSdNodes do
          val g = a * logSdNodes + bb
          val en = scoreNode(g, z, e)
          nodeEnergy(g) = en
          scored += 1
          if en < bestE then
            bestE = en
            best = g
          bb += coarseStep
        a += coarseStep
      val ca = best / logSdNodes
      val cb = best % logSdNodes
      var a2 = math.max(0, ca - coarseStep + 1)
      while a2 <= math.min(tauNodes - 1, ca + coarseStep - 1) do
        var b2 = math.max(0, cb - coarseStep + 1)
        while b2 <= math.min(logSdNodes - 1, cb + coarseStep - 1) do
          val g = a2 * logSdNodes + b2
          if nodeEnergy(g).isNaN then
            val en = scoreNode(g, z, e)
            nodeEnergy(g) = en
            scored += 1
            if en < bestE then
              bestE = en
              best = g
          b2 += 1
        a2 += 1
    counters.nodeScores += scored
    counters.scanNanos += System.nanoTime() - tScan
    result.nodeIndex = best
    result.nodeEnergy = bestE
    var tau = nodeTau(best)
    var v = nodeLogSd(best)
    result.predictedDecrease = 0.0
    result.actualDecrease = 0.0
    var exactUsed = 0
    var steps = 0
    // The first jet sits at a node and comes from the bank; it yields the node
    // energy, amplitudes, gradient and full Hessian.
    var current = jetAtNode(best, z, e, grad, hess, counters)
    System.arraycopy(w, 0, betaAccepted, 0, c)
    var det = hess(0) * hess(3) - hess(1) * hess(2)
    val curvatureOk = current.isFinite && hess(0) > 0.0 && det > 0.0
    var continue = curvatureOk
    while continue && steps < maxNewtonSteps do
      det = hess(0) * hess(3) - hess(1) * hess(2)
      if !(hess(0) > 0.0 && det > 0.0) then continue = false
      else
        var dTau = -(hess(3) * grad(0) - hess(1) * grad(1)) / det
        var dV = -(-hess(2) * grad(0) + hess(0) * grad(1)) / det
        var alpha = 1.0
        if dTau > 0.0 then alpha = math.min(alpha, (domain.tauMax - tau) / dTau)
        if dTau < 0.0 then alpha = math.min(alpha, (domain.tauMin - tau) / dTau)
        if dV > 0.0 then alpha = math.min(alpha, (domain.logSdMax - v) / dV)
        if dV < 0.0 then alpha = math.min(alpha, (domain.logSdMin - v) / dV)
        dTau *= alpha
        dV *= alpha
        val gd = grad(0) * dTau + grad(1) * dV
        val dHd = hess(0) * dTau * dTau + 2.0 * hess(1) * dTau * dV + hess(3) * dV * dV
        if math.abs(dTau) <= 1e-7 && math.abs(dV) <= 1e-7 then continue = false
        else
          val lastStep = steps == maxNewtonSteps - 1
          var accepted = false
          var jetAtAccepted = false
          var scale = 1.0
          var tries = 0
          while !accepted && tries < 4 && exactUsed < maxExactEvaluations do
            val tTau = domain.clampTau(tau + scale * dTau)
            val tV = domain.clampLogSd(v + scale * dV)
            // A further step needs a jet at the trial point anyway, and a jet
            // computes the exact energy, so it doubles as the verification.
            val useJet = !lastStep && tries == 0
            val trial =
              if useJet then refinementJet(tTau, tV, z, e, grad, hess, counters)
              else
                exactUsed += 1
                exactEnergy(tTau, tV, z, e, counters)
            if trial < current then
              accepted = true
              jetAtAccepted = useJet
              result.actualDecrease += current - trial
              result.predictedDecrease += -(scale * gd + 0.5 * scale * scale * dHd)
              current = trial
              tau = tTau
              v = tV
              System.arraycopy(w, 0, betaAccepted, 0, c)
            else
              scale *= 0.5
              tries += 1
          if accepted then
            steps += 1
            if !lastStep && !jetAtAccepted then
              // backtracked onto an exact-only evaluation: recover a jet there
              val recovered = refinementJet(tau, v, z, e, grad, hess, counters)
              if !(recovered <= current + 1e-12 * math.abs(current)) then continue = false
          else continue = false
    if steps == 0 && !curvatureOk then
      // derivative-free fallback: parabolic interpolation on the node grid
      val a = best / logSdNodes
      val bb = best % logSdNodes
      var dTau = 0.0
      var dV = 0.0
      if a > 0 && a < tauNodes - 1 then
        val em = nodeEnergy((a - 1) * logSdNodes + bb)
        val ep = nodeEnergy((a + 1) * logSdNodes + bb)
        val den = em - 2.0 * bestE + ep
        if den > 0.0 && !em.isNaN && !ep.isNaN then dTau = math.max(-tauStep, math.min(tauStep, 0.5 * tauStep * (em - ep) / den))
      if bb > 0 && bb < logSdNodes - 1 then
        val em = nodeEnergy(a * logSdNodes + bb - 1)
        val ep = nodeEnergy(a * logSdNodes + bb + 1)
        val den = em - 2.0 * bestE + ep
        if den > 0.0 && !em.isNaN && !ep.isNaN then dV = math.max(-logSdStep, math.min(logSdStep, 0.5 * logSdStep * (em - ep) / den))
      if (dTau != 0.0 || dV != 0.0) && exactUsed < maxExactEvaluations then
        val trial = exactEnergy(domain.clampTau(tau + dTau), domain.clampLogSd(v + dV), z, e, counters)
        exactUsed += 1
        if trial < current then
          current = trial
          tau = domain.clampTau(tau + dTau)
          v = domain.clampLogSd(v + dV)
          System.arraycopy(w, 0, betaAccepted, 0, c)
    result.tau = tau
    result.logSd = v
    result.energy = current
    result.newtonSteps = steps
    System.arraycopy(betaAccepted, 0, result.beta, 0, c)
    // status from the last computed curvature (labelled conditional)
    val df = prep.nT - prep.nuisanceRank - c - SpikeGaussianFamily.Dimension
    result.sigma2 = current / df
    det = hess(0) * hess(3) - hess(1) * hess(2)
    if hess(0) > 0.0 && det > 0.0 then
      // Var(tau) ~ 2 sigma^2 [H^-1]_11 for the energy Hessian (E/(2 sigma^2) is the negative log-likelihood)
      result.sdTau = math.sqrt(2.0 * result.sigma2 * hess(3) / det)
    else result.sdTau = Double.NaN
    result.status =
      if !curvatureOk && steps == 0 then SpikeStatus.CurvatureNotPositive
      else if domain.onBoundary(tau, v) then SpikeStatus.Boundary
      else if !(result.sdTau <= 0.5) then SpikeStatus.WeaklyIdentified
      else SpikeStatus.Accepted

object SpikeProfileSolver:
  /** Continuous refinement jets carry the full observed Hessian (six components). */
  val FullNewton: Int = 0
  /** Continuous refinement jets carry value and first derivatives only (three components). */
  val GaussNewton: Int = 1
  /** First-order jets for the gradient; the node's exact observed Hessian is kept for every step. */
  val FrozenHessian: Int = 2
