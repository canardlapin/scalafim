package scalafim.fmri.laws.profile.spike

/** Synthetic cohort: `y = F gamma + B(theta) beta + AR(1) noise`, voxel-major. */
final class SpikeCohort(
    val voxels: Int,
    val nT: Int,
    val conditions: Int,
    val y: Array[Double],
    val trueTau: Array[Double],
    val trueLogSd: Array[Double],
    val trueBeta: Array[Double],
    val snr: Double)

object SpikeCohort:

  def generate(
      schedule: SpikeSchedule,
      nuisance: Array[Double],
      nuisanceCols: Int,
      fineTimes: Array[Double],
      truthTauMin: Double,
      truthTauMax: Double,
      truthLogSdMin: Double,
      truthLogSdMax: Double,
      voxels: Int,
      snr: Double,
      arPhi: Double,
      rng: scala.util.Random
  ): SpikeCohort =
    val nT = schedule.nT
    val c = schedule.conditions
    val y = new Array[Double](voxels * nT)
    val tau = new Array[Double](voxels)
    val logSd = new Array[Double](voxels)
    val beta = new Array[Double](voxels * c)
    val kernel = new Array[Double](fineTimes.length)
    val fine = new Array[Double](schedule.nFine)
    val design = new Array[Double](nT * c)
    val signal = new Array[Double](nT)
    var v = 0
    while v < voxels do
      tau(v) = truthTauMin + rng.nextDouble() * (truthTauMax - truthTauMin)
      logSd(v) = truthLogSdMin + rng.nextDouble() * (truthLogSdMax - truthLogSdMin)
      SpikeGaussianFamily.valueInto(fineTimes, tau(v), logSd(v), kernel)
      var cond = 0
      while cond < c do
        SpikeSchedule.convolveSampledInto(schedule, cond, kernel, 0, kernel.length, fine, design, cond, c)
        beta(v * c + cond) = (if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble())
        cond += 1
      var sum = 0.0
      var sum2 = 0.0
      var t = 0
      while t < nT do
        var acc = 0.0
        cond = 0
        while cond < c do
          acc += design(t * c + cond) * beta(v * c + cond)
          cond += 1
        signal(t) = acc
        sum += acc
        sum2 += acc * acc
        t += 1
      val signalSd = math.sqrt(math.max(1e-12, sum2 / nT - (sum / nT) * (sum / nT)))
      val noiseSd = signalSd / snr
      val innovationSd = noiseSd * math.sqrt(1.0 - arPhi * arPhi)
      var noise = rng.nextGaussian() * noiseSd
      t = 0
      while t < nT do
        if t > 0 then noise = arPhi * noise + rng.nextGaussian() * innovationSd
        var nuis = 0.0
        var f = 0
        while f < nuisanceCols do
          nuis += nuisance(t * nuisanceCols + f) * (if f == 0 then 10.0 * signalSd else signalSd) * (if f == 0 then 1.0 else 0.3)
          f += 1
        y(v * nT + t) = signal(t) + noise + nuis
        t += 1
      v += 1
    new SpikeCohort(voxels, nT, c, y, tau, logSd, beta, snr)

final class SpikeOracleResult(conditions: Int):
  var tau: Double = 0.0
  var logSd: Double = 0.0
  var energy: Double = 0.0
  val beta: Array[Double] = new Array[Double](conditions)
  var exactEvaluations: Int = 0

/** Dense time-domain oracle: the exact kernel (no basis) convolved per
  * shape, whitened and nuisance-projected, scored on a fine shape grid, then
  * refined by a shrinking compass search with exact evaluations.
  */
final class SpikeOracle(
    schedule: SpikeSchedule,
    prep: SpikePreparation,
    whitener: SpikeWhitener,
    fineTimes: Array[Double],
    domain: SpikeGaussianFamily.Domain,
    tauGrid: Int,
    logSdGrid: Int,
    compassLevels: Int = 7):

  private val nT = schedule.nT
  private val c = schedule.conditions
  private val shapes = tauGrid * logSdGrid
  private val gridTau = new Array[Double](shapes)
  private val gridLogSd = new Array[Double](shapes)
  private val tauStep = (domain.tauMax - domain.tauMin) / (tauGrid - 1)
  private val logSdStep = (domain.logSdMax - domain.logSdMin) / (logSdGrid - 1)
  /** Orthonormal projected designs, shape-major: `q((s * nT + t) * c + i)`. */
  private val q = new Array[Double](shapes * nT * c)
  private val kernel = new Array[Double](fineTimes.length)
  private val fine = new Array[Double](schedule.nFine)
  private val design = new Array[Double](nT * c)
  private val gram = new Array[Double](c * c)
  private val b = new Array[Double](c)
  private val qy = new Array[Double](prep.nuisanceRank)
  private val residual = new Array[Double](nT)
  private val energies = new Array[Double](shapes)

  locally {
    val all = new Array[Double](nT * c * shapes)
    var s = 0
    var a = 0
    while a < tauGrid do
      var bb = 0
      while bb < logSdGrid do
        gridTau(s) = domain.tauMin + a * tauStep
        gridLogSd(s) = domain.logSdMin + bb * logSdStep
        SpikeGaussianFamily.valueInto(fineTimes, gridTau(s), gridLogSd(s), kernel)
        var cond = 0
        while cond < c do
          SpikeSchedule.convolveSampledInto(schedule, cond, kernel, 0, kernel.length, fine, all, s * c + cond, c * shapes)
          cond += 1
        s += 1
        bb += 1
      a += 1
    val whitened = whitener(c * shapes, all)
    s = 0
    while s < shapes do
      var t = 0
      while t < nT do
        var cond = 0
        while cond < c do
          design(t * c + cond) = whitened(t * (c * shapes) + s * c + cond)
          cond += 1
        t += 1
      projectAndOrthonormalise(design)
      System.arraycopy(design, 0, q, s * nT * c, nT * c)
      s += 1
  }

  /** Remove the nuisance span and orthonormalise the columns in place. */
  private def projectAndOrthonormalise(d: Array[Double]): Unit =
    val f = prep.nuisanceRank
    var cond = 0
    while cond < c do
      java.util.Arrays.fill(qy, 0.0)
      var t = 0
      while t < nT do
        var i = 0
        while i < f do
          qy(i) += prep.qF(t * f + i) * d(t * c + cond)
          i += 1
        t += 1
      t = 0
      while t < nT do
        var acc = 0.0
        var i = 0
        while i < f do
          acc += prep.qF(t * f + i) * qy(i)
          i += 1
        d(t * c + cond) -= acc
        t += 1
      cond += 1
    // Gram-Schmidt (modified) on the C columns.
    cond = 0
    while cond < c do
      var prev = 0
      while prev < cond do
        var dot = 0.0
        var t = 0
        while t < nT do
          dot += d(t * c + prev) * d(t * c + cond)
          t += 1
        t = 0
        while t < nT do
          d(t * c + cond) -= dot * d(t * c + prev)
          t += 1
        prev += 1
      var norm = 0.0
      var t = 0
      while t < nT do
        norm += d(t * c + cond) * d(t * c + cond)
        t += 1
      val inv = 1.0 / math.sqrt(norm)
      t = 0
      while t < nT do
        d(t * c + cond) *= inv
        t += 1
      cond += 1

  /** Exact projected energy at a continuous shape; leaves amplitudes in `b`. */
  private def exact(tau: Double, logSd: Double, e: Double, out: SpikeOracleResult): Double =
    out.exactEvaluations += 1
    SpikeGaussianFamily.valueInto(fineTimes, tau, logSd, kernel)
    var cond = 0
    while cond < c do
      SpikeSchedule.convolveSampledInto(schedule, cond, kernel, 0, kernel.length, fine, design, cond, c)
      cond += 1
    val whitened = whitener(c, design)
    System.arraycopy(whitened, 0, design, 0, nT * c)
    // project nuisance only (keep raw columns for least squares)
    val f = prep.nuisanceRank
    cond = 0
    while cond < c do
      java.util.Arrays.fill(qy, 0.0)
      var t = 0
      while t < nT do
        var i = 0
        while i < f do
          qy(i) += prep.qF(t * f + i) * design(t * c + cond)
          i += 1
        t += 1
      t = 0
      while t < nT do
        var acc = 0.0
        var i = 0
        while i < f do
          acc += prep.qF(t * f + i) * qy(i)
          i += 1
        design(t * c + cond) -= acc
        t += 1
      cond += 1
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        var acc = 0.0
        var t = 0
        while t < nT do
          acc += design(t * c + i) * design(t * c + j)
          t += 1
        gram(i * c + j) = acc
        j += 1
      var acc = 0.0
      var t = 0
      while t < nT do
        acc += design(t * c + i) * residual(t)
        t += 1
      b(i) = acc
      i += 1
    if !Dense.choleskyInPlace(c, gram) then return Double.PositiveInfinity
    val bw = new Array[Double](c)
    System.arraycopy(b, 0, bw, 0, c)
    Dense.choleskySolve(c, gram, bw)
    var fit = 0.0
    i = 0
    while i < c do
      fit += b(i) * bw(i)
      b(i) = bw(i)
      i += 1
    e - fit

  /** Test hook: exact direct energy at a shape for a whitened response. */
  def energyAtForTest(wy: Array[Double], tau: Double, logSd: Double, out: SpikeOracleResult): Double =
    System.arraycopy(wy, 0, residual, 0, nT)
    val e = prep.project(residual, 0, new Array[Double](prep.rank), qy)
    prep.residualizeInPlace(residual, qy)
    val energy = exact(tau, logSd, e, out)
    System.arraycopy(b, 0, out.beta, 0, c)
    energy

  /** Solve one voxel from its whitened response. */
  def solve(wy: Array[Double], offset: Int, out: SpikeOracleResult): Unit =
    out.exactEvaluations = 0
    System.arraycopy(wy, offset, residual, 0, nT)
    val e = prep.project(residual, 0, new Array[Double](prep.rank), qy)
    prep.residualizeInPlace(residual, qy)
    var best = -1
    var bestE = Double.PositiveInfinity
    var s = 0
    while s < shapes do
      var fit = 0.0
      var i = 0
      while i < c do
        var acc = 0.0
        var t = 0
        while t < nT do
          acc += q((s * nT + t) * c + i) * residual(t)
          t += 1
        fit += acc * acc
        i += 1
      energies(s) = e - fit
      if energies(s) < bestE then
        bestE = energies(s)
        best = s
      s += 1
    var tau = gridTau(best)
    var v = gridLogSd(best)
    var current = exact(tau, v, e, out)
    System.arraycopy(b, 0, out.beta, 0, c)
    var hTau = tauStep / 2.0
    var hV = logSdStep / 2.0
    var level = 0
    while level < compassLevels do
      var improved = false
      var dir = 0
      while dir < 4 do
        val tTau = domain.clampTau(tau + (if dir == 0 then hTau else if dir == 1 then -hTau else 0.0))
        val tV = domain.clampLogSd(v + (if dir == 2 then hV else if dir == 3 then -hV else 0.0))
        if tTau != tau || tV != v then
          val trial = exact(tTau, tV, e, out)
          if trial < current then
            current = trial
            tau = tTau
            v = tV
            System.arraycopy(b, 0, out.beta, 0, c)
            improved = true
        dir += 1
      if !improved then
        hTau /= 2.0
        hV /= 2.0
        level += 1
    out.tau = tau
    out.logSd = v
    out.energy = current
