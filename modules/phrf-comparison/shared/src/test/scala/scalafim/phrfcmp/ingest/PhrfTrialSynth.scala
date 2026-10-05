// Declared in the ingest package so the private[ingest] FitInputs constructor is reachable (as ConditionSynth is): the S5
// tests need trial-cell inputs whose truth is the PHRF model itself (a Gaussian kernel, per-voxel condition means and
// stimulus-persistent deviations) so that recovery, E-trial conversion and the held-out score have a known answer.
package scalafim.phrfcmp.ingest

import scalafim.fmri.hrf.{HrfFunctions, Lag}

/** Trial-cell `FitInputs` generated from the PHRF model: `a_i * gaussianPdf(t - onset_i; tau, sd)` plus AR(1) noise. */
object PhrfTrialSynth:

  final case class Truth(tau: Double, sd: Double, condMeans: Vector[Double])

  final case class Spec(
      truths: Vector[Truth],
      runs: Int = 4,
      runLen: Int = 100,
      stimPerCond: Int = 3,
      /** SD of the stimulus-persistent deviation (the same deviation every time a stimulus repeats). */
      devSd: Double = 0.0,
      noiseSd: Double = 0.0,
      rho: Double = 0.3,
      seed: Long = 1L,
      first: Double = 4.0,
      /** a multiple of 0.2 s so the 0.2 s lowering grid carries no onset quantisation */
      gap: Double = 8.4,
      /** append, per run, a copy of the linear nuisance column: the baseline is then rank deficient (a dataset-level PHRF refusal) */
      duplicateLinear: Boolean = false
  )

  /** `trialAmp(v)(i)` is the truth amplitude in library (density) units, `eTrial(v)(i)` the true trial peak height. */
  final case class Data(inputs: FitInputs, trialAmp: Vector[Vector[Double]], eTrial: Vector[Vector[Double]])

  final class Lcg(var s: Long):
    def next(): Double =
      s = s * 6364136223846793005L + 1442695040888963407L
      ((s >>> 11).toDouble + 0.5) / 9007199254740992.0
    def gauss(): Double = math.sqrt(-2.0 * math.log(next())) * math.cos(2.0 * math.Pi * next())

  private def mat(r: Int, c: Int, d: Array[Double]): Matrix = Matrix.of(r, c, d).fold(e => throw new AssertionError(e.message), identity)

  def generate(spec: Spec): Data =
    val rng = new Lcg(spec.seed * 7919L + 31L)
    val nCond = 3
    val perRun = nCond * spec.stimPerCond
    val nEv = spec.runs * perRun
    val t = spec.runs * spec.runLen
    val v = spec.truths.length
    val pPer = if spec.duplicateLinear then 7 else 6
    val p = pPer * spec.runs
    val runId = Array.tabulate(t)(_ / spec.runLen)
    val sampleTime = Array.tabulate(t)(i => (i % spec.runLen).toDouble)
    val evRun = Array.tabulate(nEv)(_ / perRun)
    val evStim = new Array[Int](nEv)
    var r = 0
    while r < spec.runs do
      val order = Array.tabulate(perRun)(identity)
      var i = perRun - 1
      while i > 0 do
        val j = (rng.next() * (i + 1)).toInt
        val tmp = order(i); order(i) = order(j); order(j) = tmp
        i -= 1
      i = 0
      while i < perRun do
        evStim(r * perRun + i) = order(i)
        i += 1
      r += 1
    val evCond = evStim.map(_ / spec.stimPerCond)
    val evOnset = Array.tabulate(nEv)(e => spec.first + spec.gap * (e % perRun))
    val dev = Array.tabulate(v, perRun)((_, _) => spec.devSd * rng.gauss())
    val amp = Vector.tabulate(v)(j => Vector.tabulate(nEv)(e => spec.truths(j).condMeans(evCond(e)) + dev(j)(evStim(e))))
    val nuis = new Array[Double](t * p)
    var i = 0
    while i < t do
      val run = runId(i)
      val n = spec.runLen
      val tt = i % n
      val lin = -1.0 + 2.0 * tt / (n - 1)
      val col0 = run * pPer
      nuis(i * p + col0) = 1.0
      nuis(i * p + col0 + 1) = lin
      nuis(i * p + col0 + 2) = lin * lin - 1.0 / 3.0
      if spec.duplicateLinear then nuis(i * p + col0 + 6) = lin
      var k = 1
      while k <= 3 do
        nuis(i * p + col0 + 2 + k) = math.cos(math.Pi * k * (tt + 0.5) / n)
        k += 1
      i += 1
    val y = new Array[Double](v * t)
    var vox = 0
    while vox < v do
      val tr = spec.truths(vox)
      var prev = 0.0
      var s = 0
      while s < t do
        var sig = 3.0 + 0.5 * nuis(s * p + runId(s) * pPer + 1)
        var e = 0
        while e < nEv do
          if evRun(e) == runId(s) && sampleTime(s) >= evOnset(e) then
            sig += amp(vox)(e) * HrfFunctions.gaussianPdf(Lag(sampleTime(s) - evOnset(e)), tr.tau, tr.sd)
          e += 1
        val w = rng.gauss() * spec.noiseSd
        prev = if s == 0 || runId(s) != runId(s - 1) then w else spec.rho * prev + w * math.sqrt(1.0 - spec.rho * spec.rho)
        y(vox * t + s) = sig + prev
        s += 1
      vox += 1
    val fit = FitInputs(
      mat(v, t, y), Some(mat(0, t, new Array[Double](0))), mat(t, p, nuis), sampleTime, runId, evOnset, evCond, evStim, evRun, Array.fill(nEv)(0.0)
    )
    val e = Vector.tabulate(v)(j => amp(j).map(a => a * HrfFunctions.gaussianPdf(Lag(spec.truths(j).tau), spec.truths(j).tau, spec.truths(j).sd)))
    Data(fit, amp, e)

  def withY(in: FitInputs, y: Matrix): FitInputs = in.copy(y = y)

  def withOnset(in: FitInputs, evOnset: Array[Double]): FitInputs = in.copy(evOnset = evOnset)

  def withNuisance(in: FitInputs, nuisance: Matrix): FitInputs = in.copy(nuisance = nuisance)

  def withSampleTime(in: FitInputs, sampleTime: Array[Double]): FitInputs = in.copy(sampleTime = sampleTime)
