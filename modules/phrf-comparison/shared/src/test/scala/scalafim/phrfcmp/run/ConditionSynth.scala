// Declared in the ingest package so the private[ingest] FitInputs constructor is reachable: the S3 tests need exact
// noise-free and constructed-degenerate inputs that no generator file provides.
package scalafim.phrfcmp.ingest

import scalafim.fmri.hrf.HrfFunctions
import scalafim.fmri.hrf.Lag

/** Condition-cell `FitInputs` with a Gaussian-kernel truth (`tau`, `sd`), per-voxel condition amplitudes and optional AR(1) noise. */
object ConditionSynth:

  final case class Truth(tau: Double, sd: Double, amps: Vector[Double])

  final case class Spec(
      truths: Vector[Truth],
      runs: Int = 3,
      runLen: Int = 100,
      eventsPerRun: Int = 8,
      /** condition index of event k within a run; length `eventsPerRun`. */
      conds: Vector[Int] = Vector(0, 1, 2, 1, 0, 2, 2, 0),
      noiseSd: Double = 0.0,
      rho: Double = 0.3,
      seed: Long = 1L,
      /** onset of event k is `first + gap * k` (multiples of 0.2 s so the 0.2 s lowering grid is exact) */
      first: Double = 4.0,
      gap: Double = 11.2,
      /** repetition time: scan k of a run is at `k * tr` */
      tr: Double = 1.0,
      /** weight applied to the response at lag exactly 0 (1 = closed interval, as the generator; 0.5 half; 0 dropped) */
      lag0Weight: Double = 1.0
  )

  private final class Lcg(var s: Long):
    def next(): Double =
      s = s * 6364136223846793005L + 1442695040888963407L
      ((s >>> 11).toDouble + 0.5) / 9007199254740992.0
    def gauss(): Double = math.sqrt(-2.0 * math.log(next())) * math.cos(2.0 * math.Pi * next())

  private def mat(r: Int, c: Int, d: Array[Double]): Matrix = Matrix.of(r, c, d).fold(e => throw new AssertionError(e.message), identity)

  def inputs(spec: Spec): FitInputs =
    val rng = new Lcg(spec.seed * 7919L + 17L)
    val t = spec.runs * spec.runLen
    val v = spec.truths.length
    val nEv = spec.runs * spec.eventsPerRun
    val pPer = 6
    val p = pPer * spec.runs
    val runId = Array.tabulate(t)(i => i / spec.runLen)
    val sampleTime = Array.tabulate(t)(i => (i % spec.runLen) * spec.tr)
    val evRun = Array.tabulate(nEv)(e => e / spec.eventsPerRun)
    // nearest double to the 0.1 s decimal, as the generator (k / 10.0): `first + gap * k` accumulates error (4.1 + 33.9 = 38.00000000000001)
    val evOnset = Array.tabulate(nEv)(e => math.round((spec.first + spec.gap * (e % spec.eventsPerRun)) * 10.0) / 10.0)
    val evCond = Array.tabulate(nEv)(e => spec.conds(e % spec.eventsPerRun))
    val nuis = new Array[Double](t * p)
    var i = 0
    while i < t do
      val r = runId(i)
      val n = spec.runLen
      val tt = i % n
      val lin = -1.0 + 2.0 * tt / (n - 1)
      val col0 = r * pPer
      nuis(i * p + col0) = 1.0
      nuis(i * p + col0 + 1) = lin
      nuis(i * p + col0 + 2) = lin * lin - 1.0 / 3.0
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
            // library Gaussian density kernel, evaluated directly at the exact lag (independent of the PHRF lowering)
            val w = if sampleTime(s) == evOnset(e) then spec.lag0Weight else 1.0
            sig += w * tr.amps(evCond(e)) * HrfFunctions.gaussianPdf(Lag(sampleTime(s) - evOnset(e)), tr.tau, tr.sd)
          e += 1
        val w = rng.gauss() * spec.noiseSd
        prev = if s == 0 || runId(s) != runId(s - 1) then w else spec.rho * prev + w * math.sqrt(1.0 - spec.rho * spec.rho)
        y(vox * t + s) = sig + prev
        s += 1
      vox += 1
    FitInputs(
      mat(v, t, y), None, mat(t, p, nuis), sampleTime, runId, evOnset, evCond, Array.fill(nEv)(-1), evRun, Array.fill(nEv)(0.0)
    )

  /** Same design, different data: replaces `y`. Used to build exact data under a preparation made from a noisy sibling. */
  def withY(in: FitInputs, y: Matrix): FitInputs = in.copy(y = y)

  /** Tampering helpers for the input-binding tests: one array changed by one bit-visible amount. */
  def withNuisance(in: FitInputs): FitInputs =
    val d = in.nuisance.data.clone()
    d(d.length / 2) += 1e-9
    in.copy(nuisance = mat(in.nuisance.rows, in.nuisance.cols, d))

  def withOnset(in: FitInputs): FitInputs =
    val o = in.evOnset.clone()
    o(1) += 1.0 // one FIR bin: the pre-fit design only resolves onsets to its 1 s bins (see ConditionRunner.inputsBound)
    in.copy(evOnset = o)

  /** 0.1 s later, staying inside the same 1 s bin of the FIR pre-fit design (onset 15.4 -> 15.5 etc.). */
  def withOnsetSubBin(in: FitInputs): FitInputs =
    val o = in.evOnset.clone()
    o(1) = o(1) + 0.1
    assert(math.floor(o(1)) == math.floor(in.evOnset(1)), "sub-bin shift must stay inside the bin")
    in.copy(evOnset = o)

  def withSampleTimeSubBin(in: FitInputs): FitInputs =
    val s = in.sampleTime.clone()
    s(5) = s(5) + 0.1
    in.copy(sampleTime = s)

  def withCond(in: FitInputs): FitInputs =
    val c = in.evCond.clone()
    c(0) = (c(0) + 1) % 3
    in.copy(evCond = c)

  def withSampleTime(in: FitInputs): FitInputs =
    val s = in.sampleTime.clone()
    s(5) += 1.0 // one FIR bin, as for onsets
    in.copy(sampleTime = s)

  /** Every odd event takes the onset of the event before it: with alternating conditions the two design columns coincide. */
  def pairedOnsets(in: FitInputs): FitInputs =
    in.copy(evOnset = Array.tabulate(in.evOnset.length)(e => in.evOnset(e - e % 2)))
