package scalafim.phrfcmp.prep

import scala.util.chaining.*

import scalafim.phrfcmp.ingest.*
import scalafim.phrfcmp.ingest.TestSupport.*

/**
  * Generator-shaped datasets with AR(1) noise, built in-test and bound through the real `PhrfDatasetBinding`, so the
  * preparation under test only ever sees a genuine `FitInputs`. A portable LCG keeps data identical on JVM and JS.
  */
object PrepSynth:

  final case class Spec(
      kind: CellKind,
      runs: Int = 3,
      runLen: Int = 100,
      voxels: Int = 8,
      eventsPerRun: Int = 8,
      rho: Double = 0.3,
      seed: Long = 1L,
      noiseSd: Double = 1.0,
      onsetShift: Double = 0.0,
      sampleTimeUlp: Boolean = false
  )
  // onsetShift and sampleTimeUlp perturb ONLY the stored ev_onset (event 0) / sample_time (sample 7), never y.

  private final class Lcg(var s: Long):
    def next(): Double =
      s = s * 6364136223846793005L + 1442695040888963407L
      ((s >>> 11).toDouble + 0.5) / 9007199254740992.0
    def gauss(): Double = math.sqrt(-2.0 * math.log(next())) * math.cos(2.0 * math.Pi * next())

  private val CodeSha = "c0de" * 16
  private val RootHex = "00000000000000ab"
  private val Root: Long = java.lang.Long.parseUnsignedLong(RootHex, 16)

  def cell(spec: Spec): CellSpec =
    val trial = spec.kind == CellKind.Trial
    CellSpec(if trial then "T-PREP" else "C-PREP", spec.kind, spec.voxels, if trial then 2 else 0, 1.0, trial)

  def expectation(spec: Spec): IngestExpectation =
    IngestExpectation(RootKind.Pilot, RootHex, CodeSha, cell(spec), 0, Seeds.ProtocolDenylist)

  /** Canonical SPM-like kernel (not the library one; only used to put signal in the data). */
  private def kernel(lag: Double): Double =
    if lag <= 0.0 then 0.0 else math.pow(lag / 5.0, 5) * math.exp(-(lag - 5.0))

  def bound(spec: Spec): BoundDataset =
    val rng = new Lcg(spec.seed * 7919L + 17L)
    val trial = spec.kind == CellKind.Trial
    val t = spec.runs * spec.runLen
    val v = spec.voxels
    val nEv = spec.runs * spec.eventsPerRun
    val pPer = 6
    val p = pPer * spec.runs
    val runId = Array.tabulate(t)(i => i / spec.runLen)
    val sampleTime = Array.tabulate(t)(i => (i % spec.runLen).toDouble)
    val evRun = Array.tabulate(nEv)(e => e / spec.eventsPerRun)
    val evOnset = Array.tabulate(nEv) { e =>
      val k = e % spec.eventsPerRun
      (spec.runLen - 20) / spec.eventsPerRun * k + 5.3 + (if k % 2 == 0 then 0.0 else 0.4)
    }
    val evCond = Array.tabulate(nEv)(e => e % 3)
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
    val amp = Array.tabulate(nEv)(e => 2.0 + evCond(e) + (if trial then 0.5 * rng.gauss() else 0.0))
    def series(): Array[Double] =
      val y = new Array[Double](t)
      var prev = 0.0
      var s = 0
      while s < t do
        val w = rng.gauss() * spec.noiseSd
        prev = if s == 0 || runId(s) != runId(s - 1) then w else spec.rho * prev + w * math.sqrt(1.0 - spec.rho * spec.rho)
        y(s) = prev
        var e = 0
        while e < nEv do
          if evRun(e) == runId(s) then y(s) += amp(e) * kernel(sampleTime(s) - evOnset(e))
          e += 1
        y(s) += 3.0 + 0.5 * nuis(s * p + runId(s) * pPer + 1)
        s += 1
      y
    val y = Array.fill(v)(series()).flatten
    val evOnsetStored = evOnset.clone().tap(a => a(0) += spec.onsetShift)
    val sampleTimeStored = sampleTime.clone().tap(a => if spec.sampleTimeUlp then a(7) = math.nextUp(a(7)))
    val pool = if trial then 2 else 0
    val yPool = Array.fill(pool)(series()).flatten
    val g = 5
    def f(name: String, shape: Seq[Int], xs: Seq[Double]) = (name, npyF64(shape, xs), "float64", shape)
    def ii(name: String, shape: Seq[Int], xs: Seq[Int]) = (name, npyI32(shape, xs), "int32", shape)
    def zeros(n: Int) = Seq.fill(n)(0.0)
    val base = Vector(
      f("y", Seq(v, t), y.toSeq), f("signal", Seq(v, t), zeros(v * t)), f("nuisance", Seq(t, p), nuis.toSeq),
      f("nuisance_coef", Seq(v, p), zeros(v * p)), f("sample_time", Seq(t), sampleTimeStored.toSeq),
      ii("run_id", Seq(t), runId.toSeq), f("ev_onset", Seq(nEv), evOnsetStored.toSeq), f("ev_onset_true", Seq(nEv), evOnset.toSeq),
      ii("ev_cond", Seq(nEv), evCond.toSeq), ii("ev_stim", Seq(nEv), if trial then evCond.toSeq else Seq.fill(nEv)(-1)),
      ii("ev_run", Seq(nEv), evRun.toSeq), f("ev_duration", Seq(nEv), zeros(nEv)), f("truth_t", Seq(g), zeros(g)),
      f("truth_kernel", Seq(v, g), zeros(v * g)), f("truth_params", Seq(v, 2), zeros(v * 2)),
      f("cond_coef", Seq(v, 3), zeros(v * 3)), f("cond_peak_amp", Seq(v, 3), zeros(v * 3)), f("signal_scale", Seq(v), zeros(v))
    )
    val extra = Vector(
      f("trial_beta", Seq(v, nEv), zeros(v * nEv)), f("signal_condition_mean", Seq(v, t), zeros(v * t)),
      f("y_pool", Seq(pool, t), yPool.toSeq), f("nuisance_coef_pool", Seq(pool, p), zeros(pool * p))
    )
    val as = if trial then base ++ extra else base
    val npz = zip(as.sortBy(_._1).map((n, b, _, _) => s"$n.npy" -> b))
    val c = cell(spec)
    def seed(purpose: String) = Seeds.streamSeed(Root, c.cellId, 0, Seeds.ManifestPurposes.toMap.apply(purpose))
    val m = ujson.Obj(
      "schema" -> "phrf-gen-npz-1",
      "generator_code_sha256" -> CodeSha,
      "root_kind" -> "pilot",
      "root_hex" -> RootHex,
      "root_denylisted" -> false,
      "denylist" -> ujson.Arr.from(Seeds.ProtocolDenylist.toVector.sorted.map(x => ujson.Num(x.toDouble))),
      "dataset" -> 0,
      "streams" -> ujson.Obj.from(Seeds.ManifestPurposes.map((q, _) => q -> ujson.Str(Seeds.unsigned(seed(q))))),
      "stream_denylist_check" -> ujson.Obj.from(Seeds.ManifestPurposes.map { (q, _) =>
        q -> ujson.Obj("hit" -> false, "seed" -> ujson.Str(Seeds.unsigned(seed(q))))
      }),
      "file" -> s"${c.cellId}__d0000.npz",
      "npz_sha256" -> Digests.sha256Hex(npz),
      "arrays" -> ujson.Obj.from(as.map { (n, b, dt, sh) =>
        n -> ujson.Obj("dtype" -> dt, "shape" -> ujson.Arr.from(sh), "npy_sha256" -> Digests.sha256Hex(b))
      }),
      "cell" -> ujson.Obj(
        "cell_id" -> c.cellId, "kind" -> (if trial then "trial" else "condition"), "n_voxels" -> c.nVoxels,
        "n_pool" -> c.nPool, "tr" -> c.tr, "tr_aligned_onsets" -> c.trAlignedOnsets
      ),
      "n_samples" -> t,
      "n_events" -> nEv
    )
    PhrfDatasetBinding.bind(npz, Synthetic.render(m), expectation(spec)).fold(r => throw new AssertionError(r.message), identity)

  def inputs(spec: Spec): FitInputs = bound(spec).fit
