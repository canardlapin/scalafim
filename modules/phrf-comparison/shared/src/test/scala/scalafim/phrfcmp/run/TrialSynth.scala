package scalafim.phrfcmp.run

import scalafim.phrfcmp.ingest.Matrix

/**
  * A small trial-cell stand-in built directly as whitened [[TrialNativeInputs]]: runs, three conditions, stimuli that
  * repeat once per run with a persistent deviation (the structure the shared fold score rewards), a block-diagonal
  * per-run nuisance, and white noise (whitening is the identity here; whitening itself is S2's). A portable LCG keeps the
  * data identical on JVM and JS.
  */
object TrialSynth:

  final case class Spec(
      runs: Int = 4,
      runLen: Int = 150,
      voxels: Int = 5,
      stimPerCond: Int = 4,
      devSd: Double = 1.0,
      noiseSd: Double = 0.5,
      seed: Long = 1L
  )

  final class Lcg(var s: Long):
    def uniform(): Double =
      s = s * 6364136223846793005L + 1442695040888963407L
      (s >>> 11).toDouble / 9007199254740992.0
    def normal(): Double = uniform() + uniform() + uniform() + uniform() - 2.0

  def bump(lag: Double): Double =
    if lag < 0.0 || lag >= 24.0 then 0.0 else lag * lag * math.exp(-lag / 3.0) / 10.0

  def inputs(spec: Spec): TrialNativeInputs =
    val rng = new Lcg(spec.seed * 7919L + 13L)
    val nCond = 3
    val perRun = nCond * spec.stimPerCond
    val n = spec.runs * perRun
    val t = spec.runs * spec.runLen
    val v = spec.voxels
    val runId = Array.tabulate(t)(_ / spec.runLen)
    val trialRun = Array.tabulate(n)(_ / perRun)
    // stimulus s = cond * stimPerCond + k; each run presents every stimulus once, in a run-specific shuffled order
    val stim = new Array[Int](n)
    val onset = new Array[Double](n)
    var r = 0
    while r < spec.runs do
      val order = Array.tabulate(perRun)(identity)
      var i = perRun - 1
      while i > 0 do
        val j = (rng.uniform() * (i + 1)).toInt
        val tmp = order(i); order(i) = order(j); order(j) = tmp
        i -= 1
      val slot = (spec.runLen - 40.0) / perRun
      i = 0
      while i < perRun do
        stim(r * perRun + i) = order(i)
        onset(r * perRun + i) = r * spec.runLen + 4.0 + slot * i + 0.5 * slot * rng.uniform()
        i += 1
      r += 1
    val cond = stim.map(_ / spec.stimPerCond)
    val x = new Array[Double](t * n)
    var s = 0
    while s < t do
      var i = 0
      while i < n do
        if trialRun(i) == runId(s) then x(s * n + i) = bump(s.toDouble - onset(i))
        i += 1
      s += 1
    val condMean = Array.tabulate(nCond, v)((c, j) => 2.0 * c + 0.5 * j + 1.0)
    val stimDev = Array.tabulate(nCond * spec.stimPerCond, v)((_, _) => spec.devSd * rng.normal())
    val fCols = 3 * spec.runs
    val f = new Array[Double](t * fCols)
    s = 0
    while s < t do
      val run = runId(s)
      val local = s % spec.runLen
      f(s * fCols + run * 3) = 1.0
      f(s * fCols + run * 3 + 1) = (local - spec.runLen / 2.0) / spec.runLen
      f(s * fCols + run * 3 + 2) = math.cos(0.17 * local)
      s += 1
    val y = new Array[Double](t * v)
    s = 0
    while s < t do
      var j = 0
      while j < v do
        var acc = spec.noiseSd * rng.normal() + 0.3 * f(s * fCols + runId(s) * 3 + 1) * (j + 1)
        var i = 0
        while i < n do
          val xi = x(s * n + i)
          if xi != 0.0 then acc += xi * (condMean(cond(i))(j) + stimDev(stim(i))(j))
          i += 1
        y(s * v + j) = acc
        j += 1
      s += 1
    def m(rows: Int, cols: Int, d: Array[Double]) = Matrix.of(rows, cols, d).fold(e => throw new AssertionError(e.message), identity)
    TrialNativeInputs
      .of(m(t, v, y), m(t, n, x), m(t, fCols, f), runId, trialRun, cond, stim, Array.tabulate(n)(identity))
      .fold(e => throw new AssertionError(e.message), identity)
