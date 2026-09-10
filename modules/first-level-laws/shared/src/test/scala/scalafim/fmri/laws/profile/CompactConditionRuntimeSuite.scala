package scalafim.fmri.laws.profile

import gale.linalg.{DMat, DVec}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{CompactConditionPreparation, CompactConditionRuntime, DecodeBudget, DecodeStatus, DecoderCounters, NodeGrid}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, NormalizationRule, ShapePoint}

/** End-to-end condition fit against a dense time-domain oracle that uses the
  * exact family kernel, the same whitening and nuisance projection, a fine
  * shape grid and a compass refinement with exact evaluations.
  */
class CompactConditionRuntimeSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val rows = 240
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val rng0 = new scala.util.Random(2026L)
  private val events = 36
  private val onsets = Vector.fill(events)(rng0.nextInt(2100) / 10.0).sorted.map(Seconds(_))
  private val conditions = Vector.tabulate(events)(i => Vector("A", "B", "C")(i % 3))
  private val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(events)(0), termTag = Some("cond"))
  private val arPhi = 0.3
  private val whitening = WhiteningPlan.global(ArmaCoefficients.ar(arPhi), Vector(TimeSegment(0, rows, 0)))
  private val nuisanceCols = 4
  private val nuisance = DMat.tabulate(rows, nuisanceCols)((t, j) => j match
    case 0 => 1.0
    case 1 => t.toDouble / rows - 0.5
    case _ => math.cos(math.Pi * (j - 1) * (t + 0.5) / rows))
  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40)).fold(e => fail(e.message), identity)
  private lazy val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
  private lazy val prep = CompactConditionPreparation.prepare(expanded, Some(whitening), Some(nuisance)).fold(e => fail(e.message), identity)

  private def whitenColumns(cols: Int, rowMajor: Array[Double]): Array[Double] =
    val b = DMat.newBuilder(rows, cols)
    var i = 0
    while i < rows * cols do
      b.writeLinear(i, rowMajor(i))
      i += 1
    val out = new Array[Double](rows * cols)
    WhiteningTransform.matrix(whitening, b.result()).fold(e => fail(e.toString), identity).copyRowMajorTo(out)
    out

  /** Whitened, nuisance-projected direct design at a shape (unnormalised kernel), row-major rows x C. */
  private def directDesign(t1: Double, t2: Double): Array[Double] =
    val point = family.chart.point(t1, t2).fold(e => fail(e.message), identity)
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(family.libraryNormalization, point, scale)
    val raw = term.convolve(family.toHrf(point), frame, precision = precision).data
    val unnormalised = raw.data.map(_ / scale(JetLayout.Value))
    val w = whitenColumns(raw.cols, unnormalised)
    projectNuisance(raw.cols, w)

  private def projectNuisance(cols: Int, w: Array[Double]): Array[Double] =
    val f = prep.nuisanceRank
    val out = w.clone()
    var col = 0
    while col < cols do
      val qy = new Array[Double](f)
      var t = 0
      while t < rows do
        var i = 0
        while i < f do
          qy(i) += prep.qF(t * f + i) * w(t * cols + col)
          i += 1
        t += 1
      t = 0
      while t < rows do
        var acc = 0.0
        var i = 0
        while i < f do
          acc += prep.qF(t * f + i) * qy(i)
          i += 1
        out(t * cols + col) -= acc
        t += 1
      col += 1
    out

  private def leastSquares(design: Array[Double], cols: Int, y: Array[Double]): (Double, Array[Double]) =
    val b = DMat.newBuilder(rows, cols)
    var i = 0
    while i < rows * cols do
      b.writeLinear(i, design(i))
      i += 1
    val fit = b.result().leastSquares(DVec.fromSeq(y.toSeq)).fold(e => throw e, identity)
    val beta = Array.tabulate(cols)(j => fit(j))
    var residual = 0.0
    var t = 0
    while t < rows do
      var pred = 0.0
      var j = 0
      while j < cols do
        pred += design(t * cols + j) * beta(j)
        j += 1
      residual += (y(t) - pred) * (y(t) - pred)
      t += 1
    (residual, beta)

  /** Dense oracle: fine grid then compass refinement, all with the exact kernel. */
  private final class Oracle(tauGrid: Int, logSdGrid: Int):
    private val c = 3
    private val tauStep = family.chart.width(0) / (tauGrid - 1)
    private val vStep = family.chart.width(1) / (logSdGrid - 1)
    private val designs = Array.tabulate(tauGrid * logSdGrid) { s =>
      val a = s / logSdGrid
      val bb = s % logSdGrid
      directDesign(family.chart.lower(0) + a * tauStep, family.chart.lower(1) + bb * vStep)
    }
    def solve(y: Array[Double]): (Double, Double, Array[Double], Int) =
      var best = 0
      var bestE = Double.PositiveInfinity
      var s = 0
      while s < designs.length do
        val (residual, _) = leastSquares(designs(s), c, y)
        if residual < bestE then
          bestE = residual
          best = s
        s += 1
      var tau = family.chart.lower(0) + (best / logSdGrid) * tauStep
      var v = family.chart.lower(1) + (best % logSdGrid) * vStep
      var current = bestE
      var beta = leastSquares(designs(best), c, y)._2
      var evals = 0
      var hTau = tauStep / 2
      var hV = vStep / 2
      var level = 0
      while level < 7 do
        var improved = false
        for (dTau, dV) <- Seq((hTau, 0.0), (-hTau, 0.0), (0.0, hV), (0.0, -hV)) do
          val tTau = family.chart.clamp(0, tau + dTau)
          val tV = family.chart.clamp(1, v + dV)
          if tTau != tau || tV != v then
            val (residual, b) = leastSquares(directDesign(tTau, tV), c, y)
            evals += 1
            if residual < current then
              current = residual
              tau = tTau
              v = tV
              beta = b
              improved = true
        if !improved then
          hTau /= 2
          hV /= 2
          level += 1
      (tau, v, beta, evals)

  test("decoded shapes and amplitudes agree with the dense oracle on a synthetic cohort"):
    val voxels = 24
    val rng = new scala.util.Random(77L)
    val fine = basis.lags
    val kernel = new Array[Double](fine.length)
    val trueTau = Array.fill(voxels)(3.5 + 4.0 * rng.nextDouble())
    val trueLogSd = Array.fill(voxels)(math.log(1.0) + rng.nextDouble() * (math.log(2.5) - math.log(1.0)))
    val trueBeta = Array.fill(voxels * 3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
    val raw = new Array[Double](rows * voxels)
    val nuisanceArr = new Array[Double](rows * nuisanceCols)
    nuisance.copyRowMajorTo(nuisanceArr)
    var v = 0
    while v < voxels do
      val point = ShapePoint.unsafe(Vector(trueTau(v), trueLogSd(v)))
      family.evalInto(fine, point, kernel)
      val design = term.convolve(family.toHrf(point), frame, precision = precision).data
      val scale = new Array[Double](family.jetComponents)
      family.scaleJetInto(family.libraryNormalization, point, scale)
      var sum = 0.0
      var sum2 = 0.0
      val signal = new Array[Double](rows)
      var t = 0
      while t < rows do
        var acc = 0.0
        var j = 0
        while j < 3 do
          acc += design.data(t * 3 + j) / scale(JetLayout.Value) * trueBeta(v * 3 + j)
          j += 1
        signal(t) = acc
        sum += acc
        sum2 += acc * acc
        t += 1
      val sd = math.sqrt(math.max(1e-12, sum2 / rows - (sum / rows) * (sum / rows)))
      val noiseSd = sd / 1.0 // SNR 1
      val innovation = noiseSd * math.sqrt(1.0 - arPhi * arPhi)
      var noise = rng.nextGaussian() * noiseSd
      t = 0
      while t < rows do
        if t > 0 then noise = arPhi * noise + rng.nextGaussian() * innovation
        var nuis = 0.0
        var j = 0
        while j < nuisanceCols do
          nuis += nuisanceArr(t * nuisanceCols + j) * (if j == 0 then 10.0 else 0.3) * sd
          j += 1
        raw(t * voxels + v) = signal(t) + noise + nuis
        t += 1
      v += 1
    val whitened = prep.whiten(voxels, raw).fold(e => fail(e.message), identity)
    val grid = NodeGrid(family.chart, Vector(15, 15))
    val runtime = new CompactConditionRuntime(prep, grid, DecodeBudget(coarseStride = 2, maxNewtonSteps = 2, maxJets = 2, maxExactEvaluations = 6, weakSdLimit = Vector(0.5, 1.0)), None, 1.0, NormalizationRule.Unnormalised)
    val oracle = new Oracle(51, 21)
    val counters = new DecoderCounters
    val column = new Array[Double](rows)
    val projectedY = new Array[Double](rows)
    var admitted = 0
    val tauErr = Array.newBuilder[Double]
    val ampErr = Array.newBuilder[Double]
    v = 0
    while v < voxels do
      var t = 0
      while t < rows do
        column(t) = whitened(t * voxels + v)
        t += 1
      val fit = runtime.fit(column, 0, counters)
      System.arraycopy(projectNuisance(1, column), 0, projectedY, 0, rows)
      val (oTau, oV, oBeta, _) = oracle.solve(projectedY)
      if fit.decode.status == DecodeStatus.Accepted then
        admitted += 1
        tauErr += math.abs(fit.decode.coordinates(0) - oTau)
        var num = 0.0
        var den = 0.0
        var j = 0
        while j < 3 do
          num += (fit.amplitudes(j) - oBeta(j)) * (fit.amplitudes(j) - oBeta(j))
          den += oBeta(j) * oBeta(j)
          j += 1
        ampErr += math.sqrt(num / den)
        assertEqualsDouble(math.exp(oV), math.exp(fit.decode.coordinates(1)), 0.05 * math.exp(oV), s"sigma at voxel $v")
      v += 1
    val te = tauErr.result().sorted
    val ae = ampErr.result().sorted
    println(s"[compact-condition] admitted $admitted/$voxels, max |tau - oracle| = ${te.lastOption.getOrElse(Double.NaN)}, max rel amp = ${ae.lastOption.getOrElse(Double.NaN)}, per voxel: nodes ${counters.perVoxel(counters.nodeScores)}, jets ${counters.perVoxel(counters.jets)}, exact ${counters.perVoxel(counters.exactEvaluations)}")
    assert(admitted >= 0.9 * voxels, s"admitted $admitted of $voxels")
    assert(te.last <= 0.02, s"max latency error ${te.last}")
    assert(ae.last <= 2e-3, s"max amplitude error ${ae.last}")
    assert(counters.perVoxel(counters.jets) <= 2.0 + 1e-9)
    assert(counters.perVoxel(counters.exactEvaluations) <= 6.0 + 1e-9)
    assert(counters.perVoxel(counters.nodeScores) <= 90.0)

  test("readout converts amplitudes into the requested normalisation"):
    val grid = NodeGrid(family.chart, Vector(8, 8))
    val unnormalised = new CompactConditionRuntime(prep, grid, DecodeBudget(), None, 1.0, NormalizationRule.Unnormalised)
    val density = new CompactConditionRuntime(prep, grid, DecodeBudget(), None, 1.0, NormalizationRule.Density)
    val rng = new scala.util.Random(5L)
    val y = Array.fill(rows)(rng.nextGaussian())
    val a = unnormalised.fit(y, 0, new DecoderCounters)
    val b = density.fit(y, 0, new DecoderCounters)
    assertEquals(a.decode.coordinates, b.decode.coordinates)
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(NormalizationRule.Density, a.decode.point, scale)
    var j = 0
    while j < 3 do
      assertEqualsDouble(b.amplitudes(j), a.amplitudes(j) / scale(0), 1e-12)
      j += 1
    assertEquals(a.nuisanceProjection.length, prep.nuisanceRank)
    assert(a.noisePlugin > 0.0)
