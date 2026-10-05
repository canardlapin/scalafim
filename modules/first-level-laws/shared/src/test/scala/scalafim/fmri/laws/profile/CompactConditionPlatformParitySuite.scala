package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{
  CompactConditionPreparation,
  CompactConditionRuntime,
  DecodeBudget,
  DecoderCounters,
  NodeGrid,
  ObservedFamilyCertification,
  ObservedFamilyRequirements
}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, NormalizationRule, ShapePoint}

/** One fixed shared golden for the JVM and Scala.js decoders: per-voxel status, stationarity reason and
  * Newton steps, and the cohort's work counters. Estimates may differ in their last bits between platforms;
  * these discrete outcomes must not. A mismatch here is platform drift in the decoder's decisions, which
  * previously surfaced only as different admission counts in CI (see
  * `docs/verification/c0-ulp-stationarity-20261004.md`).
  *
  * The fixture is the synthetic cohort of [[CompactConditionRuntimeSuite]] (seed 77, SNR 1, 24 voxels).
  */
class CompactConditionPlatformParitySuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val rows = 240
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val rng0 = new scala.util.Random(2026L)
  private val events = 36
  private val onsets = Vector.fill(events)(rng0.nextInt(2100) / 10.0).sorted.map(Seconds(_))
  private val conditions = Vector.tabulate(events)(i => Vector("A", "B", "C")(i % 3))
  private val term = EventTerm(
    events = Vector(Event.factor(conditions, "cond")),
    onsets = onsets,
    blockIds = Vector.fill(events)(0),
    termTag = Some("cond")
  )
  private val arPhi = 0.3
  private val whitening = WhiteningPlan.global(ArmaCoefficients.ar(arPhi), Vector(TimeSegment(0, rows, 0)))
  private val nuisanceCols = 4
  private val nuisance = DMat.tabulate(rows, nuisanceCols)((t, j) =>
    j match
      case 0 => 1.0
      case 1 => t.toDouble / rows - 0.5
      case _ => math.cos(math.Pi * (j - 1) * (t + 0.5) / rows)
  )
  private lazy val basis = HrfKernelBasis
    .compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(e => fail(e.message), identity)
  private lazy val expanded =
    ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
  private lazy val prep =
    val points = Vector((4.0, math.log(1.2)), (6.0, math.log(2.0))).map { case (tau, logSd) =>
      family.chart.point(tau, logSd).fold(e => fail(e.message), identity)
    }
    val admission = ObservedFamilyCertification
      .admitForCompact(
        expanded,
        term,
        frame,
        precision,
        Some(whitening),
        Some(nuisance),
        points,
        ObservedFamilyRequirements(1e-2, 1e8, 1e-6)
      )
      .fold(e => fail(e.message), identity)
    CompactConditionPreparation
      .prepare(expanded, admission, Some(whitening), Some(nuisance), term, frame, precision)
      .fold(e => fail(e.message), identity)

  /** The cohort generator of [[CompactConditionRuntimeSuite]], unchanged. */
  private def cohort(voxels: Int): Array[Double] =
    val rng = new scala.util.Random(77L)
    val trueTau = Array.fill(voxels)(3.5 + 4.0 * rng.nextDouble())
    val trueLogSd = Array.fill(voxels)(math.log(1.0) + rng.nextDouble() * (math.log(2.5) - math.log(1.0)))
    val trueBeta = Array.fill(voxels * 3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
    val raw = new Array[Double](rows * voxels)
    val nuisanceArr = new Array[Double](rows * nuisanceCols)
    nuisance.copyRowMajorTo(nuisanceArr)
    var v = 0
    while v < voxels do
      val point = ShapePoint.unsafe(Vector(trueTau(v), trueLogSd(v)))
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
      val innovation = sd * math.sqrt(1.0 - arPhi * arPhi)
      var noise = rng.nextGaussian() * sd
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
    raw

  test("decoder statuses, stationarity reasons, steps and work counters are identical on JVM and JS"):
    val voxels = 24
    val whitened = prep.whiten(voxels, cohort(voxels)).fold(e => fail(e.message), identity)
    val runtime = new CompactConditionRuntime(
      prep,
      NodeGrid(family.chart, Vector(15, 15)),
      DecodeBudget(coarseStride = 2, maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2,
        weakSdLimit = Vector(0.5, 1.0)),
      None,
      1.0,
      NormalizationRule.Unnormalised
    )
    val counters = new DecoderCounters
    val column = new Array[Double](rows)
    val outcomes = Vector.tabulate(voxels) { v =>
      var t = 0
      while t < rows do
        column(t) = whitened(t * voxels + v)
        t += 1
      val decode = runtime.fit(column, 0, counters).decode
      s"${decode.status}/${decode.stationarity.fold("-")(_.toString)}/${decode.newtonSteps}"
    }
    val work = Vector(
      counters.voxels,
      counters.nodeScores,
      counters.jets,
      counters.exactEvaluations,
      counters.candidateAttempts,
      counters.terminalVerifications,
      counters.newtonSteps,
      counters.fallbacks
    )
    assertEquals(outcomes, ExpectedOutcomes)
    assertEquals(work, ExpectedWork)

  private val ExpectedOutcomes: Vector[String] = Vector.empty
  private val ExpectedWork: Vector[Long] = Vector.empty
