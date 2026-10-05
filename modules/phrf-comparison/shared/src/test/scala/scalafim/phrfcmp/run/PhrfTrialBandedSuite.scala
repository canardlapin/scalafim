package scalafim.phrfcmp.run

import gale.linalg.DMat

import scalafim.fmri.design.{ColumnId, ConditionId, ScanIndex, TrialId}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, TrialMembership}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.NormalizationRule
import scalafim.fmri.model.PositiveAlpha
import scalafim.phrfcmp.ingest.{CellKind, Matrix, PhrfTrialSynth}
import scalafim.phrfcmp.ingest.PhrfTrialSynth.{Spec, Truth}
import scalafim.phrfcmp.prep.CommonPreparation

/**
  * (a) PHRF's TrialBanded solution against rLSS at the mapped penalty (review F2): equal to ~1e-10 on a balanced,
  * orthogonal, equal-norm design; a documented bound when trials overlap. (b) PHRF's true effective df
  * `tr(W^{-1} X~'X~)` against the self-influences of TrialBanded's own readout, its limits, and the rLSS closed form.
  */
class PhrfTrialBandedSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private val config = PhrfTrialConfig()
  private lazy val basis: HrfKernelBasis = PhrfTrialRunner.basisOf(config).fold(e => fail(e.message), identity)
  private lazy val shape = PhrfCanonicalShape.derive(basis.family).fold(m => fail(m), identity)

  private final class Lcg(var s: Long):
    def normal(): Double =
      def u =
        s = s * 6364136223846793005L + 1442695040888963407L
        (s >>> 11).toDouble / 9007199254740992.0
      u + u + u + u - 2.0

  /** A single run of `rows` scans, trials `i` at `first + spacing * i` in condition `i % 3`. */
  private final case class Design(expanded: ExpandedTrialDesign, cond: Array[Int], x: DMat, rows: Int):
    def n: Int = cond.length

  private def design(trials: Int, spacing: Double, first: Double, rows: Int): Design =
    val onsets = Vector.tabulate(trials)(i => first + spacing * i)
    val cond = Array.tabulate(trials)(_ % 3)
    val frame = SamplingFrame(blockLens = Vector(rows), tr = Vector(1.0), startTime = Vector(0.0))
    val membership = TrialMembership.make(cond.toVector, 3).fold(e => fail(e.message), identity)
    val ex = ExpandedTrialDesign
      .lower(onsets.map(Seconds(_)), Vector.fill(trials)(0), Vector.fill(trials)(Seconds(0.0)), membership, frame, basis, basis.spec.fineStep.seconds)
      .fold(e => fail(e.message), identity)
    Design(ex, cond, PhrfEdf.rawRegressors(ex, shape.coordinates).fold(e => fail(e.message), identity), rows)

  /** TrialBanded's trial amplitudes (raw coefficients) at the fixed canonical shape and `lambda`, no whitening, no nuisance. */
  private def banded(d: Design, y: Array[Double], lambda: Double): Array[Double] =
    val tb = TrialBandedPreparation.prepare(d.expanded, None, None, lambda).fold(e => fail(e.toString), identity)
    val grid = NodeGrid(basis.family.chart, Vector(3, 3))
    val bank = tb.objective(grid).fold(e => fail(e.toString), identity)
    val labels = Vector.tabulate(3)(k => ConditionId.unsafe(s"c$k"))
    val axis = ProfileTrialAxis
      .make(d.expanded, tb, Vector.tabulate(d.n)(i => TrialId.unsafe(s"t$i")), labels, d.cond.toVector.map(labels(_)), Vector.empty[ColumnId], Vector.tabulate(d.rows)(i => ScanIndex.unsafeOneBased(i + 1)))
      .fold(e => fail(e.message), identity)
    val readout = ProfileTrialReadout
      .freeze(bank, axis, shape.coordinates, PhrfCanProbe.nearestNode(grid, shape.coordinates), NormalizationRule.Density, ProfileTrialReadoutMode.ExactShape)
      .fold(e => fail(e.message), identity)
    val response = ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, y).fold(e => fail(e.message), identity)
    val r = readout.newWorker().evaluate(response, OutputRequest.TrialAmplitudes(NormalizationRule.Density)).fold(e => fail(e.message), identity)
    r.trialAmplitudes.get.map(_ * r.normalizationScale).toArray

  private def mat(rows: Int, cols: Int, f: (Int, Int) => Double): Matrix =
    Matrix.of(rows, cols, Array.tabulate(rows * cols)(k => f(k / cols, k % cols))).fold(e => fail(e.message), identity)

  private def nativeInputs(d: Design, y: Array[Double]): TrialNativeInputs =
    TrialNativeInputs
      .of(mat(d.rows, 1, (r, _) => y(r)), mat(d.rows, d.n, (r, i) => d.x(r, i)), mat(d.rows, 0, (_, _) => 0.0),
        Array.fill(d.rows)(0), Array.fill(d.n)(0), d.cond, Array.tabulate(d.n)(identity), Array.tabulate(d.n)(identity))
      .fold(e => fail(e.code), identity)

  private def norms2(d: Design): Array[Double] = Array.tabulate(d.n)(i => (0 until d.rows).map(r => d.x(r, i) * d.x(r, i)).sum)

  private def compare(d: Design, label: String): Double =
    val rng = new Lcg(17L)
    val y = Array.fill(d.rows)(rng.normal())
    val in = nativeInputs(d, y)
    val n2 = norms2(d)
    val scale = PenaltyScale.derive(n2, n2, d.cond).fold(e => fail(e.message), identity) // same regressors: kappa^2 = 1
    var worst = 0.0
    for alpha <- Seq(0.25, 1.0, 4.0) do
      val lambda = PositiveAlpha(alpha).fold(e => fail(e.message), _.lambda)
      val b = banded(d, y, lambda)
      val r = TrialNativeRunner.rlssCentred(in, scale.penalty(lambda)).fold(e => fail(e.message), identity)
      val ref = b.map(math.abs).max
      worst = math.max(worst, b.indices.map(i => math.abs(b(i) - r.amplitudes(i, 0))).max / ref)
    println(f"S5-BANDED-VS-RLSS $label: max relative |TrialBanded - rLSS(mapped penalty)| = $worst%.3e")
    worst

  test("balanced orthogonal equal-norm design: TrialBanded equals rLSS at the mapped penalty (1 - 1/n_c) lambda"):
    val d = design(trials = 12, spacing = 25.0, first = 2.0, rows = 330) // disjoint supports (24 s kernel), 4 trials per condition
    val n2 = norms2(d)
    assert(n2.max - n2.min < 1e-12 * n2.max, "equal norms")
    var off = 0.0
    for i <- 0 until d.n; j <- 0 until i do off = math.max(off, math.abs((0 until d.rows).map(r => d.x(r, i) * d.x(r, j)).sum))
    assert(off == 0.0, s"orthogonal: off-diagonal $off")
    assert(compare(d, "orthogonal") < 1e-10)

  test("overlapping trials: the discrepancy is reported and bounded, and shrinks as the trials separate"):
    val heavy = compare(design(trials = 12, spacing = 6.0, first = 2.0, rows = 110), "overlapping (6 s spacing)")
    val light = compare(design(trials = 12, spacing = 12.0, first = 2.0, rows = 170), "overlapping (12 s spacing)")
    // Documented bound: the penalty mapping is exact only for orthogonal trials (rLSS decouples the deviations that
    // TrialBanded fits jointly). Measured 0.27 at 6 s spacing (kernel sd 2.2 s, strong overlap); the bound is 0.5 there,
    // and the discrepancy must fall with the spacing.
    assert(heavy > 1e-6 && heavy < 0.5, s"heavy overlap discrepancy $heavy")
    assert(light < heavy, s"light $light vs heavy $heavy")

  // ---------------------------------------------------------------------------------------------- edf

  test("PHRF edf: equals the explicit self-influence trace of TrialBanded's readout to 1e-10"):
    val truths = Vector(Truth(5.5, 1.6, Vector(3.0, 5.0, 2.0)))
    val data = PhrfTrialSynth.generate(Spec(truths, devSd = 0.6, noiseSd = 0.3, seed = 3L))
    val prep = CommonPreparation.prepare(data.inputs, CellKind.Trial).fold(e => fail(e.message), identity)
    val all = prep.segments.map(_.runIndex)
    val problem = PhrfTrialAssembly.problem(data.inputs, prep, all, prep.sigma2All.sigma2, config).fold(e => fail(e.message), identity)
    val grid = AlphaGrid.Pilot
    val edf = PhrfEdf.forProblem(problem, basis, shape, grid).fold(e => fail(e.message), identity)
    val n = problem.trials
    // y_k = X e_k (raw regressor k): ahat_k(k) is d ahat_k / d a_k, and the sum over k is tr(S X)
    val expanded = PhrfTrialAssembly.expandedOf(problem, basis).fold(e => fail(e.message), identity)
    val raw = PhrfEdf.rawRegressors(expanded, shape.coordinates).fold(e => fail(e.message), identity)
    val yAll = mat(n, problem.timepoints, (k, r) => raw(r, k))
    val asN = new PhrfTrialProblem(problem.runs, problem.rows, problem.trialIndex, problem.trialCond, problem.trialStim, problem.conditions, problem.spec, problem.frame, problem.dataset, problem.baseline, problem.drive, problem.sigma2, n)
    var worst = 0.0
    for k <- 0 until grid.size by 2 do
      val (fit, _) = PhrfCanProbe.fixedShapeFit(asN, basis, shape, grid.alpha(k), config, PhrfClock.Wall, yAll).fold(e => fail(e.message), identity)
      val trace = (0 until n).map(i => fit.amplitudes(i, i)).sum
      worst = math.max(worst, math.abs(trace - edf.edf(k)) / edf.edf(k))
    println(f"S5-EDF max relative |tr(W^-1 G) - sum of readout self-influences| = $worst%.3e; edf by alpha ${edf.edf.map(x => f"$x%.3f").mkString(", ")} (N=$n, G=${problem.conditions})")
    assert(worst < 1e-10, s"worst $worst")
    assert(edf.edf.zip(edf.edf.tail).forall((a, b) => a < b), "edf strictly increases with alpha (weaker shrinkage)")
    assert(edf.edf.forall(e => e > problem.conditions && e < n))

  test("PHRF edf limits: N with no penalty, G as lambda grows"):
    val d = design(trials = 12, spacing = 6.0, first = 2.0, rows = 110)
    val g = PhrfEdf.projectedGram(d.x, None).fold(m => fail(m), identity)
    assertEqualsDouble(PhrfEdf.amplitudeEdf(g, d.cond, 0.0).fold(m => fail(m), identity), 12.0, 1e-8)
    assertEqualsDouble(PhrfEdf.amplitudeEdf(g, d.cond, 1e8).fold(m => fail(m), identity), 3.0, 1e-4)
    val v = Seq(1e-3, 1e-1, 1.0, 10.0, 1e3).map(l => PhrfEdf.amplitudeEdf(g, d.cond, l).fold(m => fail(m), identity))
    assert(v.zip(v.tail).forall((a, b) => a > b), s"strictly decreasing in lambda: $v")

  test("PHRF edf equals the rLSS closed form at the S4 scale-mapped penalty on a balanced orthogonal design"):
    val d = design(trials = 12, spacing = 25.0, first = 2.0, rows = 330)
    val g = PhrfEdf.projectedGram(d.x, None).fold(m => fail(m), identity)
    val dd = norms2(d)(0)
    val nc = 4.0
    var worst = 0.0
    for lambda <- Seq(0.25, 1.0, 4.0, 64.0) do
      val phrf = PhrfEdf.amplitudeEdf(g, d.cond, lambda).fold(m => fail(m), identity)
      val r = (1.0 - 1.0 / nc) * lambda
      val q = dd * (1.0 - 1.0 / nc)
      val rlss = 12.0 * (1.0 / nc + (1.0 - 1.0 / nc) * q / (q + r)) // sum_i [phi_i + (1 - phi_i) q_i / (q_i + r)], phi = 1/n_c
      worst = math.max(worst, math.abs(phrf - rlss) / rlss)
    println(f"S5-EDF PHRF vs rLSS closed form (orthogonal): max relative $worst%.3e")
    assert(worst < 1e-10, s"worst $worst")
