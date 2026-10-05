package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.{
  LssFixedDesign,
  LssTrialDesign,
  ResponseBlock,
  RidgeLeastSquaresSeparate,
  RidgeLssGrouping
}
import scalafim.fmri.fit.profile.TrialBandedPreparation
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}

/**
  * The df mapping and the PHRF-to-rLSS penalty scale. The edf is checked against analytic forms (equal information,
  * orthogonal-design ridge trace, monotonicity), the mapping against the identity it must have when both sides share `q`
  * and the scaling law when they do not, and the scale factors against `TrialBanded`'s own Gram blocks and its
  * `lambda (I - M)` penalty structure.
  */
class DfMappingSuite extends munit.FunSuite:

  private def ok[A](r: Either[DfMappingRefusal, A]): A = r.fold(e => fail(e.message), identity)

  private val q = Array(0.4, 1.3, 0.9, 2.2, 0.15, 3.1, 0.7, 1.8)

  // ------------------------------------------------------------------------------------------------- edf

  test("the information ratio is 1 at no penalty, strictly decreasing, and tends to 0"):
    val ps = Vector(0.0, 1e-6, 1e-3, 0.1, 1.0, 10.0, 1e3, 1e9)
    val e = ps.map(InformationRatio(q, _))
    assertEqualsDouble(e.head, 1.0, 1e-15)
    assert(e.zip(e.tail).forall((a, b) => a > b), s"not strictly decreasing: $e")
    assert(e.last < 1e-8)

  test("equal information has the closed form ratio = q / (q + p), so p = q (1 / edf - 1)"):
    val equal = Array.fill(6)(1.7)
    for target <- Vector(0.05, 0.3, 0.5, 0.9, 0.999) do
      val p = ok(InformationRatio.solvePenalty(equal, target))
      assertEqualsDouble(p, 1.7 * (1.0 / target - 1.0), 1e-12 * math.max(1.0, p), s"target $target")

  test("on an orthogonal design the information ratio is the ridge trace sum d_i^2 / (d_i^2 + alpha), divided by N"):
    // columns with disjoint supports and norms d_i: X'X = diag(d_i^2); hat trace = tr[X (X'X + alpha I)^-1 X'] computed densely
    val d = Vector(0.5, 1.0, 1.5, 2.0, 3.0)
    val x = DMat.tabulate(15, 5)((t, i) => if t / 3 == i then d(i) / math.sqrt(3.0) else 0.0)
    val norms2 = Array.tabulate(5)(i => (0 until 15).map(t => x(t, i) * x(t, i)).sum)
    norms2.zip(d).foreach((n2, di) => assertEqualsDouble(n2, di * di, 1e-14))
    for alpha <- Vector(1e-3, 0.1, 1.0, 7.0, 100.0) do
      val gram = DMat.tabulate(5, 5)((i, j) => (0 until 15).map(t => x(t, i) * x(t, j)).sum + (if i == j then alpha else 0.0))
      val hatInner = gram.cholesky.fold(e => fail(e.toString), identity).solve(x.t).fold(e => fail(e.toString), identity)
      val trace = (0 until 15).map(t => (0 until 5).map(i => x(t, i) * hatInner(i, t)).sum).sum
      assertEqualsDouble(trace, d.map(di => di * di / (di * di + alpha)).sum, 1e-12)
      assertEqualsDouble(trace / 5.0, InformationRatio(norms2, alpha), 1e-12)

  test("solvePenalty inverts the ratio across the whole range, including the bracket edges"):
    for target <- Vector(1e-6, 0.01, 0.1, 0.5, 0.9, 0.99, 1.0 - 1e-9) do
      val p = ok(InformationRatio.solvePenalty(q, target))
      assertEqualsDouble(InformationRatio(q, p), target, 1e-12, s"target $target")
    assertEquals(ok(InformationRatio.solvePenalty(q, 1.0)), 0.0)

  test("solvePenalty is deterministic and refuses out-of-range targets and unusable information"):
    assertEquals(ok(InformationRatio.solvePenalty(q, 0.37)), ok(InformationRatio.solvePenalty(q, 0.37)))
    assertEquals(InformationRatio.solvePenalty(q, 0.0), Left(DfMappingRefusal.TargetOutOfRange(0.0)))
    assertEquals(InformationRatio.solvePenalty(q, 1.5), Left(DfMappingRefusal.TargetOutOfRange(1.5)))
    assert(InformationRatio.solvePenalty(q, Double.NaN).isLeft)
    assertEquals(InformationRatio.solvePenalty(Array.empty[Double], 0.5), Left(DfMappingRefusal.EmptyDesign))
    assertEquals(InformationRatio.solvePenalty(Array(1.0, 0.0), 0.5), Left(DfMappingRefusal.NonPositiveInformation(1, 0.0)))
    assert(InformationRatio.solvePenalty(Array(1.0, Double.NaN), 0.5).isLeft)

  // ------------------------------------------------------------------------------------------------- the mapping

  test("map solves edf_R(r_k) = target_k on the rLSS amplitude edf; targets inside the reachable range never flag"):
    val curve = EdfCurve(r => 2.0 + 10.0 / (1.0 + r), 12.0, 2.0)
    val targets = Vector.tabulate(9)(k => 11.0 - k)
    val m = ok(DfMapping.map(AlphaGrid.Pilot, targets, curve))
    assertEquals(m.ridges.length, 9)
    (0 until 9).foreach { k =>
      assertEqualsDouble(curve.edf(m.ridges(k)), targets(k), 1e-12)
      assertEqualsDouble(m.ridges(k), 10.0 / (targets(k) - 2.0) - 1.0, 1e-9 * math.max(1.0, m.ridges(k)))
    }
    assert(m.maxEdfResidual < 1e-12)
    assert(!m.flagged)
    assertEquals(m.scaleDiagnosticRidges, None)

  test("a target above the unpenalised edf saturates at ridge 0, and below the limit at a large cap"):
    val curve = EdfCurve(r => 2.0 + 10.0 / (1.0 + r), 12.0, 2.0)
    assertEquals(ok(curve.ridgeFor(12.0)), (0.0, 12.0))
    assertEquals(ok(curve.ridgeFor(15.0)), (0.0, 12.0))
    val (r, e) = ok(curve.ridgeFor(1.0))
    assert(r > 1e6 && e - 2.0 < 1e-8)
    assert(curve.ridgeFor(0.0).isLeft)
    assert(curve.ridgeFor(Double.NaN).isLeft)

  test("the 5 % end-overlap flag fires when an end target is out of reach by more than 5 %, and only then"):
    val curve = EdfCurve(r => 2.0 + 10.0 / (1.0 + r), 12.0, 2.0)
    val inside = Vector.tabulate(9)(k => 11.0 - k)
    assert(!ok(DfMapping.map(AlphaGrid.Pilot, inside, curve)).flagged)
    // top end 13 > 12: miss 1/13 = 7.7 % -> flagged
    assert(ok(DfMapping.map(AlphaGrid.Pilot, 13.0 +: inside.tail, curve)).flagged)
    // top end 12.5: miss 0.5/12.5 = 4 % -> not flagged (and the ridge is not altered, it saturates at 0)
    val near = ok(DfMapping.map(AlphaGrid.Pilot, 12.5 +: inside.tail, curve))
    assert(!near.flagged)
    assertEquals(near.ridges.head, 0.0)
    // bottom end 1.5 < limit 2: miss 0.5/1.5 = 33 % -> flagged
    assert(ok(DfMapping.map(AlphaGrid.Pilot, inside.init :+ 1.5, curve)).flagged)
    // an interior target out of reach does not flag by itself
    assert(!ok(DfMapping.map(AlphaGrid.Pilot, inside.updated(4, 13.0), curve)).flagged)

  test("map refuses a wrong number of targets or diagnostics"):
    val curve = EdfCurve(r => 2.0 + 10.0 / (1.0 + r), 12.0, 2.0)
    assert(DfMapping.map(AlphaGrid.Pilot, Vector(5.0), curve).isLeft)
    assert(DfMapping.map(AlphaGrid.Pilot, Vector.fill(9)(5.0), curve, Some(Vector(1.0))).isLeft)
    assert(scala.util.Try(EdfCurve(identity, 1.0, 2.0)).isFailure)

  test("the scale diagnostic is carried through and never changes the ridges"):
    val curve = EdfCurve(r => 2.0 + 10.0 / (1.0 + r), 12.0, 2.0)
    val targets = Vector.tabulate(9)(k => 11.0 - k)
    val withDiag = ok(DfMapping.map(AlphaGrid.Pilot, targets, curve, Some(Vector.fill(9)(123.0))))
    assertEquals(withDiag.scaleDiagnosticRidges, Some(Vector.fill(9)(123.0)))
    assertEquals(withDiag.ridges, ok(DfMapping.map(AlphaGrid.Pilot, targets, curve)).ridges)

  // ------------------------------------------------------------------------------------------------- scale factors

  test("centring factor is 1 - 1/n_c per trial; a one-trial condition is refused"):
    val c = ok(PenaltyScale.centringByTrial(Array(0, 0, 0, 1, 1, 2, 2, 2, 2)))
    c.toVector.zip(Vector(2.0 / 3, 2.0 / 3, 2.0 / 3, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75)).foreach((a, b) => assertEqualsDouble(a, b, 1e-15))
    assert(PenaltyScale.centringByTrial(Array(0, 0, 1)).isLeft)
    assert(PenaltyScale.centringByTrial(Array(0, -1)).isLeft)
    assertEquals(PenaltyScale.centringByTrial(Array.empty[Int]), Left(DfMappingRefusal.EmptyDesign))

  test("derive recovers kappa^2 exactly for proportional regressors and records the spread otherwise"):
    val phrf = Array(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    val cond = Array(0, 0, 0, 1, 1, 1)
    val s = ok(PenaltyScale.derive(phrf.map(_ * 6.25), phrf, cond))
    assertEqualsDouble(s.shape, 6.25, 1e-14)
    assertEqualsDouble(s.shapeSpread, 0.0, 1e-14)
    assertEqualsDouble(s.centring, 2.0 / 3.0, 1e-15)
    assertEqualsDouble(s.centringSpread, 0.0, 1e-15)
    assertEqualsDouble(s.penalty(4.0), 6.25 * 2.0 / 3.0 * 4.0, 1e-12)
    val skew = ok(PenaltyScale.derive(Array(2.0, 3.0, 4.0, 5.0, 6.0, 7.0).zip(Array(1.0, 1.0, 1.0, 1.1, 1.1, 1.1)).map(_ * _), phrf, cond))
    assert(skew.shapeSpread > 0.0)
    assert(PenaltyScale.derive(phrf, phrf.take(3), cond).isLeft)
    assert(PenaltyScale.derive(Array(1.0, 0.0), Array(1.0, 1.0), Array(0, 0)).isLeft)

  test("unit change: rLSS in regressor units x^R = kappa x^P with p = kappa^2 c lambda equals the PHRF-unit single-deviation penalty"):
    // PHRF-unit model: one trial deviates against its condition level, penalised lambda (1 - 1/n_c) delta_P^2 with regressor
    // x^P. rLSS sees x^R = kappa x^P and ridge scale.penalty(lambda). Amplitudes must agree as a_P = kappa a_R.
    val rng = new TrialSynth.Lcg(31L)
    val rows = 90
    val n = 9
    val onsets = Vector.tabulate(n)(i => 4.0 + 9.0 * i + 3.0 * rng.uniform())
    val xP = DMat.tabulate(rows, n)((t, i) => TrialSynth.bump(t - onsets(i)))
    val f = DMat.tabulate(rows, 2)((t, c) => if c == 0 then 1.0 else (t - 45.0) / 90.0)
    val groups = Vector.tabulate(n)(_ % 3)
    val y = DMat.tabulate(rows, 3)((t, v) => (0 until n).map(i => xP(t, i) * (groups(i) + v + 0.7 * i)).sum + 0.3 * rng.normal())
    val kappa = 2.5
    val xR = DMat.tabulate(rows, n)((t, i) => kappa * xP(t, i))
    val norm2P = Array.tabulate(n)(i => (0 until rows).map(t => xP(t, i) * xP(t, i)).sum)
    val norm2R = Array.tabulate(n)(i => (0 until rows).map(t => xR(t, i) * xR(t, i)).sum)
    val scale = ok(PenaltyScale.derive(norm2R, norm2P, groups.toArray))
    assertEqualsDouble(scale.shape, kappa * kappa, 1e-12)
    val lambda = 8.0
    val rlss = RidgeLeastSquaresSeparate
      .fit(LssTrialDesign.unsafe(xR), ResponseBlock.unsafe(y), LssFixedDesign.unsafe(f), RidgeLssGrouping.ByGroup(groups), scale.penalty(lambda))
      .fold(e => fail(e.message), identity)
    // dense oracle in PHRF units: augmented least squares with penalty row sqrt(lambda (1 - 1/n_c))
    val centring = 1.0 - 1.0 / 3.0
    var worst = 0.0
    var i = 0
    while i < n do
      val cols = 3 + 1 + 2
      val d = DMat.tabulate(rows + 1, cols): (t, c) =>
        if t == rows then (if c == 3 then math.sqrt(lambda * centring) else 0.0)
        else if c < 3 then (0 until n).filter(groups(_) == c).map(j => xP(t, j)).sum
        else if c == 3 then xP(t, i)
        else f(t, c - 4)
      val rhs = DMat.tabulate(rows + 1, 3)((t, v) => if t == rows then 0.0 else y(t, v))
      val beta = d.qr(QROptions(QRPivoting.Column, Some(1e-12))).solveLeastSquares(rhs).fold(e => fail(e.toString), identity)
      var v = 0
      while v < 3 do
        val ampP = beta(groups(i), v) + beta(3, v)
        worst = math.max(worst, math.abs(ampP - kappa * rlss.amplitudes(i, v)) / math.max(1.0, math.abs(ampP)))
        v += 1
      i += 1
    assert(worst < 1e-10, s"PHRF-unit and rLSS-unit amplitudes disagree: $worst")

  // ------------------------------------------------------------------------------------------------- against TrialBanded

  private val step = PositiveSeconds(0.2).fold(e => fail(e.message), identity)
  private lazy val basis = HrfKernelBasis
    .compile(KernelBasisSpec(GaussianFamily.Default, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(e => fail(e.message), identity)

  private val runLen = 80
  private val lambda = 2.5

  /** The generator's sample times: `k * TR` from 0 within each run (never the frame's default `TR / 2` start). */
  private def generatorSampleTimes(runs: Int, tr: Double): Vector[Double] =
    Vector.tabulate(runs)(_ => Vector.tabulate(runLen)(k => k * tr)).flatten

  private def frame = SamplingFrame(blockLens = Seq(runLen, runLen), tr = Seq(1.0, 1.0), startTime = Seq(0.0, 0.0))

  test("the sampling frame used for the PHRF-side design starts at the generator's first sample time, not TR/2"):
    assertEquals(frame.samples(global = false).map(_.value), generatorSampleTimes(2, 1.0))
    val defaulted = SamplingFrame(blockLens = Seq(runLen, runLen), tr = Seq(1.0, 1.0))
    assertEquals(defaulted.samples(global = false).head.value, 0.5, "the default start is TR/2: it must be set explicitly")

  private def fixture =
    val membership = TrialMembership.make(Vector(0, 0, 1, 1, 2, 2), 3).fold(e => fail(e.message), identity)
    val onsets = Vector(2.5, 8.0, 15.0, 21.0, 4.0, 14.0)
    val expanded = ExpandedTrialDesign
      .lower(onsets.map(Seconds(_)), Vector(0, 0, 0, 0, 1, 1), Vector.fill(6)(Seconds(0.0)), membership, frame, basis, Seconds(0.2))
      .fold(e => fail(e.message), identity)
    val center = ShapePoint.unsafe(Vector.tabulate(basis.family.dimension)(a => 0.5 * (basis.family.chart.lower(a) + basis.family.chart.upper(a))))
    val coefficients = new Array[Double](basis.rank)
    basis.coefficientsInto(center, new Array[Double](basis.fineCount), coefficients)
    (expanded, membership, coefficients)

  test("phrfNorm2 equals the squared norms of the shape regressors read from the raw expanded design"):
    val (expanded, _, c) = fixture
    val prep = TrialBandedPreparation.prepare(expanded, None, None, lambda).fold(e => fail(e.message), identity)
    val got = ok(PenaltyScale.phrfNorm2(prep, c))
    val n = expanded.trials
    val m = basis.rank
    val source = expanded.term.data.data
    (0 until n).foreach { i =>
      val col = (0 until expanded.rows).map { t =>
        (0 until m).map(p => source(t * n * m + p * n + i) * c(p)).sum
      }
      val expected = col.map(x => x * x).sum
      assert(expected > 0.0)
      assertEqualsDouble(got(i), expected, 1e-10 * expected, s"trial $i")
    }
    assert(PenaltyScale.phrfNorm2(prep, c.take(1)).isLeft)

  test("the centring factor equals e_i' (lambda (I - M)) e_i / lambda of TrialBanded's penalty structure"):
    val (expanded, membership, _) = fixture
    val prep = TrialBandedPreparation.prepare(expanded, None, None, lambda).fold(e => fail(e.message), identity)
    assertEqualsDouble(prep.lambda, lambda, 0.0)
    val cond = Array.tabulate(prep.trials)(i => prep.membership.conditionOfTrial(i))
    val centring = ok(PenaltyScale.centringByTrial(cond))
    // the penalty matrix of the dense oracle that TrialBanded is verified against: lambda (delta_ij - same(i, j) / n_c)
    (0 until prep.trials).foreach { i =>
      val n = membership.trialsOf(membership.conditionOfTrial(i)).length
      val penaltyII = prep.lambda * (1.0 - 1.0 / n)
      assertEqualsDouble(penaltyII / prep.lambda, centring(i), 1e-15)
    }

  test("derive on the real TrialBanded regressors recovers the scale of an rLSS regressor built as kappa times them"):
    val (expanded, _, c) = fixture
    val prep = TrialBandedPreparation.prepare(expanded, None, None, lambda).fold(e => fail(e.message), identity)
    val phrf = ok(PenaltyScale.phrfNorm2(prep, c))
    val cond = Array.tabulate(prep.trials)(i => prep.membership.conditionOfTrial(i))
    val s = ok(PenaltyScale.derive(phrf.map(_ * 0.04), phrf, cond))
    assertEqualsDouble(s.shape, 0.04, 1e-14)
    assertEqualsDouble(s.centring, 0.5, 1e-15)
    assertEqualsDouble(s.penalty(prep.lambda), 0.04 * 0.5 * lambda, 1e-14)
