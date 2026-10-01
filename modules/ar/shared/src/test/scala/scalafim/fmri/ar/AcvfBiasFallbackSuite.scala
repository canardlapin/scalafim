package scalafim.fmri.ar

import gale.linalg.{DMat, DVec, LinAlgError, Matrix, Vec}
import scalafim.fmri.ar.fixtures.FmriArBiasRFixture

/** Solve-fallback provenance (a correction that silently used the raw estimate must not be reported as applied)
  * and reuse of a correction prepared once for a design and layout.
  */
class AcvfBiasFallbackSuite extends munit.FunSuite:

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))

  private def layoutFor(runLengths: Vector[Int], censorOneBased: Vector[Int]): NoiseEstimationLayout =
    NoiseEstimationLayout
      .excludingRows(TimeSegments.fromRunLengths(runLengths), runLengths.sum, censorOneBased.map(_ - 1).toSet)
      .fold(e => fail(e.message), identity)

  // --- correct(): every fallback path, with the typed reason --------------------------------------------

  private val Identity = Matrix.tabulate(2, 2)((r, c) => if r == c then 1.0 else 0.0)

  test("a healthy solve returns the corrected vector") {
    assertEquals(AcvfBias.correct(Vector(2.0, 0.5, 0.25), Identity), Right(Vector(2.0, 0.5, 0.25)))
  }

  test("fallback: the solved block fails the reciprocal-condition gate") {
    val nearlySingular = Matrix.tabulate(2, 2)((r, c) => if r == 1 && c == 1 then 1.0 + 1e-9 else 1.0)
    AcvfBias.correct(Vector(1.0, 0.5), nearlySingular) match
      case Left(CorrectionFallback.IllConditionedBlock(rcond)) =>
        assert(rcond < AcvfBias.ReciprocalConditionFloor, clues(rcond))
      case other => fail(s"expected IllConditionedBlock, got $other")
  }

  test("fallback: the linear solve fails") {
    val failing: (DMat, DVec) => Either[LinAlgError, DVec] = (_, _) => Left(LinAlgError.RankDeficient(1, 2))
    assertEquals(AcvfBias.correct(Vector(1.0, 0.5), Identity, failing), Left(CorrectionFallback.SingularSystem))
  }

  test("fallback: the solution is not finite") {
    val nan: (DMat, DVec) => Either[LinAlgError, DVec] = (_, _) => Right(Vec(Double.NaN, 1.0))
    assertEquals(AcvfBias.correct(Vector(1.0, 0.5), Identity, nan), Left(CorrectionFallback.NonFiniteSolution))
  }

  test("fallback: the corrected variance is not positive (real solve)") {
    // [[1, 2], [0, 1]] x = (1, 1) gives x = (-1, 1).
    val upper = matrix(Vector(Vector(1.0, 2.0), Vector(0.0, 1.0)))
    AcvfBias.correct(Vector(1.0, 1.0), upper) match
      case Left(CorrectionFallback.NonPositiveVariance(v)) => assertEqualsDouble(v, -1.0, 1e-12)
      case other                                           => fail(s"expected NonPositiveVariance, got $other")
  }

  test("fallback: the raw variance is not positive, so no correction is attempted") {
    assertEquals(
      AcvfBias.correct(Vector(0.0, 0.0), Identity),
      Left(CorrectionFallback.NonPositiveRawVariance(0.0))
    )
    val zeros = Matrix.tabulate(10, 1)((_, _) => 0.0)
    val pooled = ArEstimation
      .pooledAutocovariance(zeros, TimeSegments.continuous(10), ArOrderValue.unsafe(1), Some(Identity))
      .fold(e => fail(e.message), identity)
    assertEquals(pooled.correctionFallback, Some(CorrectionFallback.NonPositiveRawVariance(0.0)))
    assert(!pooled.correctionApplied)
  }

  // --- reporting through NoiseAcvf and NoiseFit ----------------------------------------------------------

  private val Rows = 40
  private val layout = layoutFor(Vector(Rows), Vector.empty)
  // Strongly autocorrelated: lag-one covariance close to the variance.
  private val smooth: DMat = Matrix.tabulate(Rows, 1)((r, _) => math.sin(0.15 * r) + 0.01 * math.cos(3.1 * r))

  /** A hand-built correction that passed the gate but whose solve cannot give a positive variance on `smooth`. */
  private def craftedPrepared(a: DMat, status: RunCorrection): PreparedCorrection =
    PreparedCorrection(AcvfBiasMatrices(1, 1, Rows - 1, Vector(a)), Vector(status), layout, None)

  private val Upper = matrix(Vector(Vector(1.0, 2.0), Vector(0.0, 1.0)))

  test("NoiseAcvf reports the solve fallback and returns the raw autocovariance, as fmriAR does") {
    val prepared = craftedPrepared(Upper, RunCorrection.Applied(0.11))
    val got = NoiseAcvf.estimateWith(smooth, layout, 1, NoisePooling.Run, prepared).fold(e => fail(e.message), identity)
    got.corrections match
      case Vector(RunCorrection.SolveFallback(CorrectionFallback.NonPositiveVariance(_))) => ()
      case other => fail(s"expected SolveFallback, got $other")
    assert(!got.corrected)
    assertEquals(got.units.head.fallback.isDefined, true)
    val raw = NoiseAcvf.estimate(smooth, layout, 1, NoisePooling.Run).fold(e => fail(e.message), identity)
    assertEquals(got.units.head.acvf, raw.units.head.acvf)
    assertEquals(raw.corrections, Vector(RunCorrection.Uncorrected))
  }

  test("NoiseFit reports SolveFallback instead of Applied, and its plan equals the raw fit") {
    val prepared = craftedPrepared(Upper, RunCorrection.Applied(0.11))
    val options = ArFitOptions(order = ArOrder.Fixed(1), exactFirstAr1 = false)
    val fit = NoiseFit.estimateBound(smooth, layout, options, prepared).fold(e => fail(e.message), identity)
    fit.corrections match
      case Vector(RunCorrection.SolveFallback(_)) => ()
      case other                                  => fail(s"expected SolveFallback, got $other")
    val raw = ArEstimation.fitNoise(smooth, layout, options).fold(e => fail(e.message), identity)
    assertEquals(fit.plan.coefficients, raw.coefficients)
  }

  test("a correction that solves cleanly stays Applied") {
    val prepared = craftedPrepared(Identity, RunCorrection.Applied(1.0))
    val got = NoiseAcvf.estimateWith(smooth, layout, 1, NoisePooling.Run, prepared).fold(e => fail(e.message), identity)
    assertEquals(got.corrections, Vector(RunCorrection.Applied(1.0)))
    assert(got.corrected)
  }

  test("the conditioning gate status is preserved when the gate already rejected the run") {
    val prepared = craftedPrepared(Identity, RunCorrection.IllConditioned(1e-9))
    val got = NoiseAcvf.estimateWith(smooth, layout, 1, NoisePooling.Run, prepared).fold(e => fail(e.message), identity)
    assertEquals(got.corrections, Vector(RunCorrection.IllConditioned(1e-9)))
  }

  // --- prepared corrections ---------------------------------------------------------------------------

  private val fixture = FmriArBiasRFixture.fitCases.find(_.name == "two_runs_censored").get
  private val fixtureLayout = layoutFor(fixture.runLengths, fixture.censorOneBased)
  private val design = matrix(fixture.design)
  private val residuals = matrix(fixture.residuals)
  private val budget = CorrectionBudget.Fixed(fixture.correctionMaxLag)
  private val options = ArFitOptions(order = ArOrder.Fixed(2), pooling = NoisePooling.Run, exactFirstAr1 = false)

  private def prepared: PreparedCorrection =
    AcvfBias.prepare(design, fixtureLayout, budget, 2).fold(e => fail(e.message), identity)

  test("prepared and unprepared estimation are bit-identical (plan, gamma, sigma2, status, acvf)") {
    val policy = EstimationPolicy.DesignCorrected(design, budget)
    val ready = prepared
    val readyFor3 = AcvfBias.prepare(design, fixtureLayout, budget, 3).fold(e => fail(e.message), identity)
    Seq(NoisePooling.Run, NoisePooling.Global).foreach { pooling =>
      val o = ArFitOptions(order = options.order, pooling = pooling, exactFirstAr1 = false)
      val a = ArEstimation.fitNoise(residuals, fixtureLayout, o, policy).fold(e => fail(e.message), identity)
      val b = ArEstimation.fitNoise(residuals, fixtureLayout, o, design, ready).fold(e => fail(e.message), identity)
      assertEquals(b.coefficients, a.coefficients)

      val fa = NoiseFit.estimate(residuals, fixtureLayout, o, policy).fold(e => fail(e.message), identity)
      val fb = NoiseFit.estimate(residuals, fixtureLayout, o, design, ready).fold(e => fail(e.message), identity)
      assertEquals(fb.plan.coefficients, fa.plan.coefficients)
      assertEquals(fb.acvf, fa.acvf)
      assertEquals(fb.innovationVariance, fa.innovationVariance)
      assertEquals(fb.corrections, fa.corrections)

      val aa = NoiseAcvf.estimate(residuals, fixtureLayout, 3, pooling, policy).fold(e => fail(e.message), identity)
      val ab = NoiseAcvf.estimate(residuals, fixtureLayout, 3, pooling, design, readyFor3).fold(e => fail(e.message), identity)
      assertEquals(ab, aa)
    }
  }

  test("one prepared correction serves many residual sets (single columns) identically") {
    val ready = prepared
    val policy = EstimationPolicy.DesignCorrected(design, budget)
    (0 until residuals.cols).foreach { col =>
      val one = Matrix.tabulate(residuals.rows, 1)((r, _) => residuals(r, col))
      val a = ArEstimation.fitNoise(one, fixtureLayout, options, policy).fold(e => fail(e.message), identity)
      val b = ArEstimation.fitNoise(one, fixtureLayout, options, design, ready).fold(e => fail(e.message), identity)
      assertEquals(b.coefficients, a.coefficients)
    }
  }

  test("a prepared correction is bound to its layout and row count") {
    val ready = prepared
    val otherLayout = layoutFor(fixture.runLengths, Vector(3))
    assertEquals(
      ArEstimation.fitNoise(residuals, otherLayout, options, design, ready).left.toOption,
      Some(ArError.PreparedCorrectionLayoutMismatch)
    )
    val shortResiduals = Matrix.tabulate(residuals.rows - 1, residuals.cols)((r, c) => residuals(r, c))
    val shortLayout = layoutFor(Vector(fixture.runLengths.head, fixture.runLengths(1) - 1), fixture.censorOneBased)
    assert(
      ArEstimation.fitNoise(shortResiduals, shortLayout, options, design, ready).left.exists(_.isInstanceOf[ArError.DesignRowMismatch])
    )
  }

  test("orthogonality is validated per residual set even with a prepared correction") {
    val notResiduals = Matrix.tabulate(residuals.rows, residuals.cols)((r, c) => math.sin(1.7 * r + c) + 2.0)
    assert(
      ArEstimation.fitNoise(notResiduals, fixtureLayout, options, design, prepared).left.exists(_.isInstanceOf[ArError.DesignResidualMismatch])
    )
  }

  test("prepare rejects a design that does not match the layout") {
    val short = Matrix.tabulate(design.rows - 1, design.cols)((r, c) => design(r, c))
    assertEquals(
      AcvfBias.prepare(short, fixtureLayout, budget, 2).left.toOption,
      Some(ArError.DesignRowMismatch(design.rows - 1, design.rows))
    )
  }

  // --- a run whose residuals are exactly zero --------------------------------------------------------------

  /** Two runs with per-run intercepts; run 2's residuals are exactly zero (orthogonal to any design). */
  private def zeroRunCase =
    val lengths = Vector(20, 20)
    val x = Matrix.tabulate(40, 2)((r, c) => if (r < 20) == (c == 0) then 1.0 else 0.0)
    val raw = Vector.tabulate(20)(i => math.sin(0.9 * i) + 0.3 * math.cos(2.3 * i))
    val mean = raw.sum / raw.length
    val residuals = Matrix.tabulate(40, 1)((r, _) => if r < 20 then raw(r) - mean else 0.0)
    (x, residuals, layoutFor(lengths, Vector.empty))

  test("a run of exactly-zero residuals is reported as a fallback, not Applied (policy path)") {
    val (x, r, l) = zeroRunCase
    val options = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = false)
    val fit = NoiseFit
      .estimate(r, l, options, EstimationPolicy.DesignCorrected(x, CorrectionBudget.Fixed(4)))
      .fold(e => fail(e.message), identity)
    fit.corrections match
      case Vector(RunCorrection.Applied(_), RunCorrection.SolveFallback(CorrectionFallback.NonPositiveRawVariance(_))) => ()
      case other => fail(s"expected the zero run to report NonPositiveRawVariance, got $other")
    val acvf = NoiseAcvf
      .estimate(r, l, 1, NoisePooling.Run, EstimationPolicy.DesignCorrected(x, CorrectionBudget.Fixed(4)))
      .fold(e => fail(e.message), identity)
    assertEquals(acvf.corrections, fit.corrections)
  }

  // --- runs that never reach the solve ------------------------------------------------------------------------

  test("a fully censored run is NotAttempted, not Applied") {
    val lengths = Vector(20, 20)
    val x = Matrix.tabulate(40, 2)((r, c) => if (r < 20) == (c == 0) then 1.0 else 0.0)
    val raw = Vector.tabulate(20)(i => math.sin(0.9 * i) + 0.3 * math.cos(2.3 * i))
    val mean = raw.sum / raw.length
    val r = Matrix.tabulate(40, 1)((row, _) => if row < 20 then raw(row) - mean else 0.0)
    val l = layoutFor(lengths, (21 to 40).toVector)
    val options = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = false)
    val fit = NoiseFit
      .estimate(r, l, options, EstimationPolicy.DesignCorrected(x, CorrectionBudget.Fixed(4)))
      .fold(e => fail(e.message), identity)
    fit.corrections match
      case Vector(RunCorrection.Applied(_), RunCorrection.NotAttempted(CorrectionSkip.FewerThanTwoObservations)) => ()
      case other => fail(s"expected NotAttempted for the empty run, got $other")
  }

  // --- binding: design, order, and bit-identity beyond the plan -------------------------------------------------

  private def sameDMat(a: DMat, b: DMat): Boolean =
    a.rows == b.rows && a.cols == b.cols && (0 until a.rows).forall(r => (0 until a.cols).forall(c => a(r, c) == b(r, c)))

  private def assertSameFit(a: NoiseFit, b: NoiseFit): Unit =
    assertEquals(b.plan.coefficients, a.plan.coefficients)
    assertEquals(b.acvf, a.acvf)
    assertEquals(b.innovationVariance, a.innovationVariance)
    assertEquals(b.corrections, a.corrections)
    (a.biasMatrices, b.biasMatrices) match
      case (Some(x), Some(y)) =>
        assertEquals((y.requestedLag, y.lag, y.residualDf), (x.requestedLag, x.lag, x.residualDf))
        assert(x.byRun.zip(y.byRun).forall { case (p, q) => sameDMat(p, q) }, "bias matrices must be bit-identical")
      case other => fail(s"expected bias matrices on both, got $other")

  test("prepared and unprepared are bit-identical for a fallback run, including biasMatrices") {
    val (x, r, l) = zeroRunCase
    val options = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = false)
    val budget = CorrectionBudget.Fixed(4)
    val a = NoiseFit.estimate(r, l, options, EstimationPolicy.DesignCorrected(x, budget)).fold(e => fail(e.message), identity)
    val ready = AcvfBias.prepare(x, l, budget, 1).fold(e => fail(e.message), identity)
    val b = NoiseFit.estimate(r, l, options, x, ready).fold(e => fail(e.message), identity)
    assert(a.corrections.exists(_.isInstanceOf[RunCorrection.SolveFallback]))
    assertSameFit(a, b)
  }

  test("prepared and unprepared are bit-identical for Auto order (gate-rejected short run included)") {
    val c = FmriArBiasRFixture.fitCases.find(_.name == "runs_70_and_14_auto").get
    val l = layoutFor(c.runLengths, c.censorOneBased)
    val x = matrix(c.design)
    val r = matrix(c.residuals)
    val budget = CorrectionBudget.Fixed(c.correctionMaxLag)
    val options = ArFitOptions(order = ArOrder.Auto(4), pooling = NoisePooling.Run, exactFirstAr1 = false)
    val a = NoiseFit.estimate(r, l, options, EstimationPolicy.DesignCorrected(x, budget)).fold(e => fail(e.message), identity)
    val ready = AcvfBias.prepare(x, l, budget, 4).fold(e => fail(e.message), identity)
    val b = NoiseFit.estimate(r, l, options, x, ready).fold(e => fail(e.message), identity)
    assertSameFit(a, b)
  }

  test("a nested design is refused: same rows and layout, different columns or entries") {
    val ready = prepared
    val fewerColumns = Matrix.tabulate(design.rows, design.cols - 1)((r, c) => design(r, c))
    assertEquals(
      ArEstimation.fitNoise(residuals, fixtureLayout, options, fewerColumns, ready).left.toOption,
      Some(ArError.PreparedCorrectionDesignMismatch)
    )
    // One entry differing in the last bit is also a different design.
    val tweaked = Matrix.tabulate(design.rows, design.cols)((r, c) =>
      if r == 3 && c == 1 then java.lang.Math.nextUp(design(r, c)) else design(r, c)
    )
    assertEquals(
      NoiseAcvf.estimate(residuals, fixtureLayout, 2, NoisePooling.Run, tweaked, ready).left.toOption,
      Some(ArError.PreparedCorrectionDesignMismatch)
    )
    assert(ArEstimation.fitNoise(residuals, fixtureLayout, options, design, ready).isRight)
  }

  test("a prepared correction refuses a different AR order") {
    val ready = prepared // prepared for order 2
    val order3 = ArFitOptions(order = ArOrder.Fixed(3), pooling = NoisePooling.Run, exactFirstAr1 = false)
    assertEquals(
      ArEstimation.fitNoise(residuals, fixtureLayout, order3, design, ready).left.toOption,
      Some(ArError.PreparedCorrectionOrderMismatch(2, 3))
    )
  }

  test("the unprepared path validates residuals before building any bias matrix") {
    // Orthogonality failure must win over a lag-budget problem that would otherwise surface first.
    val notResiduals = Matrix.tabulate(residuals.rows, residuals.cols)((r, c) => math.sin(1.7 * r + c) + 2.0)
    val result = ArEstimation.fitNoise(
      notResiduals,
      fixtureLayout,
      options,
      EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(0))
    )
    assert(result.left.exists(_.isInstanceOf[ArError.DesignResidualMismatch]), clues(result))
  }
