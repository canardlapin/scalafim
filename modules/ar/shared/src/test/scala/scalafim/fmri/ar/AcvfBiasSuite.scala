package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

/** Contract tests for the explicit estimation policy: the raw default is unchanged, and every way the correction
  * can be misapplied is a typed error rather than a silent fallback.
  */
class AcvfBiasSuite extends munit.FunSuite:

  private val Rows = 40

  private def design: DMat =
    Matrix.tabulate(Rows, 3)((r, c) =>
      c match
        case 0 => 1.0
        case 1 => r.toDouble / Rows
        case _ => math.sin(0.9 * r + 0.2)
    )

  /** OLS residuals of deterministic AR(1)-like data against `design`, via a normal-equations projection. */
  private def olsResiduals(x: DMat, y: DMat): DMat =
    val coefficients = x.leastSquares(y).fold(e => fail(e.toString), identity)
    val fitted = x * coefficients
    Matrix.tabulate(y.rows, y.cols)((r, c) => y(r, c) - fitted(r, c))

  private def noise(rows: Int, cols: Int): DMat =
    val data = Array.ofDim[Double](rows, cols)
    var c = 0
    while c < cols do
      var r = 0
      while r < rows do
        val raw = math.sin((r + 1) * 12.9898 + (c + 1) * 78.233) * 43758.5453
        val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
        data(r)(c) = innovation + (if r == 0 then 0.0 else 0.5 * data(r - 1)(c))
        r += 1
      c += 1
    Matrix.tabulate(rows, cols)((r, c) => data(r)(c))

  private lazy val residuals: DMat = olsResiduals(design, noise(Rows, 4))
  private val segments = TimeSegments.continuous(Rows)
  private val options = ArFitOptions(order = ArOrder.Fixed(2), exactFirstAr1 = false)

  private def layout(excluded: Set[Int] = Set.empty): NoiseEstimationLayout =
    NoiseEstimationLayout.excludingRows(segments, Rows, excluded).fold(e => fail(e.message), identity)

  test("Raw is the default and reproduces the legacy entry points exactly") {
    val legacy = ArEstimation.fitNoise(residuals, segments, options)
    val explicit = ArEstimation.fitNoise(residuals, segments, options, EstimationPolicy.Raw)
    val viaLayout = ArEstimation.fitNoise(residuals, layout(), options, EstimationPolicy.Raw)
    assertEquals(explicit.map(_.coefficients), legacy.map(_.coefficients))
    assertEquals(viaLayout.map(_.coefficients), legacy.map(_.coefficients))
  }

  test("DesignCorrected is a different estimator from Raw on the same residuals") {
    val raw = ArEstimation.fitNoise(residuals, segments, options).fold(e => fail(e.message), identity)
    val corrected = ArEstimation
      .fitNoise(residuals, segments, options, EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(10)))
      .fold(e => fail(e.message), identity)
    assertNotEquals(raw.coefficients, corrected.coefficients)
  }

  test("residuals that are not orthogonal to the design are rejected with a typed error") {
    val notResiduals = noise(Rows, 4)
    val result = ArEstimation.fitNoise(
      notResiduals,
      segments,
      options,
      EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(10))
    )
    result match
      case Left(ArError.DesignResidualMismatch(relative, tolerance)) =>
        assert(relative > tolerance, clues(relative, tolerance))
      case other => fail(s"expected DesignResidualMismatch, got $other")
  }

  test("a design with the wrong row count is rejected") {
    val short = Matrix.tabulate(Rows - 1, 3)((r, c) => design(r, c))
    val result = ArEstimation.fitNoise(residuals, segments, options, EstimationPolicy.DesignCorrected(short))
    assertEquals(result.left.toOption, Some(ArError.DesignRowMismatch(Rows - 1, Rows)))
  }

  test("a non-finite design entry is rejected") {
    val bad = Matrix.tabulate(Rows, 3)((r, c) => if r == 4 && c == 1 then Double.NaN else design(r, c))
    val result = AcvfBias.matrices(bad, layout(), 5)
    result match
      case Left(ArError.NonFiniteDesign(row, column, value)) =>
        assertEquals((row, column), (4, 1))
        assert(value.isNaN)
      case other => fail(s"expected NonFiniteDesign, got $other")
  }

  test("a lag budget below one is rejected") {
    assertEquals(AcvfBias.matrices(design, layout(), 0).left.toOption, Some(ArError.InvalidCorrectionLag(0)))
    val fixed = EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(0))
    assertEquals(ArEstimation.fitNoise(residuals, segments, options, fixed).left.toOption, Some(ArError.InvalidCorrectionLag(0)))
    val adaptive = EstimationPolicy.DesignCorrected(design, CorrectionBudget.Adaptive(-3))
    assertEquals(ArEstimation.fitNoise(residuals, segments, options, adaptive).left.toOption, Some(ArError.InvalidCorrectionLag(-3)))
  }

  test("a design that leaves no residual degrees of freedom is a typed error (R warns and proceeds)") {
    val saturated = Matrix.tabulate(5, 5)((r, c) => if r == c then 1.0 else 0.0)
    val tiny = NoiseEstimationLayout.allRows(TimeSegments.continuous(5), 5).fold(e => fail(e.message), identity)
    assertEquals(AcvfBias.matrices(saturated, tiny, 2).left.toOption, Some(ArError.NoResidualDegreesOfFreedom(5, 5)))
  }

  test("the lag budget is capped when the design leaves too few residual degrees of freedom") {
    val wide = Matrix.tabulate(12, 7)((r, c) =>
      if c == 0 then 1.0 else math.sin((c + 1) * 0.37 * (r + 1) + c)
    )
    val small = NoiseEstimationLayout.allRows(TimeSegments.continuous(12), 12).fold(e => fail(e.message), identity)
    val result = AcvfBias.matrices(wide, small, 9).fold(e => fail(e.message), identity)
    assertEquals(result.residualDf, 5)
    assertEquals(result.requestedLag, 9)
    assertEquals(result.lag, 4)
    assert(result.budgetCapped)
  }

  private def adaptiveLag(order: Int, ceiling: Int, x: DMat = design, rows: Int = Rows): Int =
    val l = NoiseEstimationLayout.allRows(TimeSegments.continuous(rows), rows).fold(e => fail(e.message), identity)
    val r = olsResiduals(x, noise(rows, 3))
    val policy = EstimationPolicy.DesignCorrected(x, CorrectionBudget.Adaptive(ceiling))
    val fitted = NoiseFit.estimate(r, l, ArFitOptions(order = ArOrder.Fixed(order), exactFirstAr1 = false), policy)
    fitted.fold(e => fail(e.message), identity).biasMatrices.map(_.lag).getOrElse(fail("no matrices"))

  test("fmrireg's adaptive budget: max(order, min(ceiling, max(5, 2p+1), floor(rdf / 3)))") {
    // rdf = 37: supported = 12.
    assertEquals(adaptiveLag(order = 1, ceiling = 25), 5) // desired = 5
    assertEquals(adaptiveLag(order = 4, ceiling = 25), 9) // desired = 9
    assertEquals(adaptiveLag(order = 4, ceiling = 7), 7) // ceiling binds
    assertEquals(adaptiveLag(order = 2, ceiling = 1), 2) // never below the order
    // rdf = 14 - 3 = 11: supported = 3, so the order floor wins over the supported lags.
    val x = Matrix.tabulate(14, 3)((r, c) => if c == 0 then 1.0 else math.sin((c + 1) * 0.9 * r + c))
    assertEquals(adaptiveLag(order = 2, ceiling = 25, x, 14), 3)
    assertEquals(adaptiveLag(order = 5, ceiling = 25, x, 14), 5)
  }

  test("fixed order beyond the estimable lags stays a typed error under the corrected policy") {
    val fragmented = layout((0 until Rows).filter(_ % 2 == 1).toSet)
    val result = ArEstimation.fitNoise(
      residuals,
      fragmented,
      ArFitOptions(order = ArOrder.Fixed(2), exactFirstAr1 = false),
      EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(5))
    )
    assert(result.left.exists(_.isInstanceOf[ArError.ArOrderNotEstimable]), clues(result))
  }

  test("global and run pooling both accept the corrected policy on a two-run design") {
    val lengths = Vector(20, 20)
    val twoRuns = TimeSegments.fromRunLengths(lengths)
    val x = Matrix.tabulate(Rows, 4)((r, c) =>
      c match
        case 0 => if r < 20 then 1.0 else 0.0
        case 1 => if r < 20 then 0.0 else 1.0
        case 2 => r.toDouble / Rows
        case _ => math.sin(0.9 * r)
    )
    val r = olsResiduals(x, noise(Rows, 4))
    val policy = EstimationPolicy.DesignCorrected(x, CorrectionBudget.Fixed(8))
    Seq(NoisePooling.Global, NoisePooling.Run).foreach { pooling =>
      val plan = ArEstimation.fitNoise(r, twoRuns, ArFitOptions(order = ArOrder.Fixed(1), pooling = pooling, exactFirstAr1 = false), policy)
      assert(plan.isRight, clues(pooling, plan))
    }
  }
