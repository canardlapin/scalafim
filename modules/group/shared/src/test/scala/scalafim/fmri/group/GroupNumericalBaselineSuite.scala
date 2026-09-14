package scalafim.fmri.group

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.SubjectId

class GroupNumericalBaselineSuite extends munit.FunSuite:
  private def value[A](result: Either[GroupError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    val out = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var col = 0
      while col < rows(row).length do
        out(row, col) = rows(row)(col)
        col += 1
      row += 1
    out.result()

  private def fit(
      designRows: Vector[Vector[Double]],
      termNames: Vector[String],
      effects: Vector[Vector[Double]],
      variances: Vector[Vector[Double]],
      weighting: GroupWeighting
  ): Either[GroupError, GroupFit] =
    val subjects = Vector.tabulate(designRows.length)(index => SubjectId(s"s$index"))
    for
      design <- GroupDesign.fromMatrix(matrix(designRows), termNames)
      data <- GroupData.withVariances(
        subjects,
        GroupSpace.SampleAxis(effects.head.length),
        "effect",
        matrix(effects),
        matrix(variances)
      )
      model <- GroupModel.build(data, design, weighting)
      result <- GroupEngine.fit(model)
      fitted <- result.fit("effect").toRight(GroupError.UnknownContrast("effect"))
    yield fitted

  test("DL and PM policies match an independent intercept-only reference"):
    val design = Vector.fill(5)(Vector(1.0))
    val effects = Vector(0.2, 0.5, -0.1, 1.0, 0.7).map(value => Vector(value))
    val variances = Vector(0.04, 0.09, 0.16, 0.25, 0.36).map(value => Vector(value))

    val dl = value(
      fit(
        design,
        Vector("(Intercept)"),
        effects,
        variances,
        GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal)
      )
    )
    assertEqualsDouble(dl.coefficients(0, 0), 0.3256131571372547, 2e-12)
    assertEqualsDouble(dl.standardErrors(0, 0), 0.1452297760029591, 2e-12)
    assertEqualsDouble(dl.heterogeneity.get.tau2(0), 0.0022295476419634344, 2e-12)
    assertEqualsDouble(dl.heterogeneity.get.q(0), 4.0727498272721565, 2e-12)
    assertEquals(dl.statistic, GroupStatistic.Normal)

    val pm = value(
      fit(
        design,
        Vector("(Intercept)"),
        effects,
        variances,
        GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung)
      )
    )
    assertEqualsDouble(pm.coefficients(0, 0), 0.3260177305479336, 2e-10)
    assertEqualsDouble(pm.standardErrors(0, 0), 0.1456431167404513, 2e-10)
    assertEqualsDouble(pm.heterogeneity.get.tau2(0), 0.002595546350388084, 2e-10)
    pm.statistic match
      case GroupStatistic.StudentT(df) => assertEqualsDouble(df.value, 4.0, 0.0)
      case other => fail(s"expected a Student-t policy, found $other")

  test("units, origins, and column order preserve a named physical contrast"):
    val baseDesign = Vector(
      Vector(1.0, 0.0, 20.0),
      Vector(1.0, 0.0, 25.0),
      Vector(1.0, 0.0, 30.0),
      Vector(1.0, 1.0, 35.0),
      Vector(1.0, 1.0, 40.0),
      Vector(1.0, 1.0, 45.0)
    )
    val effects = Vector(
      Vector(1.1, -0.2),
      Vector(1.4, 0.0),
      Vector(1.7, 0.1),
      Vector(2.2, 0.7),
      Vector(2.5, 0.8),
      Vector(2.9, 1.1)
    )
    val variances = Vector.tabulate(6)(row => Vector(0.08 + row * 0.02, 0.12 + row * 0.01))
    val modes = Vector(
      GroupWeighting.Unweighted,
      GroupWeighting.InverseVariance,
      GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal),
      GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung)
    )

    modes.foreach: mode =>
      val baseFit = value(fit(baseDesign, Vector("(Intercept)", "group", "age"), effects, variances, mode))
      val base = value(GroupContrast.unsafe("physical", Map("(Intercept)" -> 1.0, "group" -> 0.5, "age" -> 0.2)).evaluate(baseFit))

      val shiftedDesign = baseDesign.map(row => row.updated(2, row(2) + 100.0))
      val shiftedFit = value(fit(shiftedDesign, Vector("(Intercept)", "group", "age"), effects, variances, mode))
      val shifted = value(GroupContrast.unsafe("physical", Map("(Intercept)" -> 1.0, "group" -> 0.5, "age" -> 100.2)).evaluate(shiftedFit))

      val scale = 1e12
      val scaledDesign = baseDesign.map(row => row.updated(2, row(2) * scale))
      val scaledFit = value(fit(scaledDesign, Vector("(Intercept)", "group", "age"), effects, variances, mode))
      val scaled = value(GroupContrast.unsafe("physical", Map("(Intercept)" -> 1.0, "group" -> 0.5, "age" -> (0.2 * scale))).evaluate(scaledFit))

      val reorderedDesign = baseDesign.map(row => Vector(row(2), row(1), row(0)))
      val reorderedFit = value(fit(reorderedDesign, Vector("age", "group", "(Intercept)"), effects, variances, mode))
      val reordered = value(GroupContrast.unsafe("physical", Map("(Intercept)" -> 1.0, "group" -> 0.5, "age" -> 0.2)).evaluate(reorderedFit))

      var sample = 0
      while sample < base.estimates.length do
        assertEqualsDouble(shifted.estimates(sample), base.estimates(sample), 2e-8)
        assertEqualsDouble(shifted.standardErrors(sample), base.standardErrors(sample), 2e-8)
        assertEqualsDouble(scaled.estimates(sample), base.estimates(sample), 2e-8)
        assertEqualsDouble(scaled.standardErrors(sample), base.standardErrors(sample), 2e-8)
        assertEqualsDouble(reordered.estimates(sample), base.estimates(sample), 2e-8)
        assertEqualsDouble(reordered.standardErrors(sample), base.standardErrors(sample), 2e-8)
        sample += 1

  test("weighted partial failures retain usable samples and failure identity"):
    val design = Vector.fill(4)(Vector(1.0))
    val effects = Vector.tabulate(4)(row => Vector(row.toDouble, 1e308))
    val variances = Vector.fill(4)(Vector(1.0, 1.0))
    val fitResult = value(fit(design, Vector("(Intercept)"), effects, variances, GroupWeighting.InverseVariance))

    assertEquals(fitResult.failures.map(_.sample), Vector(1))
    assertEqualsDouble(fitResult.coefficients(0, 0), 1.5, 1e-12)
    assert(fitResult.coefficients(0, 1).isNaN)
    val contrast = value(GroupContrast.term("(Intercept)").evaluate(fitResult))
    assertEquals(contrast.failures, fitResult.failures)
    assert(contrast.pValues(0).isFinite)
    assert(contrast.pValues(1).isNaN)

    val allFailed = fit(
      design,
      Vector("(Intercept)"),
      effects.map(_.takeRight(1)),
      variances.map(_.takeRight(1)),
      GroupWeighting.InverseVariance
    )
    assert(allFailed.left.toOption.exists(_.isInstanceOf[GroupError.AllSamplesFailed]))

  test("extreme precision imbalance preserves coefficient units and covariance"):
    val design = Vector(
      Vector(1.0, 0.0),
      Vector(1.0, 0.0),
      Vector(0.0, 1.0),
      Vector(0.0, 1.0)
    )
    val effects = Vector(1.0, 3.0, 7.0, 9.0).map(value => Vector(value))
    val variances = Vector(1e-16, 1e-16, 1.0, 1.0).map(value => Vector(value))
    val result = value(fit(design, Vector("a", "b"), effects, variances, GroupWeighting.InverseVariance))

    assertEquals(result.failures, Vector.empty)
    assertEqualsDouble(result.coefficients(0, 0), 2.0, 1e-12)
    // sqrt(W)X has condition number 1e8 here. Bound the recovered coefficient
    // at the corresponding first-order floating-point scale, while retaining
    // much tighter checks for the well-conditioned coefficient and covariance.
    assertEqualsDouble(result.coefficients(1, 0), 8.0, 2e-7)
    assertEqualsDouble(result.standardErrors(0, 0), math.sqrt(1e-16 / 2.0), 1e-18)
    assertEqualsDouble(result.standardErrors(1, 0), math.sqrt(0.5), 1e-12)

  test("invalid designs, weights, and zero contrasts remain typed failures"):
    val rankDeficient = Vector.fill(4)(Vector(1.0, 1.0))
    val effects = Vector(1.0, 2.0, 3.0, 4.0).map(value => Vector(value))
    val variances = Vector.fill(4)(Vector(1.0))
    assert(fit(rankDeficient, Vector("a", "b"), effects, variances, GroupWeighting.InverseVariance).isLeft)
    assert(GroupContrast.fromStrings("zero", Map("a" -> 0.0)).isLeft)
    assert(GroupContrast.fromStrings("bad", Map("a" -> Double.NaN)).isLeft)
    assert(GroupContrast.difference("self", "a", " a ").isLeft)
    assert(GroupDesign.twoSample(Vector("control", " control ", "control", " control ")).isLeft)

    val x = matrix(Vector.fill(4)(Vector(1.0)))
    val y = matrix(Vector.fill(4)(Vector(2.0)))
    assert(GroupGlm.wls(x, y, matrix(Vector.fill(3)(Vector(1.0)))).isLeft)
    assert(GroupGlm.wls(x, y, matrix(Vector.fill(4)(Vector(0.0)))).isLeft)
