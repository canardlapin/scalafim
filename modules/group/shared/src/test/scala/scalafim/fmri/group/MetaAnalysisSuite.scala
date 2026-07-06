package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.linalg.DoubleMatrix

class MetaAnalysisSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def column(values: Double*): DoubleMatrix =
    DoubleMatrix.fromRows(values.toVector.map(v => Vector(v)))

  private def metaFit(
      effects: DoubleMatrix,
      variances: DoubleMatrix,
      design: GroupDesign,
      weighting: GroupWeighting
  ): GroupFit =
    val data = value(GroupData.single(subjects(effects.rows), GroupSpace.SampleAxis(effects.cols), "c", effects, Some(variances)))
    value(GroupEngine.fit(value(GroupModel.build(data, design, weighting)))).fit("c").get

  test("fixed-effects meta-analysis reproduces inverse-variance weighting") {
    // metafor rma(c(0.5,0.3), vi=c(0.04,0.09), method="FE"):
    //   estimate 0.438462, se 0.166410, z 2.634921, Q 0.307692, I2 0.
    val fit = metaFit(column(0.5, 0.3), column(0.04, 0.09), GroupDesign.intercept(2), GroupWeighting.InverseVariance)

    assertEquals(fit.statistic, GroupStatistic.Normal)
    // coef = sum(w y)/sum(w) = 57/130; se = sqrt(1/sum w) = sqrt(9/325).
    assertEqualsDouble(fit.coefficients(0, 0), 57.0 / 130.0, 1e-12)
    assertEqualsDouble(fit.standardErrors(0, 0), math.sqrt(9.0 / 325.0), 1e-12)
    // z is coefficient / standard error, tested against a standard normal.
    assertEqualsDouble(
      fit.term("(Intercept)").get.statistics(0),
      fit.coefficients(0, 0) / fit.standardErrors(0, 0),
      1e-12
    )

    val het = fit.heterogeneity.get
    assertEqualsDouble(het.q(0), 0.3076923077, 1e-9)
    assertEqualsDouble(het.i2(0), 0.0, 1e-12)
    assertEqualsDouble(het.tau2(0), 0.0, 1e-12)
  }

  test("random-effects meta-analysis reproduces DerSimonian-Laird") {
    // metafor rma(c(0.2,0.5,0.9), vi=rep(0.01,3), method="DL"):
    //   tau2 0.113333, estimate 0.533333, se 0.202759, Q 24.6667, I2 0.918919.
    val fit = metaFit(column(0.2, 0.5, 0.9), column(0.01, 0.01, 0.01), GroupDesign.intercept(3), GroupWeighting.RandomEffects())

    assertEquals(fit.statistic, GroupStatistic.Normal)
    assertEqualsDouble(fit.coefficients(0, 0), 0.5333333333, 1e-9)
    // se = sqrt(1 / sum w*) = sqrt(74/1800).
    assertEqualsDouble(fit.standardErrors(0, 0), math.sqrt(74.0 / 1800.0), 1e-12)

    val het = fit.heterogeneity.get
    assertEqualsDouble(het.tau2(0), 0.1133333333, 1e-9)
    assertEqualsDouble(het.q(0), 24.6666666667, 1e-8)
    assertEqualsDouble(het.i2(0), 0.9189189189, 1e-9)
  }

  test("random effects collapses to fixed effects with no heterogeneity") {
    // Q < df here, so tau2 = 0 and RE == FE.
    val effects = column(0.5, 0.3)
    val variances = column(0.04, 0.09)
    val fe = metaFit(effects, variances, GroupDesign.intercept(2), GroupWeighting.InverseVariance)
    val re = metaFit(effects, variances, GroupDesign.intercept(2), GroupWeighting.RandomEffects())
    assertEqualsDouble(re.heterogeneity.get.tau2(0), 0.0, 1e-12)
    assertEqualsDouble(re.coefficients(0, 0), fe.coefficients(0, 0), 1e-12)
    assertEqualsDouble(re.standardErrors(0, 0), fe.standardErrors(0, 0), 1e-12)
  }

  test("inverse-variance two-sample WLS gives weighted group difference") {
    // groups a = {1,2} (var 1), b = {4,6} (var 1): wmean a 1.5, wmean b 5,
    // diff 3.5, se sqrt(1/2 + 1/2) = 1, z 3.5; Cochran Q 2.5.
    val labels = Vector("a", "a", "b", "b")
    val effects = column(1.0, 2.0, 4.0, 6.0)
    val variances = column(1.0, 1.0, 1.0, 1.0)
    val design = value(GroupDesign.twoSample(labels))
    val fit = metaFit(effects, variances, design, GroupWeighting.InverseVariance)

    assertEqualsDouble(fit.coefficients(0, 0), 1.5, 1e-12)
    val diff = fit.term("b").get
    assertEqualsDouble(diff.estimates(0), 3.5, 1e-12)
    assertEqualsDouble(diff.standardErrors(0), 1.0, 1e-12)
    assertEqualsDouble(diff.statistics(0), 3.5, 1e-12)
    assertEqualsDouble(fit.heterogeneity.get.q(0), 2.5, 1e-12)
  }
