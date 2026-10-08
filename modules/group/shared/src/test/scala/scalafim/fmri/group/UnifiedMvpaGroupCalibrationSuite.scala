package scalafim.fmri.group

import gale.linalg.DMat
import scalafim.dataset.SubjectId
import scalafim.fmri.group.fixtures.UnifiedMvpaGroupOracle

/** Independent deterministic precursor to M5.04's simulation court.
  * These fixtures never certify Type-I error, coverage or a first-level df rule. */
class UnifiedMvpaGroupCalibrationSuite extends munit.FunSuite:
  private def right[A](result: Either[GroupError, A]): A = result.fold(error => fail(error.message), identity)
  private def close(actual: Double, expected: Double, label: String): Unit =
    assertEqualsDouble(actual, expected, 1e-10 * (1.0 + math.abs(expected)), label)

  private def policy(name: String): GroupWeighting = name match
    case "FE-z" => GroupWeighting.InverseVariance
    case "DL-z" => GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal)
    case "DL-mKH" => GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.ModifiedKnappHartung)
    case "PM-mKH" => GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung)
    case other => fail(s"unknown oracle policy $other")

  UnifiedMvpaGroupOracle.cases.foreach: fixture =>
    fixture.expected.foreach: expected =>
      test(s"${fixture.id} ${expected.policy}: independent uncertainty and heterogeneity oracle"):
        val k = fixture.terms.size
        val subjects = Vector.tabulate(fixture.n)(i => SubjectId(s"oracle-subject-$i"))
        val design = right(GroupDesign.fromMatrix(DMat.dense(fixture.n, k, fixture.design), fixture.terms))
        // Truth variances are retained in the fixture, never passed to the feasible fit.
        val data = right(GroupData.withVariances(subjects, GroupSpace.SampleAxis(1), "effect",
          DMat.dense(fixture.n, 1, fixture.effects), DMat.dense(fixture.n, 1, fixture.reportedVariances)))
        val fitted = right(GroupEngine.fit(right(GroupModel.build(data, design, policy(expected.policy)))))
          .fit("effect").getOrElse(fail("missing effect"))
        assertEquals(fitted.failures, Vector.empty)
        val df = fixture.n - k
        assertEquals(fitted.statistic, if expected.policy.endsWith("mKH") then GroupStatistic.unsafeStudentT(df)
          else GroupStatistic.Normal)
        // First-level df must not replace the subject-model residual df.
        fixture.firstLevelDf.foreach(first => assert(first != df.toDouble))
        for term <- 0 until k do
          close(fitted.coefficients(term, 0), expected.beta(term), s"beta-$term")
          close(fitted.standardErrors(term, 0), expected.se(term), s"se-$term")
          val contrast = right(GroupContrast.term(fixture.terms(term)).evaluate(fitted))
          close(contrast.pValues(0), expected.p(term), s"p-$term")
          // R supplies interval quantiles; check the public reference distribution
          // at their boundary rather than adding a local inverse-CDF helper.
          val critical = (expected.upper(term) - expected.beta(term)) / expected.se(term)
          close(fitted.statistic.twoSidedP(critical), .05, s"interval-reference-$term")
          close(fitted.coefficients(term, 0) - critical * fitted.standardErrors(term, 0), expected.lower(term), s"lower-$term")
          close(fitted.coefficients(term, 0) + critical * fitted.standardErrors(term, 0), expected.upper(term), s"upper-$term")
          val a = Array.tabulate(k)(i => if i == term then 1.0 else 0.0)
          val variance = fitted.covariance.contrastVariance(a, 0)
          close(variance, expected.covariance(term * k + term), s"variance-$term")
          for other <- 0 until term do
            val b = Array.tabulate(k)(i => if i == other then 1.0 else 0.0)
            val both = Array.tabulate(k)(i => a(i) + b(i))
            val covariance = (fitted.covariance.contrastVariance(both, 0) - variance -
              fitted.covariance.contrastVariance(b, 0)) / 2
            close(covariance, expected.covariance(term * k + other), s"covariance-$term-$other")
        val h = fitted.heterogeneity.getOrElse(fail("missing heterogeneity"))
        close(h.q(0), expected.q, "Q")
        close(h.i2(0), expected.i2, "I2")
        close(h.tau2(0), expected.tau, "tau2")

  test("known versus estimated first-level variance controls retain different feasible inputs"):
    val known = UnifiedMvpaGroupOracle.cases.find(_.id == "GADV-DF8.known-control.quantile").get
    val estimated = UnifiedMvpaGroupOracle.cases.find(_.id == "GADV-DF8.estimated.quantile").get
    assertEquals(known.effects, estimated.effects)
    assertEquals(known.trueVariances, estimated.trueVariances)
    assertEquals(known.trueVariances, known.reportedVariances)
    assertNotEquals(estimated.trueVariances, estimated.reportedVariances)
    assertEquals(known.firstLevelDf, None)
    assertEquals(estimated.firstLevelDf, Some(8.0))

  test("normal tails meet the independent R precision gate through the series boundary and deep tails"):
    UnifiedMvpaGroupOracle.normal.foreach: (z, survival, twoSided) =>
      def tolerance(expected: Double) = math.max(2e-322, math.abs(expected) * 3e-13)
      assertEqualsDouble(Distributions.normalSf(z), survival, tolerance(survival), s"survival-$z")
      assertEqualsDouble(Distributions.normalTwoSidedP(z), twoSided, tolerance(twoSided), s"two-sided-$z")
    assertEqualsDouble(Distributions.erfc(Double.PositiveInfinity), 0.0, 0.0)
    assertEqualsDouble(Distributions.erfc(Double.NegativeInfinity), 2.0, 0.0)
    assertEqualsDouble(Distributions.erfc(0.0), 1.0, 0.0)
    assert(Distributions.erfc(Double.NaN).isNaN)
