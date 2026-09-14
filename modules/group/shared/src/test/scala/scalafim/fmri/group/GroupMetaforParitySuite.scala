package scalafim.fmri.group

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.SubjectId
import scalafim.fmri.group.fixtures.GroupMetaforFixtures
import scalafim.fmri.group.fixtures.GroupMetaforFixtures.{Case, Expected}

class GroupMetaforParitySuite extends munit.FunSuite:
  private def value[A](result: Either[GroupError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def matrix(rows: Int, cols: Int, values: Vector[Double]): DMat =
    assertEquals(values.length, rows * cols)
    Matrix.tabulate(rows, cols)((row, col) => values(row * cols + col))

  private def weighting(policy: String): GroupWeighting =
    policy match
      case "FE-z" => GroupWeighting.InverseVariance
      case "DL-z" => GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal)
      case "PM-z" => GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.Normal)
      case "DL-mKH" =>
        GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.ModifiedKnappHartung)
      case "PM-mKH" =>
        GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung)
      case other => fail(s"unknown fixture policy $other")

  private def close(actual: Double, expected: Double, clue: String): Unit =
    val tolerance = GroupMetaforFixtures.AbsoluteTolerance +
      GroupMetaforFixtures.RelativeTolerance * math.abs(expected)
    assertEqualsDouble(actual, expected, tolerance, clue)

  private def pClose(actual: Double, expected: Double, clue: String): Unit =
    assertEqualsDouble(actual, expected, GroupMetaforFixtures.PValueTolerance, clue)

  private def fitted(fixture: Case, expected: Expected): GroupFit =
    val designMatrix = matrix(fixture.subjects, fixture.terms.length, fixture.designRowMajor)
    val effects = matrix(fixture.subjects, 1, fixture.effects)
    val variances = matrix(fixture.subjects, 1, fixture.variances)
    val subjects = Vector.tabulate(fixture.subjects)(index => SubjectId(s"subject-$index"))
    val result = for
      design <- GroupDesign.fromMatrix(designMatrix, fixture.terms)
      data <- GroupData.withVariances(
        subjects,
        GroupSpace.SampleAxis(1),
        "effect",
        effects,
        variances
      )
      model <- GroupModel.build(data, design, weighting(expected.policy))
      group <- GroupEngine.fit(model)
      fit <- group.fit("effect").toRight(GroupError.UnknownContrast("effect"))
    yield fit
    value(result)

  private def covariance(fit: GroupFit, row: Int, col: Int): Double =
    val rowWeight = Array.fill(fit.terms)(0.0)
    rowWeight(row) = 1.0
    if row == col then fit.covariance.contrastVariance(rowWeight, 0)
    else
      val colWeight = Array.fill(fit.terms)(0.0)
      colWeight(col) = 1.0
      val sumWeight = rowWeight.clone()
      sumWeight(col) = 1.0
      val sumVariance = fit.covariance.contrastVariance(sumWeight, 0)
      val rowVariance = fit.covariance.contrastVariance(rowWeight, 0)
      val colVariance = fit.covariance.contrastVariance(colWeight, 0)
      (sumVariance - rowVariance - colVariance) / 2.0

  GroupMetaforFixtures.Cases.foreach { fixture =>
    fixture.expected.foreach { expected =>
      test(s"${fixture.id} ${expected.policy} matches ${GroupMetaforFixtures.Oracle}"):
        val fit = fitted(fixture, expected)
        assertEquals(fit.failures, Vector.empty)
        assertEquals(fit.terms, fixture.terms.length)
        assertEquals(expected.residualDf, fixture.subjects - fixture.terms.length)

        val expectedStatistic =
          if expected.policy.endsWith("mKH") then
            GroupStatistic.StudentT(DegreesOfFreedom.unsafe(expected.residualDf))
          else GroupStatistic.Normal
        assertEquals(fit.statistic, expectedStatistic)

        var term = 0
        while term < fixture.terms.length do
          val result = fit.term(fixture.terms(term)).getOrElse(fail(s"missing term ${fixture.terms(term)}"))
          close(fit.coefficients(term, 0), expected.coefficients(term), s"coefficient $term")
          close(fit.standardErrors(term, 0), expected.standardErrors(term), s"standard error $term")
          close(result.statistics(0), expected.statistics(term), s"statistic $term")
          pClose(result.pValues(0), expected.pValues(term), s"p-value $term")
          var col = 0
          while col < fixture.terms.length do
            close(
              covariance(fit, term, col),
              expected.covarianceRowMajor(term * fixture.terms.length + col),
              s"covariance ($term,$col)"
            )
            col += 1
          term += 1

        val heterogeneity = fit.heterogeneity.getOrElse(fail("missing heterogeneity result"))
        close(heterogeneity.tau2(0), expected.tau2, "tau2")
        close(heterogeneity.q(0), expected.fixedQ, "fixed Q")
    }
  }
