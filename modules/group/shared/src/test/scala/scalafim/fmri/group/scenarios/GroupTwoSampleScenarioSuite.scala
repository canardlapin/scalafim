package scalafim.fmri.group.scenarios

import scalafim.dataset.SubjectId
import scalafim.fmri.group.{
  GroupContrast,
  GroupData,
  GroupDesign,
  GroupEngine,
  GroupError,
  GroupModel,
  GroupSpace,
  GroupStatistic
}
import scalafim.fmri.group.GroupTestMatrix
import gale.linalg.{DMat, Matrix}

class GroupTwoSampleScenarioSuite extends munit.FunSuite:
  private val Tol = 1e-10
  private val PValueTol = 1e-8

  test("two-sample group fit has an analytic pass verdict across the sample axis") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val labels = Vector("control", "control", "control", "control", "patient", "patient", "patient", "patient", "patient")
    val effectsBySubject =
      Vector(
        Vector(1.0, 2.0, -1.0),
        Vector(2.0, 1.0, 0.0),
        Vector(3.0, 3.0, 1.0),
        Vector(4.0, 2.0, -2.0),
        Vector(5.0, 4.0, 2.0),
        Vector(6.0, 5.0, 1.0),
        Vector(7.0, 4.0, 3.0),
        Vector(8.0, 6.0, 2.0),
        Vector(9.0, 5.0, 4.0)
      )

    val design = value(GroupDesign.twoSample(labels))
    val data = value(
      GroupData.single(
        subjects = subjects(effectsBySubject.length),
        space = GroupSpace.SampleAxis(3),
        contrast = "task",
        effects = GroupTestMatrix.fromRows(effectsBySubject)
      )
    )
    val model = value(GroupModel.build(data, design))
    val fit = value(GroupEngine.fit(model)).fit("task").get
    val intercept = fit.term("(Intercept)").get
    val differenceTerm = fit.term("patient").get
    val namedDifference = value(GroupContrast.term("patient").evaluate(fit))
    val expected = columns(effectsBySubject).map(twoSampleOracle(labels, _, reference = "control", other = "patient"))
    val df = labels.length - 2

    val observations =
      Vector(
        ScenarioCheck.fact(
          "design terms",
          design.termNames == Vector("(Intercept)", "patient"),
          s"actual=${design.termNames.mkString(",")}"
        ),
        ScenarioCheck.fact(
          "reference statistic",
          fit.statistic == GroupStatistic.unsafeStudentT(df),
          s"actual=${fit.statistic.label} expected=t($df)"
        ),
        ScenarioCheck.fact(
          "sample count",
          differenceTerm.estimates.length == expected.length,
          s"actual=${differenceTerm.estimates.length} expected=${expected.length}"
        ),
        ScenarioCheck.finite("intercept estimate finite", intercept.estimates.toSeq.toVector),
        ScenarioCheck.finite("difference estimate finite", differenceTerm.estimates.toSeq.toVector),
        ScenarioCheck.finite("difference standard error finite", differenceTerm.standardErrors.toSeq.toVector),
        ScenarioCheck.finite("difference statistic finite", differenceTerm.statistics.toSeq.toVector),
        ScenarioCheck.finite("difference p-value finite", differenceTerm.pValues.toSeq.toVector)
      ) ++
        ScenarioCheck.vector("reference mean", intercept.estimates, expected.map(_.referenceMean), Tol) ++
        ScenarioCheck.vector("reference standard error", intercept.standardErrors, expected.map(_.referenceStandardError), Tol) ++
        ScenarioCheck.vector("difference estimate", differenceTerm.estimates, expected.map(_.difference), Tol) ++
        ScenarioCheck.vector("difference standard error", differenceTerm.standardErrors, expected.map(_.differenceStandardError), Tol) ++
        ScenarioCheck.vector("difference statistic", differenceTerm.statistics, expected.map(_.differenceStatistic), Tol) ++
        ScenarioCheck.vector("difference p value", differenceTerm.pValues, expected.map(_.differencePValue), PValueTol) ++
        ScenarioCheck.vector("named contrast estimate", namedDifference.estimates, expected.map(_.difference), Tol) ++
        ScenarioCheck.vector("named contrast standard error", namedDifference.standardErrors, expected.map(_.differenceStandardError), Tol) ++
        ScenarioCheck.vector("named contrast statistic", namedDifference.statistics, differenceTerm.statistics.toSeq.toVector, Tol) ++
        ScenarioCheck.vector("named contrast p value", namedDifference.pValues, differenceTerm.pValues.toSeq.toVector, PValueTol)

    ScenarioResult("group.two-sample-analytic.v1", observations)

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def columns(rows: Vector[Vector[Double]]): Vector[Vector[Double]] =
    val nCols = rows.head.length
    (0 until nCols).toVector.map { col =>
      rows.map(row => row(col))
    }

  private final case class TwoSampleOracle(
      referenceMean: Double,
      otherMean: Double,
      difference: Double,
      pooledVariance: Double,
      referenceStandardError: Double,
      differenceStandardError: Double,
      referenceStatistic: Double,
      differenceStatistic: Double,
      referencePValue: Double,
      differencePValue: Double
  )

  private def twoSampleOracle(
      labels: Vector[String],
      values: Vector[Double],
      reference: String,
      other: String
  ): TwoSampleOracle =
    val referenceValues = values.zip(labels).collect { case (value, label) if label == reference => value }
    val otherValues = values.zip(labels).collect { case (value, label) if label == other => value }
    val nReference = referenceValues.length
    val nOther = otherValues.length
    val df = nReference + nOther - 2
    val referenceMean = mean(referenceValues)
    val otherMean = mean(otherValues)
    val referenceVariance = sampleVariance(referenceValues, referenceMean)
    val otherVariance = sampleVariance(otherValues, otherMean)
    val pooledVariance =
      ((nReference - 1).toDouble * referenceVariance + (nOther - 1).toDouble * otherVariance) / df.toDouble
    val referenceStandardError = math.sqrt(pooledVariance / nReference.toDouble)
    val difference = otherMean - referenceMean
    val differenceStandardError = math.sqrt(pooledVariance * (1.0 / nReference.toDouble + 1.0 / nOther.toDouble))
    val referenceStatistic = referenceMean / referenceStandardError
    val differenceStatistic = difference / differenceStandardError
    val statistic = GroupStatistic.unsafeStudentT(df)

    TwoSampleOracle(
      referenceMean = referenceMean,
      otherMean = otherMean,
      difference = difference,
      pooledVariance = pooledVariance,
      referenceStandardError = referenceStandardError,
      differenceStandardError = differenceStandardError,
      referenceStatistic = referenceStatistic,
      differenceStatistic = differenceStatistic,
      referencePValue = statistic.twoSidedP(referenceStatistic),
      differencePValue = statistic.twoSidedP(differenceStatistic)
    )

  private def mean(values: Vector[Double]): Double =
    values.sum / values.length.toDouble

  private def sampleVariance(values: Vector[Double], mean: Double): Double =
    values.map { value =>
      val centered = value - mean
      centered * centered
    }.sum / (values.length.toDouble - 1.0)
