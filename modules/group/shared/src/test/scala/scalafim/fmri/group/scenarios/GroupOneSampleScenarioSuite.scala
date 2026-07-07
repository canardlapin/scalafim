package scalafim.fmri.group.scenarios

import scalafim.dataset.SubjectId
import scalafim.fmri.group.{GroupData, GroupDesign, GroupEngine, GroupError, GroupModel, GroupSpace, GroupStatistic}
import scalafim.linalg.DoubleMatrix

class GroupOneSampleScenarioSuite extends munit.FunSuite:
  private val Tol = 1e-10

  test("one-sample group fit has an analytic pass verdict across the sample axis") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val effectsBySubject =
      Vector(
        Vector(1.0, -1.0, 2.0),
        Vector(2.0, -0.5, 1.0),
        Vector(3.0, 0.0, 2.0),
        Vector(4.0, 0.5, 1.0),
        Vector(5.0, 1.0, 2.0),
        Vector(6.0, 1.5, 1.0)
      )
    val data = value(
      GroupData.single(
        subjects = subjects(effectsBySubject.length),
        space = GroupSpace.SampleAxis(3),
        contrast = "task",
        effects = DoubleMatrix.fromRows(effectsBySubject)
      )
    )
    val model = value(GroupModel.build(data, GroupDesign.intercept(effectsBySubject.length)))
    val fit = value(GroupEngine.fit(model)).fit("task").get
    val intercept = fit.term("(Intercept)").get
    val expected = columns(effectsBySubject).map(oneSampleOracle)

    val observations =
      Vector(
        ScenarioCheck.fact(
          "reference statistic",
          fit.statistic == GroupStatistic.unsafeStudentT(effectsBySubject.length - 1),
          s"actual=${fit.statistic.label} expected=t(${effectsBySubject.length - 1})"
        ),
        ScenarioCheck.fact(
          "sample count",
          intercept.estimates.length == expected.length,
          s"actual=${intercept.estimates.length} expected=${expected.length}"
        ),
        ScenarioCheck.finite("estimate finite", intercept.estimates.toVector),
        ScenarioCheck.finite("standard error finite", intercept.standardErrors.toVector),
        ScenarioCheck.finite("statistic finite", intercept.statistics.toVector),
        ScenarioCheck.finite("p-value finite", intercept.pValues.toVector)
      ) ++
        ScenarioCheck.vector("mean", intercept.estimates, expected.map(_.mean), Tol) ++
        ScenarioCheck.vector("standard error", intercept.standardErrors, expected.map(_.standardError), Tol) ++
        ScenarioCheck.vector("t statistic", intercept.statistics, expected.map(_.statistic), Tol) ++
        ScenarioCheck.vector("p value", intercept.pValues, expected.map(_.pValue), 1e-8)

    ScenarioResult("group.one-sample-analytic.v1", observations)

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def columns(rows: Vector[Vector[Double]]): Vector[Vector[Double]] =
    val nCols = rows.head.length
    (0 until nCols).toVector.map { col =>
      rows.map(row => row(col))
    }

  private final case class OneSampleOracle(mean: Double, standardError: Double, statistic: Double, pValue: Double)

  private def oneSampleOracle(values: Vector[Double]): OneSampleOracle =
    val n = values.length
    val mean = values.sum / n.toDouble
    val variance =
      values.map { value =>
        val centered = value - mean
        centered * centered
      }.sum / (n.toDouble - 1.0)
    val standardError = math.sqrt(variance / n.toDouble)
    val statistic = mean / standardError
    val reference = GroupStatistic.unsafeStudentT(n - 1)

    OneSampleOracle(
      mean = mean,
      standardError = standardError,
      statistic = statistic,
      pValue = reference.twoSidedP(statistic)
    )
