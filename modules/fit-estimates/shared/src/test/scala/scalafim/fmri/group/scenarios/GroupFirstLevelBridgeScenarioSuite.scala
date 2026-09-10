package scalafim.fmri.group.scenarios

import scalafim.fmri.fit.estimates.FitGroupAdapter

import scalafim.dataset.SubjectId
import scalafim.fmri.fit.{ResidualDegreesOfFreedom, TContrastResult}
import scalafim.fmri.group.{
  GroupData,
  GroupDesign,
  GroupEngine,
  GroupError,
  GroupModel,
  GroupSpace,
  GroupStatistic,
  GroupWeighting,
  VarianceCapability
}
import scalafim.fmri.group.GroupTestMatrix
import gale.linalg.{DMat, DVec, Matrix, Vec}

class GroupFirstLevelBridgeScenarioSuite extends munit.FunSuite:
  private val Tol = 1e-10
  private val PValueTol = 1e-8

  test("first-level T contrast results bridge into fixed-effects group inference") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = bridgeFixture
    val data = value(
      FitGroupAdapter.groupData(
        space = GroupSpace.SampleAxis(fixture.samples),
        subjects = fixture.subjects,
        contrasts = Vector("faces", "places"),
        results = fixture.results
      )
    )
    val model = value(GroupModel.build(data, GroupDesign.intercept(fixture.subjects.length), GroupWeighting.InverseVariance))
    val result = value(GroupEngine.fit(model))
    val facesFit = result.fit("faces").get
    val faces = facesFit.term("(Intercept)").get
    val expectedFaces = fixedEffectsOracle(fixture.estimates("faces"), fixture.standardErrors("faces"))
    val bridgedFaces = data.response("faces").get
    val bridgedPlaces = data.response("places").get

    val observations =
      Vector(
        ScenarioCheck.fact(
          "data has variances",
          data.hasVariances,
          s"hasVariances=${data.hasVariances}"
        ),
        ScenarioCheck.fact(
          "contrast names",
          data.contrasts == Vector("faces", "places"),
          s"actual=${data.contrasts.mkString(",")}"
        ),
        ScenarioCheck.fact(
          "subject order",
          data.subjects == fixture.subjects,
          s"actual=${data.subjects.map(_.value).mkString(",")}"
        ),
        ScenarioCheck.fact(
          "group weighting",
          result.weighting == GroupWeighting.InverseVariance,
          s"actual=${result.weighting.label} expected=${GroupWeighting.InverseVariance.label}"
        ),
        ScenarioCheck.fact(
          "group statistic",
          faces.statistic == GroupStatistic.Normal,
          s"actual=${faces.statistic.label} expected=z"
        ),
        ScenarioCheck.fact(
          "sample count",
          faces.estimates.length == fixture.samples,
          s"actual=${faces.estimates.length} expected=${fixture.samples}"
        ),
        ScenarioCheck.finite("faces estimates finite", faces.estimates.toSeq.toVector),
        ScenarioCheck.finite("faces standard errors finite", faces.standardErrors.toSeq.toVector),
        ScenarioCheck.finite("faces z statistics finite", faces.statistics.toSeq.toVector),
        ScenarioCheck.finite("faces p-values finite", faces.pValues.toSeq.toVector)
      ) ++
        ScenarioCheck.matrix("bridged faces effects", bridgedFaces.effects, GroupTestMatrix.fromRows(fixture.estimates("faces")), Tol) ++
        ScenarioCheck.matrix("bridged faces variances", bridgedFaces.variances.get, varianceMatrix(fixture.standardErrors("faces")), Tol) ++
        ScenarioCheck.matrix("bridged places effects", bridgedPlaces.effects, GroupTestMatrix.fromRows(fixture.estimates("places")), Tol) ++
        ScenarioCheck.matrix("bridged places variances", bridgedPlaces.variances.get, varianceMatrix(fixture.standardErrors("places")), Tol) ++
        ScenarioCheck.vector("fixed-effects estimate", faces.estimates, expectedFaces.map(_.estimate), Tol) ++
        ScenarioCheck.vector("fixed-effects standard error", faces.standardErrors, expectedFaces.map(_.standardError), Tol) ++
        ScenarioCheck.vector("fixed-effects z statistic", faces.statistics, expectedFaces.map(_.statistic), Tol) ++
        ScenarioCheck.vector("fixed-effects p value", faces.pValues, expectedFaces.map(_.pValue), PValueTol)

    ScenarioResult("group.first-level-bridge.v1", observations)

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private final case class FixedEffectsOracle(
      estimate: Double,
      standardError: Double,
      statistic: Double,
      pValue: Double
  )

  private def fixedEffectsOracle(
      estimates: Vector[Vector[Double]],
      standardErrors: Vector[Vector[Double]]
  ): Vector[FixedEffectsOracle] =
    val samples = estimates.head.length
    (0 until samples).toVector.map { sample =>
      val weights =
        standardErrors.map { row =>
          val se = row(sample)
          1.0 / (se * se)
        }
      val denominator = weights.sum
      val estimate =
        estimates.zip(weights).map { case (row, weight) => row(sample) * weight }.sum / denominator
      val standardError = math.sqrt(1.0 / denominator)
      val statistic = estimate / standardError

      FixedEffectsOracle(
        estimate = estimate,
        standardError = standardError,
        statistic = statistic,
        pValue = GroupStatistic.Normal.twoSidedP(statistic)
      )
    }

  private def varianceMatrix(standardErrors: Vector[Vector[Double]]): DMat =
    GroupTestMatrix.fromRows(
      standardErrors.map(row => row.map(se => se * se))
    )

  private final case class BridgeFixture(
      subjects: Vector[SubjectId],
      estimates: Map[String, Vector[Vector[Double]]],
      standardErrors: Map[String, Vector[Vector[Double]]]
  ):
    def samples: Int =
      estimates.values.head.head.length

    def results: Map[(SubjectId, String), TContrastResult] =
      val out = scala.collection.mutable.Map.empty[(SubjectId, String), TContrastResult]
      estimates.foreach { case (contrast, rows) =>
        val seRows = standardErrors(contrast)
        subjects.indices.foreach { subjectIndex =>
          out.update(
            subjects(subjectIndex) -> contrast,
            contrastResult(contrast, rows(subjectIndex), seRows(subjectIndex))
          )
        }
      }
      out.toMap

  private def contrastResult(
      name: String,
      estimates: Vector[Double],
      standardErrors: Vector[Double]
  ): TContrastResult =
    TContrastResult(
      name = name,
      estimates = DVec.fromSeq(estimates),
      standardErrors = DVec.fromSeq(standardErrors),
      statistics = DVec.fromSeq(estimates.zip(standardErrors).map { case (estimate, se) => estimate / se }),
      residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(80),
      voxelIndices = estimates.indices.toVector
    )

  private def bridgeFixture: BridgeFixture =
    BridgeFixture(
      subjects = Vector("s1", "s2", "s3", "s4").map(SubjectId(_)),
      estimates = Map(
        "faces" -> Vector(
          Vector(0.42, 0.10, -0.20),
          Vector(0.55, 0.20, -0.05),
          Vector(0.62, 0.15, 0.10),
          Vector(0.48, 0.30, 0.00)
        ),
        "places" -> Vector(
          Vector(-0.10, 0.35, 0.20),
          Vector(0.05, 0.45, 0.15),
          Vector(0.10, 0.40, 0.25),
          Vector(-0.05, 0.50, 0.30)
        )
      ),
      standardErrors = Map(
        "faces" -> Vector(
          Vector(0.20, 0.30, 0.25),
          Vector(0.25, 0.35, 0.20),
          Vector(0.22, 0.28, 0.24),
          Vector(0.18, 0.32, 0.22)
        ),
        "places" -> Vector(
          Vector(0.30, 0.25, 0.28),
          Vector(0.27, 0.30, 0.26),
          Vector(0.24, 0.29, 0.25),
          Vector(0.31, 0.27, 0.30)
        )
      )
    )
