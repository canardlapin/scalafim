package scalafim.fmri.group.scenarios

import scalafim.image.SampleSpaces

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend, SubjectId}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.fit.{DenseFmriFitResult, FitError, FitPlanExecutor, TContrast, TContrastResult}
import scalafim.fmri.group.{
  FirstLevel,
  GroupDesign,
  GroupEngine,
  GroupError,
  GroupModel,
  GroupSpace,
  GroupStatistic,
  GroupWeighting
}
import scalafim.fmri.group.GroupTestMatrix
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import scalafim.image.{DMat as ImageDMat, SomeSampleSpace}
import gale.linalg.{DMat, Matrix}

class FirstLevelToGroupKnownEffectScenarioSuite extends munit.FunSuite:
  private val Tol = 1e-10
  private val PValueTol = 1e-8
  private val ContrastName = "task"

  test("known effects survive public first-level fit, bridge, and fixed-effects group inference") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = knownEffectFixture
    val firstLevels = fixture.subjects.map(runFirstLevel)
    val results =
      firstLevels.map(firstLevel => (firstLevel.subject -> ContrastName) -> firstLevel.contrast).toMap
    val data = groupValue(
      FirstLevel.groupData(
        space = GroupSpace.SampleAxis(fixture.samples),
        subjects = fixture.subjectIds,
        contrasts = Vector(ContrastName),
        results = results
      )
    )
    val model = groupValue(
      GroupModel.build(data, GroupDesign.intercept(fixture.subjectIds.length), GroupWeighting.InverseVariance)
    )
    val groupResult = groupValue(GroupEngine.fit(model))
    val groupFit = groupResult.fit(ContrastName).get
    val intercept = groupFit.term("(Intercept)").get
    val bridged = data.response(ContrastName).get
    val expectedEffects = fixture.effectMatrix
    val expectedStandardErrors = fixture.standardErrorRows
    val expectedVariances = fixture.varianceMatrix
    val expectedGroup = fixedEffectsOracle(fixture.effectRows, expectedStandardErrors)

    val observations =
      Vector(
        ScenarioCheck.fact(
          "subject order",
          data.subjects == fixture.subjectIds,
          s"actual=${data.subjects.map(_.value).mkString(",")}"
        ),
        ScenarioCheck.fact(
          "contrast names",
          data.contrasts == Vector(ContrastName),
          s"actual=${data.contrasts.mkString(",")} expected=$ContrastName"
        ),
        ScenarioCheck.fact(
          "variance capability",
          data.hasVariances,
          s"hasVariances=${data.hasVariances}"
        ),
        ScenarioCheck.fact(
          "group weighting",
          groupResult.weighting == GroupWeighting.InverseVariance,
          s"actual=${groupResult.weighting.label} expected=${GroupWeighting.InverseVariance.label}"
        ),
        ScenarioCheck.fact(
          "group statistic",
          intercept.statistic == GroupStatistic.Normal,
          s"actual=${intercept.statistic.label} expected=${GroupStatistic.Normal.label}"
        ),
        ScenarioCheck.finite("first-level statistics finite", firstLevels.flatMap(_.contrast.statistics.toSeq.toVector)),
        ScenarioCheck.finite("group estimates finite", intercept.estimates.toSeq.toVector),
        ScenarioCheck.finite("group standard errors finite", intercept.standardErrors.toSeq.toVector),
        ScenarioCheck.finite("group statistics finite", intercept.statistics.toSeq.toVector),
        ScenarioCheck.finite("group p-values finite", intercept.pValues.toSeq.toVector)
      ) ++
        ScenarioCheck.matrix("first-level task estimates", firstLevelEffectMatrix(firstLevels), expectedEffects, Tol) ++
        ScenarioCheck.matrix(
          "first-level task standard errors",
          firstLevelStandardErrorMatrix(firstLevels),
          GroupTestMatrix.fromRows(expectedStandardErrors),
          Tol
        ) ++
        ScenarioCheck.matrix("bridged task effects", bridged.effects, expectedEffects, Tol) ++
        ScenarioCheck.matrix("bridged task variances", bridged.variances.get, expectedVariances, Tol) ++
        ScenarioCheck.vector("fixed-effects estimate", intercept.estimates, expectedGroup.map(_.estimate), Tol) ++
        ScenarioCheck.vector("fixed-effects standard error", intercept.standardErrors, expectedGroup.map(_.standardError), Tol) ++
        ScenarioCheck.vector("fixed-effects z statistic", intercept.statistics, expectedGroup.map(_.statistic), Tol) ++
        ScenarioCheck.vector("fixed-effects p value", intercept.pValues, expectedGroup.map(_.pValue), PValueTol)

    ScenarioResult("group.first-level-to-group-known-effect.v1", observations)

  private def runFirstLevel(subject: SubjectFixture): FirstLevelFit =
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId(s"scenario-known-effect-${subject.id.value}"),
          ImageDMat.fromRows(subject.responseRows),
          SampleSpaces(Vector(subject.samples, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(KnownDesign.task.length), tr = Seq(1.0)),
        events = DatasetEvents(
          KnownDesign.task.indices.toVector.map { i =>
            Map("onset" -> i.toString, "task" -> KnownDesign.task(i).toString)
          }
        )
      )
    val nuisance = Mat.fromRows(KnownDesign.motion.map(value => Vector(value)))
    val plan =
      FmriModelBuilder.buildPlan(
        dataset,
        ModelBuildSpec(
          formula = "onset ~ covariate(task)",
          baselineIntercept = Intercept.Global,
          nuisance = Some(
            NuisanceRegressors(
              matrices = Vector(nuisance),
              names = Some(Vector(Vector("motion_x"))),
              check = NuisanceCheck.Drop
            )
          )
        )
      )
    val fit = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val contrast = fitValue(TContrast(ContrastName, Map("task" -> 1.0)).evaluate(fit))

    FirstLevelFit(subject.id, contrast)

  private def firstLevelEffectMatrix(firstLevels: Vector[FirstLevelFit]): DMat =
    GroupTestMatrix.fromRows(firstLevels.map(_.contrast.estimates.toSeq.toVector))

  private def firstLevelStandardErrorMatrix(firstLevels: Vector[FirstLevelFit]): DMat =
    GroupTestMatrix.fromRows(firstLevels.map(_.contrast.standardErrors.toSeq.toVector))

  private def fitValue[A](e: Either[FitError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def groupValue[A](e: Either[GroupError, A]): A =
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

  private final case class FirstLevelFit(subject: SubjectId, contrast: TContrastResult)

  private final case class SubjectFixture(
      id: SubjectId,
      effects: Vector[Double],
      intercepts: Vector[Double],
      motionCoefficients: Vector[Double],
      residualScales: Vector[Double]
  ):
    require(effects.length == samples, "effect count must match samples")
    require(intercepts.length == samples, "intercept count must match samples")
    require(motionCoefficients.length == samples, "motion coefficient count must match samples")
    require(residualScales.length == samples, "residual scale count must match samples")

    def samples: Int = effects.length

    def standardErrors: Vector[Double] =
      residualScales.map(_ / math.sqrt(5.0))

    def responseRows: Vector[Vector[Double]] =
      KnownDesign.task.indices.toVector.map { time =>
        effects.indices.toVector.map { sample =>
          intercepts(sample) +
            effects(sample) * KnownDesign.task(time) +
            motionCoefficients(sample) * KnownDesign.motion(time) +
            residualScales(sample) * KnownDesign.residualPattern(sample, time)
        }
      }

  private final case class KnownEffectFixture(subjects: Vector[SubjectFixture]):
    def subjectIds: Vector[SubjectId] =
      subjects.map(_.id)

    def samples: Int =
      subjects.head.samples

    def effectRows: Vector[Vector[Double]] =
      subjects.map(_.effects)

    def standardErrorRows: Vector[Vector[Double]] =
      subjects.map(_.standardErrors)

    def effectMatrix: DMat =
      GroupTestMatrix.fromRows(effectRows)

    def varianceMatrix: DMat =
      GroupTestMatrix.fromRows(standardErrorRows.map(row => row.map(se => se * se)))

  private object KnownDesign:
    val task: Vector[Double] =
      Vector(-1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0)

    val motion: Vector[Double] =
      Vector(-1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0)

    private val residualPatterns: Vector[Vector[Double]] =
      Vector(
        Vector(-1.0, 1.0, 1.0, -1.0, 1.0, -1.0, -1.0, 1.0),
        Vector(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, -1.0),
        Vector(1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0)
      )

    def residualPattern(sample: Int, time: Int): Double =
      residualPatterns(sample)(time)

  private def knownEffectFixture: KnownEffectFixture =
    KnownEffectFixture(
      Vector(
        SubjectFixture(
          id = SubjectId("s1"),
          effects = Vector(0.40, -0.10, 0.20),
          intercepts = Vector(1.0, 0.5, -0.25),
          motionCoefficients = Vector(0.15, -0.05, 0.10),
          residualScales = Vector(0.08, 0.09, 0.07)
        ),
        SubjectFixture(
          id = SubjectId("s2"),
          effects = Vector(0.55, 0.00, 0.25),
          intercepts = Vector(1.1, 0.6, -0.15),
          motionCoefficients = Vector(0.10, -0.02, 0.12),
          residualScales = Vector(0.10, 0.08, 0.09)
        ),
        SubjectFixture(
          id = SubjectId("s3"),
          effects = Vector(0.65, 0.10, 0.30),
          intercepts = Vector(0.9, 0.4, -0.35),
          motionCoefficients = Vector(0.12, -0.06, 0.08),
          residualScales = Vector(0.07, 0.11, 0.08)
        ),
        SubjectFixture(
          id = SubjectId("s4"),
          effects = Vector(0.50, 0.05, 0.35),
          intercepts = Vector(1.2, 0.7, -0.10),
          motionCoefficients = Vector(0.18, -0.04, 0.11),
          residualScales = Vector(0.09, 0.10, 0.10)
        ),
        SubjectFixture(
          id = SubjectId("s5"),
          effects = Vector(0.70, 0.15, 0.40),
          intercepts = Vector(1.05, 0.55, -0.20),
          motionCoefficients = Vector(0.14, -0.03, 0.09),
          residualScales = Vector(0.08, 0.07, 0.11)
        )
      )
    )
