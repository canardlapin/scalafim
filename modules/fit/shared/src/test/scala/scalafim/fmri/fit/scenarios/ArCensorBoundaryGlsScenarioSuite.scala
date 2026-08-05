package scalafim.fmri.fit.scenarios

import scalafim.dataset.{
  DataSelection,
  DatasetEvents,
  DatasetId,
  FmriDataset,
  InMemoryDatasetBackend,
  SynchronousFmriDataset,
  TimepointSelection
}
import scalafim.fmri.ar.{InitialConditionPolicy, NoisePooling, WhiteningMethod}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.fit.fixtures.ArCensorGlsRFixture
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{
  ArCoefficientSpec,
  AutocorrelationConfig,
  FitPlan,
  FitStrategy,
  FmriModelBuilder,
  ModelBuildSpec,
  ModelError
}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.DVec

/** Public run/censor GLS workflow anchored to an independent exact-first R receipt. */
class ArCensorBoundaryGlsScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(2e-10, 2e-10)
  private val OracleProfile = ScenarioComparisonTolerance.bounded(1e-7, 1e-9, 0.999999999, 1e-9)
  private val ScenarioId = "fit.ar-censor-boundary-gls.v1"

  test("P3.4 row-deleted censor gaps bound public AR/GLS whitening") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = ArCensorGlsRFixture
    val selection =
      DataSelection(time = TimepointSelection.excluding(fixture.censoredTimepoints*))
    val fixedAutocorrelation = modelValue(
      ArCoefficientSpec.rho(fixture.rho).flatMap { coefficients =>
        AutocorrelationConfig(
          order = 1,
          iterations = 0,
          global = true,
          exactFirst = fixture.exactFirst,
          coefficients = coefficients
        )
      }
    )
    val fixedPlan = plan(fixture.responseRows, fixedAutocorrelation)
    val complete = dense(FitPlanExecutor.fit(fixedPlan, selection))
    val chunked = dense(
      FitPlanExecutor.fitChunked(
        fixedPlan,
        selection,
        fitValue(FitChunkingStrategy.byVoxelCount(2))
      )
    )
    val selectedSeries = datasetValue(fixedPlan.model.dataset.resolve(selection)).timepoints
    val selectedDesign = fitValue(MatrixAdapters.designMatrix(fixedPlan.model, fixture.selectedTimepoints)).value
    val selectedResponse = fitValue(
      fixedPlan.model.dataset.seriesEither(selection).left.map(error => FitError.InvalidFitAxis("selected response", error.message))
        .flatMap(MatrixAdapters.responseBlock)
    ).value
    val sanitizedRows =
      fixture.responseRows.zipWithIndex.map { case (row, timepoint) =>
        if fixture.censoredTimepoints.contains(timepoint) then Vector.fill(row.length)(0.0)
        else row
      }
    val sanitized = dense(FitPlanExecutor.fit(plan(sanitizedRows, fixedAutocorrelation), selection))
    val estimatedAutocorrelation = modelValue(
      AutocorrelationConfig(
        order = 1,
        iterations = 1,
        global = false,
        voxelwise = false,
        exactFirst = true,
        coefficients = ArCoefficientSpec.Estimate
      )
    )
    val estimated = dense(FitPlanExecutor.fit(plan(fixture.responseRows, estimatedAutocorrelation), selection))
    val schema = fixedPlan.model.designSchema.getOrElse(fail("AR/GLS model must expose a structural schema"))
    val task = sampled("task").coefficient
    val taskT = fitValue(
      task
        .named("p34-task", "task coefficient under segmented fixed AR(1)")
        .compile(schema)
        .flatMap(_.evaluate(complete))
    )
    val taskF = fitValue(
      task.asOmnibusRow
        .named("p34-task-f", "task coefficient omnibus under segmented fixed AR(1)")
        .compile(schema)
        .flatMap(_.evaluate(complete))
    )
    val whitening = complete.autocorrelation.getOrElse(fail("fixed GLS must report AR diagnostics")).whitening
    val estimatedDiagnostics = estimated.autocorrelation.getOrElse(fail("estimated GLS must report AR diagnostics"))
    val expectedSegments =
      fixture.segments.map(value =>
        ArWhiteningSegment(
          value.runIndex,
          value.startRow,
          value.endRowExclusive,
          value.startTimepoint,
          value.endTimepointExclusive
        )
      )
    val expectedGaps =
      fixture.censorGaps.map(value =>
        ArCensorGap(value.runIndex, value.startTimepoint, value.endTimepointExclusive)
      )
    val runPartitions = datasetValue(fixedPlan.model.dataset.runPartitionsEither(selection))
    val incompatiblePooling = AutocorrelationConfig(order = 1, global = true, voxelwise = true)
    val incompatibleFixedVoxelwise =
      ArCoefficientSpec.rho(fixture.rho).flatMap { coefficients =>
        AutocorrelationConfig(order = 1, voxelwise = true, coefficients = coefficients)
      }
    val rankComparison = fitValue(complete.rankPreviewComparison)

    val observations =
      Vector(
        ScenarioHarness.fact(
          "first-class censor selection",
          selectedSeries == fixture.selectedTimepoints && complete.timepoints == fixture.selectedTimepoints,
          s"selected=$selectedSeries result=${complete.timepoints} expected=${fixture.selectedTimepoints}"
        ),
        ScenarioHarness.fact(
          "isolated and consecutive censor gaps",
          whitening.censorGaps == expectedGaps && expectedGaps.map(_.timepoints) == Vector(Vector(3), Vector(7, 8), Vector(14, 15), Vector(20)),
          s"actual=${whitening.censorGaps}"
        ),
        ScenarioHarness.fact(
          "two-run whitening segments",
          whitening.segments == expectedSegments && whitening.segments.map(_.runIndex).distinct == Vector(0, 1),
          s"actual=${whitening.segments} expected=$expectedSegments"
        ),
        ScenarioHarness.fact(
          "fixed whitening policy",
          whitening.method == WhiteningMethod.Fixed &&
            whitening.pooling == NoisePooling.Global &&
            whitening.initialCondition == InitialConditionPolicy.ExactAr1,
          s"actual=$whitening"
        ),
        ScenarioHarness.fact(
          "run partitions preserve selected source time",
          runPartitions.map(_.timepoints) == Vector(fixture.selectedTimepoints.take(9), fixture.selectedTimepoints.drop(9)),
          s"actual=${runPartitions.map(_.timepoints)}"
        ),
        ScenarioHarness.fact(
          "censored scan leakage canary is present in source data",
          fixture.censoredTimepoints.forall(timepoint => fixture.responseRows(timepoint).exists(value => math.abs(value) >= 1e6)),
          s"censored=${fixture.censoredTimepoints}"
        ),
        ScenarioHarness.fact(
          "boundary leakage canary exercises every reset class",
          fixture.boundaryCanaryTimepoints == Vector(2, 6, 11, 13, 19) &&
            fixture.boundaryCanaryTimepoints.forall(timepoint => math.abs(fixture.responseRows(timepoint)(3)) > 200.0),
          s"canaries=${fixture.boundaryCanaryTimepoints.map(timepoint => fixture.responseRows(timepoint)(3))}"
        ),
        ScenarioHarness.fact(
          "fit rank and residual degrees of freedom",
          complete.olsDiagnostics.exists(_.rank == fixture.rank) &&
            complete.residualDegreesOfFreedom.value == fixture.residualDf &&
            rankComparison.differences.contains(RankPreviewDifference.GeneralizedLeastSquares),
          s"rank=${complete.olsDiagnostics.map(_.rank)} df=${complete.residualDegreesOfFreedom} differences=${rankComparison.differences}"
        ),
        ScenarioHarness.fact(
          "shared normalized covariance",
          complete.coefficientCovariance.scope == CoefficientCovarianceScope.Shared &&
            complete.coefficientCovariance.matrixCount == 1,
          s"scope=${complete.coefficientCovariance.scope} count=${complete.coefficientCovariance.matrixCount}"
        ),
        ScenarioHarness.fact(
          "estimated AR declares run pooling",
          estimatedDiagnostics.whitening.method == WhiteningMethod.Estimated &&
            estimatedDiagnostics.whitening.pooling == NoisePooling.Run &&
            estimatedDiagnostics.runs.map(_.method) == Vector("estimated", "estimated") &&
            estimatedDiagnostics.runs.forall(run => math.abs(run.rho) < 1.0),
          s"whitening=${estimatedDiagnostics.whitening} runs=${estimatedDiagnostics.runs}"
        ),
        ScenarioHarness.fact(
          "incompatible voxelwise shared policies fail at construction",
          incompatiblePooling.left.exists(_.message.contains("global and voxelwise")) &&
            incompatibleFixedVoxelwise.left.exists(_.message.contains("requires estimated coefficients")),
          s"pooling=$incompatiblePooling fixedVoxelwise=$incompatibleFixedVoxelwise"
        ),
        ScenarioHarness.fact(
          "chunked execution reuses one whitening topology",
          chunked.autocorrelation == complete.autocorrelation &&
            chunked.timepoints == complete.timepoints &&
            chunked.voxelIndices == complete.voxelIndices,
          s"complete=${complete.autocorrelation} chunked=${chunked.autocorrelation}"
        ),
        ScenarioHarness.fact(
          "receipt source is pinned",
          fixture.fmriregRevision != "unresolved" && fixture.fmriregVersion != "unresolved" && fixture.acceptedDifferences.nonEmpty,
          s"revision=${fixture.fmriregRevision} version=${fixture.fmriregVersion}"
        ),
        ScenarioHarness.fact(
          "T and F hypotheses retain structural identity",
          taskT.hypothesis.nonEmpty && taskF.hypothesis.nonEmpty &&
            taskF.numeratorDegreesOfFreedom == fixture.taskF.numeratorDf,
          s"t=${taskT.hypothesis.map(_.id.value)} f=${taskF.hypothesis.map(_.id.value)}"
        )
      ) ++
        ScenarioHarness.matrix("selected public design", selectedDesign, GaleTestMatrix.fromRows(fixture.selectedDesignRows), Tol) ++
        ScenarioHarness.matrix("selected public response", selectedResponse, GaleTestMatrix.fromRows(fixture.selectedResponseRows), Tol) ++
        ScenarioHarness.matrix("GLS coefficients vs independent whitened QR", complete.coefficients.value, GaleTestMatrix.fromRows(fixture.coefficients), Tol) ++
        ScenarioHarness.matrixMetrics("GLS coefficients vs independent whitened QR", complete.coefficients.value, GaleTestMatrix.fromRows(fixture.coefficients), OracleProfile) ++
        ScenarioHarness.matrix("GLS normalized covariance vs independent whitened QR", complete.normalizedCovariance, GaleTestMatrix.fromRows(fixture.normalizedCovariance), Tol) ++
        ScenarioHarness.matrix("GLS standard errors vs independent whitened QR", complete.standardErrors.value, GaleTestMatrix.fromRows(fixture.standardErrors), Tol) ++
        ScenarioHarness.vector("GLS residual variance vs independent whitened QR", complete.residualVariance, DVec.fromSeq(fixture.residualVariance), Tol) ++
        ScenarioHarness.vector("semantic task T estimates", taskT.estimates, DVec.fromSeq(fixture.taskT.estimates), Tol) ++
        ScenarioHarness.vector("semantic task T standard errors", taskT.standardErrors, DVec.fromSeq(fixture.taskT.standardErrors), Tol) ++
        ScenarioHarness.vector("semantic task T statistics", taskT.statistics, DVec.fromSeq(fixture.taskT.statistics), Tol) ++
        ScenarioHarness.vectorMetrics("semantic task T statistics", taskT.statistics, DVec.fromSeq(fixture.taskT.statistics), OracleProfile) ++
        ScenarioHarness.vector("semantic task F statistics", taskF.statistics, DVec.fromSeq(fixture.taskF.statistics), Tol) ++
        ScenarioHarness.matrix("censored scan canaries do not affect coefficients", complete.coefficients.value, sanitized.coefficients.value, Tol) ++
        ScenarioHarness.matrix("censored scan canaries do not affect standard errors", complete.standardErrors.value, sanitized.standardErrors.value, Tol) ++
        ScenarioHarness.matrix("complete vs chunked coefficients", chunked.coefficients.value, complete.coefficients.value, Tol) ++
        ScenarioHarness.matrix("complete vs chunked standard errors", chunked.standardErrors.value, complete.standardErrors.value, Tol) ++
        ScenarioHarness.vector("complete vs chunked residual variance", chunked.residualVariance, complete.residualVariance, Tol)

    ScenarioHarness.result(ScenarioId, observations)

  private def plan(
      responseRows: Vector[Vector[Double]],
      autocorrelation: AutocorrelationConfig
  ): FitPlan =
    modelValue(
      FmriModelBuilder.buildPlanEither(
        dataset(responseRows),
        ModelBuildSpec(
          formula = "onset ~ covariate(task)",
          baselineIntercept = Intercept.Runwise,
          strategy = FitStrategy.GeneralizedLeastSquares(autocorrelation)
        )
      )
    )

  private def dataset(responseRows: Vector[Vector[Double]]): SynchronousFmriDataset =
    val fixture = ArCensorGlsRFixture
    val samplingFrame = SamplingFrame(
      blockLens = fixture.blockLengths,
      tr = Vector.fill(fixture.blockLengths.length)(1.0)
    )
    datasetValue(
      FmriDataset.open(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-ar-censor-boundary-gls"),
          ImageDMat.fromRows(responseRows),
          NeuroSpace(Vector(responseRows.head.length, 1, 1))
        ),
        samplingFrame = samplingFrame,
        events = DatasetEvents(
          fixture.task.indices.toVector.map { timepoint =>
            Map(
              "onset" -> timepoint.toString,
              "task" -> fixture.task(timepoint).toString
            )
          }
        )
      )
    )

  private def dense(value: Either[FitError, FmriFitResult]): DenseFmriFitResult =
    fitValue(value) match
      case result: DenseFmriFitResult => result
      case other                      => fail(s"expected dense GLS result, got ${other.engine}")

  private def fitValue[A](value: Either[FitError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def modelValue[A](value: Either[ModelError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def datasetValue[A](value: Either[scalafim.dataset.DatasetError, A]): A =
    value.fold(error => fail(error.message), identity)
