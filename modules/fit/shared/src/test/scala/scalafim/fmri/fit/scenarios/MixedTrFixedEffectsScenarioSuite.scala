package scalafim.fmri.fit.scenarios

import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{SampleSpaces, SomeSampleSpace}
import gale.linalg.{DMat, DVec}

/** S13 extension: mixed acquisition grids remain explicit through a public
  * fixed-effects fit and agree with an independent R design/runwise/fixed-
  * effects receipt.
  */
class MixedTrFixedEffectsScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "fit.mixed-tr-fixed-effects.v1"
  private val Tol = ScenarioTolerance.mixed(1e-9, 1e-9)
  private val RTol = ScenarioTolerance.mixed(1e-7, 1e-7)
  private val RProfile = ScenarioComparisonTolerance.bounded(2e-6, 2e-7, 0.9999999, 2e-7)

  test("mixed-TR public fixed-effects fit preserves run grids and semantic axes") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  test("fixed effects omit one invalid run-voxel contribution without failing healthy voxels") {
    val fixture = MixedTrFixture()
    val runwise = runwiseResult(
      FitPlanExecutor.fit(FitPlan(fixture.model, FitStrategy.RunwiseLeastSquares()))
    )
    val invalidVoxelPosition = 1
    val invalidVoxelIndex = runwise.voxelIndices(invalidVoxelPosition)
    val affectedRun = runwise.runs(1)
    val invalidVariance = DVec.fromSeq(
      affectedRun.residualVariance.toVector.updated(invalidVoxelPosition, 0.0)
    )
    val altered = runwise.copy(
      runs = runwise.runs.updated(
        1,
        affectedRun.copy(
          residualVariance = invalidVariance,
          voxelStatuses = Some(
            affectedRun.resolvedVoxelStatuses.updated(invalidVoxelPosition, VoxelFitStatus.Estimable)
          )
        )
      )
    )

    val fixed = FixedEffects.combine(altered).fold(error => fail(error.message), identity)
    val schema = fixture.model.designSchema.getOrElse(fail("mixed-TR model must expose structural identity"))
    val contrast = fixture.crossRunContrast.compile(schema).flatMap(_.evaluate(fixed)).fold(error => fail(error.message), identity)

    assertEquals(fixed.voxelIndices, Vector(runwise.voxelIndices.head))
    assertEquals(
      fixed.fitExclusions,
      Vector(VoxelInferenceExclusion(invalidVoxelIndex, VoxelFitStatus.ZeroResidualVariance))
    )
    assertEquals(fixed.policy.voxelPolicy, FixedEffectsVoxelPolicy.RequireAllRuns)
    assert(fixed.coefficients.value.toRows.flatten.forall(_.isFinite))
    assertEquals(contrast.voxelIndices, fixed.voxelIndices)
    assertEquals(contrast.excludedVoxels, fixed.fitExclusions)
    assert(contrast.statistics.toVector.forall(_.isFinite))
    assert(
      AnalysisProvenance.fromResult(fixed).voxelStatuses.exists(
        _.contains(VoxelFitStatusRecord(invalidVoxelIndex, VoxelFitStatus.ZeroResidualVariance))
      )
    )
  }

  private def runScenario(): ScenarioResult =
    val fixture = MixedTrFixture()
    val runwisePlan = FitPlan(fixture.model, FitStrategy.RunwiseLeastSquares())
    val fixedPlan = FitPlan(fixture.model, FitStrategy.SeparateRunsThenFixedEffects())
    val runwise = runwiseResult(FitPlanExecutor.fit(runwisePlan))
    val fixed = fixedEffectsResult(FitPlanExecutor.fit(fixedPlan))
    val fixedChunked = fixedEffectsResult(
      FitPlanExecutor.fitChunked(fixedPlan, FitChunkingStrategy.unsafeByVoxelCount(1))
    )
    val schema = fixture.model.designSchema.getOrElse(fail("mixed-TR model must expose structural identity"))
    val evaluated = fixture.crossRunContrast.compile(schema).flatMap(_.evaluate(fixed))

    val eventScopes = runwise.runs.map { run =>
      run.coefficientAxis.toVector.flatMap(_.columns.flatMap { column =>
        column.origin match
          case event: StructuralColumnOrigin.Event => Some(event.runScope)
          case _                                   => None
      })
    }

    val observations =
      ScenarioHarness.values("mixed TR", fixture.samplingFrame.tr.map(_.value), Vector(2.0, 0.8), Tol) ++
      Vector(
        ScenarioHarness.fact(
          "fixed-effects engine and scope",
          fixed.engine == FitEngine.FixedEffects &&
            fixed.summary.coefficientScope == scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects,
          s"actual=${fixed.engine}/${fixed.summary.coefficientScope}"
        ),
        ScenarioHarness.fact(
          "run partitions retain mixed-grid row identity",
          runwise.runs.map(_.timepoints) == Vector(
            (0 until 8).toVector,
            (8 until 20).toVector
          ),
          s"actual=${runwise.runs.map(_.timepoints)}"
        ),
        ScenarioHarness.fact(
          "run contribution provenance",
          fixed.perRunContributions.map(c => (c.runIndex, c.rowCount, c.timepoints)) == Vector(
            (0, 8, (0 until 8).toVector),
            (1, 12, (8 until 20).toVector)
          ),
          s"actual=${fixed.perRunContributions.map(c => (c.runIndex, c.rowCount, c.timepoints))}"
        ),
        ScenarioHarness.fact(
          "runwise event axes retain structural run scope",
          eventScopes.forall(_.nonEmpty) &&
            eventScopes(0).forall(_ == RunScope.Run(scalafim.fmri.design.RunIndex.unsafeOneBased(1))) &&
            eventScopes(1).forall(_ == RunScope.Run(scalafim.fmri.design.RunIndex.unsafeOneBased(2))),
          s"actual=$eventScopes"
        ),
        ScenarioHarness.fact(
          "cross-run semantic contrast evaluates",
          evaluated.toOption.exists(_.hypothesis.exists(_.id == fixture.crossRunContrast.id)),
          evaluated.fold(_.message, result => s"actual=${result.hypothesis.map(_.id)}")
        ),
        ScenarioHarness.fact(
          "fixed-effects chunking preserves scope and provenance",
          fixedChunked.summary.coefficientScope == fixed.summary.coefficientScope &&
            fixedChunked.perRunContributions.map(_.runIndex) == fixed.perRunContributions.map(_.runIndex),
          s"actual=${fixedChunked.summary.coefficientScope}/${fixedChunked.perRunContributions.map(_.runIndex)}"
        ),
        ScenarioHarness.fact(
          "effective degrees of freedom sums run contributions",
          fixed.effectiveResidualDegreesOfFreedom.value == MixedTrFixedEffectsRFixture.fixedResidualDf,
          s"actual=${fixed.effectiveResidualDegreesOfFreedom.value} expected=${MixedTrFixedEffectsRFixture.fixedResidualDf}"
        ),
        ScenarioHarness.fact(
          "semantic contrast matches the independent R receipt",
          evaluated.toOption.exists(result =>
            close(result.estimates.toVector, MixedTrFixedEffectsRFixture.contrastEstimate, 1e-8) &&
              close(result.standardErrors.toVector, MixedTrFixedEffectsRFixture.contrastStandardError, 1e-8) &&
              close(result.statistics.toVector, MixedTrFixedEffectsRFixture.contrastStatistic, 1e-8)
          ),
          evaluated.fold(_.message, result => s"statistics=${result.statistics.toVector}")
        )
      ) ++
        ScenarioHarness.matrix(
          "public mixed-TR design matches R fmridesign",
          gale(fixture.model.designMatrix),
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.design),
          RTol
        ) ++
        ScenarioHarness.matrixMetrics(
          "public mixed-TR design matches R fmridesign",
          gale(fixture.model.designMatrix),
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.design),
          RProfile
        ) ++
        runwiseReceiptObservations(runwise) ++
        ScenarioHarness.matrix(
          "fixed-effects coefficients match the R full-precision fold",
          fixed.coefficients.value,
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.fixedCoefficients),
          RTol
        ) ++
        ScenarioHarness.matrixMetrics(
          "fixed-effects coefficients match the R full-precision fold",
          fixed.coefficients.value,
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.fixedCoefficients),
          RProfile
        ) ++
        ScenarioHarness.matrix(
          "fixed-effects standard errors match the R full-precision fold",
          fixed.standardErrors.value,
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.fixedStandardErrors),
          RTol
        ) ++
        fixed.coefficientCovariance.matrices.zip(MixedTrFixedEffectsRFixture.fixedCovarianceByVoxel).zipWithIndex.flatMap {
          case ((actual, expected), voxel) =>
            ScenarioHarness.matrix(
              s"fixed-effects covariance voxel $voxel matches R",
              actual,
              GaleTestMatrix.fromRows(expected),
              RTol
            )
        } ++
        ScenarioHarness.matrix("fixed-effects coefficients", fixed.coefficients.value, fixedChunked.coefficients.value, Tol) ++
        ScenarioHarness.matrix("fixed-effects covariance", fixed.coefficientCovariance.matrices.head, fixedChunked.coefficientCovariance.matrices.head, Tol) ++
        Vector(
          ScenarioHarness.finite("fixed-effects coefficients finite", fixed.coefficients.value.copyData.toVector),
          ScenarioHarness.finite("fixed-effects standard errors finite", fixed.standardErrors.value.copyData.toVector)
        )

    ScenarioHarness.result(ScenarioId, observations)

  private def runwiseReceiptObservations(result: RunwiseFmriFitResult): Vector[ScenarioObservation] =
    result.runs.zipWithIndex.flatMap { case (run, index) =>
      ScenarioHarness.matrix(
        s"run $index coefficients match R lm.fit",
        run.coefficients.value,
        GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.runCoefficients(index)),
        RTol
      ) ++
        ScenarioHarness.matrix(
          s"run $index normalized covariance matches R",
          run.normalizedCovariance,
          GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.runNormalizedCovariance(index)),
          RTol
        ) ++
        ScenarioHarness.vector(
          s"run $index residual variance matches R",
          run.residualVariance,
          DVec.fromSeq(MixedTrFixedEffectsRFixture.runResidualVariance(index)),
          RTol
        ) ++
        Vector(
          ScenarioHarness.fact(
            s"run $index residual df matches R",
            run.residualDegreesOfFreedom.value == MixedTrFixedEffectsRFixture.runResidualDf(index),
            s"actual=${run.residualDegreesOfFreedom.value} expected=${MixedTrFixedEffectsRFixture.runResidualDf(index)}"
          )
        )
    }

  private def runwiseResult(result: Either[FitError, FmriFitResult]): RunwiseFmriFitResult =
    result match
      case Right(value: RunwiseFmriFitResult) => value
      case Right(value)                       => fail(s"expected runwise result, found ${value.engine}")
      case Left(error)                        => fail(error.message)

  private def fixedEffectsResult(result: Either[FitError, FmriFitResult]): FixedEffectsFmriFitResult =
    result match
      case Right(value: FixedEffectsFmriFitResult) => value
      case Right(value)                            => fail(s"expected fixed-effects result, found ${value.engine}")
      case Left(error)                             => fail(error.message)

  private def gale(matrix: Mat): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(matrix.rows) { row =>
        Vector.tabulate(matrix.cols)(column => matrix(row, column))
      }
    )

  private def close(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Boolean =
    actual.length == expected.length && actual.zip(expected).forall { case (left, right) =>
      math.abs(left - right) <= tolerance
    }

  private final case class MixedTrFixture():
    val samplingFrame: SamplingFrame =
      SamplingFrame(
        blockLens = Seq(8, 12),
        tr = Seq(2.0, 0.8),
        startTime = Seq(0.0, 0.0),
        precision = 0.1
      )

    private val events =
      DatasetEvents(
        Vector(
          Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
          Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
          Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
          Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2")
        )
      )

    private val factorLevels = FactorLevelRegistry.of("cond" -> Seq("A", "B")).toOption.get

    private val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-mixed-tr-fixed-effects"),
          scalafim.fmri.fit.GaleTestMatrix.fromRows(MixedTrFixedEffectsRFixture.response),
          SampleSpaces(Vector(2, 1, 1))
        ),
        samplingFrame = samplingFrame,
        events = events
      )

    private val spec =
      ModelBuildSpec(
        formula = "onset ~ hrf(cond)",
        blockColumn = Some("run"),
        baselineIntercept = Intercept.Global,
        factorLevels = factorLevels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        precision = 0.1.s,
        strategy = FitStrategy.SeparateRunsThenFixedEffects()
      )

    val model = FmriModelBuilder.buildModel(dataset, spec)

    private val condition = factor("cond")
    private val conditionTerm = term("cond")
    val crossRunContrast =
      (conditionTerm.cell(condition === "A").coefficient(Hrfs.SPMG1, BasisRole.Canonical) -
        conditionTerm.cell(condition === "B").coefficient(Hrfs.SPMG1, BasisRole.Canonical))
        .named("mixed-tr-A-minus-B", "condition A minus B after mixed-TR fixed-effects fitting")
