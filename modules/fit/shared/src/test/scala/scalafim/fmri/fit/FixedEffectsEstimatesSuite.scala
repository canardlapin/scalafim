package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.design.{RunIndex as DesignRunIndex, *}
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class FixedEffectsEstimatesSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
  private val schema = checked(DesignSchema.validated(
    Mat.unsafe(8, 3, Array.tabulate(24) { index =>
      val row = index / 3
      index % 3 match
        case 0 => if row % 2 == 0 then 1.0 else 0.0
        case 1 => if row % 2 == 1 then 1.0 else 0.0
        case _ => (row % 4).toDouble
    }),
    RowLayout(Vector.tabulate(8)(i => DesignRunIndex.unsafeOneBased(i / 4 + 1)),
      Vector.tabulate(8)(i => Seconds(i % 4)), (1 to 8).toVector.map(ScanIndex.unsafeOneBased)),
    Vector(
      checked(StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Sampled(ModulatorId.unsafe("A"), ColumnRole.Task, RunScope.Global), "A")),
      checked(StructuralColumn.fromOrigin(2, StructuralColumnOrigin.Sampled(ModulatorId.unsafe("B"), ColumnRole.Task, RunScope.Global), "B")),
      checked(StructuralColumn.fromOrigin(3, StructuralColumnOrigin.Nuisance(TermId.unsafe("nuisance"), ModulatorId.unsafe("n"), RunScope.Global), "n"))
    )
  ))
  private val axis = checked(schema.coefficientAxis.select(Vector(0, 1)))
  private val precision1 = Matrix(2, 2)(4, 1, 1, 2)
  private val precision2 = Matrix(2, 2)(2, -0.5, -0.5, 3)
  private val contributions = Vector(
    FixedEffectsRunContribution(0, 4, Vector(0, 1, 2, 3), checked(ResidualDegreesOfFreedom(1)),
      checked(CoefficientMatrixStorage.scaled(precision1, Vector(1, 2))), Matrix(2, 2)(8, 12, 9, -4), Vector(0, 1)),
    FixedEffectsRunContribution(1, 4, Vector(4, 5, 6, 7), checked(ResidualDegreesOfFreedom(1)),
      checked(CoefficientMatrixStorage.scaled(precision2, Vector(1, 3))), Matrix(2, 2)(6.5, -13.5, -4.5, 46.5), Vector(0, 1))
  )
  private val statistics = FixedEffectsSufficientStatistics(axis, Vector(7, 3), contributions, sourceColumnIndices = Vector(0, 1))

  private def selection(uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None): CompiledEstimateSelection =
    val contrast = StructuralTContrast.fromIds(ContrastId.unsafe("A-minus-B"), "A minus B",
      Map(axis.columnIds(0) -> 1.0, axis.columnIds(1) -> -1.0))
    checked(checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(contrast), EstimateOutput.Coefficient(axis.columnIds(0))), uncertainty)).compile(schema))

  private def close(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEqualsDouble(actual(row, col), expected(row, col), 1e-12)

  test("selected pooling matches independent rational inverse-precision calculations") {
    val result = checked(FixedEffectsEstimates.estimate(statistics, selection()))
    // Python Fraction calculation on the two explicitly declared correlated
    // precision matrices and run coefficient vectors; no Gale oracle involved.
    close(result.estimates, Matrix(2, 2)(202.0 / 119, -2546.0 / 727, 281.0 / 119, -163.0 / 727))
    assertEquals(result.uncertainty, FixedEffectsEstimateUncertainty.NotRequested)
    assertEquals(result.voxelIndices, Vector(7, 3))
    assertEquals(result.runIndices, Vector(0, 1))
    assertEquals(result.residualDegreesOfFreedom.value, 2)
    // Pooling scalar contrasts first gives 50/37 here, a different estimand.
    assert(math.abs(result.estimates(0, 0) - 50.0 / 37) > 0.1)
  }

  test("only requested pooled covariance and standard errors are propagated") {
    val joint = checked(FixedEffectsEstimates.estimate(statistics, selection(EstimateUncertaintyRequest.Joint)))
    val marginal = checked(FixedEffectsEstimates.estimate(statistics, selection(EstimateUncertaintyRequest.Marginal)))
    close(joint.estimates, marginal.estimates)
    (joint.uncertainty, marginal.uncertainty) match
      case (FixedEffectsEstimateUncertainty.Joint(se, covariance), FixedEffectsEstimateUncertainty.Marginal(other)) =>
        close(se.value, other.value)
        close(covariance.unsafeMatrixForVoxelPosition(0), Matrix(2, 2)(48.0 / 119, 22.0 / 119, 22.0 / 119, 20.0 / 119))
        close(covariance.unsafeMatrixForVoxelPosition(1), Matrix(2, 2)(112.0 / 727, 54.0 / 727, 54.0 / 727, 52.0 / 727))
        assertEqualsDouble(se(0, 0), math.sqrt(48.0 / 119), 1e-12)
      case other => fail(s"Unexpected selected uncertainty: $other")
    val single = checked(checked(FirstLevelEstimateRequest.make(selection().request.outputs.take(1), EstimateUncertaintyRequest.Joint)).compile(schema))
    checked(FixedEffectsEstimates.estimate(statistics, single)).uncertainty match
      case FixedEffectsEstimateUncertainty.Joint(se, covariance) =>
        assertEquals(se.value.rows, 1)
        assertEquals(covariance.predictors, 1)
        assertEqualsDouble(covariance.unsafeMatrixForVoxelPosition(0)(0, 0), 48.0 / 119, 1e-12)
      case other => fail(s"Unexpected one-output uncertainty: $other")
  }

  test("pooled coefficient reorder and disjoint run order preserve selected estimates") {
    val swappedAxis = checked(axis.select(Vector(1, 0)))
    val swappedContributions = contributions.reverse.map { run =>
      run.copy(
        precisionByVoxel = run.precisionByVoxel.map(matrix => Matrix.tabulate(2, 2)((r, c) => matrix(1 - r, 1 - c))),
        precisionWeightedCoefficients = Matrix.tabulate(2, 2)((r, v) => run.precisionWeightedCoefficients(1 - r, v)),
        sourceColumnIndices = Vector(1, 0))
    }
    val changed = statistics.copy(coefficientAxis = swappedAxis, contributions = swappedContributions, sourceColumnIndices = Vector(1, 0))
    close(checked(FixedEffectsEstimates.estimate(changed, selection())).estimates,
      checked(FixedEffectsEstimates.estimate(statistics, selection())).estimates)
  }

  test("missing pooled coefficients and foreign design identity fail explicitly") {
    val nuisance = checked(checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Coefficient(schema.columns(2).id)))).compile(schema))
    assert(FixedEffectsEstimates.estimate(statistics, nuisance).isLeft)
    val changedSchema = checked(DesignSchema.validated(Mat.unsafe(8, 3, schema.matrix.data.map(_ * 2)), schema.rows, schema.columns))
    val foreign = checked(selection().request.compile(changedSchema))
    assert(FixedEffectsEstimates.estimate(statistics, foreign).isLeft)
    val singular = statistics.copy(contributions = contributions.map(run => run.copy(precisionByVoxel = Vector.fill(2)(Matrix.zeros(2, 2)))))
    assert(FixedEffectsEstimates.estimate(singular, selection()).isLeft)
  }

  test("public selected run combination preserves mixed-TR R estimates and native exclusions") {
    import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture as R
    import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
    import scalafim.image.{DMat as ImageDMat, NeuroSpace}
    import scalafim.fmri.design.baseline.Intercept
    import scalafim.fmri.hrf.*
    import scalafim.fmri.hrf.design.SamplingFrame
    import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, FitPlan, FitStrategy}

    val events = DatasetEvents(Vector(
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2")))
    val dataset = FmriDataset.unsafe(backend = InMemoryDatasetBackend(DatasetId("selected-mixed-tr"),
      ImageDMat.fromRows(R.response), NeuroSpace(Vector(2, 1, 1))),
      samplingFrame = SamplingFrame(blockLens = Seq(8, 12), tr = Seq(2.0, 0.8), startTime = Seq(0.0, 0.0), precision = 0.1), events = events)
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = Intercept.Global,
      factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    val modelSchema = model.designSchema.get
    val contrast = StructuralTContrast.fromIds(ContrastId.unsafe("condition-difference"), "A minus B",
      Map(modelSchema.columns(0).id -> 1.0, modelSchema.columns(1).id -> -1.0))
    val request = checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(contrast)), EstimateUncertaintyRequest.Marginal))
    val selected = checked(request.compile(modelSchema))
    val source = checked(FitPlanExecutor.fit(FitPlan(model, FitStrategy.RunwiseLeastSquares()))).asInstanceOf[RunwiseFmriFitResult]
    val result = checked(FixedEffectsEstimates.combine(source, selected))
    val full = checked(FixedEffects.combine(source))
    for v <- 0 until 2 do
      assertEqualsDouble(result.result.estimates(0, v), R.contrastEstimate(v), 1e-7)
    result.result.uncertainty match
      case FixedEffectsEstimateUncertainty.Marginal(se) =>
        for v <- 0 until 2 do assertEqualsDouble(se(0, v), R.contrastStandardError(v), 1e-7)
      case other => fail(s"Unexpected uncertainty: $other")
    assertEquals(result.timepoints, full.timepoints)
    assertEquals(result.summary, full.summary)
    assertEquals(result.preparationProvenance, full.preparationProvenance)
    assertEquals(result.result.pooledCoefficientAxis, full.coefficientAxis.get)
    assertEquals(result.result.residualDegreesOfFreedom.value, R.fixedResidualDf)
    val badRun = source.runs.head.copy(residualVariance = gale.linalg.DVec.fromSeq(Vector(0.0, source.runs.head.residualVariance(1))))
    val excludedSource = source.copy(runs = source.runs.updated(0, badRun),
      fitExclusions = Vector(VoxelInferenceExclusion(9, VoxelFitStatus.NonFinite)))
    val excluded = checked(FixedEffectsEstimates.combine(excludedSource, selected))
    val excludedFull = checked(FixedEffects.combine(excludedSource))
    assertEquals(excluded.result.voxelIndices, Vector(source.voxelIndices(1)))
    assertEquals(excluded.fitExclusions, excludedFull.fitExclusions)
    assertEquals(excluded.fitExclusions.map(_.voxelIndex).toSet, Set(0, 9))
    assertEqualsDouble(excluded.result.estimates(0, 0), R.contrastEstimate(1), 1e-7)
    val noVariance = source.copy(runs = source.runs.map(run => run.copy(residualVariance = gale.linalg.DVec.fromSeq(Vector(0.0, 0.0)))))
    assert(FixedEffectsEstimates.combine(noVariance, selected).left.toOption.exists(_.isInstanceOf[FitError.AllVoxelsExcluded]))
  }
