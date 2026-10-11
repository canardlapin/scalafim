package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, InMemoryDatasetBackend, IndexSelection}
import scalafim.image.SampleSpaces
import scalafim.fmri.ar.TimeSegments
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArBiasCorrection, ArCensorTreatment, ArInitialization, ArOptions, ArStructure, AutocorrelationConfig, FitControls, FitPlan, FitStrategy, FmriModel, MissingDataPolicy, VoxelwiseFglsOptions}

class VoxelwiseArFitPlanSuite extends munit.FunSuite:
  private val lengths = Vector(40, 60)
  private val n = lengths.sum
  private val censor = Vector(5, 53)
  private val frame = SamplingFrame(lengths, Vector(1.0, 1.0))
  private val x = Matrix.tabulate(n, 2)((i, j) => if j == 0 then math.cos(i * .3) else math.sin(i * .11))
  private val y = Matrix.tabulate(n, 3)((i, j) => math.sin((i + j) * .7) + .3 * math.cos(1.3 * math.pow(i + j + 1, 1.1)) + .2 * x(i, 0))
  private def model(data: DMat = y): FmriModel =
    val events = EventModel(Vector.empty, frame, Mat.fromRows(Vector.tabulate(n)(i => Vector(x(i, 0), x(i, 1)))), Vector("a", "b"), Vector(0 -> 1, 1 -> 2), Map("a" -> Vector(0), "b" -> Vector(1)))
    val baseline = BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Runwise)
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("fgls-plan"), data, SampleSpaces(Vector(3, 1, 1))), frame)
    FmriModel(events, baseline, dataset)
  private val options = ArOptions(
    ArStructure.Ar(2), voxelwise = true, censoredTimepoints = censor,
    initialization = Some(ArInitialization.Stationary), biasCorrection = ArBiasCorrection.olsDesign(25).toOption.get,
    censorTreatment = ArCensorTreatment.ContinuousMissing,
    fgls = Some(VoxelwiseFglsOptions(VoxelArScope.AcrossRuns, CovarianceUncertainty.KenwardRoger))
  )
  private def plan(data: DMat = y, missing: MissingDataPolicy = MissingDataPolicy.Error): FitPlan =
    FitPlan(model(data), FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.fromLegacy(options).toOption.get, FitControls(missingData = missing)))
  private def result(value: Either[FitError, FmriFitResult]): VoxelwiseArFmriFitResult =
    value.fold(e => fail(e.message), identity) match
      case fit: VoxelwiseArFmriFitResult => fit
      case other => fail(s"expected voxelwise FGLS, got $other")

  test("FitPlan and chunked execution preserve the standalone estimates and contrast-specific KR tests"):
    val p = plan()
    val whole = result(FitPlanExecutor.fit(p))
    val chunked = result(FitPlanExecutor.fitChunked(p, FitChunkingStrategy.unsafeByVoxelCount(1)))
    val design = Matrix.tabulate(n, 4)((i, j) => if j < 2 then x(i, j) else if (j == 2 && i < 40) || (j == 3 && i >= 40) then 1.0 else 0.0)
    val direct = VoxelwiseArFgls.fit(design, y, TimeSegments.fromRunLengths(lengths), censor,
      VoxelwiseArSpec(2, VoxelArCoefficients.Estimated(VoxelArScope.AcrossRuns, 25), CensorContinuity.ContinuousMissing, CovarianceUncertainty.KenwardRoger)).toOption.get
    assertEquals(whole.voxelIndices, Vector(0, 1, 2))
    assertEquals(chunked.preparationProvenance, whole.preparationProvenance)
    for v <- 0 until 3; q <- 1 to 2 do
      val contrast = Matrix.tabulate(q, 4)((i, j) => if i == j then 1.0 else 0.0)
      val expected = direct.voxels(v).test(contrast, Vector.fill(q)(0.0)).toOption.get
      Vector(whole, chunked).foreach: fitted =>
        val actual = fitted.fit.voxels(v).test(contrast, Vector.fill(q)(0.0)).toOption.get
        assertEqualsDouble(actual.statistic, expected.statistic, 1e-9)
        assertEqualsDouble(actual.denominatorDf, expected.denominatorDf, 1e-9)
        assertEqualsDouble(actual.scale, expected.scale, 1e-9)
        assertEquals(fitted.fit.voxels(v).phiByRun, direct.voxels(v).phiByRun)
    assert(FitPlanExecutor.fitDense(p).isLeft)
    assert(whole.preparationProvenance.get.applied.exists(_.step.isInstanceOf[ResponsePreparationStep.Whitening]))

  test("policy choices alter the preparation identity"):
    val identity = ResponsePreparationIdentity.autocorrelation(options)
    assert(identity.contains("ar_options/v4"))
    val changes = Vector(
      options.copy(fgls = Some(VoxelwiseFglsOptions(VoxelArScope.PerRun, CovarianceUncertainty.KenwardRoger))),
      options.copy(fgls = Some(VoxelwiseFglsOptions(VoxelArScope.AcrossRuns, CovarianceUncertainty.Conditional))),
      options.copy(censorTreatment = ArCensorTreatment.RestartWhitening)
    )
    changes.foreach(o => assertNotEquals(ResponsePreparationIdentity.autocorrelation(o), identity))

  test("unsupported strategy and observation policies are refused before fitting"):
    val ar = AutocorrelationConfig.fromLegacy(options).toOption.get
    assert(FitPlan.make(model(), FitStrategy.RunwiseGeneralizedLeastSquares(ar)).isLeft)
    assert(FitPlan.make(model(), FitStrategy.GeneralizedLeastSquares(ar, FitControls(missingData = MissingDataPolicy.OmitRowsPerVoxel))).isLeft)

  test("time gaps are refused and exclusion of a bad voxel survives chunk merging"):
    assert(FitPlanExecutor.fit(plan(), DataSelection(time = IndexSelection.indices(0, 2, 3))).isLeft)
    val bad = Matrix.tabulate(n, 3)((i, j) => if j == 1 && i == 4 then Double.NaN else y(i, j))
    val p = plan(bad, MissingDataPolicy.ExcludeVoxel)
    val whole = result(FitPlanExecutor.fit(p))
    val chunks = result(FitPlanExecutor.fitChunked(p, FitChunkingStrategy.unsafeByVoxelCount(1)))
    assertEquals(whole.voxelIndices, Vector(0, 2))
    assertEquals(chunks.voxelIndices, whole.voxelIndices)
    assertEquals(chunks.fitExclusions, whole.fitExclusions)
    assertEquals(chunks.fitExclusions.map(_.voxelIndex), Vector(1))
