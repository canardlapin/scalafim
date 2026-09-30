package scalafim.fmri.fit

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitConfig, FitPlan, FmriModel}
import scalafim.image.SampleSpaces

class FitProvenanceSuite extends munit.FunSuite:
  private def frame: SamplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def result(): DenseFmriFitResult =
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("provenance"), GaleTestMatrix.fromRows(Vector(
        Vector(1.0, 2.0), Vector(2.0, 1.0), Vector(3.0, 0.0), Vector(4.0, -1.0)
      )), SampleSpaces(Vector(2, 1, 1))),
      frame
    )
    val event = EventModel(Vector.empty, frame,
      Mat.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0), Vector(3.0))),
      Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
    val baseline = BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    FitPlanExecutor.unsafeFit(FitPlan(FmriModel(event, baseline, dataset))).asInstanceOf[DenseFmriFitResult]

  test("direct provenance retains preparation, structural rank, selected statuses and axis") {
    val fit = result().copy(
      voxelIndices = Vector(1, 0),
      voxelStatuses = Some(Vector(VoxelFitStatus.AllZero, VoxelFitStatus.Estimable)),
      preparationProvenance = Some(ResponsePreparationPlan.fromConfig(FitConfig()).provenance)
    )
    val provenance = AnalysisProvenance.fromResult(fit, source = "provenance-suite")

    assertEquals(provenance.source, "provenance-suite")
    assertEquals(provenance.responsePreparation, fit.preparationProvenance)
    assertEquals(provenance.coefficientAxis, fit.coefficientAxis)
    assertEquals(provenance.timepoints.toVector, Vector(0, 1, 2, 3))
    assertEquals(provenance.rankReports.length, 1)
    assertEquals(provenance.rankReports.head.numericalRank, 2)
    assertEquals(provenance.rankReports.head.predictorCount, 2)
    assertEquals(provenance.voxelStatuses,
      Some(Vector(
        VoxelFitStatusRecord(1, VoxelFitStatus.AllZero),
        VoxelFitStatusRecord(0, VoxelFitStatus.Estimable)
      )))
  }

  test("restricted direct provenance retains only inferable names and structural ids") {
    val fit = result()
    val restricted = fit.copy(
      inference = CoefficientInference.fromCovariance(
        CoefficientInferenceScope.unsafeOnly(Vector(0), "task coefficient"),
        fit.coefficientCovariance,
        fit.inference.varianceScale,
        fit.residualDegreesOfFreedom,
        CoefficientInferenceMethod.ReducedRankConditional
      ).toOption.get
    )
    val provenance = AnalysisProvenance.fromResult(restricted)

    assertEquals(provenance.coefficientInference.map(_.method), Some(CoefficientInferenceMethod.ReducedRankConditional))
    assertEquals(provenance.coefficientInference.map(_.inferableColumns), Some(Vector("task")))
    assertEquals(restricted.coefficientAxis.map(_.columnIds.distinct.length), Some(2))
    assertEquals(provenance.coefficientInference.map(_.inferableColumnIds),
      restricted.coefficientAxis.map(axis => Vector(axis.columnIds.head)))
  }
