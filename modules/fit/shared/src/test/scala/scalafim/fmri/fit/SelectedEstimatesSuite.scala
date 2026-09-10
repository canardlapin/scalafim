package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetBackend, DatasetError, DatasetEvents, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.fmri.design.{EmptyCellPolicy, FactorLevelRegistry}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture as R
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.SampleSpaces
import scalafim.fmri.fit.GaleTestMatrix

class SelectedEstimatesSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private final class Fixture(intercept: Intercept = Intercept.Global, excluded: Boolean = false):
    var reads = 0
    private val values = R.response.map(row => if excluded then Vector(0.0) ++ row else row)
    private val source = InMemoryDatasetBackend(DatasetId("compiled-fixed-effects"),
      GaleTestMatrix.fromRows(values), SampleSpaces(Vector(values.head.size, 1, 1)))
    private val backend = new DatasetBackend:
      export source.{id, shape, mask, metadata}
      def readEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads += 1
        source.readEither(selection)
    val dataset = FmriDataset.unsafe(backend,
      SamplingFrame(blockLens = Seq(8, 12), tr = Seq(2.0, 0.8), startTime = Seq(0.0, 0.0), precision = 0.1),
      DatasetEvents(Vector(
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2"))))
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = intercept,
      factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    val fitPlan = FitPlan(model, FitStrategy.SeparateRunsThenFixedEffects())
    val reader = new DatasetSeriesReader:
      val dataset: FmriDataset = Fixture.this.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)
    def request(uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None): FirstLevelEstimateRequest =
      val axis = model.designSchema.get.coefficientAxis
      checked(FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(
        StructuralTContrast.fromIds(ContrastId.unsafe("difference"), "A minus B", Map(axis.columnIds(0) -> 1.0, axis.columnIds(1) -> -1.0)))), uncertainty))

  test("unified pooling retains exact R estimates and reports required internal variance") {
    val f = new Fixture()
    val plan = checked(SelectedEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1)))
    val report = plan.description
    assertEquals(f.reads, 0)
    assertEquals(report.topology, EstimateTopology.RunwiseInverseCovarianceFixedEffects)
    assertEquals(report.inputVoxelCount, 2)
    assertEquals(report.maximumVoxelsPerRead, 1)
    assertEquals(report.diagnostics.map(_.predictors), Vector(3, 3))
    assertEquals(report.products(EstimateProduct.ResidualVariance), EstimateProductDisposition.InternalOnly)
    assertEquals(report.products(EstimateProduct.JointCovariance), EstimateProductDisposition.NotRequested)
    assert(report.computations.contains(EstimateComputation.JointRunPrecision))
    var seen = Vector.empty[Int]
    val outcome = checked(plan.foreachBlock(f.reader, {
      case SelectedEstimateBlock.Pooled(block) => block.result match
        case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
          assert(exclusions.isEmpty)
          assertEquals(result.uncertainty, FixedEffectsEstimateUncertainty.NotRequested)
          val voxel = result.voxelIndices.head
          seen :+= voxel
          assertEqualsDouble(result.estimates(0, 0), R.contrastEstimate(voxel), 1e-7)
          Right(())
        case other => fail(s"Unexpected exclusion: $other")
      case other => fail(s"Incorrect scientific route: $other")
    }))
    assertEquals(outcome, EstimateExecutionOutcome.Completed(2, 2))
    assertEquals(seen, Vector(0, 1))
    assertEquals(f.reads, 2)
  }

  test("shared retention requests describe and deliver actual native uncertainty") {
    val f = new Fixture()
    for uncertainty <- EstimateUncertaintyRequest.values do
      val plan = checked(SelectedEstimates.prepare(FitPlan(f.model), f.request(uncertainty), ChunkSize.unsafe(2)))
      val report = plan.description
      assertEquals(report.topology, EstimateTopology.SharedOrdinaryLeastSquares)
      assertEquals(report.products(EstimateProduct.JointCovariance),
        if uncertainty == EstimateUncertaintyRequest.Joint then EstimateProductDisposition.Retained else EstimateProductDisposition.NotRequested)
      assertEquals(report.computations.contains(EstimateComputation.ResidualVariance), uncertainty != EstimateUncertaintyRequest.None)
      checked(plan.foreachBlock(f.reader, {
        case SelectedEstimateBlock.Shared(block) =>
          assertEquals(block.voxelIndices, Vector(0, 1))
          (uncertainty, block.result.uncertainty) match
            case (EstimateUncertaintyRequest.None, OlsEstimateUncertainty.NotRequested) => ()
            case (EstimateUncertaintyRequest.Marginal, _: OlsEstimateUncertainty.Marginal) => ()
            case (EstimateUncertaintyRequest.Joint, _: OlsEstimateUncertainty.Joint) => ()
            case other => fail(s"Retention does not match delivered product: $other")
          Right(())
        case other => fail(s"Incorrect scientific route: $other")
      }))
    assertEquals(f.reads, 3)
  }

  test("unsupported engines, cancellation and sink failure retain typed outcomes") {
    val f = new Fixture(excluded = true)
    assert(SelectedEstimates.prepare(FitPlan(f.model, FitEngine.RunwiseLeastSquares), f.request(), ChunkSize.unsafe(1)).isLeft)
    assertEquals(f.reads, 0)
    val plan = checked(SelectedEstimates.prepare(f.fitPlan, f.request(), ChunkSize.unsafe(1)))
    assertEquals(checked(plan.foreachBlock(f.reader, _ => fail("must not deliver"), () => true)), EstimateExecutionOutcome.Cancelled(0, 0))
    assertEquals(f.reads, 0)
    val failure = FitError.InvalidFitAxis("sink", "injected failure")
    assertEquals(plan.foreachBlock(f.reader, {
      case SelectedEstimateBlock.Pooled(block) =>
        assertEquals(block.result, FixedEffectsEstimateBlockResult.Excluded(Vector(VoxelInferenceExclusion(0, VoxelFitStatus.AllZero))))
        Left(failure)
      case other => fail(s"Incorrect scientific route: $other")
    }), Left(failure))
    assertEquals(f.reads, 1)
  }
