package scalafim.examples

import gale.linalg.DMat
import scalafim.dataset.*
import scalafim.fmri.design.StructuralColumnOrigin
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture as R
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.*
import scalafim.image.SampleSpaces
import scalafim.fmri.fit.GaleTestMatrix

/** Executes the exact guide helpers; numerical kernel admission lives in the
  * independent fit suites. Small collected blocks here are test data only.
  */
class SelectedEstimatesExampleSuite extends munit.FunSuite:
  import SelectedEstimatesExample.*
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private final class Fixture(basis: Hrf = Hrfs.SPMG1, addNuisance: Boolean = false):
    var reads = 0
    private val source = InMemoryDatasetBackend(DatasetId("selected-guide"),
      GaleTestMatrix.fromRows(R.response), SampleSpaces(Vector(2, 1, 1)))
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
    private val nuisance = if !addNuisance then None else Some(checked(NuisanceRegressors.fromRuns(
      Vector(8, 12).zipWithIndex.map { (length, run) =>
        checked(SampledRegressorRun.fromColumns("motion" -> Vector.tabulate(length)(row =>
          math.sin((row + 1) * 1.317 + run * 0.7))))
      })))
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = Intercept.Global,
      defaultHrf = basis, nuisance = nuisance, precision = 0.1.s))
    val task = model.designSchema.get.coefficientAxis.columns.filter(_.origin.isInstanceOf[StructuralColumnOrigin.Event])
    val nuisanceColumns = model.designSchema.get.coefficientAxis.columns.filter(_.origin.isInstanceOf[StructuralColumnOrigin.Nuisance])
    val pooled = FitPlan(model, FitStrategy.SeparateRunsThenFixedEffects())
    val shared = FitPlan(model, FitStrategy.OrdinaryLeastSquares())
    val reader: DatasetSeriesReader = dataset

  private def values(block: SelectedEstimateBlock): DMat = block match
    case SelectedEstimateBlock.Shared(value) => value.result.estimates
    case SelectedEstimateBlock.Pooled(value) => value.result match
      case FixedEffectsEstimateBlockResult.Selected(result, exclusions) =>
        assert(exclusions.isEmpty)
        result.estimates
      case other => fail(s"Unexpected excluded example block: $other")

  test("condition and contrast guide executes the declared pooled engine against the R reference") {
    val f = new Fixture()
    val request = checked(conditionRequest(f.task(0).id, f.task(1).id))
    var delivered = Vector.empty[SelectedEstimateBlock]
    val result = checked(estimateWithDeclaredEngine(f.pooled, f.reader, request, block => {
      delivered :+= block
      Right(())
    }))
    assertEquals(result, EstimateExecutionOutcome.Completed(1, 2))
    assertEquals(f.reads, 1)
    val actual = values(delivered.head)
    for voxel <- 0 until 2 do
      assertEqualsDouble(actual(0, voxel), R.fixedCoefficients(0)(voxel), 1e-7)
      assertEqualsDouble(actual(1, voxel), R.fixedCoefficients(1)(voxel), 1e-7)
      assertEqualsDouble(actual(2, voxel), R.contrastEstimate(voxel), 1e-7)
    val prepared = checked(prepareSelected(f.pooled, request, 1))
    assertEquals(prepared.description.products(EstimateProduct.ResidualVariance), EstimateProductDisposition.InternalOnly)
    assertEquals(prepared.description.products(EstimateProduct.StandardErrors), EstimateProductDisposition.NotRequested)
  }

  test("FIR guide preserves structural bin identities and bounded metadata-only preparation") {
    val f = new Fixture(Hrfs.fir(nBasis = 2, span = 4.s))
    val request = checked(firRequest(f.task.map(_.id)))
    val prepared = checked(prepareSelected(f.shared, request, 1))
    assertEquals(f.reads, 0)
    assertEquals(f.task.size, 4)
    assertEquals(prepared.description.outputs, f.task.map(EstimateOutputMetadata.Coefficient.apply))
    assertEquals(prepared.description.maximumVoxelsPerRead, 1)
    var voxels = Vector.empty[Int]
    val result = checked(prepared.foreachBlock(f.reader, block => {
      assertEquals(values(block).rows, 4)
      voxels ++= block.inputVoxelIndices
      Right(())
    }))
    assertEquals(result, EstimateExecutionOutcome.Completed(2, 2))
    assertEquals(voxels, Vector(0, 1))
    assertEquals(f.reads, 2)
  }

  test("optional uncertainty and nuisance helpers preserve the requested shared-model estimates") {
    val f = new Fixture(addNuisance = true)
    val base = checked(conditionRequest(f.task(0).id, f.task(1).id))
    assertEquals(f.nuisanceColumns.size, 2)
    val retained = checked(withNuisance(base, f.nuisanceColumns.map(_.id)))
    assertEquals(retained.outputs.take(3), base.outputs)
    var reference = Vector.empty[Double]
    for uncertainty <- EstimateUncertaintyRequest.values do
      val request = checked(withUncertainty(retained, uncertainty))
      val prepared = checked(prepareSelected(f.shared, request, 2))
      checked(prepared.foreachBlock(f.reader, block => {
        block match
          case SelectedEstimateBlock.Shared(delivered) =>
            (uncertainty, delivered.result.uncertainty) match
              case (EstimateUncertaintyRequest.None, OlsEstimateUncertainty.NotRequested) => ()
              case (EstimateUncertaintyRequest.Marginal, _: OlsEstimateUncertainty.Marginal) => ()
              case (EstimateUncertaintyRequest.Joint, _: OlsEstimateUncertainty.Joint) => ()
              case other => fail(s"Incorrect delivered uncertainty: $other")
          case other => fail(s"Incorrect engine: $other")
        val matrix = values(block)
        assertEquals(matrix.rows, 5)
        val flattened = Vector.tabulate(matrix.rows, matrix.cols)(matrix.apply).flatten
        if reference.isEmpty then reference = flattened
        else reference.zip(flattened).foreach((expected, actual) => assertEqualsDouble(actual, expected, 1e-10))
        Right(())
      }))
  }

  test("run-local nuisance maps are explicitly unavailable as pooled estimates") {
    val f = new Fixture(addNuisance = true)
    val base = checked(conditionRequest(f.task(0).id, f.task(1).id))
    val request = checked(withNuisance(base, f.nuisanceColumns.map(_.id)))
    val result = prepareSelected(f.pooled, request, 1)
    assert(result.left.toOption.exists(_.message.contains("absent from the fitted axis")))
    assertEquals(f.reads, 0)
  }


  test("run retention guide composes with output and uncertainty choices without changing pooling") {
    val f = new Fixture(addNuisance = true)
    val base = checked(conditionRequest(f.task(0).id, f.task(1).id))
    val ids = f.nuisanceColumns.reverse.map(_.id)
    // Constant baseline bases use the structural Drift origin.
    val baseline = f.model.designSchema.get.columns.filter(_.origin.isInstanceOf[StructuralColumnOrigin.Drift]).map(_.id)
    assertEquals(baseline.size, 1)
    val runRequest = checked(withRunCoefficients(base, ids))
    val request = checked(withNuisance(checked(withUncertainty(runRequest, EstimateUncertaintyRequest.Joint)), baseline))
    assertEquals(request.retainRunCoefficients, ids)
    assertEquals(request.uncertainty, EstimateUncertaintyRequest.Joint)
    val plain = checked(withRunCoefficients(request, Vector.empty))
    assertEquals(plain.outputs, request.outputs)
    assertEquals(plain.uncertainty, request.uncertainty)
    val plan = checked(prepareSelected(f.pooled, request, 2))
    assertEquals(f.reads, 0)
    assertEquals(plan.description.products(EstimateProduct.RunCoefficients), EstimateProductDisposition.Retained)
    assertEquals(plan.description.retainedRunCoefficients.size, 2)
    var reference = Vector.empty[Double]
    checked(estimateWithDeclaredEngine(f.pooled, f.reader, plain, block => {
      val matrix = values(block)
      reference = Vector.tabulate(matrix.rows, matrix.cols)(matrix.apply).flatten
      Right(())
    }))
    checked(plan.foreachBlock(f.reader, block => {
      val matrix = values(block)
      assertEquals(matrix.rows, 4)
      assertEquals(Vector.tabulate(matrix.rows, matrix.cols)(matrix.apply).flatten, reference)
      block match
        case SelectedEstimateBlock.Pooled(value) =>
          value.runCoefficients match
            case RunCoefficientRetentionResult.Retained(runs) =>
              assertEquals(runs.map(_.description.partition.runIndex), Vector(0, 1))
              runs.foreach { run =>
                assertEquals((run.estimates.rows, run.estimates.cols), (1, 2))
                assertEquals(run.voxelIndices, Vector(0, 1))
                assertEquals(run.uncertainty, OlsEstimateUncertainty.NotRequested)
                assert(run.description.coefficientAxis.columns.head.origin.isInstanceOf[StructuralColumnOrigin.Nuisance])
                for voxel <- 0 until 2 do assert(run.estimates(0, voxel).isFinite)
              }
            case other => fail(s"Missing requested guide run coefficients: $other")
        case other => fail(s"Incorrect guide engine: $other")
      Right(())
    }))
    assertEquals(f.reads, 2)
  }


  test("semantic response guide keeps full derivative-basis readout and propagated uncertainty") {
    val f = new Fixture(Hrfs.SPMG2)
    val cell = f.task.head.origin match
      case StructuralColumnOrigin.Event(term, phase, key, None, _, _, _) =>
        val base = StructuralHypothesisDsl.term(term)
        phase.fold(base)(base.inPhase).cell(key.assignments*)
      case other => fail(s"Unexpected guide event cell: $other")
    val request = checked(withUncertainty(checked(responseRequest(cell, Hrfs.SPMG2, ResponseFunctional.At(6.s))),
      EstimateUncertaintyRequest.Joint))
    val prepared = checked(prepareSelected(f.shared, request, 2))
    assertEquals(f.reads, 0)
    val structural = request.outputs.head match
      case EstimateOutput.Contrast(value) => value
      case other => fail(s"Expected response contrast: $other")
    val compiled = checked(structural.compile(f.model.designSchema.get))
    assertEquals(compiled.metadata.responseFunctionals.map(_.functional), Vector(ResponseFunctional.At(6.s)))
    assertEquals(compiled.metadata.responseFunctionals.map(_.units), Vector(ResponseUnits.ResponseValue))
    assertEquals(compiled.selectedColumnIds.size, 2)
    val full = checked(FitPlanExecutor.fitChunked(f.reader, f.shared)) match
      case value: DenseFmriFitResult => value
      case other => fail(s"Unexpected guide reference fit: $other")
    val expected = checked(compiled.evaluate(full))
    checked(prepared.foreachBlock(f.reader, {
      case SelectedEstimateBlock.Shared(block) =>
        assertEquals((block.result.estimates.rows, block.result.estimates.cols), (1, 2))
        block.result.uncertainty match
          case OlsEstimateUncertainty.Joint(covariance, errors, variance, _) =>
            for voxel <- 0 until 2 do
              assertEqualsDouble(block.result.estimates(0, voxel), expected.estimates(voxel), 1e-10)
              assertEqualsDouble(errors.value(0, voxel), expected.standardErrors(voxel), 1e-10)
              assertEqualsDouble(covariance(0, 0) * variance(voxel), math.pow(expected.standardErrors(voxel), 2), 1e-10)
          case other => fail(s"Missing requested response covariance: $other")
        Right(())
      case other => fail(s"Unexpected guide execution: $other")
    }))
  }
