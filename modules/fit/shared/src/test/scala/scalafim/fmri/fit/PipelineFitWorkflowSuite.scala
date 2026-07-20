package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.DMat
import scalafim.pipeline.*

class PipelineFitWorkflowSuite extends munit.FunSuite:
  private val datasetKind = ArtifactKind.unsafe[FmriDataset]("fmri-dataset")
  private val planKind = ArtifactKind.unsafe[FitPlan]("fit-plan")
  private val fitKind = ArtifactKind.unsafe[DenseFmriFitResult]("dense-fmri-fit")

  private object BuildOlsPlan extends PipelineStep[FmriDataset, FitPlan]:
    override val id: StepId = StepId.unsafe("build-ols-plan")
    override val outputKind: ArtifactKind[FitPlan] = planKind

    override def description: StepDescription =
      StepDescription(id, "Build an in-memory OLS fMRI model")

    override def run(input: FmriDataset, context: RunContext): Either[PipelineError, FitPlan] =
      buildPlan(input).left.map(error => PipelineError.InvalidGraph(error.message))

  private object FitSelectedVoxels extends PipelineStep[(FitPlan, DataSelection), DenseFmriFitResult]:
    override val id: StepId = StepId.unsafe("fit-selected-voxels")
    override val outputKind: ArtifactKind[DenseFmriFitResult] = fitKind

    override def description: StepDescription =
      StepDescription(id, "Fit selected voxels with dense OLS")

    override def run(input: (FitPlan, DataSelection), context: RunContext): Either[PipelineError, DenseFmriFitResult] =
      FitPlanExecutor.fit(input._1, input._2)
        .left
        .map(error => PipelineError.InvalidGraph(error.message))
        .flatMap {
          case dense: DenseFmriFitResult => Right(dense)
          case other =>
            Left(PipelineError.InvalidGraph(s"expected dense fit result, got ${other.engine}"))
        }

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = ImageDMat.fromRows(
      Vector(
        Vector(1.0, 2.0, 10.0),
        Vector(3.0, 1.0, 9.0),
        Vector(5.0, 0.0, 8.0),
        Vector(7.0, -1.0, 7.0)
      )
    )
    FmriDataset(
      backend = InMemoryDatasetBackend(DatasetId("pipeline-fit-demo"), data, NeuroSpace(Vector(3, 1, 1))),
      samplingFrame = samplingFrame
    )

  private def buildPlan(dataset: FmriDataset): Either[scalafim.fmri.model.ModelError, FitPlan] =
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = samplingFrame,
        designMatrix = Mat.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0), Vector(3.0))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = samplingFrame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )

    for
      model <- FmriModel.make(eventModel, baseline, dataset)
      plan <- FitPlan.make(model)
    yield plan

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  private def assertFitClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.columnNames, expected.columnNames)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol = 1e-10)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, tol = 1e-10)

  test("in-memory fMRI OLS workflow runs through the pipeline graph and matches direct execution") {
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0))
    val built =
      for
        b0 <- PipelineBuilder("in-memory-fmri-ols")
        ds <- b0.input("dataset", datasetKind)
        plan <- ds.builder.step("build-plan", BuildOlsPlan, ds.ref)
        fit <- plan.builder.step(
          "fit",
          FitSelectedVoxels,
          plan.expr.zip(PipelineExpr.const(selection, "selected-voxels"))
        )
        graph <- fit.builder.output("fit", fit.ref).map(_.build)
      yield (graph, ds.ref)

    val (graph, datasetRef) = built.toOption.get
    val input = dataset
    val directPlan = buildPlan(input).toOption.get
    val direct = FitPlanExecutor.unsafeFit(directPlan, selection).asInstanceOf[DenseFmriFitResult]
    val run = LocalPipelineRunner.run(graph, RunContext.empty.withInput(datasetRef, input))
    val piped = run.output("fit", fitKind).toOption.get

    assert(run.succeeded)
    assertEquals(graph.outputs.map(_.name.value), Vector("fit"))
    assertEquals(run.receipts.map(_.nodeId.value), Vector("dataset", "build-plan", "fit"))
    assertFitClose(piped, direct)
    assertEqualsDouble(piped.coefficient("task", 2).get, -1.0, 1e-10)
    assertEqualsDouble(piped.coefficient("base_constant", 0).get, 1.0, 1e-10)
  }
