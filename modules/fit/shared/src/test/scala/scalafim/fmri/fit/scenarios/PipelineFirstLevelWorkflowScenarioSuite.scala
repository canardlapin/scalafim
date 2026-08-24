package scalafim.fmri.fit.scenarios

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.fit.{
  DenseFmriFitResult,
  DesignMatrix,
  FContrast,
  FContrastResult,
  FitError,
  FitPlanExecutor,
  MatrixAdapters,
  Ols,
  OlsFit,
  ResponseBlock,
  TContrast,
  TContrastResult
}
import scalafim.fmri.fit.GaleTestMatrix
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}
import scalafim.pipeline.*

class PipelineFirstLevelWorkflowScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "fit.pipeline-first-level-workflow.v1"
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)
  private val VoxelOrder = Vector(1, 0)

  private val datasetKind = ArtifactKind.unsafe[FmriDataset]("scenario-fmri-dataset")
  private val planKind = ArtifactKind.unsafe[FitPlan]("scenario-fit-plan")
  private val fitKind = ArtifactKind.unsafe[DenseFmriFitResult]("scenario-dense-fit")
  private val tKind = ArtifactKind.unsafe[TContrastResult]("scenario-t-contrast")
  private val fKind = ArtifactKind.unsafe[FContrastResult]("scenario-f-contrast")

  test("pipeline-backed first-level workflow returns one scenario verdict"):
    runScenario().map { result =>
      if !result.ciPass then fail(result.render)
    }

  private object BuildFitPlan extends PipelineStep[FmriDataset, FitPlan]:
    override val id: StepId = StepId.unsafe("build-fit-plan")
    override val outputKind: ArtifactKind[FitPlan] = planKind

    override def description: StepDescription =
      StepDescription(id, "Build public first-level fit plan")

    override def run(input: FmriDataset, context: RunContext): Either[PipelineError, FitPlan] =
      Right(buildPlan(input))

  private object FitSelectedVoxels extends PipelineStep[(FitPlan, DataSelection), DenseFmriFitResult]:
    override val id: StepId = StepId.unsafe("fit-selected-voxels")
    override val outputKind: ArtifactKind[DenseFmriFitResult] = fitKind

    override def description: StepDescription =
      StepDescription(id, "Fit selected voxels with dense OLS")

    override def run(input: (FitPlan, DataSelection), context: RunContext): Either[PipelineError, DenseFmriFitResult] =
      FitPlanExecutor
        .fit(input._1, input._2)
        .left
        .map(toPipelineError)
        .flatMap {
          case dense: DenseFmriFitResult => Right(dense)
          case other =>
            Left(PipelineError.InvalidGraph(s"expected dense fit result, got ${other.engine}"))
        }

  private object EvaluateTaskT extends PipelineStep[DenseFmriFitResult, TContrastResult]:
    override val id: StepId = StepId.unsafe("evaluate-task-t")
    override val outputKind: ArtifactKind[TContrastResult] = tKind

    override def description: StepDescription =
      StepDescription(id, "Evaluate task T contrast")

    override def run(input: DenseFmriFitResult, context: RunContext): Either[PipelineError, TContrastResult] =
      TContrast("task", Map("task" -> 1.0))
        .evaluate(input)
        .left
        .map(toPipelineError)

  private object EvaluateTaskF extends PipelineStep[DenseFmriFitResult, FContrastResult]:
    override val id: StepId = StepId.unsafe("evaluate-task-f")
    override val outputKind: ArtifactKind[FContrastResult] = fKind

    override def description: StepDescription =
      StepDescription(id, "Evaluate task F contrast")

    override def run(input: DenseFmriFitResult, context: RunContext): Either[PipelineError, FContrastResult] =
      FContrast("task", Vector(Map("task" -> 1.0)))
        .evaluate(input)
        .left
        .map(toPipelineError)

  private final case class ScenarioGraph(graph: PipelineGraph, datasetRef: ArtifactRef[FmriDataset])

  private def runScenario(): Future[ScenarioResult] =
    val fixture = PublicFContrastNilearnFixture
    val input = dataset(fixture)
    val selection = DataSelection(voxels = IndexSelection.indices(VoxelOrder*))
    val scenarioGraph = buildGraph(selection)
    val graph = scenarioGraph.graph
    val context =
      RunContext.empty
        .withMetadata("scenario", ScenarioId)
        .withMetadata("voxel_order", VoxelOrder.mkString(","))
        .withInput(scenarioGraph.datasetRef, input)
    val local = LocalPipelineRunner.run(graph, context)

    FuturePipelineRunner.run(graph, context, ParallelPolicy.unsafe(1)).map { future =>
      val plan = buildPlan(input)
      val design = value(MatrixAdapters.designMatrix(plan.model, fixture.task.indices.toVector)).value
      val oracle = Ols.unsafeFit(DesignMatrix.unsafe(design), ResponseBlock.unsafe(selectedResponse(fixture.responseRows)))
      val localFit = local.output("fit", fitKind).toOption
      val localT = local.output("task-t", tKind).toOption
      val localF = local.output("task-f", fKind).toOption
      val futureFit = future.output("fit", fitKind).toOption
      val futureT = future.output("task-t", tKind).toOption
      val futureF = future.output("task-f", fKind).toOption

      val observations =
        graphObservations(graph, local, future) ++
          outputObservations(oracle, localFit, localT, localF, futureFit, futureT, futureF)

      ScenarioHarness.result(ScenarioId, observations)
    }

  private def buildGraph(selection: DataSelection): ScenarioGraph =
    val built =
      for
        b0 <- PipelineBuilder(ScenarioId)
        ds <- b0.input("dataset", datasetKind)
        plan <- ds.builder.step("build-plan", BuildFitPlan, ds.ref)
        fit <- plan.builder.step(
          "fit-selected",
          FitSelectedVoxels,
          plan.expr.zip(PipelineExpr.const(selection, "selected-voxels"))
        )
        t <- fit.builder.step("task-t", EvaluateTaskT, fit.ref)
        f <- t.builder.step("task-f", EvaluateTaskF, fit.ref)
        withFit <- f.builder.output("fit", fit.ref)
        withT <- withFit.output("task-t", t.ref)
        withF <- withT.output("task-f", f.ref)
      yield ScenarioGraph(withF.build, ds.ref)

    built.fold(error => fail(error.message), identity)

  private def dataset(fixture: PublicFContrastNilearnFixture.type): FmriDataset =
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("scenario-pipeline-first-level"),
        GaleTestMatrix.fromRows(fixture.responseRows),
        SampleSpaces(Vector(2, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(fixture.task.length), tr = Seq(1.0)),
      events = DatasetEvents(
        fixture.task.indices.toVector.map { i =>
          Map("onset" -> i.toString, "task" -> fixture.task(i).toString)
        }
      )
    )

  private def buildPlan(dataset: FmriDataset): FitPlan =
    val fixture = PublicFContrastNilearnFixture
    val nuisance = Mat.fromRows(fixture.motion.map(value => Vector(value)))
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

  private def graphObservations(
      graph: PipelineGraph,
      local: PipelineRun,
      future: PipelineRun
  ): Vector[ScenarioObservation] =
    val descriptions = graph.describe.map(description => description.nodeId.value -> description).toMap
    val stages =
      graph.executionPlan.map(_.stages.map(_.nodeIds.map(_.value))).toOption.getOrElse(Vector.empty)
    val outputNames = graph.outputs.map(_.name.value)
    val expectedNodes = Vector("dataset", "build-plan", "fit-selected", "task-t", "task-f")
    val expectedStages = Vector(Vector("dataset"), Vector("build-plan"), Vector("fit-selected"), Vector("task-t", "task-f"))

    Vector(
      ScenarioHarness.fact("graph id", graph.id.value == ScenarioId, s"actual=${graph.id.value} expected=$ScenarioId"),
      ScenarioHarness.fact("pipeline output names", outputNames == Vector("fit", "task-t", "task-f"), s"actual=${outputNames.mkString(",")}"),
      ScenarioHarness.fact("pipeline node order", graph.describe.map(_.nodeId.value) == expectedNodes, s"actual=${graph.describe.map(_.nodeId.value).mkString(",")}"),
      ScenarioHarness.fact("pipeline stages", stages == expectedStages, s"actual=$stages expected=$expectedStages"),
      ScenarioHarness.fact("fit dependencies", deps(descriptions, "fit-selected") == Vector("build-plan"), s"actual=${deps(descriptions, "fit-selected").mkString(",")}"),
      ScenarioHarness.fact("task t dependencies", deps(descriptions, "task-t") == Vector("fit-selected"), s"actual=${deps(descriptions, "task-t").mkString(",")}"),
      ScenarioHarness.fact("task f dependencies", deps(descriptions, "task-f") == Vector("fit-selected"), s"actual=${deps(descriptions, "task-f").mkString(",")}"),
      ScenarioHarness.fact("local run succeeded", local.succeeded, s"status=${local.status} error=${local.error.map(_.message)}"),
      ScenarioHarness.fact("future run succeeded", future.succeeded, s"status=${future.status} error=${future.error.map(_.message)}"),
      ScenarioHarness.fact("local receipts succeeded", local.receipts.forall(_.status == PipelineStatus.Succeeded), s"statuses=${local.receipts.map(_.status).mkString(",")}"),
      ScenarioHarness.fact("future receipts match local", future.receipts == local.receipts, s"future=${future.receipts.map(_.nodeId.value).mkString(",")} local=${local.receipts.map(_.nodeId.value).mkString(",")}"),
      ScenarioHarness.fact("future outputs match local", future.outputs == local.outputs, s"future=${future.outputs.map(_.name.value).mkString(",")} local=${local.outputs.map(_.name.value).mkString(",")}"),
      ScenarioHarness.fact("local metadata sorted", local.userMetadata == Vector("scenario" -> ScenarioId, "voxel_order" -> "1,0"), s"metadata=${local.userMetadata}"),
      ScenarioHarness.fact("future metadata matches local", future.userMetadata == local.userMetadata, s"future=${future.userMetadata} local=${local.userMetadata}"),
      ScenarioHarness.fact("local trace names pipeline", local.trace.renderText.contains("pipeline fit.pipeline-first-level-workflow.v1 status=succeeded"), local.trace.renderText)
    )

  private def outputObservations(
      oracle: OlsFit,
      localFit: Option[DenseFmriFitResult],
      localT: Option[TContrastResult],
      localF: Option[FContrastResult],
      futureFit: Option[DenseFmriFitResult],
      futureT: Option[TContrastResult],
      futureF: Option[FContrastResult]
  ): Vector[ScenarioObservation] =
    (localFit, localT, localF, futureFit, futureT, futureF) match
      case (Some(lFit), Some(lT), Some(lF), Some(fFit), Some(fT), Some(fF)) =>
        val taskRow = lFit.columnNames.indexOf("task")
        val expectedT = ratio(oracle.coefficients.value.row(taskRow), oracle.standardErrors.value.row(taskRow))
        val expectedF = DVec.fromSeq(expectedT.toVector.map(value => value * value))

        Vector(
          ScenarioHarness.fact("typed outputs available", true, "fit, task-t, and task-f retrieved by artifact kind"),
          ScenarioHarness.fact("selected voxel order", lFit.voxelIndices == VoxelOrder, s"actual=${lFit.voxelIndices.mkString(",")} expected=${VoxelOrder.mkString(",")}"),
          ScenarioHarness.fact("task t voxel order", lT.voxelIndices == VoxelOrder, s"actual=${lT.voxelIndices.mkString(",")}"),
          ScenarioHarness.fact("task f voxel order", lF.voxelIndices == VoxelOrder, s"actual=${lF.voxelIndices.mkString(",")}"),
          ScenarioHarness.fact("column names", lFit.columnNames == Vector("task", "base_constant", "motion_x"), s"actual=${lFit.columnNames.mkString(",")} expected=task,base_constant,motion_x"),
          ScenarioHarness.fact("task row exists", taskRow >= 0, s"taskRow=$taskRow"),
          ScenarioHarness.fact("future fit metadata matches local", fFit.columnNames == lFit.columnNames && fFit.voxelIndices == lFit.voxelIndices, s"future=${fFit.columnNames}/${fFit.voxelIndices} local=${lFit.columnNames}/${lFit.voxelIndices}"),
          ScenarioHarness.fact("future t metadata matches local", fT.voxelIndices == lT.voxelIndices && fT.residualDegreesOfFreedom == lT.residualDegreesOfFreedom, s"future=${fT.voxelIndices} local=${lT.voxelIndices}"),
          ScenarioHarness.fact("future f metadata matches local", fF.voxelIndices == lF.voxelIndices && fF.numeratorDegreesOfFreedom == lF.numeratorDegreesOfFreedom, s"future=${fF.voxelIndices} local=${lF.voxelIndices}")
        ) ++
          ScenarioHarness.matrix("coefficients vs OLS oracle", lFit.coefficients.value, oracle.coefficients.value, Tol) ++
          ScenarioHarness.matrix("standard errors vs OLS oracle", lFit.standardErrors.value, oracle.standardErrors.value, Tol) ++
          ScenarioHarness.vector("residual variance vs OLS oracle", lFit.residualVariance, oracle.residualVariance, Tol) ++
          ScenarioHarness.vector("task t estimates vs OLS oracle", lT.estimates, oracle.coefficients.value.row(taskRow), Tol) ++
          ScenarioHarness.vector("task t standard errors vs OLS oracle", lT.standardErrors, oracle.standardErrors.value.row(taskRow), Tol) ++
          ScenarioHarness.vector("task t statistics vs OLS oracle", lT.statistics, expectedT, Tol) ++
          ScenarioHarness.matrix("task F estimates vs OLS oracle", lF.estimates, scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(oracle.coefficients.value.row(taskRow).toVector)), Tol) ++
          ScenarioHarness.vector("task F statistics equal t squared", lF.statistics, expectedF, Tol) ++
          ScenarioHarness.matrix("future coefficients match local", fFit.coefficients.value, lFit.coefficients.value, Tol) ++
          ScenarioHarness.vector("future task t statistics match local", fT.statistics, lT.statistics, Tol) ++
          ScenarioHarness.vector("future task f statistics match local", fF.statistics, lF.statistics, Tol) ++
          Vector(
            ScenarioHarness.finite("local t statistics finite", lT.statistics.toVector),
            ScenarioHarness.finite("local f statistics finite", lF.statistics.toVector)
          )
      case _ =>
        Vector(
          ScenarioHarness.fact(
            "typed outputs available",
            passed = false,
            detail = s"localFit=${localFit.isDefined} localT=${localT.isDefined} localF=${localF.isDefined} futureFit=${futureFit.isDefined} futureT=${futureT.isDefined} futureF=${futureF.isDefined}"
          )
        )

  private def selectedResponse(rows: Vector[Vector[Double]]): DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(rows.map(row => VoxelOrder.map(row)))

  private def ratio(numerator: DVec, denominator: DVec): DVec =
    DVec.fromSeq(
      numerator.toVector.zip(denominator.toVector).map { case (n, d) => n / d }
    )

  private def deps(descriptions: Map[String, NodeDescription], node: String): Vector[String] =
    descriptions.get(node).map(_.dependencies.map(_.value)).getOrElse(Vector.empty)

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)

  private def toPipelineError(error: FitError): PipelineError =
    PipelineError.InvalidGraph(error.message)
