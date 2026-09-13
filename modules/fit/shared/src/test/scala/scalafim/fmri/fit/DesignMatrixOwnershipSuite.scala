package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.StructuralColumnOrigin
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FitStrategy, FmriModel, FmriModelBuilder, ModelBuildSpec, ModelError}
import scalafim.image.SampleSpaces

class DesignMatrixOwnershipSuite extends munit.FunSuite:
  private def plan(trialwise: Boolean = false): FitPlan =
    val data = Matrix.tabulate(48, 2)((row, col) => math.sin(row.toDouble / 5.0 + col) + row.toDouble / 20.0)
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("matrix-ownership"), data, SampleSpaces(Vector(2, 1, 1))),
      samplingFrame = SamplingFrame(blockLens = Seq(48), tr = Seq(1.0)),
      events = DatasetEvents(Vector.tabulate(5)(index => Map("onset" -> (2 + index * 8).toString, "condition" -> "A")))
    )
    FmriModelBuilder.buildPlan(dataset, ModelBuildSpec(
      formula = if trialwise then "onset ~ trialwise(basis = spmg1, add_sum = TRUE, label = trial)" else "onset ~ hrf(condition, basis = spmg1)",
      baselineIntercept = Intercept.Global,
      strategy = if trialwise then FitStrategy.LeastSquaresSeparate() else FitStrategy.OrdinaryLeastSquares()
    ))

  private def poison(matrix: Mat): Unit =
    val data = matrix.data
    var index = 0
    while index < data.length do
      data(index) = data(index) * 2.0 + 0.25
      index += 1

  test("a compiled hypothesis and ordinary or chunked fits share an immutable design") {
    val prepared = plan()
    val schema = prepared.model.designSchema.get
    val task = schema.columns.find(_.origin.isInstanceOf[StructuralColumnOrigin.Event]).get
    val hypothesis = StructuralTContrast.fromColumn(ContrastId.unsafe("task"), "task coefficient", task.id).compile(schema).toOption.get
    val before = FitPlanExecutor.fitDense(prepared).toOption.get
    val beforeContrast = hypothesis.evaluate(before).toOption.get

    poison(prepared.model.designMatrix)
    poison(prepared.model.designBlock.matrix)
    poison(schema.matrix)
    poison(prepared.model.eventModel.designMatrix)
    poison(prepared.model.baselineModel.designMatrix)

    val after = FitPlanExecutor.fitDense(prepared).toOption.get
    val chunked = FitPlanExecutor.fitChunked(prepared, FitChunkingStrategy.unsafeByVoxelCount(1)).toOption.get.asInstanceOf[DenseFmriFitResult]
    for result <- Vector(after, chunked) do
      assertEquals(result.coefficientAxis, before.coefficientAxis)
      val contrast = hypothesis.evaluate(result).toOption.get
      for voxel <- 0 until 2 do
        assertEqualsDouble(result.coefficients.value(0, voxel), before.coefficients.value(0, voxel), 1e-12)
        assertEqualsDouble(contrast.estimates(voxel), beforeContrast.estimates(voxel), 1e-12)
        assertEqualsDouble(contrast.statistics(voxel), beforeContrast.statistics(voxel), 1e-12)
    assert(schema.validate.isRight)
  }

  test("LSS uses the canonical design after event and baseline arrays are mutated") {
    val prepared = plan(trialwise = true)
    val before = FitPlanExecutor.fit(prepared).toOption.get.asInstanceOf[LssFmriFitResult]
    poison(prepared.model.eventModel.designMatrix)
    poison(prepared.model.baselineModel.designMatrix)
    poison(prepared.model.designMatrix)
    val after = FitPlanExecutor.fitChunked(prepared, FitChunkingStrategy.unsafeByVoxelCount(1)).toOption.get.asInstanceOf[LssFmriFitResult]
    assertEquals(after.coefficientAxis, before.coefficientAxis)
    for row <- 0 until before.coefficients.predictors; voxel <- 0 until 2 do
      assertEqualsDouble(after.coefficients.value(row, voxel), before.coefficients.value(row, voxel), 1e-10)
  }

  test("model construction rejects a matrix changed without rebuilding its schema") {
    val original = plan().model
    val changed = original.eventModel.copy(designMatrix = original.eventModel.designMatrix.updated(0, 0, 42.0))
    assert(FmriModel.make(changed, original.baselineModel, original.dataset).left.exists {
      case ModelError.DesignFailure(_) => true
      case _ => false
    })
    val changedBaseline = original.baselineModel.copy(designMatrix = original.baselineModel.designMatrix.updated(0, 0, 42.0))
    assert(FmriModel.make(original.eventModel, changedBaseline, original.dataset).left.exists {
      case ModelError.DesignFailure(_) => true
      case _ => false
    })
  }
