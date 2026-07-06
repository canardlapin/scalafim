package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{
  ConvolvedTerm,
  EventModel,
  EventTermColumnRole
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitConfig, FitEngine, FitPlan, FmriModel, FmriModelBuilder, LssConfig, ModelBuildSpec}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.DoubleMatrix

class FitPlanExecutorSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = DMat.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 1.0),
        Vector(5.0, 0.0),
        Vector(7.0, -1.0)
      )
    )
    FmriDataset(
      backend = InMemoryDatasetBackend(DatasetId("ols-demo"), data, NeuroSpace(Vector(2, 1, 1))),
      samplingFrame = samplingFrame
    )

  private def model: FmriModel =
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
    FmriModel(eventModel, baseline, dataset)

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  test("FitPlanExecutor runs end-to-end OLS against an in-memory dataset") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(model)).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(result.columnNames, Vector("task", "base_constant"))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.timepoints, Vector(0, 1, 2, 3))
    assertEquals(result.residualDegreesOfFreedom, 2)
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, 2.0, 1e-10)
    assertEqualsDouble(result.residualVariance(0), 0.0, 1e-10)
    assertEqualsDouble(result.residualVariance(1), 0.0, 1e-10)
  }

  test("FitPlanExecutor preserves voxel selections in the result surface") {
    val result = FitPlanExecutor.unsafeFit(
      FitPlan(model),
      DataSelection(voxels = IndexSelection.indices(1))
    ).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(1))
    assertEquals(result.voxels, 1)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEquals(result.coefficient("task", 0), None)
  }

  test("FitPlanExecutor rejects unsupported engine/configuration paths") {
    val result = FitPlanExecutor.fit(FitPlan(model, engine = FitEngine.GeneralizedLeastSquares))
    assert(result.left.toOption.exists {
      case FitError.UnsupportedAutocorrelation(msg) => msg.contains("ArStructure.Ar(p)")
      case _                                       => false
    })
  }

  test("FitPlanExecutor runs LeastSquaresSeparate from a builder-created trialwise model") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val events = DatasetEvents(
      Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString))
    )
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-trialwise-demo"),
          DMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = events
      )
    val plan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        engine = FitEngine.LeastSquaresSeparate
      )
    )

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector
    val trialDesign =
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      )
    val fixedEvent =
      MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols)
    val fixedBaseline =
      MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
    val fixed =
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(fixedEvent, fixedBaseline),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    val expected = LeastSquaresSeparate.unsafeFit(
      trialDesign,
      MatrixAdapters.responseBlock(series).toOption.get,
      fixed
    )

    assertEquals(result.engine, FitEngine.LeastSquaresSeparate)
    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(aggregateCols.map(plan.model.eventModel.columnNames).forall(name => !result.trialNames.contains(name)))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.lssDiagnostics.fixedRank, expected.diagnostics.fixedRank)
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor selects LSS trial columns by metadata rather than span position") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-reordered-trialwise-demo"),
          DMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString)))
      )
    val basePlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        engine = FitEngine.LeastSquaresSeparate
      )
    )
    val plan = moveTrialwiseMeanFirst(basePlan, termKey = "trial")
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val expected = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      ),
      MatrixAdapters.responseBlock(series).toOption.get,
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(
          MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols),
          MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
        ),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    )

    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(aggregateCols.map(plan.model.eventModel.columnNames).forall(name => !result.trialNames.contains(name)))
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor selects LSS aggregate columns by metadata rather than column name") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-renamed-aggregate-demo"),
          DMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString)))
      )
    val basePlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        engine = FitEngine.LeastSquaresSeparate
      )
    )
    val plan = renameTrialwiseAggregate(basePlan, termKey = "trial", aggregateName = "trial_shared_signal")
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val expected = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      ),
      MatrixAdapters.responseBlock(series).toOption.get,
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(
          MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols),
          MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
        ),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    )

    assertEquals(aggregateCols.map(plan.model.eventModel.columnNames), Vector("trial_shared_signal"))
    assert(!plan.model.eventModel.columnNames.exists(_ == "trial_mean"))
    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(!result.trialNames.contains("trial_shared_signal"))
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor requires explicit LSS trial term when a model has multiple trialwise terms") {
    val nTime = 24
    val nTrials = 3
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-ambiguous-demo"),
          DMat.fromRows(Vector.tabulate(nTime)(i => Vector(i.toDouble))),
          NeuroSpace(Vector(1, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 5).toString)))
      )
    val ambiguousPlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(label = \"trial_a\") + trialwise(label = \"trial_b\")",
        baselineIntercept = Intercept.Global,
        engine = FitEngine.LeastSquaresSeparate
      )
    )

    val ambiguous = FitPlanExecutor.fit(ambiguousPlan)
    assertEquals(ambiguous.left.toOption, Some(FitError.AmbiguousLssTrialTerms(Vector("trial_a", "trial_b"))))

    val selectedPlan = ambiguousPlan.copy(config = FitConfig(lss = LssConfig(trialTerm = Some("trial_a"))))
    val selected = FitPlanExecutor.unsafeFit(selectedPlan).asInstanceOf[LssFmriFitResult]
    assertEquals(selected.trialNames.length, nTrials)
    assert(selected.trialNames.forall(_.startsWith("trial_a_")))
  }

  private def moveTrialwiseMeanFirst(plan: FitPlan, termKey: String): FitPlan =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val convolved = term.asInstanceOf[ConvolvedTerm]
    val roles = convolved.resolvedColumnRoles
    val aggregateLocal = roles.indexOf(EventTermColumnRole.TrialAggregate)
    require(aggregateLocal >= 0, s"term $termKey has no aggregate column")
    val localOrder = (aggregateLocal +: convolved.columnNames.indices.filterNot(_ == aggregateLocal).toVector).toVector
    val reorderedTerm =
      convolved.copy(
        data = reorderColumns(convolved.data, localOrder),
        columnNames = localOrder.map(convolved.columnNames),
        columnRoles = localOrder.map(roles)
      )

    val globalOrder = eventModel.columnNames.indices.map { col =>
      if col >= start && col < endExcl then start + localOrder(col - start) else col
    }.toVector
    val model =
      plan.model.copy(
        eventModel = eventModel.copy(
          terms = eventModel.terms.updated(termIndex, termKey -> reorderedTerm),
          designMatrix = reorderColumns(eventModel.designMatrix, globalOrder),
          columnNames = globalOrder.map(eventModel.columnNames)
        )
    )
    plan.copy(model = model)

  private def renameTrialwiseAggregate(plan: FitPlan, termKey: String, aggregateName: String): FitPlan =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val convolved = term.asInstanceOf[ConvolvedTerm]
    val roles = convolved.resolvedColumnRoles
    val aggregateLocal = roles.indexOf(EventTermColumnRole.TrialAggregate)
    require(aggregateLocal >= 0, s"term $termKey has no aggregate column")

    val renamedLocalNames = convolved.columnNames.updated(aggregateLocal, aggregateName)
    val renamedGlobalNames = eventModel.columnNames.zipWithIndex.map { case (name, col) =>
      if col >= start && col < endExcl then renamedLocalNames(col - start) else name
    }

    plan.copy(
      model = plan.model.copy(
        eventModel = eventModel.copy(
          terms = eventModel.terms.updated(termIndex, termKey -> convolved.copy(columnNames = renamedLocalNames)),
          columnNames = renamedGlobalNames
        )
      )
    )

  private def eventColumnsByRole(plan: FitPlan, termKey: String, role: EventTermColumnRole): Vector[Int] =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val roles = term.resolvedColumnRoles
    require(roles.length == endExcl - start, s"term $termKey role metadata length mismatch")
    roles.zipWithIndex.collect { case (r, localCol) if r == role => start + localCol }

  private def reorderColumns(matrix: Mat, columnOrder: IndexedSeq[Int]): Mat =
    require(columnOrder.length == matrix.cols, "column order length must match matrix cols")
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val sourceCol = columnOrder(col)
        require(sourceCol >= 0 && sourceCol < matrix.cols, s"column $sourceCol out of bounds")
        out(row * matrix.cols + col) = matrix.data(row * matrix.cols + sourceCol)
        col += 1
      row += 1
    Mat.unsafe(matrix.rows, matrix.cols, out)
