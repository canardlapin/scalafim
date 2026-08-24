package scalafim.fmri.fit.scenarios

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.design.event.{ConvolvedTerm, EventTermColumnRole}
import scalafim.fmri.fit.{
  FitError,
  FitPlanExecutor,
  LeastSquaresSeparate,
  LssFixedDesign,
  LssFit,
  LssFmriFitResult,
  LssTrialDesign,
  MatrixAdapters
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import scalafim.image.DMat as ImageDMat

class LssTrialwiseRecoveryScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)

  test("public LSS path recovers metadata-selected trialwise estimates") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = lssFixture
    val scaffold = moveTrialwiseMeanFirst(buildPlan(fixture.placeholderDataset, fixture), termKey = "trial")
    val generatedRows = responseRows(scaffold, fixture)
    val plan = moveTrialwiseMeanFirst(buildPlan(fixture.dataset(generatedRows), fixture), termKey = "trial")
    val publicResult = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val oracle = directOracle(plan)
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val aggregateNames = aggregateCols.map(plan.model.eventModel.columnNames)
    val trialNames = trialCols.map(plan.model.eventModel.columnNames)

    val observations =
      Vector(
        ScenarioHarness.fact(
          "engine",
          publicResult.engine == FitEngine.LeastSquaresSeparate,
          s"actual=${publicResult.engine} expected=${FitEngine.LeastSquaresSeparate}"
        ),
        ScenarioHarness.fact(
          "aggregate moved before trials",
          aggregateCols.headOption.exists(_ < trialCols.min),
          s"aggregateCols=${aggregateCols.mkString(",")} trialCols=${trialCols.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "aggregate names excluded",
          aggregateNames.forall(name => !publicResult.trialNames.contains(name)),
          s"aggregate=${aggregateNames.mkString(",")} trialNames=${publicResult.trialNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "trial names",
          publicResult.trialNames == trialNames,
          s"actual=${publicResult.trialNames.mkString(",")} expected=${trialNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "voxel indices",
          publicResult.voxelIndices == Vector(0, 1),
          s"actual=${publicResult.voxelIndices.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "fixed rank",
          publicResult.lssDiagnostics.fixedRank == oracle.diagnostics.fixedRank,
          s"actual=${publicResult.lssDiagnostics.fixedRank} expected=${oracle.diagnostics.fixedRank}"
        ),
        ScenarioHarness.fact(
          "zero trial regressors",
          publicResult.lssDiagnostics.zeroTrialRegressors == Vector.empty,
          s"actual=${publicResult.lssDiagnostics.zeroTrialRegressors.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "degenerate other regressors",
          publicResult.lssDiagnostics.degenerateOtherRegressors == Vector.empty,
          s"actual=${publicResult.lssDiagnostics.degenerateOtherRegressors.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "non-estimable trials",
          publicResult.lssDiagnostics.nonEstimableTrials == Vector.empty,
          s"actual=${publicResult.lssDiagnostics.nonEstimableTrials.mkString(",")}"
        ),
        ScenarioHarness.finite("public LSS coefficients finite", publicResult.coefficients.value.copyData.toVector)
      ) ++
        ScenarioHarness.matrix("public LSS coefficients", publicResult.coefficients.value, oracle.coefficients.value, Tol) ++
        namedCoefficientChecks(publicResult, oracle)

    ScenarioHarness.result("fit.lss-trialwise-recovery.v1", observations)

  private def buildPlan(dataset: FmriDataset, fixture: LssFixture): FitPlan =
    FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate(),
        nuisance = Some(
          NuisanceRegressors(
            matrices = Vector(Mat.fromRows(fixture.motion.map(value => Vector(value)))),
            names = Some(Vector(Vector("motion_x"))),
            check = NuisanceCheck.Drop
          )
        )
      )
    )

  private def responseRows(plan: FitPlan, fixture: LssFixture): Vector[Vector[Double]] =
    val timepoints = plan.model.dataset.series().timepoints
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val trialDesign = MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, timepoints, trialCols)
    val fixed = MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, timepoints)
    val fixedBetas = fixedBetaRows(fixed.cols)

    Vector.tabulate(trialDesign.rows) { row =>
      Vector.tabulate(fixture.voxels) { voxel =>
        var value = 0.0
        var trial = 0
        while trial < trialDesign.cols do
          value += trialDesign(row, trial) * fixture.trialBetas(trial)(voxel)
          trial += 1
        var fixedCol = 0
        while fixedCol < fixed.cols do
          value += fixed(row, fixedCol) * fixedBetas(fixedCol)(voxel)
          fixedCol += 1
        value
      }
    }

  private def fixedBetaRows(cols: Int): Vector[Vector[Double]] =
    Vector.tabulate(cols) { col =>
      col match
        case 0 => Vector(0.75, -1.2)
        case 1 => Vector(0.35, 0.55)
        case _ => Vector(0.1 / col.toDouble, -0.05 / col.toDouble)
    }

  private def directOracle(plan: FitPlan): LssFit =
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val excluded = (trialCols ++ aggregateCols).toSet
    val fixedEventCols = plan.model.eventModel.columnNames.indices.filterNot(excluded.contains).toVector
    val trialDesign =
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      )
    val fixed =
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(
          MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols),
          MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
        ),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    val response = value(MatrixAdapters.responseBlock(series))

    LeastSquaresSeparate.unsafeFit(trialDesign, response, fixed)

  private def namedCoefficientChecks(result: LssFmriFitResult, oracle: LssFit): Vector[ScenarioObservation] =
    result.trialNames.zipWithIndex.flatMap { case (name, row) =>
      result.voxelIndices.zipWithIndex.map { case (voxel, col) =>
        ScenarioHarness.scalar(
          s"trial coefficient $name voxel $voxel",
          result.coefficient(name, voxel).get,
          oracle.coefficients.value(row, col),
          Tol
        )
      }
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

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)

  private final case class LssFixture(
      nTime: Int,
      onsets: Vector[Int],
      motion: Vector[Double],
      trialBetas: Vector[Vector[Double]]
  ):
    def voxels: Int =
      trialBetas.head.length

    def placeholderDataset: FmriDataset =
      dataset(Vector.fill(nTime)(Vector.fill(voxels)(0.0)))

    def dataset(rows: Vector[Vector[Double]]): FmriDataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-lss-trialwise-recovery"),
          ImageDMat.fromRows(rows),
          SampleSpaces(Vector(voxels, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(
          onsets.map(onset => Map("onset" -> onset.toString))
        )
      )

  private def lssFixture: LssFixture =
    val nTime = 72
    val onsets = Vector(4, 13, 22, 31, 40, 49)
    val motion =
      Vector.tabulate(nTime) { i =>
        val centered = (i.toDouble - (nTime.toDouble - 1.0) / 2.0) / nTime.toDouble
        centered + 0.1 * math.sin(i.toDouble / 4.0)
      }
    val trialBetas =
      Vector(
        Vector(1.25, -0.5),
        Vector(0.8, 0.15),
        Vector(1.65, -1.1),
        Vector(-0.35, 0.9),
        Vector(2.1, -0.25),
        Vector(0.45, 1.35)
      )

    LssFixture(nTime, onsets, motion, trialBetas)
