package scalafim.fmri.fit.scenarios

import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend, TimepointSelection}
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat as ImageDMat, SampleSpaces}
import gale.linalg.DMat

/** Source-confirmed P2.6 follow-up.
  *
  * Run-local baseline columns are fitted on a local structural axis and are
  * not averaged by fixed effects.  A shared cell missing from one run is
  * retained as a typed non-estimable decision and fixed-effects combination
  * rejects it rather than aligning coefficients by position.
  */
class RunLocalAxesScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-9, 1e-9)

  test("run-local nuisance axes and missing shared cells preserve structural identity") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val complete = Fixture("complete", includeSecondRunB = true)
    val missing = Fixture("missing", includeSecondRunB = false)
    val omitted = Fixture("omitted", includeSecondRunB = false, emptyCellPolicy = EmptyCellPolicy.Omit)
    val completeRunwise = runwiseResult(
      FitPlanExecutor.fit(FitPlan(complete.model, FitStrategy.RunwiseLeastSquares()), complete.selection)
    )
    val completeFixed = fixedEffectsResult(
      FitPlanExecutor.fit(FitPlan(complete.model, FitStrategy.SeparateRunsThenFixedEffects()), complete.selection)
    )
    val completeChunked = fixedEffectsResult(
      FitPlanExecutor.fitChunked(
        FitPlan(complete.model, FitStrategy.SeparateRunsThenFixedEffects()),
        complete.selection,
        FitChunkingStrategy.unsafeByVoxelCount(1)
      )
    )
    val missingRunwise = runwiseResult(
      FitPlanExecutor.fit(FitPlan(missing.model, FitStrategy.RunwiseLeastSquares()), missing.selection)
    )
    val missingFixed = FitPlanExecutor.fit(
      FitPlan(missing.model, FitStrategy.SeparateRunsThenFixedEffects()),
      missing.selection
    )
    val omittedRunwise = runwiseResult(
      FitPlanExecutor.fit(FitPlan(omitted.model, FitStrategy.RunwiseLeastSquares()), omitted.selection)
    )
    val omittedFixed = FitPlanExecutor.fit(
      FitPlan(omitted.model, FitStrategy.SeparateRunsThenFixedEffects()),
      omitted.selection
    )
    val rejectedModel = missing.modelFor(EmptyCellPolicy.Reject)
    val completeSchema = complete.model.designSchema.getOrElse(fail("complete model must expose a structural schema"))
    val missingSchema = missing.model.designSchema.getOrElse(fail("missing-cell model must expose a structural schema"))
    val omittedSchema = omitted.model.designSchema.getOrElse(fail("omitted-cell model must expose a structural schema"))
    val compiledContrast = complete.crossRunContrast.compile(completeSchema)
    val evaluated = compiledContrast.flatMap(_.evaluate(completeFixed)).toOption
    val oracleCoefficients = completeRunwise.runs.map { run =>
      independentLeastSquares(
        sourceMatrix(complete.model.designMatrix, run.timepoints, run.sourceColumns),
        sourceMatrix(complete.response, run.timepoints, (0 until complete.response.cols).toVector)
      )
    }
    val actualCoefficients = stack(completeRunwise.runs.map(_.coefficients.value))
    val expectedCoefficients = stack(oracleCoefficients)

    val completeProjections = completeRunwise.runs.flatMap(_.projection)
    val missingProjections = missingRunwise.runs.flatMap(_.projection)
    val omittedProjections = omittedRunwise.runs.flatMap(_.projection)
    val completeLocalOrigins = completeRunwise.runs.map { run =>
      run.coefficientAxis.toVector.flatMap(_.columns.map(_.origin))
    }
    val fixedTaskColumns = completeFixed.coefficientAxis.toVector.flatMap(_.columns.collect {
      case column @ StructuralColumn(_, _, _: StructuralColumnOrigin.Event, _, _, _) => column.id
    })
    val expectedTaskColumns = completeSchema.columns.collect {
      case column @ StructuralColumn(_, _, _: StructuralColumnOrigin.Event, _, _, _) => column.id
    }
    val fixedMappedColumns = completeFixed.perRunContributions.map { contribution =>
      contribution.sourceColumns.map(completeSchema.columns(_).id)
    }
    val effectiveFixedColumns = completeFixed.sufficientStatistics.effectiveSourceColumnIndices.map {
      completeSchema.columns(_).id
    }
    val missingBColumn = missingSchema.columns.find { column =>
      column.origin match
        case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) =>
          cell.get(FactorId.unsafe("cond")).exists(_.value == "B")
        case _ => false
    }.map(_.id)
    val omittedBColumn = omittedSchema.columns.find { column =>
      column.origin match
        case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) =>
          cell.get(FactorId.unsafe("cond")).exists(_.value == "B")
        case _ => false
    }.map(_.id)
    val secondRunMissingProjection = missingProjections.find(_.run == scalafim.fmri.design.RunIndex.unsafeOneBased(2))
    val secondRunMissingAxis = missingRunwise.runs.find(_.runIndex == 1).flatMap(_.coefficientAxis)
    val secondRunOmittedProjection = omittedProjections.find(_.run == scalafim.fmri.design.RunIndex.unsafeOneBased(2))

    val observations =
      Vector(
        ScenarioHarness.fact(
          "run-local projections are present",
          completeProjections.length == 2,
          s"actual=${completeProjections.length}"
        ),
        ScenarioHarness.fact(
          "run-local baseline columns are fitted locally",
          completeLocalOrigins.forall { origins =>
            origins.count {
              case _: StructuralColumnOrigin.Event => true
              case _                               => false
            } == 2 && origins.count {
              case _: StructuralColumnOrigin.Intercept => true
              case _: StructuralColumnOrigin.Drift     => true
              case _: StructuralColumnOrigin.Baseline  => true
              case _                                    => false
            } == 1
          },
          s"actual=$completeLocalOrigins"
        ),
        ScenarioHarness.fact(
          "projection classifies run-local nuisance",
          completeProjections.forall(_.runLocalSourceColumnIndices.nonEmpty) &&
            completeProjections.forall(_.decisions.exists(_.disposition == RunColumnDisposition.RunLocalNuisance)),
          s"actual=${completeProjections.map(_.decisions.map(_.disposition))}"
        ),
        ScenarioHarness.fact(
          "fixed effects retain only shared structural task columns",
          fixedTaskColumns == expectedTaskColumns && completeFixed.columnNames.length == expectedTaskColumns.length,
          s"actual=${fixedTaskColumns}/${completeFixed.columnNames} expected=$expectedTaskColumns"
        ),
        ScenarioHarness.fact(
          "fixed effects carry source-column mappings",
          fixedMappedColumns.forall(_ == expectedTaskColumns) && effectiveFixedColumns == expectedTaskColumns,
          s"actual=$fixedMappedColumns/$effectiveFixedColumns expected=$expectedTaskColumns"
        ),
        ScenarioHarness.fact(
          "cross-run hypothesis projects onto the shared result axis",
          evaluated.exists(_.hypothesis.exists(_.id == complete.crossRunContrast.id)),
          s"actual=${evaluated.map(_.hypothesis.map(_.id))}"
        ),
        ScenarioHarness.fact(
          "fixed-effects chunking preserves local-axis semantics",
          completeChunked.columnNames == completeFixed.columnNames &&
            completeChunked.perRunContributions.map(_.sourceColumns) == completeFixed.perRunContributions.map(_.sourceColumns) &&
            completeChunked.timepoints == completeFixed.timepoints,
          s"actual=${completeChunked.columnNames}/${completeChunked.perRunContributions.map(_.sourceColumns)}/${completeChunked.timepoints}"
        ),
        ScenarioHarness.fact(
          "censor gaps are retained on every run-local projection",
          completeFixed.timepoints == complete.keepTimepoints &&
            completeRunwise.runs.map(_.timepoints) == Vector(
              complete.keepTimepoints.filter(_ < complete.samplingFrame.blockLens.head),
              complete.keepTimepoints.filter(_ >= complete.samplingFrame.blockLens.head)
            ) &&
            completeProjections.forall(_.selectedRows.map(_.oneBased - 1) == complete.keepTimepoints),
          s"selected=${completeFixed.timepoints}; runs=${completeRunwise.runs.map(_.timepoints)}; projections=${completeProjections.map(_.selectedRows.map(_.oneBased - 1))}"
        ),
        ScenarioHarness.fact(
          "missing shared cell is typed non-estimable",
          missingBColumn.exists { columnId =>
            secondRunMissingProjection.exists(_.nonEstimable.exists(_.sourceColumn.id == columnId)) &&
              secondRunMissingAxis.exists(axis => !axis.columnIds.contains(columnId))
          },
          s"missing=$missingBColumn decisions=${secondRunMissingProjection.map(_.nonEstimable.map(_.sourceColumn.id))} axis=${secondRunMissingAxis.map(_.columnIds)}"
        ),
        ScenarioHarness.fact(
          "missing shared cell rejects fixed effects before combination",
          missingFixed.left.toOption.exists {
            case FitError.FixedEffectsIncompatible(detail) => detail.contains("not supported in every run")
            case _ => false
          },
          s"actual=${missingFixed.left.toOption.map(_.message)}"
        ),
        ScenarioHarness.fact(
          "Omit records a run-scoped audit and cannot align the absent cell by position",
          omittedBColumn.exists { columnId =>
            omittedSchema.audit.emptyCellAudits.exists { audit =>
              audit.run.contains(scalafim.fmri.design.RunIndex.unsafeOneBased(2)) &&
                audit.cell.get(FactorId.unsafe("cond")).exists(_.value == "B") &&
                audit.policy == EmptyCellPolicy.Omit &&
                audit.disposition == EmptyCellDisposition.Omitted
            } && secondRunOmittedProjection.exists(_.omitted.exists(_.sourceColumn.id == columnId))
          } && omittedFixed.left.toOption.exists {
            case FitError.FixedEffectsIncompatible(detail) => detail.contains("not supported in every run")
            case _ => false
          },
          s"audits=${omitted.model.designSchema.toVector.flatMap(_.audit.emptyCellAudits)}; decisions=${secondRunOmittedProjection.map(_.omitted.map(_.sourceColumn.id))}; fixed=${omittedFixed.left.toOption.map(_.message)}"
        ),
        ScenarioHarness.fact(
          "Reject fails model construction at the run-scoped policy boundary",
          rejectedModel.left.toOption.exists {
            case scalafim.fmri.model.ModelError.DesignFailure(
                  DesignError.EmptyFactorCellInRun(_, cell, run, EmptyCellPolicy.Reject)
                ) =>
              cell.get(FactorId.unsafe("cond")).exists(_.value == "B") && run.oneBased == 2
            case _ => false
          },
          s"actual=${rejectedModel.left.toOption.map(_.message)}"
        ),
        ScenarioHarness.fact(
          "runwise rank evidence binds local axes",
          completeRunwise.structuralRankReports.isRight && missingRunwise.structuralRankReports.isRight,
          s"actual=${completeRunwise.structuralRankReports.isRight}/${missingRunwise.structuralRankReports.isRight}"
        ),
        ScenarioHarness.fact(
          "fixed-effects engine and coefficient scope are explicit",
          completeFixed.engine == FitEngine.FixedEffects &&
            completeFixed.summary.coefficientScope == scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects,
          s"actual=${completeFixed.engine}/${completeFixed.summary.coefficientScope}"
        ),
        ScenarioHarness.fact(
          "independent oracle and policy receipt are explicit",
          complete.model.designSchema.exists(_.audit.policyReceipts.exists(_.detail.contains("retain-zero"))) &&
            completeFixed.policy.sharedRunPolicy == FixedEffectsSharedRunPolicy.RequireAllRuns,
          "oracle=independent normal-equation solve on each projected local matrix; " +
            "empty-cell policies=RetainZero/Omit/Reject; shared-run-policy=RequireAllRuns; selected rows contain censor gaps in both runs"
        ),
        ScenarioHarness.fact(
          "response synthesis scope is explicit",
          passed = true,
          "responses are generated from the inspected compiled design; the independent local-axis solve proves fitting and projection semantics, not end-to-end design construction"
        )
      ) ++
        ScenarioHarness.matrix(
          "runwise coefficients vs independent local-axis oracle",
          actualCoefficients,
          expectedCoefficients,
          ScenarioTolerance.mixed(1e-8, 1e-8)
        ) ++
        ScenarioHarness.matrix("fixed-effects coefficients chunk equivalence", completeFixed.coefficients.value, completeChunked.coefficients.value, Tol) ++
        Vector(
          ScenarioHarness.finite("complete runwise coefficients finite", completeRunwise.runs.flatMap(_.coefficients.value.copyData.toVector)),
          ScenarioHarness.finite("missing runwise coefficients finite", missingRunwise.runs.flatMap(_.coefficients.value.copyData.toVector))
        )

    ScenarioHarness.result("fit.run-local-axes.v1", observations)

  private def runwiseResult(result: Either[FitError, FmriFitResult]): RunwiseFmriFitResult =
    result match
      case Right(value: RunwiseFmriFitResult) => value
      case Right(value)                       => fail(s"expected runwise fit, found ${value.engine}")
      case Left(error)                        => fail(error.message)

  private def fixedEffectsResult(result: Either[FitError, FmriFitResult]): FixedEffectsFmriFitResult =
    result match
      case Right(value: FixedEffectsFmriFitResult) => value
      case Right(value)                            => fail(s"expected fixed-effects fit, found ${value.engine}")
      case Left(error)                             => fail(error.message)

  /** A deliberately independent test-only oracle for the small local axes.
    * It uses normal equations and partial-pivot Gaussian elimination rather
    * than Gale's QR implementation or the runwise production path.
    */
  private def independentLeastSquares(design: DMat, response: DMat): DMat =
    require(design.rows == response.rows, "oracle design and response rows must match")
    require(design.rows >= design.cols && design.cols > 0, "oracle local design must be overdetermined")
    val predictors = design.cols
    val voxels = response.cols
    val lhs = Array.fill(predictors * predictors)(0.0)
    val rhs = Array.fill(predictors * voxels)(0.0)
    var row = 0
    while row < design.rows do
      var left = 0
      while left < predictors do
        val leftValue = design(row, left)
        var right = 0
        while right < predictors do
          lhs(left * predictors + right) += leftValue * design(row, right)
          right += 1
        var voxel = 0
        while voxel < voxels do
          rhs(left * voxels + voxel) += leftValue * response(row, voxel)
          voxel += 1
        left += 1
      row += 1

    var pivot = 0
    while pivot < predictors do
      val pivotRow = bestPivot(lhs, predictors, pivot)
      val pivotValue = lhs(pivotRow * predictors + pivot)
      require(math.abs(pivotValue) > 1e-12, s"independent local-axis oracle is singular at pivot $pivot")
      if pivotRow != pivot then
        swapRows(lhs, predictors, pivot, pivotRow)
        swapRows(rhs, voxels, pivot, pivotRow)
      var lower = pivot + 1
      while lower < predictors do
        val factor = lhs(lower * predictors + pivot) / lhs(pivot * predictors + pivot)
        lhs(lower * predictors + pivot) = 0.0
        var column = pivot + 1
        while column < predictors do
          lhs(lower * predictors + column) -= factor * lhs(pivot * predictors + column)
          column += 1
        var voxel = 0
        while voxel < voxels do
          rhs(lower * voxels + voxel) -= factor * rhs(pivot * voxels + voxel)
          voxel += 1
        lower += 1
      pivot += 1

    val coefficients = Array.fill(predictors * voxels)(0.0)
    var voxel = 0
    while voxel < voxels do
      var current = predictors - 1
      while current >= 0 do
        var value = rhs(current * voxels + voxel)
        var column = current + 1
        while column < predictors do
          value -= lhs(current * predictors + column) * coefficients(column * voxels + voxel)
          column += 1
        coefficients(current * voxels + voxel) = value / lhs(current * predictors + current)
        current -= 1
      voxel += 1
    GaleTestMatrix.fromArray(predictors, voxels, coefficients)

  private def sourceMatrix(matrix: Mat, rows: Vector[Int], columns: Vector[Int]): DMat =
    GaleTestMatrix.fromRows(rows.map(row => columns.map(column => matrix(row, column))))

  private def sourceMatrix(matrix: DMat, rows: Vector[Int], columns: Vector[Int]): DMat =
    GaleTestMatrix.fromRows(rows.map(row => columns.map(column => matrix(row, column))))

  private def stack(matrices: Vector[DMat]): DMat =
    require(matrices.nonEmpty, "oracle stack must contain at least one matrix")
    val cols = matrices.head.cols
    require(matrices.forall(_.cols == cols), "oracle stack columns must agree")
    GaleTestMatrix.fromRows(matrices.flatMap(_.toRows))

  private def bestPivot(matrix: Array[Double], size: Int, pivot: Int): Int =
    var best = pivot
    var bestValue = math.abs(matrix(pivot * size + pivot))
    var row = pivot + 1
    while row < size do
      val value = math.abs(matrix(row * size + pivot))
      if value > bestValue then
        best = row
        bestValue = value
      row += 1
    best

  private def swapRows(matrix: Array[Double], columns: Int, left: Int, right: Int): Unit =
    var column = 0
    while column < columns do
      val leftIndex = left * columns + column
      val rightIndex = right * columns + column
      val value = matrix(leftIndex)
      matrix(leftIndex) = matrix(rightIndex)
      matrix(rightIndex) = value
      column += 1

  private final case class Fixture(
      label: String,
      includeSecondRunB: Boolean,
      emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.RetainZero
  ):
    val samplingFrame = SamplingFrame(
      blockLens = Seq(8, 8),
      tr = Seq(2.0, 0.8),
      startTime = Seq(0.0, 0.0),
      precision = 0.1
    )

    val keepTimepoints = Vector(0, 1, 3, 4, 6, 7, 8, 9, 10, 12, 13, 15)
    val selection = DataSelection(time = TimepointSelection.indices(keepTimepoints*))

    private val runTwoEvents =
      Vector(Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2")) ++
        (if includeSecondRunB then Vector(Map("onset" -> "2.4", "cond" -> "B", "run" -> "run-2")) else Vector.empty)

    private val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
        Map("onset" -> "2.0", "cond" -> "B", "run" -> "run-1")
      ) ++ runTwoEvents
    )

    private val factorLevels = FactorLevelRegistry.of("cond" -> Seq("A", "B")).toOption.get

    private val placeholder = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId(s"scenario-run-local-$label"),
        ImageDMat.fromRows(Vector.fill(samplingFrame.blockLens.sum)(Vector(0.0, 0.0))),
        SampleSpaces(Vector(2, 1, 1))
      ),
      samplingFrame = samplingFrame,
      events = events
    )

    private val spec = ModelBuildSpec(
      formula = "onset ~ hrf(cond)",
      blockColumn = Some("run"),
      baselineIntercept = Intercept.Runwise,
      factorLevels = factorLevels,
      emptyCellPolicy = emptyCellPolicy,
      precision = 0.1.s,
      strategy = FitStrategy.SeparateRunsThenFixedEffects()
    )

    private val compiled = modelFor(emptyCellPolicy)
      .fold(error => fail(error.message), identity)
    val response: DMat = GaleTestMatrix.fromRows(responseRows(compiled))
    private val responseDataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId(s"scenario-run-local-$label-response"),
        ImageDMat.fromRows(response.toRows),
        SampleSpaces(Vector(2, 1, 1))
      ),
      samplingFrame = samplingFrame,
      events = events
    ).dataset

    val model = compiled.copy(dataset = responseDataset)

    def modelFor(policy: EmptyCellPolicy): Either[scalafim.fmri.model.ModelError, scalafim.fmri.model.FmriModel] =
      FmriModelBuilder.buildModelEither(placeholder, spec.copy(emptyCellPolicy = policy))

    private val condition = factor("cond")
    private val task = term("cond")
    val crossRunContrast =
      (task.cell(condition === "A").coefficient(Hrfs.SPMG1, BasisRole.Canonical) -
        task.cell(condition === "B").coefficient(Hrfs.SPMG1, BasisRole.Canonical))
        .named(
          s"run-local-$label-A-minus-B",
          "shared A minus B after run-local nuisance adjustment"
        )

    private def responseRows(model: scalafim.fmri.model.FmriModel): Vector[Vector[Double]] =
      val beta = model.designSchema.get.columns.map { column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) if cell.get(FactorId.unsafe("cond")).exists(_.value == "A") => 1.25
          case StructuralColumnOrigin.Event(_, _, _, _, _, _, _) => -0.75
          case StructuralColumnOrigin.Intercept(_) => 0.50
          case _ => 0.0
      }
      val noise = Vector.tabulate(model.designMatrix.rows)(row => 0.002 * ((row % 5) - 2).toDouble)
      Vector.tabulate(model.designMatrix.rows) { row =>
        Vector.tabulate(2) { voxel =>
          var value = noise(row)
          var column = 0
          while column < model.designMatrix.cols do
            value += model.designMatrix(row, column) * beta(column) * (if voxel == 0 then 1.0 else -0.5)
            column += 1
          value
        }
      }
