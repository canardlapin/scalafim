package scalafim.fmri.fit.scenarios

import scalafim.dataset.{DatasetEventColumn, DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.{BaselineBasis, Intercept, NuisanceCheck, NuisanceIssue}
import scalafim.fmri.fit.*
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.fit.fixtures.RealisticNuisanceRFixture
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, ModelError, NuisanceRegressors, SampledRegressorRun}
import scalafim.image.{DMat as ImageDMat, SampleSpaces}

/** Public acceptance examples for mixed task timing and scan-aligned nuisance.
  *
  * S12 proves design construction and structural identity. S15 separately fits
  * an independently rendered and planted R receipt. Neither workflow encodes
  * sampled confounds as events or authors hypotheses by predictor position.
  */
class MixedBlockNuisanceScenarioSuite extends munit.FunSuite:

  test("S12 combines sustained blocks, transient events, and variable epochs") {
    val result = mixedDesignScenario()
    if !result.ciPass then fail(result.render)
  }

  test("S15 sampled nuisance preserves task inference against an R direct fit") {
    val result = realisticNuisanceScenario()
    if !result.ciPass then fail(result.render)
  }

  private def mixedDesignScenario(): ScenarioResult =
    val sampling = SamplingFrame(
      blockLens = Seq(100, 100),
      tr = Seq(1.0, 1.0),
      startTime = Seq(0.0, 0.0)
    )
    val rows = Vector.tabulate(16)(identity)
    val withinRun = rows.map(_ % 8)
    val trialOnset = withinRun.map(index => 3.25 + index * 11.5)
    val events = DatasetEvents.fromColumns(
      "trial_id" -> DatasetEventColumn.text(rows.map(index => f"mixed-trial-${index + 1}%02d")),
      "run" -> DatasetEventColumn.text(rows.map(index => s"run-${index / 8 + 1}")),
      "instruction_onset" -> DatasetEventColumn.numbers(trialOnset),
      "instruction_duration" -> DatasetEventColumn.numbers(rows.map(index => 8.0 + index % 2)),
      "cue_onset" -> DatasetEventColumn.numbers(trialOnset.map(_ + 0.75)),
      "cue_duration" -> DatasetEventColumn.numbers(Vector.fill(rows.length)(0.0)),
      "feedback_onset" -> DatasetEventColumn.numbers(trialOnset.map(_ + 5.25)),
      "feedback_duration" -> DatasetEventColumn.numbers(Vector.fill(rows.length)(0.0)),
      "epoch_onset" -> DatasetEventColumn.numbers(trialOnset.map(_ + 1.5)),
      "epoch_duration" -> DatasetEventColumn.numbers(rows.map(index => 0.8 + 0.4 * (index % 4))),
      "instruction" -> DatasetEventColumn.text(rows.map(index => if index % 2 == 0 then "maintain" else "switch")),
      "cue" -> DatasetEventColumn.text(rows.map(index => if index % 2 == 0 then "left" else "right")),
      "feedback" -> DatasetEventColumn.text(rows.map(index => if index % 3 == 0 then "error" else "correct")),
      "epoch" -> DatasetEventColumn.text(rows.map(index => if index % 2 == 0 then "easy" else "hard")),
      "salience" -> DatasetEventColumn.numbers(rows.map(index => 0.2 + 0.1 * (index % 5)))
    )
    val dataset = events.flatMap { table =>
      FmriDataset.open(
        backend = InMemoryDatasetBackend(
          DatasetId("mixed-block-transient"),
          ImageDMat.fromRows(Vector.fill(200)(Vector(0.0))),
          SampleSpaces(Vector(1, 1, 1))
        ),
        samplingFrame = sampling,
        events = table
      )
    }
    val factors = FactorLevelRegistry.of(
      "instruction" -> Seq("maintain", "switch"),
      "cue" -> Seq("left", "right", "neutral"),
      "feedback" -> Seq("correct", "error"),
      "epoch" -> Seq("easy", "hard")
    )
    val plan = for
      data <- dataset.left.map(error => ModelError.BuildFailed(error.message))
      levels <- factors.left.map(ModelError.fromDesignError)
      value <- FmriModelBuilder.buildPlanEither(
        data,
        ModelBuildSpec(
          formula =
            """instruction_onset ~
              |  hrf(instruction, onsets = instruction_onset, durations = instruction_duration, basis = spmg1, phase = instruction, parent = trial_id, id = instruction) +
              |  hrf(cue, onsets = cue_onset, durations = cue_duration, basis = spmg2, phase = cue, parent = trial_id, id = cue) +
              |  hrf(cue, center_within(salience, cue), onsets = cue_onset, durations = cue_duration, basis = spmg2, phase = cue, parent = trial_id, id = cue_salience) +
              |  hrf(feedback, onsets = feedback_onset, durations = feedback_duration, basis = spmg1, phase = feedback, parent = trial_id, id = feedback) +
              |  hrf(epoch, onsets = epoch_onset, durations = epoch_duration, basis = fir, nbasis = 4, phase = epoch, parent = trial_id, id = epoch)""".stripMargin,
          blockColumn = Some("run"),
          baselineBasis = BaselineBasis.Poly,
          baselineDegree = 2,
          baselineIntercept = Intercept.Runwise,
          precision = 0.05.s,
          factorLevels = levels,
          emptyCellPolicy = EmptyCellPolicy.Omit
        )
      )
    yield value

    plan match
      case Left(error) =>
        ScenarioHarness.result(
          "fit.mixed-block-transient.v1",
          Vector(ScenarioHarness.fact("public mixed model builds", passed = false, detail = error.message))
        )
      case Right(value) =>
        val schema = value.model.designSchema.getOrElse(fail("mixed model must expose a structural schema"))
        val eventOrigins = schema.columns.collect {
          case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => origin
        }
        val eventCounts = eventOrigins.groupMapReduce(_.term.value)(_ => 1)(_ + _)
        val provenance = schema.audit.eventProvenance
        val phases = provenance.flatMap(_.phase.map(_.value)).toSet
        val instructionDurations = provenance.filter(_.phase.exists(_.value == "instruction")).map(_.duration.value).toSet
        val cueDurations = provenance.filter(_.phase.exists(_.value == "cue")).map(_.duration.value).toSet
        val feedbackDurations = provenance.filter(_.phase.exists(_.value == "feedback")).map(_.duration.value).toSet
        val epochDurations = provenance.filter(_.phase.exists(_.value == "epoch")).map(_.duration.value).toSet
        val cueFactor = factor("cue")
        val neutral = cueFactor === "neutral"
        val neutralCell = CellKey.from(Vector(neutral)).fold(error => fail(error.message), identity)
        val baselineOrigins = schema.columns.map(_.origin).filter {
          case _: StructuralColumnOrigin.Event => false
          case _                               => true
        }
        val rankPreview = schema.audit.rankPreview

        ScenarioHarness.result(
          "fit.mixed-block-transient.v1",
          Vector(
            ScenarioHarness.fact("one public formula owns the mixed task", value.model.eventModel.termKeys == Vector("instruction", "cue", "cue_salience", "feedback", "epoch"), s"terms=${value.model.eventModel.termKeys}"),
            ScenarioHarness.fact("sustained instruction blocks remain explicit", instructionDurations == Set(8.0, 9.0), s"durations=$instructionDurations"),
            ScenarioHarness.fact("cue and feedback events are impulses", cueDurations == Set(0.0) && feedbackDurations == Set(0.0), s"cue=$cueDurations feedback=$feedbackDurations"),
            ScenarioHarness.fact("epoch durations remain trial-specific", Vector(0.8, 1.2, 1.6, 2.0).forall(expected => epochDurations.exists(actual => math.abs(actual - expected) <= 1e-12)) && epochDurations.size == 4, s"durations=$epochDurations"),
            ScenarioHarness.fact("canonical informed and FIR widths are structural", eventCounts == Map("instruction" -> 2, "cue" -> 4, "cue_salience" -> 4, "feedback" -> 2, "epoch" -> 8), s"counts=$eventCounts"),
            ScenarioHarness.fact("all five phase identities survive lowering", phases == Set("instruction", "cue", "feedback", "epoch"), s"phases=$phases"),
            ScenarioHarness.fact("all parent trials retain their source rows", provenance.map(_.parent.value).distinct.length == 16 && provenance.map(_.sourceRow).distinct.sorted == rows, s"parents=${provenance.map(_.parent.value).distinct.length} rows=${provenance.map(_.sourceRow).distinct.sorted}"),
            ScenarioHarness.fact("salience centering is owned by the model", schema.audit.centeringReceipts.exists(_.policy == CenteringPolicy.WithinFactor(cueFactor.id)), s"centering=${schema.audit.centeringReceipts.map(_.policy)}"),
            ScenarioHarness.fact("declared neutral cues are omitted with a typed receipt", schema.audit.emptyCellAudits.exists(audit => audit.cell == neutralCell && audit.policy == EmptyCellPolicy.Omit && audit.disposition == EmptyCellDisposition.Omitted), s"empty=${schema.audit.emptyCellAudits}"),
            ScenarioHarness.fact("task columns keep global coefficient scope", eventOrigins.forall(_.runScope == RunScope.Global), s"scopes=${eventOrigins.map(_.runScope).distinct}"),
            ScenarioHarness.fact("intercepts and drifts remain run scoped", baselineOrigins.forall {
              case StructuralColumnOrigin.Intercept(RunScope.Run(_))    => true
              case StructuralColumnOrigin.Drift(_, _, RunScope.Run(_)) => true
              case _                                                   => false
            }, s"baseline=$baselineOrigins"),
            ScenarioHarness.fact("HRF scaling is explicit", schema.columns.collect {
              case column @ StructuralColumn(_, _, _: StructuralColumnOrigin.Event, _, _, _) => column.hrfScale
            }.forall(_ == HrfColumnScale.identity), s"scales=${schema.columns.map(_.hrfScale).distinct}"),
            ScenarioHarness.fact("compiled rank preview is retained", rankPreview.nonEmpty, s"rankPreview=$rankPreview"),
            ScenarioHarness.fact("sampled nuisance never masquerades as an event", eventOrigins.forall(_.role == ColumnRole.Task) && baselineOrigins.forall {
              case _: StructuralColumnOrigin.Nuisance => false
              case _                                  => true
            }, "unexpected nuisance event origin")
          )
        )

  private def realisticNuisanceScenario(): ScenarioResult =
    val sampling = SamplingFrame(
      blockLens = Seq(80, 80),
      tr = Seq(1.0, 1.0),
      startTime = Seq(0.0, 0.0)
    )
    val events = DatasetEvents.fromColumns(
      "trial_id" -> DatasetEventColumn.text(RealisticNuisanceRFixture.trialIds),
      "run" -> DatasetEventColumn.text(RealisticNuisanceRFixture.runs),
      "onset" -> DatasetEventColumn.numbers(RealisticNuisanceRFixture.onsets),
      "condition" -> DatasetEventColumn.text(RealisticNuisanceRFixture.conditions)
    )
    val dataset = events.flatMap { table =>
      FmriDataset.open(
        backend = InMemoryDatasetBackend(
          DatasetId("realistic-nuisance"),
          ImageDMat.fromRows(RealisticNuisanceRFixture.response.map(value => Vector(value))),
          SampleSpaces(Vector(1, 1, 1))
        ),
        samplingFrame = sampling,
        events = table
      )
    }
    val nuisance = for
      runOne <- SampledRegressorRun.fromColumns(RealisticNuisanceRFixture.nuisanceRuns(0).columns*)
      runTwo <- SampledRegressorRun.fromColumns(RealisticNuisanceRFixture.nuisanceRuns(1).columns*)
      regressors <- NuisanceRegressors.fromRuns(
        Vector(runOne, runTwo),
        check = NuisanceCheck.Drop,
        duplicateThreshold = 0.999
      )
    yield regressors
    val factors = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C"))
    val specification = for
      regressors <- nuisance
      levels <- factors.left.map(ModelError.fromDesignError)
    yield ModelBuildSpec(
      formula = "onset ~ hrf(condition, basis = spmg1, phase = task, parent = trial_id, id = task)",
      blockColumn = Some("run"),
      baselineBasis = BaselineBasis.Poly,
      baselineDegree = 2,
      baselineIntercept = Intercept.Runwise,
      precision = 0.05.s,
      nuisance = Some(regressors),
      factorLevels = levels,
      emptyCellPolicy = EmptyCellPolicy.Omit
    )
    val planAndSpecification = for
      data <- dataset.left.map(error => ModelError.BuildFailed(error.message))
      spec <- specification
      value <- FmriModelBuilder.buildPlanEither(data, spec)
    yield value -> spec

    planAndSpecification match
      case Left(error) =>
        ScenarioHarness.result(
          "fit.realistic-nuisance.v1",
          Vector(ScenarioHarness.fact("public nuisance model builds", passed = false, detail = error.message))
        )
      case Right((value, spec)) =>
        FitPlanExecutor.fit(value) match
          case Left(error) =>
            ScenarioHarness.result(
              "fit.realistic-nuisance.v1",
              Vector(ScenarioHarness.fact("public nuisance model fits", passed = false, detail = error.message))
            )
          case Right(fitted: DenseFmriFitResult) =>
            evaluateNuisanceFit(value, fitted, spec)
          case Right(other) =>
            ScenarioHarness.result(
              "fit.realistic-nuisance.v1",
              Vector(ScenarioHarness.fact("ordinary least squares returns a dense fit", passed = false, detail = s"engine=${other.engine}"))
            )

  private def evaluateNuisanceFit(
      plan: scalafim.fmri.model.FitPlan,
      fitted: DenseFmriFitResult,
      specification: ModelBuildSpec
  ): ScenarioResult =
    val schema = plan.model.designSchema.getOrElse(fail("nuisance model must expose a structural schema"))
    val condition = factor("condition")
    val task = term("task").inPhase("task")
    val a = task.cell(condition === "A")
    val b = task.cell(condition === "B")
    val aCoefficient = a.coefficient(Hrfs.SPMG1, BasisRole.Canonical)
      .named("task-a", "condition A canonical task coefficient")
    val bCoefficient = b.coefficient(Hrfs.SPMG1, BasisRole.Canonical)
      .named("task-b", "condition B canonical task coefficient")
    val difference = (a.coefficient(Hrfs.SPMG1, BasisRole.Canonical) -
      b.coefficient(Hrfs.SPMG1, BasisRole.Canonical))
      .named("task-a-minus-b", "condition A minus B")
    val omnibus = (a.omnibus(Hrfs.SPMG1) + b.omnibus(Hrfs.SPMG1))
      .named("task-omnibus", "joint A and B task response")
    val aResult = aCoefficient.compile(schema).flatMap(_.evaluate(fitted))
    val bResult = bCoefficient.compile(schema).flatMap(_.evaluate(fitted))
    val differenceResult = difference.compile(schema).flatMap(_.evaluate(fitted))
    val omnibusResult = omnibus.compile(schema).flatMap(_.evaluate(fitted))
    val report = plan.model.baselineModel.nuisanceReport
    val expectedRetained = Vector.fill(2)(RealisticNuisanceRFixture.retainedNuisance)
    val expectedDropped = Vector.fill(2)(RealisticNuisanceRFixture.droppedNuisance)
    val nuisanceOrigins = schema.columns.collect {
      case StructuralColumn(_, _, origin: StructuralColumnOrigin.Nuisance, _, _, _) => origin
    }
    val taskOrigins = schema.columns.collect {
      case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => origin
    }
    val baselineOrigins = schema.columns.map(_.origin).collect {
      case origin: StructuralColumnOrigin.Intercept => origin
      case origin: StructuralColumnOrigin.Drift     => origin
    }
    val shortened = RealisticNuisanceRFixture.nuisanceRuns(0).columns.map { case (name, values) =>
      name -> values.dropRight(1)
    }
    val misaligned = for
      first <- SampledRegressorRun.fromColumns(shortened*)
      second <- SampledRegressorRun.fromColumns(RealisticNuisanceRFixture.nuisanceRuns(1).columns*)
      regressors <- NuisanceRegressors.fromRuns(Vector(first, second), check = NuisanceCheck.Drop)
      invalid <- FmriModelBuilder.buildPlanEither(plan.model.dataset, specification.copy(nuisance = Some(regressors)))
    yield invalid
    val tolerance = ScenarioTolerance.mixed(1e-8, 1e-6)
    val profileTolerance = ScenarioComparisonTolerance.bounded(5e-4, 2e-6, 0.999999, 2e-6)
    val actualReceiptValues = Vector(
      aResult.toOption.map(_.estimates(0)),
      bResult.toOption.map(_.estimates(0)),
      differenceResult.toOption.map(_.statistics(0)),
      omnibusResult.toOption.map(_.statistics(0))
    ).flatten
    val expectedReceiptValues = Vector(
      RealisticNuisanceRFixture.taskCoefficients(0),
      RealisticNuisanceRFixture.taskCoefficients(1),
      RealisticNuisanceRFixture.taskDifference.statistic,
      RealisticNuisanceRFixture.taskOmnibus.statistic
    )

    ScenarioHarness.result(
      "fit.realistic-nuisance.v1",
      Vector(
        ScenarioHarness.fact("sampled columns align directly to two runs", plan.model.dataset.samplingFrame.blockLens == Vector(80, 80), s"blocks=${plan.model.dataset.samplingFrame.blockLens}"),
        ScenarioHarness.fact("misaligned sampled runs fail through the typed model boundary", misaligned.left.exists {
          case ModelError.BaselineFailure(_) => true
          case _                             => false
        }, misaligned.fold(_.message, _ => "unexpectedly built")),
        ScenarioHarness.fact("constant and exact duplicate confounds are dropped", report.exists(_.droppedByBlock == expectedDropped), s"actual=${report.map(_.droppedByBlock)} expected=$expectedDropped"),
        ScenarioHarness.fact("the declared nuisance set remains semantic", report.exists(_.retainedByBlock == expectedRetained), s"actual=${report.map(_.retainedByBlock)} expected=$expectedRetained"),
        ScenarioHarness.fact("duplicate and alias diagnostics are retained", report.exists(value =>
          value.problems.exists(_.issue == NuisanceIssue.Duplicate) &&
            value.problems.exists(_.issue == NuisanceIssue.AliasedColumns) &&
            value.byBlock.forall(_.duplicatePairs.exists(_.column == "rot_z_near"))
        ), s"problems=${report.map(_.problems)}"),
        ScenarioHarness.fact("nuisance columns retain names and run scope", nuisanceOrigins.length == 40 && nuisanceOrigins.map(_.regressor.value).toSet == RealisticNuisanceRFixture.retainedNuisance.toSet && nuisanceOrigins.forall {
          case StructuralColumnOrigin.Nuisance(_, _, RunScope.Run(_)) => true
          case _                                                      => false
        }, s"origins=$nuisanceOrigins"),
        ScenarioHarness.fact("task coefficients remain global", taskOrigins.length == 2 && taskOrigins.forall(_.runScope == RunScope.Global), s"task=$taskOrigins"),
        ScenarioHarness.fact("intercepts and drifts remain run scoped", baselineOrigins.length == 6 && baselineOrigins.forall {
          case StructuralColumnOrigin.Intercept(RunScope.Run(_))    => true
          case StructuralColumnOrigin.Drift(_, _, RunScope.Run(_)) => true
          case _                                                   => false
        }, s"baseline=$baselineOrigins"),
        ScenarioHarness.fact("declared condition C is omitted with an audit", schema.audit.emptyCellAudits.exists(audit => audit.policy == EmptyCellPolicy.Omit && audit.disposition == EmptyCellDisposition.Omitted && audit.cell.canonical.contains("condition=C")), s"empty=${schema.audit.emptyCellAudits}"),
        ScenarioHarness.fact("fit rank matches the independent direct fit", fitted.rankReport.exists(report => report.fullRank && report.numericalRank == RealisticNuisanceRFixture.designRank), s"rank=${fitted.rankReport}"),
        ScenarioHarness.fact("residual degrees of freedom match the independent direct fit", fitted.residualDegreesOfFreedom.value == RealisticNuisanceRFixture.residualDf, s"actual=${fitted.residualDegreesOfFreedom.value} expected=${RealisticNuisanceRFixture.residualDf}"),
        ScenarioHarness.fact("structural fit provenance retains the compiled fingerprint", fitted.coefficientAxis.exists(_.designFingerprint == schema.fingerprint), s"axis=${fitted.coefficientAxis.map(_.designFingerprint)} schema=${schema.fingerprint}"),
        ScenarioHarness.fact("R receipt identifies its external HRF source", RealisticNuisanceRFixture.fmrihrfVersion == "0.4.0" && RealisticNuisanceRFixture.fmrihrfRevision.nonEmpty, s"fmrihrf=${RealisticNuisanceRFixture.fmrihrfVersion}@${RealisticNuisanceRFixture.fmrihrfRevision}"),
        ScenarioHarness.fact("direct-fit and design-construction evidence remain separately labelled", RealisticNuisanceRFixture.acceptedDifferences.length == 3, RealisticNuisanceRFixture.acceptedDifferences.mkString("; "))
      ) ++
        tReceipt("task-a", aResult, RealisticNuisanceRFixture.taskCoefficients(0), None, tolerance) ++
        tReceipt("task-b", bResult, RealisticNuisanceRFixture.taskCoefficients(1), None, tolerance) ++
        tReceipt("task-a-minus-b", differenceResult, RealisticNuisanceRFixture.taskDifference.estimate, Some(RealisticNuisanceRFixture.taskDifference.standardError -> RealisticNuisanceRFixture.taskDifference.statistic), tolerance) ++
        fReceipt(omnibusResult, tolerance) ++
        ScenarioHarness.comparisonMetrics(
          "R nuisance-fit receipt",
          actualReceiptValues,
          expectedReceiptValues,
          profileTolerance
        )
    )

  private def tReceipt(
      name: String,
      result: Either[FitError, TContrastResult],
      estimate: Double,
      inference: Option[(Double, Double)],
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    result match
      case Left(error) =>
        Vector(ScenarioHarness.fact(s"$name evaluates", passed = false, detail = error.message))
      case Right(actual) =>
        Vector(ScenarioHarness.scalar(s"$name estimate matches R", actual.estimates(0), estimate, tolerance)) ++
          inference.toVector.flatMap { case (standardError, statistic) =>
            Vector(
              ScenarioHarness.scalar(s"$name standard error matches R", actual.standardErrors(0), standardError, tolerance),
              ScenarioHarness.scalar(s"$name statistic matches R", actual.statistics(0), statistic, tolerance)
            )
          }

  private def fReceipt(
      result: Either[FitError, FContrastResult],
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    result match
      case Left(error) =>
        Vector(ScenarioHarness.fact("task omnibus evaluates", passed = false, detail = error.message))
      case Right(actual) =>
        Vector(
          ScenarioHarness.scalar("task omnibus statistic matches R", actual.statistics(0), RealisticNuisanceRFixture.taskOmnibus.statistic, tolerance),
          ScenarioHarness.fact("task omnibus numerator df matches R", actual.numeratorDegreesOfFreedom == RealisticNuisanceRFixture.taskOmnibus.numeratorDf, s"actual=${actual.numeratorDegreesOfFreedom} expected=${RealisticNuisanceRFixture.taskOmnibus.numeratorDf}"),
          ScenarioHarness.fact("task omnibus residual df matches R", actual.residualDegreesOfFreedom.value == RealisticNuisanceRFixture.taskOmnibus.denominatorDf, s"actual=${actual.residualDegreesOfFreedom.value} expected=${RealisticNuisanceRFixture.taskOmnibus.denominatorDf}")
        )
