package scalafim.fmri.fit.scenarios

import scalafim.dataset.{DatasetEventColumn, DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.fit.fixtures.DmsRFixture
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, FmriModelBuilder, ModelBuildSpec, ModelError}
import scalafim.image.{DMat as ImageDMat, SampleSpaces}

/** The delayed-match-to-sample design as an ordinary ScalaFIM consumer program.
  *
  * The source table is declared once.  One model value owns phase timing, HRFs,
  * centering, missingness, and empty-cell policy; hypotheses refer only to
  * semantic terms, cells, modulators, basis roles, and response functionals.
  */
class DelayedMatchToSampleDslScenarioSuite extends munit.FunSuite:

  test("one public model and semantic hypotheses express delayed match to sample") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    // docs:first-level-model:start
    val model =
      FactorLevelRegistry
        .of(
          "stimulus" -> Seq("face", "scene"),
          "load" -> Seq("low", "medium", "high"),
          "match" -> Seq("match", "mismatch")
        )
        .left
        .map(ModelError.fromDesignError)
        .map { factorLevels =>
          ModelBuildSpec(
            formula =
              """sample_onset ~
                |  hrf(stimulus, onsets = sample_onset, durations = sample_duration, basis = spmg1, phase = sample, parent = trial_id, id = sample) +
                |  hrf(load, onsets = delay_onset, durations = delay_duration, basis = fir, nbasis = 8, phase = delay, parent = trial_id, id = delay) +
                |  hrf(match, load, onsets = probe_onset, durations = probe_duration, basis = spmg2, phase = probe, parent = trial_id, id = probe) +
                |  hrf(match, load, center_within(rt, match, load), onsets = probe_onset, durations = probe_duration, basis = spmg2, phase = probe, parent = trial_id, id = probe_rt)""".stripMargin,
            blockColumn = Some("run"),
            baselineIntercept = Intercept.Runwise,
            precision = 0.05.s,
            factorLevels = factorLevels,
            emptyCellPolicy = EmptyCellPolicy.RetainZero,
            missingValuePolicy = MissingValuePolicy.ZeroContribution,
            strategy = FitStrategy.SeparateRunsThenFixedEffects()
          )
        }

    val planResult = model.flatMap(FmriModelBuilder.buildPlanEither(dataset, _))
    // docs:first-level-model:end

    planResult match
      case Left(error) =>
        ScenarioHarness.result(
          "fit.dms-multiphase-dsl.v1",
          Vector(ScenarioHarness.fact("public model builds", passed = false, detail = error.message))
        )
      case Right(plan) =>
        plan.model.designSchema match
          case None =>
            ScenarioHarness.result(
              "fit.dms-multiphase-dsl.v1",
              Vector(ScenarioHarness.fact("public model exposes a structural schema", passed = false, detail = "schema was absent"))
            )
          case Some(schema) =>
            FitPlanExecutor.fit(plan) match
              case Left(error) =>
                ScenarioHarness.result(
                  "fit.dms-multiphase-dsl.v1",
                  Vector(ScenarioHarness.fact("public fixed-effects model fits", passed = false, detail = error.message))
                )
              case Right(fixed: FixedEffectsFmriFitResult) =>
                evaluateScenario(plan, schema, fixed)
              case Right(other) =>
                ScenarioHarness.result(
                  "fit.dms-multiphase-dsl.v1",
                  Vector(
                    ScenarioHarness.fact(
                      "public fixed-effects model fits",
                      passed = false,
                      detail = s"expected fixed-effects result, found ${other.engine}"
                    )
                  )
                )

  private def evaluateScenario(
      plan: FitPlan,
      schema: DesignSchema,
      fixed: FixedEffectsFmriFitResult
  ): ScenarioResult =
    // docs:first-level-hypotheses:start
    val stimulus = factor("stimulus")
    val load = factor("load")
    val matchStatus = factor("match")
    val sample = term("sample").inPhase("sample")
    val delay = term("delay").inPhase("delay")
    val probe = term("probe").inPhase("probe")
    val probeRt = term("probe_rt").inPhase("probe")

    val face = sample.cell(stimulus === "face")
    val scene = sample.cell(stimulus === "scene")
    val delayLow = delay.cell(load === "low")
    val delayHigh = delay.cell(load === "high")
    val matchLow = probe.cell(matchStatus === "match", load === "low")
    val matchMedium = probe.cell(matchStatus === "match", load === "medium")
    val mismatchLow = probe.cell(matchStatus === "mismatch", load === "low")
    val mismatchMedium = probe.cell(matchStatus === "mismatch", load === "medium")
    val mismatchHigh = probe.cell(matchStatus === "mismatch", load === "high")
    val rtMatchMedium = probeRt.cell(matchStatus === "match", load === "medium").modulatedBy("rt")
    val rtMismatchMedium = probeRt.cell(matchStatus === "mismatch", load === "medium").modulatedBy("rt")

    val sampleWindow =
      (face.response(Hrfs.SPMG1, ResponseFunctional.WindowMean(4.s, 8.s)) -
        scene.response(Hrfs.SPMG1, ResponseFunctional.WindowMean(4.s, 8.s)))
        .named("sample-face-minus-scene", "sample response averaged from four to eight seconds")
    val delayWindow =
      (delayHigh.response(Hrfs.fir(nBasis = 8), ResponseFunctional.WindowMean(3.s, 9.s)) -
        delayLow.response(Hrfs.fir(nBasis = 8), ResponseFunctional.WindowMean(3.s, 9.s)))
        .named("delay-high-minus-low", "high minus low load over the delay response window")
    val probeAtSix =
      (mismatchMedium.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)) -
        matchMedium.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)))
        .named("probe-mismatch-at-six", "mismatch minus match at six seconds")
    val matchByLoad =
      ((mismatchMedium.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)) -
        matchMedium.response(Hrfs.SPMG2, ResponseFunctional.At(6.s))) -
        (mismatchLow.response(Hrfs.SPMG2, ResponseFunctional.At(6.s)) -
          matchLow.response(Hrfs.SPMG2, ResponseFunctional.At(6.s))))
        .named("probe-match-by-load", "change in the mismatch effect from low to medium load")
    val rtSlope =
      (rtMismatchMedium.coefficient(Hrfs.SPMG2, BasisRole.Canonical) -
        rtMatchMedium.coefficient(Hrfs.SPMG2, BasisRole.Canonical))
        .named("probe-rt-slope", "mismatch minus match RT slope at medium load")
    val probeShape =
      mismatchMedium
        .omnibus(Hrfs.SPMG2, BasisScope.NonCanonical)
        .named("probe-shape", "non-canonical probe response shape")
    val delayOmnibus =
      delayHigh
        .omnibus(Hrfs.fir(nBasis = 8))
        .named("delay-high-omnibus", "all high-load delay FIR bins")
    val declaredEmpty =
      mismatchHigh
        .coefficient(Hrfs.SPMG2, BasisRole.Canonical)
        .named("probe-empty-mismatch-high", "declared but unobserved mismatch-high probe cell")
    // docs:first-level-hypotheses:end

    val eventOrigins = schema.columns.collect {
      case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => origin
    }
    val blockCounts = eventOrigins.groupMapReduce { origin =>
      (origin.term.value, origin.modulator.fold("main")(_.value))
    }(_ => 1)(_ + _)
    val expectedBlocks = Map(
      ("sample", "main") -> 2,
      ("delay", "main") -> 24,
      ("probe", "main") -> 12,
      ("probe_rt", "rt") -> 12
    )
    val provenance = schema.audit.eventProvenance
    val sampleWindowResult = sampleWindow.compile(schema)
    val delayWindowResult = delayWindow.compile(schema)
    val probeAtSixResult = probeAtSix.compile(schema)
    val matchByLoadResult = matchByLoad.compile(schema)
    val rtSlopeResult = rtSlope.compile(schema)
    val probeShapeResult = probeShape.compile(schema)
    val delayOmnibusResult = delayOmnibus.compile(schema)
    val emptyResult = declaredEmpty.compile(schema)
    val sampleWindowEvaluation = sampleWindowResult.flatMap(_.evaluate(fixed))
    val delayWindowEvaluation = delayWindowResult.flatMap(_.evaluate(fixed))
    val probeAtSixEvaluation = probeAtSixResult.flatMap(_.evaluate(fixed))
    val matchByLoadEvaluation = matchByLoadResult.flatMap(_.evaluate(fixed))
    val rtSlopeEvaluation = rtSlopeResult.flatMap(_.evaluate(fixed))
    val probeShapeEvaluation = probeShapeResult.flatMap(_.evaluate(fixed))
    val delayOmnibusEvaluation = delayOmnibusResult.flatMap(_.evaluate(fixed))
    val hypothesisActual = Vector(
      sampleWindowEvaluation.toOption.map(_.statistics(0)),
      delayWindowEvaluation.toOption.map(_.statistics(0)),
      probeAtSixEvaluation.toOption.map(_.statistics(0)),
      matchByLoadEvaluation.toOption.map(_.statistics(0)),
      rtSlopeEvaluation.toOption.map(_.statistics(0)),
      probeShapeEvaluation.toOption.map(_.statistics(0)),
      delayOmnibusEvaluation.toOption.map(_.statistics(0))
    ).flatten
    val hypothesisExpected = Vector(
      DmsRFixture.tExpected.get("sample-face-minus-scene").map(_.statistic),
      DmsRFixture.tExpected.get("delay-high-minus-low").map(_.statistic),
      DmsRFixture.tExpected.get("probe-mismatch-at-six").map(_.statistic),
      DmsRFixture.tExpected.get("probe-match-by-load").map(_.statistic),
      DmsRFixture.tExpected.get("probe-rt-slope").map(_.statistic),
      DmsRFixture.fExpected.get("probe-shape").map(_.statistic),
      DmsRFixture.fExpected.get("delay-high-omnibus").map(_.statistic)
    ).flatten

    ScenarioHarness.result(
      "fit.dms-multiphase-dsl.v1",
      Vector(
        ScenarioHarness.fact("one public model owns all four task blocks", plan.model.eventModel.termKeys == Vector("sample", "delay", "probe", "probe_rt"), s"terms=${plan.model.eventModel.termKeys.mkString(",")}"),
        ScenarioHarness.fact("task block cardinalities are structural", blockCounts == expectedBlocks, s"actual=$blockCounts expected=$expectedBlocks"),
        ScenarioHarness.fact("task design has exactly fifty columns", eventOrigins.length == 50, s"actual=${eventOrigins.length}"),
        ScenarioHarness.fact("every task column retains phase and basis identity", eventOrigins.forall(origin => origin.phase.nonEmpty && origin.basis.nonEmpty), "a task origin lost phase or basis identity"),
        ScenarioHarness.fact("all sixty parent trials survive phase lowering", provenance.map(_.parent.value).distinct.length == TrialCount, s"parents=${provenance.map(_.parent.value).distinct.length}"),
        ScenarioHarness.fact("source row provenance is preserved", provenance.map(_.sourceRow).distinct.sorted == (0 until TrialCount).toVector, s"rows=${provenance.map(_.sourceRow).distinct.sorted.mkString(",")}"),
        ScenarioHarness.fact("sample, delay, and probe phases remain explicit", provenance.flatMap(_.phase.map(_.value)).distinct.sorted == Vector("delay", "probe", "sample"), s"phases=${provenance.flatMap(_.phase.map(_.value)).distinct.sorted.mkString(",")}"),
        ScenarioHarness.fact("missing RT is a library-owned zero contribution", schema.audit.missingValues.exists(value => value.modulator.value == "rt" && value.action == "zero-contribution"), s"missing=${schema.audit.missingValues}"),
        ScenarioHarness.fact("missing RT retains its source trial", schema.audit.missingValues.exists(value => value.source.exists(source => source.parent.value == "trial-018" && source.phase.exists(_.value == "probe") && source.sourceRow == 17)), s"missing=${schema.audit.missingValues}"),
        ScenarioHarness.fact("RT centering is within match-by-load cells", schema.audit.centeringReceipts.exists(_.policy == CenteringPolicy.WithinCells(Vector(matchStatus.id, load.id))), s"centering=${schema.audit.centeringReceipts.map(_.policy)}"),
        ScenarioHarness.fact("the empty mismatch-high cell has a receipt", schema.audit.emptyCells.contains(mismatchHigh.cell), s"empty=${schema.audit.emptyCells.map(_.canonical).mkString(",")}"),
        ScenarioHarness.fact("sample window hypothesis compiles semantically", sampleWindowResult.isRight, sampleWindowResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("delay window hypothesis compiles semantically", delayWindowResult.isRight, delayWindowResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("probe at-time hypothesis compiles semantically", probeAtSixResult.isRight, probeAtSixResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("match-by-load hypothesis compiles semantically", matchByLoadResult.isRight, matchByLoadResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("RT slope hypothesis selects only the modulator block", rtSlopeResult.toOption.exists(_.selectedColumnIds.length == 2), rtSlopeResult.fold(_.message, value => s"selected=${value.selectedColumnIds.length}")),
        ScenarioHarness.fact("non-canonical probe omnibus compiles", probeShapeResult.toOption.exists(_.numeratorRank == 1), probeShapeResult.fold(_.message, value => s"rank=${value.numeratorRank}")),
        ScenarioHarness.fact("delay FIR omnibus compiles", delayOmnibusResult.toOption.exists(_.numeratorRank == 8), delayOmnibusResult.fold(_.message, value => s"rank=${value.numeratorRank}")),
        ScenarioHarness.fact("empty cell fails through typed estimability", emptyResult.left.exists {
          case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, _) => true
          case _ => false
        }, emptyResult.fold(_.message, _ => "unexpectedly compiled")),
        ScenarioHarness.fact(
          "fit uses separate-runs fixed effects",
          fixed.engine == FitEngine.FixedEffects &&
            fixed.summary.coefficientScope == scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects,
          s"actual=${fixed.engine}/${fixed.summary.coefficientScope}"
        ),
        ScenarioHarness.fact("fixed-effects axis contains the forty-six estimable task columns", fixed.predictors == 46, s"actual=${fixed.predictors}"),
        ScenarioHarness.fact("each run contributes the external estimable axis", fixed.perRunContributions.map(_.predictors) == Vector(46, 46), s"actual=${fixed.perRunContributions.map(_.predictors)}"),
        ScenarioHarness.fact("run residual degrees of freedom match R", fixed.perRunContributions.map(_.residualDegreesOfFreedom.value) == DmsRFixture.runResidualDf, s"actual=${fixed.perRunContributions.map(_.residualDegreesOfFreedom.value)} expected=${DmsRFixture.runResidualDf}"),
        ScenarioHarness.fact("fixed residual degrees of freedom match R", fixed.effectiveResidualDegreesOfFreedom.value == DmsRFixture.fixedResidualDf, s"actual=${fixed.effectiveResidualDegreesOfFreedom.value} expected=${DmsRFixture.fixedResidualDf}"),
        ScenarioHarness.fact("receipt declares independent R sources", DmsRFixture.fmridesignRevision.nonEmpty && DmsRFixture.fmrihrfRevision.nonEmpty && DmsRFixture.fmridesignVersion == "0.6.0" && DmsRFixture.fmrihrfVersion == "0.4.0", s"fmridesign=${DmsRFixture.fmridesignVersion}@${DmsRFixture.fmridesignRevision} fmrihrf=${DmsRFixture.fmrihrfVersion}@${DmsRFixture.fmrihrfRevision}"),
        ScenarioHarness.fact("receipt declares every cross-system difference", DmsRFixture.acceptedDifferences.length == 4, s"differences=${DmsRFixture.acceptedDifferences.length}")
      ) ++
        tReceipt("sample-face-minus-scene", sampleWindowEvaluation) ++
        tReceipt("delay-high-minus-low", delayWindowEvaluation) ++
        tReceipt("probe-mismatch-at-six", probeAtSixEvaluation) ++
        tReceipt("probe-match-by-load", matchByLoadEvaluation) ++
        tReceipt("probe-rt-slope", rtSlopeEvaluation) ++
        fReceipt("probe-shape", probeShapeEvaluation) ++
        fReceipt("delay-high-omnibus", delayOmnibusEvaluation) ++
        ScenarioHarness.comparisonMetrics(
          "R hypothesis statistics",
          hypothesisActual,
          hypothesisExpected,
          ReceiptProfile
        )
    )

  private val ReceiptTolerance = ScenarioTolerance.mixed(1e-8, 1e-6)
  private val ReceiptProfile = ScenarioComparisonTolerance.bounded(
    maxAbsoluteL2Error = 2.0,
    maxRelativeL2Error = 2e-6,
    minimumSignedCorrelation = 0.99999999,
    maxNormRatioDeviation = 2e-6
  )

  private def tReceipt(
      name: String,
      evaluated: Either[FitError, TContrastResult]
  ): Vector[ScenarioObservation] =
    (DmsRFixture.tExpected.get(name), evaluated) match
      case (Some(expected), Right(actual)) =>
        Vector(
          ScenarioHarness.scalar(s"$name estimate matches R", actual.estimates(0), expected.estimate, ReceiptTolerance),
          ScenarioHarness.scalar(s"$name standard error matches R", actual.standardErrors(0), expected.standardError, ReceiptTolerance),
          ScenarioHarness.scalar(s"$name statistic matches R", actual.statistics(0), expected.statistic, ReceiptTolerance),
          ScenarioHarness.fact(s"$name residual df matches R", actual.residualDegreesOfFreedom.value == DmsRFixture.fixedResidualDf, s"actual=${actual.residualDegreesOfFreedom.value} expected=${DmsRFixture.fixedResidualDf}")
        )
      case (None, _) =>
        Vector(ScenarioHarness.fact(s"$name has an R receipt", passed = false, detail = "receipt entry was absent"))
      case (Some(_), Left(error)) =>
        Vector(ScenarioHarness.fact(s"$name evaluates", passed = false, detail = error.message))

  private def fReceipt(
      name: String,
      evaluated: Either[FitError, FContrastResult]
  ): Vector[ScenarioObservation] =
    (DmsRFixture.fExpected.get(name), evaluated) match
      case (Some(expected), Right(actual)) =>
        Vector(
          ScenarioHarness.scalar(s"$name statistic matches R", actual.statistics(0), expected.statistic, ReceiptTolerance),
          ScenarioHarness.fact(s"$name numerator df matches R", actual.numeratorDegreesOfFreedom == expected.numeratorDf, s"actual=${actual.numeratorDegreesOfFreedom} expected=${expected.numeratorDf}"),
          ScenarioHarness.fact(s"$name residual df matches R", actual.residualDegreesOfFreedom.value == expected.denominatorDf, s"actual=${actual.residualDegreesOfFreedom.value} expected=${expected.denominatorDf}")
        )
      case (None, _) =>
        Vector(ScenarioHarness.fact(s"$name has an R receipt", passed = false, detail = "receipt entry was absent"))
      case (Some(_), Left(error)) =>
        Vector(ScenarioHarness.fact(s"$name evaluates", passed = false, detail = error.message))

  private val TrialCount = DmsRFixture.trialIds.length

  private val samplingFrame = SamplingFrame(
    blockLens = Seq(180, 180),
    tr = Seq(1.0, 1.0),
    startTime = Seq(0.0, 0.0)
  )

  private val trials = DatasetEvents
    .fromColumns(
      "trial_id" -> DatasetEventColumn.text(DmsRFixture.trialIds),
      "run" -> DatasetEventColumn.text(DmsRFixture.runs),
      "sample_onset" -> DatasetEventColumn.numbers(DmsRFixture.sampleOnsets),
      "sample_duration" -> DatasetEventColumn.numbers(DmsRFixture.sampleDurations),
      "delay_onset" -> DatasetEventColumn.numbers(DmsRFixture.delayOnsets),
      "delay_duration" -> DatasetEventColumn.numbers(DmsRFixture.delayDurations),
      "probe_onset" -> DatasetEventColumn.numbers(DmsRFixture.probeOnsets),
      "probe_duration" -> DatasetEventColumn.numbers(DmsRFixture.probeDurations),
      "stimulus" -> DatasetEventColumn.text(DmsRFixture.stimulus),
      "load" -> DatasetEventColumn.text(DmsRFixture.load),
      "match" -> DatasetEventColumn.text(DmsRFixture.matchStatus),
      "rt" -> DatasetEventColumn.numbers(DmsRFixture.rt)
    )
    .fold(error => fail(error.message), identity)

  private val dataset = FmriDataset
    .open(
      backend = InMemoryDatasetBackend(
        DatasetId("dms-multiphase-dsl"),
        ImageDMat.fromRows(DmsRFixture.response.map(value => Vector(value))),
        SampleSpaces(Vector(1, 1, 1))
      ),
      samplingFrame = samplingFrame,
      events = trials
    )
    .fold(error => fail(error.message), identity)
