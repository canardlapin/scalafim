package scalafim.fmri.fit.scenarios

import scalafim.dataset.{DatasetEventRow, DatasetEvents, DatasetFieldId, DatasetId, DatasetValue, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.event.{ConvolvedTerm, ContinuousEvent}
import scalafim.fmri.fit.*
import scalafim.fmri.fit.fixtures.WlsRFixture
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.fit.StructuralHypothesisDsl.*
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{
  FitControls,
  FitStrategy,
  FmriModel,
  FmriModelBuilder,
  ModelBuildSpec,
  ModelError,
  ModelVolumeWeighting
}
import scalafim.image.SampleSpaces
import gale.linalg.DMat

/** Acceptance scenarios for the structural design/hypothesis seam.
  *
  * These deliberately construct the model through the public event/model
  * builders. The only dense arithmetic below is deterministic response
  * synthesis; no contrast uses a column offset or parses a rendered name.
  */
class StructuralFirstLevelScenarioSuite extends munit.FunSuite:

  private val Sampling = scalafim.fmri.hrf.design.SamplingFrame(blockLens = Seq(80), tr = Seq(1.0))
  private val F1 = FactorId.unsafe("f1")
  private val F2 = FactorId.unsafe("f2")
  private val Condition = FactorId.unsafe("condition")
  private val Cells = Vector(
    CellKey.unsafe(Seq(CellAssignment(F1, LevelId.unsafe("A")), CellAssignment(F2, LevelId.unsafe("X")))),
    CellKey.unsafe(Seq(CellAssignment(F1, LevelId.unsafe("B")), CellAssignment(F2, LevelId.unsafe("X")))),
    CellKey.unsafe(Seq(CellAssignment(F1, LevelId.unsafe("A")), CellAssignment(F2, LevelId.unsafe("Y")))),
    CellKey.unsafe(Seq(CellAssignment(F1, LevelId.unsafe("B")), CellAssignment(F2, LevelId.unsafe("Y"))))
  )
  private val ThreeCells =
    for
      first <- Vector("A", "B", "C")
      second <- Vector("X", "Y", "Z")
    yield CellKey.unsafe(Seq(CellAssignment(F1, LevelId.unsafe(first)), CellAssignment(F2, LevelId.unsafe(second))))

  test("S07 2x2 public design supports semantic simple and interaction hypotheses"):
    val result = runTwoByTwoScenario()
    if !result.ciPass then fail(result.render)

  test("S08 3x3 public design supports unequal-score trends and interaction"):
    val result = runThreeByThreeScenario()
    if !result.ciPass then fail(result.render)

  test("S09 public design supports heterogeneous HRF bases by structural cell"):
    val result = runHeterogeneousHrfScenario()
    if !result.ciPass then fail(result.render)

  test("S06 public model preserves factor/modulator scope and missing-value policy"):
    val result = runFactorModulatorScenario()
    if !result.ciPass then fail(result.render)

  test("S10 reconstructed basis responses use explicit functional weights"):
    val result = runResponseFunctionalScenario()
    if !result.ciPass then fail(result.render)

  test("S16 rank-deficient structural hypotheses report row-space evidence"):
    val result = runRankDeficientScenario()
    if !result.ciPass then fail(result.render)

  test("S18 public WLS agrees with an independent base-R receipt"):
    val result = runWeightedLeastSquaresScenario()
    if !result.ciPass then fail(result.render)

  private def runTwoByTwoScenario(): ScenarioResult =
    val plan = buildPlan(dataset(Vector.fill(80)(Vector(0.0, 0.0))))
    val schema = plan.model.designSchema.getOrElse(fail("public model must expose a design schema"))
    val responses = synthesize(plan.model)
    val fitted = FitPlanExecutor.fit(buildPlan(dataset(responses))) match
      case Right(value: DenseFmriFitResult) => value
      case Right(value) => fail(s"ordinary least squares returned ${value.engine} instead of a dense fit")
      case Left(error) => fail(s"ordinary least squares fit failed: ${error.message}")
    val f1 = factor(F1)
    val f2 = factor(F2)
    val task = term("task")
    val aX = task.cell(f1 === "A", f2 === "X")
    val bX = task.cell(f1 === "B", f2 === "X")
    val aY = task.cell(f1 === "A", f2 === "Y")
    val bY = task.cell(f1 === "B", f2 === "Y")
    val aXCanonical = aX.coefficient(Hrfs.SPMG3, BasisRole.Canonical)
    val bXCanonical = bX.coefficient(Hrfs.SPMG3, BasisRole.Canonical)
    val aYCanonical = aY.coefficient(Hrfs.SPMG3, BasisRole.Canonical)
    val bYCanonical = bY.coefficient(Hrfs.SPMG3, BasisRole.Canonical)

    val simple =
      (aXCanonical - bXCanonical)
        .named("s07-simple-A-minus-B-at-X", "A minus B at X, canonical HRF")
    val interaction =
      (aXCanonical - bXCanonical - aYCanonical + bYCanonical)
        .named("s07-interaction", "(A-B) at X minus (A-B) at Y")
    val f1Main =
      (aXCanonical - bXCanonical + aYCanonical - bYCanonical)
        .named("s07-main-f1", "A minus B averaged across the second factor")
    val omnibus =
      (aX.omnibus(Hrfs.SPMG3) + bX.omnibus(Hrfs.SPMG3) +
        aY.omnibus(Hrfs.SPMG3) + bY.omnibus(Hrfs.SPMG3))
        .named("s07-probe-omnibus", "joint canonical and temporal/dispersion task effects")
    val simpleCompiled = simple.compile(schema)
    val interactionCompiled = interaction.compile(schema)
    val simpleResult = simpleCompiled.flatMap(_.evaluate(fitted))
    val interactionResult = interactionCompiled.flatMap(_.evaluate(fitted))
    val f1MainResult = f1Main.compile(schema).flatMap(_.evaluate(fitted))
    val omnibusResult = omnibus.compile(schema).flatMap(_.evaluate(fitted))
    val simpleEstimate = simpleResult.toOption.map(_.estimates(0))
    val interactionEstimate = interactionResult.toOption.map(_.estimates(0))
    val f1MainEstimate = f1MainResult.toOption.map(_.estimates(0))
    val structuralCells = schema.columns.flatMap { column =>
      column.origin match
        case event: StructuralColumnOrigin.Event => Some(event.cell)
        case _                                   => None
    }.toSet
    val hasLegacyOrigin = schema.columns.exists { column =>
      column.origin match
        case _: StructuralColumnOrigin.Legacy => true
        case _                                => false
    }
    ScenarioHarness.result(
      "fit.structural-s07-2x2.v1",
      Vector(
        ScenarioHarness.fact("public schema present", plan.model.designSchema.nonEmpty, "FmriModelBuilder did not attach DesignSchema"),
        ScenarioHarness.fact("four structural cells", structuralCells == Cells.toSet, s"cells=${structuralCells.mkString(",")}"),
        ScenarioHarness.fact("no rendered-name bookkeeping", !hasLegacyOrigin, "legacy origin leaked into the public event schema"),
        ScenarioHarness.fact("simple effect compiles", simpleResult.isRight, simpleResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("interaction compiles", interactionResult.isRight, interactionResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("main effect compiles", f1MainResult.isRight, f1MainResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("basis omnibus compiles", omnibusResult.isRight, omnibusResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("basis omnibus has twelve independent rows", omnibusResult.toOption.exists(_.numeratorDegreesOfFreedom == 12), omnibusResult.fold(_.message, result => s"numerator df=${result.numeratorDegreesOfFreedom}")),
        ScenarioHarness.fact("simple effect carries estimability", simpleCompiled.toOption.exists(_.estimability.rowSpacePreserved), "simple effect was not estimable"),
        ScenarioHarness.fact("interaction carries estimability", interactionCompiled.toOption.exists(_.estimability.rowSpacePreserved), "interaction was not estimable"),
        ScenarioHarness.fact("simple effect recovers the planted difference", simpleEstimate.exists(value => math.abs(value - 0.6) < 0.1), s"estimate=${simpleEstimate.getOrElse(Double.NaN)} expected=0.6"),
        ScenarioHarness.fact("interaction recovers the planted difference", interactionEstimate.exists(value => math.abs(value + 0.1) < 0.1), s"estimate=${interactionEstimate.getOrElse(Double.NaN)} expected=-0.1"),
        ScenarioHarness.fact("main effect recovers the planted difference", f1MainEstimate.exists(value => math.abs(value - 1.3) < 0.1), s"estimate=${f1MainEstimate.getOrElse(Double.NaN)} expected=1.3"),
        ScenarioHarness.fact("fitted coefficients are finite", fitted.coefficients.value.valuesRowMajor.forall(_.isFinite), "non-finite coefficients")
      ),
      Vector.empty
    )

  private def runResponseFunctionalScenario(): ScenarioResult =
    val plan = buildPlan(dataset(Vector.fill(80)(Vector(0.0, 0.0))))
    val schema = plan.model.designSchema.getOrElse(fail("public model must expose a design schema"))
    val responseBasis = ResponseBasis.of(Hrfs.SPMG3)
    val atTime = responseBasis
      .responseFunctional(ResponseFunctional.At(Seconds(6.0)), FunctionalDiscretization.Exact)
      .fold(error => fail(error.message), identity)
    val weights = responseBasis
      .responseFunctional(ResponseFunctional.WindowMean(Seconds(4.0), Seconds(8.0)), FunctionalDiscretization.Exact)
      .fold(error => fail(error.message), identity)
    val firBasis = ResponseBasis.of(Hrfs.fir(nBasis = 4, span = Seconds(8.0)))
    val firWindow = firBasis
      .responseFunctional(ResponseFunctional.WindowMean(Seconds(2.0), Seconds(6.0)), FunctionalDiscretization.Exact)
      .fold(error => fail(error.message), identity)
    val f1 = factor(F1)
    val f2 = factor(F2)
    val task = term("task")
    val left = task.cell(f1 === "A", f2 === "X")
    val right = task.cell(f1 === "B", f2 === "X")
    val canonicalHypothesis =
      left.coefficient(Hrfs.SPMG3, BasisRole.Canonical)
        .named("s10-canonical-coefficient", "canonical coefficient for the left cell")
    val temporalHypothesis =
      left.coefficient(Hrfs.SPMG3, BasisRole.TemporalDerivative)
        .named("s10-temporal-derivative-coefficient", "temporal derivative coefficient for the left cell")
    val basisOmnibus =
      left.omnibus(Hrfs.SPMG3)
        .named("s10-all-basis-omnibus", "all three basis coefficients for the left cell")
    val atTimeHypothesis =
      (left.response(Hrfs.SPMG3, ResponseFunctional.At(Seconds(6.0))) -
        right.response(Hrfs.SPMG3, ResponseFunctional.At(Seconds(6.0))))
        .named("s10-at-six-A-minus-B", "A minus B reconstructed response at six seconds")
    val hypothesis =
      (left.response(Hrfs.SPMG3, ResponseFunctional.WindowMean(Seconds(4.0), Seconds(8.0))) -
        right.response(Hrfs.SPMG3, ResponseFunctional.WindowMean(Seconds(4.0), Seconds(8.0))))
        .named(
          "s10-window-mean-A-minus-B",
          "A minus B reconstructed response averaged from four to eight seconds"
        )
    val compiled = hypothesis.compile(schema)
    val canonicalCompiled = canonicalHypothesis.compile(schema)
    val temporalCompiled = temporalHypothesis.compile(schema)
    val basisOmnibusCompiled = basisOmnibus.compile(schema)
    val atTimeCompiled = atTimeHypothesis.compile(schema)
    val selected = compiled.toOption.map(_.selectedColumnIds.toSet).getOrElse(Set.empty)
    val selectedCells = schema.columns.collect {
      case column if selected.contains(column.id) =>
        column.origin match
          case event: StructuralColumnOrigin.Event => Some(event.cell)
          case _                                   => None
    }.flatten.toSet
    ScenarioHarness.result(
      "fit.structural-s10-response-functional.v1",
      Vector(
        ScenarioHarness.fact("canonical coefficient is a distinct semantic selector", canonicalCompiled.toOption.exists(_.selectedColumnIds.length == 1), canonicalCompiled.fold(_.message, value => s"selected=${value.selectedColumnIds}")),
        ScenarioHarness.fact("temporal derivative coefficient is a distinct semantic selector", temporalCompiled.toOption.exists(_.selectedColumnIds.length == 1) && canonicalCompiled.toOption.map(_.selectedColumnIds) != temporalCompiled.toOption.map(_.selectedColumnIds), temporalCompiled.fold(_.message, value => s"selected=${value.selectedColumnIds}")),
        ScenarioHarness.fact("canonical coefficient hypothesis compiles", canonicalCompiled.isRight, canonicalCompiled.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("temporal derivative coefficient hypothesis compiles", temporalCompiled.isRight, temporalCompiled.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("all-basis omnibus hypothesis compiles", basisOmnibusCompiled.isRight, basisOmnibusCompiled.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("all-basis omnibus retains three numerator degrees of freedom", basisOmnibusCompiled.toOption.exists(_.numeratorRank == 3), basisOmnibusCompiled.fold(_.message, result => s"numerator rank=${result.numeratorRank}")),
        ScenarioHarness.fact("at-time semantic hypothesis compiles", atTimeCompiled.isRight, atTimeCompiled.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("at-time functional has response-value units", atTime.units == ResponseUnits.ResponseValue, s"units=${atTime.units}"),
        ScenarioHarness.fact("at-time functional has one value per basis element", atTime.weights.dimension == responseBasis.dimension, s"dimension=${atTime.weights.dimension}"),
        ScenarioHarness.fact("exact primitive window is explicit", weights.receipt.policy == FunctionalDiscretization.Exact, s"receipt=${weights.receipt}"),
        ScenarioHarness.fact("functional units are response values", weights.units == ResponseUnits.ResponseValue, s"units=${weights.units}"),
        ScenarioHarness.fact("FIR-window functional has explicit bin weights", firWindow.weights.dimension == firBasis.dimension && firWindow.receipt.policy == FunctionalDiscretization.Exact, s"dimension=${firWindow.weights.dimension} receipt=${firWindow.receipt}"),
        ScenarioHarness.fact("functional hypothesis compiles", compiled.isRight, compiled.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("functional selects all six basis coordinates", selected.size == 6, s"selected=$selected"),
        ScenarioHarness.fact("functional selects both semantic cells", selectedCells == Set(Cells(0), Cells(1)), s"selected cells=$selectedCells"),
        ScenarioHarness.fact("functional receipt retained", compiled.toOption.exists(_.metadata.responseFunctionals.nonEmpty), "compiled hypothesis lost functional provenance"),
        ScenarioHarness.fact(
          "nonlinear summary is not a linear functional",
          NonlinearResponseSummary.PeakLatency(Seconds(4.0), Seconds(8.0)) match
            case NonlinearResponseSummary.PeakLatency(_, _) => true,
          "nonlinear summary was unexpectedly coerced"
        )
      ),
      Vector.empty
    )

  private def runHeterogeneousHrfScenario(): ScenarioResult =
    val registry = FactorLevelRegistry.of(
      "condition" -> Seq("A", "B", "C")
    ).fold(error => fail(error.message), identity)
    val fir = Hrfs.fir(nBasis = 2, span = Seconds(4.0))
    val cells = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2,
      "C" -> fir
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "probe" -> HrfAssignment.ByCell(cells)
    ).fold(error => fail(error.message), identity)
    val spec = ModelBuildSpec(
      formula = "onset ~ hrf(condition, phase = probe, parent = trial, id = probe)",
      baselineIntercept = Intercept.Global,
      factorLevels = registry,
      emptyCellPolicy = EmptyCellPolicy.RetainZero,
      hrfByPhase = Some(assignments)
    )
    val emptyResponse = Vector.fill(80)(Vector(0.0, 0.0))
    val plan = FmriModelBuilder.buildPlanEither(heterogeneousDataset(emptyResponse), spec)
      .fold(error => fail(error.message), identity)
    val schema = plan.model.designSchema.getOrElse(fail("heterogeneous-HRF model must expose a structural schema"))
    val responses = synthesizeHeterogeneous(plan.model)
    val responsePlan = FmriModelBuilder.buildPlanEither(heterogeneousDataset(responses), spec)
      .fold(error => fail(error.message), identity)
    val fitted = FitPlanExecutor.fit(responsePlan) match
      case Right(value: DenseFmriFitResult) => value
      case Right(value) => fail(s"ordinary least squares returned ${value.engine} instead of a dense fit")
      case Left(error) => fail(s"heterogeneous-HRF fit failed: ${error.message}")

    val condition = factor(Condition)
    val probe = term("probe").inPhase("probe")
    val a = probe.cell(condition === "A")
    val b = probe.cell(condition === "B")
    val c = probe.cell(condition === "C")
    val aMinusB =
      (a.coefficient(Hrfs.SPMG1, BasisRole.Canonical) -
        b.coefficient(Hrfs.SPMG2, BasisRole.Canonical))
        .named("s09-a-minus-b-canonical", "A minus B canonical response under heterogeneous cell bases")
    val cOmnibus = c.omnibus(fir)
      .named("s09-c-fir-omnibus", "joint FIR basis effect for structural cell C")
    val absentRole = a.coefficient(Hrfs.SPMG2, BasisRole.TemporalDerivative)
      .named("s09-a-temporal", "temporal derivative in canonical-only cell A")
    val aMinusBResult = aMinusB.compile(schema).flatMap(_.evaluate(fitted))
    val cOmnibusResult = cOmnibus.compile(schema).flatMap(_.evaluate(fitted))
    val absentRoleResult = absentRole.compile(schema)
    val aMinusBEstimate = aMinusBResult.toOption.map(_.estimates(0))
    val eventOrigins = schema.columns.flatMap(_.origin match
      case event: StructuralColumnOrigin.Event => Some(event)
      case _                                   => None
    )
    val basisCounts = eventOrigins.flatMap(origin => origin.cell.get(Condition).map(_.value))
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toMap
    ScenarioHarness.result(
      "fit.structural-s09-heterogeneous-hrf.v1",
      Vector(
        ScenarioHarness.fact("public model path accepts phase-scoped heterogeneous assignments", plan.model.eventModel.policyReceipts.exists(_.name == "hrf-by-phase"), s"receipts=${plan.model.eventModel.policyReceipts}"),
        ScenarioHarness.fact("canonical, informed, and FIR assignments coexist", basisCounts == Map("A" -> 1, "B" -> 2, "C" -> 2), s"basisCounts=$basisCounts"),
        ScenarioHarness.fact("heterogeneous event cardinality is derived from assigned bases", eventOrigins.size == 5 && schema.matrix.cols == 6, s"eventColumns=${eventOrigins.size} matrixCols=${schema.matrix.cols}"),
        ScenarioHarness.fact("every realized column retains structural phase cell role and element provenance", eventOrigins.forall(origin => origin.phase.exists(_.value == "probe") && origin.basis.exists(ref => ref.role.nonEmpty && ref.elementId.nonEmpty)), "an event column lost its structural origin"),
        ScenarioHarness.fact("semantic canonical contrast compiles", aMinusBResult.isRight, aMinusBResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("semantic FIR omnibus compiles with two numerator degrees of freedom", cOmnibusResult.toOption.exists(_.numeratorDegreesOfFreedom == 2), cOmnibusResult.fold(_.message, result => s"numerator df=${result.numeratorDegreesOfFreedom}")),
        ScenarioHarness.fact("absent cell-specific basis roles are typed", absentRoleResult match
          case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownBasis, _)) => true
          case _ => false,
          absentRoleResult.fold(_.message, _ => "unexpectedly compiled")
        ),
        ScenarioHarness.fact("solver-only truth is explicitly identified", true, "responses are generated from the compiled design; the independently generated design oracle is design.heterogeneous-hrf.v1"),
        ScenarioHarness.fact("canonical contrast recovers the planted cell difference", aMinusBEstimate.exists(value => math.abs(value + 0.4) < 0.1), s"estimate=${aMinusBEstimate.getOrElse(Double.NaN)} expected=-0.4"),
        ScenarioHarness.fact("fit retains heterogeneous structural rank evidence", fitted.structuralRankReport.isRight, fitted.structuralRankReport.fold(_.message, report => s"rank=${report.numericalRank}"))
      ),
      Vector.empty
    )

  private def runFactorModulatorScenario(): ScenarioResult =
    val registry = FactorLevelRegistry.of(
      "condition" -> Seq("A", "B")
    ).toOption.getOrElse(fail("factor/modulator registry must be valid"))
    val formula =
      "onset ~ hrf(condition, basis=\"spmg1\", id=\"condition\") + " +
        "hrf(condition, center_within(rt, condition), basis=\"spmg1\", id=\"withinSlope\") + " +
        "hrf(center(rt), basis=\"spmg1\", id=\"sharedSlope\")"
    val spec = ModelBuildSpec(
      formula = formula,
      baselineIntercept = Intercept.Global,
      defaultHrf = Hrfs.SPMG1,
      factorLevels = registry,
      emptyCellPolicy = EmptyCellPolicy.RetainZero,
      missingValuePolicy = MissingValuePolicy.ZeroContribution
    )
    val emptyResponse = Vector.fill(80)(Vector(0.0, 0.0))
    def planWith(policy: MissingValuePolicy, response: Vector[Vector[Double]]) =
      FmriModelBuilder.buildPlanEither(
        factorModulatorDataset(response, missingRt = true),
        spec.copy(missingValuePolicy = policy)
      )

    val initial = planWith(MissingValuePolicy.ZeroContribution, emptyResponse)
      .fold(error => fail(error.message), identity)
    val schema = initial.model.designSchema.getOrElse(fail("factor/modulator model must expose a structural schema"))
    val response = synthesizeFactorModulator(initial.model)
    val fittedPlan = planWith(MissingValuePolicy.ZeroContribution, response)
      .fold(error => fail(error.message), identity)
    val fitted = FitPlanExecutor.fit(fittedPlan) match
      case Right(value: DenseFmriFitResult) => value
      case Right(value) => fail(s"S06 ordinary least squares returned ${value.engine} instead of a dense fit")
      case Left(error) => fail(s"S06 ordinary least squares failed: ${error.message}")
    val origins = schema.columns.flatMap { column =>
      column.origin match
        case event: StructuralColumnOrigin.Event => Some(event)
        case _                                   => None
    }
    val conditionOrigins = origins.filter(_.term.value == "condition")
    val withinOrigins = origins.filter(_.term.value == "withinSlope")
    val sharedOrigins = origins.filter(_.term.value == "sharedSlope")
    val centering = schema.audit.centeringReceipts
    val withinTermValues = continuousValues(initial.model, "withinSlope")
    val sharedTermValues = continuousValues(initial.model, "sharedSlope")
    val grandMean = Vector(1.0, 3.0, 5.0, 2.0, 4.0, 6.0, 8.0).sum / 7.0
    val imputed = planWith(MissingValuePolicy.ImputeConstant(9.0), emptyResponse)
      .fold(error => fail(error.message), identity)
    val dropped = planWith(MissingValuePolicy.DropFromTerm, emptyResponse)
      .fold(error => fail(error.message), identity)
    val rejected = planWith(MissingValuePolicy.Reject, emptyResponse)
    ScenarioHarness.result(
      "fit.structural-s06-factor-modulator.v1",
      Vector(
        ScenarioHarness.fact("public model exposes declared factor levels", schema.audit.factorLevels.map(_.declared.map(_.size)) == Vector(Some(2)), s"factor audit=${schema.audit.factorLevels}"),
        ScenarioHarness.fact("categorical and per-cell slope columns are distinct", conditionOrigins.size == 2 && withinOrigins.size == 2, s"condition=${conditionOrigins.size} within=${withinOrigins.size}"),
        ScenarioHarness.fact("shared slope is structurally separate from per-cell slopes", sharedOrigins.size == 1 && sharedOrigins.head.cell == CellKey.empty && withinOrigins.forall(_.cell != CellKey.empty), s"shared=$sharedOrigins within=$withinOrigins"),
        ScenarioHarness.fact("all modulator columns retain rt provenance", (withinOrigins ++ sharedOrigins).forall(_.modulator.exists(_.value == "rt")), s"origins=${withinOrigins ++ sharedOrigins}"),
        ScenarioHarness.fact("within-cell and grand-mean centering receipts are retained", centering.exists(_.policy == CenteringPolicy.WithinFactor(FactorId.unsafe("condition"))) && centering.exists(_.policy == CenteringPolicy.GrandMean), s"centering=$centering"),
        ScenarioHarness.fact("within-cell centering uses finite values within each factor level", withinTermValues.take(4).zip(Vector(-2.0, 0.0, 2.0, 0.0)).forall { case (actual, expected) => math.abs(actual - expected) < 1e-12 } && withinTermValues.drop(4).zip(Vector(-3.0, -1.0, 1.0, 3.0)).forall { case (actual, expected) => math.abs(actual - expected) < 1e-12 }, s"within=$withinTermValues"),
        ScenarioHarness.fact("grand-mean centering uses finite values before missing repair", math.abs(sharedTermValues.head - (1.0 - grandMean)) < 1e-12 && sharedTermValues(3) == 0.0, s"shared=$sharedTermValues grandMean=$grandMean"),
        ScenarioHarness.fact("zero-contribution is recorded for the missing trial", schema.audit.missingValues.exists(value => value.action == "zero-contribution" && value.eventIndex == 3) && initial.model.eventModel.policyReceipts.exists(_.detail.contains("zero-contribution")), s"missing=${schema.audit.missingValues} policies=${initial.model.eventModel.policyReceipts}"),
        ScenarioHarness.fact("impute-constant is a public model policy", imputed.model.eventModel.policyReceipts.exists(_.detail.contains("impute-constant")) && imputed.model.eventModel.missingValueResolutions.forall(_.action == "imputed-constant"), s"missing=${imputed.model.eventModel.missingValueResolutions}"),
        ScenarioHarness.fact("drop-from-term is a public model policy", dropped.model.eventModel.policyReceipts.exists(_.detail.contains("drop-from-term")) && dropped.model.eventModel.missingValueResolutions.forall(_.action == "dropped-event"), s"missing=${dropped.model.eventModel.missingValueResolutions}"),
        ScenarioHarness.fact(
          "reject remains a typed public-model failure",
          rejected match
            case Left(ModelError.DesignFailure(DesignError.MissingModulatorValue(term, column, row, policy))) =>
              term == "withinSlope" && column == "rt" && row == 3 && policy == MissingValuePolicy.Reject.label
            case _ => false,
          rejected.fold(_.message, _ => "reject unexpectedly succeeded")
        ),
        ScenarioHarness.fact("semantic model fit retains structural rank evidence", fitted.structuralRankReport.isRight, fitted.structuralRankReport.fold(_.message, report => s"rank=${report.numericalRank}")),
        ScenarioHarness.fact("solver-only response truth is declared", true, "responses are generated from the inspected compiled design; this is a solver/provenance scenario, not an independent end-to-end design oracle")
      ),
      Vector.empty
    )

  private def runThreeByThreeScenario(): ScenarioResult =
    val registry = FactorLevelRegistry.of(
      "f1" -> Seq("A", "B", "C"),
      "f2" -> Seq("X", "Y", "Z")
    ).toOption.getOrElse(fail("3x3 factor registry must be valid"))
    val emptyResponse = Vector.fill(80)(Vector(0.0, 0.0))
    val plan = FmriModelBuilder.buildPlan(
      datasetThreeByThree(emptyResponse),
      ModelBuildSpec(
        formula = "onset ~ hrf(f1, f2, basis=\"spmg1\", id=\"task\")",
        baselineIntercept = Intercept.Global,
        defaultHrf = Hrfs.SPMG1,
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.RetainZero
      )
    )
    val schema = plan.model.designSchema.getOrElse(fail("3x3 model must expose a structural schema"))
    val responses = synthesizeThreeByThree(plan.model)
    val fitted = FitPlanExecutor.fit(
      FmriModelBuilder.buildPlan(
        datasetThreeByThree(responses),
        ModelBuildSpec(
          formula = "onset ~ hrf(f1, f2, basis=\"spmg1\", id=\"task\")",
          baselineIntercept = Intercept.Global,
          defaultHrf = Hrfs.SPMG1,
          factorLevels = registry,
          emptyCellPolicy = EmptyCellPolicy.RetainZero
        )
      )
    ) match
      case Right(value: DenseFmriFitResult) => value
      case Right(value)                     => fail(s"S08 expected a dense fit, found ${value.engine}")
      case Left(error)                      => fail(error.message)

    val firstScores = Map("A" -> -17.0, "B" -> -2.0, "C" -> 19.0)
    val firstQuadratic = Map("A" -> 49.0, "B" -> -84.0, "C" -> 35.0)
    val secondScores = Map("X" -> -4.0, "Y" -> 1.0, "Z" -> 3.0)
    val firstLevels = Vector("A", "B", "C")
    val secondLevels = Vector("X", "Y", "Z")
    val firstFactor = factor("f1")
    val secondFactor = factor("f2")
    val task = term("task")
    def coefficient(first: String, second: String): SemanticT =
      task
        .cell(firstFactor === first, secondFactor === second)
        .coefficient(Hrfs.SPMG1, BasisRole.Canonical)
    def trend(values: Map[String, Double]): SemanticT =
      val terms =
        for
          first <- firstLevels
          second <- secondLevels
        yield coefficient(first, second) * (values(first) / secondLevels.length.toDouble)
      terms.reduce(_ + _)
    val linearExpression = trend(firstScores)
    val quadraticExpression = trend(firstQuadratic)
    val interactionExpression =
      val terms =
        for
          first <- firstLevels
          second <- secondLevels
        yield coefficient(first, second) *
          (firstScores(first) * secondScores(second) /
            (firstLevels.length * secondLevels.length).toDouble)
      terms.reduce(_ + _)

    val linear = linearExpression
      .named("s08-f1-linear", "unequal-score linear trend across f1")
    val quadratic = quadraticExpression
      .named("s08-f1-quadratic", "unequal-score quadratic trend across f1")
    val interaction = interactionExpression
      .named("s08-linear-by-linear", "linear f1 by linear f2 interaction")
    val trendOmnibus =
      (linearExpression.asOmnibusRow + quadraticExpression.asOmnibusRow)
        .named("s08-f1-trend-omnibus", "joint unequal-score linear and quadratic f1 trends")
    val linearResult = linear.compile(schema).flatMap(_.evaluate(fitted))
    val quadraticResult = quadratic.compile(schema).flatMap(_.evaluate(fitted))
    val interactionResult = interaction.compile(schema).flatMap(_.evaluate(fitted))
    val omnibusResult = trendOmnibus.compile(schema).flatMap(_.evaluate(fitted))
    val linearEstimate = linearResult.toOption.map(_.estimates(0))
    val quadraticEstimate = quadraticResult.toOption.map(_.estimates(0))
    val interactionEstimate = interactionResult.toOption.map(_.estimates(0))
    val eventCells = schema.columns.flatMap { column =>
      column.origin match
        case event: StructuralColumnOrigin.Event => Some(event.cell)
        case _                                   => None
    }.toSet
    ScenarioHarness.result(
      "fit.structural-s08-3x3-trends.v1",
      Vector(
        ScenarioHarness.fact("declared 3x3 factor schema is retained", schema.audit.factorLevels.map(_.declared.map(_.size)) == Vector(Some(3), Some(3)), s"factor audit=${schema.audit.factorLevels}"),
        ScenarioHarness.fact("all nine structural cells are present", eventCells == ThreeCells.toSet, s"cells=${eventCells.mkString(",")}"),
        ScenarioHarness.fact("stable cell identity is independent of rendered labels", eventCells == ThreeCells.toSet, "structural cell set changed"),
        ScenarioHarness.fact("unequal-score linear trend compiles", linearResult.isRight, linearResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("unequal-score quadratic trend compiles", quadraticResult.isRight, quadraticResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("linear-by-linear interaction compiles", interactionResult.isRight, interactionResult.fold(_.message, _ => "compiled")),
        ScenarioHarness.fact("trend omnibus compiles with two numerator degrees of freedom", omnibusResult.toOption.exists(_.numeratorDegreesOfFreedom == 2), omnibusResult.fold(_.message, result => s"numerator df=${result.numeratorDegreesOfFreedom}")),
        ScenarioHarness.fact("linear trend recovers the planted direction", linearEstimate.exists(_ > 0.0), s"estimate=${linearEstimate.getOrElse(Double.NaN)}"),
        ScenarioHarness.fact("quadratic trend recovers the planted direction", quadraticEstimate.exists(_ > 0.0), s"estimate=${quadraticEstimate.getOrElse(Double.NaN)}"),
        ScenarioHarness.fact("interaction recovers the planted direction", interactionEstimate.exists(_ > 0.0), s"estimate=${interactionEstimate.getOrElse(Double.NaN)}"),
        ScenarioHarness.fact("semantic selectors resolve without numeric offsets", linear.compile(schema).toOption.exists(_.selectedColumnIds.nonEmpty), "no structural columns selected"),
        ScenarioHarness.fact("fit retains structural rank evidence", fitted.structuralRankReport.isRight, fitted.structuralRankReport.fold(_.message, report => s"rank=${report.numericalRank}"))
      ),
      Vector.empty
    )

  private def runRankDeficientScenario(): ScenarioResult =
    val plan = buildPlan(
      dataset(
        Vector.fill(80)(Vector(0.0, 0.0)),
        includeCells = Some(Set(Cells(0)))
      ),
      formula =
        "onset ~ hrf(f1, f2, basis=\"spmg1\", id=\"task_a\") + " +
          "hrf(f1, f2, basis=\"spmg1\", id=\"task_b\")"
    )
    val schema = plan.model.designSchema.getOrElse(fail("public model must expose a design schema"))
    val f1 = factor(F1)
    val f2 = factor(F2)
    val taskA = term("task_a").cell(f1 === "A", f2 === "X")
    val taskB = term("task_b").cell(f1 === "A", f2 === "X")
    val taskBId = schema.columns.find { column =>
      column.origin match
        case event: StructuralColumnOrigin.Event =>
          event.term == TermId.unsafe("task_b") &&
            event.cell == Cells(0) &&
            event.basis.exists(_.role.contains(BasisRole.Canonical))
        case _ => false
    }.map(_.id).getOrElse(fail("rank fixture requires the second semantic task coordinate"))
    val taskBIndex = schema.columns.indexWhere(_.id == taskBId)
    require(taskBIndex >= 0, "semantic rank-fixture column must belong to the schema")
    val p = schema.matrix.cols
    val duplicate = schema
    val duplicateDesign = GaleTestMatrix.fromRows(
      Vector.tabulate(duplicate.matrix.rows) { row =>
        Vector.tabulate(duplicate.matrix.cols)(column => duplicate.matrix(row, column))
      }
    )
    val duplicateRankError = Ols.prepare(DesignMatrix.unsafe(duplicateDesign)).left.toOption.collect {
      case FitError.RankDeficientDesign(report) => report
    }
    val duplicateStructuralRank = duplicateRankError.flatMap(_.bind(duplicate.coefficientAxis).toOption)
    val reducedDesign = dropColumn(duplicateDesign, dropped = taskBIndex)
    val response = GaleTestMatrix.fromRows(synthesize(plan.model))
    val reducedFit = Ols.unsafeFit(
      DesignMatrix.unsafe(reducedDesign),
      ResponseBlock.unsafe(response)
    )
    val reducedOracle = independentFullRankFit(reducedDesign, response)
    val fitted = multiply(reducedDesign, reducedFit.coefficients.value)
    val residuals = subtract(response, fitted)
    val probeResponse = GaleTestMatrix.fromRows(
      Vector.tabulate(duplicate.matrix.rows) { row =>
        Vector(
          math.sin(row.toDouble * 0.17) + 0.2 * math.cos(row.toDouble * 0.03),
          math.cos(row.toDouble * 0.23) - 0.1 * math.sin(row.toDouble * 0.11)
        )
      }
    )
    val probeFit = Ols.unsafeFit(
      DesignMatrix.unsafe(reducedDesign),
      ResponseBlock.unsafe(probeResponse)
    )
    val probeOracle = independentFullRankFit(reducedDesign, probeResponse)
    val probeProjection = multiply(reducedDesign, probeFit.coefficients.value)
    val sum =
      (taskA.coefficient(Hrfs.SPMG1, BasisRole.Canonical) +
        taskB.coefficient(Hrfs.SPMG1, BasisRole.Canonical))
        .named("s16-estimable-duplicate-sum", "sum of two duplicate semantic coordinates")
    val component =
      taskB.coefficient(Hrfs.SPMG1, BasisRole.Canonical)
        .named("s16-non-estimable-component", "one duplicate semantic coordinate")
    val sumResult = sum.compile(duplicate)
    val componentResult = component.compile(duplicate)
    val emptyCellPlan = buildPlan(
      dataset(Vector.fill(80)(Vector(0.0, 0.0)), omitCell = Some(Cells(3)))
    )
    val emptyCellSchema = emptyCellPlan.model.designSchema.getOrElse(fail("empty-cell model must expose a design schema"))
    val emptyCellHypothesis =
      term("task").cell(f1 === "B", f2 === "Y")
        .coefficient(Hrfs.SPMG3, BasisRole.Canonical)
        .named("s16-empty-cell", "hypothesis for an absent B/Y cell")
    val emptyCellResult = emptyCellHypothesis.compile(emptyCellSchema)
    val zeroModulatorPlan = buildPlan(
      dataset(Vector.fill(80)(Vector(0.0, 0.0)), zeroModulator = true),
      formula = "onset ~ hrf(f1, f2, Scale(rt), basis=\"spmg3\", id=\"task\")",
      dropEmpty = false
    )
    val zeroModulatorSchema = zeroModulatorPlan.model.designSchema.getOrElse(fail("zero-modulator model must expose a design schema"))
    val observations = Vector(
      ScenarioHarness.fact("rank deficiency carries a typed numerical report", duplicateRankError.nonEmpty, "rank-deficient OLS preparation returned no RankDiagnostics"),
      ScenarioHarness.fact("rank report partitions predictors", duplicateRankError.exists(report => report.pivotOrder.length == p && report.independentPredictors.length == report.numericalRank && report.aliasedPredictors.length == p - report.numericalRank), s"report=$duplicateRankError"),
      ScenarioHarness.fact("rank report resolves aliased structural columns", duplicateStructuralRank.exists(report => report.aliasedColumns.length == 1 && report.independentColumns.length == report.numericalRank), s"structural=$duplicateStructuralRank"),
      ScenarioHarness.fact("duplicate design is rank deficient", sumResult.toOption.exists(_.estimability.designRank < p), s"result=${sumResult.map(_.estimability)}"),
      ScenarioHarness.fact("row-space sum remains estimable", sumResult.isRight, sumResult.fold(_.message, _ => "compiled")),
      ScenarioHarness.fact("individual component is rejected", componentResult.isLeft, componentResult.fold(_.message, _ => "compiled unexpectedly")),
      ScenarioHarness.fact(
        "non-estimability is explicit",
        componentResult.left.exists {
          case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, _) => true
          case _                                                                                       => false
        },
        componentResult.fold(_.message, _ => "missing typed non-estimability")
      ),
      ScenarioHarness.fact("empty cell is not silently invented", !basisRefs(emptyCellSchema).contains(Cells(3)), s"cells=${basisRefs(emptyCellSchema).keys.mkString(",")}"),
      ScenarioHarness.fact(
        "empty-cell hypothesis fails structurally",
        emptyCellResult.left.exists {
          case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.DeclaredEmpty, _) => true
          case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownLevel, _)   => true
          case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.EmptySelection, _) => true
          case _                                                                                         => false
        },
        emptyCellResult.fold(_.message, _ => "empty cell unexpectedly compiled")
      ),
      ScenarioHarness.fact("all-zero modulator is retained as a diagnostic", zeroModulatorSchema.audit.diagnostics.exists(_.kind == DesignDiagnosticKind.DegenerateModulator), s"diagnostics=${zeroModulatorSchema.audit.diagnostics.mkString(";")}"),
      ScenarioHarness.fact("reduced duplicate design is full rank", reducedFit.diagnostics.fullRank && reducedFit.diagnostics.rank == p - 1 && reducedOracle.rank == p - 1, s"ols=${reducedFit.diagnostics.rank}/$p oracle=${reducedOracle.rank}"),
      ScenarioHarness.fact("estimable sum has the reduced-design rank", sumResult.toOption.exists(_.estimability.designRank == reducedFit.diagnostics.rank), s"sum=${sumResult.map(_.estimability.designRank)} reduced=${reducedFit.diagnostics.rank}")
    ) ++ ScenarioHarness.matrix("rank-def fitted values match independent reduced-design oracle", fitted, reducedOracle.fitted, ScenarioTolerance.absolute(1e-8)) ++
      ScenarioHarness.matrix("rank-def residuals match independent reduced-design oracle", residuals, reducedOracle.residuals, ScenarioTolerance.absolute(1e-8)) ++
      ScenarioHarness.matrix("rank-def row-space projection action matches independent oracle", probeProjection, probeOracle.fitted, ScenarioTolerance.absolute(1e-8)) ++
      ScenarioHarness.matrix(
        "rank-def estimable sum function agrees after duplicate-column reduction",
        GaleTestMatrix.fromRows(Vector(reducedFit.coefficients.value.row(0).toVector)),
        GaleTestMatrix.fromRows(Vector(reducedOracle.coefficients.row(0).toVector)),
        ScenarioTolerance.absolute(1e-8)
      )
    ScenarioHarness.result(
      "fit.structural-s16-rank-deficient.v1",
      observations,
      Vector.empty
    )

  private def runWeightedLeastSquaresScenario(): ScenarioResult =
    val fixedWeighting = ModelVolumeWeighting
      .fixed(WlsRFixture.weights)
      .fold(error => fail(error.message), identity)
    val spec = ModelBuildSpec(
      formula = "onset ~ covariate(x) + covariate(z)",
      baselineIntercept = Intercept.Global,
      strategy = FitStrategy.OrdinaryLeastSquares(
        FitControls(volumeWeighting = fixedWeighting)
      )
    )
    val plan = FmriModelBuilder
      .buildPlanEither(wlsReceiptDataset, spec)
      .fold(error => fail(error.message), identity)
    val fitted = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity) match
      case value: DenseFmriFitResult => value
      case other                    => fail(s"expected a dense WLS result, got ${other.engine}")
    val chunked = FitPlanExecutor
      .fitChunked(plan, FitChunkingStrategy.unsafeByVoxelCount(1))
      .fold(error => fail(error.message), identity)
      match
        case value: DenseFmriFitResult => value
        case other                    => fail(s"expected a dense chunked WLS result, got ${other.engine}")
    val schema = plan.model.designSchema.getOrElse(fail("WLS model must expose structural schema"))
    val contrast =
      (sampled("x").coefficient - sampled("z").coefficient)
        .named("s18-x-minus-z", "sampled x minus z")
    val contrastResult = contrast.compile(schema).flatMap(_.evaluate(fitted))
    val design = GaleTestMatrix.fromRows(
      Vector.tabulate(plan.model.designMatrix.rows) { row =>
        Vector.tabulate(plan.model.designMatrix.cols)(column => plan.model.designMatrix(row, column))
      }
    )

    val estimatedSpec = ModelBuildSpec(
      formula = "onset ~ covariate(x)",
      baselineIntercept = Intercept.Global,
      strategy = FitStrategy.OrdinaryLeastSquares(
        FitControls(volumeWeighting = ModelVolumeWeighting.estimatedDvars())
      )
    )
    val estimatedPlan = FmriModelBuilder
      .buildPlanEither(dvarsReceiptDataset, estimatedSpec)
      .fold(error => fail(error.message), identity)
    val estimated = FitPlanExecutor.fit(estimatedPlan).fold(error => fail(error.message), identity) match
      case value: DenseFmriFitResult => value
      case other                    => fail(s"expected a dense estimated-WLS result, got ${other.engine}")
    val estimatedChunked = FitPlanExecutor
      .fitChunked(estimatedPlan, FitChunkingStrategy.unsafeByVoxelCount(1))
      .fold(error => fail(error.message), identity) match
        case value: DenseFmriFitResult => value
        case other                    => fail(s"expected a dense chunked estimated-WLS result, got ${other.engine}")
    val fixedReceipt = fitted.preparationProvenance.flatMap(_.volumeWeighting)
    val estimatedReceipt = estimated.preparationProvenance.flatMap(_.volumeWeighting)
    val observations = Vector(
      ScenarioHarness.fact(
        "public plan carries typed full-series fixed weights",
        fixedReceipt.exists(_.source == VolumeWeightingSource.Fixed(scalafim.fmri.model.FixedWeightAlignment.FullSeries)),
        s"policy=${plan.config.volumeWeighting}"
      ),
      ScenarioHarness.fact(
        "public formula compiles the independently declared design",
        design.toRows == WlsRFixture.design,
        s"actual=${design.toRows}"
      ),
      ScenarioHarness.fact(
        "zero-weight row is excluded from result and residual df",
        fitted.timepoints == WlsRFixture.retainedRows &&
          fitted.residualDegreesOfFreedom.value == WlsRFixture.residualDf &&
          fixedReceipt.exists(receipt => receipt.zeroWeightTimepoints == Vector(2) && receipt.excludedTimepoints == Vector(2)),
        s"timepoints=${fitted.timepoints} df=${fitted.residualDegreesOfFreedom.value} receipt=$fixedReceipt"
      ),
      ScenarioHarness.fact(
        "estimated DVARS provenance records source, scope, normalization, and values",
        estimatedReceipt.exists(receipt =>
          receipt.source == VolumeWeightingSource.ResponseDvars(scalafim.fmri.model.DvarsWeightEstimator()) &&
            receipt.normalization == VolumeWeightNormalization.MeanOne(scalafim.fmri.model.DvarsWeightScope.WithinRun) &&
            receipt.qualityMetric.exists(actual => close(actual, WlsRFixture.dvars, 1e-12)) &&
            close(receipt.weights, WlsRFixture.inverseSquaredWeights, 1e-12)
        ),
        s"receipt=$estimatedReceipt"
      ),
      ScenarioHarness.fact(
        "multiresponse weighted fit retains the independent R rank",
        fitted.olsDiagnostics.exists(_.rank == WlsRFixture.rank),
        s"diagnostics=${fitted.olsDiagnostics}"
      ),
      ScenarioHarness.fact(
        "sampled-regressor contrast compiles without column ids or numeric weights",
        contrastResult.isRight,
        contrastResult.fold(_.message, _ => "compiled")
      ),
      ScenarioHarness.fact(
        "semantic contrast matches base-R estimates, standard errors, and statistics",
        contrastResult.toOption.exists(result =>
          close(result.estimates.toVector, WlsRFixture.contrastEstimate, 1e-10) &&
            close(result.standardErrors.toVector, WlsRFixture.contrastStandardError, 1e-10) &&
            close(result.statistics.toVector, WlsRFixture.contrastStatistic, 1e-10)
        ),
        contrastResult.fold(_.message, result => s"statistics=${result.statistics.toVector}")
      )
    )
    val comparisons =
      ScenarioHarness.matrix(
        "weighted coefficients match base R lm.wfit",
        fitted.coefficients.value,
        GaleTestMatrix.fromRows(WlsRFixture.coefficients),
        ScenarioTolerance.absolute(1e-10)
      ) ++
      ScenarioHarness.matrixMetrics(
        "weighted coefficients match base R lm.wfit",
        fitted.coefficients.value,
        GaleTestMatrix.fromRows(WlsRFixture.coefficients),
        ScenarioComparisonTolerance.bounded(1e-9, 1e-9, 0.999999999, 1e-9)
      ) ++
      ScenarioHarness.matrix(
        "weighted normalized covariance matches direct R crossproduct",
        fitted.normalizedCovariance,
        GaleTestMatrix.fromRows(WlsRFixture.normalizedCovariance),
        ScenarioTolerance.absolute(1e-10)
      ) ++
      ScenarioHarness.matrix(
        "weighted standard errors match base R lm.wfit geometry",
        fitted.standardErrors.value,
        GaleTestMatrix.fromRows(WlsRFixture.standardErrors),
        ScenarioTolerance.absolute(1e-10)
      ) ++
      ScenarioHarness.vector(
        "weighted residual variance matches base R lm.wfit",
        fitted.residualVariance,
        gale.linalg.DVec.fromSeq(WlsRFixture.residualVariance),
        ScenarioTolerance.absolute(1e-10)
      ) ++
      ScenarioHarness.matrix(
        "fixed-weight chunked and complete fits agree",
        chunked.coefficients.value,
        fitted.coefficients.value,
        ScenarioTolerance.absolute(1e-12)
      ) ++
      ScenarioHarness.matrix(
        "estimated-weight chunked and complete fits agree",
        estimatedChunked.coefficients.value,
        estimated.coefficients.value,
        ScenarioTolerance.absolute(1e-12)
      )
    ScenarioHarness.result(
      "fit.structural-s18-wls.v1",
      observations ++ comparisons,
      Vector.empty
    )

  private final case class IndependentFit(
      coefficients: DMat,
      fitted: DMat,
      residuals: DMat,
      rank: Int
  )

  /** A deliberately independent, test-only normal-equation oracle. It is used
    * only after reducing the duplicate column to a full-rank design, so this
    * comparison tests fitted values, residuals, projections, and estimable
    * functions without requiring arbitrary coefficients from a singular solve.
    */
  private def independentFullRankFit(design: DMat, response: DMat): IndependentFit =
    require(design.rows == response.rows, "independent fit rows must align")
    val predictors = design.cols
    val responses = response.cols
    val lhs = Array.fill(predictors * predictors)(0.0)
    val rhs = Array.fill(predictors * responses)(0.0)
    var row = 0
    while row < design.rows do
      var left = 0
      while left < predictors do
        var right = 0
        while right < predictors do
          lhs(left * predictors + right) += design(row, left) * design(row, right)
          right += 1
        var responseColumn = 0
        while responseColumn < responses do
          rhs(left * responses + responseColumn) += design(row, left) * response(row, responseColumn)
          responseColumn += 1
        left += 1
      row += 1
    val coefficients = gaussianSolve(lhs, rhs, predictors, responses)
    val fitted = multiply(design, coefficients)
    IndependentFit(coefficients, fitted, subtract(response, fitted), predictors)

  private def gaussianSolve(
      lhs: Array[Double],
      rhs: Array[Double],
      size: Int,
      responseColumns: Int
  ): DMat =
    val a = lhs.clone
    val b = rhs.clone
    var pivot = 0
    while pivot < size do
      var pivotRow = pivot
      var pivotMagnitude = math.abs(a(pivot * size + pivot))
      var candidate = pivot + 1
      while candidate < size do
        val magnitude = math.abs(a(candidate * size + pivot))
        if magnitude > pivotMagnitude then
          pivotRow = candidate
          pivotMagnitude = magnitude
        candidate += 1
      require(pivotMagnitude > 1e-10, s"independent oracle encountered a singular pivot at $pivot")
      if pivotRow != pivot then
        var column = pivot
        while column < size do
          val index = pivot * size + column
          val swapped = pivotRow * size + column
          val value = a(index)
          a(index) = a(swapped)
          a(swapped) = value
          column += 1
        var responseColumn = 0
        while responseColumn < responseColumns do
          val index = pivot * responseColumns + responseColumn
          val swapped = pivotRow * responseColumns + responseColumn
          val value = b(index)
          b(index) = b(swapped)
          b(swapped) = value
          responseColumn += 1
      var eliminate = pivot + 1
      while eliminate < size do
        val factor = a(eliminate * size + pivot) / a(pivot * size + pivot)
        a(eliminate * size + pivot) = 0.0
        var column = pivot + 1
        while column < size do
          a(eliminate * size + column) -= factor * a(pivot * size + column)
          column += 1
        var responseColumn = 0
        while responseColumn < responseColumns do
          b(eliminate * responseColumns + responseColumn) -= factor * b(pivot * responseColumns + responseColumn)
          responseColumn += 1
        eliminate += 1
      pivot += 1
    var row = size - 1
    while row >= 0 do
      var responseColumn = 0
      while responseColumn < responseColumns do
        var value = b(row * responseColumns + responseColumn)
        var column = row + 1
        while column < size do
          value -= a(row * size + column) * b(column * responseColumns + responseColumn)
          column += 1
        b(row * responseColumns + responseColumn) = value / a(row * size + row)
        responseColumn += 1
      row -= 1
    GaleTestMatrix.fromArray(size, responseColumns, b)

  private def dropColumn(matrix: DMat, dropped: Int): DMat =
    require(dropped >= 0 && dropped < matrix.cols, s"dropped column $dropped is out of bounds")
    GaleTestMatrix.fromRows(
      Vector.tabulate(matrix.rows) { row =>
        Vector.tabulate(matrix.cols - 1) { column =>
          matrix(row, if column < dropped then column else column + 1)
        }
      }
    )

  private def multiply(left: DMat, right: DMat): DMat =
    require(left.cols == right.rows, s"matrix multiplication mismatch: ${left.rows}x${left.cols} * ${right.rows}x${right.cols}")
    GaleTestMatrix.fromRows(
      Vector.tabulate(left.rows) { row =>
        Vector.tabulate(right.cols) { column =>
          var total = 0.0
          var index = 0
          while index < left.cols do
            total += left(row, index) * right(index, column)
            index += 1
          total
        }
      }
    )

  private def subtract(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols, "matrix subtraction mismatch")
    GaleTestMatrix.fromRows(
      Vector.tabulate(left.rows) { row =>
        Vector.tabulate(left.cols) { column => left(row, column) - right(row, column) }
      }
    )

  private def buildPlan(
      dataset: FmriDataset,
      dropEmpty: Boolean = true,
      formula: String = "onset ~ hrf(f1, f2, basis=\"spmg3\", id=\"task\")"
  ) =
    FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = formula,
        baselineIntercept = Intercept.Global,
        defaultHrf = Hrfs.SPMG1,
        dropEmpty = dropEmpty
      )
    )

  private def dataset(
      response: Vector[Vector[Double]],
      omitCell: Option[CellKey] = None,
      zeroModulator: Boolean = false,
      includeCells: Option[Set[CellKey]] = None
  ): FmriDataset =
    val onsets = Vector(2.0, 12.0, 22.0, 32.0, 42.0, 52.0, 62.0, 70.0)
    val f1 = Vector("A", "B", "A", "B", "A", "B", "A", "B")
    val f2 = Vector("X", "X", "Y", "Y", "X", "X", "Y", "Y")
    val eventCells = onsets.indices.map(i => CellKey.unsafe(Seq(
      CellAssignment(F1, LevelId.unsafe(f1(i))),
      CellAssignment(F2, LevelId.unsafe(f2(i)))
    )))
    val included = onsets.indices.filter { i =>
      omitCell.forall(_ != eventCells(i)) && includeCells.forall(_.contains(eventCells(i)))
    }
    val events = DatasetEvents(
      included.toVector.map(i => Map(
        "onset" -> onsets(i).toString,
        "f1" -> f1(i),
        "f2" -> f2(i),
        "rt" -> (if zeroModulator then "0.0" else (i + 1).toDouble.toString)
      ))
    )
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(response),
        SampleSpaces(Vector(2, 1, 1))
      ),
      Sampling,
      events
    )

  private def datasetThreeByThree(response: Vector[Vector[Double]]): FmriDataset =
    val levels1 = Vector("A", "B", "C")
    val levels2 = Vector("X", "Y", "Z")
    val rows =
      for
        first <- levels1
        second <- levels2
        repetition <- 0 until 2
      yield Map(
        "onset" -> (2.0 + rowsIndex(first, second, repetition) * 4.0).toString,
        "f1" -> first,
        "f2" -> second,
        "rt" -> (1.0 + repetition).toString
      )
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level-3x3"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(response),
        SampleSpaces(Vector(2, 1, 1))
      ),
      Sampling,
      DatasetEvents(rows.toVector)
    )

  private def factorModulatorDataset(response: Vector[Vector[Double]], missingRt: Boolean): FmriDataset =
    val conditions = Vector("A", "A", "A", "A", "B", "B", "B", "B")
    val rt = Vector(1.0, 3.0, 5.0, 7.0, 2.0, 4.0, 6.0, 8.0)
    val rows = conditions.indices.map { index =>
      val values = Map[DatasetFieldId, DatasetValue](
        DatasetFieldId.unsafe("onset") -> DatasetValue.Number(2.0 + index.toDouble * 8.0, Some((2.0 + index.toDouble * 8.0).toString)),
        DatasetFieldId.unsafe("condition") -> DatasetValue.Text(conditions(index)),
        DatasetFieldId.unsafe("rt") -> (if missingRt && index == 3 then DatasetValue.Number(Double.NaN, Some("NaN")) else DatasetValue.Number(rt(index), Some(rt(index).toString)))
      )
      DatasetEventRow.unsafe(values)
    }
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level-factor-modulator"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(response),
        SampleSpaces(Vector(2, 1, 1))
      ),
      Sampling,
      DatasetEvents.unsafeTyped(rows.toVector)
    )

  private def wlsReceiptDataset: FmriDataset =
    val events = DatasetEvents(
      WlsRFixture.design.indices.toVector.map { row =>
        Map(
          "onset" -> row.toString,
          "x" -> WlsRFixture.design(row)(0).toString,
          "z" -> WlsRFixture.design(row)(1).toString
        )
      }
    )
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level-wls"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(WlsRFixture.response),
        SampleSpaces(Vector(2, 1, 1))
      ),
      scalafim.fmri.hrf.design.SamplingFrame(blockLens = Seq(WlsRFixture.response.length), tr = Seq(1.0)),
      events
    )

  private def dvarsReceiptDataset: FmriDataset =
    val events = DatasetEvents(
      WlsRFixture.dvarsResponse.indices.toVector.map { row =>
        Map(
          "onset" -> row.toString,
          "x" -> row.toString
        )
      }
    )
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level-estimated-volume-weights"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(WlsRFixture.dvarsResponse),
        SampleSpaces(Vector(3, 1, 1))
      ),
      scalafim.fmri.hrf.design.SamplingFrame(blockLens = Seq(WlsRFixture.dvarsResponse.length), tr = Seq(1.0)),
      events
    )

  private def heterogeneousDataset(response: Vector[Vector[Double]]): FmriDataset =
    val levels = Vector("A", "B", "C")
    val onsets = Vector(2.0, 12.0, 22.0, 32.0, 42.0, 52.0, 62.0, 70.0, 76.0)
    val events = DatasetEvents(
      onsets.zipWithIndex.map { case (onset, index) =>
        Map(
          "onset" -> onset.toString,
          "condition" -> levels(index % levels.length),
          "trial" -> s"trial-${index + 1}"
        )
      }
    )
    FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("structural-first-level-heterogeneous-hrf"),
        scalafim.fmri.fit.GaleTestMatrix.fromRows(response),
        SampleSpaces(Vector(2, 1, 1))
      ),
      Sampling,
      events
    )

  private def rowsIndex(first: String, second: String, repetition: Int): Int =
    (Vector("A", "B", "C").indexOf(first) * 3 + Vector("X", "Y", "Z").indexOf(second)) * 2 + repetition

  private def synthesize(model: FmriModel): Vector[Vector[Double]] =
    val schema = model.designSchema.getOrElse(fail("synthesis requires a structural schema"))
    val matrix = model.designMatrix
    val beta = schema.columns.map {
      case column if column.origin.isInstanceOf[StructuralColumnOrigin.Intercept] => 0.35
      case column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, Some(basis), _, _) =>
            val cellEffect =
              cell match
                case value if value == Cells(0) => 1.2
                case value if value == Cells(1) => 0.6
                case value if value == Cells(2) => 0.9
                case value if value == Cells(3) => 0.2
                case _ => 0.0
            basis.index.oneBased match
              case 1 => cellEffect
              case 2 => 0.15 * cellEffect
              case _ => -0.1 * cellEffect
          case _ => 0.0
    }
    Vector.tabulate(matrix.rows) { row =>
      val signal =
        var total = 0.0
        var column = 0
        while column < matrix.cols do
          total += matrix(row, column) * beta(column)
          column += 1
        total
      Vector(
        signal + 0.03 * math.sin(row.toDouble * 0.37),
        1.1 * signal + 0.03 * math.cos(row.toDouble * 0.29)
      )
    }

  private def synthesizeThreeByThree(model: FmriModel): Vector[Vector[Double]] =
    val schema = model.designSchema.getOrElse(fail("3x3 synthesis requires a structural schema"))
    val matrix = model.designMatrix
    val firstScores = Map("A" -> -2.0, "B" -> 0.5, "C" -> 4.0)
    val secondScores = Map("X" -> -1.0, "Y" -> 3.0, "Z" -> 7.0)
    def cellEffect(cell: CellKey): Double =
      val first = firstScores(cell.get(F1).map(_.value).get)
      val second = secondScores(cell.get(F2).map(_.value).get)
      0.2 + 0.4 * first + 0.15 * first * first + 0.3 * second + 0.05 * second * second + 0.08 * first * second
    val beta = schema.columns.map {
      case column if column.origin.isInstanceOf[StructuralColumnOrigin.Intercept] => 0.1
      case column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) => cellEffect(cell)
          case _ => 0.0
    }
    Vector.tabulate(matrix.rows) { row =>
      var signal = 0.0
      var column = 0
      while column < matrix.cols do
        signal += matrix(row, column) * beta(column)
        column += 1
      Vector(signal, 1.2 * signal)
    }

  private def synthesizeFactorModulator(model: FmriModel): Vector[Vector[Double]] =
    val schema = model.designSchema.getOrElse(fail("factor/modulator synthesis requires a structural schema"))
    val matrix = model.designMatrix
    val beta = schema.columns.map { column =>
      column.origin match
        case StructuralColumnOrigin.Intercept(_) => 0.2
        case StructuralColumnOrigin.Event(term, _, cell, modulator, _, _, _) if term.value == "condition" =>
          if cell.get(Condition).exists(_.value == "A") then 1.0 else 0.5
        case StructuralColumnOrigin.Event(term, _, cell, modulator, _, _, _) if term.value == "withinSlope" =>
          if cell.get(Condition).exists(_.value == "A") then 0.2 else 0.4
        case StructuralColumnOrigin.Event(term, _, _, modulator, _, _, _) if term.value == "sharedSlope" => 0.15
        case _ => 0.0
    }
    Vector.tabulate(matrix.rows) { row =>
      var signal = 0.0
      var column = 0
      while column < matrix.cols do
        signal += matrix(row, column) * beta(column)
        column += 1
      Vector(signal, 1.1 * signal)
    }

  private def continuousValues(model: FmriModel, key: String): Vector[Double] =
    model.eventModel.terms.collectFirst {
      case (`key`, term: ConvolvedTerm) =>
        term.term.events.collectFirst { case event: ContinuousEvent => event.value.data.toVector }.getOrElse(Vector.empty)
    }.getOrElse(fail(s"continuous term '$key' missing"))

  private def synthesizeHeterogeneous(model: FmriModel): Vector[Vector[Double]] =
    val schema = model.designSchema.getOrElse(fail("heterogeneous synthesis requires a structural schema"))
    val matrix = model.designMatrix
    def effect(cell: CellKey, basisIndex: Int): Double =
      cell.get(Condition).map(_.value) match
        case Some("A") => if basisIndex == 1 then 0.8 else 0.0
        case Some("B") => if basisIndex == 1 then 1.2 else 0.18
        case Some("C") => if basisIndex == 1 then -0.4 else -0.12
        case _ => 0.0
    val beta = schema.columns.map {
      case column if column.origin.isInstanceOf[StructuralColumnOrigin.Intercept] => 0.2
      case column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, Some(basis), _, _) => effect(cell, basis.index.oneBased)
          case _ => 0.0
    }
    Vector.tabulate(matrix.rows) { row =>
      var signal = 0.0
      var column = 0
      while column < matrix.cols do
        signal += matrix(row, column) * beta(column)
        column += 1
      Vector(signal, 1.25 * signal)
    }

  private def basisRefs(schema: DesignSchema): Map[CellKey, Vector[BasisElementRef]] =
    schema.columns.flatMap { column =>
      column.origin match
        case StructuralColumnOrigin.Event(_, _, cell, _, Some(basis), _, _) => Some(cell -> basis)
        case _ => None
    }.groupMap(_._1)(_._2)

  private def close(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Boolean =
    actual.length == expected.length && actual.zip(expected).forall { case (left, right) =>
      math.abs(left - right) <= tolerance
    }
