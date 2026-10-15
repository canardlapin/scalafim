package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.dataset.{DatasetEventRow, DatasetEvents, DatasetFieldId, DatasetId, DatasetValue, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.{DesignError, MissingValuePolicy}
import scalafim.fmri.design.baseline.{BaselineBasis, DctCutoffPeriod, Intercept}
import scalafim.fmri.design.formula.{DerivedEventPlan, DerivedMissingRows, FormulaParser}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.SampleSpaces

/** A printed formula, a portable build spec and a saved model document are
  * second views of one model: each must rebuild the design the original spec
  * builds, with the same column ids, fingerprint and values.
  *
  * The corpus covers the five model-studio studies (block, mixed gambles,
  * delayed match-to-sample phases, stop-signal FIR, 3x4 factorial with a
  * three-column basis), LSA and LSS trialwise designs, every
  * [[MissingValuePolicy]] case at the build-spec level and every formula
  * `modulator(missing = ...)` spelling. Event tables are deterministic
  * arithmetic sequences over two runs; nothing is random.
  */
class RebuildIdentityCorpusSuite extends munit.FunSuite:
  private final case class Study(name: String, spec: ModelBuildSpec, dataset: FmriDataset, eventColumns: Int)

  private val tr = 2.0

  private def num(value: Double): DatasetValue = DatasetValue.Number(value)
  private def text(value: String): DatasetValue = DatasetValue.Text(value)
  private def runId(run: Int): DatasetValue = DatasetValue.Text(s"run-$run")
  private def row(fields: (String, DatasetValue)*): DatasetEventRow =
    DatasetEventRow.unsafe(fields.map((name, value) => DatasetFieldId(name) -> value).toMap)

  private def dataset(name: String, scansPerRun: Int, rows: Vector[DatasetEventRow]): FmriDataset =
    val frame = SamplingFrame(blockLens = Seq(scansPerRun, scansPerRun), tr = Seq(tr, tr))
    val scans = 2 * scansPerRun
    val data = Matrix.dense(scans, 1, Vector.tabulate(scans)(i => math.sin(0.37 * i)))
    val events = DatasetEvents.fromTypedRows(rows).fold(error => fail(error.message), identity)
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId(name), data, SampleSpaces(Vector(1, 1, 1))),
      samplingFrame = frame,
      events = events
    ).dataset

  private val runs = Vector(1, 2)
  private val dct = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(128))

  // (1) Block design: 12 s blocks of three rotating conditions.
  private val block: Study =
    val conditions = Vector("faces", "houses", "scrambled")
    val rows = runs.flatMap: run =>
      (0 until 6).map: cycle =>
        row("run" -> runId(run), "onset" -> num(6.0 + 28.0 * cycle), "duration" -> num(12.0),
          "condition" -> text(conditions((cycle + run) % 3)))
    Study("block",
      ModelBuildSpec("onset ~ hrf(condition, basis = spmg1, durations = duration, id = blocks)", blockColumn = Some("run"),
        baselineBasis = dct, baselineIntercept = Intercept.Runwise, strategy = FitStrategy.RunwiseLeastSquares()),
      dataset("block", 90, rows), 3)

  // (2) Mixed gambles: one main effect plus centered gain and scaled loss slopes.
  private def gambleRows(withRt: Boolean): Vector[DatasetEventRow] =
    runs.flatMap: run =>
      (0 until 16).map: trial =>
        val fields = Vector(
          "run" -> runId(run), "onset" -> num(4.0 + 14.0 * trial + (trial % 3)), "trial" -> text("gamble"),
          "gain" -> num(10.0 + 2.0 * ((7 * trial + 3 * run) % 11)), "loss" -> num(5.0 + 1.5 * ((5 * trial + run) % 9))
        )
        row((if withRt then fields :+ ("rt" -> num(0.8 + 0.15 * ((3 * trial) % 5))) else fields)*)

  private val gamble: Study =
    Study("mixed-gambles",
      ModelBuildSpec("onset ~ hrf(trial, id = main) + hrf(trial, modulators(center(gain), scale(loss)), id = slopes)",
        blockColumn = Some("run"), baselineBasis = BaselineBasis.Poly, baselineDegree = 2, baselineIntercept = Intercept.Runwise,
        strategy = FitStrategy.RunwiseGeneralizedLeastSquares(AutocorrelationConfig.unsafe(order = 1, iterations = 1))),
      dataset("gamble", 120, gambleRows(withRt = true)), 3)

  // (2b) Mixed gambles through a derived expected-value column. The portable
  // build spec cannot carry derived declarations; the model document can.
  private val gambleDerived: Study =
    val derived = DerivedEventPlan.parse(Vector("ev: number = (gain - loss) / 2"), DerivedMissingRows.Drop)
      .fold(error => fail(error.message), identity)
    Study("mixed-gambles-derived",
      ModelBuildSpec("onset ~ hrf(trial, modulators(center(ev)), include_main = TRUE, id = value)", blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise, derived = derived),
      dataset("gamble-derived", 120, gambleRows(withRt = false)), 2)

  // (3) Delayed match-to-sample: encoding, delay and probe phases of one trial.
  private val dms: Study =
    val rows = runs.flatMap: run =>
      (0 until 10).map: trial =>
        val encoding = 4.0 + 20.0 * trial
        val delay = encoding + 2.0
        val probe = delay + 6.0 + 2.0 * (trial % 2)
        row("run" -> runId(run), "onset" -> num(encoding), "trial_id" -> text(f"r${run}t$trial%02d"),
          "load" -> text(if (trial + run) % 2 == 0 then "low" else "high"),
          "encoding_onset" -> num(encoding), "encoding_duration" -> num(2.0),
          "delay_onset" -> num(delay), "delay_duration" -> num(probe - delay),
          "probe_onset" -> num(probe))
    val formula =
      "onset ~ hrf(load, onsets = encoding_onset, durations = encoding_duration, phase = encoding, parent = trial_id, id = encoding) + " +
        "hrf(load, onsets = delay_onset, durations = delay_duration, phase = delay, parent = trial_id, id = delay) + " +
        "hrf(load, onsets = probe_onset, phase = probe, parent = trial_id, id = probe)"
    Study("delayed-match-to-sample",
      ModelBuildSpec(formula, blockColumn = Some("run"), baselineBasis = dct, baselineIntercept = Intercept.Runwise),
      dataset("dms", 110, rows), 6)

  // (4) Stop-signal task with an eight-bin FIR basis.
  private val stop: Study =
    val rows = runs.flatMap: run =>
      (0 until 24).map: trial =>
        val condition =
          if trial % 4 != 3 then "go"
          else if (trial / 4 + run) % 2 == 0 then "stop_success"
          else "stop_failure"
        row("run" -> runId(run), "onset" -> num(3.0 + 7.5 * trial), "condition" -> text(condition))
    Study("stop-signal-fir",
      ModelBuildSpec("onset ~ hrf(condition, basis = fir, nbasis = 8, span = 16, id = stop)", blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise, strategy = FitStrategy.SeparateRunsThenFixedEffects()),
      dataset("stop", 100, rows), 24)

  // (5) 3x4 factorial: twelve cells, each seen twice per run, with SPMG3.
  private val factorial: Study =
    val load = Vector("low", "mid", "high")
    val cue = Vector("w", "x", "y", "z")
    val rows = runs.flatMap: run =>
      (0 until 24).map: trial =>
        val cell = (5 * trial + run) % 12
        row("run" -> runId(run), "onset" -> num(2.0 + 6.0 * trial + 0.5 * (trial % 3)),
          "load" -> text(load(cell / 4)), "cue" -> text(cue(cell % 4)))
    Study("factorial-3x4-spmg3",
      ModelBuildSpec("onset ~ hrf(load, cue, basis = spmg3, id = factorial)", blockColumn = Some("run"),
        baselineBasis = BaselineBasis.Poly, baselineDegree = 3, baselineIntercept = Intercept.Runwise),
      dataset("factorial", 80, rows), 36)

  private val trialRows: Vector[DatasetEventRow] =
    runs.flatMap: run =>
      (0 until 8).map: trial =>
        row("run" -> runId(run), "onset" -> num(6.0 + 13.0 * trial), "trial" -> text(s"tr$trial"),
          "response" -> text(if (trial + run) % 3 == 0 then "left" else "right"))

  private val lsa: Study =
    Study("lsa",
      ModelBuildSpec("onset ~ trialwise(id = trial, label = lsa)", blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise, strategy = FitStrategy.OrdinaryLeastSquares()),
      dataset("lsa", 60, trialRows), 16)

  private val lss: Study =
    Study("lss",
      ModelBuildSpec("onset ~ trialwise(id = trial, label = lss) + hrf(response, id = motor)", blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise,
        strategy = FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = None, eps = 1e-10, rankTol = 1e-5))),
      dataset("lss", 60, trialRows), 18)

  // A gain modulator with two missing values (one per run) for the n/a policies.
  private def policyRows(missing: Boolean): Vector[DatasetEventRow] =
    runs.flatMap: run =>
      (0 until 10).map: trial =>
        val gain = if missing && trial == 3 + run then Double.NaN else 1.0 + ((3 * trial + run) % 7)
        row("run" -> runId(run), "onset" -> num(4.0 + 11.0 * trial), "trial" -> text("choice"), "gain" -> num(gain))

  private def specPolicy(policy: MissingValuePolicy, label: String): Study =
    val missing = policy != MissingValuePolicy.Reject
    Study(s"spec-missing-$label",
      ModelBuildSpec("onset ~ hrf(trial, modulators(gain), include_main = TRUE, id = slopes)", blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise, missingValuePolicy = policy),
      dataset(s"spec-$label", 60, policyRows(missing)), 2)

  private def formulaPolicy(spelling: String): Study =
    Study(s"formula-missing-$spelling",
      ModelBuildSpec(s"onset ~ hrf(trial, modulators(modulator(gain, center = run, scale = raw, missing = $spelling)), include_main = TRUE, id = slopes)",
        blockColumn = Some("run"), baselineIntercept = Intercept.Runwise),
      dataset(s"formula-$spelling", 60, policyRows(missing = spelling != "reject")), 2)

  // Exhaustive over the enum: a new case fails to compile here until it is covered.
  private def policyLabel(policy: MissingValuePolicy): String =
    policy match
      case MissingValuePolicy.Reject => "reject"
      case MissingValuePolicy.ZeroContribution => "zero"
      case MissingValuePolicy.DropFromTerm => "drop"
      case MissingValuePolicy.ImputeConstant(_) => "impute"

  private val policies = Vector(
    MissingValuePolicy.Reject, MissingValuePolicy.ZeroContribution, MissingValuePolicy.DropFromTerm, MissingValuePolicy.ImputeConstant(2.5)
  )

  private val studies: Vector[Study] =
    Vector(block, gamble, gambleDerived, dms, stop, factorial, lsa, lss) ++
      policies.map(policy => specPolicy(policy, policyLabel(policy))) ++
      Vector("reject", "drop_from_term", "zero").map(formulaPolicy)

  /** Every second view of `spec`, each as the spec it rebuilds. The build-spec
    * JSON path is absent exactly when the spec carries derived declarations,
    * which that schema refuses at `$.derived`.
    */
  private def views(spec: ModelBuildSpec): Vector[(String, ModelBuildSpec)] =
    val formula = FormulaParser.parseEither(spec.formula).fold(error => fail(error.message), identity)
    val printed = formula.textEither.fold(error => fail(error.message), identity)
    assertEquals(FormulaParser.parseEither(printed), Right(formula), clue = printed)
    val printedSpec = spec.copy(formula = printed)

    val buildJson = ModelBuildSpecJsonCodec.encode(spec) match
      case Right(json) =>
        val decoded = ModelBuildSpecJsonCodec.decode(json).fold(error => fail(error.message), identity)
        assertEquals(decoded, spec)
        Vector("build-spec json" -> decoded)
      case Left(error) =>
        assert(spec.derived != DerivedEventPlan.empty, s"unexpected build-spec refusal: ${error.message}")
        assertEquals(error.path, "$.derived")
        Vector.empty

    val document = ModelDocument(spec)
    val documentJson = ModelDocumentJsonCodec.encode(document).flatMap(ModelDocumentJsonCodec.decode)
      .fold(error => fail(error.message), identity)
    assertEquals(documentJson, document)

    // The text form replaces every piece it carries (formula, derived plan,
    // baseline, confounds, estimation, runs); the rest come from the base.
    val lines = ModelDocumentText.render(document).fold(error => fail(error.message), identity)
    val base = ModelDocument(spec.copy(formula = "onset ~ trialwise()", derived = DerivedEventPlan.empty,
      baselineBasis = BaselineBasis.Constant, baselineDegree = 1, baselineIntercept = Intercept.Global, strategy = FitStrategy.Default))
    val documentText = ModelDocumentText.parse(lines, base).fold(error => fail(error.message), identity)
    assertEquals(documentText, document)

    val printedJson = buildJson.map: _ =>
      "printed formula via build-spec json" -> ModelBuildSpecJsonCodec.encode(printedSpec).flatMap(ModelBuildSpecJsonCodec.decode)
        .fold(error => fail(error.message), identity)

    Vector("original" -> spec, "printed formula" -> printedSpec) ++ buildJson ++ printedJson ++
      Vector("document json" -> documentJson.build, "document text" -> documentText.build)

  private def build(study: Study, view: String, spec: ModelBuildSpec): FmriModel =
    FmriModelBuilder.buildModelEither(study.dataset, spec).fold(error => fail(s"${study.name} [$view]: ${error.message}"), identity)

  studies.foreach: study =>
    test(s"${study.name}: every second view rebuilds the identical design"):
      val original = build(study, "original", study.spec)
      assertEquals(original.eventModel.designMatrix.cols, study.eventColumns, clue = original.eventModel.columnNames)
      val expectedFingerprint = original.designFingerprint.getOrElse(fail(s"${study.name}: no design fingerprint"))
      val expected = original.designMatrix
      views(study.spec).foreach: (view, spec) =>
        val rebuilt = build(study, view, spec)
        assertEquals(rebuilt.designBlock.columnIds, original.designBlock.columnIds, clue = view)
        assertEquals(rebuilt.columnNames, original.columnNames, clue = view)
        assertEquals(rebuilt.designFingerprint, Some(expectedFingerprint), clue = view)
        assertEquals(rebuilt.eventModel.designSchema.fingerprint, original.eventModel.designSchema.fingerprint, clue = view)
        val actual = rebuilt.designMatrix
        assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols), clue = view)
        actual.data.indices.foreach: i =>
          assertEqualsDouble(actual.data(i), expected.data(i), 1e-12, clue = s"${study.name} [$view] value $i")

  test("the missing-value policies are not vacuous: each repairs the same events differently"):
    val repaired = Vector(MissingValuePolicy.ZeroContribution, MissingValuePolicy.DropFromTerm, MissingValuePolicy.ImputeConstant(2.5))
      .map(policy => specPolicy(policy, policyLabel(policy)))
    val fingerprints = repaired.map(study => build(study, "original", study.spec).designFingerprint)
    assertEquals(fingerprints.distinct.length, repaired.length)
    val formula = Vector("drop_from_term", "zero").map(formulaPolicy)
    assertEquals(formula.map(study => build(study, "original", study.spec).designFingerprint).distinct.length, 2)

  test("the reject policy refuses missing modulator values identically on every view"):
    Vector(
      Study("spec-reject-na", specPolicy(MissingValuePolicy.Reject, "reject").spec, dataset("spec-reject-na", 60, policyRows(missing = true)), 2),
      formulaPolicy("reject").copy(dataset = dataset("formula-reject-na", 60, policyRows(missing = true)))
    ).foreach: study =>
      val messages = views(study.spec).map: (view, spec) =>
        FmriModelBuilder.buildModelEither(study.dataset, spec).fold(_.message, _ => fail(s"${study.name} [$view] accepted missing values"))
      assertEquals(messages.distinct.length, 1, clue = messages)

  test("basis orthogonalization refuses per-event peak scales with a pinned message on every view"):
    val rows = runs.flatMap: run =>
      (0 until 6).map: trial =>
        row("run" -> runId(run), "onset" -> num(5.0 + 15.0 * trial), "duration" -> num(if trial % 2 == 0 then 0.0 else 3.0),
          "condition" -> text(if (trial + run) % 2 == 0 then "a" else "b"))
    val study = Study("unit-peak-orthogonal",
      ModelBuildSpec("onset ~ hrf(condition, basis = spmg2, durations = duration, event_normalization = \"unit-peak\", orthogonalize_basis = TRUE, id = task)",
        blockColumn = Some("run")),
      dataset("unit-peak-orthogonal", 60, rows), 4)
    val detail = "basis orthogonalization does not yet support per-event peak scales"
    views(study.spec).foreach: (view, spec) =>
      FmriModelBuilder.buildModelEither(study.dataset, spec) match
        case Left(error @ ModelError.DesignFailure(DesignError.InvalidSchema(actual))) =>
          assertEquals(actual, detail, clue = view)
          assertEquals(error.message, s"Invalid design schema: $detail", clue = view)
        case other => fail(s"[$view] expected the per-event peak refusal, got $other")
    // The same declaration without orthogonalization builds, so the refusal is
    // the orthogonalization's and not the normalization's.
    val unorthogonal = study.spec.copy(formula = study.spec.formula.replace(", orthogonalize_basis = TRUE", ""))
    assert(FmriModelBuilder.buildModelEither(study.dataset, unorthogonal).isRight)
