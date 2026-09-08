# First-level fMRI analysis

ScalaFIM treats a first-level analysis as a staged program. The event table and
model policy compile into an inspectable `FitPlan`; fitting consumes that plan;
semantic hypotheses compile against the resulting `DesignSchema`. This keeps
scientific identity visible before numerical work begins.

The example below is the delayed-match-to-sample acceptance scenario used by
the JVM and Scala.js test suites. Its Scala blocks are extracted from the
compiled test source, not copied by hand. Run the documentation check after
editing either file:

```sh
python -S tools/docs/check_first_level_docs.py --check
```

## 1. Declare one trial table

Start with one row per parent trial. A multiphase task can keep separate onset
and duration columns for sample, delay, and probe while sharing trial identity,
run, condition, and behavioral columns. Construct an `FmriDataset` from those
events, a `SamplingFrame`, and an image backend. The example below assumes that
value is named `dataset`.

The parent-trial column is not decorative metadata. Each `hrf` term names its
phase and parent column, so phase provenance survives subsetting, missing-value
handling, design compilation, fitting, and hypothesis evaluation.

## 2. Validate policy and compile a plan

Factor levels are an explicit scientific declaration, including declared but
unobserved cells. Their smart constructor returns `Either`; the ordinary model
program carries that validation into `buildPlanEither` instead of throwing or
guessing levels from the current sample.

<!-- BEGIN extracted:first-level-model -->
```scala
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
```
<!-- END extracted:first-level-model -->

This one model owns the consequential choices:

- `Intercept.Runwise` gives each run its own baseline intercept.
- `SeparateRunsThenFixedEffects` estimates runs independently and combines
  them on a common coefficient axis.
- `RetainZero` preserves declared empty cells in the structural audit rather
  than silently changing the factorial model.
- `ZeroContribution` retains a trial with missing RT while assigning zero to
  that modulator contribution. The audit records the affected parent and
  source row.
- `center_within(rt, match, load)` centers RT within the named factorial cells;
  it does not rely on a separately prepared ad-hoc column.

Before fitting, inspect `plan.model.designSchema`. Its columns expose term,
phase, cell, modulator, basis element, role, and run scope as typed structure.
Its audit contains phase provenance, factor-level resolution, empty-cell,
missing-value, and centering receipts. Treat an unexpected schema or audit as a
model error, not as something to diagnose from rendered column names later.

## 3. Fit the inspected plan

For selected betas, FIR coefficients and contrasts without full-fit products,
use the [selected-estimates guide](selected-estimates.md). It explains bounded
execution, optional uncertainty and the actual work required by run pooling.

After accepting the plan, call `FitPlanExecutor.fit(plan)`. The strategy in the
plan determines the engine and coefficient scope. The DMS scenario requires a
`FixedEffectsFmriFitResult` and checks each run's predictor count and residual
degrees of freedom before evaluating hypotheses.

Planning and fitting remain separate on purpose. Applications can render or
persist the schema and policy receipts before spending time on a large response
matrix, and repeated response blocks can reuse the accepted plan.

## 4. State hypotheses in scientific terms

Hypotheses select terms, phases, factorial cells, modulators, basis roles, and
response functionals. They do not select integer column offsets or parse
rendered column labels.

<!-- BEGIN extracted:first-level-hypotheses -->
```scala
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
```
<!-- END extracted:first-level-hypotheses -->

`response` asks a basis to evaluate a scientifically named functional, such as
the response at six seconds or a mean over a window. `coefficient` selects a
semantic basis role when the coefficient itself is the target. `omnibus`
constructs a basis-aware F hypothesis; `BasisScope.NonCanonical` asks whether
the non-canonical shape dimensions jointly contribute.

Compile each hypothesis against the inspected schema, then evaluate it against
the fit result. Compilation checks selection, basis identity, rank, and
estimability. In the example, the declared but unobserved mismatch-high cell is
expected to fail through the typed non-estimability path; it is never treated
as an all-zero successful contrast.

## Evidence and current limits

The DMS scenario checks the exact 2 + 24 + 12 + 12 task-column structure,
phase/parent/source-row provenance, empty and missing-cell receipts, semantic T
and F hypotheses, separate-run fixed effects, and independent R fixture
statistics on both supported runtimes. The broader scenario manifest also
tracks R and Nilearn provenance and accepted cross-system differences.

ScalaFIM remains `0.1-development` research software. The public surface does
not yet carry a binary-compatibility promise. Passing the focused correctness,
coverage, documentation, and performance courts is necessary release evidence;
it does not replace review of the scientific policy, a clean checkout, or
remote CI and branch-protection verification. See
[`release-assurance.md`](release-assurance.md) for the exact courts and
[`benchmarks/first-level.md`](benchmarks/first-level.md) for performance scope
and receipts.
