# Model authoring APIs

These APIs live in the portable `hrf`, `design`, `model`, and `fit` modules. They expose an
inspectable compiled design, source-preserving event summaries, and versioned
model specifications. Builders return `Either` where a request can be rejected;
renderers return recipes or scenes rather than a UI.

Scientific conventions are caller choices. No application selects library
defaults: event preprocessing, missing-value treatment, response normalization,
noise models, and run combination remain separate, inspectable operations.

## Review a compiled design

`DesignReview.forRun` makes one run and a selected set of source scan rows
explicit. `RunwiseFit` uses the runwise coefficient axis; `SourceColumns` keeps
the source design axis.

```scala
import scalafim.fmri.design.*
import scalafim.fmri.hrf.design.SamplingFrame

val reviewed = DesignReview.forRun(
  schema, frame, RunIndex.unsafeOneBased(1), schema.rows.selectedRows,
  DesignReviewScope.RunwiseFit
)
```

The resulting `DesignReview` carries its `fingerprint`, `scans`, structural
`columns`, source-column indices, and the raw/scaled value accessors. A
`ReviewSeries` from `review.trace(columnId)` retains missing samples as
`Option[Double]`; missing observations are never replaced by zero.

`DesignReviewGraphics` compiles renderer-neutral Intaglio plots:

```scala
val linked = for
  review <- reviewed.left.map(_.message)
  matrix <- DesignReviewGraphics.matrixPlot(review, width = 800, height = 350)
    .left.map(_.toString)
  // Select a column after checking that the review is nonempty.
  series <- review.trace(columnId).left.map(_.message)
  trace <- DesignReviewGraphics.tracePlot(
    review.scans, Vector(series), alignWith = matrix.layout
  ).left.map(_.toString)
yield (matrix, trace)
```

`matrixPlot` is a paged, labelled matrix view (`firstColumn`, `columnCount`)
for bounded inspection. It checks positive finite dimensions and a cell budget.
`tracePlot` preserves gaps between observed trace segments. `eventTimelinePlot`
uses the compiled audit's source-event provenance for the selected run.

For a single unpaged image of every column, use `DesignMatrixRaster.build`.
Its raster is time-down: image width is the number of review columns, image
height is the number of review scans, and it produces one nearest-neighbour
image scene while retaining column and scan identities alongside it.

```scala
val raster = reviewed.left.map(_.message).flatMap: review =>
  DesignMatrixRaster.build(review, maximumCells = 2000000).left.map(_.toString)
```

`DesignMatrixRaster` is an image-oriented overview; use `matrixPlot` when
column labels and page selection are needed.

## Inspect a response basis

`ResponseBasisReview.make` samples the native basis and optionally records a
response functional. It validates sampling density, value budget, basis
identities, finite values, and FIR intervals.

```scala
import scalafim.fmri.hrf.*

val basis = ResponseBasisReview.make(Hrfs.SPMG1, samples = 257)
val plot = basis.flatMap(ResponseBasisGraphics.plot)
```

The review exposes the descriptor, sample times, basis preview curves, and an
optional readout receipt. `ResponseBasisGraphics.plotSelected` accepts explicit
`BasisElementId`s when a wide basis needs a smaller displayed subset:

```scala
val selected = basis.flatMap { review =>
  ResponseBasisGraphics.plotSelected(review, review.basisIds.take(4))
}
```

## Incremental formula updates

`EventModelBuilder.prepareIncremental` fixes an `EventDesignRequest` context
and returns an `IncrementalDesign`. Update only the formula with
`updateFormula`, or replace a single term with `replaceTerm`.

```scala
import scalafim.fmri.design.formula.EventModelBuilder.*

val prepared = prepareIncremental(request)
val changed = prepared.flatMap(_.replaceTerm(1, editedTerm))
```

The cache is bounded to that fixed request context. Changing event data,
sampling, policies, or extension implementations requires a new preparation.
Returned model buffers do not own cache storage: mutating an exposed realized
matrix cannot corrupt a later cache hit. `recompiledTerms` and `reusedTerms`
make the update result inspectable.

## Summarize events and trial phases

`EventTrialSummary.forModel` groups retained convolved events by term, cell,
run, and phase. It reports event count plus duration and, where applicable,
parent-relative onset min/median/max summaries.

```scala
val parents = ParentTrialOnsets.validated(Vector(
  ParentTrialOnset(
    ParentTrialKey(RunIndex.unsafeOneBased(1), TrialId.unsafe("trial-1")),
    0.0.s
  )
))
val summary = parents.flatMap(timing => EventTrialSummary.forModel(model, Some(timing)))
```

Phase offsets require caller-supplied `ParentTrialOnsets`, keyed by both run and
trial. Parent onsets are run-relative seconds, on the same clock as that run's
event onsets, not the concatenated global clock. A phase onset is never treated
as evidence of its parent onset. A phase that starts before its parent returns
`EventTrialSummaryError.PhaseBeforeParent`, which also catches parent onsets
given on the global clock for later runs. Missing, duplicate, inconsistent, and
non-finite timing evidence return the typed `EventTrialSummaryError`. Unphased
provenance remains validated but has no parent-relative offset. Summaries are
keyed by the model's unique term key, so two terms sharing a tag are never
pooled.

## Formula text, source locations, and JSON

`ModelFormula.renderEither` produces tokenized formula text only when the
current admitted grammar can round-trip it; `textEither` joins those tokens and
`FormulaPrinter.expressionTextEither` prints one expression. The non-`Either`
forms `render`, `text`, and `FormulaPrinter.expressionText` throw on the same
failures. Formula tokens carry term and argument paths.
`FormulaParser.parsePositioned` additionally returns zero-based, end-exclusive
source spans for parsed nodes.

```scala
import scalafim.fmri.design.formula.*

val positioned = FormulaParser.parsePositioned(
  "onset ~ hrf(condition, basis = fir, nbasis = 4, span = 8)"
)
val text = positioned.map(_.formula.text)
```

`ModelJsonCodec` uses a versioned `scalafim.model-spec` envelope for formulas,
HRF specifications, and contrast specifications:

```scala
val json = ModelJsonCodec.encodeFormula(formula)
val restored = json.flatMap(ModelJsonCodec.decodeFormula)
```

The codec rejects non-finite numbers and runtime callbacks. In particular,
custom selector callbacks and legacy callback contrasts have no portable JSON
representation.

Decoding is strict: objects must contain exactly the expected fields, duplicate
keys are rejected, numbers must be finite, versions must be integers, enum
tokens are case-exact, and identifiers must already be canonical (no trimming).
Each `ModelJsonError` carries the JSON path of the failure. Encoding rejects any
value the decoder could not read back.

Formula text, JSON, and CSV print numbers through `PortableNumber.format`: the
fewest significant digits that read back as the same double, integral values
without a fraction (`4`, not `4.0`), plain notation for decimal exponents in
`[-6, 21)`, and `d.ddde±x` otherwise (`1e-7`, `1e21`); negative zero prints as
`0`. The output is byte-identical on the JVM and Scala.js.

Column-pattern contrasts store `java.util.regex` pattern source. Decoded
patterns are fresh `Regex` instances; compare them by pattern source, not by
`Regex` equality.

`ModelBuildSpecJsonCodec.encode` and `decode` provide the separate versioned
portable build profile for `ModelBuildSpec`:

```scala
import scalafim.fmri.model.ModelBuildSpecJsonCodec

val json = ModelBuildSpecJsonCodec.encode(spec)
val restored = json.flatMap(ModelBuildSpecJsonCodec.decode)
```

The v1 build profile rejects unsupported executable configuration rather than
substituting defaults. It admits the built-in `Hrfs.SPMG1` default HRF and does
not serialize factor-schema bindings or HRF-by-cell/phase assignments.

## HRF formula options

`hrf(...)` accepts finite support separately from scan-space column scaling,
plus kernel normalization and explicit main-effect inclusion:

```text
onset ~ hrf(condition,
  modulators(amplitude),
  basis = fir,
  nbasis = 4,
  span = 8,
  kernel_normalization = unit_peak,
  include_main = TRUE,
  id = task)
```

In the typed AST these are `HrfCall.span: Option[PositiveSeconds]`,
`kernelNormalization: Option[HrfNormalization]`, and
`includeMain: Option[Boolean]`. `include_main` requires a continuous expression or `modulators(...)` family.
`span` must be positive and currently applies to explicitly selected FIR,
B-spline, tent, or Fourier bases; unknown normalization
names are rejected. `kernel_normalization` controls the kernel, while
`scaling` (or compatibility `normalize`) controls realized scan-space columns;
they are distinct choices. `id` and its alias `name` are exclusive; supplying
both is a parse error.

## Missing values, expressions, and derived columns

`EventExpressions.evaluate` returns typed number, text, logical, or missing
values. Every expression is type-checked statically, from column types and
declared derived types, before any row is evaluated; whether an expression is
well-typed never depends on which rows are missing. `typeOf` exposes the static
type. Missing values (NaN in a numeric column, or the portable literal
`missing()`) propagate through arithmetic and comparisons. `&`, `|`, `!`, and
`in` use Kleene three-valued logic: `in` is true when the value equals some
non-missing candidate, otherwise missing if the value or a candidate is
missing, otherwise false. `!` follows R precedence: looser than comparison and
arithmetic, tighter than `&` and `|`, so `!a == b` means `!(a == b)`. The
vector operators `&` and `|` remain aliases for `&&` and `||`. `filter`
requires a statically logical expression and retains only explicit true values.

Leaving the finite range from finite operands (`x / 0`, `log(0)`, `log(-1)`) is
an evaluation error naming the expression path and row, never a missing value.
An infinite source value and a non-finite literal are also errors. `round(x)`
rounds half to even, as R does (`round(2.5)` is `2`); a `digits` argument is
not supported. `ifelse` branches must have one type. Text comparisons check
observed levels by default, with an explicit switch for callers using declared
empty levels.

`DerivedColumn.parse` and `DerivedColumn.from(id, valueType, expression)` return
`Either[DerivedEventError, DerivedColumn]`. Both reject an expression that the
admitted grammar cannot print losslessly (for example a non-finite numeric
literal), so `text` is total and `DerivedColumn.parse(c.text)` restores `c`.
`parse` accepts declarations such as:

```text
net: number = gain - loss
category: text = ifelse(net > 0, "positive", "nonpositive")
band: text = cut(net, c(-Inf, 0, Inf), c("negative", "nonnegative"))
```

Evaluate declarations in order with `DerivedEvents.evaluate`; overwrites and
forward references fail, and a declared type that differs from the inferred
type returns `DerivedEventError.DeclaredType`. Materialize only the columns
needed by a term and choose `DerivedMissingRows.Reject`, `Drop`, or
`PreserveNumeric`. The result retains original row indices and exclusions.
Missing text is never encoded as a factor level. Numeric missing values can be
preserved for a subsequent modulator policy.

`EventBins.quantiles` uses observed finite values and type-7 quantiles. It rejects
repeated cut points instead of silently merging bins. `cut_quantiles(x, k)` is
also available in expressions. The caller supplies the population: for term
bins, select that term's events before pooling over runs. `breaksText` records
explicit left-closed, right-open cuts; missing values receive no bin.

`EventOnsetSelection.select` is an explicit preprocessing operation, applied
before an event schedule is constructed. It takes run-relative onsets as
`Vector[Option[Seconds]]` (`None` marks a missing onset), their runs, the
sampling frame, and an `EventOnsetPolicy` (`RejectInvalid` or
`DropOutsideRun`). It reports missing, negative, and at-or-after-run-end onsets,
returning retained source rows or a typed rejection. This preserves the
existing ability to model pre-run events when that is the caller's intended
design.

```scala
val selection = EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.DropOutsideRun)
val audited = selection.map: s =>
  audit.copy(excludedEvents = audit.excludedEvents ++ s.auditEntries(Some(term)))
```

Nothing records dropped rows automatically: they reach
`DesignAudit.excludedEvents` only through `auditEntries`.

The formula wrapper below uses observed-only centering and sample-SD scaling
within each run, after term subsetting:

```text
onset ~ hrf(condition,
  modulators(modulator(rt, center = cell, scale = z, missing = zero)),
  include_main = TRUE)
```

`center` accepts `none`, `run`, or `cell`; `scale` accepts `raw`, `z`, or `sd`.
`sd` divides by sample SD without implicitly subtracting the mean. `z` with
`center = none` centres globally within the run; the receipt records
`effective-center`. With `center = cell`, the scale is the pooled within-cell SD
`sqrt(SS_within / (n - k))` over `k` cells. Each run records an
`observed-modulator` policy receipt, plus `observed-modulator-degenerate` when
fewer than two values are observed or the prepared values do not vary. "Do not
vary" uses a relative tolerance: a scale estimate at most `n · ε · max|x|`
(`ε = 2^-52`, `n` observed values) is treated as zero, so a constant that is not
exactly representable (for example `0.3` repeated) is degenerate rather than
scaled by its rounding residue. Missing
policies are `reject`, `drop_from_term`, and `zero`. Dropping applies to the whole
paired term before centering. Zero contribution is applied after centering and
requires centering (z-scoring supplies it when needed). Degenerate scales have
explicit receipts. `scale_within_run(x)` and `sd_within_run(x)` are concise
reject-on-missing forms; legacy `scale` and `scalewithin` keep their existing
contracts.

## Modulator and response conventions

`orthogonalize = TRUE` follows SPM's parametric-modulator convention within each
run × realized cell group, before convolution: the first modulator is
mean-centred, and each later modulator is residualized against an intercept and
every earlier modulator. Means and fits use observed rows only: rows zero-filled
under `missing = zero` are excluded from the group means and from each
modulator's residualization, and stay at the centering reference (0), so a
missing event carries no modulation. (A predecessor's zero-filled rows enter a
later modulator's fit at that predecessor's reference.) It requires an explicit
term ID and records a `formula-orthogonalization` receipt with
`rows=observed-only;zero-filled=held-at-reference` and, per group, the removed
first-modulator mean and the number of observed rows behind it. The typed `ModulatorOrthogonalization`
build option is a separate policy: it keeps its declared scope and residualizes
against earlier modulators only, without an intercept.
`product(center_within_run(gain), center_within_run(loss))` is accepted inside
`modulators`; its component centering occurs after subset and term-wide drop
selection. `shared_slopes = TRUE` with `include_main = TRUE` keeps split
categorical main effects and one common slope per modulator.

```text
onset ~ hrf(condition,
  modulators(gain, loss,
    product(center_within_run(gain), center_within_run(loss))),
  include_main = TRUE, shared_slopes = TRUE, id = task)
```

For SPMG2/3, `temporal_derivative = "analytic"` declares the continuous
derivative `dh/dt`. `temporal_derivative = "spm-1s"` declares SPM12's informed
basis (`spm_get_bf` followed by `spm_orth`): the canonical kernel and the
one-second backward difference of two separately sum-normalized kernels (for
SPMG3, also the 0.01 dispersion difference), serially orthogonalized without
mean removal on SPM's kernel grid, `dt = TR / 16` over `[0, 32]` seconds. The
kernels equal SPM's up to one common scale and are orthogonal on that grid. The
realized basis is compactly supported on SPM's window and its convolution span
is the grid length (32 s), not the canonical's 24 s span: design columns keep
the full 0–32 s kernel, including the temporal and dispersion tails past 25 s,
and are zero beyond it. The
grid needs one repetition time across runs; mixed TRs are rejected. The typed
forms are `TemporalDerivativeConvention.derive(canonical, convention, grid)`
with an `SpmKernelGrid`, and `spmInformedBasis`. The un-orthogonalized
`h(t) - h(t - 1 s)` is available only as
`TemporalDerivativeConvention.rawOneSecondDifference`; it is not SPM's design
column. The third SPMG3 component remains the dispersion derivative.

`event_normalization = "unit-peak"` divides each event's response, separately
for each basis column, by that column's peak absolute pulse response on the
declared reference grid (`event_peak_step`, default 0.1 seconds). This matches
fmrihrf `block_hrf(normalize = TRUE)`. Convolution itself keeps the build
precision, span truncation, and onset window. Event unit-peak differs from
kernel `unit_peak`, which divides every basis column by one factor taken from
the canonical column; the two are not interchangeable for multi-column bases.
Both are distinct from final column scaling. `event_normalization =
"as-convolved"` is identical to omitting the option: the same matrix, no
per-event scales, no policy receipt, and therefore the same audit text and
`DesignFingerprint`. `ConvolvedTerm.eventPeakScales` retains per-event duration
and divisor receipts; `DesignAudit.eventResponseScales` records, per run, the
distinct divisors of the events that reach each realized column's scans in that
run. An event whose response window `[onset, onset + duration + span]` meets
none of its run's scan times (for example an onset after the run's last scan)
contributes nothing and is not counted. The typed `EventResponse` and
`EventResponseConvolution` APIs expose the same operations independently of
formula parsing.

Response readouts transport a column to kernel units exactly when its events
share one divisor within the readout's scope. A run-local coefficient (a
runwise axis, or a column that belongs to one run) uses only that run's
divisors, so a column whose event durations differ only between runs reads out
run by run. A coefficient shared across runs needs one divisor across every
run. A column whose in-scope events carry different divisors (event unit-peak
with mixed durations) has no kernel-unit coefficient: response readouts refuse
it with `StructuralHypothesisErrorKind.IncompatibleDesign`. More than one scale
receipt for one column identity is refused as ambiguous rather than resolved to
the first match. Coefficient contrasts on such columns remain available.

`orthogonalize_basis = TRUE` applies a serial basis transform separately to each
cell/modulator stream **over all supplied scans**. It does not promise separate
orthogonality within each run. Independent single-run builds obtain run-local
transforms. The audit records source basis identities, rows, original scaling
divisors, and the complete transform. A basis column that is collinear with
earlier columns of its group (relative residual norm at most `1e-10`) is a typed
error; a group whose columns are all exactly zero is left unchanged and recorded
with rank 0 and the identity transform. Effective HRFs are recorded per column:
`ConvolvedTerm.hrfForColumn` returns each column's effective transformed HRF,
while the term's `hrf` keeps the source kernel. Use the effective HRF for
response functionals. Original basis identities are not silently reused.
Per-event response normalization and basis orthogonalization are currently an
explicitly unsupported combination.
Per-event HRF assignments are also rejected by this operation, because a single
effective response basis cannot represent those event-dependent kernels.
For a transformed/custom basis without an exact primitive, window readouts
require an explicit rule, for example
`cell.response(effectiveHrf, window, FunctionalDiscretization.Trapezoid(step))`.
The structural hypothesis and its compiled metadata retain the rule, sample
count, and effective step. The two-argument overload keeps its exact default
and rejects an unregistered primitive. A point readout is evaluated exactly and
records exact evaluation with one sample, even under a declared window rule.

## Diagnostics and run combination

`DesignDiagnostics` reports schema-bound VIF, block R-squared, joint standardized
contributions, and projection ranks. Constant, aliased, and unidentified cases
have typed outcomes.

`RankPolicy` is a value holding one dimensionless tolerance `τ(m, n)`:
`RankPolicy.Default` uses `max(m, n) · ε`, and `RankPolicy.relative(k)` returns
`Either[DesignDiagnosticsError, RankPolicy]` for a finite positive multiplier.
Separately constructed equal policies are equal. Every design and contrast
decision is a scale-invariant comparison against `τ`:

- rank keeps singular values above `τ · σmax`;
- a column is degenerate when its centred norm is at most `τ` times its
  uncentred norm;
- a hypothesis is estimable when its unit directions leave the numerical row
  space by at most `τ · σmax / σmin,kept`;
- contrast rows are unit-scaled and orthonormalized by SVD with the same cutoff;
- an alias equation's residual is `‖Xc‖ / (σmax ‖c‖)`.

```scala
val diagnostics = for
  policy <- RankPolicy.relative(1e-8)
  result <- ContrastDiagnostics.analyze(schema, tContrasts, fContrasts, rankPolicy = policy)
yield result
```

`ContrastDiagnostics.analyze` provides t variance divided by sigma squared,
row-invariant F worst-direction SE divided by sigma, sparse alias equations, and
estimability. Lost estimability is reported as `LostEstimability.DroppedColumns`
(checked first: a non-zero weight on an absent column or one at or below the
rank cutoff), `Aliased(residual)`, or `EmptyHypothesis`. These are unwhitened
OLS diagnostics. `preflight` (rank-based residual degrees of freedom) and
`checkColumnBudget` (construction column budget) are separate checks.

`fixedEffectsT`, `fixedEffectsF`, `concatenatedTaskT`, and `concatenatedTaskF`
combine per-run results and return `Either[DesignDiagnosticsError, _]`. Combined
runs must share one rank policy (`RankPolicyMismatch` otherwise). Fixed effects
re-evaluates the contrast from each run's own design geometry, so equivalent
spellings of a contrast combine identically. Run receipts are `RunIndex`
values: one-based positions in the supplied run vector.

`highPassVarianceRatio` compares a t contrast with and without selected cosine
columns. `highPassFVarianceRatio` compares both variances along one pinned
direction, the worst-variance direction of the without-cosines model; when that
direction is tied, it reports the largest with-cosines variance within the tied
eigenspace.

`WeightedContrastDiagnostics.t` and `.f` accept positive caller-supplied
residual sigmas and `RunContrastCombination.FixedEffects` or
`Concatenated(taskColumns)`. They report absolute variance and SE under those
sigmas; they do not estimate sigma or noise correlation, or imply AR-whitened
diagnostics. Receipts identify contributing and excluded runs.

## Single trials, confounds, and fit specifications

`trialwise()` supports term-specific `onsets`, `durations`, `subset`,
`phase`/`parent`, and a per-run `id` column. `label` names the term. Explicit IDs
must be unique within each run among the rows retained by `subset`; numeric IDs
print portably (`1`, `2.5`, `1e-7`). Run-qualified identities and source rows
survive filtering.

OLS on trialwise columns supplies an LSA specification. The portable model codec
also supports the existing LSS strategy and shared/runwise GLS with complete
AR(1)/AR(2) configuration. `LssTermDesign.make` and `LssTargetDesign.select`
expose one LSS-1 target design for inspection; they do not introduce a second
estimator.

```scala
import scalafim.fmri.fit.*

val target = for
  term <- LssTermDesign.make(trials, fixed, identities, rowProvenance)
  design <- LssTargetDesign.select(term, LssTrialKey(run, trialId))
yield design.matrix
```

`make` binds each `LssTrialIdentity(run, trialId, sourceColumnId)` to the trial
column of the same name, not only the same position, and keys trials by
`(run, trialId)`, so per-run IDs may repeat across runs. It rejects duplicate
keys or source columns, non-finite trial or fixed values, and any of the term's
trial columns in the fixed design. Each target design is `[target, sum of the
term's other trials, fixed]`; a single-trial term has no peer column. Other
conditions' trials, other task terms, drift, intercepts, and nuisance belong in
`fixed`. An absent target returns `LssTargetDesignError.MissingTarget`.

`ConfoundDesign.prepare` accepts scan-aligned `ConfoundRunInput`s and a typed
`ConfoundSpec`: motion 6/12/24, selected aCompCor components, tissue/global
signals, and FD spikes. `ConfoundRunInput.acompcor` is an `Option[Mat]`; it may
be `None` only when the spec requests zero components. Derivatives restart at
each run. Neighbour intervals are unioned, short retained segments are then
censored, and each censored scan produces one spike. Column IDs are
run-qualified (`run1_motion_x`, `run2_censor_scan_0003`) so names stay unique
when runs are concatenated; receipt and spike scan positions are one-based
within their run.

`FdCensorPolicy.maximumCensoredFraction` defaults to `1.0`, which never excludes
a run on censored fraction alone: an exclusion threshold is a study-level
decision. A run above a declared threshold becomes an explicit
`ExcludedConfoundRun`, and converting to nuisance regressors fails until the
caller handles exclusions. Independently of the threshold, a run that retains no
scans, or fewer scans than its non-spike nuisance columns, fails with
`ConfoundError.NoRetainedScans` or `InsufficientRetainedScans`. Missing inputs
are never silently padded and this API does not estimate aCompCor components.

`ConfoundSpecJsonCodec` preserves these options with a strict versioned schema.
`ModelJsonCodec` additionally encodes derived declarations and run-combination
policies. Runtime callbacks remain explicit portable-codec errors.

## Exports

`DesignMatrixRaster.toCsv(review)` returns
`Either[DesignMatrixRasterError, String]`: identified raw source values preceded
by the `source_scan`, `run`, `time_seconds`, and `retained` sidecar columns.
Header fields are always RFC 4180 quoted, rows end with `\n`, and numbers use
`PortableNumber`, so the text is byte-identical on the JVM and Scala.js. A design
column named like a sidecar column returns `ReservedColumnId` rather than being
renamed; a non-finite value returns `NonFiniteValue`. Raster colors are display
values; the CSV sidecar does not substitute those normalized colors for
measurements.

```scala
val csv = reviewed.left.map(_.message).flatMap: review =>
  DesignMatrixRaster.toCsv(review).left.map(_.message)
```

## Numerical evidence and scope

The shared tests run on both JVM and Scala.js. They combine independent numeric
fixtures with invariance and failure tests:

| Capability | Evidence |
| --- | --- |
| Design diagnostics | Independent VIF/projection values, rank loss, constants, near-collinear columns |
| Contrast diagnostics | Expected values from an independent NumPy oracle over frozen audit matrices; row-invariant F hypotheses, sparse aliases, unchanged estimable contrasts under duplication, nuisance-confounded runs, rank-policy sensitivity |
| Missing modulators | Frozen stop-signal events plus all admitted centering/scaling/missing-policy combinations with and without a factor split |
| Response conventions | `spm-1s` against an SPM12 informed-basis parity fixture (TR 2 s SPMG3 and TR 0.72 s SPMG2) and orthogonality on SPM's kernel grid; formula-built `spm-1s` design columns for an impulse and a 2 s boxcar against the SPM kernel, including the 25–32 s tail; analytic derivative; duration-dependent pulse peaks; per-run divisor transport and refusal of mixed-divisor readouts; transformed point/window estimates and variances |
| Confounds and trials | Run-boundary differences, deduplicated spikes, explicit run exclusion, run-qualified IDs, trial identities; `LssTargetDesignSuite` checks that OLS on every target design reproduces the LSS engine and its fmrilss coefficient fixture |
| Incremental builds | Exact matrix, identity and audit equality with full rebuilding, including mutation-isolation checks |

`tools/fixtures/extract_durable_contrast_matrices.py` freezes the synthetic
consumer audit matrices into `DurableContrastMatrixFixture` and records the
SHA-256 of each source dump. `tools/fixtures/generate_contrast_diagnostics.py`
recomputes every expected contrast-diagnostic value from those matrices with
textbook NumPy formulas (pseudo-inverse covariances, QR row bases, `eigh`); it
shares no code with the Scala implementation and writes
`ContrastDiagnosticsFixture`. The runtime library does not read an external
evidence directory. After orthonormalizing contrast rows, the three combined
worst-direction SE/sigma values are approximately 0.0314190, 0.1732023, and
0.2577200. Raw-row F values depend on arbitrary row scaling and are not the
contract.

`tools/fixtures/generate_spm_informed_basis.py` reimplements in NumPy the parts
of SPM12 used by `spm_get_bf` (`spm_Gpdf`, `spm_hrf`, the temporal and
dispersion differences, and `spm_orth`) and writes `SpmInformedBasisFixture`
(and the same TR 2 s kernel as `SpmInformedKernelFixture` for design-column
tests).
It requires no SPM or MATLAB installation. The Scala kernels are in continuous
fmrihrf units, so the test estimates one common positive scale from the
canonical column only.

The frozen 240-scan gamble matrix has rank 23. Adding independent unit spikes at
source scans 0 through 7 raises rank to 31 and changes residual degrees of
freedom from 217 to 209. This is a fixture-specific rank result; arbitrary spikes
can overlap existing column space and must be checked on the realized design.

Diagnostics here describe unwhitened OLS geometry. The model codec can retain
an existing GLS fit strategy without implying that these diagnostics model its
whitening operator. Raster timings are local benchmark specimens, not browser
latency or production performance guarantees.

## Known limitations

- Gale does not yet provide policy-cutoff least squares (its `pinv` fixes the
  cutoff and `leastSquares` rejects rank-deficient systems), an RREF or sparse
  null-space basis, or a row-space utility with a caller-supplied cutoff. The
  diagnostics keep minimal private helpers for these until they land upstream.
- Column-pattern contrasts compare by pattern source, not `Regex` equality. On
  Scala.js the stored source compiles with JavaScript regex semantics, which can
  differ from `java.util.regex` for some constructs.
- `spm-1s` columns are orthogonal on SPM's kernel grid (`dt = TR / 16`), not in
  continuous time.
- Basis-orthogonalized coefficients are design-dependent. A group analysis of
  canonical-only coefficients from such fits carries subject-specific
  derivative variance absorbed into the canonical column.
- Response readouts refuse columns that mix event unit-peak divisors within the
  readout's scope (for example mixed durations within a run, or different
  divisors across runs for a shared coefficient); only coefficient contrasts are
  available for them.
- Factor levels taken from a `Double` event column still use the platform
  `Double.toString` (`1.0` on the JVM, `1` on Scala.js), so such level names are
  not portable. Use text or integer factor columns where levels must match
  across platforms.
