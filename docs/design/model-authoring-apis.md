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
trial. A phase onset is never treated as evidence of its parent onset. Missing,
duplicate, inconsistent, and non-finite timing evidence return the typed
`EventTrialSummaryError`. Unphased provenance remains validated but has no
parent-relative offset.

## Formula text, source locations, and JSON

`ModelFormula.renderEither` produces tokenized formula text only when the
current admitted grammar can round-trip it. Formula tokens carry term and
argument paths. `FormulaParser.parsePositioned` additionally returns
zero-based, end-exclusive source spans for parsed nodes.

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
they are distinct choices.

## Missing values, expressions, and derived columns

`EventExpressions.evaluate` returns typed number, text, logical, or missing
values. Arithmetic and comparisons propagate missingness; logical operators use
Kleene three-valued logic. `filter` retains only explicit true values. The
existing vector operators `&` and `|` remain aliases for `&&` and `||`.
`missing()` is a portable missing literal. Text comparisons check observed
levels by default, with an explicit switch for callers using declared empty
levels.

`DerivedColumn.parse` accepts declarations such as:

```text
net: number = gain - loss
category: text = ifelse(net > 0, "positive", "nonpositive")
band: text = cut(net, c(-Inf, 0, Inf), c("negative", "nonnegative"))
```

Evaluate declarations in order with `DerivedEvents.evaluate`; overwrites,
forward references, and incompatible declared types fail. Materialize only the
columns needed by a term and choose `DerivedMissingRows.Reject`, `Drop`, or
`PreserveNumeric`. The result retains original row indices and exclusions.
Missing text is never encoded as a factor level. Numeric missing values can be
preserved for a subsequent modulator policy.

`EventBins.quantiles` uses observed finite values and type-7 quantiles. It rejects
repeated cut points instead of silently merging bins. `cut_quantiles(x, k)` is
also available in expressions. The caller supplies the population: for term
bins, select that term's events before pooling over runs. `breaksText` records
explicit left-closed, right-open cuts; missing values receive no bin.

`EventOnsetSelection.select` is an explicit preprocessing operation. It reports
missing, negative, and at-or-after-run-end onsets, returning retained source rows
or a typed rejection. This preserves the existing ability to model pre-run
events when that is the caller's intended design.

The formula wrapper below uses observed-only centering and sample-SD scaling
within each run, after term subsetting:

```text
onset ~ hrf(condition,
  modulators(modulator(rt, center = cell, scale = z, missing = zero)),
  include_main = TRUE)
```

`center` accepts `none`, `run`, or `cell`; `scale` accepts `raw`, `z`, or `sd`.
`sd` divides by sample SD without implicitly subtracting the mean. Missing
policies are `reject`, `drop_from_term`, and `zero`. Dropping applies to the whole
paired term before centering. Zero contribution is applied after centering and
requires centering (z-scoring supplies it when needed). Degenerate scales have
explicit receipts. `scale_within_run(x)` and `sd_within_run(x)` are concise
reject-on-missing forms; legacy `scale` and `scalewithin` keep their existing
contracts.

## Modulator and response conventions

`orthogonalize = TRUE` serially residualizes the ordered modulator streams
within runs using the existing typed modulator policy. It requires an explicit
term ID. `product(center_within_run(gain), center_within_run(loss))` is accepted
inside `modulators`; its component centering occurs after subset and term-wide
drop selection. `shared_slopes = TRUE` with `include_main = TRUE` keeps split
categorical main effects and one common slope per modulator.

```text
onset ~ hrf(condition,
  modulators(gain, loss,
    product(center_within_run(gain), center_within_run(loss))),
  include_main = TRUE, shared_slopes = TRUE, id = task)
```

For SPMG2/3, `temporal_derivative = "analytic"` or `"spm-1s"` declares the
continuous derivative or `h(t) - h(t-1s)` convention. The third SPMG3 component
remains the dispersion derivative.

`event_normalization = "unit-peak"` normalizes each event's duration-dependent
pulse response on its declared reference grid (`event_peak_step`, default 0.1
seconds). It is distinct from kernel normalization and final column scaling.
`event_normalization = "as-convolved"` explicitly selects the unchanged pulse
scale. `ConvolvedTerm.eventPeakScales` retains duration and divisor receipts.
The typed `EventResponse` and `EventResponseConvolution` APIs expose the same
operations independently of formula parsing.

`orthogonalize_basis = TRUE` applies a serial basis transform separately to each
cell/modulator stream **over all supplied scans**. It does not promise separate
orthogonality within each run. Independent single-run builds obtain run-local
transforms. The audit records source basis identities, rows, original scaling
divisors, and the complete transform. `hrfForColumn` returns each group's
effective transformed HRF; use this HRF for response functionals. Original
basis identities are not silently reused. Per-event response normalization and
basis orthogonalization are currently an explicitly unsupported combination.
Per-event HRF assignments are also rejected by this operation, because a single
effective response basis cannot represent those event-dependent kernels.
For a transformed/custom basis without an exact primitive, window readouts
require an explicit rule, for example
`cell.response(effectiveHrf, window, FunctionalDiscretization.Trapezoid(step))`.
The structural hypothesis and its compiled metadata retain the rule, sample
count, and effective step. The two-argument overload keeps its exact default
and rejects an unregistered primitive.

## Diagnostics and run combination

`DesignDiagnostics` reports schema-bound VIF, block R-squared, joint standardized
contributions, and projection ranks. `RankPolicy` makes the numerical rank rule
explicit. Constant, aliased, and unidentified cases have typed outcomes.

`ContrastDiagnostics` provides t variance divided by sigma squared, row-invariant
F worst-direction SE divided by sigma, sparse alias equations, estimability
reasons, fixed-effects and concatenated task information, and high-pass variance
ratios. These are unwhitened OLS diagnostics. Rank-based residual degrees of
freedom and construction column budgets are separate checks.

`WeightedContrastDiagnostics` accepts positive caller-supplied residual sigmas
and `RunContrastCombination.FixedEffects` or `Concatenated(taskColumns)`. Its
variance/SE is conditional on those sigmas; it does not estimate them or imply
AR-whitened diagnostics. Receipts identify contributing and excluded runs.

## Single trials, confounds, and fit specifications

`trialwise()` supports term-specific `onsets`, `durations`, `subset`,
`phase`/`parent`, and a per-run `id` column. `label` names the term. Explicit IDs
must be unique within each run; run-qualified identities and source rows survive
filtering.

OLS on trialwise columns supplies an LSA specification. The portable model codec
also supports the existing LSS strategy and shared/runwise GLS with complete
AR(1)/AR(2) configuration. `LssTermDesign.make` and `LssTargetDesign.select` expose
one target-versus-summed-other-trials design for inspection, retaining fixed
columns and source identities; they do not introduce a second estimator.

`ConfoundDesign.prepare` accepts scan-aligned per-run inputs and a typed
`ConfoundSpec`: motion 6/12/24, selected aCompCor components, tissue/global
signals, and FD spikes. Derivatives restart at each run. Neighbour intervals
are unioned, short retained segments are then censored, and each censored scan
produces one spike. Excluded runs are explicit; converting to nuisance
regressors fails until the caller handles exclusions. Missing inputs are never
silently padded and this API does not estimate aCompCor components.

`ConfoundSpecJsonCodec` preserves these options with a strict versioned schema.
`ModelJsonCodec` additionally encodes derived declarations and run-combination
policies. Runtime callbacks remain explicit portable-codec errors.

## Exports

`DesignMatrixRaster.toCsv(review)` emits identified raw source values with
source scan, run, time, and retention status. Raster colors are display values;
the CSV sidecar does not substitute those normalized colors for measurements.

## Numerical evidence and scope

The shared tests run on both JVM and Scala.js. They combine independent numeric
fixtures with invariance and failure tests:

| Capability | Evidence |
| --- | --- |
| Design diagnostics | Independent VIF/projection values, rank loss, constants, near-collinear columns |
| Contrast diagnostics | Frozen audit matrices, row-invariant F hypotheses, sparse aliases, unchanged estimable contrasts under duplication, nuisance-confounded runs |
| Missing modulators | Frozen stop-signal events plus all admitted centering/scaling/missing-policy combinations with and without a factor split |
| Response conventions | Analytic and one-second difference kernels, duration-dependent pulse peaks, transformed point/window estimates and variances |
| Confounds and trials | Run-boundary differences, deduplicated spikes, explicit run exclusion, trial identities, existing fmrilss coefficient fixtures |
| Incremental builds | Exact matrix, identity and audit equality with full rebuilding, including mutation-isolation checks |

The contrast fixture generator in `tools/fixtures/` records source hashes.
These synthetic consumer audit matrices are test inputs; the runtime library
does not read an external evidence directory. Independent recomputation corrected
the earlier F-efficiency expectations: after orthonormalizing contrast rows,
the three combined worst-direction SE/sigma values are approximately 0.0314190,
0.1732023, and 0.2577200. Raw-row F values depend on arbitrary row scaling and
are not the contract.

The frozen 240-scan gamble matrix has rank 23. Adding independent unit spikes at
source scans 0 through 7 raises rank to 31 and changes residual degrees of
freedom from 217 to 209. This is a fixture-specific rank result; arbitrary spikes
can overlap existing column space and must be checked on the realized design.

Diagnostics here describe unwhitened OLS geometry. The model codec can retain
an existing GLS fit strategy without implying that these diagnostics model its
whitening operator. Raster timings are local benchmark specimens, not browser
latency or production performance guarantees.
