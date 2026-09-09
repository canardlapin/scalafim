# scalafim-fmri-design

Cross-compiled JVM/Scala.js fMRI design module for `scalafim`.

Package root:

```scala
import scalafim.fmri.design.*
```

This module contains event models, formula parsing, condition bases, baseline
models, contrast definitions, design-matrix metadata, and design export helpers.
It depends on `scalafim-fmri-hrf` for sampling/convolution, standalone Gale for
shared QR/rank primitives, and Intaglio core for renderer-neutral plot and scene
exports.

Preferred typed entry points:

- `ResponseBasisReview` and `ResponseBasisGraphics` expose native HRF/FIR
  components, physical response time and exact functional receipts for
  [basis previews](../../docs/response-basis-preview.md).
- `EventModelBuilder.EventDesignRequest` collects formula, data, schedule,
  build options, and extension registries into one inspectable request value.
- `FormulaParser.parseEither` and `EventModelBuilder.buildEither` return
  `DesignError` values instead of throwing at API boundaries.
- `BaselinePlan`, `NuisanceInput`, and `NuisancePolicy` provide a validated
  baseline/nuisance model request surface over the legacy parameter list.
- `HrfSelection` distinguishes shared HRFs from per-event HRFs without
  union casts.
- `DesignColumnDescriptor` and typed export indices (`DesignColumnIndex`,
  `ScanIndex`, `RunIndex`, `BasisIndex`, `TermIndex`) sit beside the legacy
  `DesignColumnMeta` fields for safer downstream plotting/reporting.
- `DesignGraphics.eventPlot` and `DesignGraphics.eventScene` adapt
  `EventPlotData` into graphics `Plot` and `Scene` values without depending on
  SVG or any platform renderer.
- `ContrastExpr`, `CellSelector`, `BasisSelection`, and `CompiledContrast`
  provide a typed, total contrast path via `compileEither` while legacy
  `ContrastSpec`/`ContrastWeights` constructors remain available.

Renderer-neutral event plot export:

```scala
import scalafim.fmri.design.*
import intaglio.svg.*

val scene = DesignGraphics.eventModelScene(model, termName = Some("task")).toOption.get
val svg = SvgRenderer.render(scene).toOption.get
```

`DesignGraphics` itself is implemented with the public plotting DSL. A design
adapter can use the same surface directly:

```scala
import intaglio.*

val trained = plot(eventPlotData.points)
  .aes(_.time, _.response)
  .group(_.regressor)
  .scaleColorDiscrete(_.regressor, levels = eventPlotData.regressors, name = "regressor")
  .geomLine()
  .resolve
```

The dependency direction is `design -> Intaglio core`; applications select an
Intaglio renderer separately. `design` produces plot specs/scenes, while SVG is
used only by the JVM integration gallery.

Run it directly with:

```sh
sbt designJVM/test
sbt designJS/test
tools/render_design_gallery.sh
```

## Source event rows

Formula-built event terms retain their original, zero-based input table rows in
`EventTerm.sourceRowIndices`, including after formula subsets and
`MissingValuePolicy.DropFromTerm`. `DesignAudit.sourceEvents` adds term identity,
lowered event index, source row, block, onset, and duration. Ordinary rows do not
imply a parent trial ID. Phased terms also retain their existing explicit parent
trial/phase provenance; conflicting source annotations are rejected.

Hand-built terms without a source table return an empty source-row vector. Empty
means unknown; the library does not fabricate a 0-to-N source mapping. Callers
can supply `sourceRows` when they know that mapping.

Source records survive schema combination and participate in the canonical
design fingerprint. Reordering source table rows can therefore change provenance
identity even when the design matrix and centering result are identical. An
empty source-record field preserves the older canonical encoding for designs
without this evidence.

`eventsSeen` and `eventsUsed` in the compiled event audit count retained term
schedule rows, not unique conceptual trials, nonzero sampled responses, or
estimable coefficients. Signed finite onsets are retained and are not recorded
as exclusions. Inspect timing diagnostics and sampled-response support separately;
these counts do not establish that a late event contributes acquired samples.

## Review resolved convolution support

Native `convolve`, `convolvePerEvent`, and `convolveByCondition` retain a runtime
recipe with the actual run/condition regressors, HRF assignments, precision,
duration convention, event indices, sample times, and column mapping. Call
`convolved.sampledSupport(retainedScans)` with strictly increasing one-based
`ScanIndex` values from the original full design. An empty selection is valid:
events in fully censored runs remain visible with zero observed samples.

The result binds each event/basis contribution to its original term event index,
optional source row, run, design column, amplitude, global onset, duration, and
column scaling. Actual contributions and counterfactual unit-input timing
potential are separate. Unit input replaces amplitudes with one; it does not
assert that an event belongs to that condition. A zero-amplitude event can have
temporal potential without contributing to its actual condition/modulator.

Support counts use an explicit absolute tolerance in **unscaled** convolution
response units. The applied column divisor is retained, and
`scaledMaximumAbsoluteResponse` reports the actual maximum in design coordinates.
Window flags describe the configured finite computational horizon relative to
the retained grid, not physiological completeness. Fully censored runs have no
observed window flags. Gaps in the retained grid remain explicit scan selections.

Recipes contribute native HRF descriptors, inputs and numerical settings to the
design audit/fingerprint. They do not serialize arbitrary user-defined closures.
Manually constructed terms without recipes return `MissingRecipe`; replacing a
matrix or changing its recorded layout returns `ChangedTerm`. Derived columns
such as an appended trial aggregate require their own recipe before support can
be reported. As elsewhere in this module, numeric backing arrays are immutable
by convention and must not be mutated by callers.

This inspection API neither excludes events nor changes fitted designs. Any
selection policy must be explicit and applied before selection-dependent
centering/modulation. Potential support alone is not condition membership,
estimability, or an inference admission rule.

## Choose event response-support policy

`EventModelBuilder.BuildOptions.responseSupport` and `ModelBuildSpec.responseSupport`
accept an explicit `ResponseSupportRequest(retainedScans, policy, tolerance)`.
Policies either reject unobserved events while allowing partial windows, exclude
unobserved events while allowing partial windows, or require a full computational
window. A partial window also includes an eligible FIR/basis column with no
observed samples. These are computational coverage rules, not physiological or
inferential completeness claims. `None` preserves the existing library behavior;
applications that require review must select and record a policy explicitly.

For HRF terms, the compiler first assesses timing with unchanged categorical
codes and unit continuous inputs. This preserves actual condition membership
while separating zero or missing modulators from temporal support. It applies
support exclusions to source rows, then refits data-dependent event bases and
performs centering, missing-value handling, degeneracy review and orthogonalization
on the retained input. The final assignment is rechecked; newly unsupported rows
after selection cause an error rather than an unrecorded second exclusion.
Trialwise terms retain their original trial labels when rows are excluded.

`DesignAudit.responseSupport` retains typed decisions, policy, selected scans,
source row/run/timing, and eligible column sample counts. Rejection returns
`DesignError.ResponseSupportRejected` with the full receipt. Excluded source rows
also appear in `excludedEvents`; `eventsSeen` includes these explicit policy
exclusions, while `eventsUsed` counts retained term rows. Receipts survive schema
combination and enter the design fingerprint. Source tables remain unchanged.

The fit matrix boundary checks the actual fit scan selection against every
recorded support request. Changing censoring requires rebuilding the model;
fitting a differently selected time axis cannot silently reuse an earlier support
policy. Voxel-specific missing-data patterns likewise cannot claim support proved
for a different selection. These checks do not replace design-rank or inference
admission checks.

## Review a complete first-level design

`DesignReview.forRun(schema, frame, run, retained, scope)` gives an identified view
of every original scan in a run, including excluded scans. `SourceColumns` keeps
the source design; `RunwiseFit` uses the native coefficient projection. It never
substitutes a source view when projection fails. Columns keep structural IDs,
source indices and raw values. `scaled` divides by the column's maximum absolute
value over the original run solely for display; zero columns remain zero.

`DesignReviewGraphics.matrixPlot`, `tracePlot` and `eventTimelinePlot` return
renderer-neutral Intaglio trained plots. Compile at the target dimensions and
use their public layout for interaction. Matrix pages have an explicit cell
budget. Traces preserve missing samples as gaps and design-column identity; the
event timeline displays effective model durations and source event rows. Scene
semantics retain values, source rows, units and identities for vector export.

## Explicit event column roles

Use `categorical(condition)` when numeric codes represent conditions, and
`continuous(amplitude)` when a source column supplies numeric event amplitudes.
Both retain the source column's scientific identity. For example,
`onset ~ hrf(categorical(condition), continuous(amplitude), id = task)` produces
one amplitude-weighted HRF/FIR response per condition. Ordinary arguments remain
interactions; `modulators(continuous(x), center_within_run(y))` produces separate
slope columns. These expressions do not scale amplitudes or change onsets.

Categorical numeric labels use the same canonical representation as declared
factor levels (`1.0` becomes `1`), and an explicit level registry still determines
order and admissible values. Numeric categories must be finite. Continuous roles
reject text columns and obey the declared missing-value policy. Each role accepts
one column identifier. Existing implicit expressions keep their original behavior.

## Cutoff-period DCT drift

Choose a cutoff explicitly; constant and polynomial defaults are unchanged.
The same basis is accepted by `ModelBuildSpec.baselineBasis`:

```scala
import scalafim.fmri.design.baseline.*
import scalafim.fmri.hrf.design.SamplingFrame

val period = DctCutoffPeriod.fromSeconds(120.0)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val frame = SamplingFrame(blockLens = Seq(100, 80), tr = Seq(2.0, 1.5))
val model = BaselineModel.buildEither(BaselinePlan(
  frame, basis = BaselineBasis.Dct(period), intercept = Intercept.Runwise
))
```

This example contributes three drift columns in the first run, two in the
second, and two separate run intercepts. For N original scans with repetition
time TR, the drift components are k = 1 through floor(2 N TR / cutoff). A
component exactly at the cutoff is included. Columns use the orthonormal
DCT-II convention, cos(pi (i + 0.5) k / N) sqrt(2/N). This follows the basis and
cutoff convention in [SPM's filter](https://github.com/spm/spm/blob/main/spm_filter.m)
and [DCT matrix](https://github.com/spm/spm/blob/main/spm_dctmtx.m); it does not
claim whole-pipeline SPM equivalence. Gale constructs only these columns.

Cutoffs must be positive and finite. A cutoff at or below twice the run's TR
requests unavailable finite-grid components and is rejected, not truncated or
aliased. A short run can contribute zero drift columns. Component zero is
excluded; `Intercept.Runwise`, `Global`, and `None` control the intercept
separately. Adding the parameterized `BaselineBasis.Dct` case means the enum no
longer has compiler-generated `values`/`valueOf`; existing named cases and
string parsing remain available, and DCT requires its explicit period.

The basis uses each run's complete original scan indices, independently of its
acquisition-start offset. Acquisition times and the offset remain in the schema.
Censoring selects those original rows; it does not rebuild a compressed DCT
grid. Schema policy receipts report cutoff seconds, run-specific TR, original
scan count, component count, and the inclusive endpoint convention. Cutoff is
also part of structural basis identity, encoded from its exact numeric bits so
platform-specific decimal display cannot change a column identifier.

Drift enters the joint native model. The selected-estimate operator adjusts the
task readout for those columns without a separate full-data filtering pass.
If residualizing explicitly, transform both task design and data on the same
retained rows. Do not assume full-grid orthonormality after censoring or that
filtering commutes with GLS whitening. Supplied cosine confounds can duplicate
the drift basis: use the existing `NuisancePolicy` to report, reject or explicitly
drop aliases, and inspect the resulting rank and estimability diagnostics.
