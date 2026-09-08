# MVPA predictive lifecycle boundary

Status: active architectural boundary

Date: 2026-08-24

Mote ticket: `bd-01M0SVDYHYP4W04XFYHCJ2WXDW`

## Decision

Alder owns the predictive fit lifecycle used by ScalaFIM MVPA:

- data-use roles and promotion authority;
- target-blind and target-aware preparation scopes;
- terminal learners and fitted-stage audit;
- scored evaluation, model selection, and receipt-gated refit;
- the interpretation of exact-once Resample4s plans for cross-fitting.

ScalaFIM does not duplicate these policies. It owns the neuroimaging evidence
boundary, the authoritative `SampleId` ledger, measurement, scientific
estimands, and conversion of typed predictive outcomes into ScalaFIM results.

The dependency is pinned to Alder commit
`aa998f88ac96c655aa6f4093ffb28492b9e31087`. The only production Scala files
permitted to mention `alder.*` are under
`scalafim.fmri.mvpa.predictive`. The identified-evidence, measurement,
relational, and fMRI evidence cores remain Alder-free. The boundary is checked
by `tools/verify-mvpa-legacy-surface.sh`.

## Evidence modes

These modes make different claims and must not be reported interchangeably.

| Mode | Selection | Claim |
|---|---|---|
| Remote pin | Default `alderBuild` Git URI at the exact commit above | The reviewed Alder source revision is immutable and its public API compiles as a ScalaFIM consumer. |
| Clean local composite | `-Dscalafim.alder.build=<clean-alder-uri>` and `-Dscalafim.resample4s.build=<same-parent/resample4s>` with exact sibling Resample4s, Gale, and Linop4s source trees | The selected revisions build without consulting a developer worktree or a mutable snapshot artifact. |
| Snapshot fallback | Alder's upstream build uses `resample4s-core` `0.1.0-SNAPSHOT` when its sibling composite is absent | Development convenience only. It is not immutable dependency evidence and cannot satisfy a release gate by itself. |

Alder commit `aa998f8` still discovers `../resample4s` dynamically and its root
build also declares `../gale` and `../linop4s` projects. A staged remote Alder
checkout can therefore use a mutable Resample4s snapshot when its sibling is
absent, and a standalone clean build cannot even load without the other two
sibling builds. Verification records every source identity and resolved
classpath; the word “remote” describes Alder's own source selection, not an
inference that every transitive input was remote or released.

Until Alder publishes stable artifacts or pins immutable transitive source
URIs itself, ScalaFIM's release court must use the clean local-composite mode,
bind ScalaFIM and Alder to the same exact Resample4s tree, and reject duplicate
Resample4s implementations or any path under a developer checkout. For the
2026-08-25 clean-admission court, the admitted dependency set is Alder
`aa998f88ac96c655aa6f4093ffb28492b9e31087`,
Resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`, Gale
`d55fe2f97196a76ab7879e1a12f1e92403aeba06`, and a clean Git archive of
Linop4s commit `03a7e14e2811233566efcb585e53b03b6f6fb80c` (tree
`8300413d194c73b7b06b809bc0e66060f5e6a1cf`). No canonical public Linop4s
remote was available to the audit, so that last component is
content-addressed local evidence, not independent fetchability proof. This is
a dependency-hygiene limitation, not a second predictive lifecycle API.

## Consumer rehearsal

`AlderConsumerSuite` is an external-package consumer test on JVM and Scala.js.
It exercises:

1. Resample4s `Coverage.ExactOnce` compilation and Alder receipt binding;
2. typed Train and Validation roles;
3. train-only `StandardScaler` preparation;
4. a terminal learner fitted through an Alder workflow;
5. held-out scoring with a typed objective metric;
6. selection evidence and receipt-authorized refit.

The fixture learner is intentionally numerical trivia. The test protects the
ownership boundary and public protocol; scientific classifiers enter through
the predictive compiler milestone.

## Complete-resampler assessment

`ClassificationCompiler` passes the exact `Compiled[Split[Selection],
Coverage.ExactOnce]` retained by `ValidationDesign` to Alder. It does not
translate typed folds into a second split representation. Alder's
`CrossValidatedSearch.runUnsplit` owns the only role transition from the whole
unsplit population to fold-local Train evidence, rejects an empty population,
fits the complete standardize-and-learn workflow on each analysis partition,
and scores only its assessment partition.

A successful fold exposes its discarded model's complete `Audit`, Alder's own
stable audit identity, and exact analysis and assessment fingerprints.
ScalaFIM binds those to the scientific `UnitKey` and its authoritative
analysis/assessment `SampleId` sets. The retained classification estimand is
assessment-only by construction: it contains no refit flag, disposition enum,
or unimplemented selection branch. A future selection/refit estimand would
have to state that different lifecycle and result type explicitly.

## Identity rule

`alder.kernel.RowId` is an execution ordinal within an Alder data value. It is
never ScalaFIM's scientific sample identity. Every predictive execution plan
must carry a total, ordered ledger between Alder row ordinals and the bound
`AxisRef[SampleId]`, and every prediction must be reconstructed and validated
against that ledger before becoming an `AnalysisResult`.

## Matrix boundary

`AlderDataBoundary` performs one explicit sequence for a local predictive
task: optional typed row restriction, neural measurement composition, budgeted
local matrix materialization, then construction of Alder-facing row views.
Complete-resampler assessment uses `prepareAll`, preserving the authoritative
sample space and leaving every fold restriction to Alder. The row views refer
to the local matrix and do not copy coordinates. Alder's `InMemoryData` owns
its `RowId` values, while `AlderRowLedger` validates and maps emitted ordinals
back to `SampleId`.

The boundary receipt reports four independent work records. In particular, it
distinguishes the one local matrix allocation from the lightweight row/example
records supplied to Alder and records that `FeatureView.read` allocates one
coordinate array while `FeatureView.writeTo` can stream into backend-owned
storage without one. The Alder `DataFingerprint` is a declared summary of the
scientific plan, restricted sample axis, and measurement; storage backend and
materialization are execution receipt fields and do not change this identity.

## Predictive result boundary

`ClassificationCompiler.run` returns `OutOfFoldClassification`, not an Alder
search result or a fold-shaped compatibility payload. Exact-once
reconstruction requires every assessment output to agree with the bound
`UnitKey`, assessment position, `SampleId`, truth value, and ordered class-score
axis. Duplicate, missing, foreign, reordered, or detached evidence fails
closed. The final prediction column, decision-score table, and per-row receipts
are in authoritative source-sample order.

Each output receipt links its prediction and score row to the corresponding
fold receipt, full Alder fit audit, stable model identity, and the ScalaFIM
preparation/materialization receipt. Accuracy, balanced accuracy, and the
confusion matrix are derived views of the typed predictions; they never replace
the primary output.

## Measurement-frame execution

`CategoricalObservationSource` and `CategoricalClassification` supply the
ordinary `ScientificSource` and open `Estimand` instances. Learner behavior is
selected by a statically supplied
`CategoricalLearnerCompiler[Configuration, Fitted, Error, Prediction]`;
built-ins and downstream learners are ordinary configurations and compiler
instances, not cases in a registry. Their compiler and task capabilities run
through `Mvpa.specify -> bind -> plan -> execute`, so region, global, and
searchlight analyses use the same `MeasurementFrame`, `AnalysisResult`, and
execution-receipt model as every other analysis.
Frame-local failures remain local outcomes. Both dense and matrix-free source
evidence compose with the measurement before the one admitted local
materialization; the receipt records source representation, measured
representation, exact cells, and budget.

Searchlight centers remain spatial rendition metadata. The scientific source
and result shells have no searchlight fields or scatter policy.
`SpatialResultScatter` consumes `SearchlightRendition` and restores the compact center-domain order
while retaining every local success, rejection, failure, and receipt.

## Predictive certification court

`PredictiveArchitectureCourtSuite` is shared and therefore runs unchanged on
the JVM and Scala.js. It checks perturbed held-out patterns and targets against
an independent train-only standardization and nearest-centroid oracle; sample,
run, row, class, and seed order laws; bind-time rejection of folds lacking a
training class; exact OOF coverage; strategy admission; and reconciliation of
per-measurement versus aggregate work. Dense/matrix-free parity and typed
spatial scattering are covered by the adjacent frame suites.

The task admits only the execution contract it actually implements: local
dense representation, binary64 arithmetic, no external solver, and an explicit
positive materialization budget. Unsupported representation, precision,
solver, or a blanket materialization ban fails during planning rather than
becoming a misleading execution receipt.

`downstream.predictive.PredictiveLearnerExtensionSuite` is the extension
court. It defines its own configuration, fitted model, error type, and richer
prediction, runs it through Alder and the unified MVPA result shell, and proves
that an incompatible minimum-feature requirement is rejected at bind time.
