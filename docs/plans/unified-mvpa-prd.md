# Unified Neuroimaging Analysis: PRD Sketch

Status: proposed requirements; not an implementation report

Version: 0.2, 2026-09-12

Implementation tracking: [live epic, work packets, and dependency map](unified-mvpa-epic.md).
Mote epic `bd-01M2BMGRP4MSM0RRRKNTHM0H4K`, label `unified-mvpa`.
Filing the implementation graph does not mark these requirements implemented.

Inference qualification follows the frozen
[known-truth calibration protocol](unified-mvpa-inference-calibration-protocol.md).
That protocol fixes claims, simulation populations, higher-rank nulls, and
decision rules; its existence is not numerical admission.

Resource and comparative qualification follows the frozen
[benchmark protocol](unified-mvpa-resource-comparative-protocol.md). It fixes
reference profiles, measurement boundaries, complete costs, matched baselines,
and budgets without claiming that any row currently passes.

Revision 0.2 selectively incorporates the four supplementary documents listed
in the [review record](unified-mvpa-supplementary-review.md). It strengthens
inspection, reuse, inference, and qualification without changing the rapid
removal policy or replacing the three required vertical slices.

This document turns the [pattern-first method notes](unified-pattern-first-mvpa-notes.md)
and [architecture notes](unified-mvpa-architecture-notes.md) into product scope,
requirements, delivery milestones, and acceptance gates. Those documents retain
the derivations and original proposals. Where they conflict with this sketch,
this sketch proposes the resolution. In particular, **rapid replacement and
deletion supersede the earlier recommendation to retain compatibility façades**.

This PRD does not itself authorize source deletion. Implementation changes
should follow the reviewed scope and acceptance gates below.

## 1. Product definition

ScalaFIM will provide one analysis foundation for predictive MVPA,
representational geometry, and global pattern analysis. Researchers will use
the same identified evidence, spatial measurements, and provenance model
without making classification, RSA, and decomposition pretend to be the same
statistical procedure.

The flagship new method is a whole-brain, pattern-first, spatially structured
reduced-rank model. It learns task-related brain patterns and derives
classification, multiresponse prediction, encoding, interpretable components,
regional predictors, and explicitly qualified inference from that relationship.
Searchlights remain supported measurements and comparison methods; the global
model does not require them.

The architectural foundation is:

> Identified spaces + typed linear evidence + explicit evidence designs +
> composable measurements + open, typed estimands.

### Users and jobs

| User | Required workflow |
| --- | --- |
| Experimental researcher | Classify conditions or predict feature vectors with held-out evaluation and anatomically meaningful maps. |
| Representational analyst | Estimate cross-validated distances, RSA, contrasts, and canonical effects from compatible runwise evidence. |
| Global-pattern researcher | Fit whole-brain components, inspect their task relationships, rotate explanatory coordinates, and query local access to information. |
| Group analyst | Combine commensurate subject estimates with uncertainty; distinguish population association, subject generalization, and prevalence claims. |
| Method developer | Add an estimand and typed output without editing a central enum or rebuilding preparation, resampling, and linear algebra. |

### Product outcomes

1. Researchers can move between whole-brain and regional questions without
   silently changing evidence identity, preparation, or interpretation.
2. Method developers implement scientific requirements and a compiler, not
   another data-loading, validation, spatial-loop, and result framework.
3. Forward signal patterns, backward filters, and conditional predictive
   importance are distinct products with explicit meanings.
4. Fast conditional confirmation is available without claiming to include
   uncertainty from every discovery and model-selection decision.
5. Each migrated workflow has one active implementation, not a new framework
   wrapped around a permanent old framework.

### Two completion levels

- **Foundation release:** supported existing analyses have migrated, superseded
  public machinery is removed, and the three decisive vertical slices pass.
- **Scientifically qualified pattern-analysis release:** the global method also
  passes its interpretation, inference, group-analysis, and performance gates.
  A working optimizer alone does not meet this level.

## 2. Scope and non-goals

### Required scope

- Categorical classification, scalar and multiresponse regression, and encoding
  and decoding from the pattern-first model.
- Validation, nested tuning, exact-coverage cross-fitting, partition pairing,
  and design-aware inference as distinct designs.
- Observations and partitioned relations, including operator-native fMRI
  readouts and sufficient-statistic execution.
- Whole-domain fits, hard and weighted ROIs, searchlights, surface measurements,
  and explicit alignment/basis measurements.
- Crossnobis, RDM, RSA, first-order contrasts, and migration of supported
  canonical/global methods.
- Sparse, spatially structured global patterns; whole-brain and restricted-region
  prediction; rotated/oblique explanatory coordinates and stability diagnostics.
- Voxel, component, and projected-rank confirmation, plus group analysis of
  commensurate subject solutions.
- Inspectable plans, typed results, visible local failures, resource policies,
  and deterministic receipts on JVM and Scala.js.

### Non-goals for the first qualified release

- Arbitrary nonlinear or covariance-only neural codes, or causal localization.
- A guarantee of superiority to every searchlight, local model, or PLS method.
- Treating anatomical clusters as independent predictive dimensions, especially
  for a binary contrast or single continuous outcome.
- Uncorrected inference over arbitrary post-hoc ROIs or rotations.
- Full-selection-uncertainty inference at frozen-confirmation cost.
- A universal fit lifecycle or a new general linear-algebra, resampling,
  serialization, or plugin registry.
- A large physical module split before boundaries are exercised.
- Long-term source compatibility with superseded MVPA orchestration APIs.

Basic group analysis and inference are required. Joint hierarchical fitting,
shared anatomical dictionaries, additional nonlinear observation models, and
prevalence-specific procedures are extensions, not substitutes for those core
deliverables.

## 3. Decisions to carry into implementation

### D1. Evidence and estimands replace ROI analyses as the public center

A specification declares its source, question, evidence design, and measurement
scope. A whole-brain fit returns a fitted artifact; an RDM query returns an RDM.
Neither is forced into a metric vector or singleton ROI result.

### D2. Predictive and relational analysis share infrastructure, not semantics

Prediction uses preparation, learners, assessment, and cross-fitting. Relational
analysis uses identified relations, partition pairing, queries, and boundary
closure. Global decompositions declare their input object and training scope.
Supervised pattern fitting participates in the predictive lifecycle without
reducing its rich artifact to a `predict` method alone.

### D3. Coordinate compatibility, observation membership, and value provenance differ

Coordinate identity includes ordered coordinates or basis, units, and relevant
scale conventions. An observation axis records which observations occur, in
which order, including repeated draw occurrences. Value identity records the
evidence and preparation lineage.

Two subjects may share anatomical coordinates without being the same data.
Equally shaped arrays may have incompatible axes. Do not put all provenance
into one space hash and make compatible measurements incompatible, or omit
provenance and accidentally reuse another subject's fitted values.

### D4. Measurements are linear maps with scientific scope

An ROI is a sparse selection measurement, not an analysis kind. A measurement
includes source/local spaces, its leg, its spatial descriptor, and any training
scope. A learned projection does not become leakage-free by being packaged as
a fixed matrix. Frames are compact, indexable or streaming programs, not
necessarily eager vectors of every neighborhood and feature index.

### D5. Preparation scope is part of evidence validity

Target-blind does not mean safe to fit on held-out brain data. Every learned
stage has a training scope; target-aware stages additionally satisfy the
appropriate own-target-exclusion contract. This includes covariance estimation,
spatial selection, rotation, target scaling, nuisance preparation, and alignment.

### D6. Scientific objects and storage representations remain separate

Dense, sparse, operator-backed, and disk-backed evidence share scientific types
when they represent the same object. Required operations and costs are exposed
as capabilities. Representation independence does not promise that every
backend supports every operation cheaply.

### D7. Replace by workflow and delete promptly

Each cutover migrates callers, tests, examples, and documentation and removes the
superseded implementation. Do not ship an enduring compatibility namespace,
deprecated forwarding classes, or two orchestration paths. Preserve useful
numerical kernels and scientific fixtures, not every old class name.

## 4. Foundation requirements

### FND-01 — Identified axes and typed evidence

Distinguish semantic keys, implementation ordinals, ordered axis identity, and
source/value identity. Bind targets and metadata to the exact sample axis.
Categorical/scalar targets may be typed columns; multiresponse targets expose
an identified feature axis rather than an unlabeled vector-valued column.

Primary evidence is observational `Table[Samples, Neural]` or relational
`Table[Effects, Neural]`, with explicit partition identity for relation sets.
Use admitted Multivar semantic capabilities and bridge existing ScalaFIM
response/read contracts and locus-backed domains. Do not create competing
generic response or spatial identity systems.

Acceptance: reordered targets fail binding without reindexing; stable keys
survive restriction and OOF assembly; equivalent dense/operator evidence has
the same scientific identity.

### FND-02 — Lawful reindexing

Bind resample4s ordinal operations to parent/child axes. Preserve distinctions
among subset selection, injective reordering, repeated draws, and permutations.
A bootstrap occurrence retains both its parent key and occurrence identity;
duplicates must not disappear in a key map. Child identity records the exact
mapping and operation kind. Repeated coverage is not exact-once coverage.

Acceptance: nested restriction equals composed restriction; OOF outputs cover
exactly the promised observations; reordered/repeated selections have distinct,
reproducible identities.

### FND-03 — Measurement legs and frames

For `L` mapping neural coordinates into local measurements, observations become
`X Lᵀ` and relations become `B Lᵀ`. Hard selection, weighted ROIs, surface
patches, identity, and basis/alignment measurements use this contract.

Respect primal/dual orientation and the metric needed to identify them; a
transpose does not erase that distinction. Centers, topology, scattering, and
overlap belong to descriptors. Traversal order must not change measurement
identity or random streams. Dependent local-space types must survive frame
packaging without erasure to `Any`.

Acceptance: identity is neutral; selection equals slicing; frame execution is
bounded-memory; wrong-space legs fail before numerical access.

### FND-04 — Distinct evidence designs

Validation, cross-fitting, pairing, randomization, and bootstrap are separate
scientific contracts sharing ordinal machinery. Declare coverage, nesting,
reduction weights, generalization units, and training/assessment roles.

Pairing retains ordered edges and explicit independence claims; different run
IDs alone do not prove independent errors. Randomization specifies its null,
exchangeability restrictions, nuisance treatment, and statistic reduction.
Types enforce declared structure, not the truth of experimental assumptions.

For fMRI/naturalistic data, record temporal alignment, HRF/readout estimation
scope, filtering support, and train-test buffers where required. An observation
split is insufficient when preparation couples evidence across its boundary.

Acceptance: unsupported coverage, missing capabilities, and incompatible
randomization structures fail binding with actionable errors.

### FND-05 — Open estimands and typed outputs

Adding a method requires its specification, result type, compatible compiler,
and laws, not changes to `RoiPayload`, `Response`, or a global method registry.
Closed ADTs remain appropriate for closed local choices and error categories.

Results distinguish unrequested, unsupported, non-estimable, failed, cancelled,
partial, and valid products. These states are not numerical zero. Reducers
retain their weights, excluded contributors, and denominators or sufficient
reducer state; pooled-trial accuracy and mean-run accuracy are different outputs.

The conceptual result shell is:

```scala
final case class AnalysisResult[A](
    plan: ScientificPlanIdentity,
    value: A,
    receipt: ExecutionReceipt
)
```

This illustrates a shape, not an existing API. Frame-valued estimands use an
appropriate `FrameResult[A]` with local success/failure. Global fits return their
artifacts directly. Space-dependent results require dependent entries. Metric
tables and image exports are views, not the scientific result ontology.

Acceptance: a test-only new estimand needs no core enum edit; failed measurements
remain visible without invalidating successful measurements.

### FND-06 — Inspectable compilation and receipts

```text
source + design + measurement scope + estimand
  -> bind identities, capabilities, and preparation scopes
  -> bound scientific plan
  -> numerical program: operations, sufficient statistics, algorithms
  -> execution plan: backend, materialization, chunking, scheduling
  -> typed result + receipt
```

The scientific plan records evidence, question, design, metrics, target coding,
preparation, model specification, and assumptions. Exact backend substitution
does not change the estimand. Rank truncation, changed noise policy, or changed
target metric cannot be hidden as backend substitutions.

Receipts record provider revisions, algorithms/approximations, tolerances,
precision, materializations, resource use, random streams, convergence, and
failure/coverage summaries. Bind randomized scientific choices to the plan;
scheduling details belong to execution. Cache keys include value lineage,
training scope, and every operation that changes the cached result.

Keep four logical bindings distinct without building four identity frameworks:

| Binding | Identifies |
| --- | --- |
| Question | Quantity, units, population/generalization intent. |
| Analysis specification | Question plus estimator, preparation, metric, measurements, design, reduction, and intended claims. |
| Evidence | Exact source revisions, ordered domains, realized splits/draws, and upstream fitted artifacts. |
| Realization | Bound analysis, code/provider versions, backend, precision, fidelity, random streams, and committed payload integrity. |

These complement coordinate compatibility and observation membership in D3.
A source path is not a content revision, and a supplied digest is not verified
content. Record declared identity, verified read blocks, and verified complete
content separately. Full hashing is a budgeted read, never hidden inspection.

Acceptance: plans are inspectable without running; schedule changes preserve
assignments and lawful random streams; caches cannot cross incompatible
subjects, folds, targets, or preparation scopes.

### FND-07 — Capabilities and resource admission

Residual moments, estimability, degrees of freedom, precision application,
random access, and replayability are capabilities, not optional-field bags.
Symmetry/PSD follows from construction or certification, not approximate
numerical appearance. Honor explicit memory/materialization budgets. A learner
requiring replay cannot silently consume a one-shot stream.

Acceptance: sentinel operators detect hidden dense reads; unsupported operations
fail admission or expose a costed materialization path; cancellation releases
owned readers/resources; shared kernels remain portable.

### FND-08 — Bounded inspection and actionable diagnostics

Provide native `describe`, `inspect`, `explain`, and specification/plan `diff`
operations. Metadata-only binding/planning must not read neural values or fit
statistics. Data-dependent facts remain unknown until explicit scoped execution
or probing. Persisted metadata can require bounded I/O; payload inspection is
a distinct, budgeted, exposure-recorded request.

Default views show the quantity and units, lifecycle/claims, blockers, relevant
dependency paths, legal execution alternatives with cost unknowns, and next
operations. Large domains and diagnostics use explicit limits/continuations;
omitted or unverified information cannot appear verified. Generate readable and
any machine-readable views from the same owning descriptors. This requires no
network service, separate string DSL, or hand-maintained schema registry.

Diagnostics contain a stable code, stage/parameter, observed fact, required
condition, and causal dependency path. Suggested repairs distinguish equivalent
execution, numerical-fidelity changes, estimator changes, population changes,
and claim downgrades. Rebind scientific changes explicitly; never silently
shrink an ROI, drop conditions, or replace a covariance model to make a job run.

Acceptance: a poison source reports zero neural reads during metadata-only
inspection/planning; views retain unknowns; wrong-axis errors name the mismatch;
native/decoded specifications agree whenever a decoder is supplied.

### FND-09 — Checked reuse and explained invalidation

Reusable computations retain exact semantic dependencies, training/read origins,
target revision, measurement support, parameters, relevant randomness, fidelity,
and implementation binding. Revalidate at use time; return admitted, rejected,
or unknown with reasons. Invalidate affected descendants of changed inputs,
not every computation or merely nodes sharing a filename.

For example, another RSA model can reuse compatible retained geometry, but its
comparison and multiplicity record change. New CV grouping invalidates affected
preparation/fits/predictions. A changed dense-covariance ROI requires a new local
solve. A display title changes no numerical values. Each reuse needs an admitted
compatibility or sufficient-statistic law; no universal additive-fold shortcut.

Acceptance: changing content at the same locator prevents stale reuse; changing
target/fold scope invalidates target-aware descendants; adding an RSA query
reuses only justified products and explains the retained/invalidated steps.

## 5. Analysis requirements

### PRED-01 — Predictive lifecycle

Categorical, scalar, and multiresponse workflows support training-only
preparation, validation, nested tuning, exact-coverage cross-fitting, held-out
predictions, explicit metrics, and optional refit. OOF predictions retain sample
keys, target coordinates, and training lineage.

Use Alder for the admitted predictive lifecycle through batched/matrix-native
adapters. Keep evidence and relational cores independent of Alder. Verify actual
provider capabilities on immutable, cross-platform-tested pins; proposed
adapters must not be assumed available in the current build.

The global estimator's numerical fit remains a rich scientific artifact through
this lifecycle. Do not implement it as an opaque scalar-score learner or
maintain a second leakage protocol outside the adapter.

Use the existing `SwiftCentroidClassifier` with its training-fitted Z-score
scaling as the first predictive migration baseline. Preserve class ordering,
priors, score-to-probability convention, predictions, and pooled-sample reduction;
do not swap in a different classifier or standardize twice. Existing correlation
centroid behavior remains a separate migration item. Probability normalization
alone must not acquire a new calibration claim.

Acceptance: leave-one-run-out classification and multiresponse regression yield
correct keyed OOF outputs; leakage sentinels reject unsafe pipelines; missing
class/target support has an explicit typed policy.

### REL-01 — Reusable partitioned relations

Consume identified runwise/partitionwise relations and admitted residual and
estimability capabilities. fMRI readouts may construct `B_r = A_r Y_r` as
composed operators rather than eager trial images.

Crossnobis, RDM, RSA, contrasts, and compatible canonical queries may reuse a
relation fit only when preparation, noise policy, and sufficient statistics
support the query. Changed requirements trigger an explicit compatible
derivation or refit.

Start migration parity with the current operator's fixed-identity metric,
all-distinct partition pairing, signed squared Euclidean distances, and optional
feature-count normalization. Report that metric explicitly. A legacy
`Crossnobis` class name is not evidence that noise precision was estimated;
noise-normalized crossnobis is a separately specified and qualified choice.

Acceptance: crossnobis, RSA, and a first-order contrast reuse one admitted
relation source; receipts explain reuse; baseline parity preserves the original
metric, normalization, class ordering, and signed output.

### REL-02 — Relational queries and boundary closure

Support second-order contractions of the form

\[
\mathscr E_{LR}(H,K)
=\operatorname{tr}(H^\top B_L K B_R^\top).
\]

Closing neural, experimental, both, or neither pair of boundaries produces
distinct typed effect forms/RDMs, neural coupling forms, scalars, or open
transports. First-order contrasts remain first-order queries. Rectangular
left/right spaces and ordered pairing are not silently symmetrized.

For fixed `K` and zero-mean errors in `d_L = δ_L + e_L` and `d_R = δ_R + e_R`,
the cross-product expectation includes `tr(K E[e_R e_Lᵀ])` in addition to
`δ_Lᵀ K δ_R`. An unbiased interpretation needs that error term to vanish.
Track acquisition/readout and fitted-preparation dependencies through row
restriction; distinct beta rows do not establish independence.

For learned `K`, require an admitted conditional-error argument, independent
metric evidence, or a method-specific justification. Merely calling one
endpoint “training” does not establish the claim. Pair contributions sharing a
partition are not independent samples for a standard error. A computable
signed statistic may remain descriptive when its stronger claim is unqualified.

Acceptance: direct query equals materialize-then-contract; forward/adjoint
queries agree; reversal obeys the transpose law; signed forms/distances are
not silently projected to PSD or zero. Shared-origin and endpoint-learned-metric
fixtures do not automatically receive an unbiasedness claim.

### GLB-01 — Global decompositions

Declare the decomposed object: observations, relations, effect forms, or neural
forms; its centering/metric; its training scope; and whether the output is
predictive or descriptive. Reuse admitted Multivar PCA/PLS/CCA/canonical
capabilities instead of private solver families.

Acceptance: a global fit returns a typed artifact, not a fake one-ROI payload;
indefinite forms cannot enter PSD-only algorithms without an explicit justified
policy; migrated canonical behavior has independent parity evidence.

### PAT-01 — Pattern-first model and task support

The initial proposed model, after recorded centering and nuisance preparation,
is

\[
x=A C^\top y+\epsilon,\qquad
\operatorname{Cov}(\epsilon)=\Psi\succ0.
\]

`A` is neural-by-component; `C` is target-by-component. The forward model
specifies zero conditional residual mean. Categorical prediction uses condition
coding and priors. Continuous prediction uses a declared target prior/covariance
and metric, including feature-block weighting. Store factors and covariance
capabilities, not a compulsory dense neural-by-target operator.

For the initial Gaussian observation model, define `u = Aᵀ Ψ⁻¹ x` and
`G = Aᵀ Ψ⁻¹ A`. Class scores use
`m_cᵀ u − ½ m_cᵀ G m_c + log π_c`, where `m_c = Cᵀ y_c`.
For Gaussian continuous targets, with `Φ = Cᵀ Σ_y C`, decoding and encoding are

\[
\hat y=\Sigma_y C(I+G\Phi)^{-1}u,
\qquad \hat x=AC^\top y.
\]

Use stable solves and declared conditioning policies, not literal matrix
inversion merely because equations display inverses. Select rank and
regularization using held-out task performance alongside model diagnostics.
These equations define the initial estimator family, not all neural coding.

Acceptance: classification, multiresponse decoding, and encoding use the same
admitted fitted relationship; reduced formulas match an independent dense
Gaussian oracle on well-conditioned fixtures.

### PAT-02 — Sparse, spatially coherent signal patterns

Regularize forward patterns primarily. Support joint row sparsity, anatomical
graph structure, and separate support-coherence and signed-smoothness controls.
The proposed default uses an envelope `g_v ≥ ||A_v:||₂`, sparsity on `g`, and
graph variation of `g`; signed-loading smoothing is separately tunable.
Graphs respect anatomical topology, including cortical sulci.

Scale constraints are optimization conventions, not independence claims. Joint
low-rank fitting is nonconvex. Expose initialization, restarts where needed,
warm starts, objective history, stopping reason, and conditioning. Unconverged
fits are not qualified successful fits.

Acceptance: suppressor and alternating-sign fixtures distinguish forward signal
from noise cancellation and preserve heterogeneous signs within coherent
support. Independent small problems check objective/subproblem behavior; do
not claim a global optimum from monotonic alternating updates.

### PAT-03 — Distinct interpretation products

| Product | Meaning |
| --- | --- |
| Structured forward pattern | Where a task-linked dimension is expressed under the fitted model. |
| Backward filter | Which measurements construct a named score or prediction, including noise cancellation. |
| Empirical Haufe diagnostic | Forward covariance pattern of named scores on independent diagnostic data. |
| Conditional predictive importance | Model-derived or held-out contribution under a declared conditioning set. |

For full-column-rank `A`, calibrated filters
`W = Ψ⁻¹ A (Aᵀ Ψ⁻¹ A)⁻¹` yield `z = Wᵀ x` and `Wᵀ A = I`.
Under the stated signal/residual covariance assumptions, their model-implied
Haufe pattern equals `A`. This is not automatically the Haufe pattern of
posterior-shrunken target predictions.

Empirical Haufe patterns identify their scores and evidence. They need not be
sparse or equal the structured estimate in finite data; disagreement is a
diagnostic, not something to conceal. These maps are neither automatically
causal nor significance maps.

Acceptance: a suppressor fixture recovers distinct pattern/filter roles; the
identity has an independent covariance oracle; rank-deficient calibration fails
or uses an explicit supported-subspace policy.

### PAT-04 — Interpretable coordinates and stability

Separate the fitted operator/subspace from explanatory coordinates. Support
separate brain/target rotations with a possibly nondiagonal relationship matrix
and an admitted oblique option with scale, conditioning, factor-correlation,
and transformation rules.

Use varimax as the initial orthogonal interpretability baseline; select and
qualify the oblique criterion explicitly. Rotation alone does not establish
that explanatory factors are genuine neural sources.

Transform all affected patterns, filters, scores, covariances, and heads
consistently. Preserve the original fitting gauge: a display rotation does not
retroactively alter its sparsity penalty. Thresholding a rotated display cannot
be presented as leaving the fitted model unchanged.

Predictive rank is bounded by effective target rank and by `K−1` for `K`-class
mean discrimination. Label anatomical subdivisions and source hypotheses
separately. Distinguish stable axes, stable subspaces, and unstable solutions.

Acceptance: orthogonal/admitted oblique changes preserve predictions and the
forward relationship; ill-conditioned rotations fail admission; a binary task
cannot report several independent discriminant ranks.

### PAT-05 — Whole-brain and regional prediction from one fit

Derive predictors for a declared measurement without repeating spatial
optimization. For a hard ROI, use `A_R` and `Ψ_RR`, not cropped whole-brain
precision or weights. Generally use `A_M = L A` and `Ψ_M = L Ψ Lᵀ`.

These predictions inherit the learned task subspace; they do not claim the best
information a separately fitted local model could recover. Strict test-time
locality forbids preparation importing outside measurements. ROI selection and
tuning must not reuse assessment outcomes.

Return held-out local accuracy, loss, or explained-variance measures with their
target reduction and evaluation units. Deriving a local head is not evidence
that its predictive performance has been measured.

A general leg can be rank-deficient and need not preserve cheap covariance
structure. Reject unsupported singular measurements or reduce explicitly to a
certified image space. Report additional solve/materialization costs.

Acceptance: restricted prediction matches a dense marginal-covariance oracle;
correlated-noise tests distinguish restriction from cropped precision; omitted
local dimensions remain visible limitations, not unexplained failures.

### PAT-06 — Conditional information without voxelwise fitting

For admitted Gaussian targets, offer model-derived conditional information
using small quadratic forms after shared precision work. State conditioning,
units, and assumptions. Do not expose an assumption-free estimate or apply the
formula to categorical targets without separate justification.

With `P = Ψ⁻¹`, `K = (Φ⁻¹ + G)⁻¹`, and
`h_v = (PA)_v:ᵀ / sqrt(P_vv)`, the proposed formula is

\[
I(y;x_v\mid x_{-v})
=-\tfrac12\log(1-h_v^\top K h_v).
\]

Require admitted positive-definite covariances or an explicit equivalent
supported-subspace formulation. Numerical boundary violations need diagnostics,
not arbitrary clipping that changes the quantity.

Acceptance: maps match independent leave-one-voxel-out Gaussian calculations;
a suppressor may have zero forward loading and positive conditional importance;
no per-voxel model refitting occurs.

## 6. Inference and group-analysis requirements

### INF-01 — Explicit inferential contract

Every inferential result names its null, estimand, independent units,
conditioning/selection scope, evidence partition, nuisance model, and
multiplicity family. State which stages freeze, update sufficient statistics,
or refit under resampling.

Fast confirmation conditions on a representation learned from independent
discovery evidence. Full-procedure inference includes additional selection
uncertainty and is separately labeled and costed. CV folds are not independent
observations because their labels differ.

For max-statistic inference, every family member uses the same replicate
transformation and each admitted replicate is family-complete. A failed ROI
can leave a useful partial map but block family-level inference. Never omit
failed members silently or report a planned-`B` p-value after an arbitrary
budget stop. Sequential stopping requires its own admitted procedure.

Randomization invalidates all dependent preparation, tuning, and fits unless a
qualified invariance law permits reuse. Label-dependent split construction
must state whether the null conditions on realized splits or regenerates them.

Acceptance: unsupported temporal/randomization designs fail binding; frozen-stage
resampling cannot be reported as full-selection inference; incomplete families,
arbitrary budget stops, and stale randomized descendants cannot produce a
complete fixed-replicate inferential result.

### INF-02 — Voxel confirmation

Freeze target coordinates/rotations in discovery. On confirmation data, use
target-derived scores `T = Y C` and an admitted nuisance/error design to estimate
unpenalized forward projections with batched small-design linear models.
Support component-specific and omnibus voxel hypotheses.

Do not use scores derived from the same confirmation brain image as evidence
of task association. Sparse coefficients, selection frequencies, and Haufe maps
are not p-values. Zero forward projection differs from zero conditional
decoding contribution. Dependence and multiplicity require admitted analytic
or design-aware resampling; residualization alone does not validate permutation.

Acceptance: type-I error and stated familywise/FDR behavior meet the frozen
qualification protocol; small-design analytic results match independent
calculations; frozen-map confirmation never reruns discovery optimization.

### INF-03 — Component association and incremental prediction

Provide separate tests of generalizing association and incremental held-out
predictive value. For incremental value, refit the small reduced prediction
head on training evidence, then compare held-out losses with valid units.
This conditions on the frozen representation; it is not a full spatial refit
without that component.

Acceptance: correlated-component fixtures distinguish adjusted predictive value
from coefficient ablation; uncertainty uses admitted trials, blocks, runs, or
subjects, not overlapping CV-fold counts.

### INF-04 — Rank confirmation

Separate validation-based rank selection from rank significance. Confirm
association rank in independently learned candidate brain/target subspaces.
Higher-rank nulls preserve lower-rank association; global label permutations
alone do not establish sequential rank validity.

Report detectable dimensions within those subspaces. Rejection can support a
lower bound; nonrejection does not establish a global upper bound or count
anatomical networks.

Acceptance: zero-, one-, and multi-dimensional fixtures establish calibration
beyond the first dimension. If an admitted higher-rank test is unavailable,
expose selection only and leave the qualified-release gate open.

### INF-05 — Holdout exposure and adaptive analysis

Record runtime-mediated evidence exposure and subsequent selection decisions
in the outer workflow, linked to immutable specifications/results. Choosing a
model after seeing outer-fold scores is analysis selection even when every fold
fit was leakage-safe. Plots, diagnostics, and performance pilots can expose
holdouts too. Preserve exploratory use without calling the holdout untouched.

Record purpose, actor/role, scope, and selection dependency, using existing
workflow/provenance facilities rather than a new persistent campaign service.
External exposure may be unknown. Scoped adapters should receive only needed
read/fit capabilities; arbitrary Scala callbacks are not a security sandbox.
Distinguish declared from instrumented or constrained access assurance.

Acceptance: model selection after recorded outer-score access changes the
available confirmation claim; a training-only cost probe cannot read the
holdout; exposure facts survive supported handoff and persistence.

### GRP-01 — Commensurate subject solutions

Bind common task coordinates, anatomical measurements, subject identities, and
estimate uncertainty. Transform uncertainty with estimates. Equally shaped maps
are not sufficient evidence of commensurability.

Learn shared coordinates/alignment in separated discovery evidence; do not
align confirmation maps to maximize apparent agreement. Support native-space
subject fits and explicit common-space measurements. Offer operator/subspace
summaries when individual axes are unstable.

Acceptance: wrong task ordering or anatomical mapping fails binding; valid
coordinate changes preserve quantities with transformed uncertainty; simulated
subject differences remain visible.

### GRP-02 — Group outputs and claims

The first qualified release supports group analysis of commensurate subject
effects under an explicit mixed-effects or other admitted population model.
Return mean effects, uncertainty, between-subject variation, component
expression, and predictive summaries where defined. Preserve within-subject
component covariance when required; scalar standard errors may not suffice.

Distinguish mean task-linked effects, held-out-subject prediction, and prevalence
of information. Ordinary second-level accuracy tests do not automatically
establish prevalence. Joint fitting such as `A_s = M_s A_0 + Δ_s` is an extension,
not a substitute for working group confirmation.

Acceptance: multi-subject known-truth scenarios check interval coverage,
heterogeneity, uncertainty transport, and leakage; results state the population
or generalization claim actually tested.

## 7. Architecture and ownership

Dependency direction is conceptual first. Settle physical module boundaries
after the vertical slices exercise them.

| Layer/provider | Responsibility | Must not become |
| --- | --- | --- |
| Gale | Generic matrix/operator kernels, solvers, numerical contracts, portable implementations. | Private duplicated SVD/eigensolver/inverse families inside MVPA. |
| Multivar semantic core and admitted methods | Typed linear algebra and directly applicable general decompositions. | A home for ScalaFIM-specific crossnobis, fMRI, or MVPA orchestration. |
| resample4s | Ordinal reindexing, plans, coverage witnesses, deterministic random streams. | A replacement for domain exchangeability/generalization assumptions. |
| Existing response/locus infrastructure | Readable evidence and spatial domains to adapt and enrich. | A second competing generic axis/read framework. |
| ScalaFIM evidence/design layer | Scientific axis binding, columns/tables, relation capabilities, measurements, scoped designs, plan/results. | A dependent of Alder or a global method registry. |
| Predictive integration | Alder lifecycle, batched adapters, learners, tuning, predictions, assessment. | A duplicate training/leakage framework. |
| Relational integration | Crossform-derived pairing, query, closure, and adjoint laws; fMRI estimands. | The entire R object graph or eager Kronecker transport. |
| fMRI adapters and frames | Readouts, estimability, temporal scope, voxel/surface/atlas measurements, scattering. | Storage codecs disguised as scientific identities. |
| `mvpa` façade | Concise typed entry points lowering into inspectable specifications. | The ontology or a compatibility archive. |

Preserve the lower-level `dataset -> model -> fit` direction; generic fitting
must not depend on high-level MVPA. Prefer exact existing provider capabilities.
Missing generic capabilities need bounded upstream proposals and consumer
qualification, not speculative expansion of Multivar with domain methods.

**Provider admission is an early gate.** Verify pinned APIs, JVM/JS behavior,
identity interoperability, matrix batching, artifact access, and resource
contracts. If support is missing, land narrow provider work before completing
that dependent slice. A shadow implementation under new API names does not
count as integration.

### Intended user journeys

These are workflow sketches, not promises that the proposed API names exist.

```text
Classification:
  observations + categorical target
    -> whole domain or measurement frame
    -> leave-one-run-out preparation/training/assessment
    -> keyed predictions, metrics, receipts

Relational analysis:
  runwise fMRI relations + residual capabilities
    -> explicit cross-run pairing + measurement frame
    -> crossnobis / RSA / contrast queries
    -> typed forms, scores or maps, receipts

Global pattern analysis:
  observations + identified multivariate targets + anatomical graph
    -> discovery fit and independently validated model selection
    -> patterns, filters, task relationships, covariance, stability
    -> whole-brain or restricted-measurement prediction
    -> independent voxel/component/rank confirmation
    -> commensurate subject estimates and group analysis
```

Each journey requires a runnable documented example. Users can inspect/modify
the specification before execution, request only needed outputs, and understand
why an operation is rejected.

## 8. Performance, numerical, and operational requirements

### PERF-01 — Structural scalability

The flagship path must not require dense `p × p` brain covariance or `p × q`
coefficients. The initial covariance candidate is residual `Ψ = D + U Uᵀ`, with
positive diagonal `D` and small noise rank `h`. Use factorized precision
application and operator products; count covariance fitting and tuning costs.

The target large-product cost per principal pass is proportional to
`n(p+q)r`, plus graph, noise-rank, and solver work. This is not a bound on total
iterations or a complete tuning experiment. Model storage scales with factors
and graphs, not quadratic voxel dimensions.

Hard ROI restriction preserves diagonal-plus-low-rank covariance; arbitrary
measurements do not, because `L D Lᵀ` can be dense. Capability/cost checks must
reflect that distinction. Cache additive summaries where valid; do not claim
every local query is constant time.

Compile toward requested queries. For admitted `H = U Vᵀ`, the relational
contraction is `tr((Uᵀ B_L) K (Vᵀ B_R)ᵀ)`; compute it without materializing
all effect geometry. For equal all-distinct pairing, sum-products minus self-products
can replace explicit pair enumeration. These are scoped rewrite candidates,
not universal shortcuts: qualify cancellation, weights, temporary dimensions,
and metric application against direct oracles. Dense metrics invalidate simple
per-feature additive shortcuts. Do not retain full geometry for every
measurement merely for hypothetical future queries.

### PERF-02 — Measured budgets, not a “lightning fast” label

M0 must freeze hardware, datasets, accuracy/conditioning tolerances, warm/cold
boundaries, and latency/memory budgets before optimization. These are
**proposed working targets, not measured results**:

| Workload | Proposed target on the agreed JVM reference workstation |
| --- | --- |
| Fixed-hyperparameter global fit: `n=1,000`, `p=100,000`, `q=256`, `r≤16`, `h≤16`, bounded-degree anatomical graph | At most 5 minutes and 8 GiB peak process memory, including covariance fitting. |
| Derive one hard-ROI prediction head, up to 5,000 voxels, from a resident admitted fit | At most 1 second; separately report evidence reads and prediction time. |
| All Gaussian conditional-information values from resident factors at reference dimensions | At most 10 seconds, including required precision products. |
| 1,000 admitted rank-confirmation resamples of frozen projections, confirmation `n≤500`, each projected dimension `≤16` | At most 30 seconds; this is not voxelwise multiplicity timing. |

Targets may be revised during M0 with measurements and reasons, before judging
implementation against them. Also freeze budgets for voxelwise multiplicity,
complete nested tuning, I/O, and Scala.js reference workloads. A milestone
cannot report a budget met without a reproducible receipt. Scientific parity
applies on both platforms; identical wall-clock speed is not required.

Benchmark tuned searchlights, a strong whole-brain linear baseline, and a
low-dimensional baseline such as thresholded PLS where available. Match splits,
preparation scope, target metrics, tuning budget, and hardware; disclose unmatched
costs. Distinguish local-model from shared-subspace estimands. No blanket
superiority claim follows from one task.

Resource estimates include simultaneously live objects, retained outputs,
workers, source buffers, and backend scratch, or label excluded costs unknown.
Check shape/byte arithmetic for overflow. State which limits can be enforced
by owned allocations and which require process isolation. Operator-native is
not automatically the cheapest route; small dense blocks may be preferable.

A cost probe is explicit budgeted execution over synthetic or training evidence,
with reads, operator applications, fit counts, and exposure recorded. Its
latency sample is not a global bound or model-quality evidence. Resource limits
cannot silently change the population, metric, target, or randomization count.

### NUM-01 — Independent numerical evidence

Use small dense oracles, analytic fixtures, and external parity fixtures when
matching established behavior. A second route through the same helper is not
independent evidence. Require explicit tolerances, conditioning diagnostics,
and scale-aware checks.

### OPS-01 — Execution and artifacts

Provide bounded-memory frame traversal, deterministic scheduling, cancellation,
owned-resource lifetimes, and reported partial failure. Failed measurements
remain visible in exports and denominators; aggregate scores cannot silently
omit them.

Persisted results, if offered, preserve space/value/plan identity and artifact
interpretation through existing archive/estimate facilities where appropriate.
Do not add an unrelated persistence registry to the core.

A supported durable profile must reopen in a fresh process without its fitter,
preserving identity, validity, coverage, and qualifications. Unsupported profiles
remain explicitly in-memory; arbitrary bytes are not a universal estimate type.
Bounded event/inspection views reference large payloads instead of dumping them.
Publication is a separate authorized operation, with privacy-aware projections.

### OPS-02 — Retry and recovery semantics

Work-unit addresses derive from immutable plan, split, replicate, stage, and
measurement coordinates, not workers or completion order. Retries cannot add a
second reducer contribution. Checkpoint/resume, when offered, uses committed
unit boundaries and verifies source revisions, scoped fits, random streams,
reducer state, and execution compatibility. Otherwise refuse resume explicitly.

Distinguish deterministic assignments, tolerance-compatible values, and bitwise
identity; a fixed seed does not guarantee all three across backends/reductions.
Reuse existing pipeline/scheduling facilities, not a new distributed runtime.

Acceptance: cancellation/retry fault injection preserves completed-unit coverage
without duplicates; supported resume agrees with uninterrupted execution under
its declared reproducibility contract; incomplete inference remains incomplete.

## 9. Rapid replacement and deletion policy

This is a deliberate breaking scientific-API redesign. The migration
deliverable is a new active implementation with the old route removed, not a
deprecation annotation.

### Rules

1. Inventory supported methods and in-repository consumers at M0. Assign every
   item a destination, owner, parity gate, and deletion milestone. Useful
   behavior cannot disappear because its wrapper is inconvenient.
2. Switch production adapters, tests, examples, benchmarks, docs, and exports
   for a migrated workflow in the same delivery slice.
3. Delete the obsolete implementation once those consumers pass. Move useful
   kernels behind the new compiler; do not retain duplicate orchestration.
4. Private bridges may support an in-progress cutover only. Each has an explicit
   removal gate, no new callers, and a maximum lifetime of the immediately
   following migration milestone. None ships in the foundation release.
5. Add no new public compatibility wrappers, flags, or old-enum cases. Delete
   common old machinery with its last scheduled workflow, no later than M3.
6. Breaking release notes give old-to-new examples. Known external consumers
   receive migration guidance, not an indefinite parallel API. Exceptions need
   a named owner, consumer, expiry, and approval.
7. Preserve scientific fixtures and useful rationale. Git history recovers
   deleted source; do not create a `legacy/` source tree. Replace historical
   links to removed paths with immutable revision references when needed;
   documentation links are not a reason to retain obsolete implementations.
8. Gate deletion with source-reference/dependency scans and passing tests. A
   renamed class delegating to the old engine does not count as removal.

### Disposition of current concepts

| Current concept | Destination | Removal gate |
| --- | --- | --- |
| `RoiAnalysis`, fold-required variants, ROI contexts | Typed estimands, scoped designs, compilers | Remove implementations per slice; shared hierarchy gone by M3. |
| `MvpaEngine`, `MvpaTask`, universal `MvpaResult` | Bind/compile/execute and typed results | Last workflow cutover, no later than M3. |
| `RoiPayload`, `RoiAnalysisResult` | Method-owned results and export views | No new cases now; delete by M3. |
| MVPA `Response` and contexts | Axis-bound columns, multivariate target tables, effect/model queries | Predictive/relational users move in M1–M2; remaining uses gone by M3. |
| `Fold` / `FoldPlan` | Axis-bound purpose-specific resample4s designs | Remove per slice; final definitions gone by M3. |
| `FeatureSet`, `FeatureSetPlan`, special center/kind | Selection measurements, lazy frames, descriptors | Replace public builders in M1; remaining old helpers gone by M3. |
| `PatternSource` feature-selection ontology | Typed evidence plus measurement composition | Remove as primary API in M1; no public legacy façade after M3. |
| `PatternMatrix`, `PatternOperator` | Storage/operator kernels behind typed adapters | Keep only useful implementations, not duplicate scientific identities. |
| `MvpaStream`, `RoiOutcome` | Typed traversal and measurement-local outcomes | Port semantics in M1–M2; old wrappers gone by M3. |
| `MetricVector` | Optional summarization/rendering view | Keep only if useful without the deleted universal result contract. |
| `OneShotDataset`, one-shot entry points | fMRI evidence adapters and operator compilation | Relational paths M2; remaining paths by M3. |
| Canonical parallel result/engine wrappers | Typed canonical/global estimands and artifacts | M3, with parity evidence. |

“MVPA `Response`” means the target ADT in `scalafim.fmri.mvpa`, **not** the
separate `modules/response` evidence/read module. Removing ROI-centered
orchestration does not remove ROI or searchlight functionality.

## 10. Delivery milestones and acceptance gates

Assign dates and named owners when approving the sketch. M1 and M2 can develop
concurrently after M0, but must share the same core rather than fork it.

| Milestone | Deliverable | Exit criteria |
| --- | --- | --- |
| **M0 — Contract and admission** | Architecture constitution; full method/consumer inventory; provider capability spike; identity/reindexing and metadata-inspection prototypes; benchmark protocol. | Resolve identity layers, typed orientation, response/locus bridge, provider pins and matrix adapters. Assign every old method a cutover gate. Approve budgets and calibration protocol; freeze exact migration baselines. |
| **M1 — Predictive replacement** | Typed evidence/design/frame/result path; existing Swift centroid under leave-one-run-out; remaining admitted predictive heads. | Metadata-only inspection, training/read scopes and exposure records, keyed OOF results, dense/operator parity, diagnostics, local failures/scattering, JVM/JS gates. Delete replaced routes and migrate examples. |
| **M2 — Relational replacement** | Operator-native relations, explicit pairing/metric, crossnobis/RDM/RSA, first-order query reuse. | Preserve identity-metric baseline; direct/adjoint/reversal laws, origin/metric claim checks, explained reuse/invalidation, visible failures. Delete replaced wrappers and old-only spatial helpers. |
| **M3 — Global fit and foundation cutover** | Global/canonical artifacts; initial pattern-first classification and multiresponse fit with restricted-ROI prediction; remaining old methods migrated or explicitly dispositioned. | Third slice needs no singleton ROI/parallel results; small-model oracles, resource/probe and retry contracts pass. Supported persistence/resume qualify or explicitly refuse. Retired orchestration, enums, bridges and dead consumers are gone. Foundation may release; method remains experimental pending M4–M5. |
| **M4 — Interpretation and confirmation** | Rotations/oblique coordinates, Haufe diagnostic, conditional information, voxel/component/rank confirmation. | Suppressor/rotation/restriction oracles, rank limits, adaptive-exposure boundaries, family-complete randomization and higher-rank calibration. Unsupported claims fail closed; confirmation budgets met. |
| **M5 — Group and qualification** | Subject confirmation, group model/uncertainty transport, scientific and performance reports. | Group coverage/heterogeneity scenarios, JVM/JS parity, full timing/memory receipts, documented limitations. Claims match evidence; qualified-release examples/docs complete. |

The foundation requires **three** distinct workflows sharing its core: a
validated classifier, operator-native crossnobis/RSA, and a whole-brain pattern
fit with a derived ROI predictor. Two local-analysis examples are insufficient
proof of the global architecture.

Each implementation task gets a bounded packet in the existing task store:
objective, owning seams/revision, inputs, allowed changes/non-goals, dependencies,
laws/scenarios, verification commands/budgets, outputs, and next handoff.
Read the relevant packet and expand dependencies as needed; do not require a
whole-repository re-audit or build for every owner-local change. No second tracker
or design registry is needed.

### Required release laws

Foundation gates cover the core and migrated workflows. The full law set below
is required for the qualified release; experimental capabilities cannot inherit
that qualification merely by appearing in the foundation release.

Keep five evidence categories separate: semantic conformance, numerical
correctness, protocol/scope correctness, measured resource behavior, and
scientific calibration. Each scenario still returns the existing harness's one
`ScenarioResult`, with typed observations recording these facts. Not tested is
not passing; numerical parity does not establish inferential validity. An
expected refusal passes only when its code, scope, and preserved state match.

| Area | Gate |
| --- | --- |
| Identity | Shape equality is insufficient; changed order requires reindexing; decoded descriptors agree with nominal witnesses. |
| Composition | Restrictions, measurements, and adjoints match independent explicit operations. |
| Representation | Dense/operator values, sufficient statistics, and results agree; no hidden materialization. |
| Design | Exact-once coverage, ordered pairing, explicit reduction, training exclusion, lawful random streams. |
| Inspection/reuse | Metadata-only access is enforced; unknowns are visible; invalidation follows exact dependencies and explains reuse. |
| Interpretation | Forward/filter distinction, Haufe identity, rotation invariance, regional covariance restriction. |
| Inference | Type-I error, interval coverage, multiplicity, higher-rank nulls, and independent-unit semantics meet the frozen protocol. |
| Group | Identity/uncertainty transport, heterogeneous effects, and held-out-subject scope are verified. |
| Operations | Budgets account for the live working set; retries do not duplicate contributions; supported persistence reopens without the fitter. |
| Extensibility | New typed estimand needs no core enum edit or second result hierarchy. |
| Removal | No active source/example/test references to retired APIs or renamed delegation to old machinery. |
| Portability | Relevant shared/integration tests pass on JVM and Scala.js; compile is warning-clean. |

### Scientific qualification scenarios

Include known truth for null signal, noise suppressors, redundant regions,
diffuse patterns, fine-scale signs, incorrect adjacency, covariance/rank
misspecification, ROI-only dimensions, unstable rotations, temporal dependence,
nuisance confounding, and heterogeneous subjects.

Report prediction, pattern/support recovery, component/subspace stability,
calibration, and cost separately. Freeze seeds, replicate counts, Monte Carlo
uncertainty criteria, and minimum relevant effects in M0. Aggregate scores
cannot hide failed scenarios. Real data demonstrate usefulness but do not
replace known-truth localization or calibration evidence.

Follow the [scenario parity harness](scenario-parity-harness.md): one explicit
scenario result, clean `Pass` by default, visible caveats requiring policy
approval. Run Scala.js suites in bounded sbt batches under
[AGENTS.md](../../AGENTS.md); JVM-only success is not product admission.

### Additional adversarial acceptance cases

| Case | Required observation |
| --- | --- |
| Inspect/plan over a poison source | No neural reads, hidden payload hashing, or fitting; data-dependent facts remain unknown. |
| Source changes at the same locator; target or fold changes | No stale cache hit; exact invalidation and any admitted reuse are explained. |
| Distinct beta rows share acquisition/preparation dependencies | No automatic independent-endpoint claim; the relevant origin path remains visible. |
| Metric learned from one cross-product endpoint | No automatic unbiasedness claim; conditional assumptions or independent metric evidence are required. |
| New RSA query or display label | Reuse only compatible retained products; update comparison/multiplicity or presentation as appropriate. |
| Memory overflow or unknown backend scratch | Refuse the strict route or choose an admitted equivalent route, not a different ROI/metric. |
| Cost probe tries to read holdout evidence | Refuse unapproved access; actual exposure and budget use are recorded. |
| Analyst/agent selects after seeing outer scores | Record adaptive selection; the original untouched-holdout claim is unavailable. |
| One max-statistic family member fails | Retain usable local results but block incomplete family inference; transformations match within each replicate. |
| Crash/retry/cancel; supported resume | No duplicated reducer contribution; exact completed-unit coverage and compatible continuation. |
| Supported artifact reopened without fitter | Identity, validity, reduction, and qualifications preserved; unsupported profiles explicitly unavailable. |
| New typed method or decoder | No core enum/runner branch; when decoding is supported, native and decoded specifications normalize identically. |

Exercise inspection and diagnostics on actual analyst/agent tasks: repair an
axis mismatch, distinguish absent data from missing residual capabilities, add
an RSA query without refitting, change a fold safely, and explain an unavailable
claim. Judge correctness first, then unnecessary reads/refits, failed attempts,
context/output volume, and accurate reporting of coverage. This is a native API
usability gate, not a requirement to build an agent server.

## 11. Risks and decisions to close

| Risk/decision | Required resolution | Gate / responsible role |
| --- | --- | --- |
| Rich identity creates an unusable API | Prototype runtime-loaded axes, dependent frames, target tables, and concise façades before freezing signatures. | M0 / core API owner |
| Provider lacks lifecycle/typed bridge support | Demonstrate pinned JVM/JS integration and artifact access; land narrow support first. | M0 / integration owner |
| Forward fit predicts poorly under misspecification | Tune held-out task loss; compare discriminative/low-rank baselines; retain diagnostic and experimental status. | M3–M5 / method owner |
| Spatial envelope/rotation yields unstable explanations | Evaluate fine-scale recovery and stability; report stable subspaces when axes fail; validate defaults independently. | M4 / method owner |
| Residual covariance limits validity or speed | Qualify diagonal-plus-low-rank estimation and sensitivity; admit richer models only with solves/resource budgets. | M3–M5 / numerical owner |
| Fast higher-rank/dependent-data inference is unqualified | Validate explicit procedures; keep significance unadmitted rather than substituting naive permutations. | M4 / inference owner |
| Group matching manufactures agreement | Freeze shared coordinates/alignment scope; transport uncertainty; retain subspace alternatives. | M5 / group owner |
| Migration leaves duplicate machinery | Per-workflow deletion gates, no new old-API callers, bounded private-bridge lifetime, release scans. | M1–M3 / migration owner |
| Existing behavior is missed | Inventory cross-decoding, probabilistic outputs, operator ridge, feature models, samplewise RSA, constrained/canonical variants, exporters and dataset workflows; port or explicitly approve disposition. | M0 and M3 / migration owner |
| Speed targets distort scientific comparison | Freeze matched workloads/costs before optimization; report failed targets separately from estimator limitations. | M0 and M5 / qualification owner |

Open choices do not reopen the main commitments: typed evidence, distinct
designs, measurement composition, two scientific branches, rich global
artifacts, distinct maps, scoped inference, and prompt removal of replaced APIs.

## 12. Source anchors and precedence

This sketch uses the two intake documents and read-only inspection of the
checkout. It does not certify provider readiness or claim proposed APIs exist.

- [Supplementary review record](unified-mvpa-supplementary-review.md): provenance
  and adopt/adapt/defer decisions for the four supplied follow-up documents.

- [Part 1 method notes](unified-pattern-first-mvpa-notes.md): full model,
  Haufe derivation, local prediction, rotation, inference, and caveats.
- [Part 2 architecture notes](unified-mvpa-architecture-notes.md): original
  evidence proposal, ownership, and laws. Section 9 here supersedes compatibility
  retention; Sections 3–4 refine identity and global-result contracts.
- [Current engine/contexts](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/MvpaEngine.scala)
  and [closed ROI results](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/RoiResult.scala):
  the orchestration/result center to replace, not extend.
- [Response axes](../../modules/response/shared/src/main/scala/scalafim/response/Axis.scala)
  and [response sources](../../modules/response/shared/src/main/scala/scalafim/response/ResponseSource.scala):
  lower-level contracts to bridge, distinct from the MVPA target ADT.
- [One-shot source](../../modules/mvpa-fit/shared/src/main/scala/scalafim/fmri/mvpa/fit/OneShotDataset.scala)
  and [canonical entry point](../../modules/mvpa-fit/shared/src/main/scala/scalafim/fmri/mvpa/fit/CanonicalEffectMvpa.scala):
  operator/sufficient-statistic work to preserve through typed compilers.
- [Existing centroid implementations](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Classification.scala)
  and [operator geometry](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/OperatorRsa.scala):
  concrete migration baselines; adapter work must preserve their declared math.
- [Vision](../../vision.md), [module relationships](../module-relations.md),
  [build](../../build.sbt), and [working contract](../../AGENTS.md): repository
  boundaries, pins, portability, and release discipline.

Next: a bounded M0 implementation plan with named owners, live consumer
inventory, provider admission evidence, and exact acceptance commands. Do not
start by generating all proposed modules or wrapping the old engine in new
names.
