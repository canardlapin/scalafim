# Typed Multivariate Inference

## Decision

Add a cross-platform `inference` module whose job is perturbation-based
inference for fitted multivariate structures.

The module should depend on `multivar` and `linalg`, and nothing above them:

```text
linalg -> multivar -> inference
```

It should support one-table, paired-table, and later multiblock contexts through
small typed capabilities. It should not become a general statistics utility
module, a second multivariate fitting module, or a Scala transcription of
`multifer`'s S3 adapter registry.

The central abstraction is:

> An inferential program combines an ordered fitted structure, an invariant
> target statistic, a design-preserving null action, and a continuation rule.

Bootstrap stability is a second interpretation of the same fitted structure.
It shares units, refitting, deterministic replicate planning, and provenance,
but it is not significance testing and must not be represented as if it were.

## Why this boundary fits ScalaFIM

`multivar` already owns the mathematical objects that inference should consume:

- `DualityDiagram` and `PairedDualityDiagram` provide metric-aware data
  geometry;
- `MvSpace`, `MvMetric`, and `MvMap` make domains, metrics, and projections
  explicit;
- `PairedGmd` gives PLSC, CCA, and RRR one metric-aware paired spine;
- `Spectrum` distinguishes covariance singular values, canonical
  correlations, eigenvalues, and ordinary singular values;
- `BiProjection` and `CrossProjection` expose fitted coordinates without
  leaking solver details;
- `RowWhitening`, `RowProjector`, and `EffectOperator` provide the nuisance and
  row-conditioning geometry needed by valid residual randomization;
- current R fixtures already anchor PLSC, CCA, and RRR fitting behavior.

Those are fitting and geometry concepts. They should stay in `multivar`.

The missing layer owns a different set of concepts:

- inferential targets and null hypotheses;
- exchangeability and sampling-unit designs;
- null actions and bootstrap actions;
- sequential ordered-root tests;
- deterministic Monte Carlo budgeting and stopping;
- latent units, including non-identifiable subspaces;
- replicate alignment and stability summaries;
- validity, assumptions, Monte Carlo receipts, and provenance.

Keeping these in a new module prevents `multivar` from turning into a mixture of
linear algebra, experimental design, randomization policy, and report schemas.

## Lessons from `multifer`

The R package is the right scientific reference and the wrong surface to copy.

### Preserve

1. **The scaffold, not one universal statistic.** Ordered latent objects,
   removal of already-claimed structure, a matched null action, and stopping at
   the first non-rejection form a coherent shared ladder. Predictive methods may
   use the scaffold while retaining a predictive-gain target.
2. **Units rather than component numbers.** A separated axis is an identifiable
   singleton unit; tied or near-tied axes form an unoriented subspace unit.
   Stability and evidence attach to the unit.
3. **Design-matched perturbations.** Row permutations, paired-block
   independence actions, within-block permutations, cluster resampling, and
   nuisance-adjusted residual randomization are different mathematical
   operations, not flags on one permutation function.
4. **Exact reduced updates are capabilities.** Core-space refits and lift-back
   formulas should be reusable exact execution strategies, never hidden
   method-specific shortcuts.
5. **Evidence contracts are explicit.** Exact, conditional, asymptotic, and
   heuristic claims must remain visible, along with checked and declared
   assumptions.
6. **Stability is not significance.** Bootstrap ratios, selection frequency,
   principal angles, and score intervals are stability/evidence summaries.
   They are not automatically p-values.

### Improve in Scala

1. Do not encode `geometry`, `relation`, `design`, targets, capabilities, and
   validity as strings joined by a runtime capability matrix.
2. Do not use one broad adapter with many optional callbacks. Express required
   behavior as small traits and compose only the capabilities an analysis
   needs.
3. Do not make invalid geometry/relation combinations constructible and defer
   all checks to a top-level dispatcher. Use closed target families, smart
   constructors, and compiler-resolved compatibility evidence where the
   relationship is static.
4. Do not put functions or fitted objects in a durable plan. Keep a serializable
   `InferenceSpec`, compile it with typed capabilities into an in-memory
   `InferenceProgram`, and interpret that program separately.
5. Do not freeze a single wide result table full of empty fields. Use a compact
   unit-centered result plus typed optional evidence channels that distinguish
   `NotRequested`, `Unavailable(reason)`, and `Computed(value)`.
6. Do not build a monolithic `infer()` method. Compilation, replicate planning,
   execution, reduction, and result assembly should be separately testable.

## Mathematical model

The inference module should not pretend that every named method is the same.
It should unify what is genuinely shared.

### Ordered fitted structure

An `OrderedFit[F]` capability exposes the method-independent observable part of
a fitted structure:

```scala
trait OrderedFit[F]:
  def spectrum(fit: F): OrderedSpectrum
  def domains(fit: F): DomainBundle
  def coordinates(fit: F, domain: DomainId): Either[InferenceError, MvMap]
```

`OrderedSpectrum` preserves the semantic kind from `multivar.Spectrum`; it is
not a bare vector called `roots`. The ordering and finite/nonnegative invariants
are checked once.

The first given instances should cover:

- `PcaFit` and `GenPcaFit` as one-table variance structures;
- `PlscFit` as a paired covariance structure;
- `CcaFit` as a paired correlation structure.

RRR should not be admitted through the ordered-root protocol merely because it
has singular values. It belongs to a predictive target protocol unless a
specific ordered-root hypothesis is justified.

### Inferential target

A target owns the observed statistic and its invariance contract:

```scala
trait InferenceTarget[F, A]:
  def label: TargetLabel
  def observe(fit: F, unit: LatentUnit): Either[InferenceError, A]
  def ordering: Alternative[A]
  def invariance: TargetInvariance
```

Initial target families:

- ordered one-table inertia/variance roots;
- ordered paired covariance roots;
- ordered canonical correlations.

Later, and deliberately separate:

- generalized-eigen roots;
- predictive gain or loss;
- method-native feature evidence.

Sign and Procrustes alignment must never be required to compute a significance
statistic. A significance target is invariant to arbitrary orientation of an
axis, and subspace targets are invariant to orthogonal rotation inside the
unit.

### Sampling design and null action

The design should factor genuinely independent concerns instead of enumerating
every cross-product:

```scala
final case class ResamplingDesign private (
    units: SamplingUnits,
    exchangeability: Exchangeability,
    conditioning: Conditioning
)

enum SamplingUnits:
  case Rows
  case Clusters(partition: ClusterPartition)

enum Exchangeability:
  case Unrestricted
  case WithinBlocks(partition: RowPartition)
  case WithinStrata(partition: StrataPartition)

enum Conditioning:
  case Unadjusted
  case Nuisance(projector: RowProjector, whitening: Option[RowWhitening])
```

Smart constructors validate coverage, nesting, row counts, and rank. For paired
data, which domain is broken belongs to the null hypothesis, not the sampling
design:

```scala
enum PairedIndependenceNull:
  case BreakX
  case BreakY
```

A `NullAction[D]` is a lawful action on data:

```scala
trait NullAction[D]:
  def label: NullLabel
  def validity: ValidityClaim
  def draw(data: D, replicate: ReplicateId, rng: RandomSource)
      : Either[InferenceError, D]
```

The compiler may produce a `NullAction` only when it has evidence that the
target family, data geometry, and design are compatible. Static Scala callers
receive missing-given errors for unsupported combinations; dynamically loaded
specifications receive a typed `UnsupportedProblem` value.

### Sequential continuation

Ordered-root testing is a state machine, not a loose loop:

```scala
enum LadderDecision:
  case Continue(selected: LatentUnit)
  case Stop(firstUnselected: LatentUnit)
  case BudgetExhausted(at: LatentUnit)

trait Continuation[S, F]:
  def initial(data: S, fit: F): Either[InferenceError, LadderState[S, F]]
  def remove(state: LadderState[S, F], unit: LatentUnit)
      : Either[InferenceError, LadderState[S, F]]
```

Removal is family-specific. PCA/PLSC/CCA can share the ladder without sharing a
fake universal deflation formula.

### Latent units

Use an algebraic value rather than a table row:

```scala
enum LatentUnit:
  case Axis(id: UnitId, component: ComponentIx)
  case Subspace(id: UnitId, components: ComponentSet)
```

`ComponentSet` is non-empty, sorted, unique, and within the fitted rank.
Identifiability is derived: axes are orientable; multi-axis subspaces are not.

Unit formation is explicit policy:

```scala
enum UnitPolicy:
  case SingleAxes
  case GroupNearTies(relativeGap: RelativeGap)
  case Declared(groups: NonEmptyVector[ComponentSet])
```

No hidden `0.01` tie rule should change the inferential object without being
present in the plan and result provenance.

## Scala 3 architecture

### Capabilities, not adapter registries

Use small traits with associated types or type parameters:

```scala
trait Refit[D, F]:
  def fit(data: D): Either[InferenceError, F]

trait OrderedFit[F]:
  def spectrum(fit: F): OrderedSpectrum
  def domains(fit: F): DomainBundle

trait Deflatable[D, F]:
  type State
  def begin(data: D, fit: F): Either[InferenceError, State]
  def remove(state: State, unit: LatentUnit): Either[InferenceError, State]

trait StabilityView[F]:
  def loadings(fit: F, domain: DomainId): Either[InferenceError, DoubleMatrix]
  def scores(fit: F, domain: DomainId): Either[InferenceError, DoubleMatrix]
```

An analysis asks only for what it consumes:

- component tests require `Refit`, `OrderedFit`, `InferenceTarget`, a compiled
  `NullAction`, and `Deflatable`;
- bootstrap stability requires `Refit`, `OrderedFit`, a `BootstrapAction`, and
  `StabilityView`;
- a root-only test does not require loadings or score hooks;
- a stability-only analysis does not require a null statistic.

This is the Scala replacement for `multifer`'s optional callback bundle.

### Specs, programs, and interpreters

Keep three layers distinct:

1. `InferenceSpec` is immutable data: target, design, unit policy, Monte Carlo
   policy, bootstrap policy, and requested evidence. It is safe to serialize.
2. `InferenceCompiler` combines a spec with concrete data/fit capabilities and
   produces a typed `InferenceProgram[D, F]`. This is the only place where
   compatibility and assumption checks are resolved.
3. `InferenceExecutor` interprets the program. A separate lowering can emit
   deterministic replicate jobs for parallel or distributed runtimes.

No scheduler, filesystem, dataset backend, Spark type, or open resource belongs
in shared `inference` code.

### Runtime extensibility

The target and execution protocols should be traits so a new family can be
provided by a downstream module. Closed enums remain appropriate for semantic
sets whose exhaustive handling matters inside the core: alternatives, unit
kinds, validity claims, stopping reasons, and built-in resampling policies.

This hybrid is preferable to either extreme:

- one sealed mega-enum would force every future method into this module;
- one untyped registry would recover the ambiguity of the R surface.

### Errors and absence

Use one `InferenceError` enum with precise cases for:

- invalid dimensions, counts, probabilities, or identifiers;
- incompatible target/design/protocol combinations;
- invalid block, cluster, or stratum partitions;
- failed checked assumptions;
- non-finite observed or replicate statistics;
- rank loss or a unit beyond available rank;
- replicate failure with its `ReplicateId`;
- exhausted Monte Carlo budget;
- unsupported evidence requested from an otherwise valid protocol.

No public API should throw for an ordinary invalid analysis request.

Use a typed evidence state instead of ambiguous `Option` fields:

```scala
enum Evidence[+A]:
  case NotRequested
  case Unavailable(reason: UnavailableReason)
  case Computed(value: A)
```

## Monte Carlo and bootstrap execution

### Deterministic random streams

Introduce a small cross-platform `RandomSource` capability with deterministic
splitting by `ReplicateId`. A replicate's random stream must be a pure function
of `(rootSeed, replicateId)`, so local, batched, and distributed execution are
bit-stable in replicate assignment.

Do not use global `scala.util.Random` and do not let scheduling order determine
the samples drawn.

### Fixed and sequential Monte Carlo

Implement the fixed primitive first:

```text
p = (1 + number of null statistics at least as extreme as observed) / (B + 1)
```

Then implement batched Besag-Clifford early non-rejection as a policy over the
same accumulator. The result must record:

- allocated and consumed draws;
- exceedance count;
- batch schedule;
- stopping boundary and reason;
- p-value and Monte Carlo standard error;
- seed derivation/version.

The global budget allocator is a separate pure state transition. The ladder
must not own mutable global budget state.

### Bootstrap as a replicate program

Bootstrap execution has the shape:

```text
design -> replicate indices -> refit -> unit matching -> orientation/subspace
alignment -> sufficient summaries -> reduction
```

The default implementation may retain replicate fits for small jobs, but the
core reducer should be able to consume streaming sufficient summaries so a
future distributed interpreter does not need all fits in memory.

Exact reduced-space updates are advertised by an optional capability:

```scala
trait ExactRefitReduction[D, F, C]:
  def core(data: D, fit: F): Either[InferenceError, C]
  def update(core: C, action: ResamplingAction): Either[InferenceError, F]
  def lift(fit: F): Either[InferenceError, F]
```

The result provenance must say whether a full refit or a proved-exact reduced
update was used. Approximate screening, if ever added, is a different explicit
execution mode and may not silently replace exact inference.

## Results

The primary result is unit-centered:

```scala
final case class InferenceResult(
    problem: ProblemSummary,
    units: NonEmptyVector[LatentUnitResult],
    tests: Evidence[Vector[UnitTest]],
    stability: Evidence[StabilityResult],
    validity: ValidityReport,
    monteCarlo: Evidence[MonteCarloReceipt],
    provenance: InferenceProvenance
)
```

Each `LatentUnitResult` carries its component set, spectrum values,
identifiability, and selection state. `UnitTest` carries a typed `PValue`,
statistic, alternative, null label, validity claim, and stopping record.

`StabilityResult` is separated into:

- variable/loading stability by domain and unit;
- score stability by domain and unit;
- subspace stability using principal angles;
- alignment diagnostics for orientable axes.

Feature-level null evidence, multiplicity correction, and method-native
importance measures should initially be sidecar result types. They should join
the primary result only after their validity and ownership are coherent.

`ValidityReport` includes:

- `ValidityClaim`: `Exact`, `Conditional`, `Asymptotic`, or `Heuristic`;
- declared assumptions;
- executed assumption checks and their outcomes;
- any downgrade and its reason.

Validity is never inferred from a method name or adapter id.

## Source layout

```text
modules/inference/
  README.md
  shared/src/main/scala/scalafim/inference/
    Types.scala             opaque ids, counts, Alpha, PValue, seeds
    InferenceError.scala
    Units.scala             LatentUnit, ComponentSet, UnitPolicy
    Validity.scala          claims, assumptions, and executed checks
    Design.scala            sampling units, blocks, strata, conditioning
    Targets.scala           target protocol, alternatives, invariance
    Capabilities.scala      Refit, OrderedFit, Deflatable, StabilityView
    NullActions.scala       compiled lawful null actions
    BootstrapActions.scala
    StructuredActions.scala structured and conditioned null actions
    Problems.scala          InferenceSpec and ProblemSummary
    Compiler.scala          spec + capabilities -> typed program
    RandomSource.scala      deterministic splittable shared RNG
    MonteCarlo.scala        fixed/sequential accumulators and receipts
    Ladder.scala            ordered state machine
    Alignment.scala         matching, sign alignment, principal angles
    Stability.scala         streaming reducers and typed summaries
    Execution.scala         replicate jobs, provenance, associative reducers
    ExactReductions.scala   optional proved-exact core refits
    OrderedFamilyProtocols.scala CCA and generalized-eigen protocols
    PredictiveInference.scala held-out RRR predictive gain
    BlockFamilyProtocols.scala CPCA and multiblock protocols
    FeatureEvidence.scala   method-native feature sidecars and multiplicity
    MultivarProtocols.scala given instances for multivar fit families
    Executor.scala          local pure interpreter
```

Keep hot assignment, inner-product, covariance, and reduction loops on primitive
arrays. Solver contracts remain in `linalg`; `inference` must not grow private
SVD, QR, inverse, or eigensolver families.

## Delivery plan

Current implementation status (2026-07-17): Phases 0 through 6 are complete.
The frozen evidence bundle, typed compiler, deterministic Monte Carlo state
machines, rolling budget, unit formation, shared ladder interpreter, exact
PCA/PLSC protocols, deterministic row/cluster bootstrap actions, component
alignment, principal-angle comparisons, and bounded-memory stability reducers
pass on both JVM and Scala.js. Structured row, block, strata, and equal-size
cluster actions; partition-nesting proofs; and nuisance residual randomization
through typed row-projector/whitening bridges are also implemented. Compiled
programs now lower to scheduler-neutral deterministic replicate jobs, an
optional exact-refit capability records its execution path, a thin-SVD paired
cross-covariance reduction matches full refits, and mergeable reducers cover
exact counts plus floating moments. The final family layer adds ridge-CCA,
generalized-eigen, held-out RRR predictive gain, constrained-PCA block,
multiblock consensus, and separate feature-evidence/multiplicity protocols.
Each has an explicit target/null/validity contract and an independent base-R
fixture.

### Phase 0 — freeze the evidence contract

Before implementing an engine:

- encode the module boundary in `build.sbt`, `README.md`, and
  `docs/module-relations.md`;
- add deterministic R-generated fixtures from `multifer` for fixed Monte Carlo
  p-values, sequential stopping, unit formation, component matching, sign
  alignment, and principal angles;
- add end-to-end small fixtures for PCA and PLSC ordered-root ladders;
- record R version, package versions, scripts, seeds, and exact target/null
  semantics beside the fixtures.

Acceptance: fixture files are reviewable without running R, and the plan states
which claims are independent oracles versus internal laws.

The frozen v1 bundle classifies its evidence as follows:

- fixed Monte Carlo counts, known component permutations/signs, and analytic
  principal angles are independent closed-form oracles;
- unit formation and the complete PCA/PLSC ladder trajectories are external
  `multifer` parity oracles, with inputs, source hashes, RNG metadata, seeds,
  null draws, and stopping receipts recorded;
- execution-order invariance, reducer associativity, compiler exclusion, and
  sign/subspace metamorphic laws remain Scala-owned internal laws for later
  phases and are not inferred from the R fixtures.

The canonical review files live under `modules/inference/fixtures/v1`; a
generated Scala mirror lets both platform test suites consume exactly the same
evidence without making R part of the sbt build.

### Phase 1 — typed core and compiler

Implement values, designs, units, validity, evidence states, small capability
traits, `InferenceSpec`, and `InferenceCompiler`.

Compile-time/API tests must demonstrate:

- a PCA variance target compiles with exchangeable rows;
- a PLSC covariance target compiles with a paired-independence null;
- CCA correlation is distinguishable from covariance despite sharing paired
  geometry;
- RRR cannot be requested as covariance-root inference;
- an invalid design/target combination cannot produce an executable program;
- plans contain no functions, matrices, fitted objects, or scheduler handles.

Acceptance: warning-clean JVM and Scala.js module tests.

### Phase 2 — exact Monte Carlo ladder

Implement deterministic streams, fixed Monte Carlo, batched early
non-rejection, the global budget state machine, and the sequential ladder.

Ship only two full protocols initially:

- PCA/GenPCA one-table variance;
- PLSC paired covariance.

Both should work through the same executor while retaining family-specific null
and removal behavior.

Acceptance:

- parity with the Phase 0 fixtures;
- metamorphic invariance under sign changes and orthogonal basis changes within
  declared subspace units;
- identical replicate assignments and results across sequential and reordered
  batch execution;
- stop-at-first-non-rejection and budget exhaustion covered explicitly;
- both JVM and Scala.js green.

### Phase 3 — bootstrap stability

Implement row/cluster bootstrap actions, unit matching, sign alignment,
principal-angle subspace summaries, and streaming stability reducers.

Acceptance:

- singleton-axis, swapped-axis, sign-flipped, tied-root, and rank-deficient
  replicate fixtures;
- stability results keyed only by `UnitId` and domain ids;
- no significance p-value synthesized from ordinary bootstrap ratios;
- bounded-memory reducer test;
- JVM and Scala.js parity.

### Phase 4 — conditioned and structured designs

Add within-block, clustered/stratified, and nuisance-adjusted residual
randomization using `RowProjector`/`RowWhitening` bridges.

Acceptance:

- smart constructors prove partition coverage and nesting;
- null actions preserve the declared exchangeability structure;
- independent fixtures for blocked and nuisance-adjusted PLSC/PCA cases;
- the module refuses unsupported conditioning instead of downgrading silently.

### Phase 5 — exact reductions and execution lowering

Add exact core-update capabilities where the algebra proves them, then lower a
compiled program into deterministic replicate jobs and an associative reducer.

Acceptance:

- full-refit versus exact-reduced differential tests at machine precision;
- provenance records the path;
- reordered/chunked reduction returns the same scientific result within an
  explicit floating tolerance;
- no Spark or platform runtime dependency in shared code.

### Phase 6 — broaden inferential families conservatively

Add only families with their own validated target and null contract:

1. CCA correlation inference on explicitly supported paired and
   nuisance-adjusted designs;
2. generalized-eigen ordered roots;
3. RRR predictive-gain inference with held-out split semantics;
4. CPCA and multiblock inference through declared block domains and a proved
   removal/null action;
5. method-native feature evidence and multiplicity correction.

Each addition requires an independent fixture, a validity statement, and a
complete protocol instance. Merely having a `Spectrum` or `MvMap` is not enough
to claim inferential support.

## Testing strategy

Every phase uses four distinct evidence layers:

1. **Constructor tests** for opaque values, partitions, and invalid-state
   rejection.
2. **Algebraic law tests** for null-action shape preservation, target
   invariance, deflation/removal behavior, and reducer associativity.
3. **Independent numerical fixtures** generated from `multifer` or another
   documented R reference for claims that must match the R ecosystem.
4. **Scenario tests** that compile one problem, execute it, and return one
   `ScenarioResult` truth value under the repository's scenario policy.

The focused gate is always both platforms:

```text
sbt inferenceJVM/test
sbt inferenceJS/test
```

Module completion also requires `compileAll`, `testAll`, warning cleanliness,
documentation sync, and an import scan proving that shared inference contains
no Breeze, dataset, image, Spark, or platform-specific dependencies.

## Non-goals

- Reimplementing PCA, PLSC, CCA, RRR, CPCA, or multiblock fitting.
- Owning GLM coefficient contrasts (`fit`), group models (`group`), spatial
  multiple testing (`threshold`), or connectivity-specific analytic tests.
- A universal p-value abstraction that erases the target/null provenance.
- Runtime discovery through string adapter ids in the core.
- A public wildcard for unsupported geometry.
- Approximate inference presented as exact.
- Automatic feature-significance claims from bootstrap stability.
- Distributed runtime dependencies in the shared module.

## Multivar cohesion boundary

Inference builds on the paired R fixtures, `Spectrum`, `MvMap`, `PairedGmd`,
constraint diagrams, row projectors, and row whitening already present in
`multivar`. It does not absorb those fitting or geometry abstractions.

The resulting boundary is deliberate:

- `multivar` owns fitted geometry, constraint resolution, and model-specific
  reconstruction;
- `inference` owns targets, null actions, validity, deterministic perturbation
  programs, evidence, and provenance;
- solver contracts stay in `linalg`, with no private inverse, SVD, or
  eigensolver family hidden in either domain layer.

New fitted families can therefore acquire inference only by supplying a
validated target, null, removal or predictive action, and independent evidence;
being representable as a spectrum is intentionally insufficient.
