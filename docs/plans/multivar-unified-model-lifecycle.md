# One lawful multivar model lifecycle

Status: implementation plan

Planning item: `bd-01KY4VDHVKA6NBQZHTXZ6VNWMS`

Implementation epic: `bd-01KY4VDG22ED2C3WCFJ3GG1NPC`

Fresh-eyes evidence: `docs/plans/multivar-cohesion-fresh-eyes-review.md`

## Decision

ScalaFIM multivar will have **one lifecycle algebra and several family-specific
model algebras**.

The shared lifecycle is:

```text
raw study
  -> fold-scoped preparation
  -> declared family program
  -> compiled execution plan
  -> solver execution
  -> fitted model + bound solver evidence
  -> typed fitted capabilities
  -> fold evaluation / final application
  -> runtime-derived ModelRun IR
```

`OperatorProgram` will remain the exact spectral/variational program algebra. It
will not be enlarged into a universal AST containing GLRM losses, multiblock
decoders, or every future model. GLRM and structured multiblock will retain
their own programs, compiled plans, and fitted payloads. They become cohesive by
implementing the same typed state transitions, identity binding, evidence
contract, ModelSpec protocol, and IR envelope.

The result is one system without pretending that every estimand is one
objective language.

## What “Oderskyan” means here

The target is a small algebra whose types teach the system:

1. **Illegal transitions do not type-check.** A declared program is not a
   compiled plan; a compiled plan is not a fit; requested claims are not
   achieved guarantees.
2. **Related types travel together.** A workflow owns associated program,
   compiled, candidate, fitted-payload, application, and score types. ModelSpec
   cannot accidentally combine a GLRM fit, a spectral scorer, and a multiblock
   transformer.
3. **Capabilities model optional behavior.** Projection, synthesis, encoding,
   reconstruction, and prediction remain distinct typeclass-style capabilities,
   not nullable methods on a universal fit.
4. **One value owns each fact.** Program identity, training scope, solver
   realization, achieved guarantee, materialization, and provenance have one
   runtime source of truth. IR is emitted from that value.
5. **Closed scientific vocabularies are exhaustive.** Model families, claims,
   losses, relationship kinds, and wire tags remain enums or sealed ADTs where
   exhaustiveness is valuable.
6. **Construction is narrow; inspection is rich.** Smart constructors and
   private fit constructors preserve invariants. Ordinary users call economical
   builders; experts can inspect every lowered operator, proof obligation,
   certificate, and receipt.
7. **No stringly typed control plane.** Strings may label durable identities,
   never select a solver, identify a hyperparameter, claim an API exists, or
   connect evidence to a result.
8. **No framework tax.** The lifecycle kernel contains no GLRM, spectral,
   multiblock, IR, dataset, or backend-specific branch.

## Scope

The plan makes the following one lifecycle:

- exact spectral and paired `OperatorProgram` fits;
- variational and sparse-functional fits;
- masked, heterogeneous-loss GLRM fitting and partial latent encoding;
- aligned shared-score GLRM fitting;
- direct-sum and hub-aligned multiblock fits;
- fold-safe selection and final fitting through `ModelSpec`;
- requested claims, solver plans, execution receipts, achieved guarantees,
  certificates, and materialization events;
- fitted projection, synthesis, encoding, reconstruction, and prediction
  capabilities;
- family program IR, fitted-result IR, and a universal lifecycle run envelope.

The plan does not move multivar into `model` or `fit`, add a JVM-only numerical
dependency, serialize live solver objects, make all result payloads matrices, or
erase real distinctions such as projection versus encoding.

## Canonical vocabulary

| Concern | Canonical concept | Existing concepts to adapt or retire |
| --- | --- | --- |
| Mathematical family | `MathematicalModelFamily` and its singleton type | Free API-name strings that merely imply a family |
| Declared model | `ModelProgram[F]` | “program” objects that only evaluate supplied fitted values without saying so |
| Execution-ready model | `CompiledModel[F, P]` | Unbound `PalmProblem` or lowering fragments passed around as if they were a fit |
| Completed training | `FittedModel[F, P, R]` | Family-specific top-level fits that lack a common binding/evidence spine |
| Program intent | `RequestedOptimizationClaim` | `SolverGuarantee` used both prospectively and retrospectively |
| Solver outcome | `AchievedOptimizationGuarantee` | `LatentEncodingAchievement` and inferred compatibility guarantees |
| Solver audit | `SolverEvidence[F]` | `SolverAttestation`, `SolverExecutionRecord`, and family receipts as unrelated top-level truths |
| Fit identity | `FitBinding[F]` | Independently authored program/data/result strings |
| Post-fit behavior | `FitCapability[Fit]` | Optional methods or one universal transformation result |
| Fold orchestration | `ModelSpec[W]` over one `ModelWorkflow` | Independent `FoldPipeline`, `ValidationScorer`, and `ModelTransformer` values |
| Wire audit | `ModelRunIr` emitted from `FittedModel` | Hand-authored mathematical evidence records |
| Cross-document link | `ArtifactRefIr(kind, schema, id, digest)` | Bare schema and program-id strings |

The existing `MathematicalModelFamily` enum remains the runtime and wire family
source of truth. Type parameters use its case singleton types; no parallel
phantom family hierarchy is introduced.

`FamilyContract[F]` is an opaque, smart-constructed view of
`MathematicalModelContract` whose runtime family has been checked against `F`.
It prevents an exact-spectral program from carrying a GLRM theorem catalog even
when both contract values are otherwise well formed.

## The lifecycle algebra

The following is the intended API shape. Exact syntax must be proved in a small
Scala 3.4.2 compilation spike before the implementation spreads, but the type
relationships are normative.

```scala
trait ModelProgram[F <: MathematicalModelFamily]:
  def family: F
  def id: ProgramId[F]
  def contract: FamilyContract[F]
  def requestedClaim: RequestedOptimizationClaim
  def provenance: SemanticProvenance

trait CompiledModel[
    F <: MathematicalModelFamily,
    P <: ModelProgram[F]
]:
  def program: P
  def id: CompiledId[F]
  def solverPlan: SolverPlan
  def materializationPlan: MaterializationPlan
  def provenance: SemanticProvenance

trait ModelWorkflow:
  type Family <: MathematicalModelFamily
  type RawStudy
  type PreparedStudy
  type Candidate <: ModelCandidate
  type Program <: ModelProgram[Family]
  type Compiled <: CompiledModel[Family, Program]
  type FitPayload
  type Applied
  type Target
  type Score <: ValidationScore

  def prepare(
      context: TrainingContext,
      study: ScopedTraining[RawStudy]
  ): Either[ModelRunError, ScopedPrepared[PreparedStudy]]

  def declare(
      context: TrainingContext,
      study: ScopedPrepared[PreparedStudy],
      candidate: Candidate
  ): Either[ModelRunError, Program]

  def compile(
      program: Program,
      policy: SolverPolicy
  )(using SolverCapabilities): Either[ModelRunError, Compiled]

  def solve(
      context: TrainingContext,
      study: ScopedPrepared[PreparedStudy],
      compiled: Compiled
  )(using SolverCapabilities): Either[ModelRunError, FittedModel[Family, Program, FitPayload]]

  def apply(
      fit: FittedModel[Family, Program, FitPayload],
      study: ScopedApplication[RawStudy]
  ): Either[ModelRunError, Applied]

  def score(
      applied: Applied,
      target: Target
  ): Either[ModelRunError, Score]
```

`using` is appropriate only for solver capabilities supplied by a backend. A
solver policy, tolerance, seed, materialization choice, or hyperparameter is an
explicit value.

The public workflow object carries the associated types, so users do not spell
the full generic signature:

```scala
val fit = Glrm.fit(data, spec, policy)
val encoded = fit.encode(partialRow)
val run = ModelRunIr.from(fit)
```

### Typed identities

```scala
opaque type ProgramId[F <: MathematicalModelFamily] = ValueIdentity
opaque type CompiledId[F <: MathematicalModelFamily] = ValueIdentity
opaque type ResultId[F <: MathematicalModelFamily] = ValueIdentity

final case class FitBinding[F <: MathematicalModelFamily] private (
    program: ProgramId[F],
    compiled: CompiledId[F],
    data: ValueIdentity,
    scope: TrainingScopeId,
    result: ResultId[F]
)
```

Smart constructors derive identities from the values being bound. Public code
cannot cast same-shaped spectral, GLRM, and multiblock identities into each
other. `TrainingScopeId` is minted by ModelSpec (or by the explicit single-fit
entry point) and replaces recursive provenance guessing as the primary leakage
proof. Provenance dependency checks remain an audit, not the authority.

### Fitted model

```scala
final class FittedModel[
    F <: MathematicalModelFamily,
    P <: ModelProgram[F],
    R
] private[multivar] (
    val program: P,
    val payload: R,
    val binding: FitBinding[F],
    val solver: SolverEvidence[F],
    val scope: TrainingScope,
    val provenance: SemanticProvenance
)
```

There is one `FittedModel` wrapper and family-specific payloads. The wrapper does
not know that a spectral payload contains frames or a GLRM payload contains row
codes and a decoder. Family executors are the only constructors.

Construction checks that the program, compiled plan, solver-evidence binding,
training scope, result identity, and payload-derived identity agree. The
evidence aggregate stored by a `FittedModel` cannot name a merely equal-looking
program or a result from another execution.

### Fitted capabilities

Runtime behavior is expressed with capabilities whose input and output types
are tied to the fitted payload:

```scala
trait CanTransform[Fit, Input]:
  type Output
  def transform(fit: Fit, input: Input): Either[ModelRunError, Output]

trait CanEncode[Fit, Input]:
  type Output
  def encode(fit: Fit, input: Input): Either[ModelRunError, AppliedResult[Output]]

trait CanReconstruct[Fit, Input]:
  type Output
  def reconstruct(fit: Fit, input: Input): Either[ModelRunError, Output]
```

The existing projection and synthesis types remain the implementations for
spectral fits. `FittedLatentEncoder` becomes the `CanEncode` implementation for
`GlrmFit`. Aligned multiblock encoding implements `CanEncode` for its own fit.
An operation that runs an optimizer returns `AppliedResult[A]` with
`ApplicationEvidence`; it never pretends that a nonlinear encoding is a second
model fit or a linear projection.

## Solver evidence: one prospective/retrospective boundary

The program asks; execution proves.

```text
RequestedOptimizationClaim
        + SolverPolicy
        + available SolverCapabilities
        -> SolverPlan
        -> algorithm-specific SolverReceipt
        -> GuaranteeAdmission
        -> AchievedOptimizationGuarantee
        -> SolverEvidence bound to FitBinding
```

The target aggregate is:

```scala
final case class SolverEvidence[F <: MathematicalModelFamily] private (
    binding: FitBinding[F],
    plan: SolverPlan,
    receipt: SolverReceipt,
    achieved: AchievedOptimizationGuarantee,
    certificates: NonEmptyCertificates,
    materializations: Vector[MaterializationEvent],
    provenance: SemanticProvenance
)
```

`SolverReceipt` is a sealed ADT with algorithm-specific payloads:

- `ExactSpectralReceipt` — spectrum, rank, residual, degeneracy clusters;
- `FirstOrderReceipt` — method, stopping state, iterations, primal/dual/KKT
  residuals, objective history;
- `PalmReceipt` — per-block traces, stationarity, descent, inexactness, KL and
  level-set evidence;
- `BlockCoordinateReceipt` — block ordering, block residuals, sweep objective;
- `ExternalBackendReceipt` — only behind a typed backend capability, with
  implementation identity and independently checked residuals.

`AchievedOptimizationGuarantee` remains the one attained-result ADT. Exact
spectral, variational, PALM, latent encoding, and multiblock code must all pass
through `GuaranteeAdmission`. `SolverGuarantee` is retained only as a decoder
compatibility view for operator-program IR 0.2, then removed from new runtime
APIs. `SolverExecutionRecord` and `SolverAttestation` become derived adapters
during migration and disappear from the final model.

Every certificate and receipt is checked against the same `FitBinding`. A
converged status alone cannot manufacture a claim. An unresolved run is a valid
fitted outcome only when the family contract and caller policy admit it; it can
never satisfy a policy requiring stationarity or global optimality.

## GLRM as the second proving family

GLRM must be genuinely executable before the lifecycle is generalized into
ModelSpec and IR.

### Declared GLRM program

The declared program owns:

- typed observation pattern, including observed, missing, structural, and
  censored cells;
- feature layout and every feature domain/loss;
- latent rank/space;
- row-code and decoder penalties using the shared penalty-functional identity;
- missingness statement and prediction target;
- requested optimization claim;
- initialization and multi-start policy as explicit, deterministic values;
- program and data identities.

It does **not** own fitted row codes or a fitted decoder. The current evaluator
logic becomes an internal `GlrmObjective`, while the public executable value is
`GlrmProgram` (with a temporary source-compatible alias if needed).

Each `EntryLoss` exposes one checked capability record: value, gradient,
curvature/Lipschitz information, natural-domain validation, and prediction
decode. A loss lacking a capability required by the selected solver is rejected
at compilation, before any iteration. Censoring similarly requires a declared
likelihood capability or fails at compilation rather than being encountered in
an objective loop.

### Compiled GLRM

`GlrmCompiler` lowers the program to:

- one typed row-code block and one typed decoder block;
- a `PalmProblem` with program, data, mask, loss, penalty, and operator identities;
- a `PalmAdmission` carrying the exact theorem obligations;
- selected block update methods and their numerical capabilities;
- derived Lipschitz/curvature bounds and normalization policy;
- an explicit materialization plan;
- a deterministic initialization/multi-start plan.

The compiler contains the GLRM-to-PALM knowledge. `PalmSolver` remains generic
and does not import GLRM concepts.

### Fitted GLRM

`GlrmFit` contains:

- learned `GlrmRowCodes` and `FeatureDecoder`;
- objective decomposition over observed loss, row penalty, and decoder penalty;
- the selected PALM start and all retained start outcomes;
- decoded training predictions and identifiability diagnostics where lawful;
- a `FittedLatentEncoder` capability constructed from the learned decoder;
- no independent achievement or receipt fields: those live in
  `FittedModel.solver`.

All currently public losses and penalties must be classified as executable,
compile-time unsupported, or planned. “Supported” means an end-to-end fit test,
not merely an objective-value test.

## Structured multiblock

The current aligned GLRM type mixes program and fitted state because blocks
already contain decoders. Split it deliberately:

```text
AlignedGlrmBlockSpec       FittedAlignedGlrmBlock
- observations            - learned decoder
- layout                   - block objective diagnostics
- block penalties          - block result identity
- scaling                  - fitted encoding capability
- structures
```

`AlignedGlrmProgram` owns a verified shared-row binding, latent space, block
specifications, shared row penalties, requested claim, and program identity. It
contains no fitted decoder.

`AlignedGlrmCompiler` creates one shared row-code block and one decoder block
per input block, preserving the rule that the shared row penalty is charged
exactly once. The compiled plan records block scaling, update order, alignment,
all operator identities, and every per-block capability. It lowers to PALM or a
typed block-coordinate solver without teaching the lifecycle kernel about
blocks.

`AlignedGlrmFit` owns the learned shared row codes, fitted blocks, objective
decomposition, solver diagnostics, and partial multiblock encoder capability.
Block scores and weighted block contributions remain distinct result types.

`IndependentDirectSum` and `HubAlignedEntities` remain different row semantics.
Their existing exact spectral programs receive lifecycle adapters; they are not
coerced into shared-score GLRM. The sealed `StructuredMultiblockStudy` family
remains exhaustive, but each executable case must lead to a `FittedModel` and
solver evidence.

## ModelSpec becomes generic over one workflow

The current `ModelSpec` owns rigorous fold scoping but hard-codes a matrix study,
string-keyed candidates, `OperatorProgram`, `OperatorFitBundle`, and three
independently supplied behavioral traits. Preserve the leakage model and replace
the family assumptions.

The target shape is:

```scala
final class ModelSpec[W <: ModelWorkflow](val workflow: W)(
    val candidates: Vector[workflow.Candidate],
    val folds: NestedFoldPlan,
    val direction: SelectionDirection,
    val solverPolicy: SolverPolicy,
    val baseSeed: DeterministicSeed
)
```

The workflow supplies associated raw/prepared study, candidate, program,
compiled, fit, application, and score types. Consequently:

- a spectral candidate can be an ADT such as `SpectralCandidate(components,
  shrinkage)` rather than a string map;
- a GLRM candidate can carry rank, penalty weights, and solver policy with smart
  constructors;
- an aligned multiblock candidate can carry block/shared penalties without
  leaking block names into ModelSpec;
- the scorer and applicator are necessarily the ones belonging to the workflow;
- ModelSpec orchestrates folds, seeds, selection, leakage checks, and final
  refit, but does not inspect a program family.

`ScopedTraining[A]`, `ScopedPrepared[A]`, and `ScopedApplication[A]` carry the
minted scope token, source identity, row selection, and usage role. A prepared
value cannot shed or replace its training scope. Only scoped training reaches
`prepare`, `declare`, and `solve`; validation-scoped application may reach only
`apply` and `score`. A separately minted external-application scope supports
post-fit use without masquerading as validation. The final `ModelFit[W]` retains
the concrete `FittedModel` and exposes only capabilities lawful for that
workflow.

Lifecycle events use typed artifact IDs and stages. Human-readable labels are
metadata, not equality keys. Leakage audit checks exact scope bindings first and
provenance closure second.

## IR: one run envelope, family-specific payloads

The wire design mirrors the runtime design.

### Keep family documents focused

- semantic/duality IR 0.1 remains immutable;
- operator-program IR 0.2 remains the spectral/variational family document;
- add a GLRM program/fit document for observations, layouts, losses, compiled
  block plans, fitted factor references, and objective decomposition;
- add a structured-multiblock program/fit document for row semantics, block
  specifications, alignment, scaling, fitted block references, and contributions.

These are family algebras, not variants in one enormous JSON object.

### Add the universal lifecycle document

`ModelRunIr` 1.0 records:

```text
schema
family + contract + estimand
declared program ArtifactRefIr
compiled plan ArtifactRefIr
training data + scope identities
solver plan + typed receipt
achieved guarantee + certificate references
fit result ArtifactRefIr
fitted capability descriptors
materialization events
provenance + reproducibility receipt
```

Every reference is:

```scala
final case class ArtifactRefIr(
    kind: ArtifactKindIr,
    schema: String,
    id: String,
    sha256: String
)
```

`ModelBundleIr` combines the run document with the referenced family documents
for validation. `ModelBundleValidator` checks schema, kind, identity, digest,
family, contract, program/compiled/result lineage, solver binding, certificate
membership, and capability references. A valid collection of individually
well-formed but mutually unrelated documents must fail.

`ModelRunIr.from(fitted)` is the only public producer of achieved runtime
evidence. Family encoders lower the program, compiled plan, fit payload, and
capabilities; the universal emitter lowers the common binding and solver
evidence. Decoding produces a `VerifiedModelRunIr`, not an executable runtime
fit unless all numerical payloads are present and revalidated.

`MathematicalModelEvidenceIr` 1.0 remains decode-compatible. New code obtains a
legacy document only as a derived export from `ModelRunIr`; direct constructors
are made package-private or deprecated. There is no second v2 evidence truth:
the run envelope is the evidence.

## Failure model

Internal error ADTs remain owned by their layer. One stable workflow boundary
preserves them:

```scala
enum ModelRunError:
  case Preparation(cause: PreparationError)
  case Declaration(cause: ProgramError)
  case Compilation(cause: CompilationError)
  case Execution(cause: SolverError)
  case Guarantee(cause: OptimizationGuaranteeError)
  case Application(cause: ApplicationError)
  case Leakage(cause: LeakageError)
```

Family workflows may expose a narrower error ADT that maps into `ModelRunError`
without discarding the cause. `InvalidDefinition(String)` is reserved for trust
boundaries where no more precise invariant exists; it is not the default for
cross-layer adaptation.

## Materialization and backend policy

Materialization is part of compilation and evidence:

- `MaterializationPlan` declares every permitted dense conversion, its operand,
  estimated shape/bytes, and reason;
- `StoragePolicy` is checked while compiling, before allocation;
- `MaterializationEvent` records what actually occurred;
- solver evidence must reconcile planned and realized materializations;
- IR retains the events;
- strict no-dense policies fail before `toDense`.

This preserves matrix-free semantic operators while honestly admitting exact
dense backends. An operator-shaped API never implies a matrix-free execution
that did not happen.

## Laws and tests

Every workflow implementation must satisfy the same shared law suite:

1. **Transition law:** only its declared program can produce its compiled plan,
   and only that compiled plan can produce its fit.
2. **Binding law:** program, compiled plan, training data, scope, result,
   receipt, guarantee, and certificates share one exact binding.
3. **No-forgery law:** convergence flags, copied IDs, or hand-authored IR cannot
   create an achieved guarantee.
4. **Scope law:** no fit-stage event touches validation rows; a fit minted in one
   scope cannot be reused in another.
5. **Determinism law:** program/compiled/result identities and selected
   multi-start are deterministic for the same data, candidate, and seed.
6. **Objective law:** family objective decomposition agrees with the selected
   solver objective within conditioning-aware tolerance.
7. **Capability law:** runtime capabilities and IR capability descriptors are
   exact; unsupported operations have no capability instance.
8. **IR law:** `decode(encode(ModelRunIr.from(fit)))` preserves every identity,
   claim, receipt, reference, and provenance event.
9. **Reference law:** a mutation to any referenced id, kind, schema, or digest is
   rejected.
10. **Materialization law:** realized dense conversions are a subset of the
    admitted plan and strict policies allocate none.
11. **Compatibility law:** convenience APIs and lifecycle APIs agree on fitted
    values and numerical oracles during migration.
12. **Extension law:** adding a solver does not edit ModelSpec or family program
    ADTs; adding a loss does not edit the lifecycle kernel; adding a family does
    not edit existing workflows.

All portable laws run in shared tests on JVM and Scala.js. Numerical behavior is
anchored by analytic, R, Julia, differential, or exhaustive fixtures according
to the mathematical contract; lifecycle structure is never accepted from a
round-trip-only test.

## Extension edit budgets after migration

| Extension | Lawful edit points |
| --- | --- |
| New GLRM loss | `EntryLoss` case and one loss-capability implementation; GLRM IR codec/schema case; loss oracle tests. No ModelSpec, lifecycle, or generic solver edits. |
| New penalty | one shared functional identity; family-specific target/capability witness where applicable; relevant compiler lowering; IR case and oracle tests. The mathematical name is not duplicated. |
| New solver | linalg solver capability/implementation; one `SolverReceipt` case or existing receipt adapter; family compiler selection; numerical and guarantee-admission tests. No ModelSpec or program-family edits unless the solver enables a genuinely new requested claim. |
| New alignment relation | row-relationship ADT/operator/certificate; multiblock compiler case; semantic/multiblock IR case; alignment laws. No lifecycle, ModelSpec, or solver-evidence edits. |
| New model family | one `MathematicalModelFamily` case, program/compiled/payload types, one workflow, one family IR codec, and shared lifecycle laws. Existing workflows remain untouched. |

## Migration and source compatibility

The migration is additive until two families prove the abstraction.

1. Land requested/achieved terminology, binding, solver evidence, and lifecycle
   kernel alongside current types.
2. Adapt exact spectral fits without changing numerical paths.
3. Implement GLRM directly on the new lifecycle. Do not first create another
   GLRM-only fit wrapper that must later be migrated.
4. Generalize ModelSpec only after spectral and GLRM workflows pass the shared
   laws.
5. Add multiblock, remaining estimators, and IR emitters.
6. Keep old constructors as explicit adapters for one declared migration window.
   If the public version has not stabilized, remove them at the release gate
   instead of preserving accidental APIs.
7. Existing IR schemas remain immutable. Compatibility is decode/read support,
   not continued runtime authorship of disconnected evidence.

No phase may introduce `Any`, unchecked casts, reflection, mutable global
registries, family-name pattern matching in ModelSpec, or a dependency from
`multivar` to `multivar-ir`.

## Implementation program

### Phase 0 — truth and vocabulary

- `bd-01KY4TXMG0YWBMNDVKPBSKS4XZ`: publish the live capability/maturity matrix
  and make docs/API bindings truthful.
- `bd-01KY4TXN54PRXR993BZ7P4KG0W`: separate requested claims from achieved
  guarantees and converge solver outcome vocabulary.
- `bd-01KY4TXNF6S1S5M4QSXNTSRXMJ`: define one cross-family penalty-functional
  identity with family-specific capability witnesses.

Exit: names used by the lifecycle have exactly one meaning, current support is
truthful, and legacy wire mappings are explicit.

### Phase 1 — lifecycle kernel and spectral proof

- `bd-01KY4VH084G1K1YT2THSCP406Y`: add the family-indexed lifecycle kernel,
  typed bindings, capabilities, and an exact spectral adapter.

Exit: spectral fit behavior is unchanged; mismatched stages fail at compile
time; the shared laws pass on JVM and Scala.js; the kernel contains no
GLRM-specific case.

### Phase 2 — executable GLRM

- `bd-01KY4TWV5AFK51HHCCFME91Y38`: compile GLRM into admitted PALM execution and
  return a bound `FittedModel` with `GlrmFit` payload and encoding capability.

This item depends on the lifecycle kernel, final guarantee vocabulary, and
shared penalty identity.

Exit: mixed masked GLRM learns row codes and decoder; every public loss/penalty
is executable or rejected during compilation; objective, stationarity,
multi-start, partial encoding, and identity laws pass on both platforms.

### Phase 3 — real structured multiblock fitting

- `bd-01KY4TXJQZ72S37EXCWTEFT5S4`: split block specs from fitted blocks and
  implement aligned joint fitting; adapt direct-sum and hub semantics without
  conflation.

Exit: shared row codes and block decoders are learned jointly; shared penalties
are charged once; alignment and block identities survive fit and encoding;
permutation, scaling, contribution, and partial-encoding laws pass.

### Phase 4 — generic ModelSpec

- `bd-01KY4TXM5VB7R9FAQFDE7T6QN0`: generalize fold preparation, candidates,
  fitting, application, scoring, final refit, and evidence binding over one
  `ModelWorkflow`.

The existing issue also tracks the shared fitted-artifact integration. Runtime
IR emission itself is deliberately completed in Phase 7 so ModelSpec is proved
by runtime families before wire design freezes.

Exit: the same ModelSpec engine tunes and refits exact spectral and GLRM
workflows; aligned multiblock is admitted without a family branch; leakage and
determinism tests pass.

### Phase 5 — migrate every estimator

- `bd-01KY4VH29QS6SNNKKBB0FMBYKS`: adapt ordinary PCA/SVD, variational and
  sparse-functional fits, direct-sum/hub fits, and solver-backed post-fit
  applications.

Exit: every executable mathematical contract resolves to a real entry point,
fitted lifecycle artifact, achieved evidence, and truthful maturity; planned
families fail explicitly.

### Phase 6 — explicit materialization

- `bd-01KY4TXNRWM0N4RX65D6JB8KAX`: expose dense backend admission and realized
  materialization.

This phase may proceed in parallel with Phases 1–5 but is a prerequisite for
wire freeze.

Exit: exact dense GPCA and rank-k paths declare and report conversion; strict
policies fail before allocation; diagnostics and provenance agree.

### Phase 7 — runtime-derived IR

- `bd-01KY4VH1XBMV2Q00BD1HJQT6QV`: add family program/fit documents, universal
  `ModelRunIr`, runtime emitters, cross-document bundle validation, and legacy
  evidence export.

Exit: every executable family emits a referentially sound run bundle; tampered
references fail; no public evidence constructor can detach a claim from a fit;
schemas/codecs/conformance pass on JVM and Scala.js.

### Phase 8 — economical public APIs

- `bd-01KY4TXMTY9M15ZME5AVDSWQWR`: add task-oriented builders over the proof
  core after every builder can lower to the final lifecycle.
- finish `bd-01KY4TXMG0YWBMNDVKPBSKS4XZ` against the landed APIs and schemas.

Exit: ordinary PCA, semantic GMD, sparse-plus-smooth factorization, heterogeneous
GLRM, and aligned multiblock each have one short documented path plus an
inspectable expert path. There is no second implementation.

### Phase 9 — release gate and legacy removal

- `bd-01KY4VH2PYYX3J46Q95KD92FCF`: run the clean detached release gate and
  remove or quarantine legacy construction paths.

The epic `bd-01KY4VDG22ED2C3WCFJ3GG1NPC` closes only after this gate.

## Dependency graph

```text
plan
  |
  +--> requested/achieved vocabulary ----+
  |                                      |
  +--> shared penalty identity ----------+--> executable GLRM
                                         |         |
requested/achieved --> lifecycle kernel -+         +--> aligned multiblock
                            |                       |
                            +-----------------------+--> generic ModelSpec
                                                        |
aligned multiblock + generic ModelSpec -----------------+--> all-family adoption
                                                               |
explicit materialization --------------------------------------+--> ModelRun IR
all-family adoption -------------------------------------------+

all-family adoption --> ergonomic builders ----+
ModelRun IR ------------------------------------+--> release gate --> epic close
truthful docs ----------------------------------+
```

## Release gate

The lifecycle is complete only when all of the following are true:

- [ ] One shared lifecycle law suite passes for spectral, GLRM, and aligned
      multiblock workflows on JVM and Scala.js.
- [ ] ModelSpec tunes and final-fits those three families without family
      pattern matching or unchecked casts.
- [ ] Every executable `MathematicalModelContract` resolves to a real API,
      workflow, fit payload, achieved-evidence path, and IR emitter.
- [ ] Every planned or partial contract rejects unsupported execution before
      numerical work and documents the missing capability.
- [ ] `SolverGuarantee`, `LatentEncodingAchievement`, hand-authored runtime
      evidence, and unbound solver receipts are removed or isolated behind
      versioned compatibility adapters.
- [ ] No fitted decoder is accepted as part of a declared GLRM or aligned
      multiblock program.
- [ ] Program, compiled plan, data, scope, result, solver receipt, guarantee,
      certificates, capabilities, and IR share one verified binding.
- [ ] Exact dense paths expose admission and realized materialization.
- [ ] Five public workflows are documented with economical builders and
      inspectable expert artifacts.
- [ ] Existing R/analytic/differential numerical oracles remain green.
- [ ] IR JSON Schemas, codecs, conformance fixtures, reference-mutation tests,
      and legacy decoding pass on both platforms.
- [ ] `sbt compileAll` and `sbt testAll` pass warning-clean from a clean detached
      worktree at the committed tip.
- [ ] README, module relations, mathematical contracts, architecture plans, and
      public names describe the same landed lifecycle.

Green tests before these conditions are evidence of local correctness, not
proof that the lifecycle is complete.

## The desired final feel

A new user should see five small verbs—declare, compile, fit, apply, emit—and the
compiler should keep every family-specific value on its lawful path. A model
author should implement one workflow and inherit fold safety, evidence binding,
IR emission, and shared laws. A solver author should implement a capability and
receipt without editing ModelSpec. An IR consumer should be unable to confuse a
self-consistent document with evidence of a runtime fit.

The mathematical richness stays in the family algebras. The system-level
coherence lives in the lifecycle connecting them.
