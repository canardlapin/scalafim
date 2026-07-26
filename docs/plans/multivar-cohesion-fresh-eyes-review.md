# Multivar cohesion fresh-eyes review

Status: complete review of commit `e60f59d65871db8c475f9b4eddf426d737aaf749`

Tracker: `bd-01KY4SS4AWW7B91C80RJFSKBW5`

## Verdict

**Cohesive with major seams.** The semantic operator and fitted-projection core is
unusually disciplined: nominal spaces, primal/dual orientation, evidence-bearing
operators, parameterized programs, explicit lowering, solver attestation, and
typed projection/synthesis form a recognizable system rather than a bag of
algorithms. That core is both clear and locally elegant.

The cohesion does not yet extend across the complete public multivar surface.
There are currently three different meanings of “program” and “fit”:

1. spectral and paired methods lower to `OperatorProgram`, execute, and produce
   `OperatorProgramFit` / `OperatorFitBundle`;
2. sparse-functional methods reuse parts of the penalty, compiler, and guarantee
   vocabulary but have their own factorization and fit lifecycle;
3. GLRM and aligned structured-multiblock types describe and evaluate objectives
   over caller-supplied factors/decoders, while PALM is a separate generic solver
   with no adapter connecting the two.

`ModelSpec` and the fit-bearing operator IR cover the first lifecycle, not all
three. The public documentation describes a more unified and more executable
system than the runtime APIs currently provide.

No P0 numerical correctness defect was found. The highest-priority findings are
P1 architecture and public-contract problems: users cannot perform some of the
joint fits the API vocabulary implies, and the evidence lifecycle can publish
synthetic records for model families that have no corresponding runtime fit
artifact.

## Review protocol and evidence

I reviewed the exact commit in a clean detached worktree. I first read public
APIs and tests without consulting the architecture plans, wrote the model below,
then read the README, plans, mathematical contracts, and IR. The dirty shared
checkout was not cleaned or used as execution evidence.

Focused verification at the reviewed commit:

| Command target | Platform | Result |
| --- | --- | --- |
| `multivarJVM/test` | JVM | 448 passed |
| `multivarJS/test` | Scala.js | 448 passed |
| `multivarIrJVM/test` | JVM | 46 passed |
| `multivarIrJS/test` | Scala.js | 44 passed |

The JVM-only difference is the conformance-file suite. These green suites support
the local laws and parity claims they exercise; they do not establish that the
public model families share one fit lifecycle.

## Architecture map from the cold read

```text
MatrixView / preprocessing / Gale capabilities
                    |
                    v
SpaceRef + SpaceEvidence + Op/Lin + certificates + provenance
                    |
          +---------+---------------------------+
          |                                     |
          v                                     v
SemanticDualityDiagram                    row relationships,
 -> GpcaProblem                           block structure,
 -> OperatorProgram                      observation patterns
 -> lowering / execution                       |
 -> OperatorProgramFit                         |
 -> OperatorFitBundle                    +-----+------------------+
          |                              |                        |
          v                              v                        v
FittedFrameTransform              sparse-functional       GLRM objectives
 / restriction                    factorization            + block decoders
 / synthesis                      + own fit/certs           + latent encoder
 / multiblock projection                                     |
          |                                                 no trainer bridge
          v                                                   to generic PALM
ModelSpec fold lifecycle
 + fit-bearing operator IR
```

The left-hand path is a strong vertical slice. The right-hand paths share
semantic ingredients but do not rejoin it at fitting, model selection,
attainment evidence, or serialization.

## Findings

### F1 — P1 architecture/correctness of public claims: GLRM and aligned multiblock do not fit the model they name

`GeneralizedLowRankProgram` exposes `evaluate(factors)` and validates an objective;
it has no training method. `AlignedSharedScoreGlrm` similarly exposes
`evaluate(rowCodes)` and builds a `FittedLatentEncoder` from block decoders that
the caller already supplied. Its tests manually construct row codes and
`FeatureDecoder` values. `PalmProblem` and `PalmSolver` are executable, but no
public adapter compiles either GLRM type into PALM blocks and returns a fitted
GLRM/multiblock artifact.

This is not a numerical bug in `evaluate` or partial encoding. It is a mismatch
between the joint-estimation language in `MathematicalModelFamily`, the public
names (`Program`, `AlignedSharedScoreGlrm`, “fitted objective”), and the operation
users can actually perform.

Smallest coherent remediation:

- immediately describe these types as objective/evaluation specifications and
  mark training as unsupported; then
- add one real trainer path from `GeneralizedLowRankProgram` to a solver problem,
  returning factors, trace, attained guarantee, and a `FittedLatentEncoder`;
- make aligned multiblock fitting compose that path rather than accepting only
  pre-fitted decoders.

### F2 — P1 architecture: the supposedly universal fit/evidence lifecycle is spectral-specific

`FoldPipelineFit` requires an `OperatorProgram`, a lowered `OperatorProgram`, an
`OperatorFitBundle`, and a `SolverExecutionRecord`. The shipped pipelines are
GPCA/exact-quadratic/constrained-canonical paths. Ordinary PCA/SVD bypass this
lifecycle, sparse-functional fitting has a separate result hierarchy, and GLRM
and structured multiblock have no fitted artifact to insert into it.

The same seam appears in IR. Runtime lowering exists for `OperatorProgram` and
for several fitted projection/synthesis capabilities. By contrast,
`MathematicalModelEvidenceIr` is a free-standing record with no constructor from a
runtime fit. Its six-family tests hand-author `programId`, solver receipt,
certificate identities, and even a stationary GLRM record. Validation checks
internal consistency but cannot prove that the named runtime program, trace,
result, or certificates exist together.

Smallest coherent remediation: define one model-family-neutral fitted-artifact
contract for identity, requested/effective specification, solver receipt,
attained guarantee, result capabilities, and evidence emission. Adapt the
existing spectral bundle first, then require a non-spectral implementation before
claiming a universal lifecycle. Do not force GLRM into the closed spectral
`BaseObjective` merely to reuse its container.

### F3 — P1 documentation/naming: normative documents and contract bindings are observably stale

Concrete drift includes:

- `modules/multivar/README.md` says all statistical methods execute on the typed
  operator/program substrate. `Pca.fit` and `Svd.fit` call the SVD solver directly;
  GLRM/multiblock do not produce an `OperatorProgramFit`.
- `docs/plans/multivar-duality-constitution.md` still names
  `DualityDiagram`, `MvMetric`, `SemanticGenPca`, `GenPcaSpectrum`, and
  `Unsafe.genPcaFromArrays`, none of which is a current public symbol.
- that constitution says old compatibility layers remain, while
  `docs/plans/multivar-operator-core.md` says they were deleted.
- the operator-core plan says named decomposition builders produce
  `OperatorProgram`; that is false for ordinary PCA/SVD.
- `MathematicalContractCatalog` binds API references to
  `SparseFunctionalFactorization`, `ConvexLowRankMatrixProgram`, and
  `StructuredMultiblockProgram`. Those public symbols do not exist. Current tests
  establish that the strings are distinct, not that they resolve to an API.
- evidence IR requires every family to name operator-program schema 0.2 even
  though GLRM and structured-multiblock runtime objects cannot create such a
  program document.

Smallest coherent remediation: publish one capability/status matrix from actual
entry points; update the README and both normative plans; bind mathematical
contracts to a closed `ApiSurfaceId` whose cases name real public symbols; and add
compile-time or explicit registry tests that each executable contract reaches a
runtime fit and an IR emitter.

### F4 — P2 architecture/usability: proof objects are excellent, but common workflows expose construction machinery

The type discipline is justified, but the system lacks task-oriented façades for
its central workflows. Semantic GPCA and sparse-functional factorization require
users to assemble many implementation-facing identities, charts, capabilities,
and certificates before the mathematical intent becomes visible. GLRM and
structured multiblock add similar setup without completing training.

Smallest coherent remediation: add narrow builders that infer safe identity
geometry, mint internal parameter/auxiliary IDs, and expose advanced proof
objects through an `inspect` result. Keep the existing constructors as the
expert layer. The façade should lower to exactly the same checked core and must
not add a second semantic model.

### F5 — P2 architecture/naming: guarantees and penalties have overlapping public vocabularies

There are three optimization-result vocabularies:

- `SolverGuarantee`, inferred syntactically on `ResultSemantics` and used by
  `ModelSpec`;
- `OptimizationClaimClass` plus `AchievedOptimizationGuarantee`, which carries
  theorem/certificate evidence;
- `LatentEncodingAchievement`, which independently represents stationary or
  unresolved encoding.

`AchievedOptimizationGuarantee.legacyGuarantee` acknowledges the overlap. A
requested `SolverGuarantee` is not attained evidence, yet both are called a
guarantee in public APIs.

Penalty extension is also split between `FunctionalKind` / `PenaltyTerm`,
`GlrmFactorPenalty`, and `BlockStructuredPenaltyKind`, with separate IR evidence
enums. Some separation is mathematically necessary, but the ownership boundary
is not stated and common functionals such as L1 and squared smoothness must be
mapped repeatedly.

Smallest coherent remediation: use “requested claim” for program semantics and
one evidence-bearing “achieved guarantee” type for every solver/encoder result;
confine `SolverGuarantee` to operator-IR compatibility. Then define a shared
functional identity with family-specific capability witnesses rather than
duplicating the functional name itself.

### F6 — P2 performance contract: important exact paths densify below operator-shaped APIs

The semantic preparation layer treats materialization as a policy decision, but
`GpcaProblem.fit` unconditionally calls `toDense` on covariance and feature
cometric before the exact solver. Joint rank-k exact sparse-functional fitting
similarly densifies data and both metrics. That may be the correct current
backend, but it is not visible in the fit signature or result diagnostics and is
stronger than the constitution's matrix-free wording suggests.

Smallest coherent remediation: expose exact-dense backend admission at the public
fit boundary, report realized materializations in diagnostics/provenance, and
limit documentation claims to preparation stages that are genuinely lazy.

### F7 — P3 API consistency: error ownership is locally typed but globally noisy

The module has many useful domain error ADTs, but cross-layer entry points mix
specific errors, wrappers, and repeated `InvalidDefinition(String)` fallbacks.
This is not itself evidence of a bad design: local ownership and exhaustive
matches are valuable. The user cost appears when composing builders across
semantic, compiler, solver, GLRM, and model-spec layers.

Smallest coherent remediation: document one stable top-level error boundary for
each user workflow and preserve typed causes through it. Do not collapse the
internal ADTs into a single undifferentiated error enum.

## Workflow traces

| Workflow | Public path found | Assessment |
| --- | --- | --- |
| Ordinary PCA | `Pca.fit(MatrixView, ComponentCount, ...)` -> `PcaFit` -> fitted projection | Excellent discoverability and good fitted projection behavior, but it bypasses program attestation, `ModelSpec`, achieved guarantees, and fit IR. |
| PCA/GMD with semantic geometry | `SemanticDualityDiagram` -> preparation -> `GpcaProblem` / `SemanticGpca.fit` -> `OperatorProgramFit` | Mathematically cohesive and well tested. Construction is ceremony-heavy and needs a safe identity/default builder. |
| Joint sparse + smooth factorization | geometries/charts -> several penalty plans -> `RankOneStructuredFactorization` or joint rank-k -> fit/certificate | Strong separation of direct proximal, split, and exact spectral cases. User intent is obscured by compiler objects and IDs, and the fit lifecycle is separate from `ModelSpec`. |
| Masked heterogeneous GLRM + partial encoding | feature layout + observation pattern + `GeneralizedLowRankProgram`; separately provide decoder -> `FittedLatentEncoder.encode` | Loss/domain/missingness semantics and partial encoding are strong. The core joint training step is absent. |
| Structured multiblock fitting/alignment | verified row binding + per-block GLRM program/decoder/geometry/scaling -> `AlignedSharedScoreGlrm.evaluate` / encoder | Alignment and block ownership are precise, but this is evaluation/encoding of supplied fitted blocks rather than end-to-end structured multiblock fitting. |

Projection, partial contribution, variable recovery, synthesis, constraints, and
solver claims deserve separate judgments:

- **Projection/partial/variable elimination:** cohesive. `FittedFrameTransform`,
  `RestrictedFrameTransform`, `SupplementaryProjector`, and multiblock weighted
  contribution types preserve distinct estimands and identities. No merge is
  warranted.
- **Synthesis:** correctly capability-gated and distinct from analysis. The
  explicit, Euclidean least-squares, and orthonormal-transpose policies should
  remain separate.
- **Constraints/penalties:** semantic intent and solver lowering are separated
  rigorously inside `OperatorProgram`; the cross-family vocabulary is the seam.
- **Solver guarantees/certificates:** the proof-bearing attainment layer is
  strong. The older compatibility enum and non-spectral result types prevent one
  system-wide contract.
- **IR round-trip:** semantic IR and spectral operator fit IR round-trip real
  runtime objects. Six-family mathematical evidence currently round-trips
  self-consistent documents, not a runtime fit-to-document-to-fit identity.

## Extension thought experiments

| Extension | Required edit points today | Cohesion judgment |
| --- | --- | --- |
| Add one entry loss | `EntryLoss` domain/value/gradient/curvature/decode; encoder capability/special cases; evidence IR enum; codec; JSON schema; validation and oracle tests | Exhaustive but non-local. Runtime and evidence copies can drift because there is no single lowering function. |
| Add one penalty | `FunctionalKind` and compiler/lowering if it is an operator-program penalty; possibly `GlrmFactorPenalty` and `BlockStructuredPenaltyKind`; program IR, evidence IR, codec/schema, capability and oracle tests | Most fragmented extension. “Penalty” names a shared idea but not a shared extension boundary. |
| Add one solver | linalg `FirstOrderMethod`/capabilities; `VariationalSolverCompiler` selection and dispatch; stopping/certificate mapping; receipts/IR; `ModelSpec` adapter and tests | Deliberately closed and auditable, but central edits are required and it still does not make the solver available to GLRM/multiblock without a separate adapter. |
| Add one alignment relation | `RowRelationshipKind` plus checked constructor/operator; semantic IR relation kind and mapping; codec/schema/validator; multiblock builders and tests | Non-local but defensible: exhaustiveness protects mathematical meaning. The missing piece is a task-level builder, not a more open enum. |

## Abstractions that should not be collapsed

The following distinctions survived the fresh-eyes challenge and should remain:

- primal versus dual coordinates and nominal space identity;
- a row measure versus a row geometry;
- an operator action versus its matrix representation;
- analysis/projection versus synthesis/reconstruction;
- partial contribution versus least-squares recovery;
- block score versus weighted block contribution;
- a nonlinear latent encoder versus a linear fitted projection;
- requested optimization claims versus achieved, evidence-bearing claims;
- greedy deflation versus simultaneous joint rank-k estimation;
- hard constraints, denominator geometry, and objective penalties.

These types reduce ambiguity at mathematically real boundaries. The needed
simplification is at construction and lifecycle seams, not by erasing these
distinctions.

## Recommended sequence

1. Correct the public status and API references so documentation tells the truth
   about what fits, evaluates, projects, and serializes.
2. Deliver one end-to-end GLRM fit path with a real solver receipt, achieved
   guarantee, fitted encoder, fold-safe adapter, and runtime evidence emission.
3. Generalize the fitted-artifact lifecycle only as far as required to admit that
   second family; use it to prove the abstraction is not spectral in disguise.
4. Add task-oriented builders for semantic GPCA, sparse-functional fitting, and
   aligned GLRM while retaining the proof-oriented constructors underneath.
5. Consolidate result/guarantee names and make dense backend admission visible.

That sequence fixes misleading contracts first, then uses a real second lifecycle
to drive abstraction. It avoids adding another universal interface before the
non-spectral execution path exists.
