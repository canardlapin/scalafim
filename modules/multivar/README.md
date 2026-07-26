# scalafim-multivar

Typed duality diagrams and multivariate analysis for ScalaFIM. The semantic
core is the directed cycle `C --R--> C* --X--> O --A--> O* --X*--> C`:
nominal spaces, primal/dual orientation, form roles, evidence, centering,
singularity policy, and provenance are part of the analysis object rather than
loose matrix arguments.

The constitutional invariants are in
[`docs/plans/multivar-duality-constitution.md`](../../docs/plans/multivar-duality-constitution.md).
The implemented single-layer architecture, evidence-transition rules, and
result-equivalence vocabulary are in
[`docs/plans/multivar-operator-core.md`](../../docs/plans/multivar-operator-core.md).
All statistical methods execute on the typed operator/program substrate. There
is no legacy diagram, metric, map/projection, GPCA, paired-GMD, or CPCA engine
behind the semantic API.

## Package map

The module now makes its mathematical lifecycle visible in the namespace:

```text
core -> contract -> optimization -> solver -> lifecycle
                                      |
                                      +-> capability -> family.*
                                                           |
                                                           +-> workflow -> validation
```

`core` owns portable semantic and numerical primitives; `contract` states the
mathematics; `optimization` declares programs; `solver` lowers and executes;
`lifecycle` binds declarations, evidence, receipts and fitted payloads;
`capability` exposes family-neutral post-fit operations; each `family.*`
package owns one statistical vertical; and `workflow` owns fold-safe
`ModelSpec` composition. Tests mirror this layout. The exact ownership and
extension rules are documented in
[`multivar-package-hierarchy.md`](../../docs/plans/multivar-package-hierarchy.md).

Import from the semantic owner rather than from a flat façade:

```scala
import scalafim.multivar.core.{ComponentCount, MatrixView}
import scalafim.multivar.family.glrm.GeneralizedLowRankProgram
import scalafim.multivar.workflow.ModelSpec
```

This module owns the portable algebra below MVPA and neuroimaging adapters:

- nominal semantic spaces plus distinct primal and dual coordinates;
- directed, composable linear maps and role-specific forms with value-bound
  numerical certificates;
- ordered feature/sample index sets;
- disjoint block partitions and matrix-free direct sums;
- matrix-view contracts over dense, sparse, and lazy operator-backed inputs;
- immutable semantic duality diagrams carrying row measure, row/column forms,
  centering evidence, singular policies, certificate effects, and provenance;
- generalized PCA as one generalized Rayleigh--Ritz program whose fitted
  `FunctionalFrame` derives scores and axes, with clustered-spectrum and
  normalization diagnostics;
- explicit exact/partial row maps, incidence and aggregation maps, couplings,
  signed row links, common-entity hub alignment, and relationship support;
- nominal multiset association, disagreement, and hard/soft constraint
  objectives compiled through direct-sum operators and a separate block design;
- one typed `PairedOperatorProblem` for PLSC, regularized CCA, and reduced-rank
  regression: cross and marginal statistics arise through `secondOrder`, PLSC
  and CCA differ by normalization geometry, and fits expose two
  `FunctionalFrame`s plus the common `OperatorProgramFit` result contract;
- fitted analysis capabilities that reuse frozen preprocessing and feature
  identity: full scores, additive partial-feature contributions, metric-aware
  partial least-squares recovery, and supplementary-variable frames under
  explicitly named compatibility or metric conventions;
- fitted synthesis capabilities with no implicit transpose decoder: explicit,
  certified orthonormal-transpose, or Euclidean least-squares construction;
  component/feature-selective reconstruction and PLSC/CCA paired transfer are
  compositions of those typed analysis and synthesis objects;
- a fitted multiblock façade returning either unweighted block scores or
  weighted block contributions, with the exact global/local frame, block
  schema, and combination weight retained in provenance;
- executable variational lowering for exact quadratic/equality programs and
  convex coefficient-space refinements with L1, L21, disjoint/overlapping
  groups, sparse-group, elastic-net, Huber/TV composition, nonnegative, box,
  simplex, and monotone constraints; fits report the guarantee actually
  attained rather than inheriting the requested one;
- jointly estimated Allen-style rank-one GMD factors with certified row and
  column geometries, independently composable metric smoothness plus
  degree-one L1/sparse-group/TV structure on both sides, epsilon-certified
  convex block solves, explicit zero/sign/gauge conventions, and achieved
  coordinatewise-stationary evidence; greedy deflation and simultaneous
  generalized-Stiefel rank-k are separate APIs, with only the empty-nonsmooth
  rank-k case admitted through the exact generalized cross-SVD reduction;
- an Udell-style generalized low-rank semantic layer with per-feature real,
  binary, count, ordinal, or categorical domains; quadratic, Huber, logistic,
  Poisson, cumulative-ordinal, and softmax entry losses; typed expanded
  decoders and domain-preserving predictions; and an observation pattern that
  keeps weighted point observations, missingness, structural inapplicability,
  and censoring disjoint. Entry losses and factor penalties remain different
  types, and missingness declarations carry no automatic MAR/MNAR claim.
  `GeneralizedLowRankProgram.fit` admits curvature-bounded, unconstrained
  losses with coercive row and decoder penalties to a two-block PALM plan and
  returns the common family-indexed `FittedModel`: learned factors, exact
  observation/program bindings, solver trace and certificate, achieved
  guarantee, and frozen latent encoder travel as one artifact. Poisson,
  ordered-natural-parameter losses, censoring, and objectives without a
  bounded-level-set witness fail before execution;
- `FittedLatentEncoder` for nonlinear new-row inference against a frozen GLRM
  decoder, deliberately separate from linear `FittedProjection`; it consumes
  explicit dense or sparse observation patterns, solves globally
  curvature-bounded convex code objectives with row ridge/L1 penalties, and
  returns support, objective, decoded values, proximal-gradient evidence, and
  a proof-bearing or explicitly uncertified uniqueness status;
- a structured multiblock GLRM family with verified shared-row bindings,
  heterogeneous block layouts and losses, explicit observed-sum or
  mean-observed weighting estimands, one shared row-code penalty, block-local
  decoder penalties, and identity-bound graph/linear TV or smoothness operators
  whose adjoints are derived algebraically. Aligned shared scores, independent
  direct sums, and hub-aligned entity studies remain different types;
- `FittedAlignedMultiblockEncoder`, which compiles frozen block decoders and
  explicit one-row observation patterns into one convex latent-code problem,
  then returns the global certificate alongside block-local support, decoded
  predictions, weighted loss contributions, and provenance;
- a convergence-honest `PalmSolver` over ordered named blocks, admitted only
  with bounded-level-set/coercivity evidence, per-block convexity and positive
  Lipschitz witnesses, exact or geometrically summable inexactness, explicit
  singular-geometry policy, and KL evidence when critical-point convergence is
  claimed. Receipts retain every objective transition, residual, normalization
  error, step, and stopping reason. Every run also retains a solver-trace
  numerical certificate whose convergence flag cannot turn an iteration limit
  into a convergence claim; deterministic multi-start retains all SVD-derived
  and named starts;
- a separate `ConvexLowRankGlobalAdmission` for witnessed convex
  loss-plus-nuclear-norm certificates, so a PALM stopping status cannot be
  relabeled as global optimality;
- `RecoveryValidation` contracts that bind all learned offsets, scales, loss
  balancing, graphs, encodings, ranks, and penalties to a fold-safe `ModelSpec`;
  keep row/column/entry/group/site/error resampling distinct; separate fixed
  masks, synthetic MCAR, MAR sensitivity, and MNAR sensitivity; and preserve
  deterministic within-fold warm-start lineage;
- checked sparse-smooth, disconnected-graph, high-dimensional, correlated-noise,
  weak-gap, block-imbalance, and graph-misspecification simulation designs,
  with projector/factor/support/risk/roughness/stability/calibration reports and
  empirical-versus-theorem-backed claim admission;
- a typed `MathematicalOracleMatrix` spanning every model contract, with
  analytic and independent differential fixtures, published Allen/GMD and GLRM
  limits, metamorphic laws, mutation sentinels, trajectory obligations,
  deterministic conditioning-aware tolerances, and explicit PR-fast,
  reference, and nightly stress tiers. The GLRM case includes pinned
  `LowRankModels.jl` fixed-factor, convex row-encoding, and deterministic
  multi-start fitted fixtures on both JVM and Scala.js, while retaining
  analytic and independent convex checks because the Julia package is rejected
  as a sole or proof oracle;
- fold-safe `ModelSpec` execution that fits preprocessing, learned operators,
  policies, programs, lowerings, and solvers on training identities only and
  returns transformations bound to the fitted feature and row provenance;
- CPCA as one `CpcaOperatorProblem` over a typed table, row relationship,
  feature covariance, and row/feature constraint operators; each nonzero block
  exposes one feature `FunctionalFrame`, derived row scores, and an
  `OperatorProgramFit`, while
  unresolved numeric constraint specs remain available for ROI planning;
- row-whitening/projector geometry for design-conditioned effect operators,
  kept separate from row bilinear geometry and connected explicitly through
  the induced `D = W' W` metric when design-conditioned GPCA is wanted;
- shared error ADTs for estimation, preprocessing, and operator layers;
- pure `MultivarPlan` / `FitArtifactShape` descriptions for sample-by-feature
  ROI execution, including diagram-backed GPCA.
- pure whole-input `PairedMultivarPlan` descriptions for paired latent
  analyses; ROI-by-ROI paired execution is deliberately deferred to a later
  adapter/executor boundary.

`multivar-ir` serializes the semantic graph—space identities, orientation,
forms, certificates, scale/gauge, centering, singular policy, alignments,
objectives, unsafe assumptions, and payload hashes—for cross-language
conformance. Its companion
`scalafim-mathematical-model-evidence-ir/2.0` envelope binds extant
operator-program identities to the model family and estimand, explicit
loss/mask/geometry/penalty declarations, theorem witnesses, solver trace,
achieved guarantee, certificate set, and reproducibility receipt. The external
review boundary and counterexamples are documented in
[`multivar-external-review.md`](../../docs/plans/multivar-external-review.md).

## Penalty identity and ownership

`PenaltyFunctionalIdentity` is the single stable name for shared mathematics:
L1 is L1 and a squared Frobenius or squared smoothness penalty is a
`SquaredNorm`. It is intentionally not executable. Each family retains a typed
`PenaltyFunctionalWitness` with the information needed to use that identity
lawfully:

- `FunctionalKind` owns operator-program geometry, groups, tuning parameters,
  traits, and `TargetExpression` compatibility;
- `GlrmFactorPenalty` owns dense factor evaluation and targets either row codes
  or the feature decoder through `GlrmFactorTarget`;
- `BlockStructuredPenaltyKind` owns graph-versus-linear topology and smooth-
  versus-nonsmooth evaluation on a block-local decoder operator;
- `QuadraticFamily` records why a squared norm exists, while
  `QuadraticPlacement` continues to distinguish objective regularization from
  denominator geometry.

Those targets, capabilities, parameters, placements, and topology choices are
genuinely family-specific and must not be inferred from the shared identity.
Evidence IR 2.0 stores the canonical identity together with its explicit owner
and optional operator identity, so serialization does not invent another
functional vocabulary.

## API boundary

GPCA code constructs a `SemanticDualityDiagram` and calls `SemanticGpca.fit`,
or constructs `GpcaProblem` directly from frozen table and geometry operators.
Dynamic ROI planning freezes its runtime spaces and `MetricSpec` lifecycle
inputs immediately into the same `Op` graph. `MetricSpec` is a validated
construction specification; it is not a second semantic metric representation.
Unsafe evidence assumptions remain explicit through
`Unsafe.assumeSymmetric/assumePsd/assumeSpd` and retain their reason in
provenance.

Typed paired code should construct `PairedOperatorProblem.fromTables`, supplying
the two self row geometries and the directed cross-row relationship explicitly.
The `Plsc`, `Cca`, and `ReducedRankRegression` matrix entry points are lifecycle
conveniences: after preprocessing they construct that typed problem and return
typed fitted frame or coefficient transforms derived from its operator fit.
The RRR coefficient is a directed `OpCoefficient`; prediction is exposed by a
`FittedCoefficientTransform` rather than a separate decoder hierarchy.

`ConstrainedCanonicalProblem` is the coordinate-constrained counterpart of the
ordinary canonical-effect problem. Its first estimand is the nonnegative
generalized-Rayleigh root. The nonnegative cone is an explicit
`ConstraintTerm`, so the resulting `OperatorProgram` reports permutation—not
orthogonal—equivalence and a stationary-point guarantee. ScalaFIM owns those
scientific semantics; the reusable projected iteration and its KKT,
feasibility, and normalization certificates come from Gale.

`FittedFrameTransform` is the analysis boundary for new data. Its partial APIs
distinguish additive contribution from latent-score recovery in their result
types. `FittedBidirectionalTransform` is a separate capability that exists only
after a decoder policy has been validated. `SupplementaryProjector` is a
training-row operation producing a variable-by-component frame, not a row-score
projection. `FittedMultiblockProjection` preserves this same distinction per
block. The complete mathematical and failure contract is
[`multivar-fitted-projection-contract.md`](../../docs/plans/multivar-fitted-projection-contract.md).

The current solver compiler deliberately rejects PSD-cone, Stiefel,
fixed-support, and rank-bounded feasible sets, and general/nonlinear target
charts without a matching executable capability. Its convex first-order path
optimizes coefficient-space refinements around a supplied anchor; it does not
claim to solve an arbitrary normalized nonconvex `OperatorProgram` itself.
`RankOneStructuredFactorization` is the narrow exception: its biconcave GMD
blocks are lowered one at a time to that certified convex compiler and then
normalized under degree-one penalty assumptions. Nonsmooth simultaneous
rank-k remains a typed rejection rather than a deflation fallback.

CPCA code constructs `CpcaOperatorProblem` and fits a validated
`CpcaBlockRequest`. Planned ROI execution constructs the same typed problem
directly and carries `PreparedCpcaOperatorFit`; no raw CPCA problem or resolved
map constraint layer exists.

`multivar` depends only on `linalg`. Keep dataset, image, MVPA adapter, JVM
solver backend, and scheduler-specific code in higher modules.

## Neuroimaging boundary

Neuroimaging modules should translate their own objects into pure multivar
plans instead of pulling dataset/image/runtime types into this module.

- `mvpa` pattern sources map naturally to `SampleByFeatureInput` plus a
  `RoiPlanSet`.
- `dataset` and ROI adapters should carry only serializable source references
  such as `MultivarSourceRef.DatasetSelection(...)` at this layer.
- CPCA formula interfaces, sample/feature metadata encoders, and model-matrix
  builders live in design, MVPA, dataset, or user-facing adapter modules.
  `multivar` accepts numeric `CpcaConstraint` values and resolves them
  only after the concrete row/ROI spaces and metrics are known.
- `LocalMultivarExecutor` is the reference interpreter for ROI/block execution.
  A later JVM adapter can partition by ROI and broadcast small fitted maps using
  the same `MultivarPlan` and `FitArtifactShape` values, without changing the
  shared algebra.

## Release evidence

The shared test suite covers the current core invariants on both JVM and JS:

- typed ids, dimensions, index sets, and complete disjoint block partitions;
- dense, sparse, and affine `MatrixView` algebra without implicit sparse
  densification, including lazy transposed views for duality symmetry;
- preprocessing, typed operator composition, fitted-transform, and
  coefficient-orientation boundaries, including schema permutations, partial
  projection laws, synthesis/reconstruction, supplementary variables, paired
  transfer, and multiblock additivity;
- SVD/PCA plus operator-program PLSC/regularized CCA/reduced-rank regression,
  including typed partial row relationships, generalized cross-SVD residuals,
  row-permutation laws, directed coefficient orientation, and unchanged R
  parity fixtures;
- row/column geometry and GPCA, including dense, diagonal, sparse-preserving,
  rank-deficient PSD preparation, and R-reference-backed paths;
- CPCA identity/zero/basis constraint specs, typed projector orientation,
  independent `X* A X` and projected-block oracles, ROI-local operator-plan
  execution, sparse materialization rejection, generic program/result
  semantics, diagonal-metric whitening, four-block orthogonality,
  reconstruction, partition inertia, and metric-orthonormal factors;
- semantic-diagram construction, algebraic dual invariants, metric
  self-adjointness, centered/support-restricted preparation, clustered-spectrum
  evidence, and backend/policy diagnostics;
- centering projection laws, certificate invalidation, and explicit support,
  quotient, regularization, or rejection policies for singular geometry;
- exact, partial, coupling, signed-link, and hub-factorized alignment laws,
  including unmatched support and global PSD construction;
- direct-sum association/agreement/constraint compilation with block design
  kept separate from row correspondence;
- typed multiblock partitions, lifted block frames, and direct-sum operators;
- row-side whitening/projector/effect-operator algebra matching the
  multivarious fixed-effect projector form, including equivalence between
  whitening-then-PCA and GPCA under the induced row metric;
- kernel and Nyström artifacts, including out-of-sample projection;
- pure ROI/sample-by-feature `MultivarPlan` execution that stays independent of
  MVPA, dataset, image IO, concrete schedulers, and JVM-only numeric libraries.
- pure whole-input `PairedMultivarPlan` validation for paired latent analyses,
  kept separate from the ROI-local `MultivarPlan` executor path.
- executable variational compiler oracles, KKT/gap/feasibility evidence,
  unsupported-capability rejection, and fold lifecycle/leakage audits.
- ordinary PCA and generalized-GMD reductions, functional and sparse limits,
  two-way sparse-smooth and TV factors, planted-direction recovery, explicit
  zero solutions, greedy-deflation semantics, and exact joint generalized
  cross-SVD rank-k evidence on both JVM and Scala.js.
- analytic mixed-domain GLRM loss/decoder oracles, exact masked and weighted
  objective accounting, typed censoring and domain failures, and the complete
  quadratic PCA-reconstruction reduction on both platforms.
- partial-code ridge/least-squares and L1 analytic reductions, an independent
  logistic convex oracle, dense/sparse mask equivalence, compatible-observation
  metamorphism, and typed empty/unseen/censored/non-identifiable outcomes.
- structured multiblock loss-scaling and shared-penalty accounting, graph/TV
  adjoint provenance, block permutation and graph-relabeling laws, split-weight
  duplication invariance, analytic joint partial encoding, and typed
  alignment/domain failures on both platforms.
- exact and summably-inexact PALM oracles, sufficient-decrease traces,
  coordinatewise versus KL-backed stationary claims, iteration-limit and
  descent-violation outcomes, singular-geometry admission, adversarial
  deterministic multi-start, and separate nuclear-global certification.
- complete fold-safety manifests, all six resampling units, disjoint
  missingness targets, deterministic warm-start provenance, checked coverage of
  seven adversarial recovery regimes, independent recovery-metric oracles, and
  rejection of empirical-only support/inferential claims.
- a complete mathematical-oracle matrix with diagonal, 2x2 Laplacian,
  soft-threshold, tiny fused-lasso, and singular-nullspace fixtures; independent
  convex and published limiting cases; permutation/relabeling/scaling/change-of-
  coordinates/penalty-limit/sparse-dense laws; targeted adjoint, norm, proximal,
  mask, and fold-provenance mutations; and objective/residual trajectory checks
  from the same shared suite on JVM and Scala.js.
- mathematical-evidence IR conformance for all six estimands, including typed
  rejection of family/estimand mismatches, incomplete theorem witnesses,
  inadmissible global claims, missing guarantee certificates, implicit GLRM
  losses, unstable reproducibility receipts, future schema versions, and
  unknown fields on both platforms.
