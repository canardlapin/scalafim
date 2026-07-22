# Multivar external mathematical review dossier

Status: normative review aid. This document states what a skeptical methods
reviewer should be able to falsify. Equations and executable contracts are in
[`multivar-mathematical-contract.md`](multivar-mathematical-contract.md); this
dossier maps them to code, proof obligations, counterexamples, and release
evidence.

## Review stance

ScalaFIM does not treat “low rank”, “sparse”, “smooth”, or “converged” as proof
claims. A supportable result must identify:

1. the estimand and its equivalence class;
2. the exact data, observation mask, row/column geometry, and block weighting;
3. the penalty owner and linear operator to which each penalty applies;
4. theorem assumptions admitted before solving;
5. the guarantee actually achieved, with its residual/gap certificate;
6. fold-local fitting and tuning provenance; and
7. independent, metamorphic, adversarial, and cross-platform evidence.

The companion `scalafim-mathematical-model-evidence-ir/1.0` record makes these
items portable. It references the existing operator-program document rather
than serializing a second numerical model.

## Formula-to-API-to-IR map

| Estimand | Formula identity | Executable API | Evidence IR family | Ordinary strongest claim |
|---|---|---|---|---|
| anchor coefficient refinement | `anchor-refinement` | `CompositeSparseSmoothProgram` | `anchor_regularized_frame` | epsilon-global / unique-minimizer bound |
| generalized spectral subspace | `generalized-spectral-frame` | `ExactSpectralPrograms` | `exact_spectral_frame` | exact or epsilon spectral global |
| joint sparse-functional factors | `joint-sparse-functional-factorization` | `RankOneStructuredFactorization` / joint rank-k API | `joint_sparse_functional_factorization` | coordinatewise stationary; stationary only with PALM/KL admission |
| generalized latent representation | `masked-generalized-low-rank-model` | `GeneralizedLowRankProgram` | `generalized_low_rank_model` | stationary / coordinatewise stationary |
| convex low-rank matrix | `convex-loss-plus-nuclear-norm` | `ConvexLowRankGlobalAdmission` | `convexified_low_rank_matrix` | formulation-specific global |
| shared-block latent representation | `aligned-structured-multiblock-factorization` | `AlignedSharedScoreGlrm` | `structured_multiblock_factorization` | stationary / coordinatewise stationary |

`FittedProjection`, `FittedLatentEncoder`, and
`FittedAlignedMultiblockEncoder` are post-fit actions. A nonlinear partial-row
code solve is not relabeled as a linear variable projection.

## Theorem admission map

| Claim | Required assumptions represented in code | Evidence checked at return |
|---|---|---|
| exact spectral global | symmetric value operator, SPD normalization or explicit support/quotient reduction, certified spectrum | generalized eigen/cross-SVD equations, normalization, rank/cluster receipt |
| epsilon-global anchor refinement | proper closed convex terms, strong anchor, exact prox/projection or sound splitting, derived norm bounds | objective decomposition, fixed-point/KKT residual, dual feasibility, primal-dual gap |
| unique minimizer bound | strong-convexity modulus plus objective-gap or stationarity bound | quantitative distance bound and certificate identity |
| coordinatewise stationary factorization | bounded iterates/level set, convex admitted blocks, exact or controlled inexact block solves | residual for every named parameter block and objective stabilization |
| stationary PALM | bounded level set, positive block Lipschitz witnesses, proper block terms, summable error if inexact, KL/semi-algebraic objective evidence | sufficient-decrease trajectory and full stationarity receipt |
| convex nuclear global | convex matrix loss, sufficient factor rank where a factor bridge is used, certified nuclear subgradient | zero/bounded subgradient residual bound to the convex formulation |

If a witness is absent, the 1.0 evidence validator rejects the stronger claim.
An iteration-limit result remains `Unresolved`; a low-level solver status cannot
select a mathematical guarantee.

## Sparse plus smooth factors

The Allen-style rank-one model places quadratic smoothness in the certified
factor geometries and degree-one sparsity/TV penalties in the block objective.
For one side,

\[
R_\beta=R_0+\sum_j\beta_j S_j^\top H_jS_j,
\qquad
P(v)=\lambda_1\lVert v\rVert_1+lambda_{TV}\lVert Dv\rVert_1+cdots.
\]

The fixed-other-factor update is convex and is solved by the same exact-prox or
sound composite-splitting compiler used by anchor refinement. The outer
bilinear problem remains nonconvex. This supports a block-stationarity claim,
not a generic global sparse-functional PCA claim. The zero solution, singular
smoothness nullspace, and degree-one homogeneity cases have explicit behavior.

The construction follows the distinction made in Allen and Weylandt's
[Sparse and Functional PCA](https://arxiv.org/abs/1309.2895) and the generalized
geometry of Allen, Grosenick, and Taylor's
[Generalized Matrix Decomposition](https://arxiv.org/abs/1102.3074). Greedy
deflation and simultaneous multi-rank estimation remain different estimands,
consistent with the issues isolated in Weylandt's
[multi-rank sparse functional PCA](https://arxiv.org/abs/1907.12012).

## Udell and LowRankModels.jl review

The useful design lesson from Madeleine Udell and collaborators is the clean
separation

\[
\text{observed-entry loss}(UV^\top;\Omega)
+\text{row regularizer}(U)+\text{column regularizer}(V).
\]

ScalaFIM retains that separation in different closed types: `EntryLoss` owns
domain-appropriate data fit; `GlrmFactorPenalty` owns structure; and
`ObservationPattern` owns point-observed, missing, structural, and censored
states. The feature layout additionally preserves expanded natural-parameter
width and categorical/ordinal gauges. This is close in spirit to the
[GLRM paper](https://arxiv.org/abs/1410.0342) and
[LowRankModels.jl](https://github.com/madeleineudell/LowRankModels.jl), while
making nominal spaces, certificates, and cross-platform execution explicit.

LowRankModels.jl is not used as a proof oracle. Its alternating proximal
optimization is subject to the same nonconvex limitations as any factored GLRM,
and software agreement can reproduce a shared mistake. The executable oracle
matrix therefore permits it only alongside an independent convex reference or
a published limiting case. The PCA/Frobenius, analytic mixed-loss, masked
objective, and convex new-row-code reductions are checked independently.

The committed secondary-oracle environment pins LowRankModels.jl 1.1.1 at
`a18f0df45f1a6ce37634bf4e347062b6090397eb`. Shared JVM/Scala.js suites consume
generated fixtures for fixed-factor objectives and gradients, frozen-decoder
convex encodings, and a ridge-regularized rank-one multi-start fit. Fitted
comparisons use reconstruction, cross-evaluated full objectives, decoded
predictions, stationarity, and monotone full-objective checkpoints rather than
literal factors. The fixture also records that the upstream
`ConvergenceHistory.objective` is not the full GLRM objective in this program;
fixed-budget fits and independently recomputed objectives avoid treating that
history field as a convergence certificate. Reproduction commands and the
complete admitted/excluded semantic map live in
`tools/oracles/lowrankmodels/README.md`.

## Counterexamples that define the boundary

1. **Post-hoc shrinkage is not joint sparse PCA.** Soft-thresholding a fitted
   PCA loading optimizes an anchor-refinement objective. It generally differs
   from jointly optimizing the bilinear sparse factor objective.
2. **A stopping flag is not stationarity.** A solver can stop at its iteration
   limit with a large block residual. The result is `Unresolved`, even if the
   last objective change is small.
3. **Blockwise convexity is not joint convexity.** Squared loss in `U` with `V`
   fixed and in `V` with `U` fixed does not make the product `UV*` jointly
   convex.
4. **Singular smoothness is not automatically harmless.** A graph Laplacian
   has a constant nullspace. Without a base metric, support restriction,
   quotient semantics, coercive penalty, or compact normalization, scale can be
   unidentified.
5. **Deflation is not simultaneous rank-k.** Sequential subtraction changes
   the next optimization problem and need not recover a joint generalized-
   Stiefel solution under nonsmooth penalties.
6. **Duplicating a block can change the estimand.** Observed-entry-sum weighting
   doubles a duplicated block's contribution. Invariance holds only when the
   declared importance is split, or under the separately declared
   mean-observed convention.
7. **A mask is not a missingness theorem.** Fixed-mask prediction, synthetic
   MCAR deletion, MAR sensitivity, and MNAR sensitivity support different
   conclusions. None alone licenses population inference.
8. **Equal row counts do not prove alignment.** Shared-score fitting requires a
   verified row binding. Otherwise the model is direct-sum or hub-aligned.
9. **Full-data graph or scale estimation leaks.** Every learned offset, scale,
   loss balance, graph, encoding, rank, and penalty selection must carry the
   training-scope identity minted by `ModelSpec`.

## Unsupported or fail-closed cases

- nonsmooth simultaneous rank-k factor fitting without a dedicated
  manifold/PALM admission;
- a generic global optimum for sparse-functional, GLRM, or multiblock factored
  programs;
- Poisson partial-row encoding without backtracking or an explicit bounded
  natural-parameter domain;
- ordinal partial-row encoding without projection onto the ordered parameter
  domain;
- censoring under a point-loss objective without a declared censoring
  likelihood;
- PSD-cone, Stiefel, fixed-support, or rank-bounded variational constraints
  without the corresponding solver capability;
- implicit alignment, automatic cross-loss variance standardization, implicit
  sparse densification, or silent nullspace regularization; and
- support-recovery or inferential claims backed only by empirical simulation.

Each case is a typed rejection or a named unsupported ledger entry, not a
documentation caveat applied after fitting.

## Reproducibility record

Every publishable mathematical evidence record contains:

- generator identity and a non-negative JSON-safe deterministic seed;
- dependency names and versions, including ScalaFIM/Gale as applicable;
- the condition estimate used to interpret numerical error;
- explicit absolute and relative tolerance;
- data, mask, program, policy, trace, certificate, and result identities; and
- the achieved quantitative residual, gap, or distance bound.

Golden output without this receipt is regression data, not release evidence.
The shared conformance corpus round-trips all six families and rejects invalid
family/estimand, theorem, guarantee, certificate, mask/loss, solver, numeric,
schema-version, and unknown-field cases on both JVM and Scala.js.

## Red-team questions and required answers

1. **What exactly is estimated?** Name one of the six estimands and its
   equivalence class; “components” is insufficient.
2. **Was structure fitted jointly or after the fact?** Point to the model family
   and formula id. Anchor refinement must not be described as joint sparse PCA.
3. **Where do nullspaces go?** Show SPD evidence, support restriction, quotient
   semantics, or the coercive/compact argument.
4. **Why is the claim global?** Produce the exact spectral, convex-gap, or
   nuclear-subgradient theorem witness. Otherwise state stationary,
   coordinatewise stationary, feasible, or unresolved.
5. **Is rank-k joint?** Identify simultaneous constraints and objective. If
   extraction is sequential, call it deflation and retain its order.
6. **What sets block scale?** State observed-sum versus mean-observed weighting,
   scientific importance, and fold-fitted preprocessing. Do not invoke an
   undocumented normalization heuristic.
7. **What does missingness mean?** Distinguish an explicit mask from an MCAR
   generator, MAR mechanism, or MNAR selection model and limit conclusions to
   that target.
8. **Could the fold see the answer?** Show training-scope identities for every
   fitted transform, graph, encoding, rank, and penalty choice.
9. **What is the independent oracle?** Name an analytic, exhaustive convex, or
   published limiting-case oracle. LowRankModels.jl alone is insufficient.
10. **Can the result be regenerated?** Supply the 1.0 evidence document and the
    exact committed-tip JVM/Scala.js verification receipt.

## Release decision

External-review readiness requires agreement among the mathematical contract,
operator-program IR, 1.0 evidence envelope, theorem witnesses, achieved
certificate, oracle matrix, fold-safety report, documentation, and exact-tip
cross-platform build. Any disagreement blocks release; closing a tracker item
or observing a green solver status is not a substitute.
