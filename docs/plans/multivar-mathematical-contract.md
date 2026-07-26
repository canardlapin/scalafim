# Multivar mathematical contract

Status: normative architecture contract; executable families remain governed by
the maturity and release evidence recorded in code and Mote.

This document separates six estimands that must not be conflated. Each family
has one formula identity, API identity, IR identity, admissible optimization
claim set, theorem ledger, oracle set, and unsupported-case ledger in
`MathematicalContractCatalog`.

## 1. Anchor-regularized frame

Given a frame `A` fitted by an earlier model, estimate a refined coefficient
frame `W`:

\[
  \min_W \frac{1}{2}\lVert W-A\rVert_F^2
  + \sum_r \lambda_r\,\phi_r(T_rW)
  \quad\text{subject to}\quad W\in\mathcal C.
\]

This is a convex coefficient-space refinement when every declared term is
proper, closed, and convex. It is not the estimand of jointly fitted sparse or
functional PCA. The strongly convex anchor may support an epsilon-global or
distance-to-unique-minimizer certificate when the final residual or gap is
actually checked.

## 2. Exact spectral frame

For symmetric value operator `A`, SPD normalization `G`, and PSD quadratic
smoothness operators `L_s`, solve

\[
  \max_W \operatorname{tr}(W^\top A W)
  - \sum_s \lambda_s\operatorname{tr}(W^\top L_s W)
  \quad\text{subject to}\quad W^\top G W=I.
\]

When the admitted problem reduces to a certified symmetric generalized
eigensystem, the selected subspace has a global spectral characterization.
Nonsmooth coordinate penalties generally destroy this reduction and must not
inherit its guarantee.

## 3. Joint sparse-functional factorization

A rank-one generalized matrix decomposition uses

\[
  \max_{u,v}\; u^\top Q_\alpha X R_\beta v-P_u(u)-P_v(v),
  \qquad u^\top Q_\alpha u\le1,\quad v^\top R_\beta v\le1.
\]

Rank-k joint fitting, sequential extraction, and deflation are different
estimands. The executable Allen-style formulation represents functional
smoothness in the certified factor geometries

\[
  Q_\alpha=Q_0+\sum_s\alpha_s T_s^\top G_sT_s,
  \qquad
  R_\beta=R_0+\sum_t\beta_t S_t^\top H_tS_t,
\]

while `P_u` and `P_v` contain only proper closed convex degree-one terms such
as L1, group/sparse-group, and total variation. Smoothness and sparsity can
therefore be active on either or both factor sides. An additive quadratic
penalty under a fixed normalization metric is a different estimand and is not
silently rewritten into this one.

For a fixed `v`, the row update is the convex generalized penalized regression

\[
  \widetilde u=\arg\min_z
  \frac12\lVert z-XR_\beta v\rVert_{Q_\alpha}^2+P_u(z),
  \qquad
  u=\begin{cases}
    \widetilde u/\lVert\widetilde u\rVert_{Q_\alpha},&\widetilde u\ne0,\\
    0,&\widetilde u=0.
  \end{cases}
\]

The column update is its typed dual. `CompositeSparseSmoothProgram` solves
each regression exactly or to a reported epsilon certificate. The outer fit
stops only after both normalized block maps and the objective stabilize; it
then recomputes both block residuals independently. A converged nonzero fit or
the coordinatewise-optimal zero pair receives coordinatewise-stationary
evidence. Exhausting the iteration budget receives `Unresolved`, never a
stationarity claim.

Greedy rank-k extraction repeatedly subtracts the fitted rank-one operator and
declares extraction ordering with no orthogonality claim. It is not a joint
rank-k fit. The separate joint program uses the product generalized-Stiefel
constraints

\[
  U^\top Q_\alpha U=I,\qquad V^\top R_\beta V=I.
\]

When both nonsmooth penalty bundles are empty, this joint program reduces to
the generalized cross-SVD of
`Q_alpha^(1/2) X R_beta^(1/2)`. The returned factors, ordering, metric
orthogonality, reconstruction, and generalized singular equations are checked,
and the special solution receives an exact-global generalized-cross-spectrum
witness. Joint rank-k nonsmooth penalties are rejected until the dedicated
manifold/PALM solver can certify them; they are never approximated by
deflation.

Even with convex block penalties, the general joint factor problem is
nonconvex. Its ordinary claims are stationary or coordinatewise stationary,
under the assumptions of the selected PALM or block-coordinate theorem. The
exact unpenalized generalized cross-SVD is a theorem-delimited special case,
not a generic factorization guarantee.

## 4. Generalized low-rank model

For an explicit observation set `Omega`, column-appropriate entry losses, and
factor penalties, estimate

\[
  \min_{U,V}\sum_{(i,j)\in\Omega}
  w_{ij}\,\ell_j(u_i^\top v_j,A_{ij})
  +P_U(U)+P_V(V).
\]

Observed zero and missing are distinct. A mask makes partial fitting
computable but does not establish MCAR, MAR, or MNAR validity. Encoding a new
partially observed row solves a latent-code optimization problem against fixed
`V`; under nonquadratic loss it is not a fitted linear projection.

The executable semantic layer is `GeneralizedLowRankProgram`. A
`GlrmFeatureLayout` assigns every original feature a stable id,
`FeatureDomain`, `EntryLoss`, natural-parameter width, and decoder slice.
`FeatureEmbedding` converts one validated raw observation into the sufficient
target expected by that loss. `FeatureDecoder` maps a latent row code into the
feature's natural parameters; `DecodedPrediction` returns a point,
probability, expected count, ordinal distribution, or categorical distribution
without erasing the feature domain.

The admitted entry-loss families are:

| Domain | Loss | Natural parameters | Convexity / gauge |
|---|---|---|---|
| real | half squared error | one unrestricted value | strictly convex |
| real | Huber with positive delta | one unrestricted value | convex |
| binary `{0,1}` | Bernoulli logistic | one unrestricted logit | strictly convex |
| nonnegative integer count | Poisson log link | one unrestricted log mean | strictly convex |
| ordered levels | cumulative logistic | ordered non-increasing logits | convex on that domain |
| categorical levels | softmax cross-entropy | one logit per level | convex; common-shift gauge |

`ObservationPattern` is a row-major typed state graph, not a Boolean matrix.
Its cells are disjoint cases: point-observed with a strictly positive weight,
missing with a reason, structurally inapplicable with a reason, or censored
with a validated bound/interval and weight. Consequently an observed zero is
never dropped by truthiness, and a zero weight cannot turn an observation into
implicit missingness. Censored cells require a declared censoring likelihood;
the point-loss objective rejects them rather than skipping them.

For factors `U` and feature decoder `V`, evaluation computes exactly

\[
  \sum_{(i,j)\in\Omega_{point}}w_{ij}
  \ell_j(e_j(A_{ij}),\eta_j(U_i,V_j))
  +\sum_r\lambda_r P_r(U)+\sum_s\gamma_s P_s(V).
\]

`EntryLoss` and `GlrmFactorPenalty` are separate closed types. Missing and
structurally inapplicable cells contribute no point loss, while every
point-observed cell contributes exactly once. `MissingnessStatement` and
`GlrmPredictionTarget` are retained on the program, but a user-declared MCAR,
MAR, or MNAR mechanism is provenance only and cannot manufacture an
inferential certificate. Under complete observations, quadratic losses, and
no factor penalties, the objective reduces exactly to one half of the
Frobenius reconstruction error used by the PCA/SVD limit.

### Partial-row encoding is not projection

`FittedProjection` remains the linear action of a fitted frame. A
`FittedLatentEncoder` instead freezes the GLRM feature decoder and solves the
new-row convex code problem

\[
  \widehat x=\arg\min_x
  \sum_{j\in\Omega_{new}}w_j\ell_j(e_j(a_j),\eta_j(x,V_j))
  +\frac{\lambda_2}{2}\lVert x\rVert_2^2
  +\lambda_1\lVert x\rVert_1.
\]

The input is an explicit one-row `ObservationPattern`; zero is never a mask.
Dense cell vectors and sparse point-observation lists compile to that same
pattern. The result binds the latent code, exact observed support, objective
decomposition, decoded values for the fitted feature domains, proximal-gradient
residual, numerical stopping record, and uniqueness status. Positive ridge
certifies strong convexity directly. Without ridge, a full-rank quadratic
observed-decoder Gram certifies uniqueness; otherwise the returned code is
marked `NotCertified` rather than declared unique.

The portable encoder currently admits the smooth losses with global curvature
bounds: quadratic, Huber, Bernoulli logistic, and categorical cross-entropy,
combined with row L1 and/or squared-Frobenius penalties. Poisson log-link loss
needs a backtracking or bounded-natural-parameter solver because its curvature
is not globally bounded. Cumulative ordinal loss needs projection onto its
ordered natural-parameter domain. Both cases are typed `UnsupportedLoss`
outcomes for now. Empty support, foreign domains, unseen categorical levels,
censoring without a likelihood, and a completely flat code objective also fail
through named outcomes rather than falling back to linear projection.

## 5. Convexified low-rank matrix

The special convex matrix problem

\[
  \min_Z L(Z)+\lambda\lVert Z\rVert_*
\]

is a separate family. Convex-loss, sufficient-rank, and nuclear-subgradient
conditions may yield a global certificate. That result must not be generalized
to arbitrary factored GLRMs.

## 6. Structured multiblock factorization

For row-aligned blocks, a shared-score formulation is

\[
  \min_{U,V_1,\ldots,V_B}
  \sum_b\sum_{(i,j)\in\Omega_b}
  w_{bij}\ell_{bj}(u_i^\top v_{bj},A_{bij})
  +P_U(U)+\sum_bP_b(V_b).
\]

Each block declares its observation family, geometry, structural penalty,
scale, and contribution normalization. Unaligned studies retain the existing
hub-alignment or direct-sum semantics rather than being silently concatenated.

The executable aligned family is `AlignedSharedScoreGlrm`. Its blocks hide
their heterogeneous feature and expanded natural-parameter spaces behind type
members while retaining the exact `GeneralizedLowRankProgram`, decoder,
verified row binding, geometry, and value identities. `StructuredMultiblockStudy`
has separate `Aligned`, `DirectSum`, and `HubAligned` cases; there is no adapter
that treats equal row counts as alignment or turns a hub study into a stacked
table.

Loss weighting is part of the estimand. `ObservedEntrySum` makes block size and
original loss units influential by definition. `MeanObservedLoss` removes the
observed-cell count before applying a positive scientific importance. Neither
choice is a hidden heuristic, and neither claims automatic variance
standardization: calibration across differently scaled losses belongs in the
declared block importance or upstream fold-fitted preprocessing. Splitting one
block's importance among identical copies is the explicit duplication law.

The shared row-code penalty is owned by the multiblock program and evaluated
once. Block programs may own decoder penalties. A `BlockDecoderStructure`
applies graph or linear TV/smoothness to one block's expanded decoder
coordinates; it stores the block id and one typed forward operator, and derives
the adjoint algebraically from that same operator. Permuting blocks, jointly
relabeling feature and graph coordinates, and duplicating a block with split
importance preserve their declared objective laws.

For new rows, `FittedAlignedMultiblockEncoder` explicitly compiles the frozen
block decoders and observation patterns into one convex code problem. The
compilation prefixes feature identities, rescales observation weights according
to each block estimand, and checks the compiled global entry loss against the
sum of block-local contributions. Its result retains the global code and
certificate plus block support, decoded predictions, effective coefficients,
loss contributions, and exact block/observation provenance. This compilation
does not collapse the training model into an untyped concatenated matrix, and
it inherits the single-block encoder's current fail-closed Poisson and ordinal
boundaries.

### PALM and block-coordinate receipts

`PalmProblem` is the portable outer-iteration boundary for nonconvex factored
programs. A problem binds the mathematical contract, data and mask identity,
objective, operator identities, ordered parameter blocks, and one executable
block oracle per parameter. Each block admission names the proper-closed-convex
subproblem functional and a positive partial-gradient Lipschitz bound. A
`PalmAdmission` additionally requires one bounded-level-set argument: either a
coercivity modulus or a compact normalization set. A singular normalization
geometry is inadmissible unless support restriction or quotient-nullspace
semantics are explicit.

Exact block solves and summably inexact solves are different policies. The
latter uses an explicit geometric error schedule `epsilon_k` with contraction
strictly below one, so the infinite error sum is finite, and every reported
block error is checked against `epsilon_k`. Critical-point convergence requires
objective-bound semi-algebraic/KL evidence. Without KL evidence, the strongest
admitted converged result is coordinatewise stationarity.

Every accepted update must obey either monotonicity or the configured
sufficient-decrease inequality, including the declared inexactness allowance.
The receipt retains every block objective transition, step norm, subproblem
residual, normalization error, inexactness value, solve kind, full sweep
objective, block stationarity residuals, and stopping reason. An iteration
limit yields `Unresolved`; an objective increase is a typed failure, never a
converged receipt. Deterministic multi-start retains every SVD-derived or named
start result and selects only by the declared objective/stationarity/id rule.

The generic PALM path can return stationary, coordinatewise-stationary, or
unresolved evidence, but no global factor optimum. `ConvexLowRankGlobalAdmission`
is a separate gate for the convex loss plus nuclear-norm model. It requires the
convexified-low-rank contract, witnessed theorem assumptions, a permitted
global oracle, and a zero nuclear-subgradient residual for exact admission.

### Statistical validation and recovery

Numerical convergence is not statistical validity. `FoldSafetyManifest` binds
the complete learned pipeline to a concrete `ModelSpec`: offsets and scaling
must come from fold-fitted standardization, while loss balancing, graphs,
encodings, rank, and penalties must match declared lifecycle artifacts and,
where tuned, named hyperparameters present in every candidate. Existing
`ModelSpec` scope identities and leakage audits then ensure those artifacts are
fitted only to each training partition.

`ResamplingDesign` requires the sampling unit to be one of rows, columns,
entries, groups, sites, or errors. Group, site, and error resampling additionally
requires a membership/exchangeability identity. These designs are not
interchangeable merely because they produce integer index sets.

Missing-data validation has four disjoint targets. Fixed-mask prediction makes
no stochastic missingness assertion. MCAR simulation is conditional on a
declared independent-deletion generator and probability. MAR and MNAR are
explicit sensitivity analyses bound to their mechanism or selection-model
identity. None of these declarations alone grants an inferential claim.

Deterministic warm starts are ordered within a fold, derive their seeds and
state identities from the split and adjacent candidates, and reset at every
fold. `RecoverySimulationCoverage` requires executed results for sparse-smooth
signals, disconnected graphs, `p >= 10 n`, correlated noise, weak eigengaps,
at least tenfold block imbalance, and materially misspecified graphs. Each
scenario has checked design parameters; a scenario label alone is not coverage.

`RecoveryMetrics` reports projector/subspace error, sign-aligned factor error,
support precision and recall, mean held-out risk, graph roughness, pairwise
support stability, and normalized block-calibration error. Reports retain the
resampling, missingness, fold, warm-start, generator, seed, and result identities.
Predictive and descriptive conclusions can be labeled `EmpiricalOnly`.
Support-recovery and inferential claims are rejected unless they name a theorem,
its assumptions, and a witness identity.

## Claim matrix

| Family | Stage | Strongest generally admissible claim | Explicit exclusion |
|---|---|---|---|
| Anchor-regularized frame | post-fit refinement | epsilon-global / unique-minimizer bound | not joint sparse PCA |
| Exact spectral frame | joint estimation | exact or epsilon global spectral result | nonsmooth penalties are not spectral |
| Joint sparse-functional factorization | joint estimation | stationary / coordinatewise stationary | no generic global factor optimum |
| Generalized low-rank model | joint estimation | stationary / coordinatewise stationary | mask is not a missingness theorem |
| Convexified low-rank matrix | convex matrix estimation | exact or epsilon global | certificate is formulation-specific |
| Structured multiblock factorization | joint estimation | stationary / coordinatewise stationary | shared scores require explicit row alignment |

## Requested claims, achieved guarantees, and proof admission

A declared program carries a `RequestedOptimizationClaim`; `Unresolved` is not
representable as a request. A completed fit instead carries an
`AchievedOptimizationGuarantee`, whose evidence determines one of these
disjoint outcomes:

- exact global, with a theorem-bound global-optimality witness;
- epsilon global, with a checked objective-gap bound;
- unique minimizer within a distance bound, derived from strong convexity and
  either an objective gap or a stationarity bound;
- stationary, with a semantic stationarity residual;
- coordinatewise stationary, with one residual for every bound parameter;
- feasible only, with a feasibility residual; or
- unresolved, with the numerical termination reason retained.

The certificate binds the model contract, program, data, explicit mask state,
operators, parameters, and returned value. Admission checks the requested claim
against the achieved claim class and separately checks every proof obligation
named by the compiler: proper closed convexity, smoothness,
strong convexity, PSD structure, nullspace coercivity, norm bounds, exact
proximal or projection laws, or a controlled inexactness bound. A low-level
stopping status is retained as trace evidence and cannot select a semantic
guarantee. Exact spectral and stationary program fits therefore enter through
different constructors, while anchor-refinement fits expose their quantitative
gap or distance certificate directly.

Historical `ProgramSolverGuaranteeIr` tags remain only in the versioned IR
compatibility boundary; they are not a second runtime truth.

## Exact sparse-smooth compiler

The executable anchor-refinement compiler represents the composite objective
as three algebraically distinct parts:

\[
  f(W) + h(W) + g(KW),
\]

where `f` is the strongly convex anchor plus certified PSD quadratic terms,
`h` is either absent or one penalty with a proven exact direct proximal map,
and `g(KW)` is a stack of weighted elementwise L1 terms. Graph, derivative,
spline, and block smoothness enter `f` only through an exact `T* G T`
pullback. Total variation and other L1 transforms enter `g(KW)` without
forming a dense stacked matrix. Sparse-group and other admitted separable
penalties enter `h` through their chart-law-checked proximal plans.

Compilation follows a closed decision rule:

| Structure | Execution |
|---|---|
| `f + h`, with zero or one exact direct proximal term | proximal gradient |
| `f + h + g(K.)` | Condat--Vu smooth composite primal-dual splitting |
| two or more direct proximal terms without a combined-prox proof | rejected |
| denominator loading presented as objective smoothness | rejected |
| unsupported split functional, foreign parameter, or wrong feature dimension | rejected |

The compiler derives operator-norm upper bounds columnwise as Frobenius norms;
stacked maps use the root-sum-square of their constituent bounds. Callers
cannot inject an optimistic bound. Zero operators are valid, and a smoothness
operator may have a constant-vector null space because the anchor contributes
strong-convexity modulus one.

Every returned fit independently recomputes the objective decomposition,
primal fixed-point residual, dual feasibility residual, and a Fenchel
primal-dual gap. The conjugate term in that gap is bounded using strong
convexity and the residual of a separate proximal-gradient oracle, so inner
iteration error cannot silently become a global-optimality claim. Numerical
termination remains trace evidence; the semantic result is admitted as an
epsilon-global guarantee carrying the measured gap.

## Executable oracle and stress matrix

`MathematicalOracleMatrix` turns the release evidence plan into checked data.
Every case binds one mathematical model contract and implementation path to a
deterministic seed, conditioning-aware absolute and relative tolerance,
evidence location, analytic fixtures, independent differential references,
metamorphic laws, mutation targets, trajectory obligations, representation
laws, and execution tier. Matrix construction fails if it omits a model family,
the diagonal-spectrum, 2x2-Laplacian, soft-threshold, tiny fused-lasso, or
singular-nullspace fixtures, any release metamorphism, any mutation target, or
an execution tier.

Madeleine Udell's `LowRankModels.jl` is a useful secondary differential
reference for loss, regularizer, masking, and alternating-solver behavior. It
is not an independent proof and cannot be the sole oracle in a conformance
case. GLRM cases pair it with a separately implemented convex reference and a
published GLRM limiting case. Allen/GMD limits likewise name the published
estimand rather than treating another software package as ground truth.

Mutation sentinels deliberately corrupt an adjoint pairing, an operator-norm
upper bound, an L1 proximal output, an observed-entry identity sequence, and a
fold training identity. Each corruption must fail through its own typed error.
Optimization cases check the whole objective, step, and residual trajectory;
final-value agreement alone is insufficient where a monotonicity,
sufficient-decrease, or convergence claim is made.

The execution tiers are intentionally different contracts:

| Tier | Required use | Release interpretation |
|---|---|---|
| PR-fast | tiny analytic, mutation, identity, and metamorphic sentinels | required on every change |
| reference | independent convex and published limiting cases | required before mathematical release |
| nightly stress | conditioning, adversarial starts, trajectory/rate, sparse-preservation, and allocation checks | trend and regression evidence; timing is not a theorem |

All conformance cases live in shared source and therefore execute unchanged on
JVM and Scala.js. Sparse paths additionally declare storage preservation and
no-hidden-densification laws. The platform gate joins the two runs at the same
committed tip and declared tolerance; a passing run on one platform is not
cross-platform evidence.

## Release law

A model is supportable only when all of the following agree:

1. formula and statistical estimand;
2. public API and versioned IR;
3. theorem assumptions and compiler admission evidence;
4. achieved numerical certificate;
5. analytic or independent differential oracle where feasible;
6. metamorphic and adversarial laws;
7. fold-local preprocessing and tuning provenance;
8. a `scalafim-mathematical-model-evidence-ir/2.0` envelope binding the
   operator-program identities to the estimand, theorem witnesses, achieved
   certificate, and reproducibility receipt; and
9. JVM and Scala.js verification at the committed tip.

The presence of source code, a low-level `Converged` status, or a closed tracker
item is not release evidence by itself.

The formula-to-API map, proof-admission table, counterexamples, unsupported
cases, Udell/LowRankModels comparison, reproducibility requirements, and
red-team questions are collected in
[`multivar-external-review.md`](multivar-external-review.md).

## Primary references

- Genevera Allen and Michael Weylandt, *Sparse and Functional Principal
  Components Analysis*.
- Genevera Allen, Logan Grosenick, and Jonathan Taylor, *A Generalized Least
  Square Matrix Decomposition*.
- Madeleine Udell, Corinne Horn, Reza Zadeh, and Stephen Boyd, *Generalized Low
  Rank Models*.
- Paul Tseng, block coordinate descent; Bolte, Sabach, and Teboulle, PALM;
  Beck and Teboulle, FISTA; Chambolle and Pock, primal-dual optimization.
